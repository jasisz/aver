//! Regression: wasm-gc `Console.print` must route through
//! `aver::services::console::capture_output`, not raw `println!`.
//!
//! Nightly fuzz `parity_vm_vs_wasm_gc` reported 3 crashes that
//! looked like real semantic divergence:
//!
//!   vm   : Some(("hello,...\n", Null))
//!   wasm : Some(("", Null))            ← empty buffer!
//!
//! Root cause: the wasm-gc backend's `Console.print` import handler
//! (`runtime::wasm_gc::imports::lm::host_print`) used direct
//! `println!` / `eprintln!`, bypassing the per-thread capture
//! buffer the parity target installs via `capture_output`. The VM
//! routes through `services::console`, so it captured correctly;
//! the asymmetry presented as "wasm-gc produced no output" which
//! is a phantom semantic divergence — both backends *did* print
//! the same bytes, the in-process harness just couldn't see the
//! wasm-gc side.
//!
//! This test pins the invariant: a trivial Console.print program
//! must yield identical captured stdout from VM and wasm-gc.

#![cfg(feature = "wasm")]

use aver::ir::{PipelineConfig, TypecheckMode};

const HELLO_SRC: &str = r#"module M
    intent =
        "capture regression"
    effects [Console]

fn main() -> Unit
    ! [Console.print]
    Console.print("hello, world")
"#;

#[test]
fn wasm_gc_console_print_writes_to_capture_buffer() {
    let mut lexer = aver::lexer::Lexer::new(HELLO_SRC);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = aver::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    let _ = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );

    let (run_res, stdout, stderr) = aver::services::console::capture_output(|| {
        aver::runtime::wasm_gc::run_in_process(
            &items,
            None,
            // Single-file program: the default config (no argv, real
            // effects, empty alias map) is exactly the embedding shape.
            aver::runtime::wasm_gc::RunConfig::default(),
        )
    });

    if let Err(e) = &run_res {
        panic!(
            "wasm-gc run_in_process should succeed on hello-world, got: {}",
            e
        );
    }
    assert_eq!(
        stdout,
        b"hello, world\n",
        "wasm-gc Console.print must populate the capture_output stdout buffer; got {:?}",
        String::from_utf8_lossy(&stdout)
    );
    assert!(
        stderr.is_empty(),
        "stderr should be empty for a pure Console.print program; got {:?}",
        String::from_utf8_lossy(&stderr)
    );
}

#[test]
fn wasm_gc_runtime_pipeline_lowers_string_join_builder() {
    const SOURCE: &str = r#"module Builder
    intent =
        "exercise the production wasm-gc String builder matrix"
    effects [Console]

fn parts(values: List<String>, acc: List<String>) -> List<String>
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> parts(tail, List.prepend(head, acc))

fn main() -> Unit
    ! [Console.print]
    joined = String.join(parts(["", "é🙂", "x", "y", "z", "long-fragment"], []), "|")
    Console.print(joined)
"#;
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("main.av");
    std::fs::write(&entry, SOURCE).expect("write builder fixture");
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&entry)
        .arg("--wasm-gc")
        .env("AVER_WASMGC_REQUIRE_MIR", "1")
        .output()
        .expect("run aver --wasm-gc");
    assert!(
        output.status.success(),
        "builder run failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "|é🙂|x|y|z|long-fragment\n"
    );
}

#[test]
fn wasm_gc_builder_preserves_computed_separator_effect_order() {
    const SOURCE: &str = r#"module BuilderOrder
    intent =
        "preserve String.join argument evaluation order"
    effects [Console]

fn observe(value: String) -> String
    ! [Console.print]
    Console.print(value)
    value

fn parts(values: List<String>, acc: List<String>) -> List<String>
    ! [Console.print]
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> parts(tail, List.prepend(observe(head), acc))

fn separator() -> String
    ! [Console.print]
    Console.print("separator")
    "|"

fn main() -> Unit
    ! [Console.print]
    joined = String.join(parts(["a", "b"], []), separator())
    Console.print(joined)
"#;
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("main.av");
    std::fs::write(&entry, SOURCE).expect("write separator-order fixture");
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&entry)
        .arg("--wasm-gc")
        .output()
        .expect("run aver --wasm-gc");
    assert!(
        output.status.success(),
        "separator-order run failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "a\nb\nseparator\na|b\n"
    );
}

#[test]
fn wasm_gc_runtime_pipeline_lowers_builder_inside_bytes_dependency() {
    const SOURCE: &str = r#"module BuilderDep
    intent =
        "exercise String builder lowering in a loaded dependency"
    depends [Bytes]
    effects [Console]

fn main() -> Unit
    ! [Console.print]
    bytes = Bytes.fromList([0, 10, 255])
    Console.print(Bytes.toHex(bytes))
"#;
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("main.av");
    std::fs::write(&entry, SOURCE).expect("write dependency fixture");
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .arg("run")
        .arg(&entry)
        .arg("--wasm-gc")
        .output()
        .expect("run aver --wasm-gc with Bytes");
    assert!(
        output.status.success(),
        "Bytes.toHex run failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "000aff\n");
}

#[test]
fn wasm_gc_verify_lowers_builder_inside_bytes_dependency() {
    const SOURCE: &str = r#"module BuilderVerify
    intent =
        "verify String builder lowering in a loaded dependency"
    depends [Bytes]

fn hex() -> String
    Bytes.toHex(Bytes.fromList([0, 10, 255]))

verify hex
    hex() => "000aff"
"#;
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("main.av");
    std::fs::write(&entry, SOURCE).expect("write verify fixture");
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .arg("verify")
        .arg(&entry)
        .arg("--wasm-gc")
        .output()
        .expect("run aver verify --wasm-gc with Bytes");
    assert!(
        output.status.success(),
        "Bytes.toHex verify failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[cfg(feature = "terminal")]
#[test]
fn wasm_gc_terminal_result_family_builds_and_validates() {
    const SOURCE: &str = r#"module M
    intent = "compile every fallible terminal result shape"
    effects [Terminal]

fn exercise() -> Result<Unit, String>
    ! [Terminal.clear, Terminal.disableRawMode, Terminal.enableRawMode, Terminal.flush, Terminal.hideCursor, Terminal.moveTo, Terminal.print, Terminal.readKey, Terminal.resetColor, Terminal.setColor, Terminal.showCursor]
    Terminal.enableRawMode()?
    Terminal.disableRawMode()?
    Terminal.clear()?
    Terminal.moveTo(0, 0)?
    Terminal.print("x")?
    Terminal.setColor("red")?
    Terminal.resetColor()?
    Terminal.hideCursor()?
    Terminal.showCursor()?
    Terminal.flush()?
    _ = Terminal.readKey()?
    Result.Ok(Unit)

fn main() -> Unit
    Unit
"#;

    let mut lexer = aver::lexer::Lexer::new(SOURCE);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = aver::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let errors = &result.typecheck.expect("typecheck").errors;
    assert!(errors.is_empty(), "unexpected type errors: {errors:?}");

    aver::codegen::wasm_gc::compile_to_wasm_gc(&items, result.analysis.as_ref())
        .expect("all Terminal Result factories must form a valid wasm-gc module");
}

// Boxed `match Int.div`/`match Int.mod` Err arms carry the VM's exact message
// strings (`src/types/int.rs`): `"division by zero"` for both. `Int = ℤ`:
// `Int.div`'s `i64::MIN / -1` is NO LONGER an overflow Err — it is the valid
// Big `+2^63` Ok (the slice-2 semantics: there is no i64 overflow over ℤ), so
// that third case renders `ok`. Pin the wasm-gc captured stdout to those
// byte-for-byte so the boxed Result construction can't drift from the VM.
const DIV_MOD_ERR_SRC: &str = r#"module M
    intent =
        "boxed Int.div/mod Err messages"
    effects [Console]

fn de(a: Int, b: Int) -> String
    match Int.div(a, b)
        Result.Ok(_)  -> "ok"
        Result.Err(e) -> e

fn me(a: Int, b: Int) -> String
    match Int.mod(a, b)
        Result.Ok(_)  -> "ok"
        Result.Err(e) -> e

fn main() -> Unit
    ! [Console.print]
    Console.print("{de(5, 0)}|{me(5, 0)}|{de(0 - 9223372036854775807 - 1, 0 - 1)}")
"#;

#[test]
fn wasm_gc_boxed_int_div_mod_err_messages_match_vm() {
    let mut lexer = aver::lexer::Lexer::new(DIV_MOD_ERR_SRC);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = aver::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    // Match the `aver run --wasm-gc` pipeline shape: mutable interpolation /
    // buffer builders stay OFF, while the carrier-free UTF-8 cursor pass is
    // ON; neutral alloc policy, full typecheck.
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: true,
            run_list_build: false,
            ..Default::default()
        },
    );

    let (run_res, stdout, stderr) = aver::services::console::capture_output(|| {
        aver::runtime::wasm_gc::run_in_process(
            &items,
            result.analysis.as_ref(),
            // Single-file program: the default config (no argv, real
            // effects, empty alias map) is exactly the embedding shape.
            aver::runtime::wasm_gc::RunConfig::default(),
        )
    });

    if let Err(e) = &run_res {
        panic!("wasm-gc run_in_process should succeed on boxed Int.div/mod, got: {e}");
    }
    // Byte-for-byte identical to the VM. The first two are `Int.div`/`mod`
    // by zero (still a genuine Err); the third is `Int.div(i64::MIN, -1)`,
    // which over ℤ is the Ok Big `+2^63` → the `Result.Ok` arm renders `ok`.
    assert_eq!(
        stdout,
        b"division by zero|division by zero|ok\n",
        "wasm-gc boxed Int.div/mod Err messages must match the VM verbatim; got {:?}",
        String::from_utf8_lossy(&stdout)
    );
    assert!(
        stderr.is_empty(),
        "stderr should be empty; got {:?}",
        String::from_utf8_lossy(&stderr)
    );
}

/// The `Bits` namespace on wasm-gc, pinned against the VM byte-for-byte.
///
/// wasm-gc is the one backend carrying its OWN bignum (a sign + 32-bit-limb
/// magnitude in hand-written WAT, not `aver-rt::AverInt`), so it is the one
/// place where "same mathematical semantics" is a claim about two separate
/// implementations rather than one shared routine. The cases chosen are the
/// ones a truncating or sign-confused implementation would fail while still
/// looking plausible:
///
///   * `Bits.shiftLeft(1, 100)` — past the 64-bit cliff, so a raw-i64 path
///     answers `0` instead of erroring.
///   * `Bits.not` / `and` / `or` / `xor` over a Big operand — exercises the
///     limb-level two's-complement expansion and the conversion back to
///     sign+magnitude, including a Big-operand result that lands back INSIDE
///     i64 (`and(not(huge), huge) == 0`).
///   * `Bits.shiftRight(-3, 1) == -2` — arithmetic, not logical.
///   * Negative and oversized dynamic counts — the `Result.Err` payload must
///     be the same bytes the VM produces, which means both synthetic string
///     literals have to be interned in the data segment table.
const BITS_SRC: &str = r#"module M
    intent =
        "Bits across the Small/Big seam"
    effects [Console]

fn dynamic(count: Int) -> String
    match Bits.shiftLeft(1, count)
        Result.Ok(v)  -> "ok {v}"
        Result.Err(e) -> e

fn dynamicDown(value: Int, count: Int) -> String
    match Bits.shiftRight(value, count)
        Result.Ok(v)  -> "ok {v}"
        Result.Err(e) -> e

fn dynamicLow(value: Int, width: Int) -> String
    match Bits.low(value, width)
        Result.Ok(v)  -> "ok {v}"
        Result.Err(e) -> e

fn main() -> Unit
    ! [Console.print]
    huge = Bits.shiftLeft(1, 100)
    Console.print("{Bits.and(6, 3)}|{Bits.or(6, 3)}|{Bits.xor(6, 3)}|{Bits.and(-1, 42)}|{Bits.not(-1)}")
    Console.print("{huge}|{Bits.not(huge)}|{Bits.or(huge, 1)}|{Bits.xor(huge, huge)}|{Bits.and(Bits.not(huge), huge)}")
    Console.print("{Bits.shiftRight(-3, 1)}|{Bits.low(-1, 8)}|{Bits.low(123, 0)}|{dynamic(4)}|{dynamic(-1)}|{dynamic(16777217)}|{dynamic(4294967296)}")
    Console.print("{Bits.shiftRight(-3, 9223372036854775808)}|{dynamicDown(-3, 4294967296)}|{dynamicLow(42, 4294967296)}|{dynamicLow(-1, 4294967296)}")
"#;

#[test]
fn wasm_gc_bits_namespace_matches_vm() {
    let mut lexer = aver::lexer::Lexer::new(BITS_SRC);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = aver::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: true,
            run_list_build: false,
            ..Default::default()
        },
    );

    let (run_res, stdout, stderr) = aver::services::console::capture_output(|| {
        aver::runtime::wasm_gc::run_in_process(
            &items,
            result.analysis.as_ref(),
            aver::runtime::wasm_gc::RunConfig::default(),
        )
    });

    if let Err(e) = &run_res {
        panic!("wasm-gc run_in_process should succeed on the Bits program, got: {e}");
    }
    assert_eq!(
        String::from_utf8_lossy(&stdout),
        "2|7|5|42|0\n\
         1267650600228229401496703205376|-1267650600228229401496703205377|\
         1267650600228229401496703205377|0|0\n\
         -2|255|0|ok 16|negative shift count|shift count exceeds the 16777216 bit limit|shift count exceeds the 16777216 bit limit\n\
         -1|ok -1|ok 42|bit width exceeds the 16777216 bit limit\n",
        "wasm-gc Bits results must match the VM verbatim"
    );
    assert!(
        stderr.is_empty(),
        "stderr should be empty; got {:?}",
        String::from_utf8_lossy(&stderr)
    );
}

/// Run `src` on both backends and return `(vm_stdout, wasm_gc_stdout)`.
/// Both go through `capture_output`, so the comparison is on the same bytes
/// the parity harness sees.
fn vm_and_wasm_gc_stdout(src: &str) -> (String, String) {
    let build = || {
        let mut lexer = aver::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = aver::parser::Parser::new(tokens);
        let mut items = parser.parse().expect("parse");
        let result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                alloc_policy: Some(&aver::ir::NeutralAllocPolicy),
                run_interp_lower: false,
                run_buffer_build: true,
                run_chars_fusion: true,
                run_list_build: false,
                ..Default::default()
            },
        );
        (items, result)
    };

    let (items, result) = build();
    let (wasm_res, wasm_out, _) = aver::services::console::capture_output(|| {
        aver::runtime::wasm_gc::run_in_process(
            &items,
            result.analysis.as_ref(),
            aver::runtime::wasm_gc::RunConfig::default(),
        )
    });
    if let Err(e) = &wasm_res {
        panic!("wasm-gc run failed: {e}\n--- source ---\n{src}");
    }

    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("main.av");
    std::fs::write(&entry, src).expect("write entry");
    let vm = std::process::Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&entry)
        .output()
        .expect("run aver");
    assert!(
        vm.status.success(),
        "VM run failed:\n{}",
        String::from_utf8_lossy(&vm.stderr)
    );

    (
        String::from_utf8_lossy(&vm.stdout).into_owned(),
        String::from_utf8_lossy(&wasm_out).into_owned(),
    )
}

/// A dynamic `Bits` call must evaluate each argument EXACTLY ONCE, in source
/// order, on both the `Ok` and the `Err` path.
///
/// The guarded lowering reads the count to test its sign and then needs both
/// operands inside each arm. Emitting them per-arm instead of hoisting them
/// ran the count twice on the `Ok` path and skipped the value entirely on the
/// `Err` path:
///
/// ```text
/// VM       : value, count 1,  ok 6      wasm-gc: count 1, value, count 1, ok 6
/// VM       : value, count -1, err …     wasm-gc: count -1,               err …
/// ```
///
/// Aver's effects are eager, so that is a semantic difference and not a
/// performance detail — the printed trace IS the observation.
#[test]
fn wasm_gc_dynamic_bits_evaluates_each_argument_once_in_source_order() {
    let src = r#"module M
    intent =
        "argument evaluation order for a dynamic Bits call"
    effects [Console]

fn value() -> Int
    ! [Console.print]
    Console.print("value")
    3

fn count(n: Int) -> Int
    ! [Console.print]
    Console.print("count {n}")
    n

fn shifted(n: Int) -> String
    ! [Console.print]
    match Bits.shiftLeft(value(), count(n))
        Result.Ok(v) -> "ok {v}"
        Result.Err(e) -> "err {e}"

fn main() -> Unit
    ! [Console.print]
    Console.print(shifted(1))
    Console.print("--")
    Console.print(shifted(-1))
"#;
    let (vm, wasm) = vm_and_wasm_gc_stdout(src);
    assert_eq!(
        vm, "value\ncount 1\nok 6\n--\nvalue\ncount -1\nerr negative shift count\n",
        "the VM's own evaluation contract changed"
    );
    assert_eq!(
        wasm, vm,
        "wasm-gc must evaluate a dynamic Bits call's arguments exactly once each, \
         in source order, on both the Ok and the Err path"
    );
}

/// A dynamic `Bits` call reachable ONLY through string interpolation must
/// still intern its error message.
///
/// The synthetic-literal registration walked the body with a traversal that
/// bottomed out at a wildcard and never descended into `InterpolatedStr`, so
/// this program type-checked, ran on the VM, and then failed wasm-gc
/// validation with `String literal "negative shift count" was not registered`.
#[test]
fn wasm_gc_registers_bits_error_message_reached_only_through_interpolation() {
    let src = r#"module M
    intent =
        "dynamic Bits inside an interpolation"
    effects [Console]

fn show(n: Int) -> String
    "{Result.withDefault(Bits.shiftLeft(1, n), 0)}"

fn main() -> Unit
    ! [Console.print]
    Console.print(show(3))
"#;
    let (vm, wasm) = vm_and_wasm_gc_stdout(src);
    assert_eq!(vm, "8\n");
    assert_eq!(wasm, vm);
}

/// A `Bits` call reachable ONLY through a map literal must still register the
/// WASM helper. Same root cause as the interpolation case, different missing
/// arm: the helper-reachability walk skipped `MapLiteral`, so this failed with
/// `Bits.and requires the __aint_bitwise helper to be registered`.
#[test]
fn wasm_gc_registers_bits_helper_reached_only_through_a_map_literal() {
    let src = r#"module M
    intent =
        "Bits inside a map literal"
    effects [Console]

fn build(a: Int, b: Int) -> Map<String, Int>
    {"v" => Bits.and(a, b)}

fn probe(a: Int, b: Int) -> Int
    Option.withDefault(Map.get(build(a, b), "v"), -1)

fn main() -> Unit
    ! [Console.print]
    Console.print("{probe(6, 3)}")
"#;
    let (vm, wasm) = vm_and_wasm_gc_stdout(src);
    assert_eq!(vm, "2\n");
    assert_eq!(wasm, vm);
}

/// Boxed `Int.div` / `Int.mod` had the same operand-evaluation defect the
/// `Bits` lowering did, and worse: the zero test reads the divisor twice (its
/// `$magf`, then its `$small`) and the `Ok` arm needs it again, so a divisor
/// with effects ran THREE times.
///
/// ```text
/// VM       value, count -1, ok -3
/// wasm-gc  count -1, count -1, value, count -1, ok -3
/// ```
///
/// This predates `Bits`; it is fixed here because the fix is the same
/// mechanism.
#[test]
fn wasm_gc_boxed_int_div_evaluates_each_argument_once_in_source_order() {
    let src = r#"module M
    intent =
        "argument evaluation order for a boxed Int.div"
    effects [Console]

fn value() -> Int
    ! [Console.print]
    Console.print("value")
    3

fn count(n: Int) -> Int
    ! [Console.print]
    Console.print("count {n}")
    n

fn divided(n: Int) -> String
    ! [Console.print]
    match Int.div(value(), count(n))
        Result.Ok(v) -> "ok {v}"
        Result.Err(e) -> "err {e}"

fn main() -> Unit
    ! [Console.print]
    Console.print(divided(1))
    Console.print("--")
    Console.print(divided(0))
"#;
    let (vm, wasm) = vm_and_wasm_gc_stdout(src);
    assert_eq!(
        vm, "value\ncount 1\nok 3\n--\nvalue\ncount 0\nerr division by zero\n",
        "the VM's own evaluation contract changed"
    );
    assert_eq!(wasm, vm, "wasm-gc boxed Int.div diverged from the VM");
}

/// The FUSED `Result.withDefault(Int.mod(a, b), default)` shape has three
/// operands, and the default is an argument too — so it runs whether or not
/// the divisor turns out to be zero. The unfixed lowering ran the divisor
/// three times and dropped whichever of the value / default the taken arm did
/// not mention.
#[test]
fn wasm_gc_fused_result_with_default_evaluates_all_three_operands_once() {
    let src = r#"module M
    intent =
        "argument evaluation order for the fused Result.withDefault(Int.mod(..))"
    effects [Console]

fn value() -> Int
    ! [Console.print]
    Console.print("value")
    7

fn divisor(n: Int) -> Int
    ! [Console.print]
    Console.print("divisor {n}")
    n

fn fallback() -> Int
    ! [Console.print]
    Console.print("fallback")
    -1

fn run(n: Int) -> Int
    ! [Console.print]
    Result.withDefault(Int.mod(value(), divisor(n)), fallback())

fn main() -> Unit
    ! [Console.print]
    Console.print("{run(3)}")
    Console.print("--")
    Console.print("{run(0)}")
"#;
    let (vm, wasm) = vm_and_wasm_gc_stdout(src);
    assert_eq!(
        vm, "value\ndivisor 3\nfallback\n1\n--\nvalue\ndivisor 0\nfallback\n-1\n",
        "the VM's own evaluation contract changed"
    );
    assert_eq!(wasm, vm, "wasm-gc fused withDefault diverged from the VM");
}

/// The operand scratch slots are per-FUNCTION, not per-call-site, so a
/// guarded call nested inside another guarded call reuses them. Draining the
/// stack after both operands are evaluated is what makes that safe; writing
/// each slot right after its own operand let the inner call overwrite the
/// outer value, silently computing `Bits.shiftLeft(12, 4)` instead of
/// `Bits.shiftLeft(7, 4)`.
#[test]
fn wasm_gc_nested_guarded_calls_do_not_clobber_the_operand_scratch() {
    let src = r#"module M
    intent =
        "a guarded call whose operand is itself a guarded call"
    effects [Console]

fn outer(x: Int, a: Int, w: Int) -> String
    match Bits.shiftLeft(x, Result.withDefault(Bits.low(a, w), 0))
        Result.Ok(v) -> "ok {v}"
        Result.Err(e) -> "err {e}"

fn divNested(x: Int, a: Int, b: Int) -> String
    match Int.div(x, Result.withDefault(Int.div(a, b), 1))
        Result.Ok(v) -> "ok {v}"
        Result.Err(e) -> "err {e}"

fn main() -> Unit
    ! [Console.print]
    Console.print(outer(7, 12, 3))
    Console.print(divNested(100, 40, 4))
"#;
    let (vm, wasm) = vm_and_wasm_gc_stdout(src);
    // Bits.low(12, 3) = 4, so 7 * 2^4 = 112 -- NOT 12 * 2^4 = 192.
    // Int.div(40, 4) = 10, so 100 / 10 = 10.
    assert_eq!(vm, "ok 112\nok 10\n");
    assert_eq!(
        wasm, vm,
        "a nested guarded call must not clobber the enclosing call's operands"
    );
}
