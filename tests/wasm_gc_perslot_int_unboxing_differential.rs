//! ETAP-2 SLICE 2b — per-slot Int unboxing on the wasm-gc backend.
//!
//! These are VM-vs-wasm-gc DIFFERENTIALS: the VM keeps full-precision
//! `$aint` Int semantics; wasm-gc, after slice 2b, lowers a provably-bounded
//! `Int` slot/param/return to a native `i64`. A wrongly-bare slot that
//! overflows `i64` would diverge — the VM correct, wasm-gc silently wrapped
//! (there is no overflow trip-wire on wasm-gc `i64.*`). Identical output is
//! the soundness gate.
//!
//! The cases drive a BARE counter near `i64::MAX` while staying in range
//! (must stay identical) and exercise the bare-param / bare-return /
//! mixed-`acc * n` boundaries (countdown / factorial). The CHECKED-`Unbox`
//! trap (an out-of-i64 value narrowing) is load-bearing — see the
//! `wasm_gc_effect_arg_overflow_regression` suite for the effect-boundary
//! trap, and the slice 2b report for the manual revert-test (swap
//! `__aint_to_i64_checked` → `_sat` at an `Unbox` and watch a differential
//! diverge).

#![cfg(feature = "wasm")]

use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    std::fs::write(&path, source).expect("write temp module source");
    path
}

fn cleanup(path: &std::path::Path) {
    let _ = std::fs::remove_dir_all(path.parent().expect("temp module has parent"));
}

fn run(prefix: &str, source: &str, wasm_gc: bool) -> (bool, String) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root).arg("run").arg(&path);
    if wasm_gc {
        cmd.arg("--wasm-gc");
    }
    let out = cmd.output().expect("aver run executes");
    cleanup(&path);
    (
        out.status.success(),
        String::from_utf8_lossy(&out.stdout).trim().to_string(),
    )
}

/// The two backends must agree on stdout for a program that drives a bare
/// counter. A divergence means a wrong-bare slot wrapped on wasm-gc.
fn assert_vm_wasm_identical(prefix: &str, source: &str) -> String {
    let (vm_ok, vm_out) = run(prefix, source, false);
    let (wg_ok, wg_out) = run(prefix, source, true);
    assert!(vm_ok, "{prefix}: VM run failed");
    assert!(wg_ok, "{prefix}: wasm-gc run failed");
    assert_eq!(
        vm_out, wg_out,
        "{prefix}: VM-vs-wasm-gc DIVERGENCE — a bare i64 slot wrapped where the VM kept \
         full precision.\n  VM     = {vm_out:?}\n  wasm-gc= {wg_out:?}"
    );
    vm_out
}

/// Compile `source` to a wasm-gc module and assert it VALIDATES (the codegen
/// emits valid wasm and the embedded validator accepts it). Returns the
/// combined stdout+stderr so a failure surfaces the validator message. This
/// is the DETERMINISTIC gate for programs that can't be run without external
/// infrastructure (an effectful fn whose body calls `Tcp.*`): the bug is a
/// COMPILE-TIME wasm validation error, so a clean compile is the assertion.
fn compile_to_wasm_gc_validates(prefix: &str, source: &str) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out_dir = path.parent().expect("temp module has parent").join("out");
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile executes");
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    cleanup(&path);
    assert!(
        out.status.success(),
        "{prefix}: wasm-gc compile FAILED (expected a clean, VALIDATING module).\n\
         A `type mismatch: expected i64, found (ref null $type)` here means a carrier-match \
         (`Result`/`Option`/etc.) arm body in a `bare_return` fn rendered boxed where the \
         `i64` block type was declared.\n  stdout = {stdout}\n  stderr = {stderr}"
    );
}

/// `countdown` — a bare param + bare return, pure raw arithmetic toward a
/// bare return. The signature is `(param i64) (result i64)` on wasm-gc; the
/// VM keeps `$aint`. The terminal value (0) must match.
#[test]
fn countdown_bare_counter_matches_vm() {
    let src = r#"module M
    intent = "countdown bare counter"
    effects [Console]

fn countdown(n: Int) -> Int
    match n
        0 -> 0
        _ -> countdown(n - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print("{countdown(50000)}")
"#;
    assert_eq!(assert_vm_wasm_identical("countdown-bare", src), "0");
}

/// `factorial` — the mixed `acc * n` boundary: `n` bare, `acc` boxed. The
/// boxed multiply takes a `Box(n)` the rewrite inserted. The full-precision
/// product (20! overflows i64, so `acc` MUST stay boxed `$aint`) must match
/// the VM exactly — this is the case that proves `acc` did NOT go bare.
#[test]
fn factorial_mixed_boundary_matches_vm() {
    let src = r#"module M
    intent = "factorial mixed bare/boxed"
    effects [Console]

fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)

fn main() -> Unit
    ! [Console.print]
    Console.print("{factorial(25, 1)}")
"#;
    // 25! = 15511210043330985984000000 — far past i64::MAX, so `acc` is a
    // Big on BOTH backends; any i64-wrap on `acc` would diverge here.
    assert_eq!(
        assert_vm_wasm_identical("factorial-mixed", src),
        "15511210043330985984000000"
    );
}

/// A bare counter driven so its arithmetic lands JUST under `i64::MAX` and
/// stays in range. The recurrence `sumTo(n)` over a bounded `n` accumulates
/// a boxed `$aint` total (the sum escapes into the boxed return), while the
/// bare `n` counter does its `n - 1` in raw `i64`. The total for n=4000000
/// is `4000000 * 4000001 / 2 = 8000002000000`, well inside i64, and must
/// match bit-for-bit.
#[test]
fn near_max_in_range_counter_matches_vm() {
    let src = r#"module M
    intent = "bare counter accumulating a large in-range total"
    effects [Console]

fn sumTo(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> sumTo(n - 1, acc + n)

fn main() -> Unit
    ! [Console.print]
    Console.print("{sumTo(4000000, 0)}")
"#;
    assert_eq!(
        assert_vm_wasm_identical("sumto-near-max", src),
        "8000002000000"
    );
}

/// A bare counter whose intermediate arithmetic would overflow `i64` IF it
/// went bare — but the analysis must keep the accumulator boxed (it escapes
/// to the boxed return and its interval is unbounded), so the full-precision
/// sum is computed on both backends. `2^62 + 2^62 = 2^63` overflows i64; the
/// VM and wasm-gc must BOTH report `9223372036854775808` (the boxed `$aint`
/// result), proving the accumulator did not silently wrap on wasm-gc.
#[test]
fn boxed_accumulator_crossing_i64_max_matches_vm() {
    let src = r#"module M
    intent = "boxed accumulator crosses i64::MAX exactly"
    effects [Console]

fn addBig(a: Int, b: Int) -> Int
    a + b

fn main() -> Unit
    ! [Console.print]
    half = 4611686018427387904
    Console.print("{addBig(half, half)}")
"#;
    // 2^62 + 2^62 = 2^63 = 9223372036854775808 (i64::MAX + 1). A bare i64
    // add would WRAP to -9223372036854775808; the boxed `$aint` add must
    // produce the correct positive Big on both backends.
    assert_eq!(
        assert_vm_wasm_identical("boxed-cross-max", src),
        "9223372036854775808"
    );
}

/// Boundary-completeness hole: a BARE-RETURNING call (`g`) used as a `match`
/// arm BODY inside a BOXED value context (the `let x = match …` whose `x` is
/// boxed). The arm body renders raw `i64` while the `match` block is typed
/// `$AverInt`, so without the boxed-branch fix wasm-gc rejected the module
/// with `type mismatch: expected (ref null $type), found i64`. The fix boxes
/// the arm result at the boundary; the VM (which never went bare) is the
/// oracle. A REVERT of the `rewrite_boxed` / `rewrite_boxed_tail`
/// boxed-branch routing makes the wasm-gc run FAIL validation (not diverge),
/// so `assert_vm_wasm_identical` panics on `wg_ok`.
#[test]
fn bare_return_call_in_boxed_match_arm_matches_vm() {
    let src = r#"module M
    intent = "bare-returning call as a match-arm body in a boxed let-value"
    effects [Console]

fn g(n: Int) -> Int
    match n
        0 -> 0
        _ -> g(n - 1)

fn choose(flag: Bool) -> Int
    x = match flag
        true -> g(2)
        false -> 0
    x + 1

fn main() -> Unit
    ! [Console.print]
    Console.print("r={choose(true)}")
"#;
    // g(2) counts down to 0, so x = 0 and x + 1 = 1.
    assert_eq!(assert_vm_wasm_identical("bare-boxed-match-arm", src), "r=1");
}

/// Same hole through the lowered `IfThenElse` path: a boolean `match`
/// (`true`/`false`) lowers to `IfThenElse`, here in a BOXED non-tail
/// let-value, with a bare-returning call (`g`) in one branch. The fix routes
/// the boxed-context `IfThenElse` branch bodies through the boxing path too,
/// so the branch result is boxed before the join.
#[test]
fn bare_return_call_in_boxed_if_branch_matches_vm() {
    let src = r#"module M
    intent = "bare-returning call in a boolean-match (IfThenElse) branch, boxed let-value"
    effects [Console]

fn g(n: Int) -> Int
    match n
        0 -> 0
        _ -> g(n - 1)

fn caller(flag: Bool) -> Int
    y = match flag
        true -> g(4)
        false -> 9
    y + 100

fn main() -> Unit
    ! [Console.print]
    Console.print("r={caller(true)}")
"#;
    // g(4) counts down to 0, so y = 0 and y + 100 = 100.
    assert_eq!(
        assert_vm_wasm_identical("bare-boxed-if-branch", src),
        "r=100"
    );
}

/// The bare-returning-call arm feeding a Map VALUE (an aggregate field is a
/// boxed context). `v` is a boxed `let` whose value is the boxed match; the
/// fixed rewrite boxes the bare-returning-call arm so the Map stores an
/// `$AverInt`. The whole-program flow stays VM-identical.
#[test]
fn bare_return_call_in_boxed_match_into_map_matches_vm() {
    let src = r#"module M
    intent = "bare-return match arm feeding a Map value"
    effects [Console]

fn g(n: Int) -> Int
    match n
        0 -> 5
        _ -> g(n - 1)

fn build(flag: Bool) -> Map<String, Int>
    v = match flag
        true -> g(2)
        false -> 0
    {"k" => v}

fn readit(flag: Bool) -> Int
    m = build(flag)
    Option.withDefault(Map.get(m, "k"), -1)

fn main() -> Unit
    ! [Console.print]
    Console.print("r={readit(true)}")
"#;
    // g(2) counts down to 5 (base case), so the stored value is 5.
    assert_eq!(assert_vm_wasm_identical("bare-boxed-match-map", src), "r=5");
}

// ── Carrier-match arm body in a `bare_return` fn (the effectful-path hole) ──
//
// These cover the DUAL of the Q2 boxed-branch hole above: a `Result` /
// `Option` / `List` / variant MATCH whose arm body is a bare-`i64` value (an
// `Int` literal or a bare-returning call) inside a fn whose RETURN is bare.
// The match block type is declared `i64`, but the carrier-match emit helpers
// (`emit_mir_result_match` et al.) did not re-set the raw colour before each
// arm body, so the literal took the boxed `__aint_from_i64` path and pushed
// an `$AverInt` ref where the block expects `i64` — a wasm VALIDATION error
// (`type mismatch: expected i64, found (ref null $type)`). The fix re-sets
// the colour for every carrier-match arm body, derived from the already-
// computed block type (`block_ty_is_raw_i64`). REVERT-TEST: drop the
// `int_result_raw.set(...)` lines in `emit_mir_result_match` and these
// validations FAIL (the `assert_vm_wasm_identical` ones panic on `wg_ok`).

/// The minimal EFFECTFUL trigger (the `wasip2_tcp_stress` failure isolated):
/// an effectful fn (`doOnce`) whose `Result`-match arm bodies are bounded
/// `Int` literals, so the analysis marks it `bare_return`. The match subject
/// is the `Tcp.close` effect call. Compile-time wasm validation is the gate
/// — running needs a TCP peer, but the bug is purely in codegen, surfaced at
/// compile. `doConnect` returns the bare-returning call in a match arm.
#[test]
fn effectful_result_match_bare_return_compiles_clean() {
    let src = r#"module M
    intent = "effectful Result-match bare-return on the wasm-gc path"
    depends []
    effects [Tcp.connect, Tcp.close, Console.print]
fn doOnce(c: Tcp.Connection) -> Int
    ! [Tcp.close]
    match Tcp.close(c)
        Result.Ok(_) -> 1
        Result.Err(_) -> 0
fn doConnect() -> Int
    ! [Tcp.connect, Tcp.close]
    match Tcp.connect("127.0.0.1", 8080)
        Result.Ok(c) -> doOnce(c)
        Result.Err(_) -> 0
fn main() -> Unit
    ! [Tcp.connect, Tcp.close, Console.print]
    Console.print("r={doConnect()}")
"#;
    compile_to_wasm_gc_validates("effectful-result-bare-return", src);
}

/// A PURE mirror of the same shape (server-free, so it both VALIDATES and
/// RUNS): `doOne` returns a bounded `Int` literal from a `Result`-match, so
/// it is `bare_return`; the VM keeps full precision and is the oracle. This
/// is the deterministic VM-vs-wasm-gc differential for the carrier-match
/// raw-colour fix — a REVERT makes the wasm-gc run FAIL validation (the
/// `assert_vm_wasm_identical` panics on `wg_ok`).
#[test]
fn result_match_bare_return_literal_matches_vm() {
    let src = r#"module M
    intent = "pure Result-match bare-return literal"
    effects [Console]

fn classify(n: Int) -> Result<Int, String>
    match n
        0 -> Result.Err("zero")
        _ -> Result.Ok(n)

fn doOne(n: Int) -> Int
    match classify(n)
        Result.Ok(_) -> 1
        Result.Err(_) -> 0

fn main() -> Unit
    ! [Console.print]
    Console.print("r={doOne(5)}{doOne(0)}")
"#;
    // classify(5)=Ok → 1, classify(0)=Err → 0, so "10".
    assert_eq!(
        assert_vm_wasm_identical("result-bare-return-literal", src),
        "r=10"
    );
}

/// The `acc + f()` boundary: a bare-returning call (`doOne`) used as an
/// arithmetic operand inside a `Result`-match arm, where the enclosing fn's
/// return is BOXED (the `acc + …` escapes). The arm body renders the
/// bare-returning call raw, then `acc + …` boxes it — both backends must
/// agree on the full-precision sum.
#[test]
fn bare_return_call_in_result_arm_arithmetic_matches_vm() {
    let src = r#"module M
    intent = "bare-return call in acc + f() inside a Result-match arm"
    effects [Console]

fn classify(n: Int) -> Result<Int, String>
    match n
        0 -> Result.Err("zero")
        _ -> Result.Ok(n)

fn doOne(n: Int) -> Int
    match classify(n)
        Result.Ok(_) -> 7
        Result.Err(_) -> 3

fn accumulate(acc: Int, n: Int) -> Int
    match classify(n)
        Result.Ok(_) -> acc + doOne(n)
        Result.Err(_) -> acc

fn main() -> Unit
    ! [Console.print]
    Console.print("a={accumulate(100, 5)} b={accumulate(100, 0)}")
"#;
    // classify(5)=Ok → 100 + doOne(5)=100+7=107; classify(0)=Err → 100.
    assert_eq!(
        assert_vm_wasm_identical("result-bare-return-arith", src),
        "a=107 b=100"
    );
}

/// A DEEPER chain of `Result`-match bare-return calls: `top → mid → leaf`,
/// every fn `bare_return` with a `Result`-match whose Ok arm calls the next.
/// Exercises the carrier-match raw-colour propagation across nested
/// bare-return seams.
#[test]
fn deep_chain_result_match_bare_return_matches_vm() {
    let src = r#"module M
    intent = "deep chain of Result-match bare-return calls"
    effects [Console]

fn classify(n: Int) -> Result<Int, String>
    match n
        0 -> Result.Err("zero")
        _ -> Result.Ok(n)

fn leaf(n: Int) -> Int
    match classify(n)
        Result.Ok(_) -> 1
        Result.Err(_) -> 0

fn mid(n: Int) -> Int
    match classify(n)
        Result.Ok(_) -> leaf(n)
        Result.Err(_) -> 0

fn top(n: Int) -> Int
    match classify(n)
        Result.Ok(_) -> mid(n)
        Result.Err(_) -> 0

fn main() -> Unit
    ! [Console.print]
    Console.print("x={top(5)} y={top(0)}")
"#;
    // top(5) → mid(5) → leaf(5) → 1; top(0)=Err → 0.
    assert_eq!(
        assert_vm_wasm_identical("result-bare-return-chain", src),
        "x=1 y=0"
    );
}
