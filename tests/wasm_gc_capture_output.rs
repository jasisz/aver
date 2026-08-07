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
    // Match the `aver run --wasm-gc` pipeline shape: interp_lower /
    // buffer_build are OFF (those fuse string interpolation into buffer
    // intrinsics the wasm-gc backend doesn't lower, which would trap),
    // neutral alloc policy, full typecheck.
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
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
