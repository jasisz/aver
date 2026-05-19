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
            aver::runtime::wasm_gc::RunConfig {
                program_args: Vec::new(),
                entry_info: None,
                mode: aver::runtime::wasm_gc::EffectMode::Normal,
            },
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
