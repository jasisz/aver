//! Gate every AFL hang reproducer for the verify runner.
//!
//! Before Iron 0.21's step-limit landed in `vm_verify.rs`, each of
//! these `.av` files used to pin the host indefinitely — a
//! byte-havoc mutation of `math.av` / `comments.av` corpus seeds
//! that dropped the terminating body of a user fn, leaving a
//! self-recursive call. TCO turned the recursion into a goto-loop
//! with no stack growth, so `VM::execute_until` ran forever.
//!
//! With the step limit installed, each case bails as a clean
//! `RuntimeError("VM step limit exceeded ...")` after ~50-200ms
//! instead of hanging. The test asserts: (a) verify returns
//! without panicking, (b) the whole pipeline finishes well inside
//! a 30s wall-clock — well above the ~10M-step budget but well
//! below "the case is hanging" territory.

use std::fs;
use std::panic;
use std::path::PathBuf;
use std::time::{Duration, Instant};

use aver::diagnostics::vm_verify::run_verify_for_items_vm;
use aver::ir;
use aver::lexer::Lexer;
use aver::parser::Parser;

#[test]
fn verify_runner_regressions_do_not_hang() {
    let dir =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/regressions/verify_runner");
    if !dir.exists() {
        return;
    }
    let mut tested = 0usize;
    for entry in fs::read_dir(&dir).expect("read regressions dir") {
        let path = entry.expect("dir entry").path();
        if path.extension().and_then(|s| s.to_str()) != Some("av") {
            continue;
        }
        let bytes = fs::read(&path).expect("read regression");
        let source = std::str::from_utf8(&bytes).expect("regression file must be UTF-8");
        let display = path.display().to_string();

        let started = Instant::now();
        let result = panic::catch_unwind(|| {
            let mut lexer = Lexer::new(source);
            let tokens = match lexer.tokenize() {
                Ok(t) => t,
                Err(_) => return,
            };
            let mut parser = Parser::new(tokens);
            let mut items = match parser.parse() {
                Ok(i) => i,
                Err(_) => return,
            };
            let _ = ir::pipeline::run(
                &mut items,
                ir::PipelineConfig {
                    typecheck: Some(ir::TypecheckMode::Full { base_dir: None }),
                    ..Default::default()
                },
            );
            let _ = run_verify_for_items_vm(items, None, None, "regression.av");
        });
        let elapsed = started.elapsed();

        assert!(
            result.is_ok(),
            "verify_runner regression panicked on {}",
            display
        );
        assert!(
            elapsed < Duration::from_secs(30),
            "verify_runner regression took {} ms on {} — step limit may not be installed",
            elapsed.as_millis(),
            display
        );
        tested += 1;
    }
    eprintln!("verify_runner_regressions_do_not_hang: {} files", tested);
}

/// Mirror of the VM regression test for the wasm-gc verify backend.
/// `aver verify --wasm-gc` and `aver::diagnostics::wasm_gc_verify`
/// install a wasmtime fuel cap analogous to the VM's `step_limit`;
/// the same fixtures must terminate here too. Gated on `wasm`
/// because the lib's wasm-gc verify path is `#[cfg(feature =
/// "wasm")]`.
#[cfg(feature = "wasm")]
#[test]
fn verify_runner_regressions_do_not_hang_wasm_gc() {
    use aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc;

    let dir =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/regressions/verify_runner");
    if !dir.exists() {
        return;
    }
    let mut tested = 0usize;
    for entry in fs::read_dir(&dir).expect("read regressions dir") {
        let path = entry.expect("dir entry").path();
        if path.extension().and_then(|s| s.to_str()) != Some("av") {
            continue;
        }
        let bytes = fs::read(&path).expect("read regression");
        let source = std::str::from_utf8(&bytes).expect("regression file must be UTF-8");
        let display = path.display().to_string();

        let started = Instant::now();
        let result = panic::catch_unwind(|| {
            let mut lexer = Lexer::new(source);
            let tokens = match lexer.tokenize() {
                Ok(t) => t,
                Err(_) => return,
            };
            let mut parser = Parser::new(tokens);
            let mut items = match parser.parse() {
                Ok(i) => i,
                Err(_) => return,
            };
            let _ = ir::pipeline::run(
                &mut items,
                ir::PipelineConfig {
                    typecheck: Some(ir::TypecheckMode::Full { base_dir: None }),
                    ..Default::default()
                },
            );
            let _ = run_verify_for_items_wasm_gc(items, None, None, "regression.av");
        });
        let elapsed = started.elapsed();

        assert!(
            result.is_ok(),
            "wasm-gc verify regression panicked on {}",
            display
        );
        assert!(
            elapsed < Duration::from_secs(30),
            "wasm-gc verify regression took {} ms on {} — fuel cap may not be installed",
            elapsed.as_millis(),
            display
        );
        tested += 1;
    }
    eprintln!(
        "verify_runner_regressions_do_not_hang_wasm_gc: {} files",
        tested
    );
}
