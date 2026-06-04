//! wasip2 codegen regression suite.
//!
//! Sister to `tests/wasm_gc_codegen_regression.rs`. For every
//! single-file `examples/**/*.av` program (no `depends [...]`,
//! no `examples/diagnostics/` typecheck-broken examples, no
//! `oracle_independent_products.av` `BranchPath` carrier — same
//! exclusions as the wasm-gc regression), drive the full frontend
//! → IR pipeline → `compile_to_wasm_gc_for_wasip2` →
//! `compile_to_component` → component-model validation.
//!
//! Catches regressions in:
//!   - the wasm-gc emitter's `TargetMode::Wasip2` arm (canonical
//!     ABI shape, distinct from the bridge target wasm-gc uses)
//!   - the wit-component wrap pass that adapts the core module
//!     into a `wasi:cli/command` component
//!   - the wasi-package bundle / WIT generator output
//!
//! Skips programs the wasm-gc regression already skips; if a
//! program's bridge-mode codegen panics the wasip2 mode won't get
//! a chance to fail differently, so the two suites share a corpus.

#![cfg(feature = "wasip2")]

use std::fs;
use std::path::{Path, PathBuf};

use aver::ast::TopLevel;
use aver::ir;
use aver::lexer::Lexer;
use aver::parser::Parser;

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples")
}

/// Paths skipped from the regression walk:
///   * `examples/diagnostics/` — same exclusions as
///     `tests/wasm_gc_codegen_regression.rs`.
///   * Three `Terminal.*` users — `status_board.av`,
///     `oracle_trace.av`, `terminal_size_snapshot.av`. The wasip2
///     backend lowered 10 effects in 0.18 "Span" (all of `Disk`,
///     `Time.sleep`, `Console.readLine`) but `Terminal.moveTo` /
///     `Terminal.readKey` / `Terminal.size` are not yet wired —
///     codegen errors with "no helper registered, no effect
///     import, no inline lowering". Tracked as a follow-up; this
///     suite is the regression net, the wiring is its own ticket.
///
/// `oracle_independent_products.av` used to be skipped (its
/// higher-order `Fn(BranchPath, …)` spec param was mis-discovered as a
/// phantom `Tuple<BranchPath, …>` carrier that the eq helper couldn't
/// dispatch). That discovery bug is fixed, so it rides the walk like
/// every other single-file example.
fn is_skipped(path: &Path) -> bool {
    let s = path.to_string_lossy();
    s.contains("/examples/diagnostics/")
        || s.ends_with("/examples/apps/status_board.av")
        || s.ends_with("/examples/formal/oracle_trace.av")
        || s.ends_with("/examples/formal/terminal_size_snapshot.av")
}

fn single_file_examples() -> Vec<PathBuf> {
    let mut out = Vec::new();
    walk(&examples_dir(), &mut out);
    out.sort();
    out
}

fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(read) = fs::read_dir(dir) else { return };
    for entry in read.flatten() {
        let path = entry.path();
        if path.is_dir() {
            walk(&path, out);
        } else if path.extension().and_then(|s| s.to_str()) == Some("av") && !is_skipped(&path) {
            let Ok(text) = fs::read_to_string(&path) else {
                continue;
            };
            if !text
                .lines()
                .any(|ln| ln.trim_start().starts_with("depends ["))
            {
                out.push(path);
            }
        }
    }
}

fn parse_pipeline(source: &str) -> Result<Vec<TopLevel>, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| format!("lex: {:?}", e))?;
    let mut parser = Parser::new(tokens);
    let mut items = parser.parse().map_err(|e| format!("parse: {:?}", e))?;
    // Mirror what `aver compile --target wasm-gc` does internally:
    // skip the VM-specific `run_interp_lower` + `run_buffer_build`
    // passes (they emit `__buf_*` / `__interp_*` calls the wasm-gc
    // codegen doesn't link against). The wasip2 wrap pass sits on
    // top of the same wasm-gc emitter, so this matches.
    let result = ir::pipeline::run(
        &mut items,
        ir::PipelineConfig {
            typecheck: Some(ir::TypecheckMode::Full { base_dir: None }),
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        return Err(format!(
            "typecheck: {} error(s) — first: {:?}",
            tc.errors.len(),
            tc.errors.first()
        ));
    }
    Ok(items)
}

#[test]
fn wasip2_codegen_emits_valid_component_for_every_single_file_example() {
    let files = single_file_examples();
    assert!(
        !files.is_empty(),
        "no single-file examples found under examples/ — did the corpus move?"
    );

    let mut failures: Vec<String> = Vec::new();
    let mut compiled = 0usize;

    for path in &files {
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                failures.push(format!("{}: read: {}", path.display(), e));
                continue;
            }
        };
        let items = match parse_pipeline(&source) {
            Ok(i) => i,
            Err(e) => {
                failures.push(format!("{}: {}", path.display(), e));
                continue;
            }
        };
        let core_bytes = match aver::codegen::wasm_gc::compile_to_wasm_gc_for_wasip2(&items, None) {
            Ok(b) => b,
            Err(e) => {
                failures.push(format!(
                    "{}: compile_to_wasm_gc_for_wasip2: {}",
                    path.display(),
                    e
                ));
                continue;
            }
        };
        let component_bytes = match aver::codegen::wasip2::compile_to_component(
            &core_bytes,
            aver::codegen::wasip2::Wasip2World::CliCommand,
        ) {
            Ok((bytes, _wit)) => bytes,
            Err(e) => {
                failures.push(format!("{}: compile_to_component: {}", path.display(), e));
                continue;
            }
        };
        // Component validation needs the component-model feature
        // explicitly enabled — `WasmFeatures::default()` enables it,
        // but spell it out so a future wasmparser default flip
        // doesn't silently turn this into a core-module check.
        let mut validator =
            wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::default());
        if let Err(e) = validator.validate_all(&component_bytes) {
            failures.push(format!(
                "{}: component validate ({} bytes): {}",
                path.display(),
                component_bytes.len(),
                e
            ));
            continue;
        }
        compiled += 1;
    }

    if !failures.is_empty() {
        panic!(
            "{} of {} single-file examples failed wasip2 codegen + component validate:\n  - {}",
            failures.len(),
            files.len(),
            failures.join("\n  - ")
        );
    }
    eprintln!(
        "wasip2_codegen_emits_valid_component_for_every_single_file_example: {} files compiled + validated",
        compiled
    );
}
