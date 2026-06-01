//! wasm-gc MIR body-emitter byte-differential (Phase 5 #340 wave 0).
//!
//! For every single-file `examples/**/*.av` program, compile it twice
//! through `compile_to_wasm_gc_mir_toggle`:
//!   * `enable_mir = false` — the `ResolvedExpr` body emitter everywhere.
//!   * `enable_mir = true`  — the MIR body emitter per-fn, with a
//!     `ResolvedExpr` fallback for anything the MIR walk doesn't cover.
//!
//! and assert the emitted module bytes are **identical**. Each build
//! reconstructs the same type registry, fn-map, slot table, and data
//! segments from the same resolved program — only the per-fn body walk
//! differs — so any covered fn whose MIR emission diverges by a single
//! byte trips this test before it can reach the corpus / game suite.
//! (Whole-module byte-identity across two independent builds is only
//! meaningful because the type registry's carrier-slot ordering is
//! deterministic; this test therefore also guards that determinism.)
//! This is the safety net the port leans on: it lets each wave widen
//! coverage knowing a divergence is caught mechanically, not by
//! eyeballing wasm.
//!
//! A second check proves the MIR walk actually fires somewhere in the
//! corpus (otherwise byte-identity would hold vacuously with zero MIR
//! coverage): `compile_to_wasm_gc_mir_toggle` returns how many fns the
//! body emitter *actually rendered* from MIR — the real
//! `emit_fn_body_via_mir` Some/None decision the seam fixed, not the
//! structural `coverage_report` predicate — and the test asserts that
//! count is non-zero. Keying the canary off the emitter rather than the
//! predicate means an over-conservative predicate can't make this pass
//! vacuously.

#![cfg(feature = "wasm-compile")]

use std::fs;
use std::path::{Path, PathBuf};

use aver::ast::TopLevel;
use aver::ir;
use aver::lexer::Lexer;
use aver::parser::Parser;

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples")
}

fn single_file_examples() -> Vec<PathBuf> {
    let mut out = Vec::new();
    walk(&examples_dir(), &mut out);
    out.sort();
    out
}

fn is_skipped(path: &Path) -> bool {
    let s = path.to_string_lossy();
    s.contains("/examples/diagnostics/")
        || s.ends_with("/examples/formal/oracle_independent_products.av")
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

/// Run the same pipeline shape `aver compile --target wasm-gc` uses
/// (skip the VM-only interp_lower / buffer_build passes). Returns both
/// the post-pipeline items fed to the compiler.
fn run_pipeline(source: &str) -> Result<Vec<TopLevel>, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| format!("lex: {:?}", e))?;
    let mut parser = Parser::new(tokens);
    let mut items = parser.parse().map_err(|e| format!("parse: {:?}", e))?;
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
fn mir_and_resolved_body_emitters_agree_byte_for_byte() {
    let files = single_file_examples();
    assert!(
        !files.is_empty(),
        "no single-file examples found under examples/ — did the corpus move?"
    );

    let mut failures: Vec<String> = Vec::new();
    let mut compared = 0usize;
    let mut total_mir_emitted = 0usize;

    for path in &files {
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                failures.push(format!("{}: read: {}", path.display(), e));
                continue;
            }
        };
        let items = match run_pipeline(&source) {
            Ok(v) => v,
            Err(e) => {
                failures.push(format!("{}: {}", path.display(), e));
                continue;
            }
        };

        let (via_resolved, _) =
            match aver::codegen::wasm_gc::compile_to_wasm_gc_mir_toggle(&items, None, false) {
                Ok(b) => b,
                Err(e) => {
                    failures.push(format!("{}: compile (mir off): {}", path.display(), e));
                    continue;
                }
            };
        let (via_mir, mir_emitted) =
            match aver::codegen::wasm_gc::compile_to_wasm_gc_mir_toggle(&items, None, true) {
                Ok(b) => b,
                Err(e) => {
                    failures.push(format!("{}: compile (mir on): {}", path.display(), e));
                    continue;
                }
            };

        if via_resolved != via_mir {
            let first_diff = via_resolved
                .iter()
                .zip(via_mir.iter())
                .position(|(a, b)| a != b);
            failures.push(format!(
                "{}: MIR vs ResolvedExpr emit diverged — {} vs {} bytes, first diff at offset {:?}",
                path.display(),
                via_resolved.len(),
                via_mir.len(),
                first_diff,
            ));
            continue;
        }

        total_mir_emitted += mir_emitted;
        compared += 1;
    }

    if !failures.is_empty() {
        panic!(
            "{} of {} single-file examples diverged between the MIR and ResolvedExpr \
             wasm-gc body emitters:\n  - {}",
            failures.len(),
            files.len(),
            failures.join("\n  - ")
        );
    }

    // Canary: byte-identity is meaningless if the MIR body emitter never
    // fired. This counts the *real* per-fn MIR dispatch the seam made
    // (the `emit_fn_body_via_mir` Some/None decision), not the structural
    // coverage predicate — so an over-conservative predicate can't make
    // it pass vacuously. Wave 0 covers Literal / Local / numeric BinOp /
    // Neg / Return / named Let, so the corpus must surface at least one.
    assert!(
        total_mir_emitted > 0,
        "MIR body emitter rendered 0 fns across {compared} examples — byte-identity is vacuous; \
         the wave-0 emitter or the seam dispatch regressed"
    );

    eprintln!(
        "mir_and_resolved_body_emitters_agree_byte_for_byte: {compared} examples byte-identical; \
         MIR body emitter rendered {total_mir_emitted} fns"
    );
}
