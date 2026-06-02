//! wasm-gc MIR body-emitter coverage + validity gate (single-file).
//!
//! MIR is the only wasm-gc codegen path: the `ResolvedExpr` body walker
//! was retired, so fns the MIR walker doesn't cover (today only
//! higher-order / `MirCallee::LocalSlot` shapes) get an `unreachable`
//! trap-stub body rather than a second walker. This test compiles every
//! single-file `examples/**/*.av` program through the production path and
//! asserts:
//!   * it compiles (no hard error), and the emitted bytes are valid
//!     wasm-gc (the compiler runs `wasmparser` validation before
//!     returning, so a successful compile *is* a validation pass), and
//!   * the MIR body emitter rendered at least `MIN_MIR_EMITTED` fns
//!     across the corpus — a coverage floor that fails CI if a covered
//!     construct silently regresses to the trap stub.
//!
//! The historical A/B byte-differential (MIR-on vs a forced
//! `ResolvedExpr` baseline) is gone with the HIR walker — there is no
//! baseline to diff against. Correctness of the emitted output is gated
//! behaviourally by `wasm_gc_spec` / `wasm_gc_capture_output` (which run
//! the module) and per-scenario size/speed by `aver bench`; both are
//! strictly stronger than byte-identity.

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
            // Exclude only genuinely multi-module files (a non-empty
            // `depends [...]`). An empty `depends []` declares no real
            // dependency, so the file compiles standalone and belongs in
            // the coverage gate.
            let has_real_depends = text.lines().any(|ln| {
                let t = ln.trim_start();
                t.starts_with("depends [") && t.trim_end() != "depends []"
            });
            if !has_real_depends {
                out.push(path);
            }
        }
    }
}

/// Run the same pipeline shape `aver compile --target wasm-gc` uses
/// (skip the VM-only interp_lower / buffer_build passes). Returns the
/// post-pipeline items fed to the compiler.
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
fn mir_body_emitter_compiles_and_validates_every_single_file_example() {
    let files = single_file_examples();
    assert!(
        !files.is_empty(),
        "no single-file examples found under examples/ — did the corpus move?"
    );

    let mut failures: Vec<String> = Vec::new();
    let mut compiled = 0usize;
    let mut total_mir_emitted = 0usize;
    let mut total_bytes = 0usize;

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

        // `compile_to_wasm_gc_with_mir_count` is the production
        // `compile_to_wasm_gc` plus the per-fn MIR-coverage count. A
        // successful return means the emitted bytes passed the backend's
        // built-in `wasmparser` validation (it validates before
        // returning) — so a compile is a validity pass.
        match aver::codegen::wasm_gc::compile_to_wasm_gc_with_mir_count(&items, None) {
            Ok((bytes, mir_emitted)) => {
                total_bytes += bytes.len();
                total_mir_emitted += mir_emitted;
                compiled += 1;
            }
            Err(e) => failures.push(format!("{}: compile: {}", path.display(), e)),
        }
    }

    if !failures.is_empty() {
        panic!(
            "{} of {} single-file examples failed to compile / validate through the MIR path:\n  - {}",
            failures.len(),
            files.len(),
            failures.join("\n  - ")
        );
    }

    // Coverage floor. Keyed off the *real* per-fn MIR dispatch the seam
    // made (the `emit_fn_body_via_mir` Some/None decision), not the
    // structural coverage predicate — an over-conservative predicate
    // can't make it pass vacuously. A drop below it (a covered construct
    // silently regressing to a trap stub) fails CI rather than passing
    // quietly. Raise `MIN_MIR_EMITTED` when new coverage lands; never
    // lower it without a deliberate reason.
    const MIN_MIR_EMITTED: usize = 734;
    assert!(
        total_mir_emitted >= MIN_MIR_EMITTED,
        "MIR body emitter rendered {total_mir_emitted} fns across {compiled} examples, \
         below the floor of {MIN_MIR_EMITTED} — MIR coverage regressed (a covered construct \
         fell back to the trap stub). If this drop is intentional, lower the floor."
    );

    eprintln!(
        "mir_body_emitter_compiles_and_validates_every_single_file_example: {compiled} examples \
         compiled + validated; MIR body emitter rendered {total_mir_emitted} fns; \
         {total_bytes} B total"
    );
}
