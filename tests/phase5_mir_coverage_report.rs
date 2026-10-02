//! Phase 5 #252 — MIR-to-Rust walker coverage diagnostic.
//!
//! End-to-end reach metric: lower a small Aver program through
//! the standard pipeline, ask `codegen::rust::coverage_report`
//! how many fns the walker can emit standalone, assert sanity
//! bounds. The point is to prove the diagnostic API works on a
//! real lowered program — not to pin a specific ratio (which
//! would force re-rolling the test on every walker widening).

use std::collections::HashSet;

use aver::codegen::rust::{CoverageReport, MirEmitCtx, coverage_report};
use aver::ir::SymbolTable;
use aver::ir::mir::lower_program;
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::source::parse_source;

fn lower_and_report(src: &str) -> CoverageReport {
    let mut items = parse_source(src).expect("parse");
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck requested");
    assert!(tc.errors.is_empty(), "typecheck failed: {:?}", tc.errors);
    let program = lower_program(&result.resolved_items);
    let symbol_table = SymbolTable::build(&items, &[]);
    let prefixes = HashSet::new();
    let emit_ctx = MirEmitCtx::for_test(&symbol_table, &prefixes);
    coverage_report(&program, &emit_ctx)
}

#[test]
fn empty_program_reports_zero_total() {
    // No fns — totals are zero, ratio is zero (not NaN).
    let report = lower_and_report("");
    assert_eq!(report.total, 0);
    assert_eq!(report.mir_covered, 0);
    assert_eq!(report.hir_fallback, 0);
    assert_eq!(report.ratio(), 0.0);
}

#[test]
fn pure_arithmetic_fn_is_walker_covered() {
    // Pure Literal + Local + BinOp — wave 1 covers every node.
    let report = lower_and_report("fn double(x: Int) -> Int\n    x + x\n");
    assert_eq!(report.total, 1, "exactly one fn lowered");
    assert_eq!(
        report.mir_covered, 1,
        "pure-arith body should emit standalone via walker: {report:?}"
    );
    assert_eq!(report.hir_fallback, 0);
    assert_eq!(report.ratio(), 1.0);
}

#[test]
fn let_chain_fn_is_walker_covered() {
    // `let x = …; let y = …; y` — wave 4a (Let consumer) +
    // wave 1 (BinOp / Literal / Local) → fully covered.
    let report = lower_and_report("fn chain() -> Int\n    x = 7\n    y = x + 1\n    y\n");
    assert_eq!(report.total, 1);
    assert_eq!(
        report.mir_covered, 1,
        "named-let chain should be walker-covered: {report:?}"
    );
}
