//! Phase 4 of #252 — VM vertical slice on MIR (skeleton PoC).
//!
//! This file tests the *coverage classifier* — what fraction of
//! a MIR program the Phase 4 vertical slice can compile, given
//! its current `MirExpr` subset (Literal / Local / BinOp / Neg /
//! Let / Call(Fn) / Return). The actual bytecode emission lives
//! in `src/vm/compiler/mir.rs`; the per-`FnCompiler` walker is
//! covered by internal smoke tests that need direct access to
//! the (private) `ProgramCompiler`.
//!
//! Phase 4b will add an end-to-end parity test that runs both
//! the HIR and MIR bytecode paths through `Vm` and asserts
//! identical `NanValue` results. That depends on exposing a
//! public `compile_program_with_mir_fallback` entry, which we
//! intentionally defer.

use aver::ir::mir::lower_program;
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::source::parse_source;
use aver::vm::mir_vm::classify_mir_program_coverage;

fn lower(source: &str) -> aver::ir::mir::MirProgram {
    let mut items = parse_source(source).unwrap_or_else(|e| panic!("parse: {e}"));
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck requested");
    assert!(tc.errors.is_empty(), "typecheck failed: {:?}", tc.errors);
    lower_program(&result.resolved_items)
}

#[test]
fn double_fn_is_phase_4_subset_covered() {
    // `fn double(x: Int) -> Int = x + x` is the canonical
    // Phase 4 vertical-slice fixture: one BinOp, two Locals.
    // Both lower into `MirExpr` variants the VM walker handles.
    let mir = lower("fn double(x: Int) -> Int\n    x + x\n");
    let cov = classify_mir_program_coverage(&mir);
    assert_eq!(
        cov.covered, 1,
        "double() should land in the Phase 4 subset: covered={}, needs_hir_fallback={}",
        cov.covered, cov.needs_hir_fallback
    );
    assert_eq!(cov.needs_hir_fallback, 0);
}

#[test]
fn user_fn_call_lands_in_subset() {
    // Multi-fn corpus with one user-fn call. `MirExpr::Call`
    // with `MirCallee::Fn(_)` is in the Phase 4 subset; the
    // walker emits CALL_KNOWN.
    let mir =
        lower("fn double(x: Int) -> Int\n    x + x\n\nfn quad(y: Int) -> Int\n    double(y + y)\n");
    let cov = classify_mir_program_coverage(&mir);
    assert_eq!(
        cov.covered, 2,
        "both double + quad should be covered: {:?}",
        cov
    );
}

#[test]
fn complex_pattern_match_fn_falls_back_to_hir() {
    // 4g-2 covers Cons + EmptyList. To keep this test honest as
    // the subset grows, use a `Ctor` pattern which is still
    // outside the subset (Phase 4g-3 territory).
    let mir = lower(
        "fn double(x: Int) -> Int\n    x + x\n\ntype Shape\n  Circle(Int)\n  Square(Int)\n\nfn area(s: Shape) -> Int\n    match s\n        Shape.Circle(r) -> r\n        Shape.Square(side) -> side\n",
    );
    let cov = classify_mir_program_coverage(&mir);
    assert_eq!(cov.covered, 1, "double() in subset: {:?}", cov);
    assert_eq!(
        cov.needs_hir_fallback, 1,
        "area() (Ctor pattern) needs HIR fallback: {:?}",
        cov
    );
}

#[test]
fn builtin_call_now_lands_in_subset_after_phase_4e() {
    // Phase 4e landed `MirCallee::Builtin(_)` → CALL_BUILTIN
    // dispatch via the VmBuiltin lookup. A fn that uses
    // `Console.print` (or any builtin in the VmBuiltin::ALL
    // table) now lands in `covered`. The test was previously
    // pinned to the opposite — Phase 4d's "falls back to HIR"
    // — so the assertion is flipped here to track the new
    // reality.
    let mir = lower(
        "fn print_hello() -> Int\n    ! [Console.print]\n    Console.print(\"hello\")\n    0\n",
    );
    let cov = classify_mir_program_coverage(&mir);
    assert_eq!(
        cov.covered, 1,
        "Console.print fn must land in the Phase 4 subset after 4e: {:?}",
        cov
    );
}
