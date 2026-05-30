//! Phase 3 → 4 coverage gate of #252.
//!
//! Until the lowerer covers every `ResolvedExpr` shape, fns with
//! unsupported constructs get dropped from `MirProgram.fns`. The
//! drop is silent at the call-site level — `MirProgram::dump()`
//! still renders cleanly for the lowered subset. That's a trap:
//! Phase 4 (VM slice) could land thinking everything is wired
//! when in fact 60% of the corpus silently disappeared.
//!
//! `MirProgram.stats` is the gate. Every wave PR can prove:
//! 1. **Conservation** — `lowered + sum(skipped) == total fns seen`.
//! 2. **Attribution** — each dropped fn names a single dominant
//!    `SkipReason`.
//! 3. **Direction** — newly-supported variants disappear from the
//!    `skipped` map; nothing previously supported regresses.
//!
//! These tests pin those invariants on a mix of supported + each
//! flavour of unsupported construct. Phase 4 gate adds a corpus-
//! wide `coverage_ratio() ≥ X%` floor on the shipped examples.

use aver::ir::mir::{SkipReason, lower_program};
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::source::parse_source;

fn lower_stats(source: &str) -> aver::ir::mir::LowerStats {
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
    lower_program(&result.resolved_items).stats
}

#[test]
fn conservation_lowered_plus_skipped_equals_total() {
    // Mix of one wave-1 supported fn and one wave-3c-bound fn
    // (`Result.Ok` construction). Total fns seen = 2; every fn
    // accounted for in `lowered` or `skipped`.
    let stats = lower_stats(
        "fn keep(x: Int) -> Int\n    x + 1\n\nfn drops_me() -> Result<Int, String>\n    Result.Ok(1)\n",
    );
    assert_eq!(
        stats.total(),
        2,
        "expected 2 fns seen, got {}: {:?}",
        stats.total(),
        stats
    );
    assert_eq!(stats.lowered, 1, "wave-1 fn must lower: {:?}", stats);
    assert_eq!(
        stats.skipped.values().sum::<u32>(),
        1,
        "exactly one fn dropped: {:?}",
        stats
    );
}

#[test]
fn coverage_ratio_pins_one_for_empty_program() {
    // Empty corpus → 1.0 by convention so empty source files
    // don't poison the gate.
    let stats = lower_stats("");
    assert_eq!(stats.total(), 0);
    assert!(
        (stats.coverage_ratio() - 1.0).abs() < f64::EPSILON,
        "empty program coverage_ratio should be 1.0, got {}",
        stats.coverage_ratio()
    );
}

#[test]
fn builtin_ctor_construction_attributes_to_dedicated_reason() {
    let stats = lower_stats("fn makes_ok(x: Int) -> Result<Int, String>\n    Result.Ok(x)\n");
    assert_eq!(stats.lowered, 0, "fn constructing Result.Ok must drop");
    let count = stats
        .skipped
        .get(&SkipReason::BuiltinCtorConstruction)
        .copied()
        .unwrap_or(0);
    assert_eq!(
        count, 1,
        "expected BuiltinCtorConstruction = 1, got {:?}",
        stats.skipped
    );
}

#[test]
fn list_literal_attributes_to_list_reason() {
    let stats = lower_stats("fn one() -> List<Int>\n    [1, 2, 3]\n");
    assert_eq!(
        stats.skipped.get(&SkipReason::UnsupportedList).copied(),
        Some(1),
        "list literal must skip with UnsupportedList:\n{:?}",
        stats.skipped
    );
}

#[test]
fn tuple_literal_attributes_to_tuple_reason() {
    let stats = lower_stats("fn pair() -> Tuple<Int, Int>\n    (1, 2)\n");
    assert_eq!(
        stats.skipped.get(&SkipReason::UnsupportedTuple).copied(),
        Some(1),
        "tuple literal must skip with UnsupportedTuple:\n{:?}",
        stats.skipped
    );
}

#[test]
fn skipped_sorted_is_stable() {
    // Two distinct skips → stable iteration order from
    // `skipped_sorted()` regardless of `HashMap` internal layout.
    let stats =
        lower_stats("fn a() -> List<Int>\n    [1]\n\nfn b() -> Tuple<Int, Int>\n    (1, 2)\n");
    let sorted = stats.skipped_sorted();
    assert_eq!(sorted.len(), 2);
    // SkipReason variant order is the declaration order in
    // `stats.rs` — UnsupportedList comes before UnsupportedTuple.
    let reasons: Vec<_> = sorted.iter().map(|(r, _)| *r).collect();
    let list_pos = reasons
        .iter()
        .position(|r| *r == SkipReason::UnsupportedList);
    let tuple_pos = reasons
        .iter()
        .position(|r| *r == SkipReason::UnsupportedTuple);
    assert!(list_pos.is_some() && tuple_pos.is_some());
    assert!(
        list_pos.unwrap() < tuple_pos.unwrap(),
        "UnsupportedList must precede UnsupportedTuple in sorted iteration: {:?}",
        sorted
    );
}

#[test]
fn skip_reason_label_is_human_readable() {
    // Smoke: every variant we use in the lowerer renders a
    // non-empty label. Future dump / diagnostic surfaces consume
    // these.
    for reason in [
        SkipReason::EmptyBody,
        SkipReason::BindingOnlyTail,
        SkipReason::MissingResolution,
        SkipReason::BindingSlotLookupMissing,
        SkipReason::PatternSlotShortfall,
        SkipReason::BuiltinCtorConstruction,
        SkipReason::BuiltinCtorPattern,
        SkipReason::UnresolvedCtor,
        SkipReason::UnsupportedCallee,
        SkipReason::BuiltinRecord,
        SkipReason::UnsupportedTry,
        SkipReason::UnsupportedTailCall,
        SkipReason::UnsupportedList,
        SkipReason::UnsupportedTuple,
        SkipReason::UnsupportedMap,
        SkipReason::UnsupportedInterpolatedStr,
        SkipReason::UnsupportedIndependentProduct,
        SkipReason::UnresolvedIdent,
        SkipReason::UnsupportedOther,
    ] {
        assert!(
            !reason.label().is_empty(),
            "every SkipReason variant must have a non-empty label"
        );
    }
}
