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
    // `dbl`, `callWith`, and `caller` all lower now: `callWith` calls
    // its `Fn(..)` param via the CALL_VALUE path, and `caller` PASSES a
    // fn as a value (`callWith(dbl)` → `dbl`), which the lowerer now
    // carries as `MirExpr::FnValue` instead of bouncing. Fn-value
    // passing was the last corpus-relevant drop shape; with it closed,
    // a clean-typechecking program has no un-lowered shapes among
    // ordinary Aver. The conservation invariant still holds:
    // `lowered + skipped == total`, now with `skipped == 0`.
    //
    // List / Tuple / Map / Result.Ok / interpolation / nullary-ctor-in-
    // value-position / first-class-fn *calls* / fn-value *passing* all
    // lower now — none survive as drop fixtures here.
    let stats = lower_stats(
        "fn dbl(n: Int) -> Int\n    n + n\n\n\
         fn callWith(f: Fn(Int) -> Int) -> Int\n    f(3)\n\n\
         fn caller() -> Int\n    callWith(dbl)\n",
    );
    assert_eq!(
        stats.total(),
        3,
        "expected 3 fns seen, got {}: {:?}",
        stats.total(),
        stats
    );
    assert_eq!(
        stats.lowered, 3,
        "all three fns must lower (fn-value passing now carries as FnValue): {:?}",
        stats
    );
    assert_eq!(
        stats.skipped.values().sum::<u32>(),
        0,
        "no fn drops — fn-value passing lowers via MirExpr::FnValue: {:?}",
        stats
    );
}

#[test]
fn fn_value_passing_no_longer_drops() {
    // `callWith(dbl)` passes the top-level fn `dbl` by bare ident into a
    // higher-order fn. Before `MirExpr::FnValue` this resolved to a bare
    // `ResolvedExpr::Ident` the lowerer dropped as `UnresolvedIdent`;
    // now it lowers and the VM walker resolves the symbol via the shared
    // `compile_ident` path. The `UnresolvedIdent` skip counter must be 0.
    let stats = lower_stats(
        "fn dbl(n: Int) -> Int\n    n + n\n\n\
         fn callWith(f: Fn(Int) -> Int) -> Int\n    f(3)\n\n\
         fn caller() -> Int\n    callWith(dbl)\n",
    );
    assert_eq!(
        stats.skipped.get(&SkipReason::UnresolvedIdent).copied(),
        None,
        "fn-value passing must not drop as UnresolvedIdent: {:?}",
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
fn wave_3c_i_builtin_ctor_construction_no_longer_drops() {
    // Wave 3c-i landed typed identity for built-in ctors. A fn
    // whose body is `Result.Ok(x)` now lowers cleanly — its skip
    // counter for `BuiltinCtorConstruction` must be 0.
    let stats = lower_stats("fn makes_ok(x: Int) -> Result<Int, String>\n    Result.Ok(x)\n");
    assert_eq!(
        stats.lowered, 1,
        "Result.Ok construction must lower after wave 3c-i: {:?}",
        stats
    );
    let count = stats
        .skipped
        .get(&SkipReason::BuiltinCtorConstruction)
        .copied()
        .unwrap_or(0);
    assert_eq!(
        count, 0,
        "BuiltinCtorConstruction must be 0 after wave 3c-i, got {:?}",
        stats.skipped
    );
}

#[test]
fn wave_3c_iv_list_literal_no_longer_drops() {
    // Wave 3c-iv landed list-literal lowering. A fn whose body
    // is `[1, 2, 3]` now lowers cleanly — `UnsupportedList`
    // must be 0 (= absent from the skip map).
    let stats = lower_stats("fn one() -> List<Int>\n    [1, 2, 3]\n");
    assert_eq!(
        stats.skipped.get(&SkipReason::UnsupportedList).copied(),
        None,
        "UnsupportedList must be absent after wave 3c-iv: {:?}",
        stats.skipped
    );
    assert_eq!(stats.lowered, 1, "list-literal fn must lower: {:?}", stats);
}

#[test]
fn wave_3c_iv_tuple_literal_no_longer_drops() {
    // Wave 3c-iv landed tuple-literal lowering. Same shape as the
    // list assertion above — `UnsupportedTuple` must be 0.
    let stats = lower_stats("fn pair() -> Tuple<Int, Int>\n    (1, 2)\n");
    assert_eq!(
        stats.skipped.get(&SkipReason::UnsupportedTuple).copied(),
        None,
        "UnsupportedTuple must be absent after wave 3c-iv: {:?}",
        stats.skipped
    );
    assert_eq!(stats.lowered, 1, "tuple-literal fn must lower: {:?}", stats);
}

#[test]
fn skipped_sorted_is_stable() {
    // No ordinary typecheck-clean program drops anymore (fn-value
    // passing was the last shape, now lowered via `MirExpr::FnValue`),
    // so the sort invariant of `skipped_sorted()` is tested by recording
    // the remaining *defensive-guard* reasons directly. Sorted iteration
    // must follow `SkipReason as u8` ascending regardless of insertion
    // order, so dumps + diagnostics don't depend on `HashMap` drift.
    let mut stats = aver::ir::mir::LowerStats::default();
    // Insert out of discriminant order on purpose.
    stats.record_skip(SkipReason::UnsupportedCallee);
    stats.record_skip(SkipReason::EmptyBody);
    stats.record_skip(SkipReason::EmptyBody);
    stats.record_skip(SkipReason::MissingResolution);
    let sorted = stats.skipped_sorted();
    assert!(
        !sorted.is_empty(),
        "skipped map should be non-empty: {:?}",
        stats.skipped
    );
    // Walk sorted output and verify monotonic ascending discriminant.
    let mut prev: Option<u8> = None;
    for (reason, _) in &sorted {
        let disc = *reason as u8;
        if let Some(p) = prev {
            assert!(
                p < disc,
                "skipped_sorted must yield strictly ascending discriminants: {:?}",
                sorted
            );
        }
        prev = Some(disc);
    }
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
