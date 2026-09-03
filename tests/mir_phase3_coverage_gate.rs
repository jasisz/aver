//! `LowerStats` / `SkipReason` plumbing tests (originally the Phase 3 →
//! 4 coverage gate of #252).
//!
//! The per-wave lowering suites (`tests/mir_phase3_wave*.rs`) now cover
//! every ordinary shape directly and clean-typechecking Aver no longer
//! produces any `SkipReason` at all (see `src/ir/mir/stats.rs`), so the
//! corpus-conservation and per-shape "still lowers" tests this file
//! used to carry were exact duplicates of assertions already made,
//! more strongly, by those wave suites — removed below with pointers to
//! their replacements.
//!
//! What's left genuinely exercises live code that has no other test:
//! the `total == 0` convention on `LowerStats::coverage_ratio()` (feeds
//! `aver compile --explain-mir-coverage`, see
//! `tests/explain_mir_coverage_spec.rs`), and the sort/label behaviour
//! of `LowerStats::skipped_sorted()` / `SkipReason::label()`, both
//! called from the same `--explain-mir-coverage` renderer in
//! `src/main/commands.rs`.

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

// `conservation_lowered_plus_skipped_equals_total` and
// `fn_value_passing_no_longer_drops` used to pin the `dbl` / `callWith`
// / `caller` fn-value-passing fixture here; removed as exact-fixture
// duplicates of `tests/mir_phase3_wave1_lowering.rs::lowers_fn_value_passing`,
// which checks the same corpus with a stronger (structural, not just
// skip-count) assertion.

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

// `wave_3c_i_builtin_ctor_construction_no_longer_drops`,
// `wave_3c_iv_list_literal_no_longer_drops` and
// `wave_3c_iv_tuple_literal_no_longer_drops` used to pin the same
// `Result.Ok(x)` / `[1, 2, 3]` / `(1, 2)` fixtures already covered,
// with a stronger structural assertion, by
// `tests/mir_phase3_wave3c_i_builtin_ctors.rs::result_ok_construction_renders_with_canonical_name`
// and `tests/mir_phase3_wave3c_iv_collections.rs::{list_literal_lowers,tuple_literal_lowers}`;
// removed as duplicates.

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
        SkipReason::UnresolvedCtor,
        SkipReason::UnsupportedCallee,
        SkipReason::UnsupportedOther,
    ] {
        assert!(
            !reason.label().is_empty(),
            "every SkipReason variant must have a non-empty label"
        );
    }
}
