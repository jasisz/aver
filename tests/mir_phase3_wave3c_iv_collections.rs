//! Phase 3 wave 3c-iv of #252 — collection literals.
//!
//! Lists, tuples, maps, and interpolated strings lower
//! element-wise; the outer MIR node preserves the structural
//! shape so backends pick their build strategy independently.
//!
//! Coverage gate: `UnsupportedList` / `UnsupportedTuple` /
//! `UnsupportedMap` / `UnsupportedInterpolatedStr` all drop out
//! of `LowerStats.skipped` on the test corpus.

use aver::ir::mir::{SkipReason, lower_program};
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::source::parse_source;

fn lower(source: &str) -> (String, aver::ir::mir::LowerStats) {
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
    let program = lower_program(&result.resolved_items);
    let dump = format!("{program}");
    let stats = program.stats;
    (dump, stats)
}

#[test]
fn list_literal_lowers() {
    let (dump, stats) = lower("fn nums() -> List<Int>\n    [1, 2, 3]\n");
    assert!(dump.contains("fn nums"), "fn must lower:\n{dump}");
    assert_eq!(
        stats.skipped.get(&SkipReason::UnsupportedList).copied(),
        None,
        "UnsupportedList must be absent after wave 3c-iv: {:?}",
        stats.skipped
    );
}

#[test]
fn tuple_literal_lowers() {
    let (dump, stats) = lower("fn pair() -> Tuple<Int, Int>\n    (1, 2)\n");
    assert!(dump.contains("fn pair"), "fn must lower:\n{dump}");
    assert_eq!(
        stats.skipped.get(&SkipReason::UnsupportedTuple).copied(),
        None,
        "UnsupportedTuple must be absent after wave 3c-iv: {:?}",
        stats.skipped
    );
}

#[test]
fn map_literal_lowers() {
    let (dump, stats) = lower("fn pairs() -> Map<String, Int>\n    {\"a\" => 1, \"b\" => 2}\n");
    assert!(dump.contains("fn pairs"), "fn must lower:\n{dump}");
    assert_eq!(
        stats.skipped.get(&SkipReason::UnsupportedMap).copied(),
        None,
        "UnsupportedMap must be absent after wave 3c-iv: {:?}",
        stats.skipped
    );
}

#[test]
fn interpolated_string_skip_reason_never_fires_after_desugar() {
    // Note: `interp_lower` upstream of MIR desugars `"...{x}..."`
    // into buffer-build calls before MIR sees the tree, so
    // `ResolvedExpr::InterpolatedStr` is effectively never reached
    // by the lowerer. `UnsupportedInterpolatedStr` becomes a
    // dead `SkipReason` in production — that's expected. The
    // desugared call chain may still drop the fn through other
    // reasons (e.g. buffer-build builtin call shape) that future
    // waves can pick up.
    let (_dump, stats) = lower("fn greet(name: String) -> String\n    \"hello, {name}!\"\n");
    assert_eq!(
        stats
            .skipped
            .get(&SkipReason::UnsupportedInterpolatedStr)
            .copied(),
        None,
        "UnsupportedInterpolatedStr must never fire — interp_lower desugars upstream: {:?}",
        stats.skipped
    );
}

#[test]
fn collections_coverage_corpus() {
    // List / Tuple / Map all lower; interpolated string is
    // upstream-desugared so it goes through buffer-build calls
    // and may drop via other reasons — assert only that the
    // three direct collection skips are 0.
    let (_dump, stats) = lower(
        "fn a() -> List<Int>\n    [1]\n\nfn b() -> Tuple<Int, Int>\n    (1, 2)\n\nfn c() -> Map<String, Int>\n    {\"k\" => 1}\n",
    );
    for reason in [
        SkipReason::UnsupportedList,
        SkipReason::UnsupportedTuple,
        SkipReason::UnsupportedMap,
    ] {
        assert!(
            stats.skipped.get(&reason).copied().unwrap_or(0) == 0,
            "{:?} must be 0 in corpus stats: {:?}",
            reason,
            stats.skipped
        );
    }
    assert_eq!(
        stats.lowered, 3,
        "all 3 direct-collection fns must lower: {:?}",
        stats
    );
}
