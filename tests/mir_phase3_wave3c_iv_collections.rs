//! Phase 3 wave 3c-iv of #252 — collection literals.
//!
//! Lists, tuples, maps, and interpolated strings lower
//! element-wise; the outer MIR node preserves the structural
//! shape so backends pick their build strategy independently.
//!
//! Coverage gate: none of the four collection literals drops the
//! fn that carries it.

use aver::ir::mir::lower_program;
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
    assert!(
        stats.skipped.is_empty(),
        "a list literal must not drop the fn: {:?}",
        stats.skipped
    );
}

#[test]
fn tuple_literal_lowers() {
    let (dump, stats) = lower("fn pair() -> Tuple<Int, Int>\n    (1, 2)\n");
    assert!(dump.contains("fn pair"), "fn must lower:\n{dump}");
    assert!(
        stats.skipped.is_empty(),
        "a tuple literal must not drop the fn: {:?}",
        stats.skipped
    );
}

#[test]
fn map_literal_lowers() {
    let (dump, stats) = lower("fn pairs() -> Map<String, Int>\n    {\"a\" => 1, \"b\" => 2}\n");
    assert!(dump.contains("fn pairs"), "fn must lower:\n{dump}");
    assert!(
        stats.skipped.is_empty(),
        "a map literal must not drop the fn: {:?}",
        stats.skipped
    );
}

#[test]
fn interpolated_string_does_not_drop_its_fn() {
    // `interp_lower` upstream of MIR desugars `"...{x}..."` into
    // buffer-build calls before MIR sees the tree, so the lowerer never
    // meets `ResolvedExpr::InterpolatedStr` itself. What it does meet is
    // the desugared call chain, and that must lower too.
    let (_dump, stats) = lower("fn greet(name: String) -> String\n    \"hello, {name}!\"\n");
    assert!(
        stats.skipped.is_empty(),
        "an interpolated string must not drop the fn: {:?}",
        stats.skipped
    );
}

#[test]
fn collections_coverage_corpus() {
    // List / Tuple / Map all lower; interpolated string is
    // upstream-desugared so it goes through buffer-build calls
    // so it is left out of this fixture.
    let (_dump, stats) = lower(
        "fn a() -> List<Int>\n    [1]\n\nfn b() -> Tuple<Int, Int>\n    (1, 2)\n\nfn c() -> Map<String, Int>\n    {\"k\" => 1}\n",
    );
    assert!(
        stats.skipped.is_empty(),
        "no collection literal may drop its fn: {:?}",
        stats.skipped
    );
    assert_eq!(
        stats.lowered, 3,
        "all 3 direct-collection fns must lower: {:?}",
        stats
    );
}
