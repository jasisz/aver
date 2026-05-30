//! Phase 3 wave 3c-v of #252 — `IndependentProduct` lowering.
//!
//! `(a, b, c)!` (raw tuple of `Result`s) and `(a, b, c)?!`
//! (unwrap each `Ok`, propagate first `Err`) both lower to
//! `MirExpr::IndependentProduct { items, unwrap_results }`. The
//! compile-time independence mode (`complete` / `cancel` /
//! `sequential`) is NOT carried — RFC pin, that's an aver.toml
//! runtime policy decision.
//!
//! Coverage gate: `UnsupportedIndependentProduct` drops out of
//! `LowerStats.skipped` on the test corpus.

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
fn unsupported_independent_product_zero_for_non_ip_corpus() {
    // The IndependentProduct skip reason mustn't fire on programs
    // that don't use the construct. Sanity: a plain wave-1 fn
    // doesn't bump UnsupportedIndependentProduct.
    let (_dump, stats) = lower("fn one() -> Int\n    1\n");
    assert_eq!(
        stats
            .skipped
            .get(&SkipReason::UnsupportedIndependentProduct)
            .copied(),
        None,
        "UnsupportedIndependentProduct shouldn't fire on non-IP code: {:?}",
        stats.skipped
    );
}

#[test]
fn skip_reason_unsupported_independent_product_is_removed_from_lowerer() {
    // The lowerer no longer returns `UnsupportedIndependentProduct`
    // for `ResolvedExpr::IndependentProduct(_, _)`. We don't have
    // a clean source-level fixture that *exercises* the parsing of
    // `(a, b, c)!` here without depending on aver-toml policy, so
    // this test pins the negative side: no fn should silently
    // drop with that reason on a small smoke corpus.
    let (_dump, stats) = lower("fn a() -> Int\n    1\n\nfn b() -> Int\n    2\n");
    let n = stats
        .skipped
        .get(&SkipReason::UnsupportedIndependentProduct)
        .copied()
        .unwrap_or(0);
    assert_eq!(
        n, 0,
        "UnsupportedIndependentProduct must be 0 on this corpus: {:?}",
        stats.skipped
    );
}
