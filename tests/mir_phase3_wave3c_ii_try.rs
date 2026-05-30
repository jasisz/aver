//! Phase 3 wave 3c-ii of #252 — `?` propagation as a `MirExpr::Try`
//! node.
//!
//! RFC pin: `Try` stays a node. Backends pick the final shape
//! (Rust `?`, VM tag-check + early return, wasm-gc
//! `br_on_struct`). The bind-and-propagate form
//! `let x = step()?; body` composes via `Let` wrapping `Try` —
//! wave 3a's stmt-chain right-fold gives that shape automatically.
//!
//! Coverage gate: `SkipReason::UnsupportedTry` must disappear
//! from `LowerStats.skipped` on the test corpus.
//!
//! End-to-end: parse → pipeline → lower → dump.

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
fn try_in_tail_position_lowers_to_try_node() {
    // fn relay() -> Result<Int, String>
    //   fetch()?
    //
    // Lowers as `Try(Call(FnId(fetch)))`. Dump renders the call
    // followed by `?`.
    let (dump, stats) = lower(
        "fn fetch() -> Result<Int, String>\n    Result.Ok(1)\n\nfn relay() -> Result<Int, String>\n    Result.Ok(fetch()?)\n",
    );
    assert!(dump.contains("fn relay"), "relay must lower:\n{dump}");
    assert!(
        dump.contains(")?"),
        "Try should render as inner expr + `?`:\n{dump}"
    );
    assert_eq!(
        stats.skipped.get(&SkipReason::UnsupportedTry).copied(),
        None,
        "UnsupportedTry must be absent from skipped after wave 3c-ii: {:?}",
        stats.skipped
    );
}

#[test]
fn try_let_composition_lowers_to_let_with_try_value() {
    // fn use_step() -> Result<Int, String>
    //   x = fetch()?
    //   Result.Ok(x + 1)
    //
    // Wave 3a's stmt-chain right-folds this into:
    //   Let { binding: x, value: Try(Call(fetch)), body: Construct(Result.Ok, [x+1]) }
    let (dump, _stats) = lower(
        "fn fetch() -> Result<Int, String>\n    Result.Ok(1)\n\nfn use_step() -> Result<Int, String>\n    x = fetch()?\n    Result.Ok(x + 1)\n",
    );
    assert!(dump.contains("fn use_step"), "use_step must lower:\n{dump}");
    assert!(
        dump.contains("let %"),
        "Let chain must wrap the Try'd step:\n{dump}"
    );
    assert!(
        dump.contains(")?"),
        "Try must render inside the Let's value side:\n{dump}"
    );
    // The Result.Ok happy-path body still renders below the let.
    assert!(
        dump.contains("Result.Ok("),
        "Let body must still render the Result.Ok wrapper:\n{dump}"
    );
}

#[test]
fn try_coverage_gate_unsupportedtry_zero_on_corpus() {
    // Coverage gate check: across a small corpus that exercises
    // both wave-3c-i (builtin ctors) and wave-3c-ii (Try),
    // `UnsupportedTry` must not appear in skipped reasons.
    let (_dump, stats) = lower(
        "fn fetch() -> Result<Int, String>\n    Result.Ok(7)\n\nfn relay() -> Result<Int, String>\n    Result.Ok(fetch()?)\n\nfn chain() -> Result<Int, String>\n    x = fetch()?\n    y = fetch()?\n    Result.Ok(x + y)\n",
    );
    let try_count = stats
        .skipped
        .get(&SkipReason::UnsupportedTry)
        .copied()
        .unwrap_or(0);
    assert_eq!(
        try_count, 0,
        "UnsupportedTry must be 0 across the corpus after wave 3c-ii: {:?}",
        stats.skipped
    );
    assert!(
        stats.lowered >= 3,
        "all three fns (fetch / relay / chain) must lower: {:?}",
        stats
    );
}
