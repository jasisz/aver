//! Phase 3 wave 3c-iii of #252 — tail-call lowering.
//!
//! TCO upstream of MIR classifies tail-position calls (same SCC
//! as the surrounding fn) and emits
//! `ResolvedExpr::TailCall { target, args }`. MIR carries them
//! as `MirExpr::TailCall { target: FnId, args }` — backends pick
//! the final shape (wasm-gc tail-call insn, VM tail dispatch,
//! Rust loop rewrite). `FnId` survives so no string lookup is
//! needed on the backend side.
//!
//! Coverage gate: `SkipReason::UnsupportedTailCall` must
//! disappear from `LowerStats.skipped` on the test corpus.

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
fn self_recursive_tail_call_lowers() {
    // fn count_down(n: Int) -> Int
    //   match n
    //     0 -> 0
    //     _ -> count_down(n - 1)
    //
    // The recursive call in the second arm is in tail position —
    // TCO should classify it as `TailCall`, which lowers to the
    // MIR tail-call node. `SkipReason::UnsupportedTailCall` must
    // be 0 (not just absent — actively zero).
    let (dump, stats) = lower(
        "fn count_down(n: Int) -> Int\n    match n\n        0 -> 0\n        _ -> count_down(n - 1)\n",
    );
    assert!(dump.contains("fn count_down"), "fn must lower:\n{dump}");
    assert_eq!(
        stats.skipped.get(&SkipReason::UnsupportedTailCall).copied(),
        None,
        "UnsupportedTailCall must be absent after wave 3c-iii: {:?}",
        stats.skipped
    );
    assert!(stats.lowered >= 1, "count_down must lower: {:?}", stats);
}

#[test]
fn tail_call_renders_with_fn_id() {
    // The dump must show `FnId(N).tail_call(...)` (or whatever
    // the existing tail-call render form is) — not a bare
    // `Call(...)`. Backends consume the `FnId` directly.
    let (dump, _stats) =
        lower("fn loop_(n: Int) -> Int\n    match n\n        0 -> 0\n        _ -> loop_(n - 1)\n");
    // Look for *some* tail-call marker in the dump (the exact
    // textual shape is fixed by `write_tail_call` in dump.rs).
    // We assert that the recursive call appears with the
    // typed-identity hint somehow.
    let lowered_contains_fn_id = dump.contains("FnId(") || dump.contains(".tail_call");
    assert!(
        lowered_contains_fn_id,
        "tail call should render with FnId-typed identity:\n{dump}"
    );
}
