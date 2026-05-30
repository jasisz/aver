//! Phase 3 wave 3b of #252 — `match` arms + `MirPattern`
//! lowering. End-to-end: parse → pipeline → lower → dump.
//!
//! Pattern bindings draw their `LocalId`s from the resolver's
//! `ResolvedMatchArm.binding_slots`, which fills in preorder of
//! the bindings as they appear in the pattern (same walk as
//! `ast_rewrite::pattern_binding_names`). Built-in / unresolved
//! ctor patterns stay wave 3c territory — fns matching on them
//! get silently skipped.

use aver::ir::mir::lower_program;
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::source::parse_source;

fn lower(source: &str) -> String {
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
    format!("{program}")
}

#[test]
fn list_match_head_or_zero_lowers_through_to_dump() {
    // The canonical wave-3b shape from the RFC:
    //   fn first(xs: List<Int>) -> Int
    //     match xs
    //       [] -> 0
    //       [h, ..t] -> h
    let dump = lower(
        "fn first(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h\n",
    );
    assert!(dump.contains("fn first"), "fn header missing:\n{dump}");
    assert!(
        dump.contains("match %0"),
        "match subject didn't render against the param slot:\n{dump}"
    );
    assert!(
        dump.contains("[] => Int(0)"),
        "EmptyList arm didn't render:\n{dump}"
    );
    // `[h, ..t] => h` — the cons head LocalId reappears in the body.
    // Slot numbering: xs=%0, h=%1, t=%2 (resolver order).
    assert!(
        dump.contains("[%1, ..%2] =>"),
        "Cons pattern didn't render with the right slots:\n{dump}"
    );
    assert!(
        dump.contains("[%1, ..%2] => %1"),
        "Cons arm body didn't reference the head binding:\n{dump}"
    );
}

#[test]
fn wildcard_and_literal_arms_render_structurally() {
    // fn classify(n: Int) -> Int
    //   match n
    //     0 -> 100
    //     _ -> 200
    let dump =
        lower("fn classify(n: Int) -> Int\n    match n\n        0 -> 100\n        _ -> 200\n");
    assert!(
        dump.contains("Int(0) =>"),
        "literal arm didn't render:\n{dump}"
    );
    assert!(dump.contains("_ =>"), "wildcard arm didn't render:\n{dump}");
}

#[test]
fn ident_pattern_binds_to_slot() {
    // fn pick(n: Int) -> Int
    //   match n
    //     x -> x
    //
    // Ident patternu LocalId pochodzi z resolver-assigned slot
    // (preorder), nie z param slot — different from the subject.
    let dump = lower("fn pick(n: Int) -> Int\n    match n\n        x -> x\n");
    // `x =>` form — Bind(LocalId) renders as `%N`, and the body
    // references that same `%N`.
    assert!(
        dump.contains("match %0"),
        "match subject (param n=%0) missing:\n{dump}"
    );
    // The bound local for `x` is slot 1 (n is slot 0, x is the
    // next slot the resolver assigns inside the arm scope).
    assert!(
        dump.contains("%1 => %1"),
        "Ident pattern bind didn't pin to slot %1:\n{dump}"
    );
}

#[test]
fn user_ctor_pattern_lowers_through_ctor_id() {
    // type Shape :- Circle(Int) | Square(Int)
    //
    // fn area(s: Shape) -> Int
    //   match s
    //     Shape.Circle(r) -> r
    //     Shape.Square(side) -> side
    //
    // Both arms ride `MirPattern::Ctor { ctor: CtorId(_), bindings }`.
    // Dump renders `CtorId(N)(%M) =>` per the wave-3b contract.
    let dump = lower(
        "type Shape\n  Circle(Int)\n  Square(Int)\n\nfn area(s: Shape) -> Int\n    match s\n        Shape.Circle(r) -> r\n        Shape.Square(side) -> side\n",
    );
    assert!(dump.contains("fn area"), "area fn missing:\n{dump}");
    assert!(
        dump.contains("CtorId("),
        "user ctor pattern didn't render typed identity:\n{dump}"
    );
    // Two arms → two CtorId pattern lines, each with one bound slot.
    let ctor_arm_count = dump.matches("CtorId(").count();
    assert!(
        ctor_arm_count >= 2,
        "expected ≥2 CtorId-pattern arms, got {ctor_arm_count}:\n{dump}"
    );
}

#[test]
fn builtin_ctor_pattern_drops_the_fn() {
    // `match Result.Ok(...) { Result.Ok(v) -> v; Result.Err(_) -> 0 }`
    // — the ctor patterns reference `Result.Ok` / `Result.Err`,
    // which are *built-in* ctors. Wave 3b drops the whole fn from
    // MIR so wave 3c can later land typed identity for built-in
    // result/option shapes.
    //
    // We pair the dropped fn with a wave-2 fn so the dump still
    // has something to render — the assertion is that
    // `drops_me` is absent.
    let dump = lower(
        "fn keep(x: Int) -> Int\n    x + 1\n\nfn drops_me(r: Result<Int, String>) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> 0\n",
    );
    assert!(
        dump.contains("fn keep"),
        "wave-2 fn should still lower:\n{dump}"
    );
    assert!(
        !dump.contains("fn drops_me"),
        "fn matching on built-in Result ctors must be dropped until wave 3c:\n{dump}"
    );
}
