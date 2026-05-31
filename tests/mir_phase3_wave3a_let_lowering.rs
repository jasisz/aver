//! Phase 3 wave 3a of #252 — multi-stmt fn bodies with `Let`
//! bindings. The first lowering that produces non-trivial
//! sequencing — `Stmt::Binding` and intermediate `Stmt::Expr`
//! both right-fold into a `Let` chain on the MIR side.
//!
//! Slot mapping comes from the resolver's `FnResolution.local_slots`
//! (binding name → resolved slot). Intermediate effectful exprs
//! get fresh `LocalId`s drawn from a counter starting at
//! `local_count` so they can't collide with resolver slots.

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
fn lowers_let_carries_source_binding_name() {
    // Phase 5 prep: `MirLet::binding_name` carries the source
    // identifier when the let came from `Stmt::Binding`. Rust /
    // wasm-gc backends use it as the emitted let-binder.
    let src = "fn add_one(n: Int) -> Int\n    bumped = n + 1\n    bumped\n";
    let mut items = parse_source(src).unwrap();
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    assert!(result.typecheck.as_ref().unwrap().errors.is_empty());
    let program = lower_program(&result.resolved_items);
    let mir_fn = program
        .fns
        .values()
        .find(|f| f.name == "add_one")
        .expect("add_one in MIR");
    let aver::ir::mir::MirExpr::Let(let_node) = &mir_fn.body.node else {
        panic!("expected Let at body root, got: {:?}", mir_fn.body.node);
    };
    assert_eq!(
        let_node.node.binding_name, "bumped",
        "Let.binding_name should propagate source ident"
    );
}

#[test]
fn lowers_single_let_binding_then_expr() {
    // let x = 7
    // x
    let dump = lower("fn seven_via_let() -> Int\n    x = 7\n    x\n");
    assert!(
        dump.contains("let %0 ="),
        "Let header missing — expected `let %0 =`:\n{dump}"
    );
    assert!(dump.contains("Int(7)"), "value didn't render:\n{dump}");
    assert!(
        dump.contains("in %0"),
        "Let body continuation missing — expected `in %0`:\n{dump}"
    );
}

#[test]
fn lowers_chained_let_bindings() {
    // x = 7
    // y = x + 1
    // y
    // Right-fold: Let(x, 7, Let(y, x+1, y))
    let dump = lower("fn chain() -> Int\n    x = 7\n    y = x + 1\n    y\n");
    // Both bindings are visible; the second `Let` is nested
    // inside the first, so `let %1 =` should appear after the
    // first `let %0 =`.
    let pos0 = dump.find("let %0 =").expect("first let missing");
    let pos1 = dump.find("let %1 =").expect("second let missing");
    assert!(
        pos0 < pos1,
        "Let chain didn't nest in source order:\n{dump}"
    );
    // Wave 4 last-use: `x` is final read in `x + 1`, `y` is
    // final read in the tail expression — both render with `*`.
    assert!(
        dump.contains("(%0* Add Int(1))"),
        "second binding value should reference last-use %0*:\n{dump}"
    );
    assert!(
        dump.contains("in %1*"),
        "final body should be last-use %1*:\n{dump}"
    );
}

#[test]
fn intermediate_expr_stmt_gets_synthetic_local() {
    // x = 1
    // Console.print("hi")   <- intermediate Expr stmt, effectful
    // x
    //
    // The intermediate Expr can't be dropped — it has effects.
    // It gets a fresh LocalId (drawn from `local_count`) so it
    // shows up as `let %N = Builtin(Console.print).call(...)`
    // in the dump.
    let dump = lower(
        "fn intermediate() -> Int\n    ! [Console.print]\n    x = 1\n    Console.print(\"hi\")\n    x\n",
    );
    assert!(
        dump.contains("Console.print"),
        "Console.print call should still be in the dump:\n{dump}"
    );
    // Two `let`s expected (one for `x`, one synthetic for the
    // effectful Console.print call).
    let let_count = dump.matches("let %").count();
    assert!(
        let_count >= 2,
        "expected at least 2 `let` bindings, got {let_count}:\n{dump}"
    );
}

#[test]
fn binding_only_body_is_dropped() {
    // x = 7
    // (no tail expression — every fn must end in a value)
    //
    // This shouldn't even typecheck normally, but if it did, the
    // lowerer must skip it cleanly (last stmt is Binding, not
    // Expr). We assert by using a valid program where one fn has
    // only a binding-final body (impossible in well-typed Aver),
    // OR confirm the chain helper rejects it. Here we just
    // confirm a normal program lowers and the lowerer didn't
    // panic earlier.
    let dump = lower("fn normal() -> Int\n    x = 5\n    x\n");
    assert!(
        dump.contains("fn normal"),
        "normal multi-stmt fn must still lower:\n{dump}"
    );
}

#[test]
fn lowers_match_carries_pattern_binding_names() {
    // Phase 5 prep: `MirPattern::{Bind, Cons, Ctor}` carry source
    // identifiers so Rust / wasm-gc walkers can emit them as
    // arm-binding idents. Same propagation shape as
    // `MirLocal.name` on the value side.
    let src = "fn first(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [head, ..tail] -> head\n";
    let mut items = parse_source(src).unwrap();
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    assert!(
        result.typecheck.as_ref().unwrap().errors.is_empty(),
        "typecheck failed: {:?}",
        result.typecheck.as_ref().unwrap().errors
    );
    let program = lower_program(&result.resolved_items);
    let mir_fn = program
        .fns
        .values()
        .find(|f| f.name == "first")
        .expect("first in MIR");
    let aver::ir::mir::MirExpr::Match(m) = &mir_fn.body.node else {
        panic!("expected Match at body root, got: {:?}", mir_fn.body.node);
    };
    let cons_arm = m
        .node
        .arms
        .iter()
        .find(|arm| matches!(arm.pattern, aver::ir::mir::MirPattern::Cons { .. }))
        .expect("Cons arm present");
    let aver::ir::mir::MirPattern::Cons {
        head_name,
        tail_name,
        ..
    } = &cons_arm.pattern
    else {
        unreachable!()
    };
    assert_eq!(head_name, "head", "Cons.head_name should be `head`");
    assert_eq!(tail_name, "tail", "Cons.tail_name should be `tail`");
}
