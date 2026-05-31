//! Phase 3 wave 1 of #252 — HIR → MIR lowering for the leaf
//! subset (literals, locals, binops, unary neg, single-stmt expr
//! bodies). These tests drive `lower_program` end-to-end: source
//! → parse → pipeline → `ResolvedTopLevel` → `lower_program` →
//! `MirProgram`. Snapshot via `Display`.

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
fn lowers_int_literal_body() {
    let dump = lower("fn answer() -> Int\n    42\n");
    assert!(dump.contains("fn answer()"), "fn header missing:\n{dump}");
    assert!(
        dump.contains("Int(42)"),
        "int literal didn't render:\n{dump}"
    );
}

#[test]
fn lowers_local_carries_source_name() {
    // Phase 5 prep: `MirExpr::Local` carries `name` from
    // `ResolvedExpr::Resolved.name`. Rust / wasm-gc backends
    // use it as the emitted ident.
    let src = "fn double(x: Int) -> Int\n    x + x\n";
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
        .find(|f| f.name == "double")
        .expect("double in MIR");
    let aver::ir::mir::MirExpr::BinOp(b) = &mir_fn.body.node else {
        panic!("expected BinOp at body root");
    };
    let aver::ir::mir::MirExpr::Local(l) = &b.node.lhs.node else {
        panic!("expected Local lhs");
    };
    let aver::ir::mir::MirExpr::Local(r) = &b.node.rhs.node else {
        panic!("expected Local rhs");
    };
    assert_eq!(l.node.name, "x", "lhs Local name should be `x`");
    assert_eq!(r.node.name, "x", "rhs Local name should be `x`");
}

#[test]
fn lowers_binop_over_locals() {
    let dump = lower("fn double(x: Int) -> Int\n    x + x\n");
    assert!(
        dump.contains("fn double(%0x:"),
        "param didn't render:\n{dump}"
    );
    // Phase 6 wave 4 — the second read of `x` is the last use,
    // so MIR's dump renders it as `%0*` (asterisk = last-use
    // annotation propagated from HIR's `AnnotBool` stamp).
    assert!(
        dump.contains("(%0 Add %0*)"),
        "BinOp(Add, Local, Local-last-use) didn't render:\n{dump}"
    );
}

#[test]
fn lowers_neg_over_int_literal() {
    let dump = lower("fn negone() -> Int\n    -1\n");
    assert!(
        dump.contains("Int(-1)") || dump.contains("-(Int(1))"),
        "Neg or signed literal expected:\n{dump}"
    );
}

#[test]
fn drops_unsupported_fn_silently() {
    // Passing a fn as a value (`callWith(dbl)` → the bare `dbl` ident)
    // is the remaining un-lowered shape — the MIR lowerer bounces it
    // with `UnresolvedIdent`. The lowerer must not panic; it just leaves
    // the fn out. (Calling a fn value, `f(x)`, already lowers via the
    // CALL_VALUE first-class-fn path; only referencing one as a value
    // is left.)
    //
    // The fixture was previously a list literal, then `Option.None`,
    // then a first-class-fn call — all lower now.
    let dump = lower(
        "fn dbl(n: Int) -> Int\n    n + n\n\n\
         fn callWith(f: Fn(Int) -> Int) -> Int\n    f(3)\n\n\
         fn passesFn() -> Int\n    callWith(dbl)\n",
    );
    assert!(
        !dump.contains("fn passesFn"),
        "fn passing a fn value shouldn't lower yet:\n{dump}"
    );
}

#[test]
fn lowers_multi_fn_program_in_fn_id_order() {
    let dump =
        lower("fn one() -> Int\n    1\n\nfn two() -> Int\n    2\n\nfn three() -> Int\n    3\n");
    let pos = |needle: &str| {
        dump.find(needle)
            .unwrap_or_else(|| panic!("missing {needle} in:\n{dump}"))
    };
    assert!(
        pos("fn one") < pos("fn two") && pos("fn two") < pos("fn three"),
        "fns out of FnId order:\n{dump}"
    );
}
