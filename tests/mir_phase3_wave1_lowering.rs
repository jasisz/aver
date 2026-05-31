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
    // Bare nullary builtin ctor `Option.None` in value position
    // lowers to `Attr(Ident("Option"), "None")` (resolver gap),
    // which the MIR lowerer bounces with `UnresolvedIdent`. The
    // lowerer must not panic; it just leaves the fn out.
    //
    // Previously the test used a list literal — wave 3c-iv now
    // lowers those, so the fixture was repurposed to a still-
    // dropped shape.
    let dump = lower("fn nothing() -> Option<Int>\n    Option.None\n");
    assert!(
        !dump.contains("fn nothing"),
        "fn referencing the bare-nullary-ctor resolver gap shouldn't lower:\n{dump}"
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
