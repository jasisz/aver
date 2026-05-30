//! Phase 3 wave 3c-i of #252 — typed identity for built-in
//! constructors (`Result.Ok` / `Result.Err` / `Option.Some` /
//! `Option.None`) in MIR.
//!
//! Construction and pattern sides ride the same shape as user
//! ctors via `MirCtor::Builtin(BuiltinCtor)`. The dump renders
//! the canonical builtin name unambiguously (`Result.Ok` /
//! `Result.Err` / `Option.Some` / `Option.None`) so backends and
//! reviewers can tell user vs builtin at a glance.
//!
//! End-to-end: parse → pipeline → lower → dump.

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
fn result_ok_construction_renders_with_canonical_name() {
    let dump = lower("fn ok_one() -> Result<Int, String>\n    Result.Ok(1)\n");
    assert!(dump.contains("fn ok_one"), "fn must lower:\n{dump}");
    assert!(
        dump.contains("Result.Ok(Int(1))"),
        "Result.Ok construction should render with the canonical name + arg:\n{dump}"
    );
}

#[test]
fn result_err_construction_renders_with_canonical_name() {
    let dump = lower("fn err_msg() -> Result<Int, String>\n    Result.Err(\"boom\")\n");
    assert!(dump.contains("fn err_msg"), "fn must lower:\n{dump}");
    // Literal::String renders as `Str(...)` in the dump (matches
    // typecheck-stamp convention).
    assert!(
        dump.contains("Result.Err(Str(\"boom\"))"),
        "Result.Err construction should render with the canonical name + arg:\n{dump}"
    );
}

#[test]
fn option_some_construction_renders_with_canonical_name() {
    let dump = lower("fn some_seven() -> Option<Int>\n    Option.Some(7)\n");
    assert!(dump.contains("fn some_seven"), "fn must lower:\n{dump}");
    assert!(
        dump.contains("Option.Some(Int(7))"),
        "Option.Some construction should render with the canonical name + arg:\n{dump}"
    );
}

// NOTE: there's no `option_none_is_nullary_construction` test
// because bare `Option.None` as a body expression currently lowers
// to `ResolvedExpr::Attr(Ident("Option"), "None")` rather than
// `Ctor(Builtin(OptionNone), [])` — a preexisting resolver gap
// for nullary builtin ctors in value position. Pattern-position
// `Option.None` (which goes through a different parser path) is
// covered by `option_match_lowers_with_none_nullary_pattern`
// below and works correctly. Fixing the value-position case is
// orthogonal to wave 3c-i.

#[test]
fn result_match_lowers_via_builtin_ctor_pattern() {
    // fn classify(r: Result<Int, String>) -> Int
    //   match r
    //     Result.Ok(v) -> v
    //     Result.Err(_) -> 0
    let dump = lower(
        "fn classify(r: Result<Int, String>) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> 0\n",
    );
    assert!(dump.contains("fn classify"), "fn must lower:\n{dump}");
    // Both arms appear with canonical builtin names.
    assert!(
        dump.contains("Result.Ok(%"),
        "Result.Ok arm should render builtin name + bound slot:\n{dump}"
    );
    assert!(
        dump.contains("Result.Err(%"),
        "Result.Err arm should render builtin name + bound slot:\n{dump}"
    );
    // No stray `CtorId(N)` for the builtin arms — those would
    // indicate the lowerer fell back to user-ctor identity.
    let cid_count = dump.matches("CtorId(").count();
    assert_eq!(
        cid_count, 0,
        "no CtorId numeric identity should appear for built-in arms:\n{dump}"
    );
}

#[test]
fn option_match_lowers_with_none_nullary_pattern() {
    // fn unwrap_or_zero(o: Option<Int>) -> Int
    //   match o
    //     Option.Some(v) -> v
    //     Option.None    -> 0
    let dump = lower(
        "fn unwrap_or_zero(o: Option<Int>) -> Int\n    match o\n        Option.Some(v) -> v\n        Option.None -> 0\n",
    );
    assert!(dump.contains("fn unwrap_or_zero"), "fn must lower:\n{dump}");
    assert!(
        dump.contains("Option.Some(%"),
        "Option.Some arm should render builtin name + bound slot:\n{dump}"
    );
    // Option.None is nullary in pattern position — empty bindings.
    assert!(
        dump.contains("Option.None()"),
        "Option.None arm should render as zero-arg pattern:\n{dump}"
    );
}

#[test]
fn user_and_builtin_ctors_coexist_in_dump() {
    // Sanity: user-ctor identity stays numeric (`CtorId(N)`) while
    // built-in ctor identity stays named. Both lower through the
    // same `MirConstruct` / `MirPattern::Ctor` shape.
    let dump = lower(
        "type Shape\n  Circle(Int)\n  Square(Int)\n\nfn area(s: Shape) -> Result<Int, String>\n    match s\n        Shape.Circle(r) -> Result.Ok(r)\n        Shape.Square(side) -> Result.Ok(side)\n",
    );
    assert!(
        dump.contains("CtorId("),
        "user Shape variants must still render via CtorId(N):\n{dump}"
    );
    assert!(
        dump.contains("Result.Ok("),
        "Result.Ok construction must render via canonical builtin name:\n{dump}"
    );
}
