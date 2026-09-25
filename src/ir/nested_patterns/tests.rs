use super::*;

fn parse(source: &str) -> Vec<TopLevel> {
    let tokens = crate::lexer::Lexer::new(source)
        .tokenize()
        .expect("lex failed");
    crate::parser::Parser::new(tokens)
        .parse()
        .expect("parse failed")
}

/// Check the program as written, compile its nested patterns, and
/// return the lowered program as source plus the lowering's errors.
fn lower(source: &str) -> (String, Vec<String>) {
    let mut items = parse(source);
    let checked = crate::types::checker::run_type_check_full(&items, None);
    assert!(
        checked.errors.is_empty(),
        "the written program must check: {:?}",
        checked.errors
    );
    let errors = lower_nested_patterns(&mut items, &checked.pattern_ctor_families);
    assert!(!has_nested_patterns(&items), "every nested pattern is gone");
    let lowered = crate::types::checker::run_type_check_full(&items, None);
    assert!(
        lowered.errors.is_empty() || !errors.is_empty(),
        "the lowered program must check too: {:?}",
        lowered.errors
    );
    (
        crate::ast::unparse::unparse(&items).expect("unparse"),
        errors.into_iter().map(|e| e.message).collect(),
    )
}

fn fn_source<'a>(program: &'a str, name: &str) -> &'a str {
    let start = program
        .find(&format!("fn {name}("))
        .unwrap_or_else(|| panic!("fn {name} in:\n{program}"));
    let rest = &program[start..];
    let end = rest[1..].find("\nfn ").map(|i| i + 1).unwrap_or(rest.len());
    rest[..end].trim_end()
}

#[test]
fn literal_inside_option_keeps_user_binder_names() {
    let (program, errors) = lower(
        "fn classify(o: Option<Int>) -> String\n    match o\n        Option.Some(0) -> \"zero\"\n        Option.Some(n) -> \"many\"\n        Option.None -> \"none\"\n",
    );
    assert!(errors.is_empty(), "{errors:?}");
    let body = fn_source(&program, "classify");
    assert!(
        body.contains("Option.Some(n) -> match n") && body.contains("0 -> \"zero\""),
        "the field is bound under the user's own name and tested by a flat literal match:\n{body}"
    );
    assert!(!body.contains("__pat"), "no fresh name is needed:\n{body}");
}

#[test]
fn complete_constructor_switch_gets_no_default_arm() {
    // `Option.Some(true)`, `Option.Some(false)` and `Option.None`
    // cover everything: no `_` arm a Lean `match` would call
    // redundant.
    let (program, errors) = lower(
        "fn flags(p: Option<Bool>) -> Int\n    match p\n        Option.Some(true) -> 1\n        Option.Some(false) -> 2\n        Option.None -> 3\n",
    );
    assert!(errors.is_empty(), "{errors:?}");
    let body = fn_source(&program, "flags");
    assert!(!body.contains("_ ->"), "no default arm:\n{body}");
}

#[test]
fn user_sum_type_family_is_known_so_no_default_is_emitted() {
    let (program, errors) = lower(
        "type Shape\n    Circle(Int)\n    Dot\n\nfn area(s: Shape) -> Int\n    match s\n        Shape.Circle(0) -> 0\n        Shape.Circle(r) -> r\n        Shape.Dot -> 1\n",
    );
    assert!(errors.is_empty(), "{errors:?}");
    let body = fn_source(&program, "area");
    assert_eq!(
        body.matches("_ ->").count(),
        1,
        "only the literal switch on the radius has a default:\n{body}"
    );
}

#[test]
fn list_patterns_become_cons_chains() {
    let (program, errors) = lower(
        "fn size(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [a] -> 1\n        [a, b, ..rest] -> 2\n",
    );
    assert!(errors.is_empty(), "{errors:?}");
    let body = fn_source(&program, "size");
    assert!(body.contains("[] -> 0"), "{body}");
    assert!(body.contains("[a, .."), "the head keeps its name:\n{body}");
}

#[test]
fn an_arm_no_value_reaches_is_reported() {
    // The pairwise check cannot see this: neither `Option.Some(true)`
    // nor `Option.Some(false)` alone covers `Option.Some(_)`.
    let (_, errors) = lower(
        "fn f(p: Option<Bool>) -> Int\n    match p\n        Option.Some(true) -> 1\n        Option.Some(false) -> 2\n        Option.Some(_) -> 3\n        Option.None -> 4\n",
    );
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(
        errors[0].contains("Unreachable match arm") && errors[0].contains("Option.Some(_)"),
        "{errors:?}"
    );
}

#[test]
fn non_variable_subject_is_evaluated_once() {
    // A computed subject that some arm binds whole is bound first, so
    // it is not recomputed for the nested test.
    let (program, errors) = lower(
        "fn g(n: Int) -> Option<Int>\n    Option.Some(n)\n\nfn f(n: Int) -> Int\n    match g(n)\n        Option.Some(0) -> 0\n        other -> 1\n",
    );
    assert!(errors.is_empty(), "{errors:?}");
    let body = fn_source(&program, "f");
    assert_eq!(body.matches("g(n)").count(), 1, "{body}");
}

#[test]
fn flat_matches_are_left_alone() {
    let source = "fn f(o: Option<Int>) -> Int\n    match o\n        Option.Some(n) -> n\n        Option.None -> 0\n";
    let items = parse(source);
    assert!(!has_nested_patterns(&items));
}
