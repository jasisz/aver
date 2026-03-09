use super::*;
use std::collections::HashSet;

#[test]
fn detects_self_recursion() {
    let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
    let items = parse(src);
    let rec = find_recursive_fns(&items);
    assert!(
        rec.contains("fib"),
        "fib should be recursive, got: {:?}",
        rec
    );
}

#[test]
fn non_recursive_fn() {
    let src = "fn double(x: Int) -> Int\n    x + x\n";
    let items = parse(src);
    let rec = find_recursive_fns(&items);
    assert!(
        rec.is_empty(),
        "double should not be recursive, got: {:?}",
        rec
    );
}

#[test]
fn mutual_recursion() {
    let src = r#"
fn isEven(n: Int) -> Bool
    match n
        0 -> true
        _ -> isOdd(n - 1)

fn isOdd(n: Int) -> Bool
    match n
        0 -> false
        _ -> isEven(n - 1)
"#;
    let items = parse(src);
    let rec = find_recursive_fns(&items);
    assert!(rec.contains("isEven"), "isEven should be recursive");
    assert!(rec.contains("isOdd"), "isOdd should be recursive");
}

#[test]
fn recursive_callsites_count_syntactic_occurrences() {
    let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
    let items = parse(src);
    let counts = recursive_callsite_counts(&items);
    assert_eq!(counts.get("fib").copied().unwrap_or(0), 2);
}

#[test]
fn recursive_callsites_are_scoped_to_scc() {
    let src = r#"
fn a(n: Int) -> Int
    match n
        0 -> 0
        _ -> b(n - 1) + fib(n)

fn b(n: Int) -> Int
    match n
        0 -> 0
        _ -> a(n - 1)

fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
    let items = parse(src);
    let counts = recursive_callsite_counts(&items);
    assert_eq!(counts.get("a").copied().unwrap_or(0), 1);
    assert_eq!(counts.get("b").copied().unwrap_or(0), 1);
    assert_eq!(counts.get("fib").copied().unwrap_or(0), 2);
}

#[test]
fn recursive_scc_ids_are_deterministic_by_group_name() {
    let src = r#"
fn z(n: Int) -> Int
    match n
        0 -> 0
        _ -> z(n - 1)

fn a(n: Int) -> Int
    match n
        0 -> 0
        _ -> b(n - 1)

fn b(n: Int) -> Int
    match n
        0 -> 0
        _ -> a(n - 1)
"#;
    let items = parse(src);
    let ids = recursive_scc_ids(&items);
    // Group {a,b} gets id=1 (min name "a"), group {z} gets id=2.
    assert_eq!(ids.get("a").copied().unwrap_or(0), 1);
    assert_eq!(ids.get("b").copied().unwrap_or(0), 1);
    assert_eq!(ids.get("z").copied().unwrap_or(0), 2);
}

#[test]
fn ordered_components_respect_module_qualified_dependencies() {
    let src = r#"
fn jsonErr() -> String
    Json.toString(Json.JsonNull)

fn toString(_j: Int) -> String
    "ok"
"#;
    let items = parse(src);
    let fns: Vec<_> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let module_prefixes = HashSet::from(["Json".to_string()]);

    let order = ordered_fn_components(&fns, &module_prefixes);
    let flattened: Vec<_> = order
        .into_iter()
        .flat_map(|group| group.into_iter().map(|fd| fd.name.clone()))
        .collect();

    assert_eq!(
        flattened,
        vec!["toString".to_string(), "jsonErr".to_string()]
    );
}

fn parse(src: &str) -> Vec<TopLevel> {
    let mut lexer = crate::lexer::Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = crate::parser::Parser::new(tokens);
    parser.parse().expect("parse failed")
}
