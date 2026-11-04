//! `warning[non-tail-recursion]` and the repair it offers.
//!
//! No pass turns a non-tail recursive call into a loop, so every function
//! below that recurses outside tail position warns. What differs is the
//! advice: a list built by direct recursion is pointed at the accumulator
//! form `list_build` builds in order, a fold to a scalar at an accumulator
//! parameter, and a tree walk at neither.

use aver::diagnostics::analyze::{AnalyzeOptions, analyze_source};

const SLUG: &str = "non-tail-recursion";

/// `(fn name, repair)` of every non-tail-recursion warning in `body`.
fn warnings(body: &str) -> Vec<(String, String)> {
    let source =
        format!("module Main\n    intent = \"non-tail recursion\"\n    effects []\n{body}");
    let report = analyze_source(&source, &AnalyzeOptions::new("main.av"));
    let errors: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|d| d.is_error() && d.slug != "missing-verify")
        .map(|d| d.summary.clone())
        .collect();
    assert!(errors.is_empty(), "the program should check: {errors:?}");
    report
        .diagnostics
        .iter()
        .filter(|d| d.slug == SLUG)
        .map(|d| {
            assert_eq!(
                d.summary,
                format!("non-tail recursion in '{}'", d.fn_name.clone().unwrap())
            );
            (
                d.fn_name.clone().unwrap_or_default(),
                d.repair.primary.clone().unwrap_or_default(),
            )
        })
        .collect()
}

#[test]
fn a_list_built_by_direct_recursion_is_pointed_at_the_collected_form() {
    let found = warnings(
        r#"
fn incAll(xs: List<Int>) -> List<Int>
    ? "Each element plus one, by direct recursion."
    match xs
        [] -> []
        [x, ..rest] -> List.prepend(x + 1, incAll(rest))
"#,
    );
    assert_eq!(found.len(), 1, "{found:?}");
    let (name, repair) = &found[0];
    assert_eq!(name, "incAll");
    assert!(
        repair.starts_with("Collect into an accumulator started at `[]`"),
        "{repair}"
    );
    assert!(repair.contains("`List.reverse(acc)`"), "{repair}");
    assert!(repair.contains("builds that list in order"), "{repair}");
}

#[test]
fn the_collected_form_does_not_warn() {
    let found = warnings(
        r#"
fn incAllInto(xs: List<Int>, acc: List<Int>) -> List<Int>
    ? "Each element plus one, collected into acc."
    match xs
        [] -> List.reverse(acc)
        [x, ..rest] -> incAllInto(rest, List.prepend(x + 1, acc))

fn incAll(xs: List<Int>) -> List<Int>
    ? "Each element plus one."
    incAllInto(xs, [])
"#,
    );
    assert!(found.is_empty(), "{found:?}");
}

#[test]
fn a_fold_to_a_scalar_is_pointed_at_an_accumulator_parameter() {
    let found = warnings(
        r#"
fn total(xs: List<Int>) -> Int
    ? "The sum, by direct recursion."
    match xs
        [] -> 0
        [x, ..rest] -> x + total(rest)
"#,
    );
    assert_eq!(found.len(), 1, "{found:?}");
    let repair = &found[0].1;
    assert!(
        repair.starts_with("Carry the pending work in an accumulator parameter"),
        "{repair}"
    );
    assert!(!repair.contains("List.reverse"), "{repair}");
}

#[test]
fn a_tree_walk_is_not_promised_anything_by_an_accumulator() {
    let found = warnings(
        r#"
type Tree
    Leaf(Int)
    Node(Tree, Tree)

fn leaves(t: Tree) -> List<Int>
    ? "The leaves, left to right."
    match t
        Tree.Leaf(n) -> [n]
        Tree.Node(l, r) -> List.concat(leaves(l), leaves(r))
"#,
    );
    assert_eq!(found.len(), 1, "{found:?}");
    let repair = &found[0].1;
    assert!(
        repair.starts_with("One path makes 2 recursive calls"),
        "{repair}"
    );
    assert!(
        repair.contains("an accumulator alone does not remove it"),
        "{repair}"
    );
    assert!(!repair.contains("List.reverse"), "{repair}");
}
