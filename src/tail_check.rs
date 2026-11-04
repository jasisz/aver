use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnBody, FnDef, Spanned, Stmt, TopLevel};
use crate::call_graph;
#[cfg(feature = "runtime")]
use crate::verify_law::canonical_spec_ref;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NonTailRecursionWarning {
    pub fn_name: String,
    pub line: usize,
    pub recursive_calls: usize,
    /// Source lines of the non-tail recursive callsites.
    pub callsite_lines: Vec<usize>,
    /// Which rewrite, if any, takes the recursion off the stack.
    pub shape: NonTailShape,
    /// `non-tail recursion in '<fn>' — <repair>`: the diagnostic layer
    /// splits the repair off at the dash.
    pub message: String,
}

/// What the recursion left after TCO looks like, which decides the advice.
///
/// No later pass turns a non-tail call into a loop: `list_build` rewrites
/// a loop that already collects into an accumulator, and the backends
/// compile a call that is not a tail call as a call. So every shape here
/// keeps one stack frame per nested call, and the repair says what removes
/// it, if anything does.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NonTailShape {
    /// One path through the body makes two or more recursive calls (a tree
    /// walk, `fib`). The depth is the depth of what is walked, and an
    /// accumulator alone does not flatten it.
    Branching,
    /// At most one recursive call per path, and the function returns a
    /// `List`. The accumulator form with a reverse at the exit is what
    /// `list_build` builds in order, so it is the shape to suggest.
    LinearList,
    /// At most one recursive call per path, returning anything else.
    Linear,
}

impl NonTailShape {
    pub fn repair(self, max_calls_on_a_path: usize) -> String {
        match self {
            Self::Branching => format!(
                "one path makes {max_calls_on_a_path} recursive calls, so the stack grows with the depth of the structure walked and an accumulator alone does not remove it; that is fine for shallow data, and for deep data keep the pending parts in a list that a tail-recursive loop works through"
            ),
            Self::LinearList => "collect into an accumulator started at `[]` (`go(rest, List.prepend(f(x), acc))`) and return `List.reverse(acc)` where the walk ends; the compiler builds that list in order, with no reverse and no stack (Rust and VM), while `List.prepend(f(x), go(rest))` keeps one stack frame per element".to_string(),
            Self::Linear => "carry the pending work in an accumulator parameter so that the recursive call is the last thing each arm does; tail calls run as a loop".to_string(),
        }
    }
}

pub fn collect_non_tail_recursion_warnings(items: &[TopLevel]) -> Vec<NonTailRecursionWarning> {
    let fn_to_scc = recursive_groups(items);
    items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => warning_for(fd, fn_to_scc.get(&fd.name)?),
            _ => None,
        })
        .collect()
}

#[cfg(feature = "runtime")]
pub fn collect_non_tail_recursion_warnings_with_sigs(
    items: &[TopLevel],
    fn_sigs: &crate::verify_law::FnSigMap,
) -> Vec<NonTailRecursionWarning> {
    let fn_to_scc = recursive_groups(items);
    let spec_fns = collect_canonical_spec_functions(items, fn_sigs);
    items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) if !spec_fns.contains(&fd.name) => {
                warning_for(fd, fn_to_scc.get(&fd.name)?)
            }
            _ => None,
        })
        .collect()
}

fn recursive_groups(items: &[TopLevel]) -> HashMap<String, HashSet<String>> {
    let mut fn_to_scc: HashMap<String, HashSet<String>> = HashMap::new();
    for scc in call_graph::find_tco_groups(items) {
        for name in &scc {
            fn_to_scc.insert(name.clone(), scc.clone());
        }
    }
    fn_to_scc
}

fn warning_for(fd: &FnDef, scc_members: &HashSet<String>) -> Option<NonTailRecursionWarning> {
    let callsite_lines: Vec<usize> =
        collect_non_tail_recursive_call_lines_body(&fd.body, scc_members)
            .into_iter()
            .filter(|&ln| ln >= fd.line)
            .collect();
    if callsite_lines.is_empty() {
        return None;
    }
    let max_on_path = max_recursive_calls_on_a_path_body(&fd.body, scc_members);
    let shape = if max_on_path >= 2 {
        NonTailShape::Branching
    } else if is_list_type(&fd.return_type) {
        NonTailShape::LinearList
    } else {
        NonTailShape::Linear
    };
    Some(NonTailRecursionWarning {
        fn_name: fd.name.clone(),
        line: fd.line,
        recursive_calls: callsite_lines.len(),
        callsite_lines,
        shape,
        message: format!(
            "non-tail recursion in '{}' — {}",
            fd.name,
            shape.repair(max_on_path)
        ),
    })
}

fn is_list_type(ty: &str) -> bool {
    ty.trim().starts_with("List<")
}

#[cfg(feature = "runtime")]
fn collect_canonical_spec_functions(
    items: &[TopLevel],
    fn_sigs: &crate::verify_law::FnSigMap,
) -> HashSet<String> {
    items
        .iter()
        .filter_map(|item| match item {
            TopLevel::Verify(v) => match &v.kind {
                crate::ast::VerifyKind::Law(law) => canonical_spec_ref(&v.fn_name, law, fn_sigs)
                    .map(|spec_ref| spec_ref.spec_fn_name),
                crate::ast::VerifyKind::Cases => None,
            },
            _ => None,
        })
        .collect()
}

/// The most non-tail recursive calls one run of the body can make: the
/// statements in sequence add up, the arms of a `match` are alternatives.
fn max_recursive_calls_on_a_path_body(body: &FnBody, recursive: &HashSet<String>) -> usize {
    body.stmts()
        .iter()
        .map(|stmt| match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                max_recursive_calls_on_a_path(expr, recursive)
            }
        })
        .sum()
}

fn max_recursive_calls_on_a_path(expr: &Spanned<Expr>, recursive: &HashSet<String>) -> usize {
    match &expr.node {
        Expr::Match { subject, arms } => {
            max_recursive_calls_on_a_path(subject, recursive)
                + arms
                    .iter()
                    .map(|arm| max_recursive_calls_on_a_path(&arm.body, recursive))
                    .max()
                    .unwrap_or(0)
        }
        _ => {
            let own = match &expr.node {
                Expr::FnCall(func, _)
                    if dotted_name(func.as_ref()).is_some_and(|c| recursive.contains(&c)) =>
                {
                    1
                }
                _ => 0,
            };
            let mut children = 0;
            crate::codegen::expr_walk::for_each_child(expr, &mut |child| {
                children += max_recursive_calls_on_a_path(child, recursive);
            });
            own + children
        }
    }
}

fn collect_non_tail_recursive_call_lines_body(
    body: &FnBody,
    recursive: &HashSet<String>,
) -> Vec<usize> {
    let mut lines = Vec::new();
    for stmt in body.stmts() {
        collect_non_tail_recursive_call_lines_stmt(stmt, recursive, &mut lines);
    }
    lines
}

fn collect_non_tail_recursive_call_lines_stmt(
    stmt: &Stmt,
    recursive: &HashSet<String>,
    out: &mut Vec<usize>,
) {
    match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
            collect_non_tail_recursive_call_lines_expr(expr, recursive, out);
        }
    }
}

fn collect_non_tail_recursive_call_lines_expr(
    expr: &Spanned<Expr>,
    recursive: &HashSet<String>,
    out: &mut Vec<usize>,
) {
    // `expr_walk::walk` descends into every sub-expression (a hand-rolled
    // walk here once skipped `Neg`, so `-f(n)` was never reported) and into
    // a `TailCall`'s arguments without visiting its target, which is what
    // "tail position" means here: only `FnCall` nodes are candidate sites.
    crate::codegen::expr_walk::walk(expr, &mut |node| {
        if let Expr::FnCall(func, _) = &node.node
            && let Some(callee) = dotted_name(func.as_ref())
            && recursive.contains(&callee)
        {
            out.push(node.line);
        }
    });
}

fn dotted_name(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(base, field) => {
            let mut prefix = dotted_name(base)?;
            prefix.push('.');
            prefix.push_str(field);
            Some(prefix)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::TopLevel;
    use crate::ir::TypecheckMode;
    use crate::parser::Parser;

    use super::*;

    fn parse(src: &str) -> Vec<TopLevel> {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex failed");
        let mut parser = Parser::new(tokens);
        parser.parse().expect("parse failed")
    }

    #[test]
    fn warns_for_recursive_calls_left_after_tco() {
        let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
        let mut items = parse(src);
        crate::ir::pipeline::tco(&mut items);

        let warnings = collect_non_tail_recursion_warnings(&items);
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].fn_name, "fib");
        assert_eq!(warnings[0].recursive_calls, 2);
        assert_eq!(warnings[0].shape, NonTailShape::Branching);
        assert!(
            warnings[0]
                .message
                .starts_with("non-tail recursion in 'fib' — one path makes 2 recursive calls"),
            "{}",
            warnings[0].message
        );
        assert!(!warnings[0].message.contains("List.reverse"));
    }

    fn only_warning(src: &str) -> NonTailRecursionWarning {
        let mut items = parse(src);
        crate::ir::pipeline::tco(&mut items);
        let mut warnings = collect_non_tail_recursion_warnings(&items);
        assert_eq!(warnings.len(), 1, "{warnings:?}");
        warnings.remove(0)
    }

    #[test]
    fn a_list_built_by_direct_recursion_is_pointed_at_the_collected_form() {
        // `List.prepend(f(x), go(rest))` stays a real call per element in
        // every backend: no pass makes a loop of it. The accumulator form
        // with a reverse at the exit is what `list_build` builds in order.
        let w = only_warning(
            r#"
fn bump(x: Int) -> Int
    x + 1

fn incAll(xs: List<Int>) -> List<Int>
    match xs
        [] -> []
        [x, ..rest] -> List.prepend(bump(x), incAll(rest))
"#,
        );
        assert_eq!(w.fn_name, "incAll");
        assert_eq!(w.shape, NonTailShape::LinearList);
        assert!(w.message.contains("List.reverse(acc)"), "{}", w.message);
        assert!(
            w.message.contains("builds that list in order"),
            "{}",
            w.message
        );
    }

    #[test]
    fn a_filter_with_a_tail_arm_is_still_one_call_per_path() {
        let w = only_warning(
            r#"
fn keepPos(xs: List<Int>) -> List<Int>
    match xs
        [] -> []
        [x, ..rest] -> match x > 0
            true -> List.prepend(x, keepPos(rest))
            false -> keepPos(rest)
"#,
        );
        assert_eq!(w.shape, NonTailShape::LinearList);
        assert_eq!(w.recursive_calls, 1);
    }

    #[test]
    fn a_linear_fold_to_a_scalar_gets_the_accumulator_advice() {
        let w = only_warning(
            r#"
fn total(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [x, ..rest] -> x + total(rest)
"#,
        );
        assert_eq!(w.shape, NonTailShape::Linear);
        assert!(w.message.contains("accumulator parameter"), "{}", w.message);
        assert!(!w.message.contains("List.reverse"), "{}", w.message);
    }

    #[test]
    fn a_tree_walk_is_branching_even_with_one_call_per_arm_elsewhere() {
        let w = only_warning(
            r#"
type Tree
    Leaf(Int)
    Node(Tree, Tree)

fn leaves(t: Tree) -> List<Int>
    match t
        Tree.Leaf(n) -> [n]
        Tree.Node(l, r) -> List.concat(leaves(l), leaves(r))
"#,
        );
        assert_eq!(w.shape, NonTailShape::Branching);
        assert!(!w.message.contains("List.reverse"), "{}", w.message);
    }

    #[test]
    fn the_collected_form_draws_no_warning() {
        let src = r#"
fn incAllInto(xs: List<Int>, acc: List<Int>) -> List<Int>
    match xs
        [] -> List.reverse(acc)
        [x, ..rest] -> incAllInto(rest, List.prepend(x + 1, acc))
"#;
        let mut items = parse(src);
        crate::ir::pipeline::tco(&mut items);
        assert!(collect_non_tail_recursion_warnings(&items).is_empty());
    }

    #[test]
    fn warns_for_a_negated_recursive_call() {
        // `-f(n)` is `Neg(FnCall)`: a non-tail recursive call that the old
        // hand-rolled walk never descended into.
        let src = r#"
fn alt(n: Int) -> Int
    match n
        0 -> 1
        _ -> -alt(n - 1)
"#;
        let mut items = parse(src);
        crate::ir::pipeline::tco(&mut items);

        let warnings = collect_non_tail_recursion_warnings(&items);
        assert_eq!(warnings.len(), 1, "{warnings:?}");
        assert_eq!(warnings[0].fn_name, "alt");
        assert_eq!(warnings[0].recursive_calls, 1);
    }

    #[test]
    fn skips_pure_tail_recursion_after_tco() {
        let src = r#"
fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)
"#;
        let mut items = parse(src);
        crate::ir::pipeline::tco(&mut items);

        let warnings = collect_non_tail_recursion_warnings(&items);
        assert!(warnings.is_empty());
    }

    #[test]
    fn skips_mutual_tail_recursion_after_tco() {
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
        let mut items = parse(src);
        crate::ir::pipeline::tco(&mut items);

        let warnings = collect_non_tail_recursion_warnings(&items);
        assert!(warnings.is_empty());
    }

    #[test]
    fn skips_canonical_spec_functions() {
        let src = r#"
fn fib(n: Int) -> Int
    fibSpec(n)

fn fibSpec(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fibSpec(n - 1) + fibSpec(n - 2)

verify fib law fibSpec
    given n: Int = [0, 1, 2, 3]
    fib(n) => fibSpec(n)
"#;
        let mut items = parse(src);
        crate::ir::pipeline::tco(&mut items);
        let tc = crate::ir::pipeline::typecheck(&items, &TypecheckMode::Full { base_dir: None });

        let warnings = collect_non_tail_recursion_warnings_with_sigs(&items, &tc.fn_sigs);
        assert!(
            warnings.is_empty(),
            "expected spec function warning to be suppressed, got {warnings:?}"
        );
    }
}
