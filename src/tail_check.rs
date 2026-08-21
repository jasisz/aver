use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnBody, Spanned, Stmt, TopLevel};
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
    pub message: String,
}

pub fn collect_non_tail_recursion_warnings(items: &[TopLevel]) -> Vec<NonTailRecursionWarning> {
    let mut fn_to_scc: HashMap<String, HashSet<String>> = HashMap::new();
    for scc in call_graph::find_tco_groups(items) {
        for name in &scc {
            fn_to_scc.insert(name.clone(), scc.clone());
        }
    }

    let mut warnings = Vec::new();
    for item in items {
        let TopLevel::FnDef(fd) = item else {
            continue;
        };
        let Some(scc_members) = fn_to_scc.get(&fd.name) else {
            continue;
        };
        let callsite_lines: Vec<usize> =
            collect_non_tail_recursive_call_lines_body(&fd.body, scc_members)
                .into_iter()
                .filter(|&ln| ln >= fd.line)
                .collect();
        if callsite_lines.is_empty() {
            continue;
        }
        let recursive_calls = callsite_lines.len();
        warnings.push(NonTailRecursionWarning {
            fn_name: fd.name.clone(),
            line: fd.line,
            recursive_calls,
            callsite_lines,
            message: format!(
                "non-tail recursion in '{}' — {} recursive callsite(s) remain after tail-call optimization; rewrite it to tail recursion or make it a spec",
                fd.name, recursive_calls
            ),
        });
    }
    warnings
}

#[cfg(feature = "runtime")]
pub fn collect_non_tail_recursion_warnings_with_sigs(
    items: &[TopLevel],
    fn_sigs: &crate::verify_law::FnSigMap,
) -> Vec<NonTailRecursionWarning> {
    collect_non_tail_recursion_warnings_in(items, Some(fn_sigs))
}

#[cfg(feature = "runtime")]
fn collect_non_tail_recursion_warnings_in(
    items: &[TopLevel],
    fn_sigs: Option<&crate::verify_law::FnSigMap>,
) -> Vec<NonTailRecursionWarning> {
    let mut fn_to_scc: HashMap<String, HashSet<String>> = HashMap::new();
    for scc in call_graph::find_tco_groups(items) {
        for name in &scc {
            fn_to_scc.insert(name.clone(), scc.clone());
        }
    }
    let spec_fns = collect_canonical_spec_functions(items, fn_sigs);

    let mut warnings = Vec::new();
    for item in items {
        let TopLevel::FnDef(fd) = item else {
            continue;
        };
        if spec_fns.contains(&fd.name) {
            continue;
        }
        let Some(scc_members) = fn_to_scc.get(&fd.name) else {
            continue;
        };
        let callsite_lines: Vec<usize> =
            collect_non_tail_recursive_call_lines_body(&fd.body, scc_members)
                .into_iter()
                .filter(|&ln| ln >= fd.line)
                .collect();
        if callsite_lines.is_empty() {
            continue;
        }
        let recursive_calls = callsite_lines.len();
        warnings.push(NonTailRecursionWarning {
            fn_name: fd.name.clone(),
            line: fd.line,
            recursive_calls,
            callsite_lines,
            message: format!(
                "non-tail recursion in '{}' — {} recursive callsite(s) remain after tail-call optimization; rewrite it to tail recursion or make it a spec",
                fd.name, recursive_calls
            ),
        });
    }
    warnings
}

#[cfg(feature = "runtime")]
fn collect_canonical_spec_functions(
    items: &[TopLevel],
    fn_sigs: Option<&crate::verify_law::FnSigMap>,
) -> HashSet<String> {
    let Some(fn_sigs) = fn_sigs else {
        return HashSet::new();
    };

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
        assert_eq!(
            warnings[0].message,
            "non-tail recursion in 'fib' — 2 recursive callsite(s) remain after tail-call optimization; rewrite it to tail recursion or make it a spec"
        );
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
