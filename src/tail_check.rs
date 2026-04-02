use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnBody, Spanned, Stmt, StrPart, TopLevel};
use crate::call_graph;
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
    collect_non_tail_recursion_warnings_in(items, None)
}

pub fn collect_non_tail_recursion_warnings_with_sigs(
    items: &[TopLevel],
    fn_sigs: &crate::verify_law::FnSigMap,
) -> Vec<NonTailRecursionWarning> {
    collect_non_tail_recursion_warnings_in(items, Some(fn_sigs))
}

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
    match &expr.node {
        Expr::FnCall(func, args) => {
            if let Some(callee) = dotted_name(func.as_ref())
                && recursive.contains(&callee)
            {
                out.push(expr.line);
            }
            collect_non_tail_recursive_call_lines_expr(func, recursive, out);
            for arg in args {
                collect_non_tail_recursive_call_lines_expr(arg, recursive, out);
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.1 {
                collect_non_tail_recursive_call_lines_expr(arg, recursive, out);
            }
        }
        Expr::Attr(obj, _) | Expr::ErrorProp(obj) => {
            collect_non_tail_recursive_call_lines_expr(obj, recursive, out);
        }
        Expr::BinOp(_, left, right) => {
            collect_non_tail_recursive_call_lines_expr(left, recursive, out);
            collect_non_tail_recursive_call_lines_expr(right, recursive, out);
        }
        Expr::Match { subject, arms } => {
            collect_non_tail_recursive_call_lines_expr(subject, recursive, out);
            for arm in arms {
                collect_non_tail_recursive_call_lines_expr(&arm.body, recursive, out);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_non_tail_recursive_call_lines_expr(item, recursive, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_non_tail_recursive_call_lines_expr(key, recursive, out);
                collect_non_tail_recursive_call_lines_expr(value, recursive, out);
            }
        }
        Expr::Constructor(_, maybe_arg) => {
            if let Some(arg) = maybe_arg.as_deref() {
                collect_non_tail_recursive_call_lines_expr(arg, recursive, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(expr) = part {
                    collect_non_tail_recursive_call_lines_expr(expr, recursive, out);
                }
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, val) in fields {
                collect_non_tail_recursive_call_lines_expr(val, recursive, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_non_tail_recursive_call_lines_expr(base, recursive, out);
            for (_, val) in updates {
                collect_non_tail_recursive_call_lines_expr(val, recursive, out);
            }
        }
        _ => {}
    }
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
    use crate::types::checker::run_type_check_full;
    use crate::{parser::Parser, tco};

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
        tco::transform_program(&mut items);

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
    fn skips_pure_tail_recursion_after_tco() {
        let src = r#"
fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)
"#;
        let mut items = parse(src);
        tco::transform_program(&mut items);

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
        tco::transform_program(&mut items);

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
        tco::transform_program(&mut items);
        let tc = run_type_check_full(&items, None);

        let warnings = collect_non_tail_recursion_warnings_with_sigs(&items, &tc.fn_sigs);
        assert!(
            warnings.is_empty(),
            "expected spec function warning to be suppressed, got {warnings:?}"
        );
    }
}
