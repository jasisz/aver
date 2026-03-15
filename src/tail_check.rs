use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnBody, Stmt, StrPart, TopLevel};
use crate::call_graph;
use crate::verify_law::canonical_spec_ref;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NonTailRecursionWarning {
    pub fn_name: String,
    pub line: usize,
    pub recursive_calls: usize,
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
        let recursive_calls = count_non_tail_recursive_calls_body(&fd.body, scc_members);
        if recursive_calls == 0 {
            continue;
        }
        warnings.push(NonTailRecursionWarning {
            fn_name: fd.name.clone(),
            line: fd.line,
            recursive_calls,
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

fn count_non_tail_recursive_calls_body(body: &FnBody, recursive: &HashSet<String>) -> usize {
    body.stmts()
        .iter()
        .map(|stmt| count_non_tail_recursive_calls_stmt(stmt, recursive))
        .sum()
}

fn count_non_tail_recursive_calls_stmt(stmt: &Stmt, recursive: &HashSet<String>) -> usize {
    match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
            count_non_tail_recursive_calls_expr(expr, recursive)
        }
    }
}

fn count_non_tail_recursive_calls_expr(expr: &Expr, recursive: &HashSet<String>) -> usize {
    match expr {
        Expr::FnCall(func, args) => {
            let mut count = 0;
            if let Some(callee) = dotted_name(func.as_ref())
                && recursive.contains(&callee)
            {
                count += 1;
            }
            count
                + count_non_tail_recursive_calls_expr(func, recursive)
                + args
                    .iter()
                    .map(|arg| count_non_tail_recursive_calls_expr(arg, recursive))
                    .sum::<usize>()
        }
        Expr::TailCall(boxed) => boxed
            .1
            .iter()
            .map(|arg| count_non_tail_recursive_calls_expr(arg, recursive))
            .sum(),
        Expr::Attr(obj, _) | Expr::ErrorProp(obj) => {
            count_non_tail_recursive_calls_expr(obj, recursive)
        }
        Expr::BinOp(_, left, right) => {
            count_non_tail_recursive_calls_expr(left, recursive)
                + count_non_tail_recursive_calls_expr(right, recursive)
        }
        Expr::Match { subject, arms, .. } => {
            count_non_tail_recursive_calls_expr(subject, recursive)
                + arms
                    .iter()
                    .map(|arm| count_non_tail_recursive_calls_expr(&arm.body, recursive))
                    .sum::<usize>()
        }
        Expr::List(items) | Expr::Tuple(items) => items
            .iter()
            .map(|item| count_non_tail_recursive_calls_expr(item, recursive))
            .sum(),
        Expr::MapLiteral(entries) => entries
            .iter()
            .map(|(key, value)| {
                count_non_tail_recursive_calls_expr(key, recursive)
                    + count_non_tail_recursive_calls_expr(value, recursive)
            })
            .sum(),
        Expr::Constructor(_, maybe_arg) => maybe_arg
            .as_deref()
            .map(|arg| count_non_tail_recursive_calls_expr(arg, recursive))
            .unwrap_or(0),
        Expr::InterpolatedStr(parts) => parts
            .iter()
            .map(|part| match part {
                StrPart::Literal(_) => 0,
                StrPart::Parsed(expr) => count_non_tail_recursive_calls_expr(expr, recursive),
            })
            .sum(),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .map(|(_, expr)| count_non_tail_recursive_calls_expr(expr, recursive))
            .sum(),
        Expr::RecordUpdate { base, updates, .. } => {
            count_non_tail_recursive_calls_expr(base, recursive)
                + updates
                    .iter()
                    .map(|(_, expr)| count_non_tail_recursive_calls_expr(expr, recursive))
                    .sum::<usize>()
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => 0,
    }
}

fn dotted_name(expr: &Expr) -> Option<String> {
    match expr {
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
