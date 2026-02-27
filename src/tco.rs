/// Tail-call optimization transform pass.
///
/// Runs after parsing, before type-checking. Uses the call graph SCC analysis
/// to find groups of mutually-recursive functions, then rewrites tail-position
/// calls within each SCC from `FnCall` to `TailCall`.
///
/// A call is in tail position if its result is the direct return value of the
/// function — no further computation wraps it. Specifically:
///   - The expression body of `FnBody::Expr`
///   - The last `Stmt::Expr` in `FnBody::Block`
///   - Each arm body of a `match` in tail position
use std::collections::HashSet;

use crate::ast::*;
use crate::call_graph;

/// Transform all eligible tail calls in the program.
pub fn transform_program(items: &mut [TopLevel]) {
    let groups = call_graph::find_tco_groups(items);
    if groups.is_empty() {
        return;
    }

    // Build a map: fn_name → set of SCC peers (including self)
    let mut fn_to_scc: std::collections::HashMap<String, &HashSet<String>> =
        std::collections::HashMap::new();
    for group in &groups {
        for name in group {
            fn_to_scc.insert(name.clone(), group);
        }
    }

    for item in items.iter_mut() {
        if let TopLevel::FnDef(fd) = item {
            if let Some(scc_members) = fn_to_scc.get(&fd.name) {
                transform_fn(fd, scc_members);
            }
        }
    }
}

fn transform_fn(fd: &mut FnDef, scc_members: &HashSet<String>) {
    let mut body = fd.body.as_ref().clone();
    match &mut body {
        FnBody::Expr(expr) => {
            transform_tail_expr(expr, scc_members);
        }
        FnBody::Block(stmts) => {
            // Only the last Stmt::Expr is in tail position
            if let Some(last) = stmts.last_mut() {
                if let Stmt::Expr(expr) = last {
                    transform_tail_expr(expr, scc_members);
                }
            }
        }
    }
    fd.body = std::rc::Rc::new(body);
}

/// Recursively transform an expression in tail position.
fn transform_tail_expr(expr: &mut Expr, scc_members: &HashSet<String>) {
    match expr {
        // Direct call: `f(args)` where f is Ident in SCC
        Expr::FnCall(fn_expr, args) => {
            if let Expr::Ident(name) = fn_expr.as_ref() {
                if scc_members.contains(name) {
                    let name = name.clone();
                    let args = std::mem::take(args);
                    *expr = Expr::TailCall(Box::new((name, args)));
                }
            }
        }
        // Match: each arm body is in tail position
        Expr::Match { arms, .. } => {
            for arm in arms {
                transform_tail_expr(&mut arm.body, scc_members);
            }
        }
        // Everything else is not a tail call
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> Vec<TopLevel> {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex failed");
        let mut parser = crate::parser::Parser::new(tokens);
        parser.parse().expect("parse failed")
    }

    /// Helper: extract the match arms from a fn body.
    /// The parser produces `Block([Expr(Match{subject, arms, ..})])` for indented match bodies.
    fn extract_match_arms(fd: &FnDef) -> &[MatchArm] {
        match fd.body.as_ref() {
            FnBody::Expr(Expr::Match { arms, .. }) => arms,
            FnBody::Block(stmts) => {
                if let Some(Stmt::Expr(Expr::Match { arms, .. })) = stmts.last() {
                    arms
                } else {
                    panic!("expected Match in block body, got {:?}", fd.body)
                }
            }
            other => panic!("expected Match body, got {:?}", other),
        }
    }

    #[test]
    fn transforms_self_tail_call() {
        let src = r#"
fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)
"#;
        let mut items = parse(src);
        transform_program(&mut items);

        let fd = match &items[0] {
            TopLevel::FnDef(fd) => fd,
            _ => panic!("expected FnDef"),
        };

        let arms = extract_match_arms(fd);
        // arm 0: literal 0 -> acc (unchanged)
        assert!(!matches!(*arms[0].body, Expr::TailCall(..)));
        // arm 1: _ -> TailCall("factorial", ...)
        match &*arms[1].body {
            Expr::TailCall(boxed) => {
                let (name, args) = boxed.as_ref();
                assert_eq!(name, "factorial");
                assert_eq!(args.len(), 2);
            }
            other => panic!("expected TailCall, got {:?}", other),
        }
    }

    #[test]
    fn does_not_transform_non_tail_call() {
        let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
        let mut items = parse(src);
        transform_program(&mut items);

        let fd = match &items[0] {
            TopLevel::FnDef(fd) => fd,
            _ => panic!("expected FnDef"),
        };

        let arms = extract_match_arms(fd);
        // arm 2: _ -> fib(n-1) + fib(n-2) — BinOp, NOT TailCall
        assert!(
            !matches!(*arms[2].body, Expr::TailCall(..)),
            "fib should NOT be tail-call transformed"
        );
    }

    #[test]
    fn transforms_mutual_recursion() {
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
        transform_program(&mut items);

        // Check isEven
        let fd_even = match &items[0] {
            TopLevel::FnDef(fd) => fd,
            _ => panic!("expected FnDef"),
        };
        let arms_even = extract_match_arms(fd_even);
        match &*arms_even[1].body {
            Expr::TailCall(boxed) => assert_eq!(boxed.0, "isOdd"),
            other => panic!("expected TailCall to isOdd, got {:?}", other),
        }

        // Check isOdd
        let fd_odd = match &items[1] {
            TopLevel::FnDef(fd) => fd,
            _ => panic!("expected FnDef"),
        };
        let arms_odd = extract_match_arms(fd_odd);
        match &*arms_odd[1].body {
            Expr::TailCall(boxed) => assert_eq!(boxed.0, "isEven"),
            other => panic!("expected TailCall to isEven, got {:?}", other),
        }
    }
}
