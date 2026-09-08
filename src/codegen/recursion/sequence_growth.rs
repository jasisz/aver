//! Strict list/string growth under a stable length bound. This source fact feeds
//! ProofIR, so Lean and Dafny use the same remaining-length measure.

use crate::ast::{BinOp, Expr, FnDef, Literal, Pattern, Spanned, Stmt};

fn ident(e: &Spanned<Expr>, name: &str) -> bool {
    matches!(&e.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == name)
}

fn length(e: &Spanned<Expr>, name: &str, string: bool) -> bool {
    matches!(&e.node, Expr::FnCall(f, args)
        if crate::checker::expr_to_str(f) == if string { "String.len" } else { "List.len" }
            && args.len() == 1 && ident(&args[0], name))
}

fn grows(e: &Spanned<Expr>, name: &str, string: bool) -> bool {
    if string {
        let Expr::BinOp(BinOp::Add, a, b) = &e.node else {
            return false;
        };
        let nonempty =
            |e: &Spanned<Expr>| matches!(&e.node, Expr::Literal(Literal::Str(s)) if !s.is_empty());
        return (ident(a, name) && nonempty(b)) || (nonempty(a) && ident(b, name));
    }
    let Expr::FnCall(f, args) = &e.node else {
        return false;
    };
    match (crate::checker::expr_to_str(f).as_str(), args.as_slice()) {
        ("List.prepend", [_, tail]) => ident(tail, name),
        ("List.concat", [a, b]) => {
            let nonempty = |e: &Spanned<Expr>| matches!(&e.node, Expr::List(xs) if !xs.is_empty());
            (ident(a, name) && nonempty(b)) || (nonempty(a) && ident(b, name))
        }
        _ => false,
    }
}

pub(super) fn detect(fd: &FnDef) -> Option<(usize, usize)> {
    let stmts = fd.body.stmts();
    let (alias, body) = match stmts {
        [Stmt::Expr(body)] => (None, body),
        [Stmt::Binding(name, _, value), Stmt::Expr(body)] => (Some((name.as_str(), value)), body),
        _ => return None,
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    let Expr::BinOp(op, left, right) = &subject.node else {
        return None;
    };
    if arms.len() != 2 {
        return None;
    }
    if !arms
        .iter()
        .any(|a| matches!(a.pattern, Pattern::Literal(Literal::Bool(true))))
        || !arms
            .iter()
            .any(|a| matches!(a.pattern, Pattern::Literal(Literal::Bool(false))))
        || arms.iter().any(|a| {
            crate::codegen::expr_walk::any(&a.body, &mut |e| matches!(e.node, Expr::Match { .. }))
        })
    {
        return None;
    }
    for (sequence_index, (sequence, ty)) in fd.params.iter().enumerate() {
        let string = ty == "String";
        if !string && !ty.starts_with("List<") {
            continue;
        }
        for (bound_index, (bound, ty)) in fd.params.iter().enumerate() {
            if ty != "Int" {
                continue;
            }
            // A local binding may name the length, but cannot shadow either
            // parameter on which the termination fact depends.
            if alias.is_some_and(|(name, value)| {
                name == sequence || name == bound || !length(value, sequence, string)
            }) {
                continue;
            }
            let is_length = |e: &Spanned<Expr>| {
                length(e, sequence, string) || alias.is_some_and(|(name, _)| ident(e, name))
            };
            let recursive_when = match op {
                BinOp::Lt if is_length(left) && ident(right, bound) => true,
                BinOp::Gt if ident(left, bound) && is_length(right) => true,
                BinOp::Gte if is_length(left) && ident(right, bound) => false,
                BinOp::Lte if ident(left, bound) && is_length(right) => false,
                _ => continue,
            };
            let mut found = false;
            let mut valid = true;
            for arm in arms {
                let Pattern::Literal(Literal::Bool(value)) = arm.pattern else {
                    valid = false;
                    break;
                };
                let mut calls = Vec::new();
                super::detect::collect_calls_from_expr(&arm.body, &mut calls);
                for (target, args) in calls {
                    if !super::detect::call_matches(&target, &fd.name) {
                        continue;
                    }
                    found = true;
                    if value != recursive_when
                        || args.len() != fd.params.len()
                        || !grows(args[sequence_index], sequence, string)
                        || !ident(args[bound_index], bound)
                    {
                        valid = false;
                    }
                }
            }
            if found && valid {
                return Some((sequence_index, bound_index));
            }
        }
    }
    None
}
