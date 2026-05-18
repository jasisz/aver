//! Variable collection and pattern binding utilities.
//!
//! Generic AST walking functions used by multiple passes:
//! - Rust codegen liveness analysis
//! - Tail-call reuse analysis
//! - Any future pass needing free-variable sets

use std::collections::HashSet;

use crate::ast::*;

/// Collect all variable names referenced in an expression.
/// Does NOT descend into match pattern bindings (they introduce new names).
pub fn collect_vars(expr: &Expr) -> HashSet<String> {
    let mut vars = HashSet::new();
    collect_vars_inner(expr, &mut vars);
    vars
}

fn collect_vars_inner(expr: &Expr, vars: &mut HashSet<String>) {
    match expr {
        Expr::Ident(name) | Expr::Resolved { name, .. } => {
            vars.insert(name.clone());
        }
        Expr::Literal(_) => {}
        Expr::Attr(obj, _) => collect_vars_inner(&obj.node, vars),
        Expr::FnCall(fn_expr, args) => {
            collect_vars_inner(&fn_expr.node, vars);
            for a in args {
                collect_vars_inner(&a.node, vars);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_vars_inner(&left.node, vars);
            collect_vars_inner(&right.node, vars);
        }
        Expr::Neg(inner) => collect_vars_inner(&inner.node, vars),
        Expr::Match { subject, arms, .. } => {
            collect_vars_inner(&subject.node, vars);
            for arm in arms {
                let mut arm_vars = HashSet::new();
                collect_vars_inner(&arm.body.node, &mut arm_vars);
                let bindings = pattern_bindings(&arm.pattern);
                for v in arm_vars {
                    if !bindings.contains(&v) {
                        vars.insert(v);
                    }
                }
            }
        }
        Expr::Constructor(_, Some(inner)) => collect_vars_inner(&inner.node, vars),
        Expr::Constructor(_, None) => {}
        Expr::ErrorProp(inner) => collect_vars_inner(&inner.node, vars),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(expr) = part {
                    collect_vars_inner(&expr.node, vars);
                }
            }
        }
        Expr::List(elements) => {
            for e in elements {
                collect_vars_inner(&e.node, vars);
            }
        }
        Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for e in items {
                collect_vars_inner(&e.node, vars);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_vars_inner(&k.node, vars);
                collect_vars_inner(&v.node, vars);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                collect_vars_inner(&expr.node, vars);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_vars_inner(&base.node, vars);
            for (_, expr) in updates {
                collect_vars_inner(&expr.node, vars);
            }
        }
        Expr::TailCall(boxed) => {
            let TailCallData {
                target: _, args, ..
            } = boxed.as_ref();
            for a in args {
                collect_vars_inner(&a.node, vars);
            }
        }
    }
}

/// Collect variables in a statement.
pub fn collect_vars_stmt(stmt: &Stmt) -> HashSet<String> {
    match stmt {
        Stmt::Binding(_, _, expr) => collect_vars(&expr.node),
        Stmt::Expr(expr) => collect_vars(&expr.node),
    }
}

/// Get names bound by a pattern (for excluding from parent scope liveness).
pub fn pattern_bindings(pat: &Pattern) -> HashSet<String> {
    let mut bindings = HashSet::new();
    match pat {
        Pattern::Ident(name) => {
            if name != "_" {
                bindings.insert(name.clone());
            }
        }
        Pattern::Cons(head, tail) => {
            if head != "_" {
                bindings.insert(head.clone());
            }
            if tail != "_" {
                bindings.insert(tail.clone());
            }
        }
        Pattern::Constructor(_, fields) => {
            for f in fields {
                if f != "_" {
                    bindings.insert(f.clone());
                }
            }
        }
        Pattern::Tuple(pats) => {
            for p in pats {
                bindings.extend(pattern_bindings(p));
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
    }
    bindings
}
