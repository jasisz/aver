//! AST construction and inspection helpers for the `yield` lowering.
//!
//! Every generated node is built here, directly as AST, so the spans the
//! lowering hands out point at the originating stop rather than at a
//! reparsed text.

use std::collections::HashSet;

use crate::ast::*;
use crate::codegen::expr_walk;

pub(super) fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

/// `Pool.claim` / `state.field` / `x` → the dotted spelling, when the
/// expression is a chain of attribute accesses over an identifier.
pub(super) fn dotted_name(expr: &Spanned<Expr>) -> Option<String> {
    let mut segments: Vec<&str> = Vec::new();
    let mut cur = expr;
    loop {
        match &cur.node {
            Expr::Ident(name) => {
                segments.push(name);
                break;
            }
            Expr::Attr(inner, field) => {
                segments.push(field);
                cur = inner;
            }
            _ => return None,
        }
    }
    segments.reverse();
    Some(segments.join("."))
}

/// A node at `line` carrying `node`, stamped with `ty` when one is known.
pub(super) fn spanned(node: Expr, line: usize, ty: Option<&Type>) -> Spanned<Expr> {
    let out = Spanned::new(node, line);
    if let Some(ty) = ty {
        out.set_ty(ty.clone());
    }
    out
}

/// `node` at the position and with the type stamp of `like`.
pub(super) fn spanned_like(like: &Spanned<Expr>, node: Expr) -> Spanned<Expr> {
    spanned(node, like.line, like.ty())
}

pub(super) fn ident(name: &str, line: usize) -> Spanned<Expr> {
    Spanned::new(Expr::Ident(name.to_string()), line)
}

/// `Type.Variant(args)`; a variant without payload is the bare
/// `Type.Variant` reference the parser produces for one.
pub(super) fn ctor(
    type_name: &str,
    variant: &str,
    args: Vec<Spanned<Expr>>,
    line: usize,
) -> Spanned<Expr> {
    let head = Spanned::new(
        Expr::Attr(Box::new(ident(type_name, line)), variant.to_string()),
        line,
    );
    if args.is_empty() {
        head
    } else {
        Spanned::new(Expr::FnCall(Box::new(head), args), line)
    }
}

pub(super) fn call(fn_name: &str, args: Vec<Spanned<Expr>>, line: usize) -> Spanned<Expr> {
    Spanned::new(Expr::FnCall(Box::new(ident(fn_name, line)), args), line)
}

pub(super) fn match_expr(
    subject: Spanned<Expr>,
    arms: Vec<MatchArm>,
    line: usize,
) -> Spanned<Expr> {
    Spanned::new(
        Expr::Match {
            subject: Box::new(subject),
            arms,
        },
        line,
    )
}

pub(super) fn fn_def(
    name: String,
    params: Vec<(String, String)>,
    return_type: String,
    desc: Option<String>,
    mut stmts: Vec<Stmt>,
    tail: Spanned<Expr>,
    line: usize,
) -> FnDef {
    stmts.push(Stmt::Expr(tail));
    FnDef {
        name,
        line,
        params,
        return_type,
        effects: Vec::new(),
        desc,
        body: std::sync::Arc::new(FnBody::Block(stmts)),
        resolution: None,
    }
}

pub(super) fn sum_type(name: String, variants: Vec<(String, Vec<String>)>, line: usize) -> TypeDef {
    TypeDef::Sum {
        name,
        variants: variants
            .into_iter()
            .map(|(name, fields)| TypeVariant { name, fields })
            .collect(),
        line,
    }
}

pub(super) fn pattern_binders(pattern: &Pattern, out: &mut Vec<String>) {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
        Pattern::Ident(name) => out.push(name.clone()),
        Pattern::Cons(head, tail) => {
            out.push(head.clone());
            out.push(tail.clone());
        }
        Pattern::Tuple(items) => {
            for item in items {
                pattern_binders(item, out);
            }
        }
        Pattern::Constructor(_, names) => {
            out.extend(names.iter().filter(|n| *n != "_").cloned());
        }
    }
}

/// Identifiers `expr` refers to that no enclosing pattern binds. Namespace
/// heads (`Pool` in `Pool.claim`) are collected too; callers intersect
/// with the local scope, which never contains a namespace.
pub(super) fn free_idents(
    expr: &Spanned<Expr>,
    bound: &HashSet<String>,
    out: &mut HashSet<String>,
) {
    match &expr.node {
        Expr::Ident(name) => {
            if !bound.contains(name) {
                out.insert(name.clone());
            }
        }
        Expr::Match { subject, arms } => {
            free_idents(subject, bound, out);
            for arm in arms {
                let mut inner = bound.clone();
                let mut binders = Vec::new();
                pattern_binders(&arm.pattern, &mut binders);
                inner.extend(binders);
                free_idents(&arm.body, &inner, out);
            }
        }
        _ => expr_walk::for_each_child(expr, &mut |child| free_idents(child, bound, out)),
    }
}

/// Free identifiers of a statement sequence followed by its tail, in the
/// scope where `bound` is already taken.
pub(super) fn free_idents_of_block(
    stmts: &[Stmt],
    tail: &Spanned<Expr>,
    bound: &HashSet<String>,
) -> HashSet<String> {
    let mut bound = bound.clone();
    let mut out = HashSet::new();
    for stmt in stmts {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                free_idents(expr, &bound, &mut out);
                bound.insert(name.clone());
            }
            Stmt::Expr(expr) => free_idents(expr, &bound, &mut out),
        }
    }
    free_idents(tail, &bound, &mut out);
    out
}

pub(super) fn mentions(stmts: &[Stmt], tail: &Spanned<Expr>, name: &str) -> bool {
    let hit = |expr: &Spanned<Expr>| {
        expr_walk::any(
            expr,
            &mut |e| matches!(&e.node, Expr::Ident(n) if n == name),
        )
    };
    stmts.iter().any(|stmt| match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => hit(expr),
    }) || hit(tail)
}

/// The first type stamp recorded on a use of `name` in the block.
pub(super) fn stamped_use(stmts: &[Stmt], tail: &Spanned<Expr>, name: &str) -> Option<Type> {
    let mut found: Option<Type> = None;
    let mut visit = |expr: &Spanned<Expr>| {
        expr_walk::walk(expr, &mut |e| {
            if found.is_none()
                && let Expr::Ident(n) = &e.node
                && n == name
                && let Some(ty) = e.ty()
            {
                found = Some(ty.clone());
            }
        });
    };
    for stmt in stmts {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => visit(expr),
        }
    }
    visit(tail);
    found
}

/// Rename every reference to the generated temporary `from` into `to`.
/// Temporaries are unique and never shadowed, so no scope check is needed.
pub(super) fn rename_ident(expr: &mut Spanned<Expr>, from: &str, to: &str) {
    if let Expr::Ident(name) = &mut expr.node
        && name == from
    {
        *name = to.to_string();
        return;
    }
    expr_walk::for_each_child_mut(expr, &mut |child| rename_ident(child, from, to));
}
