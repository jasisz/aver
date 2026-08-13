//! The one place that knows what an [`Expr`]'s sub-expressions are.
//!
//! Several analyses walk an expression tree looking for something — a call, a
//! comparison, a name. Each used to carry its own `match` over [`Expr`] with a
//! `_ => {}` arm at the bottom, and a wildcard arm is silent by construction:
//! it neither recurses into a variant nor says that it declined to. Two
//! variants were being skipped that way, and both were live soundness holes in
//! the map-iteration-order refusal (#887):
//!
//! - [`Expr::InterpolatedStr`], because proof export deliberately runs without
//!   interpolation lowering, so `"{firstValue(m)}"` still holds a real call at
//!   export time;
//! - [`Expr::TailCall`], because the TCO transform runs before typechecking, so
//!   EVERY in-SCC tail call is one of these by the time any backend looks.
//!
//! A law reaching an iteration-order observer through either shape saw a cone
//! with no observer in it and exported as a certified theorem that `aver
//! verify` refutes. The two shapes have nothing in common except that a
//! wildcard swallowed them, which is the argument for fixing the walk rather
//! than adding two arms: the next variant would be swallowed the same way.
//!
//! So the recursion lives here once, with **no wildcard arm**. Adding a variant
//! to [`Expr`] now fails this build with `E0004: non-exhaustive patterns`, and
//! whoever adds it has to say what its children are. That compile error is the
//! whole point of the module; `deny(clippy::wildcard_enum_match_arm)` below is
//! the second lock, stopping a future `_` from quietly restoring the hole.
//! Consumers keep their own node-level match — "is THIS node interesting?" —
//! and delegate the descent.
#![deny(clippy::wildcard_enum_match_arm)]

use crate::ast::{Expr, Spanned, StrPart};

/// Call `f` on every immediate sub-expression of `expr`.
///
/// Immediate, not transitive: [`walk`] is the transitive form. Non-expression
/// children (a match arm's pattern, a record field's name, a tail call's
/// target) are not sub-expressions and are not visited.
pub fn for_each_child<'a, F: FnMut(&'a Spanned<Expr>)>(expr: &'a Spanned<Expr>, f: &mut F) {
    match &expr.node {
        // Leaves. Spelled out rather than covered by a wildcard so that
        // "this variant has no sub-expressions" is a statement someone made,
        // not the absence of one.
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}

        Expr::FnCall(callee, args) => {
            f(callee);
            for a in args {
                f(a);
            }
        }
        // The TCO transform rewrites in-SCC tail calls into this before the
        // typechecker runs, so a mutual-recursion cone is made of these, not
        // of `FnCall`. `target` is a name, not a sub-expression.
        Expr::TailCall(tc) => {
            for a in &tc.args {
                f(a);
            }
        }
        // Proof export runs with interpolation lowering OFF (it wants
        // source-level IR), so an interpolated segment still holds the call
        // the user wrote.
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                match p {
                    StrPart::Parsed(inner) => f(inner),
                    StrPart::Literal(_) => {}
                }
            }
        }
        Expr::Attr(inner, _)
        | Expr::ErrorProp(inner)
        | Expr::Neg(inner)
        | Expr::Constructor(_, Some(inner)) => f(inner),
        Expr::BinOp(_, l, r) => {
            f(l);
            f(r);
        }
        Expr::Match { subject, arms } => {
            f(subject);
            for a in arms {
                f(&a.body);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for i in items {
                f(i);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                f(k);
                f(v);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                f(v);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            f(base);
            for (_, v) in updates {
                f(v);
            }
        }
    }
}

/// [`for_each_child`], applied transitively: `f` sees `expr` and every
/// expression below it, parents before children.
pub fn walk<'a, F: FnMut(&'a Spanned<Expr>)>(expr: &'a Spanned<Expr>, f: &mut F) {
    f(expr);
    for_each_child(expr, &mut |child| walk(child, f));
}

/// True when `pred` holds for `expr` or anything below it.
///
/// Short-circuits: `pred` stops being called once one node has answered yes.
pub fn any(expr: &Spanned<Expr>, pred: &mut impl FnMut(&Spanned<Expr>) -> bool) -> bool {
    if pred(expr) {
        return true;
    }
    let mut found = false;
    for_each_child(expr, &mut |child| {
        if !found && any(child, pred) {
            found = true;
        }
    });
    found
}

/// Immediate sub-expressions of `expr`, collected.
///
/// Allocating form of [`for_each_child`], for callers that want to index or
/// re-order the children rather than stream them.
pub fn child_exprs(expr: &Spanned<Expr>) -> Vec<&Spanned<Expr>> {
    let mut out = Vec::new();
    for_each_child(expr, &mut |c| out.push(c));
    out
}
