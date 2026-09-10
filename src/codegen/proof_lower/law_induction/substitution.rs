//! Simultaneous substitution after name resolution. Source callees keep their
//! canonical IDs even when arguments originate in a different module.

use crate::ast::Spanned;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr as Expr};
use std::collections::BTreeMap;

pub(super) fn substitute(
    expr: &Spanned<Expr>,
    bindings: &BTreeMap<String, Spanned<Expr>>,
) -> Option<Spanned<Expr>> {
    let rec = |e| substitute(e, bindings);
    let node = match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => {
            let replacement = bindings.get(name)?;
            let replacement = replacement.clone();
            // A new projection starts without an annotation. Its occurrence
            // retains the source element type (including named container types).
            if replacement.ty().is_none()
                && let Some(ty) = expr.ty()
            {
                replacement.set_ty(ty.clone());
            }
            return Some(replacement);
        }
        Expr::Literal(_) => return Some(expr.clone()),
        Expr::BinOp(op, a, b) => Expr::BinOp(*op, Box::new(rec(a)?), Box::new(rec(b)?)),
        Expr::Neg(e) => Expr::Neg(Box::new(rec(e)?)),
        Expr::List(xs) => Expr::List(xs.iter().map(rec).collect::<Option<_>>()?),
        Expr::Tuple(xs) => Expr::Tuple(xs.iter().map(rec).collect::<Option<_>>()?),
        Expr::Attr(base, field) => Expr::Attr(Box::new(rec(base)?), field.clone()),
        Expr::Call(callee, args) => {
            if !matches!(
                callee,
                ResolvedCallee::Fn(_) | ResolvedCallee::Builtin(_) | ResolvedCallee::Intrinsic(_)
            ) {
                return None;
            }
            Expr::Call(callee.clone(), args.iter().map(rec).collect::<Option<_>>()?)
        }
        Expr::TailCall { target, args } => Expr::Call(
            ResolvedCallee::Fn(*target),
            args.iter().map(rec).collect::<Option<_>>()?,
        ),
        // Binder-bearing expressions require an explicit scope contract.
        _ => return None,
    };
    let result = Spanned::new(node, expr.line);
    if let Some(ty) = expr.ty() {
        result.set_ty(ty.clone());
    }
    Some(result)
}
