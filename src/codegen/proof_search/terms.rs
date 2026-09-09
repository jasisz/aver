//! Small binder-free term language for application search. Names of declarations
//! are canonical HIR callees; substitutions touch values, never declarations.

use std::collections::{BTreeMap, BTreeSet};

use crate::ast::Spanned;
use crate::ir::hir::{ResolvedCallee as Callee, ResolvedExpr as Expr};

pub(super) type Term = Spanned<Expr>;
pub(super) type Bindings = BTreeMap<String, Term>;

pub(super) fn variable(t: &Term) -> Option<&str> {
    match &t.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n),
        _ => None,
    }
}

pub(super) fn map(t: &Term, f: &mut impl FnMut(&Term) -> Term) -> Term {
    let node = match &t.node {
        Expr::Call(c @ (Callee::Fn(_) | Callee::Builtin(_) | Callee::Intrinsic(_)), args) => {
            Expr::Call(c.clone(), args.iter().map(f).collect())
        }
        Expr::TailCall { target, args } => {
            Expr::Call(Callee::Fn(*target), args.iter().map(f).collect())
        }
        Expr::List(xs) => Expr::List(xs.iter().map(f).collect()),
        Expr::Tuple(xs) => Expr::Tuple(xs.iter().map(f).collect()),
        Expr::BinOp(op, a, b) => Expr::BinOp(*op, Box::new(f(a)), Box::new(f(b))),
        Expr::Neg(a) => Expr::Neg(Box::new(f(a))),
        _ => return t.clone(),
    };
    let out = Spanned::new(node, t.line);
    if let Some(ty) = t.ty() {
        out.set_ty(ty.clone());
    }
    out
}

pub(super) fn supported(t: &Term) -> bool {
    match &t.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => true,
        Expr::Call(Callee::Fn(_) | Callee::Builtin(_) | Callee::Intrinsic(_), args)
        | Expr::TailCall { args, .. }
        | Expr::List(args)
        | Expr::Tuple(args) => args.iter().all(supported),
        Expr::BinOp(_, a, b) => supported(a) && supported(b),
        Expr::Neg(a) => supported(a),
        // A binder or dynamic callee requires a richer scoped term language.
        _ => false,
    }
}

pub(super) fn substitute(t: &Term, bindings: &Bindings) -> Term {
    if let Some(value) = variable(t).and_then(|n| bindings.get(n)) {
        return value.clone();
    }
    map(t, &mut |child| substitute(child, bindings))
}

pub(super) fn matches(
    pattern: &Term,
    target: &Term,
    variables: &BTreeSet<String>,
    bindings: &mut Bindings,
) -> bool {
    if let Some(name) = variable(pattern) {
        if !variables.contains(name) {
            return variable(target) == Some(name);
        }
        return match bindings.get(name) {
            Some(prior) => prior == target,
            None => {
                bindings.insert(name.to_string(), target.clone());
                true
            }
        };
    }
    let mut pair = |a: &[Term], b: &[Term]| {
        a.len() == b.len()
            && a.iter()
                .zip(b)
                .all(|(a, b)| matches(a, b, variables, bindings))
    };
    match (&pattern.node, &target.node) {
        (Expr::Literal(a), Expr::Literal(b)) => a == b,
        (Expr::Call(a, xs), Expr::Call(b, ys)) => a == b && pair(xs, ys),
        (Expr::List(xs), Expr::List(ys)) | (Expr::Tuple(xs), Expr::Tuple(ys)) => pair(xs, ys),
        (Expr::BinOp(a, al, ar), Expr::BinOp(b, bl, br)) => {
            a == b && matches(al, bl, variables, bindings) && matches(ar, br, variables, bindings)
        }
        (Expr::Neg(a), Expr::Neg(b)) => matches(a, b, variables, bindings),
        _ => false,
    }
}

pub(super) fn replace(t: &Term, from: &Term, to: &Term) -> Term {
    if t == from {
        to.clone()
    } else {
        map(t, &mut |child| replace(child, from, to))
    }
}

fn builtin(name: &str, args: Vec<Term>) -> Term {
    let ty = args.iter().find_map(|a| match a.ty() {
        Some(ty @ crate::ast::Type::List(_)) => Some(ty.clone()),
        _ => None,
    });
    let result = Spanned::bare(Expr::Call(Callee::Builtin(name.into()), args));
    if let Some(ty) = ty {
        result.set_ty(ty);
    }
    result
}

fn is_nil(t: &Term) -> bool {
    matches!(&t.node, Expr::List(xs) if xs.is_empty())
}

/// Builtin sequence normal forms only expose candidate terms. No normalized
/// equality is exported as a fact; the target checker still proves the claim.
pub(super) fn normalize(t: &Term) -> Term {
    let result = normalize_inner(t);
    if let Some(ty) = t.ty() {
        result.set_ty(ty.clone());
    }
    result
}

fn normalize_inner(t: &Term) -> Term {
    let t = map(t, &mut normalize);
    let Expr::Call(Callee::Builtin(name), args) = &t.node else {
        return t;
    };
    match (name.as_str(), args.as_slice()) {
        ("List.prepend", [head, tail]) => normalize(&builtin(
            "List.concat",
            vec![Spanned::bare(Expr::List(vec![head.clone()])), tail.clone()],
        )),
        ("List.concat", [a, b]) if is_nil(a) => b.clone(),
        ("List.concat", [a, b]) if is_nil(b) => a.clone(),
        ("List.reverse", [a]) => match &a.node {
            Expr::List(xs) => {
                let mut result = a.clone();
                result.node = Expr::List(xs.iter().rev().cloned().collect());
                result
            }
            Expr::Call(Callee::Builtin(n), xs) if n == "List.concat" && xs.len() == 2 => {
                normalize(&builtin(
                    "List.concat",
                    vec![
                        builtin("List.reverse", vec![xs[1].clone()]),
                        builtin("List.reverse", vec![xs[0].clone()]),
                    ],
                ))
            }
            _ => t,
        },
        _ => t,
    }
}

pub(super) fn size(t: &Term) -> usize {
    let mut count = 1;
    map(t, &mut |child| {
        count += size(child);
        child.clone()
    });
    count
}
