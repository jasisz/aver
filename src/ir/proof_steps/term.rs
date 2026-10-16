//! Terms of the step format: resolved Aver expressions.
//!
//! A term is a [`ResolvedExpr`] in canonical form ([`canon`]): every variable
//! is an `Ident`, every call to a user function a `Call(Fn)`. Structural
//! equality (`==`, which ignores spans and slots) is the syntactic equality
//! the checkers compare with.

use num_bigint::BigInt;

use crate::ast::{BinOp, Literal, Spanned, Type};
use crate::ir::hir::{
    BuiltinIntrinsic, ResolvedCallee, ResolvedExpr, ResolvedMatchArm, ResolvedPattern,
    ResolvedStrPart,
};

pub type Term = Spanned<ResolvedExpr>;

/// The reserved variable that marks the rewrite position in a
/// [`super::Proof::Congr`] context. Not a legal Aver identifier.
pub const HOLE: &str = "__steps_hole";

fn typed(node: ResolvedExpr, ty: Option<Type>) -> Term {
    let t = Spanned::bare(node);
    if let Some(ty) = ty {
        t.set_ty(ty);
    }
    t
}

pub fn hole() -> Term {
    Spanned::bare(ResolvedExpr::Ident(HOLE.to_string()))
}

pub fn var(name: &str) -> Term {
    Spanned::bare(ResolvedExpr::Ident(name.to_string()))
}

pub fn int(value: &BigInt) -> Term {
    let lit = match i64::try_from(value) {
        Ok(v) if v >= 0 => Literal::Int(v),
        _ if value.sign() != num_bigint::Sign::Minus => Literal::BigInt(value.to_string()),
        _ => {
            // Aver has no negative literal: a negative value is `0 - n`.
            let magnitude = int(&(-value.clone()));
            return typed(
                ResolvedExpr::BinOp(
                    BinOp::Sub,
                    crate::ast::Shared::new(int(&BigInt::from(0))),
                    crate::ast::Shared::new(magnitude),
                ),
                Some(Type::Int),
            );
        }
    };
    typed(ResolvedExpr::Literal(lit), Some(Type::Int))
}

/// The empty list.
pub fn nil() -> Term {
    list(Vec::new())
}

/// `[x, …rest]` as `List.prepend(x, […rest])`, for a literal with at least
/// one element.
pub fn cell_of(t: &Term) -> Option<Term> {
    let ResolvedExpr::List(xs) = &t.node else {
        return None;
    };
    let (x, rest) = xs.split_first()?;
    let tail = list(rest.to_vec());
    if let Some(ty) = t.ty.get() {
        tail.set_ty_shared(ty);
    }
    Some(builtin(
        "List.prepend",
        vec![x.clone(), tail],
        t.ty().cloned(),
    ))
}

/// A list literal.
pub fn list(items: Vec<Term>) -> Term {
    Spanned::bare(ResolvedExpr::List(crate::ast::shared::vec(items)))
}

pub fn boolean(value: bool) -> Term {
    typed(
        ResolvedExpr::Literal(Literal::Bool(value)),
        Some(Type::Bool),
    )
}

pub fn binop(op: BinOp, a: Term, b: Term) -> Term {
    let ty = match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul => a.ty().cloned().or(Some(Type::Int)),
        BinOp::Div => a.ty().cloned(),
        _ => Some(Type::Bool),
    };
    typed(ResolvedExpr::BinOp(op, crate::ast::Shared::new(a), crate::ast::Shared::new(b)), ty)
}

pub fn builtin(name: &str, args: Vec<Term>, ty: Option<Type>) -> Term {
    typed(
        ResolvedExpr::Call(ResolvedCallee::Builtin(name.to_string()), crate::ast::shared::vec(args)),
        ty,
    )
}

pub fn intrinsic(which: BuiltinIntrinsic, args: Vec<Term>) -> Term {
    typed(
        ResolvedExpr::Call(ResolvedCallee::Intrinsic(which), crate::ast::shared::vec(args)),
        Some(Type::Int),
    )
}

pub fn bool_and(a: Term, b: Term) -> Term {
    builtin("Bool.and", vec![a, b], Some(Type::Bool))
}

pub fn bool_or(a: Term, b: Term) -> Term {
    builtin("Bool.or", vec![a, b], Some(Type::Bool))
}

pub fn bool_not(a: Term) -> Term {
    builtin("Bool.not", vec![a], Some(Type::Bool))
}

/// The value of an integer literal term (`7`, a big literal, or `0 - n`).
pub fn int_value(t: &Term) -> Option<BigInt> {
    match &t.node {
        ResolvedExpr::Literal(Literal::Int(v)) => Some(BigInt::from(*v)),
        ResolvedExpr::Literal(Literal::BigInt(s)) => s.parse().ok(),
        ResolvedExpr::BinOp(BinOp::Sub, a, b) if int_value(a) == Some(BigInt::from(0)) => {
            int_value(b).map(|v| -v)
        }
        ResolvedExpr::Neg(a) => int_value(a).map(|v| -v),
        _ => None,
    }
}

pub fn bool_value(t: &Term) -> Option<bool> {
    match &t.node {
        ResolvedExpr::Literal(Literal::Bool(b)) => Some(*b),
        _ => None,
    }
}

pub fn is_literal(t: &Term) -> bool {
    matches!(t.node, ResolvedExpr::Literal(_)) || int_value(t).is_some()
}

fn retag(t: &Term, node: ResolvedExpr) -> Term {
    let out = Spanned::new(node, t.line);
    if let Some(ty) = t.ty.get() {
        out.set_ty_shared(ty);
    }
    out
}

/// Canonical form: variables as `Ident`, tail calls as calls.
pub fn canon(t: &Term) -> Term {
    if canon_keeps(t) {
        return t.clone();
    }
    let node = match &t.node {
        ResolvedExpr::Resolved { name, .. } => ResolvedExpr::Ident(name.clone()),
        ResolvedExpr::TailCall { target, args } => ResolvedExpr::Call(
            ResolvedCallee::Fn(*target),
            args.iter().map(canon).collect(),
        ),
        _ => return map_children(t, &mut |c| Ok(canon(c))).expect("canon is total"),
    };
    retag(t, node)
}

/// Whether `t` is in canonical form already: no `Resolved` variable and no
/// tail call in it, where [`canon`] rewrites nothing and gives a term equal
/// to `t`. Visits every part [`map_children`] does.
pub fn is_canon(t: &Term) -> bool {
    match &t.node {
        ResolvedExpr::Resolved { .. } | ResolvedExpr::TailCall { .. } => false,
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) => true,
        ResolvedExpr::Attr(o, _) | ResolvedExpr::Neg(o) | ResolvedExpr::ErrorProp(o) => is_canon(o),
        ResolvedExpr::Call(_, xs)
        | ResolvedExpr::Ctor(_, xs)
        | ResolvedExpr::List(xs)
        | ResolvedExpr::Tuple(xs)
        | ResolvedExpr::IndependentProduct(xs, _) => xs.iter().all(is_canon),
        ResolvedExpr::BinOp(_, l, r) => is_canon(l) && is_canon(r),
        ResolvedExpr::Match { subject, arms } => {
            is_canon(subject) && arms.iter().all(|arm| is_canon(&arm.body))
        }
        ResolvedExpr::InterpolatedStr(parts) => parts.iter().all(|p| match p {
            ResolvedStrPart::Literal(_) => true,
            ResolvedStrPart::Parsed(e) => is_canon(e),
        }),
        ResolvedExpr::MapLiteral(kvs) => kvs.iter().all(|(k, v)| is_canon(k) && is_canon(v)),
        ResolvedExpr::RecordCreate { fields, .. } => fields.iter().all(|(_, v)| is_canon(v)),
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            is_canon(base) && updates.iter().all(|(_, v)| is_canon(v))
        }
    }
}

/// Whether [`canon`] gives `t` back exactly (also each match arm's slots,
/// which rebuilding drops): canonical already, with no slots on an arm.
fn canon_keeps(t: &Term) -> bool {
    match &t.node {
        ResolvedExpr::Match { subject, arms } => {
            canon_keeps(subject)
                && arms
                    .iter()
                    .all(|arm| arm.binding_slots.get().is_none() && canon_keeps(&arm.body))
        }
        ResolvedExpr::Resolved { .. } | ResolvedExpr::TailCall { .. } => false,
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) => true,
        ResolvedExpr::Attr(o, _) | ResolvedExpr::Neg(o) | ResolvedExpr::ErrorProp(o) => {
            canon_keeps(o)
        }
        ResolvedExpr::Call(_, xs)
        | ResolvedExpr::Ctor(_, xs)
        | ResolvedExpr::List(xs)
        | ResolvedExpr::Tuple(xs)
        | ResolvedExpr::IndependentProduct(xs, _) => xs.iter().all(canon_keeps),
        ResolvedExpr::BinOp(_, l, r) => canon_keeps(l) && canon_keeps(r),
        ResolvedExpr::InterpolatedStr(parts) => parts.iter().all(|p| match p {
            ResolvedStrPart::Literal(_) => true,
            ResolvedStrPart::Parsed(e) => canon_keeps(e),
        }),
        ResolvedExpr::MapLiteral(kvs) => kvs.iter().all(|(k, v)| canon_keeps(k) && canon_keeps(v)),
        ResolvedExpr::RecordCreate { fields, .. } => fields.iter().all(|(_, v)| canon_keeps(v)),
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            canon_keeps(base) && updates.iter().all(|(_, v)| canon_keeps(v))
        }
    }
}

/// [`canon`] of `t` for reading: `t` itself where it is canonical already
/// ([`is_canon`]), so comparing with it builds nothing.
pub fn canon_ref(t: &Term) -> std::borrow::Cow<'_, Term> {
    if is_canon(t) {
        std::borrow::Cow::Borrowed(t)
    } else {
        std::borrow::Cow::Owned(canon(t))
    }
}

/// Whether `canon(a) == canon(b)`, without building either where it is
/// canonical already.
pub fn canon_eq(a: &Term, b: &Term) -> bool {
    *canon_ref(a) == *canon_ref(b)
}

/// Whether `a` and `b` are the same term part for part, with the type and
/// source line of every part and the slots of every `match` arm: what
/// their `Debug` text shows, which structural equality (`==`) partly
/// ignores.
pub fn exact_eq(a: &Term, b: &Term) -> bool {
    let all = |xs: &[Term], ys: &[Term]| {
        xs.len() == ys.len() && xs.iter().zip(ys).all(|(x, y)| exact_eq(x, y))
    };
    let named = |xs: &[(String, Term)], ys: &[(String, Term)]| {
        xs.len() == ys.len()
            && xs
                .iter()
                .zip(ys)
                .all(|((n, x), (m, y))| n == m && exact_eq(x, y))
    };
    if a.line != b.line || a.ty() != b.ty() {
        return false;
    }
    match (&a.node, &b.node) {
        // A Float by its bits: `0.0 == -0.0` and `NaN != NaN`.
        (ResolvedExpr::Literal(Literal::Float(x)), ResolvedExpr::Literal(Literal::Float(y))) => {
            x.to_bits() == y.to_bits()
        }
        (ResolvedExpr::Literal(x), ResolvedExpr::Literal(y)) => x == y,
        (ResolvedExpr::Ident(x), ResolvedExpr::Ident(y)) => x == y,
        (
            ResolvedExpr::Resolved {
                slot: s1,
                name: n1,
                last_use: l1,
            },
            ResolvedExpr::Resolved {
                slot: s2,
                name: n2,
                last_use: l2,
            },
        ) => s1 == s2 && n1 == n2 && l1.0 == l2.0,
        (ResolvedExpr::Attr(x, f), ResolvedExpr::Attr(y, g)) => f == g && exact_eq(x, y),
        (ResolvedExpr::Call(c, xs), ResolvedExpr::Call(d, ys)) => c == d && all(xs, ys),
        (ResolvedExpr::BinOp(o, l1, r1), ResolvedExpr::BinOp(p, l2, r2)) => {
            o == p && exact_eq(l1, l2) && exact_eq(r1, r2)
        }
        (ResolvedExpr::Neg(x), ResolvedExpr::Neg(y))
        | (ResolvedExpr::ErrorProp(x), ResolvedExpr::ErrorProp(y)) => exact_eq(x, y),
        (
            ResolvedExpr::Match {
                subject: x,
                arms: xs,
            },
            ResolvedExpr::Match {
                subject: y,
                arms: ys,
            },
        ) => {
            exact_eq(x, y)
                && xs.len() == ys.len()
                && xs.iter().zip(ys).all(|(p, q)| {
                    format!("{:?}", p.pattern) == format!("{:?}", q.pattern)
                        && p.binding_slots.get() == q.binding_slots.get()
                        && exact_eq(&p.body, &q.body)
                })
        }
        (ResolvedExpr::Ctor(c, xs), ResolvedExpr::Ctor(d, ys)) => c == d && all(xs, ys),
        (ResolvedExpr::InterpolatedStr(xs), ResolvedExpr::InterpolatedStr(ys)) => {
            xs.len() == ys.len()
                && xs.iter().zip(ys).all(|(p, q)| match (p, q) {
                    (ResolvedStrPart::Literal(x), ResolvedStrPart::Literal(y)) => x == y,
                    (ResolvedStrPart::Parsed(x), ResolvedStrPart::Parsed(y)) => exact_eq(x, y),
                    _ => false,
                })
        }
        (ResolvedExpr::List(xs), ResolvedExpr::List(ys))
        | (ResolvedExpr::Tuple(xs), ResolvedExpr::Tuple(ys)) => all(xs, ys),
        (ResolvedExpr::MapLiteral(xs), ResolvedExpr::MapLiteral(ys)) => {
            xs.len() == ys.len()
                && xs
                    .iter()
                    .zip(ys)
                    .all(|((k1, v1), (k2, v2))| exact_eq(k1, k2) && exact_eq(v1, v2))
        }
        (
            ResolvedExpr::RecordCreate {
                type_id: t1,
                type_name: n1,
                fields: f1,
            },
            ResolvedExpr::RecordCreate {
                type_id: t2,
                type_name: n2,
                fields: f2,
            },
        ) => t1 == t2 && n1 == n2 && named(f1, f2),
        (
            ResolvedExpr::RecordUpdate {
                type_id: t1,
                type_name: n1,
                base: b1,
                updates: u1,
            },
            ResolvedExpr::RecordUpdate {
                type_id: t2,
                type_name: n2,
                base: b2,
                updates: u2,
            },
        ) => t1 == t2 && n1 == n2 && exact_eq(b1, b2) && named(u1, u2),
        (
            ResolvedExpr::TailCall {
                target: t1,
                args: a1,
            },
            ResolvedExpr::TailCall {
                target: t2,
                args: a2,
            },
        ) => t1 == t2 && all(a1, a2),
        (ResolvedExpr::IndependentProduct(xs, u), ResolvedExpr::IndependentProduct(ys, v)) => {
            u == v && all(xs, ys)
        }
        _ => false,
    }
}

/// Whether `canon(subst(t, map))` may equal `want` for some `map`: a
/// substitution keeps the outermost part of `t` (a tail call read as a
/// call) unless it is a variable.
pub fn may_become(t: &Term, want: &Term) -> bool {
    use std::mem::discriminant;
    match (&t.node, &want.node) {
        (ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. }, _) => true,
        (ResolvedExpr::TailCall { target, .. }, ResolvedExpr::Call(c, _)) => {
            *c == ResolvedCallee::Fn(*target)
        }
        (ResolvedExpr::Call(c, _), ResolvedExpr::Call(d, _)) => c == d,
        (ResolvedExpr::BinOp(o, _, _), ResolvedExpr::BinOp(p, _, _)) => o == p,
        (ResolvedExpr::TailCall { .. }, _) => false,
        (a, b) => discriminant(a) == discriminant(b),
    }
}

/// Rebuild `t` with `f` applied to every direct child (match arm bodies
/// included).
pub fn map_children(
    t: &Term,
    f: &mut dyn FnMut(&Term) -> Result<Term, String>,
) -> Result<Term, String> {
    let b = |x: &Term,
             f: &mut dyn FnMut(&Term) -> Result<Term, String>|
     -> Result<Box<Term>, String> { Ok(Box::new(f(x)?)) };
    let all = |xs: &[Term],
               f: &mut dyn FnMut(&Term) -> Result<Term, String>|
     -> Result<Vec<Term>, String> { xs.iter().map(&mut *f).collect() };
    let node = match &t.node {
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {
            return Ok(t.clone());
        }
        ResolvedExpr::Attr(o, field) => ResolvedExpr::Attr(crate::ast::Shared::from(b(o, f)?), field.clone()),
        ResolvedExpr::Call(callee, args) => ResolvedExpr::Call(callee.clone(), crate::ast::shared::vec(all(args, f)?)),
        ResolvedExpr::BinOp(op, l, r) => ResolvedExpr::BinOp(*op, crate::ast::Shared::from(b(l, f)?), crate::ast::Shared::from(b(r, f)?)),
        ResolvedExpr::Neg(a) => ResolvedExpr::Neg(crate::ast::Shared::from(b(a, f)?)),
        ResolvedExpr::Match { subject, arms } => ResolvedExpr::Match {
            subject: crate::ast::Shared::from(b(subject, f)?),
            arms: arms
                .iter()
                .map(|arm| {
                    Ok(ResolvedMatchArm {
                        pattern: arm.pattern.clone(),
                        body: crate::ast::Shared::from(b(&arm.body, f)?),
                        binding_slots: std::sync::OnceLock::new(),
                    })
                })
                .collect::<Result<_, String>>()?,
        },
        ResolvedExpr::Ctor(c, args) => ResolvedExpr::Ctor(c.clone(), crate::ast::shared::vec(all(args, f)?)),
        ResolvedExpr::ErrorProp(a) => ResolvedExpr::ErrorProp(crate::ast::Shared::from(b(a, f)?)),
        ResolvedExpr::InterpolatedStr(parts) => ResolvedExpr::InterpolatedStr(
            parts
                .iter()
                .map(|p| match p {
                    ResolvedStrPart::Literal(s) => Ok(ResolvedStrPart::Literal(s.clone())),
                    ResolvedStrPart::Parsed(e) => Ok(ResolvedStrPart::Parsed(crate::ast::Shared::from(b(e, f)?))),
                })
                .collect::<Result<_, String>>()?,
        ),
        ResolvedExpr::List(xs) => ResolvedExpr::List(crate::ast::shared::vec(all(xs, f)?)),
        ResolvedExpr::Tuple(xs) => ResolvedExpr::Tuple(crate::ast::shared::vec(all(xs, f)?)),
        ResolvedExpr::MapLiteral(kvs) => ResolvedExpr::MapLiteral(
            kvs.iter()
                .map(|(k, v)| Ok((f(k)?, f(v)?)))
                .collect::<Result<_, String>>()?,
        ),
        ResolvedExpr::RecordCreate {
            type_id,
            type_name,
            fields,
        } => ResolvedExpr::RecordCreate {
            type_id: *type_id,
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(n, v)| Ok((n.clone(), f(v)?)))
                .collect::<Result<_, String>>()?,
        },
        ResolvedExpr::RecordUpdate {
            type_id,
            type_name,
            base,
            updates,
        } => ResolvedExpr::RecordUpdate {
            type_id: *type_id,
            type_name: type_name.clone(),
            base: crate::ast::Shared::from(b(base, f)?),
            updates: updates
                .iter()
                .map(|(n, v)| Ok((n.clone(), f(v)?)))
                .collect::<Result<_, String>>()?,
        },
        ResolvedExpr::TailCall { target, args } => ResolvedExpr::TailCall {
            target: *target,
            args: crate::ast::shared::vec(all(args, f)?),
        },
        ResolvedExpr::IndependentProduct(xs, u) => {
            ResolvedExpr::IndependentProduct(crate::ast::shared::vec(all(xs, f)?), *u)
        }
    };
    Ok(retag(t, node))
}

/// Names a pattern binds.
/// The names a pattern's parts take, in order, a wildcard part as `_`: the
/// binders a step gives a constructor or cons pattern line up with these.
pub fn pattern_parts(p: &ResolvedPattern) -> Vec<String> {
    match p {
        ResolvedPattern::Cons(h, t) => vec![h.clone(), t.clone()],
        ResolvedPattern::Ctor(_, names) => names.clone(),
        _ => pattern_binders(p),
    }
}

pub fn pattern_binders(p: &ResolvedPattern) -> Vec<String> {
    match p {
        ResolvedPattern::Wildcard | ResolvedPattern::Literal(_) | ResolvedPattern::EmptyList => {
            Vec::new()
        }
        ResolvedPattern::Ident(n) => vec![n.clone()],
        ResolvedPattern::Cons(h, t) => vec![h.clone(), t.clone()],
        ResolvedPattern::Tuple(ps) => ps.iter().flat_map(pattern_binders).collect(),
        ResolvedPattern::Ctor(_, names) => names.clone(),
    }
    .into_iter()
    .filter(|n| n != "_")
    .collect()
}

pub fn free_vars(t: &Term, out: &mut Vec<String>) {
    match &t.node {
        ResolvedExpr::Ident(n) | ResolvedExpr::Resolved { name: n, .. } => {
            if !out.contains(n) {
                out.push(n.clone());
            }
        }
        ResolvedExpr::Match { subject, arms } => {
            free_vars(subject, out);
            for arm in arms {
                let bound = pattern_binders(&arm.pattern);
                let mut inner = Vec::new();
                free_vars(&arm.body, &mut inner);
                for n in inner {
                    if !bound.contains(&n) && !out.contains(&n) {
                        out.push(n);
                    }
                }
            }
        }
        _ => {
            let _ = map_children(t, &mut |c| {
                free_vars(c, out);
                Ok(c.clone())
            });
        }
    }
}

/// Simultaneous substitution of free variables. Refuses (instead of
/// renaming) when a substituted term would be captured by a pattern binder.
pub fn subst(t: &Term, map: &[(String, Term)]) -> Result<Term, String> {
    match &t.node {
        ResolvedExpr::Ident(n) | ResolvedExpr::Resolved { name: n, .. } => {
            Ok(match map.iter().find(|(k, _)| k == n) {
                Some((_, v)) => v.clone(),
                None => canon(t),
            })
        }
        ResolvedExpr::Match { subject, arms } => {
            let subject = subst(subject, map)?;
            let mut new_arms = Vec::new();
            for arm in arms {
                let bound = pattern_binders(&arm.pattern);
                let inner: Vec<(String, Term)> = map
                    .iter()
                    .filter(|(k, _)| !bound.contains(k))
                    .cloned()
                    .collect();
                let mut arm_free = Vec::new();
                free_vars(&arm.body, &mut arm_free);
                for (k, v) in &inner {
                    if !arm_free.contains(k) {
                        continue;
                    }
                    let mut fv = Vec::new();
                    free_vars(v, &mut fv);
                    if let Some(c) = fv.iter().find(|n| bound.contains(n)) {
                        return Err(format!("substituting {k} would capture {c}"));
                    }
                }
                new_arms.push(ResolvedMatchArm {
                    pattern: arm.pattern.clone(),
                    body: crate::ast::Shared::new(subst(&arm.body, &inner)?),
                    binding_slots: std::sync::OnceLock::new(),
                });
            }
            Ok(retag(
                t,
                ResolvedExpr::Match {
                    subject: crate::ast::Shared::new(subject),
                    arms: crate::ast::shared::vec(new_arms),
                },
            ))
        }
        ResolvedExpr::TailCall { .. } => subst(&canon(t), map),
        _ => map_children(t, &mut |c| subst(c, map)),
    }
}

/// Replace the hole of `ctx` with `t`.
pub fn plug(ctx: &Term, t: &Term) -> Term {
    subst(ctx, &[(HOLE.to_string(), t.clone())]).expect("a hole is never under a binder")
}

/// Direct children in position order (match arm bodies excluded: the
/// producers never rewrite under a binder).
pub fn children(t: &Term) -> Vec<&Term> {
    match &t.node {
        ResolvedExpr::Attr(o, _) | ResolvedExpr::Neg(o) | ResolvedExpr::ErrorProp(o) => vec![o],
        ResolvedExpr::Call(_, args) | ResolvedExpr::Ctor(_, args) => args.iter().collect(),
        ResolvedExpr::TailCall { args, .. } => args.iter().collect(),
        ResolvedExpr::BinOp(_, l, r) => vec![l, r],
        ResolvedExpr::Match { subject, .. } => vec![subject],
        ResolvedExpr::List(xs) | ResolvedExpr::Tuple(xs) => xs.iter().collect(),
        ResolvedExpr::RecordCreate { fields, .. } => fields.iter().map(|(_, v)| v).collect(),
        ResolvedExpr::RecordUpdate { base, updates, .. } => std::iter::once(&**base)
            .chain(updates.iter().map(|(_, v)| v))
            .collect(),
        ResolvedExpr::InterpolatedStr(parts) => parts
            .iter()
            .filter_map(|p| match p {
                ResolvedStrPart::Parsed(e) => Some(&**e),
                ResolvedStrPart::Literal(_) => None,
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// `t` with the child at `index` replaced by `new`.
pub fn with_child(t: &Term, index: usize, new: Term) -> Term {
    let mut i = 0usize;
    let mut slot = Some(new);
    let mut in_match_body = false;
    map_children(t, &mut |c| {
        // Match arm bodies are visited by map_children but are not
        // positions; count only `children()` entries.
        if in_match_body {
            return Ok(c.clone());
        }
        let out = if i == index {
            slot.take().unwrap_or_else(|| c.clone())
        } else {
            c.clone()
        };
        i += 1;
        if matches!(t.node, ResolvedExpr::Match { .. }) {
            in_match_body = true;
        }
        Ok(out)
    })
    .expect("with_child is total")
}

/// The context of position `path` in `t`: `t` with a hole there.
pub fn context_at(t: &Term, path: &[usize]) -> Term {
    match path.split_first() {
        None => hole(),
        Some((i, rest)) => {
            let child = children(t)[*i].clone();
            with_child(t, *i, context_at(&child, rest))
        }
    }
}

pub fn at<'a>(t: &'a Term, path: &[usize]) -> &'a Term {
    match path.split_first() {
        None => t,
        Some((i, rest)) => at(children(t)[*i], rest),
    }
}

/// Evaluate a closed term built from literals, Int arithmetic, Euclidean
/// division by a literal, comparisons, Bool operations, and lists of such
/// values with `List.prepend`, `concat`, `len`, `reverse`, `take` and `drop`
/// (a count below zero counts as zero). `None` when the term has any other
/// shape.
pub fn eval_closed(t: &Term) -> Option<Term> {
    #[derive(Clone, PartialEq)]
    enum V {
        I(BigInt),
        B(bool),
        L(Vec<V>),
    }
    fn go(t: &Term) -> Option<V> {
        if let Some(i) = int_value(t) {
            return Some(V::I(i));
        }
        if let Some(b) = bool_value(t) {
            return Some(V::B(b));
        }
        let ints = |a: &Term, b: &Term| match (go(a)?, go(b)?) {
            (V::I(x), V::I(y)) => Some((x, y)),
            _ => None,
        };
        let list = |a: &Term| match go(a)? {
            V::L(xs) => Some(xs),
            _ => None,
        };
        let count = |n: &Term| -> Option<usize> {
            match go(n)? {
                V::I(k) if k.sign() == num_bigint::Sign::Minus => Some(0),
                V::I(k) => Some(usize::try_from(k).unwrap_or(usize::MAX)),
                _ => None,
            }
        };
        let bools = |args: &[Term]| -> Option<Vec<bool>> {
            args.iter()
                .map(|a| match go(a)? {
                    V::B(b) => Some(b),
                    _ => None,
                })
                .collect()
        };
        match &t.node {
            ResolvedExpr::BinOp(op, a, b) => match op {
                BinOp::Add => ints(a, b).map(|(x, y)| V::I(x + y)),
                BinOp::Sub => ints(a, b).map(|(x, y)| V::I(x - y)),
                BinOp::Mul => ints(a, b).map(|(x, y)| V::I(x * y)),
                BinOp::Lt => ints(a, b).map(|(x, y)| V::B(x < y)),
                BinOp::Gt => ints(a, b).map(|(x, y)| V::B(x > y)),
                BinOp::Lte => ints(a, b).map(|(x, y)| V::B(x <= y)),
                BinOp::Gte => ints(a, b).map(|(x, y)| V::B(x >= y)),
                BinOp::Eq | BinOp::Neq => {
                    if let Some(same) = same_constructor(a, b) {
                        return Some(V::B(if matches!(op, BinOp::Eq) { same } else { !same }));
                    }
                    let same = match (go(a)?, go(b)?) {
                        (V::I(x), V::I(y)) => x == y,
                        (V::B(x), V::B(y)) => x == y,
                        (V::L(x), V::L(y)) => x == y,
                        _ => return None,
                    };
                    Some(V::B(if matches!(op, BinOp::Eq) { same } else { !same }))
                }
                BinOp::Div => None,
            },
            ResolvedExpr::Call(ResolvedCallee::Intrinsic(w), args) if args.len() == 2 => {
                let (x, k) = ints(&args[0], &args[1])?;
                if k == BigInt::from(0) {
                    return None;
                }
                let (q, r) = euclid(&x, &k);
                match w {
                    BuiltinIntrinsic::IntDivEuclid => Some(V::I(q)),
                    BuiltinIntrinsic::IntModEuclid => Some(V::I(r)),
                    _ => None,
                }
            }
            ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) => match name.as_str() {
                "Bool.and" => bools(args).map(|v| V::B(v.iter().all(|b| *b))),
                "Bool.or" => bools(args).map(|v| V::B(v.iter().any(|b| *b))),
                "Bool.not" if args.len() == 1 => bools(args).map(|v| V::B(!v[0])),
                "List.prepend" if args.len() == 2 => {
                    let mut xs = list(&args[1])?;
                    xs.insert(0, go(&args[0])?);
                    Some(V::L(xs))
                }
                "List.concat" if args.len() == 2 => {
                    let mut xs = list(&args[0])?;
                    xs.extend(list(&args[1])?);
                    Some(V::L(xs))
                }
                "List.len" if args.len() == 1 => Some(V::I(BigInt::from(list(&args[0])?.len()))),
                "List.reverse" if args.len() == 1 => {
                    let mut xs = list(&args[0])?;
                    xs.reverse();
                    Some(V::L(xs))
                }
                "List.take" if args.len() == 2 => {
                    let xs = list(&args[0])?;
                    let n = count(&args[1])?.min(xs.len());
                    Some(V::L(xs[..n].to_vec()))
                }
                "List.drop" if args.len() == 2 => {
                    let xs = list(&args[0])?;
                    let n = count(&args[1])?.min(xs.len());
                    Some(V::L(xs[n..].to_vec()))
                }
                _ => None,
            },
            ResolvedExpr::List(xs) => Some(V::L(xs.iter().map(go).collect::<Option<_>>()?)),
            _ => None,
        }
    }
    fn back(v: V) -> Term {
        match v {
            V::I(i) => int(&i),
            V::B(b) => boolean(b),
            V::L(xs) => Spanned::bare(ResolvedExpr::List(xs.into_iter().map(back).collect())),
        }
    }
    Some(back(go(t)?))
}

/// Whether two constructor applications are the same value, where their
/// constructors decide it: two different constructors of one type differ
/// whatever they hold, variables too (`Option.Some(x) == Option.None` is
/// `false`), and one constructor with no fields is equal to itself. `None`
/// otherwise.
fn same_constructor(a: &Term, b: &Term) -> Option<bool> {
    use crate::ir::hir::{BuiltinCtor, ResolvedCtor};
    let (ResolvedExpr::Ctor(c, xs), ResolvedExpr::Ctor(d, ys)) = (&a.node, &b.node) else {
        return None;
    };
    let distinct = match (c, d) {
        (
            ResolvedCtor::User {
                ctor_id: c1,
                type_id: t1,
                ..
            },
            ResolvedCtor::User {
                ctor_id: c2,
                type_id: t2,
                ..
            },
        ) if t1 == t2 => c1 != c2,
        (ResolvedCtor::Builtin(b1), ResolvedCtor::Builtin(b2)) => {
            let option =
                |b: &BuiltinCtor| matches!(b, BuiltinCtor::OptionSome | BuiltinCtor::OptionNone);
            if option(b1) != option(b2) {
                return None;
            }
            b1 != b2
        }
        _ => return None,
    };
    if distinct {
        Some(false)
    } else {
        (xs.is_empty() && ys.is_empty()).then_some(true)
    }
}

/// Euclidean division: `x = q*k + r`, `0 <= r < |k|`.
pub fn euclid(x: &BigInt, k: &BigInt) -> (BigInt, BigInt) {
    use num_traits::Signed;
    let mut q = x / k;
    let mut r = x - &q * k;
    if r.is_negative() {
        if k.is_positive() {
            q -= 1;
            r += k;
        } else {
            q += 1;
            r -= k;
        }
    }
    (q, r)
}
