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
                    Box::new(int(&BigInt::from(0))),
                    Box::new(magnitude),
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
    if let Some(ty) = t.ty() {
        tail.set_ty(ty.clone());
    }
    Some(builtin(
        "List.prepend",
        vec![x.clone(), tail],
        t.ty().cloned(),
    ))
}

/// A list literal.
pub fn list(items: Vec<Term>) -> Term {
    Spanned::bare(ResolvedExpr::List(items))
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
    typed(ResolvedExpr::BinOp(op, Box::new(a), Box::new(b)), ty)
}

pub fn builtin(name: &str, args: Vec<Term>, ty: Option<Type>) -> Term {
    typed(
        ResolvedExpr::Call(ResolvedCallee::Builtin(name.to_string()), args),
        ty,
    )
}

pub fn intrinsic(which: BuiltinIntrinsic, args: Vec<Term>) -> Term {
    typed(
        ResolvedExpr::Call(ResolvedCallee::Intrinsic(which), args),
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
    if let Some(ty) = t.ty() {
        out.set_ty(ty.clone());
    }
    out
}

/// Canonical form: variables as `Ident`, tail calls as calls.
pub fn canon(t: &Term) -> Term {
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
        ResolvedExpr::Attr(o, field) => ResolvedExpr::Attr(b(o, f)?, field.clone()),
        ResolvedExpr::Call(callee, args) => ResolvedExpr::Call(callee.clone(), all(args, f)?),
        ResolvedExpr::BinOp(op, l, r) => ResolvedExpr::BinOp(*op, b(l, f)?, b(r, f)?),
        ResolvedExpr::Neg(a) => ResolvedExpr::Neg(b(a, f)?),
        ResolvedExpr::Match { subject, arms } => ResolvedExpr::Match {
            subject: b(subject, f)?,
            arms: arms
                .iter()
                .map(|arm| {
                    Ok(ResolvedMatchArm {
                        pattern: arm.pattern.clone(),
                        body: b(&arm.body, f)?,
                        binding_slots: std::sync::OnceLock::new(),
                    })
                })
                .collect::<Result<_, String>>()?,
        },
        ResolvedExpr::Ctor(c, args) => ResolvedExpr::Ctor(c.clone(), all(args, f)?),
        ResolvedExpr::ErrorProp(a) => ResolvedExpr::ErrorProp(b(a, f)?),
        ResolvedExpr::InterpolatedStr(parts) => ResolvedExpr::InterpolatedStr(
            parts
                .iter()
                .map(|p| match p {
                    ResolvedStrPart::Literal(s) => Ok(ResolvedStrPart::Literal(s.clone())),
                    ResolvedStrPart::Parsed(e) => Ok(ResolvedStrPart::Parsed(b(e, f)?)),
                })
                .collect::<Result<_, String>>()?,
        ),
        ResolvedExpr::List(xs) => ResolvedExpr::List(all(xs, f)?),
        ResolvedExpr::Tuple(xs) => ResolvedExpr::Tuple(all(xs, f)?),
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
            base: b(base, f)?,
            updates: updates
                .iter()
                .map(|(n, v)| Ok((n.clone(), f(v)?)))
                .collect::<Result<_, String>>()?,
        },
        ResolvedExpr::TailCall { target, args } => ResolvedExpr::TailCall {
            target: *target,
            args: all(args, f)?,
        },
        ResolvedExpr::IndependentProduct(xs, u) => {
            ResolvedExpr::IndependentProduct(all(xs, f)?, *u)
        }
    };
    Ok(retag(t, node))
}

/// Names a pattern binds.
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
                    body: Box::new(subst(&arm.body, &inner)?),
                    binding_slots: std::sync::OnceLock::new(),
                });
            }
            Ok(retag(
                t,
                ResolvedExpr::Match {
                    subject: Box::new(subject),
                    arms: new_arms,
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

/// Whether two constructor applications with no free variable are the same
/// value, where their constructors decide it: two different constructors of
/// one type differ whatever they hold, and one constructor with no fields
/// is equal to itself. `None` otherwise.
fn same_constructor(a: &Term, b: &Term) -> Option<bool> {
    use crate::ir::hir::{BuiltinCtor, ResolvedCtor};
    let (ResolvedExpr::Ctor(c, xs), ResolvedExpr::Ctor(d, ys)) = (&a.node, &b.node) else {
        return None;
    };
    let mut fv = Vec::new();
    free_vars(a, &mut fv);
    free_vars(b, &mut fv);
    if !fv.is_empty() {
        return None;
    }
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
