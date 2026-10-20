//! Element types for the empty lists of a step equation.
//!
//! A rule instance or a rewrite writes `[]` fresh, with no type: the `[]`
//! of `List.take([], n) = []`. Lean then has to guess what it is a list of,
//! and two guesses in one chain of equations need not agree (`List Unit`
//! on one side, `List Int` on the other), so the step term does not check.
//! Every `[]` of an equation stands for a value whose type the terms around
//! it already know: the other side of the equation, the equation the step
//! sits in, or the list builtin it is an argument of. This module takes the
//! type from there.

use crate::ast::Type;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
use crate::ir::proof_steps::Eqn;
use crate::ir::proof_steps::term::{self, HOLE, Term};

fn concrete(t: &Term) -> Option<&Type> {
    t.ty()
        .filter(|ty| crate::types::checker::type_is_fully_concrete(ty))
}

fn untyped_empty(t: &Term) -> bool {
    matches!(&t.node, ResolvedExpr::List(xs) if xs.is_empty()) && concrete(t).is_none()
}

/// The builtins whose result is a list of the same type as their list
/// arguments, with the positions of those arguments.
fn same_list_args(t: &Term) -> &'static [usize] {
    match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Builtin(b), _) => match b.as_str() {
            "List.take" | "List.drop" | "List.reverse" => &[0],
            "List.concat" => &[0, 1],
            "List.prepend" => &[1],
            _ => &[],
        },
        _ => &[],
    }
}

/// The type of `t`, when it or the lists it is built from know it.
fn known(t: &Term) -> Option<Type> {
    if let Some(ty) = concrete(t) {
        return Some(ty.clone());
    }
    let children = term::children(t);
    if let Some(ty) = same_list_args(t)
        .iter()
        .filter_map(|&i| children.get(i))
        .find_map(|c| known(c))
    {
        return Some(ty);
    }
    match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) if b == "List.prepend" => args
            .first()
            .and_then(known)
            .map(|e| Type::List(Box::new(e))),
        ResolvedExpr::List(xs) => xs.iter().find_map(known).map(|e| Type::List(Box::new(e))),
        _ => None,
    }
}

fn with_ty(t: &Term, ty: Type) -> Term {
    let out = t.clone();
    out.set_ty(ty);
    out
}

/// `t` with each untyped `[]` typed from the term it sits in: `expected`
/// for `t` itself, and a list builtin's type for its list arguments.
fn within(t: &Term, expected: Option<&Type>) -> Term {
    if untyped_empty(t) {
        return match expected {
            Some(ty) => with_ty(t, ty.clone()),
            None => t.clone(),
        };
    }
    let same = same_list_args(t);
    let own = known(t).or_else(|| expected.cloned());
    let mut i = 0;
    term::map_children(t, &mut |c| {
        let at = i;
        i += 1;
        let ty = if same.contains(&at) {
            own.as_ref()
        } else {
            None
        };
        Ok(within(c, ty))
    })
    .unwrap_or_else(|_| t.clone())
}

/// Whether `a` and `b` are the same node with their children in the same
/// positions, so the children pair up.
fn same_head(a: &Term, b: &Term) -> bool {
    use ResolvedExpr as E;
    match (&a.node, &b.node) {
        (E::Call(f, xs), E::Call(g, ys)) => f == g && xs.len() == ys.len(),
        (E::Ctor(f, xs), E::Ctor(g, ys)) => f == g && xs.len() == ys.len(),
        (E::BinOp(p, ..), E::BinOp(q, ..)) => p == q,
        (E::List(xs), E::List(ys)) | (E::Tuple(xs), E::Tuple(ys)) => xs.len() == ys.len(),
        (E::Attr(_, f), E::Attr(_, g)) => f == g,
        (E::Neg(_), E::Neg(_)) => true,
        _ => false,
    }
}

/// `t` with each untyped `[]` typed after `like`, a term of the same value
/// that shares `t`'s shape down to where the two differ: where `t` has a
/// `[]`, `like` has a term of the same type.
fn like(t: &Term, other: &Term) -> Term {
    if untyped_empty(t) {
        return match known(other) {
            Some(ty) => with_ty(t, ty),
            None => t.clone(),
        };
    }
    if !same_head(t, other) {
        return t.clone();
    }
    let others = term::children(other);
    let mut i = 0;
    term::map_children(t, &mut |c| {
        let r = match others.get(i) {
            Some(o) => like(c, o),
            None => c.clone(),
        };
        i += 1;
        Ok(r)
    })
    .unwrap_or_else(|_| t.clone())
}

/// `t` with its empty lists typed from itself and from `others`, terms of
/// the same value.
fn typed(t: &Term, others: &[&Term]) -> Term {
    let mut out = within(t, None);
    for o in others {
        out = like(&out, &within(o, None));
    }
    within(&out, None)
}

/// The equation `e` with its empty lists typed: from each side, from the
/// other side, and from `expected`, the same equation as the term around
/// the step states it.
pub(super) fn equation(e: &Eqn, expected: Option<&Eqn>) -> Eqn {
    let (lhs, rhs) = match expected {
        Some(x) => (typed(&e.lhs, &[&x.lhs]), typed(&e.rhs, &[&x.rhs])),
        None => (typed(&e.lhs, &[]), typed(&e.rhs, &[])),
    };
    let lhs = typed(&lhs, &[&rhs]);
    let rhs = typed(&rhs, &[&lhs]);
    Eqn::new(lhs, rhs)
}

/// The terms of a chain of equations, each typed from its neighbours.
pub(super) fn chain(terms: &[Term], expected: Option<&Eqn>) -> Vec<Term> {
    let mut out: Vec<Term> = terms.iter().map(|t| typed(t, &[])).collect();
    if let (Some(x), Some(first)) = (expected, out.first_mut()) {
        *first = typed(first, &[&x.lhs]);
    }
    if let (Some(x), Some(last)) = (expected, out.last_mut()) {
        *last = typed(last, &[&x.rhs]);
    }
    for i in 1..out.len() {
        out[i] = typed(&out[i], &[&out[i - 1]]);
    }
    for i in (0..out.len().saturating_sub(1)).rev() {
        out[i] = typed(&out[i], &[&out[i + 1]]);
    }
    out
}

/// The term at the hole of `ctx` within `t`, a term of `ctx`'s shape.
pub(super) fn at_hole(ctx: &Term, t: &Term) -> Option<Term> {
    at_name(ctx, HOLE, t)
}

/// The term at the variable `name` of `ctx` within `t`, a term of `ctx`'s
/// shape.
fn at_name(ctx: &Term, name: &str, t: &Term) -> Option<Term> {
    if matches!(&ctx.node, ResolvedExpr::Ident(n) if n == name) {
        return Some(t.clone());
    }
    if !same_head(ctx, t) {
        return None;
    }
    term::children(ctx)
        .into_iter()
        .zip(term::children(t))
        .find_map(|(c, u)| at_name(c, name, u))
}

/// The argument `value` a rule's lemma takes for its binder `name`, with
/// its empty lists typed after `eq`, the step's equation, which is the
/// rule's equation `schema` at that argument. A lemma over `List α` reads
/// an untyped `[]` among its arguments as a list of an element type still
/// to be found, and Lean then unfolds the list functions around it while
/// it unifies the step with its neighbours, which in a long chain of
/// steps runs out of time.
pub(super) fn argument(value: &Term, name: &str, schema: &Eqn, eq: &Eqn) -> Term {
    let around: Vec<Term> = [(&schema.lhs, &eq.lhs), (&schema.rhs, &eq.rhs)]
        .into_iter()
        .filter_map(|(s, t)| at_name(s, name, t))
        .collect();
    let around: Vec<&Term> = around.iter().collect();
    typed(value, &around)
}

/// The argument `value` at position `i` of the call `call`, the step's
/// side it rewrites, with its empty lists typed after that call's argument.
pub(super) fn call_argument(value: &Term, i: usize, call: &Term) -> Term {
    match &call.node {
        ResolvedExpr::Call(_, args) => match args.get(i) {
            Some(a) => typed(value, &[a]),
            None => typed(value, &[]),
        },
        _ => typed(value, &[]),
    }
}

/// The context of a congruence step with its empty lists typed after
/// `t`, the side of the equation it is the context of.
pub(super) fn context(ctx: &Term, t: &Term) -> Term {
    typed(ctx, &[t])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ints() -> Type {
        Type::List(Box::new(Type::Int))
    }

    fn typed_nil() -> Term {
        with_ty(&term::nil(), ints())
    }

    fn take(xs: Term) -> Term {
        term::builtin("List.take", vec![xs, term::var("n")], None)
    }

    fn len(xs: Term) -> Term {
        term::builtin("List.len", vec![xs], Some(Type::Int))
    }

    fn nil_ty(t: &Term) -> Option<Type> {
        if let ResolvedExpr::List(xs) = &t.node
            && xs.is_empty()
        {
            return t.ty().cloned();
        }
        term::children(t).into_iter().find_map(nil_ty)
    }

    #[test]
    fn an_empty_list_takes_the_type_of_the_list_it_rewrites() {
        // `len(take([] : List Int, n)) = len([])`: the `[]` the rule wrote
        // stands for `take([], n)`, a `List Int`.
        let e = equation(&Eqn::new(len(take(typed_nil())), len(term::nil())), None);
        assert_eq!(nil_ty(&e.rhs), Some(ints()));
    }

    #[test]
    fn a_step_takes_the_types_of_the_equation_around_it() {
        let expected = Eqn::new(take(typed_nil()), term::nil());
        let e = equation(&Eqn::new(take(term::nil()), term::nil()), Some(&expected));
        assert_eq!(nil_ty(&e.lhs), Some(ints()));
        assert_eq!(nil_ty(&e.rhs), Some(ints()));
    }

    #[test]
    fn a_chain_passes_types_along_and_a_hole_finds_its_term() {
        let terms = chain(&[len(take(typed_nil())), len(term::nil())], None);
        assert_eq!(nil_ty(&terms[1]), Some(ints()));
        let ctx = len(term::hole());
        let inner = at_hole(&ctx, &terms[0]).unwrap();
        assert_eq!(nil_ty(&inner), Some(ints()));
    }

    #[test]
    fn a_rule_argument_takes_the_type_of_the_equation_it_instantiates() {
        // `list_concat_cons x ([]) rest`: the `[]` the rule takes for `a`
        // sits at a `List Int` of the step's equation.
        let rule = crate::ir::proof_steps::WallRule::ConcatCons;
        let rest = with_ty(&term::var("rest"), ints());
        let x = term::var("x");
        x.set_ty(Type::Int);
        let subst = vec![
            ("x".to_string(), x),
            ("a".to_string(), term::nil()),
            ("b".to_string(), rest.clone()),
        ];
        let (_, concl) = rule.instantiate(&subst).unwrap();
        let eq = equation(&concl, None);
        let (_, schema) = rule.schema();
        let a = argument(&term::nil(), "a", &schema, &eq);
        assert_eq!(nil_ty(&a), Some(ints()));
        let b = argument(&rest, "b", &schema, &eq);
        assert_eq!(b.ty(), Some(&ints()));
    }

    #[test]
    fn an_empty_list_with_nothing_to_go_by_stays_untyped() {
        let e = equation(
            &Eqn::new(len(term::nil()), term::int(&num_bigint::BigInt::from(0))),
            None,
        );
        assert_eq!(nil_ty(&e.lhs), None);
    }
}
