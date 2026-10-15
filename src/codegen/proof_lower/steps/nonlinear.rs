//! Products of comparisons for linear arithmetic, as `nlinarith` adds
//! them: where the comparisons in scope do not settle a goal about a
//! product of two Int terms, each pair of them, read as `p >= 0` and
//! `q >= 0`, gives `0 <= p * q` (`int.mul_nonneg`), and linear arithmetic
//! reads that product multiplied out with the rest. The opposite of the
//! goal is a factor too, in the case of a split on the goal where it
//! holds. Only a goal with a monomial of degree two or more, or one about a
//! quotient or a remainder by a divisor that is not a literal, is read so
//! (see [`wanted`]); only a product that shares such a monomial is kept,
//! and only a few comparisons are paired.
//!
//! Each factor's own `0 <= p` is a one-line linear step from its
//! comparison, and the product is one rule instance; the kernels check
//! both, nothing here is trusted.

use num_bigint::BigInt;

use crate::ast::BinOp;
use crate::ir::hir::ResolvedExpr;
use crate::ir::proof_steps::linear;
use crate::ir::proof_steps::ring::Poly;
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, Proof, WallRule};

/// Most comparisons paired with each other (squares included), so at most
/// `n * (n + 1) / 2` products.
const MAX_FACTORS: usize = 6;

/// The name a factor's own fact has inside the step that reads it.
const FACTOR: &str = "h_factor";

/// The hypothesis that the goal does not hold, in the case of a split on
/// the goal where a product reads it as a factor.
pub(super) const OPPOSITE: &str = "h_opposite";

/// Where a comparison comes from: a hypothesis in scope by name, or a fact
/// with its proof (`fact = true`).
#[derive(Clone)]
pub(super) enum Source {
    Hyp(String),
    Fact(Box<Proof>),
}

/// The monomials of degree two or more a product must share to be kept:
/// the goal's (`rows[0]`), or, for a goal that reads a quotient or a
/// remainder by a divisor that is not a literal, those of the other rows
/// about an atom of the goal (`q * d` beside `q`). None for any other goal,
/// which products are not tried for.
pub(super) fn wanted(rows: &[Poly], atoms: &[Term]) -> Vec<Vec<usize>> {
    use crate::ir::hir::{BuiltinIntrinsic, ResolvedCallee};
    let Some(goal) = rows.first() else {
        return Vec::new();
    };
    let own: Vec<Vec<usize>> = goal.keys().filter(|m| m.len() >= 2).cloned().collect();
    if !own.is_empty() {
        return own;
    }
    let named_division = |t: &Term| {
        matches!(
            &t.node,
            ResolvedExpr::Call(
                ResolvedCallee::Intrinsic(BuiltinIntrinsic::IntDivEuclid | BuiltinIntrinsic::IntModEuclid),
                args,
            ) if args.len() == 2 && term::int_value(&args[1]).is_none()
        )
    };
    let read: Vec<usize> = goal.keys().flatten().copied().collect();
    if !read.iter().any(|&i| named_division(&atoms[i])) {
        return Vec::new();
    }
    let mut out = Vec::new();
    for row in &rows[1..] {
        for m in row.keys() {
            if m.len() >= 2 && m.iter().any(|i| read.contains(i)) && !out.contains(m) {
                out.push(m.clone());
            }
        }
    }
    out
}

/// The comparison `t = value` as `0 <= p`, `p` the term whose polynomial
/// is the row [`linear::as_nonneg`] reads: `b - a` for `a <= b`,
/// `b - a - 1` for `a < b`, a false comparison as its complement.
fn nonneg_side(t: &Term, value: bool) -> Option<Term> {
    let ResolvedExpr::BinOp(op, a, b) = &t.node else {
        return None;
    };
    let (lower, upper, strict) = match (op, value) {
        (BinOp::Lt, true) => (a, b, true),
        (BinOp::Lte, true) => (a, b, false),
        (BinOp::Gt, true) => (b, a, true),
        (BinOp::Gte, true) => (b, a, false),
        (BinOp::Lt, false) => (b, a, false),
        (BinOp::Lte, false) => (b, a, true),
        (BinOp::Gt, false) => (a, b, false),
        (BinOp::Gte, false) => (a, b, true),
        _ => return None,
    };
    let diff = match term::int_value(lower) {
        Some(k) if k == BigInt::from(0) => canon(upper),
        _ => term::binop(BinOp::Sub, canon(upper), canon(lower)),
    };
    Some(canon(&if strict {
        term::binop(BinOp::Sub, diff, term::int(&BigInt::from(1)))
    } else {
        diff
    }))
}

/// `(0 <= p) = true` from the comparison `e` it is read from: the two
/// rows add up to `-1 >= 0`.
fn factor_proof(e: &Eqn, p: &Term, source: &Source) -> Option<Proof> {
    let goal = canon(&term::binop(
        BinOp::Lte,
        term::int(&BigInt::from(0)),
        p.clone(),
    ));
    let mut atoms = Vec::new();
    let rows = vec![
        linear::as_nonneg(&goal, false, &mut atoms)?,
        linear::as_nonneg(&e.lhs, term::bool_value(&e.rhs)?, &mut atoms)?,
    ];
    let weights = linear::certificate(&rows)?;
    let (name, wrap) = match source {
        Source::Hyp(n) => (n.clone(), None),
        Source::Fact(proof) => (FACTOR.to_string(), Some(proof.as_ref().clone())),
    };
    let step = Proof::Linear {
        goal,
        value: true,
        hyps: vec![name],
        weights,
        cuts: Vec::new(),
    };
    Some(match wrap {
        None => step,
        Some(proof) => Proof::Have {
            name: FACTOR.to_string(),
            fact: e.lhs.clone(),
            proof: Box::new(proof),
            body: Box::new(step),
        },
    })
}

/// The products of pairs of `comparisons` (each `fact = value` with where
/// it comes from) that share one of the `wanted` monomials, each as
/// `0 <= p * q` with its proof. `atoms` is the list the rows are read
/// over.
pub(super) fn products(
    wanted: &[Vec<usize>],
    atoms: &[Term],
    comparisons: &[(Eqn, Source)],
) -> Vec<(Term, Proof)> {
    let factors: Vec<(Term, Proof)> = comparisons
        .iter()
        .filter_map(|(e, source)| {
            let p = nonneg_side(&e.lhs, term::bool_value(&e.rhs)?)?;
            // A literal factor only scales a row linear arithmetic has.
            if term::int_value(&p).is_some() {
                return None;
            }
            let proof = factor_proof(e, &p, source)?;
            Some((p, proof))
        })
        .take(MAX_FACTORS)
        .collect();
    let mut out: Vec<(Term, Proof)> = Vec::new();
    for i in 0..factors.len() {
        for j in i..factors.len() {
            let (p, pp) = &factors[i];
            let (q, pq) = &factors[j];
            let subst = vec![("a".to_string(), p.clone()), ("b".to_string(), q.clone())];
            let Some((_, concl)) = WallRule::MulNonneg.instantiate(&subst) else {
                continue;
            };
            let mut seen = atoms.to_vec();
            let Some(row) = linear::as_nonneg(&concl.lhs, true, &mut seen) else {
                continue;
            };
            if !row.keys().any(|m| wanted.contains(m)) || out.iter().any(|(f, _)| *f == concl.lhs) {
                continue;
            }
            out.push((
                concl.lhs,
                Proof::Rule {
                    rule: WallRule::MulNonneg,
                    subst,
                    premises: vec![pp.clone(), pq.clone()],
                },
            ));
        }
    }
    out
}

impl super::env::Env<'_> {
    /// What the products for `goal = value` are read from: the opposite of
    /// the goal and the comparisons `known`, as equations.
    pub(super) fn product_key(
        &self,
        goal: &Term,
        value: bool,
        known: &[(String, Eqn)],
    ) -> Vec<Eqn> {
        let mut key = vec![Eqn::new(canon(goal), term::boolean(!value))];
        key.extend(known.iter().map(|(_, e)| e.clone()));
        key
    }
}
