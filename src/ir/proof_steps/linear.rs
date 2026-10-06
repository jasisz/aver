//! Linear arithmetic over ℤ with a certificate: a comparison holds because
//! its negation, together with comparisons the hypotheses decide, adds up
//! with nonnegative weights to `c >= 0` for a negative constant `c`.
//!
//! Every comparison is read as `p >= 0` for a polynomial `p` over the
//! atoms of [`super::ring`]: `a < b` as `b - a - 1 >= 0` (Int), `a <= b` as
//! `b - a >= 0`, a comparison that is `false` as its complement. The
//! producer finds the weights by Fourier–Motzkin elimination; the kernels
//! only check them.

use num_bigint::BigInt;
use num_traits::{Signed, Zero};

use crate::ast::BinOp;
use crate::ir::hir::ResolvedExpr;

use super::ring::{Poly, poly as poly_over};
use super::term::{self, Term};

/// `t = value` for an Int comparison `t`, as `p >= 0`; `None` when `t` is
/// not one.
pub fn as_nonneg(t: &Term, value: bool, atoms: &mut Vec<Term>) -> Option<Poly> {
    let ResolvedExpr::BinOp(op, a, b) = &t.node else {
        return None;
    };
    let int =
        |x: &Term| matches!(x.ty(), Some(crate::ast::Type::Int)) || term::int_value(x).is_some();
    if !int(a) && !int(b) {
        return None;
    }
    // (lower, upper, strict): `lower < upper` or `lower <= upper`.
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
    let mut p = poly_over(upper, atoms);
    for (m, c) in poly_over(lower, atoms) {
        *p.entry(m).or_default() -= c;
    }
    if strict {
        *p.entry(Vec::new()).or_default() -= 1;
    }
    p.retain(|_, c| !c.is_zero());
    Some(p)
}

/// The weighted sum, when every weight is nonnegative.
pub fn combine(ps: &[Poly], weights: &[BigInt]) -> Option<Poly> {
    if ps.len() != weights.len() || weights.iter().any(|w| w.is_negative()) {
        return None;
    }
    let mut out = Poly::new();
    for (p, w) in ps.iter().zip(weights) {
        for (m, c) in p {
            *out.entry(m.clone()).or_default() += c * w;
        }
    }
    out.retain(|_, c| !c.is_zero());
    Some(out)
}

/// Whether `p >= 0` is false: `p` is a negative constant.
pub fn contradicts(p: &Poly) -> bool {
    match p.len() {
        0 => false,
        1 => p.get(&Vec::new()).is_some_and(|c| c.is_negative()),
        _ => false,
    }
}

/// The rows with one equality used up: for the first pair of rows `p >= 0`
/// and `-p >= 0` with a monomial `m`, every other row that has `m` gets
/// `|e|` times the one of the two whose `m` cancels its own (`e` its
/// coefficient), after being scaled by `|c|` (`c` the coefficient of `m`
/// in `p`). The weights stay nonnegative, so the result is still a sum the
/// kernels check. `None` when no row has its opposite.
fn substitute_equality(rows: &[(Poly, Vec<BigInt>)]) -> Option<Vec<(Poly, Vec<BigInt>)>> {
    let negated = |p: &Poly| -> Poly { p.iter().map(|(k, c)| (k.clone(), -c)).collect() };
    let (i, j, m) = rows.iter().enumerate().find_map(|(i, (p, _))| {
        let m = p.keys().find(|m| !m.is_empty())?.clone();
        let opposite = negated(p);
        let j = rows
            .iter()
            .enumerate()
            .position(|(j, (q, _))| j > i && *q == opposite)?;
        Some((i, j, m))
    })?;
    let (p, wp) = &rows[i];
    let (q, wq) = &rows[j];
    let c = p[&m].clone();
    let mut out = Vec::new();
    for (k, (r, wr)) in rows.iter().enumerate() {
        if k == i || k == j {
            continue;
        }
        let Some(e) = r.get(&m) else {
            out.push((r.clone(), wr.clone()));
            continue;
        };
        // The row whose `m` has the opposite sign to `e`.
        let (x, wx) = if e.is_positive() == c.is_positive() {
            (q, wq)
        } else {
            (p, wp)
        };
        let (scale, times) = (c.abs(), e.abs());
        let mut sum = Poly::new();
        for (key, v) in r {
            *sum.entry(key.clone()).or_default() += v * &scale;
        }
        for (key, v) in x {
            *sum.entry(key.clone()).or_default() += v * &times;
        }
        sum.retain(|_, v| !v.is_zero());
        let w = wr
            .iter()
            .zip(wx)
            .map(|(a, b)| a * &scale + b * &times)
            .collect();
        out.push((sum, w));
    }
    Some(out)
}

/// Weights, one per inequality, that add up to a contradiction, if
/// Fourier–Motzkin elimination over the monomials finds them.
pub fn certificate(ps: &[Poly]) -> Option<Vec<BigInt>> {
    // Each row: the inequality and the weights that build it.
    let n = ps.len();
    let mut rows: Vec<(Poly, Vec<BigInt>)> = ps
        .iter()
        .enumerate()
        .map(|(i, p)| {
            let mut w = vec![BigInt::zero(); n];
            w[i] = BigInt::from(1);
            (p.clone(), w)
        })
        .collect();
    for _ in 0..16 {
        // An equality, two rows `p >= 0` and `-p >= 0`, removes a monomial
        // of `p` from every other row by substitution, which keeps the
        // number of rows down where pairing every positive with every
        // negative row would multiply it. Each one takes two rows away.
        while let Some(next) = substitute_equality(&rows) {
            if let Some((_, w)) = rows.iter().find(|(p, _)| contradicts(p)) {
                return Some(w.clone());
            }
            rows = next;
        }
        if let Some((_, w)) = rows.iter().find(|(p, _)| contradicts(p)) {
            return Some(w.clone());
        }
        // A monomial to eliminate: one that some row has.
        let m = rows
            .iter()
            .flat_map(|(p, _)| p.keys())
            .find(|m| !m.is_empty())
            .cloned()?;
        let (with, without): (Vec<_>, Vec<_>) =
            rows.into_iter().partition(|(p, _)| p.contains_key(&m));
        let (pos, neg): (Vec<_>, Vec<_>) = with.into_iter().partition(|(p, _)| p[&m].is_positive());
        let mut next = without;
        for (pp, wp) in &pos {
            for (pn, wn) in &neg {
                let a = pp[&m].clone();
                let b = -pn[&m].clone();
                let mut p = Poly::new();
                for (k, c) in pp {
                    *p.entry(k.clone()).or_default() += c * &b;
                }
                for (k, c) in pn {
                    *p.entry(k.clone()).or_default() += c * &a;
                }
                p.retain(|_, c| !c.is_zero());
                let w: Vec<BigInt> = wp.iter().zip(wn).map(|(x, y)| x * &b + y * &a).collect();
                next.push((p, w));
            }
        }
        if next.len() > 400 {
            return None;
        }
        rows = next;
    }
    None
}
