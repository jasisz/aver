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

/// One rounding step of a certificate over ℤ: the weighted sum `s >= 0` of
/// the comparisons and of the cuts before this one, whose every
/// coefficient but the constant `c` is a multiple of `divisor`, read as
/// `(s - c) / divisor + floor(c / divisor) >= 0`. That holds because the
/// atoms are integers: `(s - c) / divisor` is one, and it is at least
/// `-c / divisor`. `weights` index the comparisons (the opposite of the
/// goal first) and then the cuts before this one.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cut {
    pub divisor: BigInt,
    pub weights: Vec<BigInt>,
}

/// Weights that add up to a contradiction over ℤ: `weights` index the
/// comparisons and then every cut.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Certificate {
    pub weights: Vec<BigInt>,
    pub cuts: Vec<Cut>,
}

fn gcd(a: &BigInt, b: &BigInt) -> BigInt {
    let (mut a, mut b) = (a.abs(), b.abs());
    while !b.is_zero() {
        let r = &a % &b;
        a = b;
        b = r;
    }
    a
}

/// `a / b` rounded down, for `b > 0`.
fn div_floor(a: &BigInt, b: &BigInt) -> BigInt {
    let q = a / b;
    if (a % b).is_negative() { q - 1 } else { q }
}

/// The greatest common divisor of the coefficients of `p` other than its
/// constant; zero when `p` is a constant.
fn content(p: &Poly) -> BigInt {
    p.iter()
        .filter(|(m, _)| !m.is_empty())
        .fold(BigInt::zero(), |g, (_, c)| gcd(&g, c))
}

/// `p >= 0` divided by `divisor`, its constant rounded down: the row a
/// [`Cut`] adds. `None` when a coefficient other than the constant is not
/// a multiple of `divisor` or `divisor` is not positive.
pub fn rounded(p: &Poly, divisor: &BigInt) -> Option<Poly> {
    if !divisor.is_positive() {
        return None;
    }
    let mut out = Poly::new();
    for (m, c) in p {
        if m.is_empty() {
            out.insert(m.clone(), div_floor(c, divisor));
        } else {
            if !(c % divisor).is_zero() {
                return None;
            }
            out.insert(m.clone(), c / divisor);
        }
    }
    out.retain(|_, c| !c.is_zero());
    Some(out)
}

/// The weighted sum of `rows`, every weight nonnegative and one per row.
fn weighted(rows: &[Poly], weights: &[BigInt]) -> Option<Poly> {
    if rows.len() != weights.len() {
        return None;
    }
    combine(rows, weights)
}

/// Whether `cert` shows the comparisons `ps` contradict each other over ℤ:
/// each cut rounds a nonnegative sum of the comparisons and the cuts
/// before it, and the weights add the comparisons and the cuts up to a
/// negative constant. The same check is in the kernel written in Aver.
pub fn checks(ps: &[Poly], cert: &Certificate) -> bool {
    let mut rows = ps.to_vec();
    for cut in &cert.cuts {
        let Some(sum) = weighted(&rows, &cut.weights) else {
            return false;
        };
        let Some(row) = rounded(&sum, &cut.divisor) else {
            return false;
        };
        rows.push(row);
    }
    weighted(&rows, &cert.weights).is_some_and(|p| contradicts(&p))
}

/// Weights and cuts that add up to a contradiction over ℤ, if
/// Fourier–Motzkin elimination finds them when every row whose
/// coefficients share a divisor its constant does not is rounded (a
/// [`Cut`]), as `omega` normalises its constraints. Only the comparisons
/// and cuts the contradiction uses keep a weight.
pub fn integer_certificate(ps: &[Poly]) -> Option<Certificate> {
    let n = ps.len();
    let mut cuts: Vec<Cut> = Vec::new();
    // A row rounded where that makes it stronger, as a new cut.
    fn round(row: (Poly, Vec<BigInt>), n: usize, cuts: &mut Vec<Cut>) -> (Poly, Vec<BigInt>) {
        let (p, mut w) = row;
        let g = content(&p);
        let c = p.get(&Vec::new()).cloned().unwrap_or_default();
        if g <= BigInt::from(1) || (&c % &g).is_zero() {
            return (p, w);
        }
        let Some(r) = rounded(&p, &g) else {
            return (p, w);
        };
        w.resize(n + cuts.len(), BigInt::zero());
        cuts.push(Cut {
            divisor: g,
            weights: w,
        });
        let mut unit = vec![BigInt::zero(); n + cuts.len()];
        unit[n + cuts.len() - 1] = BigInt::from(1);
        (r, unit)
    }
    fn pad(rows: &mut [(Poly, Vec<BigInt>)], len: usize) {
        for (_, w) in rows.iter_mut() {
            w.resize(len, BigInt::zero());
        }
    }
    let mut rows: Vec<(Poly, Vec<BigInt>)> = Vec::new();
    for (i, p) in ps.iter().enumerate() {
        let mut w = vec![BigInt::zero(); n];
        w[i] = BigInt::from(1);
        let row = round((p.clone(), w), n, &mut cuts);
        rows.push(row);
    }
    let found = 'search: {
        for _ in 0..16 {
            pad(&mut rows, n + cuts.len());
            while let Some(next) = substitute_equality(&rows) {
                if let Some((_, w)) = rows.iter().find(|(p, _)| contradicts(p)) {
                    break 'search Some(w.clone());
                }
                rows = next.into_iter().map(|r| round(r, n, &mut cuts)).collect();
                pad(&mut rows, n + cuts.len());
            }
            if let Some((_, w)) = rows.iter().find(|(p, _)| contradicts(p)) {
                break 'search Some(w.clone());
            }
            let m = rows
                .iter()
                .flat_map(|(p, _)| p.keys())
                .find(|m| !m.is_empty())
                .cloned()?;
            let (with, without): (Vec<_>, Vec<_>) =
                rows.into_iter().partition(|(p, _)| p.contains_key(&m));
            let (pos, neg): (Vec<_>, Vec<_>) =
                with.into_iter().partition(|(p, _)| p[&m].is_positive());
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
                    next.push(round((p, w), n, &mut cuts));
                }
            }
            if next.len() > 400 {
                return None;
            }
            rows = next;
        }
        None
    }?;
    let mut weights = found;
    weights.resize(n + cuts.len(), BigInt::zero());
    // Only the cuts the contradiction reaches, the later ones first.
    let mut used = vec![false; cuts.len()];
    for j in (0..cuts.len()).rev() {
        let by_later = (j + 1..cuts.len())
            .any(|k| used[k] && cuts[k].weights.get(n + j).is_some_and(|w| !w.is_zero()));
        used[j] = !weights[n + j].is_zero() || by_later;
    }
    let mut index = Vec::new();
    let mut kept: Vec<Cut> = Vec::new();
    for (j, cut) in cuts.iter().enumerate() {
        if !used[j] {
            index.push(None);
            continue;
        }
        let mut w: Vec<BigInt> = cut.weights[..n].to_vec();
        for (k, slot) in index.iter().enumerate() {
            if slot.is_some() {
                w.push(cut.weights.get(n + k).cloned().unwrap_or_default());
            }
        }
        index.push(Some(kept.len()));
        kept.push(Cut {
            divisor: cut.divisor.clone(),
            weights: w,
        });
    }
    let mut final_weights: Vec<BigInt> = weights[..n].to_vec();
    for j in 0..cuts.len() {
        if index[j].is_some() {
            final_weights.push(weights[n + j].clone());
        }
    }
    let cert = Certificate {
        weights: final_weights,
        cuts: kept,
    };
    checks(ps, &cert).then_some(cert)
}

/// `cert` over only the comparisons `keep` names (in order), every other
/// one unused; `None` when one that is dropped has a weight somewhere.
pub fn restricted(cert: &Certificate, n: usize, keep: &[usize]) -> Option<Certificate> {
    let pick = |w: &[BigInt]| -> Option<Vec<BigInt>> {
        for (i, x) in w.iter().enumerate().take(n) {
            if !x.is_zero() && !keep.contains(&i) {
                return None;
            }
        }
        let mut out: Vec<BigInt> = keep.iter().map(|&i| w[i].clone()).collect();
        out.extend(w[n..].iter().cloned());
        Some(out)
    };
    Some(Certificate {
        weights: pick(&cert.weights)?,
        cuts: cert
            .cuts
            .iter()
            .map(|c| {
                Some(Cut {
                    divisor: c.divisor.clone(),
                    weights: pick(&c.weights)?,
                })
            })
            .collect::<Option<Vec<_>>>()?,
    })
}

/// Whether comparison `i` of `n` has a weight anywhere in `cert`.
pub fn uses(cert: &Certificate, i: usize) -> bool {
    !cert.weights[i].is_zero() || cert.cuts.iter().any(|c| !c.weights[i].is_zero())
}
