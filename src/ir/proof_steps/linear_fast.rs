//! The Fourier–Motzkin search of [`super::linear`] over a flat
//! representation: every monomial the comparisons name gets its rank in
//! the order the polynomial maps keep them in, a row is the list of its
//! (rank, coefficient) pairs in that order, and a coefficient is a machine
//! integer until it outgrows one. The search takes the same steps in the
//! same order as over the maps, so it finds the same weights.

use std::collections::BTreeMap;

use num_bigint::BigInt;
use num_traits::{Signed, Zero};

use super::ring::Poly;

/// An integer, a machine word while it fits one.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum Z {
    S(i64),
    B(BigInt),
}

impl Z {
    fn norm(b: BigInt) -> Z {
        match i64::try_from(&b) {
            Ok(v) => Z::S(v),
            Err(_) => Z::B(b),
        }
    }
    pub(super) fn from_big(b: &BigInt) -> Z {
        match i64::try_from(b) {
            Ok(v) => Z::S(v),
            Err(_) => Z::B(b.clone()),
        }
    }
    pub(super) fn to_big(&self) -> BigInt {
        match self {
            Z::S(v) => BigInt::from(*v),
            Z::B(b) => b.clone(),
        }
    }
    fn zero() -> Z {
        Z::S(0)
    }
    fn one() -> Z {
        Z::S(1)
    }
    fn is_zero(&self) -> bool {
        matches!(self, Z::S(0))
    }
    fn is_positive(&self) -> bool {
        match self {
            Z::S(v) => *v > 0,
            Z::B(b) => b.is_positive(),
        }
    }
    fn is_negative(&self) -> bool {
        match self {
            Z::S(v) => *v < 0,
            Z::B(b) => b.is_negative(),
        }
    }
    fn at_most_one(&self) -> bool {
        match self {
            Z::S(v) => *v <= 1,
            Z::B(b) => *b <= BigInt::from(1),
        }
    }
    fn add(&self, o: &Z) -> Z {
        if let (Z::S(a), Z::S(b)) = (self, o)
            && let Some(c) = a.checked_add(*b)
        {
            return Z::S(c);
        }
        Z::norm(self.to_big() + o.to_big())
    }
    fn mul(&self, o: &Z) -> Z {
        if let (Z::S(a), Z::S(b)) = (self, o)
            && let Some(c) = a.checked_mul(*b)
        {
            return Z::S(c);
        }
        Z::norm(self.to_big() * o.to_big())
    }
    fn neg(&self) -> Z {
        if let Z::S(a) = self
            && let Some(c) = a.checked_neg()
        {
            return Z::S(c);
        }
        Z::norm(-self.to_big())
    }
    fn abs(&self) -> Z {
        if self.is_negative() { self.neg() } else { self.clone() }
    }
    /// Remainder with the sign of the dividend, as `BigInt`'s `%`.
    fn rem(&self, o: &Z) -> Z {
        if let (Z::S(a), Z::S(b)) = (self, o)
            && let Some(c) = a.checked_rem(*b)
        {
            return Z::S(c);
        }
        Z::norm(self.to_big() % o.to_big())
    }
    /// Quotient rounded toward zero, as `BigInt`'s `/`.
    fn div(&self, o: &Z) -> Z {
        if let (Z::S(a), Z::S(b)) = (self, o)
            && let Some(c) = a.checked_div(*b)
        {
            return Z::S(c);
        }
        Z::norm(self.to_big() / o.to_big())
    }
    fn div_floor(&self, o: &Z) -> Z {
        let q = self.div(o);
        if self.rem(o).is_negative() {
            q.add(&Z::S(-1))
        } else {
            q
        }
    }
    fn gcd(&self, o: &Z) -> Z {
        let (mut a, mut b) = (self.abs(), o.abs());
        while !b.is_zero() {
            let r = a.rem(&b);
            a = b;
            b = r;
        }
        a
    }
}

type P = Vec<(u32, Z)>;
type Row = (P, Vec<Z>);

/// The monomials of the comparisons, ranked in the order of the maps.
pub(super) struct Table {
    monos: Vec<Vec<usize>>,
}

impl Table {
    pub(super) fn new(ps: &[Poly]) -> Table {
        let mut all: std::collections::BTreeSet<&Vec<usize>> = Default::default();
        for p in ps {
            all.extend(p.keys());
        }
        Table {
            monos: all.into_iter().cloned().collect(),
        }
    }
    fn is_const(&self, m: u32) -> bool {
        self.monos[m as usize].is_empty()
    }
    fn row(&self, p: &Poly) -> P {
        let index: BTreeMap<&Vec<usize>, u32> = self
            .monos
            .iter()
            .enumerate()
            .map(|(i, m)| (m, i as u32))
            .collect();
        p.iter()
            .map(|(m, c)| (index[m], Z::from_big(c)))
            .collect()
    }
    fn contradicts(&self, p: &P) -> bool {
        p.len() == 1 && self.is_const(p[0].0) && p[0].1.is_negative()
    }
    fn first_monomial(&self, p: &P) -> Option<u32> {
        p.iter().map(|(m, _)| *m).find(|m| !self.is_const(*m))
    }
}

fn get(p: &P, m: u32) -> Option<&Z> {
    p.binary_search_by_key(&m, |(k, _)| *k)
        .ok()
        .map(|i| &p[i].1)
}

/// `x * a + y * b`, zero coefficients dropped.
fn sum(x: &P, a: &Z, y: &P, b: &Z) -> P {
    let mut out = Vec::with_capacity(x.len() + y.len());
    let (mut i, mut j) = (0, 0);
    while i < x.len() || j < y.len() {
        let c = if j == y.len() || (i < x.len() && x[i].0 < y[j].0) {
            let c = (x[i].0, x[i].1.mul(a));
            i += 1;
            c
        } else if i == x.len() || y[j].0 < x[i].0 {
            let c = (y[j].0, y[j].1.mul(b));
            j += 1;
            c
        } else {
            let c = (x[i].0, x[i].1.mul(a).add(&y[j].1.mul(b)));
            i += 1;
            j += 1;
            c
        };
        if !c.1.is_zero() {
            out.push(c);
        }
    }
    out
}

fn weights(x: &[Z], a: &Z, y: &[Z], b: &Z) -> Vec<Z> {
    x.iter().zip(y).map(|(u, v)| u.mul(a).add(&v.mul(b))).collect()
}

fn substitute_equality(t: &Table, rows: &[Row]) -> Option<Vec<Row>> {
    let (i, j, m) = rows.iter().enumerate().find_map(|(i, (p, _))| {
        let m = t.first_monomial(p)?;
        let j = rows.iter().enumerate().position(|(j, (q, _))| {
            j > i
                && q.len() == p.len()
                && q.iter()
                    .zip(p)
                    .all(|((k1, c1), (k2, c2))| k1 == k2 && *c1 == c2.neg())
        })?;
        Some((i, j, m))
    })?;
    let (p, wp) = &rows[i];
    let (q, wq) = &rows[j];
    let c = get(p, m).expect("the monomial is in its row").clone();
    let mut out = Vec::new();
    for (k, (r, wr)) in rows.iter().enumerate() {
        if k == i || k == j {
            continue;
        }
        let Some(e) = get(r, m) else {
            out.push((r.clone(), wr.clone()));
            continue;
        };
        let (x, wx) = if e.is_positive() == c.is_positive() {
            (q, wq)
        } else {
            (p, wp)
        };
        let (scale, times) = (c.abs(), e.abs());
        out.push((sum(r, &scale, x, &times), weights(wr, &scale, wx, &times)));
    }
    Some(out)
}

/// One step of elimination: the rows without the first monomial any row
/// has, then each row with it positive against each with it negative.
/// `None` when no row has a monomial.
fn eliminate(t: &Table, rows: Vec<Row>, mut each: impl FnMut(Row) -> Row) -> Option<Vec<Row>> {
    let m = rows.iter().find_map(|(p, _)| t.first_monomial(p))?;
    let (with, without): (Vec<_>, Vec<_>) =
        rows.into_iter().partition(|(p, _)| get(p, m).is_some());
    let (pos, neg): (Vec<_>, Vec<_>) = with
        .into_iter()
        .partition(|(p, _)| get(p, m).is_some_and(Z::is_positive));
    let mut next = without;
    for (pp, wp) in &pos {
        for (pn, wn) in &neg {
            let a = get(pp, m).expect("in the row").clone();
            let b = get(pn, m).expect("in the row").neg();
            next.push(each((sum(pp, &b, pn, &a), weights(wp, &b, wn, &a))));
        }
    }
    Some(next)
}

fn unit(n: usize, i: usize) -> Vec<Z> {
    let mut w = vec![Z::zero(); n];
    w[i] = Z::one();
    w
}

fn big(w: &[Z]) -> Vec<BigInt> {
    w.iter().map(Z::to_big).collect()
}

/// [`super::linear::certificate`].
pub(super) fn certificate(ps: &[Poly]) -> Option<Vec<BigInt>> {
    let t = Table::new(ps);
    let n = ps.len();
    let mut rows: Vec<Row> = ps
        .iter()
        .enumerate()
        .map(|(i, p)| (t.row(p), unit(n, i)))
        .collect();
    for _ in 0..16 {
        while let Some(next) = substitute_equality(&t, &rows) {
            if let Some((_, w)) = rows.iter().find(|(p, _)| t.contradicts(p)) {
                return Some(big(w));
            }
            rows = next;
        }
        if let Some((_, w)) = rows.iter().find(|(p, _)| t.contradicts(p)) {
            return Some(big(w));
        }
        let next = eliminate(&t, rows, |r| r)?;
        if next.len() > 400 {
            return None;
        }
        rows = next;
    }
    None
}

/// The search of [`super::linear::integer_certificate`]: the weights it
/// finds and every cut, before the unused cuts are dropped.
pub(super) fn integer_search(ps: &[Poly]) -> Option<(Vec<BigInt>, Vec<super::linear::Cut>)> {
    let t = Table::new(ps);
    let n = ps.len();
    let mut cuts: Vec<(Z, Vec<Z>)> = Vec::new();
    fn round(t: &Table, row: Row, n: usize, cuts: &mut Vec<(Z, Vec<Z>)>) -> Row {
        let (p, mut w) = row;
        let g = p
            .iter()
            .filter(|(m, _)| !t.is_const(*m))
            .fold(Z::zero(), |g, (_, c)| g.gcd(c));
        let c = p
            .iter()
            .find(|(m, _)| t.is_const(*m))
            .map(|(_, c)| c.clone())
            .unwrap_or(Z::zero());
        if g.at_most_one() || c.rem(&g).is_zero() {
            return (p, w);
        }
        let r: P = p
            .iter()
            .map(|(m, c)| {
                (
                    *m,
                    if t.is_const(*m) {
                        c.div_floor(&g)
                    } else {
                        c.div(&g)
                    },
                )
            })
            .filter(|(_, c)| !c.is_zero())
            .collect();
        w.resize(n + cuts.len(), Z::zero());
        cuts.push((g, w));
        (r, unit(n + cuts.len(), n + cuts.len() - 1))
    }
    fn pad(rows: &mut [Row], len: usize) {
        for (_, w) in rows.iter_mut() {
            w.resize(len, Z::zero());
        }
    }
    let mut rows: Vec<Row> = Vec::new();
    for (i, p) in ps.iter().enumerate() {
        let row = round(&t, (t.row(p), unit(n, i)), n, &mut cuts);
        rows.push(row);
    }
    let found = 'search: {
        for _ in 0..16 {
            pad(&mut rows, n + cuts.len());
            while let Some(next) = substitute_equality(&t, &rows) {
                if let Some((_, w)) = rows.iter().find(|(p, _)| t.contradicts(p)) {
                    break 'search Some(w.clone());
                }
                rows = next.into_iter().map(|r| round(&t, r, n, &mut cuts)).collect();
                pad(&mut rows, n + cuts.len());
            }
            if let Some((_, w)) = rows.iter().find(|(p, _)| t.contradicts(p)) {
                break 'search Some(w.clone());
            }
            let next = eliminate(&t, rows, |r| round(&t, r, n, &mut cuts))?;
            if next.len() > 400 {
                return None;
            }
            rows = next;
        }
        None
    }?;
    let cuts = cuts
        .into_iter()
        .map(|(g, w)| super::linear::Cut {
            divisor: g.to_big(),
            weights: big(&w),
        })
        .collect();
    Some((big(&found), cuts))
}
