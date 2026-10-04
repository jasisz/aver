//! Polynomial normal form of Int terms, for the `ring` step: two Int terms
//! are equal when, read as polynomials over their atoms, they have the same
//! monomials with the same coefficients.
//!
//! `+`, `-`, `*`, negation and integer literals are the ring operations;
//! every other subterm (a variable, a call, a field, Euclidean division) is
//! an atom, compared by its exact term. The same reading is in the kernel
//! written in Aver (`tools/proof-kernel/kernel/ring.av`).

use std::collections::BTreeMap;

use num_bigint::BigInt;

use crate::ast::BinOp;
use crate::ir::hir::ResolvedExpr;

use super::term::{self, Term, canon};

/// Monomials, each a sorted list of atom indices, to their coefficients.
pub type Poly = BTreeMap<Vec<usize>, BigInt>;

fn add(mut a: Poly, b: Poly) -> Poly {
    for (m, c) in b {
        *a.entry(m).or_default() += c;
    }
    a.retain(|_, c| *c != BigInt::from(0));
    a
}

fn scale(a: Poly, k: &BigInt) -> Poly {
    a.into_iter()
        .map(|(m, c)| (m, c * k))
        .filter(|(_, c)| *c != BigInt::from(0))
        .collect()
}

fn mul(a: &Poly, b: &Poly) -> Poly {
    let mut out = Poly::new();
    for (ma, ca) in a {
        for (mb, cb) in b {
            let mut m = ma.clone();
            m.extend(mb.iter().copied());
            m.sort_unstable();
            *out.entry(m).or_default() += ca * cb;
        }
    }
    out.retain(|_, c| *c != BigInt::from(0));
    out
}

/// `t` as a polynomial over `atoms`, adding the atoms it meets.
pub fn poly(t: &Term, atoms: &mut Vec<Term>) -> Poly {
    if let Some(v) = term::int_value(t) {
        let mut p = Poly::new();
        if v != BigInt::from(0) {
            p.insert(Vec::new(), v);
        }
        return p;
    }
    // Joining texts and Float arithmetic are not ring operations.
    let int_op = !matches!(
        t.ty(),
        Some(crate::ast::Type::Str | crate::ast::Type::Float)
    );
    match &t.node {
        ResolvedExpr::BinOp(BinOp::Add, a, b) if int_op => add(poly(a, atoms), poly(b, atoms)),
        ResolvedExpr::BinOp(BinOp::Sub, a, b) if int_op => {
            let pa = poly(a, atoms);
            let pb = poly(b, atoms);
            add(pa, scale(pb, &BigInt::from(-1)))
        }
        ResolvedExpr::BinOp(BinOp::Mul, a, b) if int_op => {
            let pa = poly(a, atoms);
            let pb = poly(b, atoms);
            mul(&pa, &pb)
        }
        ResolvedExpr::Neg(a) if int_op => scale(poly(a, atoms), &BigInt::from(-1)),
        _ => {
            let c = canon(t);
            let i = match atoms.iter().position(|x| *x == c) {
                Some(i) => i,
                None => {
                    atoms.push(c);
                    atoms.len() - 1
                }
            };
            let mut p = Poly::new();
            p.insert(vec![i], BigInt::from(1));
            p
        }
    }
}

/// Whether `a` and `b` are the same polynomial.
pub fn same_polynomial(a: &Term, b: &Term) -> bool {
    let mut atoms = Vec::new();
    let pa = poly(a, &mut atoms);
    let pb = poly(b, &mut atoms);
    pa == pb
}
