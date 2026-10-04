#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Mono {
    pub atoms: aver_rt::AverIntList,
    pub coef: aver_rt::AverInt,
}

impl PartialOrd for Mono {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Mono {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.atoms.cmp(&other.atoms))
            .then_with(|| self.coef.cmp(&other.coef))
    }
}

impl aver_rt::AverDisplay for Mono {
    fn aver_display(&self) -> String {
        format!(
            "Mono({})",
            vec![
                format!("atoms: {}", self.atoms.aver_display_inner()),
                format!("coef: {}", self.coef.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Read {
    pub poly: aver_rt::AverList<Mono>,
    pub atoms: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
}

impl PartialOrd for Read {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Read {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.atoms.cmp(&other.atoms))
            .then_with(|| self.poly.cmp(&other.poly))
    }
}

impl aver_rt::AverDisplay for Read {
    fn aver_display(&self) -> String {
        format!(
            "Read({})",
            vec![
                format!("poly: {}", self.poly.aver_display_inner()),
                format!("atoms: {}", self.atoms.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    WeightedSum(
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
        aver_rt::AverIntList,
        aver_rt::AverList<Mono>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    ),
    WeightedNext(
        Option<Read>,
        aver_rt::AverInt,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
        aver_rt::AverIntList,
        aver_rt::AverList<Mono>,
    ),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> Option<aver_rt::AverList<Mono>> {
    loop {
        __state = match __state {
            __MutualTco1::WeightedSum(
                mut facts @ _,
                mut weights @ _,
                mut acc @ _,
                mut atoms @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                {
                    let __int_match_subject = (facts, weights);
                    let (__lit0, __lit1) = &__int_match_subject;
                    if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
                        let Some((f, fs)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        let Some((w, ws)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        if ((w >= aver_rt::AverInt::from_i64(0))
                            && crate::proof_kernel::aver_generated::kernel::ring::isTruth(&f.rhs))
                        {
                            __MutualTco1::WeightedNext(crate::proof_kernel::aver_generated::kernel::ring::asNonneg(&f.lhs, (f.rhs == crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true)), &atoms), w, fs, ws, acc)
                        } else {
                            return None;
                        }
                    } else {
                        return Some(acc);
                    }
                }
            }
            __MutualTco1::WeightedNext(
                mut r @ _,
                mut w @ _,
                mut fs @ _,
                mut ws @ _,
                mut acc @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                match r {
                    None => return None,
                    Some(read @ _) => __MutualTco1::WeightedSum(
                        fs,
                        ws,
                        crate::proof_kernel::aver_generated::kernel::ring::addPoly(
                            acc,
                            crate::proof_kernel::aver_generated::kernel::ring::scalePoly(
                                &read.poly, w,
                            ),
                        ),
                        read.atoms,
                    ),
                }
            }
        };
    }
}

/// The weighted sum of the facts read as p >= 0; None when a fact is not an Int comparison or a weight is negative.
pub fn weightedSum(
    facts @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    weights @ _: aver_rt::AverIntList,
    acc @ _: aver_rt::AverList<Mono>,
    atoms @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Option<aver_rt::AverList<Mono>> {
    __mutual_tco_trampoline_1(__MutualTco1::WeightedSum(facts, weights, acc, atoms))
}

/// Add one weighted fact, then the rest.
pub fn weightedNext(
    r @ _: Option<Read>,
    w @ _: aver_rt::AverInt,
    fs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    ws @ _: aver_rt::AverIntList,
    acc @ _: aver_rt::AverList<Mono>,
) -> Option<aver_rt::AverList<Mono>> {
    __mutual_tco_trampoline_1(__MutualTco1::WeightedNext(r, w, fs, ws, acc))
}

/// Whether two Int terms are the same polynomial.
pub fn samePolynomial(
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    b @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    let left @ _ =
        crate::proof_kernel::aver_generated::kernel::ring::readPoly(a, &aver_rt::AverList::empty());
    let right @ _ = crate::proof_kernel::aver_generated::kernel::ring::readPoly(b, &left.atoms);
    (left.poly == right.poly)
}

/// t as a polynomial over atoms, with the atoms it adds.
pub fn readPoly(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TInt(n) => {
            crate::proof_kernel::aver_generated::kernel::ring::Read {
                poly: crate::proof_kernel::aver_generated::kernel::ring::constant(n),
                atoms: atoms.clone(),
            }
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(__pat0, a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            {
                let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&__pat0));
                if __dispatch_subject == aver_rt::AverInt::from_i64(43) {
                    crate::proof_kernel::aver_generated::kernel::ring::combine(
                        crate::proof_kernel::aver_generated::kernel::ring::readPoly(&a, atoms),
                        &b,
                        aver_rt::AverInt::from_i64(0),
                    )
                } else {
                    if __dispatch_subject == aver_rt::AverInt::from_i64(45) {
                        crate::proof_kernel::aver_generated::kernel::ring::combine(
                            crate::proof_kernel::aver_generated::kernel::ring::readPoly(&a, atoms),
                            &b,
                            aver_rt::AverInt::from_i64(1),
                        )
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(42) {
                            crate::proof_kernel::aver_generated::kernel::ring::combine(
                                crate::proof_kernel::aver_generated::kernel::ring::readPoly(
                                    &a, atoms,
                                ),
                                &b,
                                aver_rt::AverInt::from_i64(2),
                            )
                        } else {
                            crate::proof_kernel::aver_generated::kernel::ring::atom(t, atoms)
                        }
                    }
                }
            }
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TNeg(a) => {
            let a = (*a).clone();
            crate::proof_kernel::aver_generated::kernel::ring::negated(
                &crate::proof_kernel::aver_generated::kernel::ring::readPoly(&a, atoms),
            )
        }
        _ => crate::proof_kernel::aver_generated::kernel::ring::atom(t, atoms),
    }
}

/// The left polynomial joined with b's: added (0), subtracted (1) or multiplied (2).
#[inline(always)]
pub fn combine(
    mut left @ _: Read,
    b @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    how @ _: aver_rt::AverInt,
) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    let right @ _ = crate::proof_kernel::aver_generated::kernel::ring::readPoly(b, &left.atoms);
    {
        let __dispatch_subject = how;
        if __dispatch_subject == aver_rt::AverInt::from_i64(0) {
            crate::proof_kernel::aver_generated::kernel::ring::Read {
                poly: crate::proof_kernel::aver_generated::kernel::ring::addPoly(
                    left.poly, right.poly,
                ),
                atoms: right.atoms,
            }
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(1) {
                crate::proof_kernel::aver_generated::kernel::ring::Read {
                    poly: crate::proof_kernel::aver_generated::kernel::ring::addPoly(
                        left.poly,
                        crate::proof_kernel::aver_generated::kernel::ring::scalePoly(
                            &right.poly,
                            aver_rt::AverInt::from_i64(-1),
                        ),
                    ),
                    atoms: right.atoms,
                }
            } else {
                crate::proof_kernel::aver_generated::kernel::ring::Read {
                    poly: crate::proof_kernel::aver_generated::kernel::ring::mulPoly(
                        &left.poly,
                        &right.poly,
                    ),
                    atoms: right.atoms,
                }
            }
        }
    }
}

/// The negation of a polynomial.
pub fn negated(r @ _: &Read) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::ring::Read {
        poly: crate::proof_kernel::aver_generated::kernel::ring::scalePoly(
            &r.poly,
            aver_rt::AverInt::from_i64(-1),
        ),
        atoms: r.atoms.clone(),
    }
}

/// A constant polynomial; zero has no monomials.
#[inline(always)]
pub fn constant(n @ _: aver_rt::AverInt) -> aver_rt::AverList<Mono> {
    crate::proof_kernel::cancel_checkpoint();
    if (n == aver_rt::AverInt::from_i64(0)) {
        aver_rt::AverList::empty()
    } else {
        aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::ring::Mono {
                atoms: aver_rt::AverIntList::empty(),
                coef: n,
            },
        ])
    }
}

/// An atom: its index in the list, adding it when new.
#[inline(always)]
pub fn atom(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::ring::indexOf(
        t.clone(),
        atoms.clone(),
        aver_rt::AverInt::from_i64(0),
    ) {
        Some(i @ _) => crate::proof_kernel::aver_generated::kernel::ring::Read {
            poly: aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::ring::Mono {
                    atoms: aver_rt::AverIntList::from_vec(vec![i]),
                    coef: aver_rt::AverInt::from_i64(1),
                },
            ]),
            atoms: atoms.clone(),
        },
        None => crate::proof_kernel::aver_generated::kernel::ring::Read {
            poly: aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::ring::Mono {
                    atoms: aver_rt::AverIntList::from_vec(vec![aver_rt::AverInt::from_i64(
                        atoms.len() as i64,
                    )]),
                    coef: aver_rt::AverInt::from_i64(1),
                },
            ]),
            atoms: aver_rt::AverList::concat(
                &atoms.clone(),
                &aver_rt::AverList::from_vec(vec![t.clone()]),
            ),
        },
    }
}

/// Where an atom is, if it is there.
#[inline(always)]
pub fn indexOf(
    t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut atoms @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut i @ _: aver_rt::AverInt,
) -> Option<aver_rt::AverInt> {
    let t @ _ = std::sync::Arc::new(t);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(atoms, [] => { return None; }, [a, rest] => { if (&(a) == &*t) { return Some(i); } else { {
            let __tco1 = rest;
            let __tco2 = i.add(&aver_rt::AverInt::from_i64(1));
            atoms = __tco1;
            i = __tco2;
            continue;
        } } })
    }
}

/// The sum of two polynomials, monomials kept in order and merged.
#[inline(always)]
pub fn addPoly(
    mut a @ _: aver_rt::AverList<Mono>,
    mut b @ _: aver_rt::AverList<Mono>,
) -> aver_rt::AverList<Mono> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(b, [] => { return a; }, [m, rest] => { {
            let __tco0 = crate::proof_kernel::aver_generated::kernel::ring::insertMono(&m, &a);
            let __tco1 = rest;
            a = __tco0;
            b = __tco1;
            continue;
        } })
    }
}

/// Add one monomial to a polynomial in order; a coefficient that sums to zero drops it.
#[inline(always)]
pub fn insertMono(m @ _: &Mono, p @ _: &aver_rt::AverList<Mono>) -> aver_rt::AverList<Mono> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(p.clone(), [] => crate::proof_kernel::aver_generated::kernel::ring::nonzero(m), [x, rest] => { { let __dispatch_subject = aver_rt::AverInt::from_i64(crate::proof_kernel::aver_generated::kernel::ring::compareAtoms(m.atoms.clone(), x.atoms.clone())); if __dispatch_subject == aver_rt::AverInt::from_i64(0) { aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::ring::nonzero(&crate::proof_kernel::aver_generated::kernel::ring::Mono { atoms: x.atoms, coef: x.coef.add(&m.coef) }), &rest) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(1) { aver_rt::AverList::prepend(x, &crate::proof_kernel::aver_generated::kernel::ring::insertMono(m, &rest)) } else { aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::ring::nonzero(m), &p.clone()) } } } })
}

/// A monomial, unless its coefficient is zero.
#[inline(always)]
pub fn nonzero(m @ _: &Mono) -> aver_rt::AverList<Mono> {
    crate::proof_kernel::cancel_checkpoint();
    if (m.coef == aver_rt::AverInt::from_i64(0)) {
        aver_rt::AverList::empty()
    } else {
        aver_rt::AverList::from_vec(vec![m.clone()])
    }
}

/// Order of two sorted atom lists: 0 equal, 1 when a comes after b, 2 when before.
pub fn compareAtoms(mut a @ _: aver_rt::AverIntList, mut b @ _: aver_rt::AverIntList) -> i64 {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __int_match_subject = (a, b);
            let (__lit0, __lit1) = &__int_match_subject;
            if (*__lit0).is_empty() && (*__lit1).is_empty() {
                return 0i64;
            } else if (*__lit0).is_empty() {
                return 2i64;
            } else if (*__lit1).is_empty() {
                return 1i64;
            } else if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
                let Some((x, xs)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                let Some((y, ys)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                if (x == y) {
                    {
                        let __tco0 = xs;
                        let __tco1 = ys;
                        a = __tco0;
                        b = __tco1;
                        continue;
                    }
                } else {
                    if (x > y) {
                        return 1i64;
                    } else {
                        return 2i64;
                    }
                }
            } else {
                unreachable!("Aver Rust codegen: non-exhaustive guard-chain match")
            }
        }
    }
}

/// Every coefficient times k.
#[inline(always)]
pub fn scalePoly(
    p @ _: &aver_rt::AverList<Mono>,
    k @ _: aver_rt::AverInt,
) -> aver_rt::AverList<Mono> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(p.clone(), [] => aver_rt::AverList::empty(), [m, rest] => aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::ring::nonzero(&crate::proof_kernel::aver_generated::kernel::ring::Mono { atoms: m.atoms, coef: m.coef.mul(&k) }), &crate::proof_kernel::aver_generated::kernel::ring::scalePoly(&rest, k)))
}

/// The product of two polynomials.
#[inline(always)]
pub fn mulPoly(
    a @ _: &aver_rt::AverList<Mono>,
    b @ _: &aver_rt::AverList<Mono>,
) -> aver_rt::AverList<Mono> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(a.clone(), [] => aver_rt::AverList::empty(), [m, rest] => crate::proof_kernel::aver_generated::kernel::ring::addPoly(crate::proof_kernel::aver_generated::kernel::ring::mulMono(&m, b), crate::proof_kernel::aver_generated::kernel::ring::mulPoly(&rest, b)))
}

/// One monomial times a polynomial.
#[inline(always)]
pub fn mulMono(m @ _: &Mono, p @ _: &aver_rt::AverList<Mono>) -> aver_rt::AverList<Mono> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(p.clone(), [] => aver_rt::AverList::empty(), [x, rest] => crate::proof_kernel::aver_generated::kernel::ring::insertMono(&crate::proof_kernel::aver_generated::kernel::ring::Mono { atoms: crate::proof_kernel::aver_generated::kernel::ring::mergeSorted(&m.atoms, &x.atoms), coef: m.coef.mul(&x.coef) }, &crate::proof_kernel::aver_generated::kernel::ring::mulMono(m, &rest)))
}

/// Two sorted lists merged into one.
pub fn mergeSorted(
    a @ _: &aver_rt::AverIntList,
    b @ _: &aver_rt::AverIntList,
) -> aver_rt::AverIntList {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (a.clone(), b.clone());
        let (__lit0, __lit1) = &__int_match_subject;
        if (*__lit0).is_empty() {
            b.clone()
        } else if (*__lit1).is_empty() {
            a.clone()
        } else if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((x, xs)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((y, ys)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            if (x > y) {
                aver_rt::AverIntList::prepend(
                    y,
                    &crate::proof_kernel::aver_generated::kernel::ring::mergeSorted(a, &ys),
                )
            } else {
                aver_rt::AverIntList::prepend(
                    x,
                    &crate::proof_kernel::aver_generated::kernel::ring::mergeSorted(&xs, b),
                )
            }
        } else {
            unreachable!("Aver Rust codegen: non-exhaustive guard-chain match")
        }
    }
}

/// Whether Int comparisons with the truth values given, weighted by nonnegative numbers and added as p >= 0, sum to a negative constant.
#[inline(always)]
pub fn linearContradiction(
    facts @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    weights @ _: &aver_rt::AverIntList,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    if (aver_rt::AverInt::from_i64(facts.len() as i64)
        == aver_rt::AverInt::from_i64(weights.len() as i64))
    {
        crate::proof_kernel::aver_generated::kernel::ring::negativeConstant(
            &crate::proof_kernel::aver_generated::kernel::ring::weightedSum(
                facts.clone(),
                weights.clone(),
                aver_rt::AverList::empty(),
                aver_rt::AverList::empty(),
            ),
        )
    } else {
        false
    }
}

/// A Bool literal.
pub fn isTruth(t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TBool(b) => true,
        _ => false,
    }
}

/// An Int comparison with its truth value as p >= 0: a < b true is b - a - 1 >= 0, a < b false is a - b >= 0, and so on.
pub fn asNonneg(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    value @ _: bool,
    atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Option<Read> {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(__pat0, a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            {
                let __dispatch_subject = __pat0;
                if &*__dispatch_subject == "<" {
                    Some(
                        crate::proof_kernel::aver_generated::kernel::ring::difference(
                            &b,
                            &a,
                            crate::proof_kernel::aver_generated::kernel::ring::strictWhen(
                                value, true,
                            ),
                            atoms,
                            value,
                            &a,
                            &b,
                        ),
                    )
                } else {
                    if &*__dispatch_subject == "<=" {
                        Some(
                            crate::proof_kernel::aver_generated::kernel::ring::difference(
                                &b,
                                &a,
                                crate::proof_kernel::aver_generated::kernel::ring::strictWhen(
                                    value, false,
                                ),
                                atoms,
                                value,
                                &a,
                                &b,
                            ),
                        )
                    } else {
                        if &*__dispatch_subject == ">" {
                            Some(
                                crate::proof_kernel::aver_generated::kernel::ring::difference(
                                    &a,
                                    &b,
                                    crate::proof_kernel::aver_generated::kernel::ring::strictWhen(
                                        value, true,
                                    ),
                                    atoms,
                                    value,
                                    &b,
                                    &a,
                                ),
                            )
                        } else {
                            if &*__dispatch_subject == ">=" {
                                Some(crate::proof_kernel::aver_generated::kernel::ring::difference(&a, &b, crate::proof_kernel::aver_generated::kernel::ring::strictWhen(value, false), atoms, value, &b, &a))
                            } else {
                                None
                            }
                        }
                    }
                }
            }
        }
        _ => None,
    }
}

/// The constant to take away: 1 for a strict comparison that holds or a non-strict one that fails.
#[inline(always)]
pub fn strictWhen(value @ _: bool, strict @ _: bool) -> aver_rt::AverInt {
    crate::proof_kernel::cancel_checkpoint();
    if (value == strict) {
        aver_rt::AverInt::from_i64(1)
    } else {
        aver_rt::AverInt::from_i64(0)
    }
}

/// upper - lower - k when the comparison holds; otherUpper - otherLower - k when it fails.
#[inline(always)]
pub fn difference(
    upper @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    lower @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    k @ _: aver_rt::AverInt,
    atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    value @ _: bool,
    otherUpper @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    otherLower @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    if value {
        crate::proof_kernel::aver_generated::kernel::ring::lessConstant(
            &crate::proof_kernel::aver_generated::kernel::ring::readPoly(
                &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    AverStr::from("-"),
                    std::sync::Arc::new(upper.clone()),
                    std::sync::Arc::new(lower.clone()),
                ),
                atoms,
            ),
            k,
        )
    } else {
        crate::proof_kernel::aver_generated::kernel::ring::lessConstant(
            &crate::proof_kernel::aver_generated::kernel::ring::readPoly(
                &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    AverStr::from("-"),
                    std::sync::Arc::new(otherUpper.clone()),
                    std::sync::Arc::new(otherLower.clone()),
                ),
                atoms,
            ),
            k,
        )
    }
}

/// A polynomial minus a constant.
pub fn lessConstant(r @ _: &Read, k @ _: aver_rt::AverInt) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::ring::Read {
        poly: crate::proof_kernel::aver_generated::kernel::ring::addPoly(
            r.poly.clone(),
            crate::proof_kernel::aver_generated::kernel::ring::constant(
                aver_rt::AverInt::from_i64(0).sub(&k),
            ),
        ),
        atoms: r.atoms.clone(),
    }
}

/// A constant below zero.
pub fn negativeConstant(p @ _: &Option<aver_rt::AverList<Mono>>) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match p.clone() {
        Some(__pat0) => {
            let __list_subject = __pat0;
            if let Some((m, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                {
                    let __list_subject = __pat1;
                    if __list_subject.is_empty() {
                        ((m.atoms == aver_rt::AverIntList::empty())
                            && (m.coef < aver_rt::AverInt::from_i64(0)))
                    } else {
                        false
                    }
                }
            } else {
                false
            }
        }
        _ => false,
    }
}
