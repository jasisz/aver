#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, Eq, Hash)]
pub struct Mono {
    pub atoms: aver_rt::AverIntList,
    pub coef: aver_rt::AverInt,
}

impl PartialEq for Mono {
    fn eq(&self, other: &Self) -> bool {
        self.coef == other.coef && self.atoms == other.atoms
    }
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

/// Whether two Int terms are the same polynomial.
pub fn samePolynomial(
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut b @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    let left @ _ = {
        let __arg4 @ _ =
            <aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>>::empty();
        crate::proof_kernel::aver_generated::kernel::ring::readPoly(a, &__arg4)
    };
    let right @ _ = crate::proof_kernel::aver_generated::kernel::ring::readPoly(b, &left.atoms);
    (left.poly == right.poly)
}

/// t as a polynomial over atoms, with the atoms it adds.
pub fn readPoly(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = &t;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TInt(__n_fields) => {
                let n = __n_fields.0.clone();
                crate::proof_kernel::aver_generated::kernel::ring::Read {
                    poly: crate::proof_kernel::aver_generated::kernel::ring::constant(n),
                    atoms: atoms.clone(),
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(____pat0_fields) => {
                let __pat0 = ____pat0_fields.0.clone();
                let a = ____pat0_fields.1.clone();
                let b = ____pat0_fields.2.clone();
                {
                    let __dispatch_subject =
                        aver_rt::AverInt::from_i64(aver_rt::str_code1(&__pat0));
                    if __dispatch_subject == aver_rt::AverInt::from_i64(43) {
                        {
                            let __arg7 @ _ =
                                crate::proof_kernel::aver_generated::kernel::ring::readPoly(
                                    a, atoms,
                                );
                            crate::proof_kernel::aver_generated::kernel::ring::combine(
                                __arg7,
                                b,
                                aver_rt::AverInt::from_i64(0),
                            )
                        }
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(45) {
                            {
                                let __arg8 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::ring::readPoly(
                                        a, atoms,
                                    );
                                crate::proof_kernel::aver_generated::kernel::ring::combine(
                                    __arg8,
                                    b,
                                    aver_rt::AverInt::from_i64(1),
                                )
                            }
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(42) {
                                {
                                    let __arg9 @ _ =
                                        crate::proof_kernel::aver_generated::kernel::ring::readPoly(
                                            a, atoms,
                                        );
                                    crate::proof_kernel::aver_generated::kernel::ring::combine(
                                        __arg9,
                                        b,
                                        aver_rt::AverInt::from_i64(2),
                                    )
                                }
                            } else {
                                crate::proof_kernel::aver_generated::kernel::ring::atom(t, atoms)
                            }
                        }
                    }
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                let a = &__a_fields.0;
                {
                    let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::ring::readPoly(
                        a.clone(),
                        atoms,
                    );
                    crate::proof_kernel::aver_generated::kernel::ring::negated(&__arg10)
                }
            }
            _ => crate::proof_kernel::aver_generated::kernel::ring::atom(t, atoms),
        }
    }
}

/// The left polynomial joined with b's: added (0), subtracted (1) or multiplied (2).
#[inline(always)]
pub fn combine(
    mut left @ _: Read,
    mut b @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    how @ _: aver_rt::AverInt,
) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    let right @ _ = crate::proof_kernel::aver_generated::kernel::ring::readPoly(b, &left.atoms);
    {
        let __dispatch_subject = how;
        if __dispatch_subject == aver_rt::AverInt::from_i64(0) {
            crate::proof_kernel::aver_generated::kernel::ring::Read {
                poly: crate::proof_kernel::aver_generated::kernel::ring::addPoly(
                    left.poly,
                    &right.poly,
                ),
                atoms: right.atoms,
            }
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(1) {
                crate::proof_kernel::aver_generated::kernel::ring::Read {
                    poly: {
                        let __arg4 @ _ =
                            crate::proof_kernel::aver_generated::kernel::ring::scalePoly(
                                &right.poly,
                                aver_rt::AverInt::from_i64(-1),
                            );
                        crate::proof_kernel::aver_generated::kernel::ring::addPoly(
                            left.poly, &__arg4,
                        )
                    },
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
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::ring::indexOf(
        &t,
        atoms,
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
            atoms: aver_rt::AverList::concat(&atoms.clone(), &aver_rt::AverList::from_vec(vec![t])),
        },
    }
}

/// Where an atom is, if it is there.
#[inline(always)]
pub fn indexOf(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut i @ _: aver_rt::AverInt,
) -> Option<aver_rt::AverInt> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>;
    let mut atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> =
        atoms;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = atoms;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (a == t) {
                        return Some(i);
                    } else {
                        {
                            let __tco1 = rest;
                            let __tco2 = i.add(&aver_rt::AverInt::from_i64(1));
                            __hold1 = __tco1;
                            atoms = &__hold1;
                            i = __tco2;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The sum of two polynomials, monomials kept in order and merged.
#[inline(always)]
pub fn addPoly(
    mut a @ _: aver_rt::AverList<Mono>,
    b @ _: &aver_rt::AverList<Mono>,
) -> aver_rt::AverList<Mono> {
    let mut __hold1: aver_rt::AverList<Mono>;
    let mut b @ _: &aver_rt::AverList<Mono> = b;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = b;
            if __list_subject.is_empty() {
                return a;
            } else {
                if let (Some(m), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = crate::proof_kernel::aver_generated::kernel::ring::insertMono(
                            m.clone(),
                            &a,
                        );
                        let __tco1 = rest;
                        a = __tco0;
                        __hold1 = __tco1;
                        b = &__hold1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Add one monomial to a polynomial in order; a coefficient that sums to zero drops it.
#[inline(always)]
pub fn insertMono(mut m @ _: Mono, p @ _: &aver_rt::AverList<Mono>) -> aver_rt::AverList<Mono> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = p;
        if __list_subject.is_empty() {
            crate::proof_kernel::aver_generated::kernel::ring::nonzero(m)
        } else {
            if let Some((x, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                {
                    let __subject = aver_rt::AverInt::from_i64(
                        crate::proof_kernel::aver_generated::kernel::ring::compareAtoms(
                            m.atoms.clone(),
                            x.atoms.clone(),
                        ),
                    );
                    {
                        let __dispatch_subject = __subject;
                        if __dispatch_subject == aver_rt::AverInt::from_i64(0) {
                            aver_rt::AverList::concat(
                                &{
                                    let __arg4 @ _ =
                                        crate::proof_kernel::aver_generated::kernel::ring::Mono {
                                            atoms: x.atoms,
                                            coef: x.coef.add(&m.coef),
                                        };
                                    crate::proof_kernel::aver_generated::kernel::ring::nonzero(
                                        __arg4,
                                    )
                                },
                                &rest,
                            )
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(1) {
                                aver_rt::AverList::prepend_owned(
                                    x,
                                    crate::proof_kernel::aver_generated::kernel::ring::insertMono(
                                        m, &rest,
                                    ),
                                )
                            } else {
                                aver_rt::AverList::concat(
                                    &crate::proof_kernel::aver_generated::kernel::ring::nonzero(m),
                                    &p.clone(),
                                )
                            }
                        }
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// A monomial, unless its coefficient is zero.
#[inline(always)]
pub fn nonzero(mut m @ _: Mono) -> aver_rt::AverList<Mono> {
    crate::proof_kernel::cancel_checkpoint();
    if (m.coef == aver_rt::AverInt::from_i64(0)) {
        aver_rt::AverList::empty()
    } else {
        aver_rt::AverList::from_vec(vec![m])
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
    {
        let __list_subject = p;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(m), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::concat(
                    &{
                        let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::ring::Mono {
                            atoms: m.atoms.clone(),
                            coef: m.coef.mul(&k),
                        };
                        crate::proof_kernel::aver_generated::kernel::ring::nonzero(__arg4)
                    },
                    &crate::proof_kernel::aver_generated::kernel::ring::scalePoly(&rest, k),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The product of two polynomials.
#[inline(always)]
pub fn mulPoly(
    a @ _: &aver_rt::AverList<Mono>,
    b @ _: &aver_rt::AverList<Mono>,
) -> aver_rt::AverList<Mono> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = a;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(m), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let __arg4 @ _ =
                        crate::proof_kernel::aver_generated::kernel::ring::mulMono(m, b);
                    {
                        let __arg5 @ _ =
                            crate::proof_kernel::aver_generated::kernel::ring::mulPoly(&rest, b);
                        crate::proof_kernel::aver_generated::kernel::ring::addPoly(__arg4, &__arg5)
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// One monomial times a polynomial.
#[inline(always)]
pub fn mulMono(m @ _: &Mono, p @ _: &aver_rt::AverList<Mono>) -> aver_rt::AverList<Mono> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = p;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::ring::Mono {
                        atoms: crate::proof_kernel::aver_generated::kernel::ring::mergeSorted(
                            &m.atoms, &x.atoms,
                        ),
                        coef: m.coef.mul(&x.coef),
                    };
                    {
                        let __arg5 @ _ =
                            crate::proof_kernel::aver_generated::kernel::ring::mulMono(m, &rest);
                        crate::proof_kernel::aver_generated::kernel::ring::insertMono(
                            __arg4, &__arg5,
                        )
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
                aver_rt::AverIntList::prepend_owned(
                    y,
                    crate::proof_kernel::aver_generated::kernel::ring::mergeSorted(a, &ys),
                )
            } else {
                aver_rt::AverIntList::prepend_owned(
                    x,
                    crate::proof_kernel::aver_generated::kernel::ring::mergeSorted(&xs, b),
                )
            }
        } else {
            unreachable!("Aver Rust codegen: non-exhaustive guard-chain match")
        }
    }
}

/// Whether Int comparisons with the truth values given, read as p >= 0, then rows rounded from them (cuts), weighted by nonnegative numbers, sum to a negative constant.
#[inline(always)]
pub fn linearContradiction(
    mut facts @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    cuts @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Cut>,
    weights @ _: &aver_rt::AverIntList,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = {
            let __arg5 @ _ = <aver_rt::AverList<aver_rt::AverList<Mono>>>::empty();
            {
                let __arg6 @ _ = <aver_rt::AverList<
                    crate::proof_kernel::aver_generated::kernel::term::Term,
                >>::empty();
                crate::proof_kernel::aver_generated::kernel::ring::readRows(facts, __arg5, &__arg6)
            }
        };
        match __subject {
            None => false,
            Some(rows @ _) => {
                match crate::proof_kernel::aver_generated::kernel::ring::withCuts(rows, cuts) {
                    None => false,
                    Some(all @ _) => {
                        let __arg8 @ _ = {
                            let __arg7 @ _ = <aver_rt::AverList<Mono>>::empty();
                            crate::proof_kernel::aver_generated::kernel::ring::sumRows(
                                all,
                                weights.clone(),
                                __arg7,
                            )
                        };
                        crate::proof_kernel::aver_generated::kernel::ring::negativeConstant(&__arg8)
                    }
                }
            }
        }
    }
}

/// Each fact read as p >= 0, over one list of atoms; None when one is not an Int comparison with a Bool value.
#[inline(always)]
pub fn readRows(
    mut facts @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    mut rows @ _: aver_rt::AverList<aver_rt::AverList<Mono>>,
    atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Option<aver_rt::AverList<aver_rt::AverList<Mono>>> {
    let mut __hold2: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>;
    let mut atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> =
        atoms;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(facts, [] => { return Some(rows); }, [f, fs] => { if crate::proof_kernel::aver_generated::kernel::ring::isTruth(&f.rhs) { { let __subject = { let __arg6 @ _ = (f.rhs == crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true))); crate::proof_kernel::aver_generated::kernel::ring::asNonneg(&f.lhs, __arg6, atoms) }; match __subject { None => { return None; }, Some(read @ _) => { {
            let __tco0 = fs;
            let __tco1 = aver_rt::AverList::concat(&rows, &aver_rt::AverList::from_vec(vec![read.poly]));
            let __tco2 = read.atoms;
            facts = __tco0;
            rows = __tco1;
            __hold2 = __tco2;
            atoms = &__hold2;
            continue;
        } } } } } else { return None; } })
    }
}

/// The rows weighted and added, one nonnegative weight per row; None otherwise.
pub fn sumRows(
    mut rows @ _: aver_rt::AverList<aver_rt::AverList<Mono>>,
    mut weights @ _: aver_rt::AverIntList,
    mut acc @ _: aver_rt::AverList<Mono>,
) -> Option<aver_rt::AverList<Mono>> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __int_match_subject = (rows, weights);
            let (__lit0, __lit1) = &__int_match_subject;
            if (*__lit0).is_empty() && (*__lit1).is_empty() {
                return Some(acc);
            } else if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
                let Some((p, ps)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                let Some((w, ws)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                if (w < aver_rt::AverInt::from_i64(0)) {
                    return None;
                } else {
                    {
                        let __tco0 = ps;
                        let __tco1 = ws;
                        let __tco2 = {
                            let __arg7 @ _ =
                                crate::proof_kernel::aver_generated::kernel::ring::scalePoly(&p, w);
                            crate::proof_kernel::aver_generated::kernel::ring::addPoly(acc, &__arg7)
                        };
                        rows = __tco0;
                        weights = __tco1;
                        acc = __tco2;
                        continue;
                    }
                }
            } else {
                return None;
            }
        }
    }
}

/// The rows, then each cut's row: the weighted sum of the rows before it, rounded.
#[inline(always)]
pub fn withCuts(
    mut rows @ _: aver_rt::AverList<aver_rt::AverList<Mono>>,
    cuts @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Cut>,
) -> Option<aver_rt::AverList<aver_rt::AverList<Mono>>> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Cut>;
    let mut cuts @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Cut> =
        cuts;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = cuts;
            if __list_subject.is_empty() {
                return Some(rows);
            } else {
                if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __subject = {
                            let __arg6 @ _ = <aver_rt::AverList<Mono>>::empty();
                            crate::proof_kernel::aver_generated::kernel::ring::sumRows(
                                rows.clone(),
                                c.weights.clone(),
                                __arg6,
                            )
                        };
                        match __subject {
                            None => {
                                return None;
                            }
                            Some(sum @ _) => {
                                match crate::proof_kernel::aver_generated::kernel::ring::rounded(
                                    &sum,
                                    c.divisor.clone(),
                                ) {
                                    None => {
                                        return None;
                                    }
                                    Some(row @ _) => {
                                        let __tco0 = aver_rt::AverList::concat(
                                            &rows,
                                            &aver_rt::AverList::from_vec(vec![row]),
                                        );
                                        let __tco1 = rest;
                                        rows = __tco0;
                                        __hold1 = __tco1;
                                        cuts = &__hold1;
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// p >= 0 divided by d > 0, sound over the integers: every coefficient but the constant a multiple of d, the constant rounded down.
#[inline(always)]
pub fn rounded(
    p @ _: &aver_rt::AverList<Mono>,
    d @ _: aver_rt::AverInt,
) -> Option<aver_rt::AverList<Mono>> {
    crate::proof_kernel::cancel_checkpoint();
    if (d > aver_rt::AverInt::from_i64(0)) {
        {
            let __arg2 @ _ = <aver_rt::AverList<Mono>>::empty();
            crate::proof_kernel::aver_generated::kernel::ring::roundedBy(p, d, __arg2)
        }
    } else {
        None
    }
}

/// Each monomial of p divided by d, in order.
#[inline(always)]
pub fn roundedBy(
    p @ _: &aver_rt::AverList<Mono>,
    mut d @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<Mono>,
) -> Option<aver_rt::AverList<Mono>> {
    let mut __hold0: aver_rt::AverList<Mono>;
    let mut p @ _: &aver_rt::AverList<Mono> = p;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = p;
            if __list_subject.is_empty() {
                return Some(acc);
            } else {
                if let (Some(m), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __subject = (
                            (match (m.coef).div_euclid(&(d)) {
                                Some(__q) => Ok(__q),
                                None => Err("division by zero".to_string()),
                            })
                            .into_aver(),
                            (match (m.coef).rem_euclid(&(d)) {
                                Some(__r) => Ok(__r),
                                None => Err("division by zero".to_string()),
                            })
                            .into_aver(),
                        );
                        match __subject {
                            (Ok(q), Ok(r)) => {
                                if ((m.atoms == aver_rt::AverIntList::empty())
                                    || (r == aver_rt::AverInt::from_i64(0)))
                                {
                                    {
                                        let __tco0 = rest;
                                        let __tco2 = aver_rt::AverList::concat(&acc, &{
                                            let __arg7 @ _ = crate::proof_kernel::aver_generated::kernel::ring::Mono { atoms: m.atoms.clone(), coef: q };
                                            crate::proof_kernel::aver_generated::kernel::ring::nonzero(__arg7)
                                        });
                                        __hold0 = __tco0;
                                        p = &__hold0;
                                        acc = __tco2;
                                        continue;
                                    }
                                } else {
                                    return None;
                                }
                            }
                            _ => {
                                return None;
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A Bool literal.
pub fn isTruth(t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TBool(__b_fields) => {
                let b = __b_fields.0.clone();
                true
            }
            _ => false,
        }
    }
}

/// An Int comparison with its truth value as p >= 0: a < b true is b - a - 1 >= 0, a < b false is a - b >= 0, and so on.
pub fn asNonneg(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    value @ _: bool,
    atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Option<Read> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(____pat0_fields) => {
                let __pat0 = ____pat0_fields.0.clone();
                let a = &____pat0_fields.1;
                let b = &____pat0_fields.2;
                {
                    let __dispatch_subject = __pat0;
                    if &*__dispatch_subject == "<" {
                        Some({
                            let __arg6 @ _ =
                                crate::proof_kernel::aver_generated::kernel::ring::strictWhen(
                                    value, true,
                                );
                            crate::proof_kernel::aver_generated::kernel::ring::difference(
                                b.clone(),
                                a.clone(),
                                __arg6,
                                atoms,
                                value,
                                a.clone(),
                                b.clone(),
                            )
                        })
                    } else {
                        if &*__dispatch_subject == "<=" {
                            Some({
                                let __arg7 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::ring::strictWhen(
                                        value, false,
                                    );
                                crate::proof_kernel::aver_generated::kernel::ring::difference(
                                    b.clone(),
                                    a.clone(),
                                    __arg7,
                                    atoms,
                                    value,
                                    a.clone(),
                                    b.clone(),
                                )
                            })
                        } else {
                            if &*__dispatch_subject == ">" {
                                Some({
                                    let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::ring::strictWhen(value, true);
                                    crate::proof_kernel::aver_generated::kernel::ring::difference(
                                        a.clone(),
                                        b.clone(),
                                        __arg8,
                                        atoms,
                                        value,
                                        b.clone(),
                                        a.clone(),
                                    )
                                })
                            } else {
                                if &*__dispatch_subject == ">=" {
                                    Some({
                                        let __arg9 @ _ = crate::proof_kernel::aver_generated::kernel::ring::strictWhen(value, false);
                                        crate::proof_kernel::aver_generated::kernel::ring::difference(a.clone(), b.clone(), __arg9, atoms, value, b.clone(), a.clone())
                                    })
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
    mut upper @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut lower @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    k @ _: aver_rt::AverInt,
    atoms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    value @ _: bool,
    mut otherUpper @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut otherLower @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    if value {
        {
            let __arg8 @ _ = {
                let __arg7 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                        ::aver_rt::str_lit!("-"),
                        upper,
                        lower,
                    ),
                );
                crate::proof_kernel::aver_generated::kernel::ring::readPoly(__arg7, atoms)
            };
            crate::proof_kernel::aver_generated::kernel::ring::lessConstant(&__arg8, k)
        }
    } else {
        {
            let __arg10 @ _ = {
                let __arg9 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                        ::aver_rt::str_lit!("-"),
                        otherUpper,
                        otherLower,
                    ),
                );
                crate::proof_kernel::aver_generated::kernel::ring::readPoly(__arg9, atoms)
            };
            crate::proof_kernel::aver_generated::kernel::ring::lessConstant(&__arg10, k)
        }
    }
}

/// A polynomial minus a constant.
pub fn lessConstant(r @ _: &Read, k @ _: aver_rt::AverInt) -> Read {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::ring::Read {
        poly: {
            let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::ring::constant(
                aver_rt::AverInt::from_i64(0).sub(&k),
            );
            crate::proof_kernel::aver_generated::kernel::ring::addPoly(r.poly.clone(), &__arg2)
        },
        atoms: r.atoms.clone(),
    }
}

/// A constant below zero.
pub fn negativeConstant(p @ _: &Option<aver_rt::AverList<Mono>>) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match p {
        Some(__pat0) => {
            let __pat0 = __pat0.clone();
            {
                let __list_subject = __pat0;
                if let (Some(m), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
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
        }
        _ => false,
    }
}
