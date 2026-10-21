#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Schema {
    pub binders: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    pub premises: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    pub concl: crate::proof_kernel::aver_generated::kernel::term::Eqn,
}

impl PartialOrd for Schema {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Schema {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.binders.cmp(&other.binders))
            .then_with(|| self.concl.cmp(&other.concl))
            .then_with(|| self.premises.cmp(&other.premises))
    }
}

impl aver_rt::AverDisplay for Schema {
    fn aver_display(&self) -> String {
        format!(
            "Schema({})",
            vec![
                format!("binders: {}", self.binders.aver_display_inner()),
                format!("premises: {}", self.premises.aver_display_inner()),
                format!("concl: {}", self.concl.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

/// A schema variable.
pub fn v(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
        crate::proof_kernel::aver_generated::kernel::term::Term__TVar(n),
    )
}

/// A binder of type Int.
#[inline(always)]
pub fn i(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::typed(
        n,
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt,
    )
}

/// A binder of type Bool.
#[inline(always)]
pub fn bo(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::typed(
        n,
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyBool,
    )
}

/// A binder of the rule's one type ?t.
#[inline(always)]
pub fn any(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::typed(
        n,
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
            crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(AverStr::from("t"), false),
        ),
    )
}

/// A binder of the rule's one type ?t, whose == must be equality.
#[inline(always)]
pub fn same(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::typed(
        n,
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
            crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(AverStr::from("t"), true),
        ),
    )
}

/// A binder of type List<?t>.
#[inline(always)]
pub fn lt(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::typed(
        n,
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyList(
            crate::proof_kernel::aver_generated::kernel::term::Ty__TyList(
                crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
                    crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
                        AverStr::from("t"),
                        false,
                    ),
                ),
            ),
        ),
    )
}

/// A binder of type Map<?k, ?v>, the key with an equality.
#[inline(always)]
pub fn kv(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::typed(
        n,
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyMap(
            crate::proof_kernel::aver_generated::kernel::term::Ty__TyMap(
                crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
                    crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
                        AverStr::from("k"),
                        true,
                    ),
                ),
                crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
                    crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
                        AverStr::from("v"),
                        false,
                    ),
                ),
            ),
        ),
    )
}

/// A binder of the map's key type ?k.
#[inline(always)]
pub fn key(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::typed(
        n,
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
            crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(AverStr::from("k"), true),
        ),
    )
}

/// A binder of the map's value type ?v.
#[inline(always)]
pub fn val(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::typed(
        n,
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
            crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(AverStr::from("v"), false),
        ),
    )
}

/// A binder of type Vector<?t>.
#[inline(always)]
pub fn vec(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::typed(
        n,
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyVector(
            crate::proof_kernel::aver_generated::kernel::term::Ty__TyVector(
                crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
                    crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
                        AverStr::from("t"),
                        false,
                    ),
                ),
            ),
        ),
    )
}

/// An equation.
pub fn eq(
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Eqn {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r }
}

/// A Bool term equals a literal.
#[inline(always)]
pub fn is(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    value @ _: bool,
) -> crate::proof_kernel::aver_generated::kernel::term::Eqn {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::rules::eq(
        t,
        crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
            crate::proof_kernel::aver_generated::kernel::term::Term__TBool(value),
        ),
    )
}

/// Bool.and or Bool.or.
pub fn conn(
    name @ _: AverStr,
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut b @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            name,
            aver_rt::AverList::from_vec(vec![a, b]),
        ),
    )
}

/// A rule without premises.
pub fn plain(
    binders @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
        binders: binders.clone(),
        premises: aver_rt::AverList::empty(),
        concl: crate::proof_kernel::aver_generated::kernel::rules::eq(l, r),
    })
}

/// The rule with this identifier.
#[inline(always)]
pub fn schema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "bool.and.true_l" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from("b")),
                ]),
                crate::proof_kernel::aver_generated::kernel::rules::conn(
                    AverStr::from("Bool.and"),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true),
                    ),
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                ),
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
            )
        } else {
            if &*__dispatch_subject == "bool.and.false_l" {
                crate::proof_kernel::aver_generated::kernel::rules::plain(
                    &aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from("b")),
                    ]),
                    crate::proof_kernel::aver_generated::kernel::rules::conn(
                        AverStr::from("Bool.and"),
                        crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false),
                        ),
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                    ),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false),
                    ),
                )
            } else {
                if &*__dispatch_subject == "bool.and.true_r" {
                    crate::proof_kernel::aver_generated::kernel::rules::plain(
                        &aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from(
                                "a",
                            )),
                        ]),
                        crate::proof_kernel::aver_generated::kernel::rules::conn(
                            AverStr::from("Bool.and"),
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "a",
                            )),
                            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                    true,
                                ),
                            ),
                        ),
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
                    )
                } else {
                    if &*__dispatch_subject == "bool.and.false_r" {
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::bo(
                                    AverStr::from("a"),
                                ),
                            ]),
                            crate::proof_kernel::aver_generated::kernel::rules::conn(
                                AverStr::from("Bool.and"),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("a"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                        false,
                                    ),
                                ),
                            ),
                            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                    false,
                                ),
                            ),
                        )
                    } else {
                        if &*__dispatch_subject == "bool.or.true_l" {
                            crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from("b"))]), crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.or"), crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true)), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b"))), crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true)))
                        } else {
                            if &*__dispatch_subject == "bool.or.false_l" {
                                crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from("b"))]), crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.or"), crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false)), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b"))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")))
                            } else {
                                if &*__dispatch_subject == "bool.or.true_r" {
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from("a"))]), crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.or"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true))), crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true)))
                                } else {
                                    if &*__dispatch_subject == "bool.or.false_r" {
                                        crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from("a"))]), crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.or"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))
                                    } else {
                                        if &*__dispatch_subject == "bool.and.elim_l" {
                                            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from("b"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.and"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b"))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), true) })
                                        } else {
                                            if &*__dispatch_subject == "bool.and.elim_r" {
                                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::bo(AverStr::from("b"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.and"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b"))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")), true) })
                                            } else {
                                                crate::proof_kernel::aver_generated::kernel::rules::comparisonSchema(id)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// A decided comparison decides another of the same two terms.
pub fn complement(
    p @ _: AverStr,
    pv @ _: bool,
    q @ _: AverStr,
    qv @ _: bool,
    binders @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
        binders: binders.clone(),
        premises: aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::rules::is(
                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                        p,
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                    ),
                ),
                pv,
            ),
        ]),
        concl: crate::proof_kernel::aver_generated::kernel::rules::is(
            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    q,
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                ),
            ),
            qv,
        ),
    })
}

/// != on Int as the negation of ==, an Int equality decided by the two orders, == as equality, as reflexive and as symmetric where the type's == is equality, and the product of two nonnegative Ints. A comparison decided by its complement (a > b false gives a <= b true) is a linear step, not a rule.
pub fn comparisonSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let ints @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")),
        crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("b")),
    ]);
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "int.ne.def" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &ints,
                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                        AverStr::from("!="),
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                    ),
                ),
                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                        AverStr::from("Bool.not"),
                        aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                    AverStr::from("=="),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("a"),
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("b"),
                                    ),
                                ),
                            ),
                        ]),
                    ),
                ),
            )
        } else {
            if &*__dispatch_subject == "int.eq.of_le_ge" {
                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                    binders: ints,
                    premises: aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::is(
                            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                    AverStr::from("<="),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("a"),
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("b"),
                                    ),
                                ),
                            ),
                            true,
                        ),
                        crate::proof_kernel::aver_generated::kernel::rules::is(
                            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                    AverStr::from(">="),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("a"),
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("b"),
                                    ),
                                ),
                            ),
                            true,
                        ),
                    ]),
                    concl: crate::proof_kernel::aver_generated::kernel::rules::is(
                        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                AverStr::from("=="),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("a"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("b"),
                                ),
                            ),
                        ),
                        true,
                    ),
                })
            } else {
                if &*__dispatch_subject == "int.mul_nonneg" {
                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: ints, premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))), true), crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("*"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")))))), true) })
                } else {
                    if &*__dispatch_subject == "int.eq.false_of_lt" {
                        crate::proof_kernel::aver_generated::kernel::rules::complement(
                            AverStr::from("<"),
                            true,
                            AverStr::from("=="),
                            false,
                            &ints,
                        )
                    } else {
                        if &*__dispatch_subject == "int.eq.false_of_gt" {
                            crate::proof_kernel::aver_generated::kernel::rules::complement(
                                AverStr::from(">"),
                                true,
                                AverStr::from("=="),
                                false,
                                &ints,
                            )
                        } else {
                            if &*__dispatch_subject == "int.eq.of_beq" {
                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::same(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::same(AverStr::from("b"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("=="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b"))) })
                            } else {
                                if &*__dispatch_subject == "bool.beq.refl" {
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::same(AverStr::from("a"))]), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("=="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))), crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true)))
                                } else {
                                    if &*__dispatch_subject == "bool.beq.symm" {
                                        crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::same(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::same(AverStr::from("b"))]), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("=="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")))), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("=="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))))
                                    } else {
                                        crate::proof_kernel::aver_generated::kernel::rules::divisionSchema(id)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Euclidean division by a literal, as the compiler writes it.
pub fn divE(
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut k @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            AverStr::from("__int_div_euclid"),
            aver_rt::AverList::from_vec(vec![a, k]),
        ),
    )
}

/// Euclidean remainder by a literal.
pub fn modE(
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut k @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            AverStr::from("__int_mod_euclid"),
            aver_rt::AverList::from_vec(vec![a, k]),
        ),
    )
}

/// 0 <= t and t < bound.
#[inline(always)]
pub fn range(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut bound @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::rules::conn(
        AverStr::from("Bool.and"),
        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                AverStr::from("<="),
                crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                        aver_rt::AverInt::from_i64(0),
                    ),
                ),
                t.clone(),
            ),
        ),
        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                AverStr::from("<"),
                t,
                bound,
            ),
        ),
    )
}

/// Int.div and Int.mod by a divisor that is not 0 as the Euclidean quotient and remainder, and Euclidean division by a positive divisor.
#[inline(always)]
pub fn divisionSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "int.div.ok" {
            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                binders: aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")),
                    crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("k")),
                ]),
                premises: aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::is(
                        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                AverStr::from("=="),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("k"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                                        aver_rt::AverInt::from_i64(0),
                                    ),
                                ),
                            ),
                        ),
                        false,
                    ),
                ]),
                concl: crate::proof_kernel::aver_generated::kernel::rules::eq(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            AverStr::from("Int.div"),
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("a"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("k"),
                                ),
                            ]),
                        ),
                    ),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                            AverStr::from("Result.Ok"),
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::divE(
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("a"),
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("k"),
                                    ),
                                ),
                            ]),
                        ),
                    ),
                ),
            })
        } else {
            if &*__dispatch_subject == "int.mod.ok" {
                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("k"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("=="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))), false)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("Int.mod"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))]))), crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(AverStr::from("Result.Ok"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::modE(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")))])))) })
            } else {
                if &*__dispatch_subject == "int.div_mod_recompose" {
                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("k"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from(">"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("+"), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("*"), crate::proof_kernel::aver_generated::kernel::rules::divE(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")))), crate::proof_kernel::aver_generated::kernel::rules::modE(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a"))) })
                } else {
                    if &*__dispatch_subject == "int.mod_range" {
                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("k"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from(">"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::range(crate::proof_kernel::aver_generated::kernel::rules::modE(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))), true) })
                    } else {
                        if &*__dispatch_subject == "int.div_range" {
                            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("k")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("m")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("n"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::range(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m"))), true), crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.and"), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from(">"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("=="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m")), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("*"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))))))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::range(crate::proof_kernel::aver_generated::kernel::rules::divE(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))), true) })
                        } else {
                            crate::proof_kernel::aver_generated::kernel::rules::bitsSchema(id)
                        }
                    }
                }
            }
        }
    }
}

/// A shift left by a count, as the compiler writes Bits.shiftLeft with a literal count.
pub fn shl(
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut n @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            AverStr::from("__bits_shl"),
            aver_rt::AverList::from_vec(vec![a, n]),
        ),
    )
}

/// 0 <= n and k == 2^n, written as a shift of 1.
#[inline(always)]
pub fn powerOfTwo(
    mut k @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::rules::conn(
        AverStr::from("Bool.and"),
        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                AverStr::from("<="),
                crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                        aver_rt::AverInt::from_i64(0),
                    ),
                ),
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")),
            ),
        ),
        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                AverStr::from("=="),
                k,
                crate::proof_kernel::aver_generated::kernel::rules::shl(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                            aver_rt::AverInt::from_i64(1),
                        ),
                    ),
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")),
                ),
            ),
        ),
    )
}

/// The bit-level view of Int, infinite two's complement: complement, a shift left, a low mask 2^n - 1 and a one-bit mask 2^n.
#[inline(always)]
pub fn bitsSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "bits.not" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")),
                ]),
                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                        AverStr::from("Bits.not"),
                        aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "a",
                            )),
                        ]),
                    ),
                ),
                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                        AverStr::from("-"),
                        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                AverStr::from("-"),
                                crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                                        aver_rt::AverInt::from_i64(0),
                                    ),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("a"),
                                ),
                            ),
                        ),
                        crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                                aver_rt::AverInt::from_i64(1),
                            ),
                        ),
                    ),
                ),
            )
        } else {
            if &*__dispatch_subject == "bits.shl" {
                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                    binders: aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")),
                        crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("n")),
                        crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("k")),
                    ]),
                    premises: aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::is(
                            crate::proof_kernel::aver_generated::kernel::rules::powerOfTwo(
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("k"),
                                ),
                            ),
                            true,
                        ),
                    ]),
                    concl: crate::proof_kernel::aver_generated::kernel::rules::eq(
                        crate::proof_kernel::aver_generated::kernel::rules::shl(
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "a",
                            )),
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "n",
                            )),
                        ),
                        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                AverStr::from("*"),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("a"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("k"),
                                ),
                            ),
                        ),
                    ),
                })
            } else {
                if &*__dispatch_subject == "bits.and_low" {
                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("m")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("k")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("n"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.and"), crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.and"), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")))), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("=="), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("+"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(1))))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))))), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("=="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")), crate::proof_kernel::aver_generated::kernel::rules::shl(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(1))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")))))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("Bits.and"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m"))]))), crate::proof_kernel::aver_generated::kernel::rules::modE(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")))) })
                } else {
                    if &*__dispatch_subject == "bits.and_bit" {
                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("k")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("n"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::powerOfTwo(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("Bits.and"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))]))), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("*"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")), crate::proof_kernel::aver_generated::kernel::rules::modE(crate::proof_kernel::aver_generated::kernel::rules::divE(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(2))))))) })
                    } else {
                        crate::proof_kernel::aver_generated::kernel::rules::listSchema(id)
                    }
                }
            }
        }
    }
}

/// An element in front of a list.
pub fn cons(
    mut x @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut xs @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            AverStr::from("List.prepend"),
            aver_rt::AverList::from_vec(vec![x, xs]),
        ),
    )
}

/// A list builtin applied to its arguments.
pub fn lb(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("List."));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                __b
            }),
            args.clone(),
        ),
    )
}

/// A rule on a count n that holds when n > 0 (positive) or when n <= 0.
pub fn counted(
    binders @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    positive @ _: bool,
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let premise @ _ = if positive {
        crate::proof_kernel::aver_generated::kernel::rules::is(
            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    AverStr::from(">"),
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                ),
            ),
            true,
        )
    } else {
        crate::proof_kernel::aver_generated::kernel::rules::is(
            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    AverStr::from("<="),
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                ),
            ),
            true,
        )
    };
    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
        binders: binders.clone(),
        premises: aver_rt::AverList::from_vec(vec![premise]),
        concl: crate::proof_kernel::aver_generated::kernel::rules::eq(l, r),
    })
}

/// Each list builtin on an element in front of a list, and on the empty list where it has another argument; List.len([]) and List.reverse([]) are closed, so compute shows them.
pub fn listSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let xa @ _ = crate::proof_kernel::aver_generated::kernel::rules::cons(
        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")),
        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
    );
    let nil @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TList(
        crate::proof_kernel::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty()),
    );
    let less @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
            AverStr::from("-"),
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")),
            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                    aver_rt::AverInt::from_i64(1),
                ),
            ),
        ),
    );
    let xan @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("x")),
        crate::proof_kernel::aver_generated::kernel::rules::lt(AverStr::from("a")),
        crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("n")),
    ]);
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "list.concat.nil" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::lt(AverStr::from("b")),
                ]),
                crate::proof_kernel::aver_generated::kernel::rules::lb(
                    AverStr::from("concat"),
                    &aver_rt::AverList::from_vec(vec![
                        nil,
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                    ]),
                ),
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
            )
        } else {
            if &*__dispatch_subject == "list.concat.cons" {
                crate::proof_kernel::aver_generated::kernel::rules::plain(
                    &aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("x")),
                        crate::proof_kernel::aver_generated::kernel::rules::lt(AverStr::from("a")),
                        crate::proof_kernel::aver_generated::kernel::rules::lt(AverStr::from("b")),
                    ]),
                    crate::proof_kernel::aver_generated::kernel::rules::lb(
                        AverStr::from("concat"),
                        &aver_rt::AverList::from_vec(vec![
                            xa,
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "b",
                            )),
                        ]),
                    ),
                    crate::proof_kernel::aver_generated::kernel::rules::cons(
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")),
                        crate::proof_kernel::aver_generated::kernel::rules::lb(
                            AverStr::from("concat"),
                            &aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("a"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("b"),
                                ),
                            ]),
                        ),
                    ),
                )
            } else {
                if &*__dispatch_subject == "list.len.cons" {
                    crate::proof_kernel::aver_generated::kernel::rules::plain(
                        &aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from(
                                "x",
                            )),
                            crate::proof_kernel::aver_generated::kernel::rules::lt(AverStr::from(
                                "a",
                            )),
                        ]),
                        crate::proof_kernel::aver_generated::kernel::rules::lb(
                            AverStr::from("len"),
                            &aver_rt::AverList::from_vec(vec![xa]),
                        ),
                        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                AverStr::from("+"),
                                crate::proof_kernel::aver_generated::kernel::rules::lb(
                                    AverStr::from("len"),
                                    &aver_rt::AverList::from_vec(vec![
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("a"),
                                        ),
                                    ]),
                                ),
                                crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                                        aver_rt::AverInt::from_i64(1),
                                    ),
                                ),
                            ),
                        ),
                    )
                } else {
                    if &*__dispatch_subject == "list.take.nil" {
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    AverStr::from("n"),
                                ),
                            ]),
                            crate::proof_kernel::aver_generated::kernel::rules::lb(
                                AverStr::from("take"),
                                &aver_rt::AverList::from_vec(vec![
                                    nil.clone(),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("n"),
                                    ),
                                ]),
                            ),
                            nil,
                        )
                    } else {
                        if &*__dispatch_subject == "list.take.cons_le" {
                            crate::proof_kernel::aver_generated::kernel::rules::counted(
                                &xan,
                                false,
                                crate::proof_kernel::aver_generated::kernel::rules::lb(
                                    AverStr::from("take"),
                                    &aver_rt::AverList::from_vec(vec![
                                        xa,
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("n"),
                                        ),
                                    ]),
                                ),
                                nil,
                            )
                        } else {
                            if &*__dispatch_subject == "list.take.cons_gt" {
                                crate::proof_kernel::aver_generated::kernel::rules::counted(&xan, true, crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("take"), &aver_rt::AverList::from_vec(vec![xa, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))])), crate::proof_kernel::aver_generated::kernel::rules::cons(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")), crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("take"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), less]))))
                            } else {
                                if &*__dispatch_subject == "list.drop.nil" {
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("n"))]), crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("drop"), &aver_rt::AverList::from_vec(vec![nil.clone(), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))])), nil)
                                } else {
                                    if &*__dispatch_subject == "list.drop.cons_le" {
                                        crate::proof_kernel::aver_generated::kernel::rules::counted(&xan, false, crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("drop"), &aver_rt::AverList::from_vec(vec![xa.clone(), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))])), xa)
                                    } else {
                                        if &*__dispatch_subject == "list.drop.cons_gt" {
                                            crate::proof_kernel::aver_generated::kernel::rules::counted(&xan, true, crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("drop"), &aver_rt::AverList::from_vec(vec![xa, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))])), crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("drop"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), less])))
                                        } else {
                                            if &*__dispatch_subject == "list.reverse.cons" {
                                                crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("x")), crate::proof_kernel::aver_generated::kernel::rules::lt(AverStr::from("a"))]), crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("reverse"), &aver_rt::AverList::from_vec(vec![xa])), crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("concat"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("reverse"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a"))])), crate::proof_kernel::aver_generated::kernel::rules::cons(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")), nil)])))
                                            } else {
                                                crate::proof_kernel::aver_generated::kernel::rules::mapSchema(id)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// A Map builtin applied to its arguments; the empty map is Map.empty().
pub fn mb(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("Map."));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                __b
            }),
            args.clone(),
        ),
    )
}

/// Each Map read on the empty map and on a set: at the key set, at another key, and the size by membership.
pub fn mapSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let set @ _ = crate::proof_kernel::aver_generated::kernel::rules::mb(
        AverStr::from("set"),
        &aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m")),
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")),
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")),
        ]),
    );
    let empty @ _ = crate::proof_kernel::aver_generated::kernel::rules::mb(
        AverStr::from("empty"),
        &aver_rt::AverList::empty(),
    );
    let other @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::is(
            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    AverStr::from("!="),
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")),
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k2")),
                ),
            ),
            true,
        ),
    ]);
    let mkv @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::kv(AverStr::from("m")),
        crate::proof_kernel::aver_generated::kernel::rules::key(AverStr::from("k")),
        crate::proof_kernel::aver_generated::kernel::rules::val(AverStr::from("v")),
    ]);
    let mkv2 @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::kv(AverStr::from("m")),
        crate::proof_kernel::aver_generated::kernel::rules::key(AverStr::from("k")),
        crate::proof_kernel::aver_generated::kernel::rules::val(AverStr::from("v")),
        crate::proof_kernel::aver_generated::kernel::rules::key(AverStr::from("k2")),
    ]);
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "map.get.empty" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::key(AverStr::from("k")),
                ]),
                crate::proof_kernel::aver_generated::kernel::rules::mb(
                    AverStr::from("get"),
                    &aver_rt::AverList::from_vec(vec![
                        empty,
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")),
                    ]),
                ),
                crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                        AverStr::from("Option.None"),
                        aver_rt::AverList::empty(),
                    ),
                ),
            )
        } else {
            if &*__dispatch_subject == "map.get.set_same" {
                crate::proof_kernel::aver_generated::kernel::rules::plain(
                    &mkv,
                    crate::proof_kernel::aver_generated::kernel::rules::mb(
                        AverStr::from("get"),
                        &aver_rt::AverList::from_vec(vec![
                            set,
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "k",
                            )),
                        ]),
                    ),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                            AverStr::from("Option.Some"),
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("v"),
                                ),
                            ]),
                        ),
                    ),
                )
            } else {
                if &*__dispatch_subject == "map.get.set_other" {
                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                        binders: mkv2,
                        premises: other,
                        concl: crate::proof_kernel::aver_generated::kernel::rules::eq(
                            crate::proof_kernel::aver_generated::kernel::rules::mb(
                                AverStr::from("get"),
                                &aver_rt::AverList::from_vec(vec![
                                    set,
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("k2"),
                                    ),
                                ]),
                            ),
                            crate::proof_kernel::aver_generated::kernel::rules::mb(
                                AverStr::from("get"),
                                &aver_rt::AverList::from_vec(vec![
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("m"),
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("k2"),
                                    ),
                                ]),
                            ),
                        ),
                    })
                } else {
                    if &*__dispatch_subject == "map.has.empty" {
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::key(
                                    AverStr::from("k"),
                                ),
                            ]),
                            crate::proof_kernel::aver_generated::kernel::rules::mb(
                                AverStr::from("has"),
                                &aver_rt::AverList::from_vec(vec![
                                    empty,
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("k"),
                                    ),
                                ]),
                            ),
                            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                    false,
                                ),
                            ),
                        )
                    } else {
                        if &*__dispatch_subject == "map.has.set_same" {
                            crate::proof_kernel::aver_generated::kernel::rules::plain(
                                &mkv,
                                crate::proof_kernel::aver_generated::kernel::rules::mb(
                                    AverStr::from("has"),
                                    &aver_rt::AverList::from_vec(vec![
                                        set,
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("k"),
                                        ),
                                    ]),
                                ),
                                crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                        true,
                                    ),
                                ),
                            )
                        } else {
                            if &*__dispatch_subject == "map.has.set_other" {
                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: mkv2, premises: other, concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("has"), &aver_rt::AverList::from_vec(vec![set, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k2"))])), crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("has"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k2"))]))) })
                            } else {
                                if &*__dispatch_subject == "map.len.empty" {
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::empty(), crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![empty])), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))
                                } else {
                                    if &*__dispatch_subject == "map.len.set_present" {
                                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: mkv, premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("has"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))])), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![set])), crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m"))]))) })
                                    } else {
                                        if &*__dispatch_subject == "map.len.set_absent" {
                                            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: mkv, premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("has"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))])), false)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![set])), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("+"), crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m"))])), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(1)))))) })
                                        } else {
                                            crate::proof_kernel::aver_generated::kernel::rules::vectorSchema(id)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// A Vector builtin applied to its arguments.
pub fn vb(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("Vector."));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                __b
            }),
            args.clone(),
        ),
    )
}

/// 0 <= at and at < end, as a premise.
#[inline(always)]
pub fn inRange(
    mut at @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut end @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Eqn {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::rules::is(
        crate::proof_kernel::aver_generated::kernel::rules::conn(
            AverStr::from("Bool.and"),
            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    AverStr::from("<="),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                    at.clone(),
                ),
            ),
            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    AverStr::from("<"),
                    at,
                    end,
                ),
            ),
        ),
        true,
    )
}

/// A vector as the list it holds; reads out of range; a read and the length after a write; a literal-size Vector.new.
pub fn vectorSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let len @ _ = crate::proof_kernel::aver_generated::kernel::rules::vb(
        AverStr::from("len"),
        &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(
            AverStr::from("v"),
        )]),
    );
    let written @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            AverStr::from("Option.withDefault"),
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::rules::vb(
                    AverStr::from("set"),
                    &aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")),
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")),
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")),
                    ]),
                ),
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")),
            ]),
        ),
    );
    let made @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            AverStr::from("__vector_new"),
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")),
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")),
            ]),
        ),
    );
    let vi @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::vec(AverStr::from("v")),
        crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("i")),
    ]);
    let vix @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::vec(AverStr::from("v")),
        crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("i")),
        crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("x")),
    ]);
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "vector.to_list.of_list" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::lt(AverStr::from("l")),
                ]),
                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                        AverStr::from("List.fromVector"),
                        aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::vb(
                                AverStr::from("fromList"),
                                &aver_rt::AverList::from_vec(vec![
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("l"),
                                    ),
                                ]),
                            ),
                        ]),
                    ),
                ),
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("l")),
            )
        } else {
            if &*__dispatch_subject == "vector.of_list.to_list" {
                crate::proof_kernel::aver_generated::kernel::rules::plain(
                    &aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::vec(AverStr::from("v")),
                    ]),
                    crate::proof_kernel::aver_generated::kernel::rules::vb(
                        AverStr::from("fromList"),
                        &aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                                    AverStr::from("List.fromVector"),
                                    aver_rt::AverList::from_vec(vec![
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("v"),
                                        ),
                                    ]),
                                ),
                            ),
                        ]),
                    ),
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")),
                )
            } else {
                if &*__dispatch_subject == "vector.len.to_list" {
                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::vec(AverStr::from("v"))]), len, crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("List.len"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("List.fromVector"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v"))])))]))))
                } else {
                    if &*__dispatch_subject == "vector.get.negative" {
                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: vi, premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("<"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))])), crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(AverStr::from("Option.None"), aver_rt::AverList::empty()))) })
                    } else {
                        if &*__dispatch_subject == "vector.get.past_end" {
                            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: vi, premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from(">="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), len)), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))])), crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(AverStr::from("Option.None"), aver_rt::AverList::empty()))) })
                        } else {
                            if &*__dispatch_subject == "vector.set.out_of_range" {
                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: vix, premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.or"), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("<"), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from(">="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), len))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("set"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x"))])), crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(AverStr::from("Option.None"), aver_rt::AverList::empty()))) })
                            } else {
                                if &*__dispatch_subject == "vector.get.set_same" {
                                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: vix, premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::inRange(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), len)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![written, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))])), crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(AverStr::from("Option.Some"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x"))])))) })
                                } else {
                                    if &*__dispatch_subject == "vector.get.set_other" {
                                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::vec(AverStr::from("v")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("i")), crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("x")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("j"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::inRange(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), len), crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("!="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("j")))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![written, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("j"))])), crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("j"))]))) })
                                    } else {
                                        if &*__dispatch_subject == "vector.len.set" {
                                            crate::proof_kernel::aver_generated::kernel::rules::plain(&vix, crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![written])), len)
                                        } else {
                                            if &*__dispatch_subject == "vector.len.new" {
                                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("n")), crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("x"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![made])), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))) })
                                            } else {
                                                if &*__dispatch_subject == "vector.get.new" {
                                                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("n")), crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("x")), crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("i"))]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::inRange(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")))]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![made, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))])), crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(AverStr::from("Option.Some"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x"))])))) })
                                                } else {
                                                    crate::proof_kernel::aver_generated::kernel::rules::pickSchema(id)
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// A rule whose value a comparison picks: l is r where the test has this value.
pub fn picked(
    binders @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    mut test @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    holds @ _: bool,
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
        binders: binders.clone(),
        premises: aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::rules::is(test, holds),
        ]),
        concl: crate::proof_kernel::aver_generated::kernel::rules::eq(l, r),
    })
}

/// Int.min and Int.max by a <= b, Int.abs by a < 0; List.contains on [], on an element in front that the value looked for is (found) and past one it is not, by y == x as the builtin compares. Both read == only at a type whose == is equality, where x == x is true of every value.
#[inline(always)]
pub fn pickSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let le @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
            AverStr::from("<="),
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
        ),
    );
    let neg @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
            AverStr::from("<"),
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                    aver_rt::AverInt::from_i64(0),
                ),
            ),
        ),
    );
    let min @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            AverStr::from("Int.min"),
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
            ]),
        ),
    );
    let max @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            AverStr::from("Int.max"),
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
            ]),
        ),
    );
    let abs @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            AverStr::from("Int.abs"),
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
            ]),
        ),
    );
    let found @ _ = crate::proof_kernel::aver_generated::kernel::rules::lb(
        AverStr::from("contains"),
        &aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("y")),
        ]),
    );
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "int.min.le" {
            crate::proof_kernel::aver_generated::kernel::rules::picked(
                &aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")),
                    crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("b")),
                ]),
                le,
                true,
                min,
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
            )
        } else {
            if &*__dispatch_subject == "int.min.gt" {
                crate::proof_kernel::aver_generated::kernel::rules::picked(
                    &aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a")),
                        crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("b")),
                    ]),
                    le,
                    false,
                    min,
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                )
            } else {
                if &*__dispatch_subject == "int.max.le" {
                    crate::proof_kernel::aver_generated::kernel::rules::picked(
                        &aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from(
                                "a",
                            )),
                            crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from(
                                "b",
                            )),
                        ]),
                        le,
                        true,
                        max,
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                    )
                } else {
                    if &*__dispatch_subject == "int.max.gt" {
                        crate::proof_kernel::aver_generated::kernel::rules::picked(
                            &aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    AverStr::from("a"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    AverStr::from("b"),
                                ),
                            ]),
                            le,
                            false,
                            max,
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "a",
                            )),
                        )
                    } else {
                        if &*__dispatch_subject == "int.abs.neg" {
                            crate::proof_kernel::aver_generated::kernel::rules::picked(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(AverStr::from("a"))]), neg, true, abs, crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("-"), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))))
                        } else {
                            if &*__dispatch_subject == "int.abs.nonneg" {
                                crate::proof_kernel::aver_generated::kernel::rules::picked(
                                    &aver_rt::AverList::from_vec(vec![
                                        crate::proof_kernel::aver_generated::kernel::rules::i(
                                            AverStr::from("a"),
                                        ),
                                    ]),
                                    neg,
                                    false,
                                    abs,
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("a"),
                                    ),
                                )
                            } else {
                                if &*__dispatch_subject == "list.contains.nil" {
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("y"))]), crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("contains"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TList(crate::proof_kernel::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty())), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("y"))])), crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false)))
                                } else {
                                    if &*__dispatch_subject == "list.contains.cons_found" {
                                        crate::proof_kernel::aver_generated::kernel::rules::picked(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::same(AverStr::from("x")), crate::proof_kernel::aver_generated::kernel::rules::lt(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::same(AverStr::from("y"))]), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("=="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("y")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")))), true, crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("contains"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::cons(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a"))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("y"))])), crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true)))
                                    } else {
                                        if &*__dispatch_subject == "list.contains.cons_other" {
                                            crate::proof_kernel::aver_generated::kernel::rules::picked(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::same(AverStr::from("x")), crate::proof_kernel::aver_generated::kernel::rules::lt(AverStr::from("a")), crate::proof_kernel::aver_generated::kernel::rules::same(AverStr::from("y"))]), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("=="), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("y")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")))), false, crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("contains"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::cons(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a"))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("y"))])), found)
                                        } else {
                                            crate::proof_kernel::aver_generated::kernel::rules::defaultSchema(id)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Option.withDefault and Result.withDefault on each constructor: the value a Some or an Ok holds, the default d for None or an Err.
#[inline(always)]
pub fn defaultSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let err @ _ = crate::proof_kernel::aver_generated::kernel::term::typed(
        AverStr::from("e"),
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
            crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(AverStr::from("e"), false),
        ),
    );
    {
        let __dispatch_subject = id;
        if &*__dispatch_subject == "option.with_default.some" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("x")),
                    crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("d")),
                ]),
                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                        AverStr::from("Option.withDefault"),
                        aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                                    AverStr::from("Option.Some"),
                                    aver_rt::AverList::from_vec(vec![
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("x"),
                                        ),
                                    ]),
                                ),
                            ),
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "d",
                            )),
                        ]),
                    ),
                ),
                crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")),
            )
        } else {
            if &*__dispatch_subject == "option.with_default.none" {
                crate::proof_kernel::aver_generated::kernel::rules::plain(
                    &aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("d")),
                    ]),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            AverStr::from("Option.withDefault"),
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                                        AverStr::from("Option.None"),
                                        aver_rt::AverList::empty(),
                                    ),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("d"),
                                ),
                            ]),
                        ),
                    ),
                    crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("d")),
                )
            } else {
                if &*__dispatch_subject == "result.with_default.ok" {
                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("x")), crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("d"))]), crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("Result.withDefault"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(AverStr::from("Result.Ok"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x"))]))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("d"))]))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")))
                } else {
                    if &*__dispatch_subject == "result.with_default.err" {
                        crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![err, crate::proof_kernel::aver_generated::kernel::rules::any(AverStr::from("d"))]), crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("Result.withDefault"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(AverStr::from("Result.Err"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("e"))]))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("d"))]))), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("d")))
                    } else {
                        None
                    }
                }
            }
        }
    }
}
