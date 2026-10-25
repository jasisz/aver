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
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt;
    crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg1)
}

/// A binder of type Bool.
#[inline(always)]
pub fn bo(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyBool;
    crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg1)
}

/// A binder of the rule's one type ?t.
#[inline(always)]
pub fn any(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
        crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
            ::aver_rt::str_lit!("t"),
            false,
        ),
    );
    crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg1)
}

/// A binder of the rule's one type ?t, whose == must be equality.
#[inline(always)]
pub fn same(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
        crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
            ::aver_rt::str_lit!("t"),
            true,
        ),
    );
    crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg1)
}

/// A binder of type List<?t>.
#[inline(always)]
pub fn lt(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyList(
        crate::proof_kernel::aver_generated::kernel::term::Ty__TyList(
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
                crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
                    ::aver_rt::str_lit!("t"),
                    false,
                ),
            ),
        ),
    );
    crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg1)
}

/// A binder of type Map<?k, ?v>, the key with an equality.
#[inline(always)]
pub fn kv(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyMap(
        crate::proof_kernel::aver_generated::kernel::term::Ty__TyMap(
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
                crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
                    ::aver_rt::str_lit!("k"),
                    true,
                ),
            ),
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
                crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
                    ::aver_rt::str_lit!("v"),
                    false,
                ),
            ),
        ),
    );
    crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg1)
}

/// A binder of the map's key type ?k.
#[inline(always)]
pub fn key(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
        crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
            ::aver_rt::str_lit!("k"),
            true,
        ),
    );
    crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg1)
}

/// A binder of the map's value type ?v.
#[inline(always)]
pub fn val(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
        crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
            ::aver_rt::str_lit!("v"),
            false,
        ),
    );
    crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg1)
}

/// A binder of type Vector<?t>.
#[inline(always)]
pub fn vec(n @ _: AverStr) -> crate::proof_kernel::aver_generated::kernel::term::TypedVar {
    crate::proof_kernel::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyVector(
        crate::proof_kernel::aver_generated::kernel::term::Ty__TyVector(
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
                crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
                    ::aver_rt::str_lit!("t"),
                    false,
                ),
            ),
        ),
    );
    crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg1)
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
    let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBool(value),
    );
    crate::proof_kernel::aver_generated::kernel::rules::eq(t, __arg2)
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
            {
                let __arg3 @ _ = aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::rules::bo(::aver_rt::str_lit!(
                        "b"
                    )),
                    aver_rt::AverList::empty(),
                );
                {
                    let __arg4 @ _ = {
                        let __arg1 @ _ =
                            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                    true,
                                ),
                            );
                        {
                            let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                                ::aver_rt::str_lit!("b"),
                            );
                            crate::proof_kernel::aver_generated::kernel::rules::conn(
                                ::aver_rt::str_lit!("Bool.and"),
                                __arg1,
                                __arg2,
                            )
                        }
                    };
                    {
                        let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                            ::aver_rt::str_lit!("b"),
                        );
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &__arg3, __arg4, __arg5,
                        )
                    }
                }
            }
        } else {
            if &*__dispatch_subject == "bool.and.false_l" {
                {
                    let __arg8 @ _ = aver_rt::AverList::prepend_owned(
                        crate::proof_kernel::aver_generated::kernel::rules::bo(
                            ::aver_rt::str_lit!("b"),
                        ),
                        aver_rt::AverList::empty(),
                    );
                    {
                        let __arg9 @ _ = {
                            let __arg6 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                        false,
                                    ),
                                );
                            {
                                let __arg7 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("b"),
                                    );
                                crate::proof_kernel::aver_generated::kernel::rules::conn(
                                    ::aver_rt::str_lit!("Bool.and"),
                                    __arg6,
                                    __arg7,
                                )
                            }
                        };
                        {
                            let __arg10 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                        false,
                                    ),
                                );
                            crate::proof_kernel::aver_generated::kernel::rules::plain(
                                &__arg8, __arg9, __arg10,
                            )
                        }
                    }
                }
            } else {
                if &*__dispatch_subject == "bool.and.true_r" {
                    {
                        let __arg13 @ _ = aver_rt::AverList::prepend_owned(
                            crate::proof_kernel::aver_generated::kernel::rules::bo(
                                ::aver_rt::str_lit!("a"),
                            ),
                            aver_rt::AverList::empty(),
                        );
                        {
                            let __arg14 @ _ = {
                                let __arg11 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("a"),
                                    );
                                {
                                    let __arg12 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true));
                                    crate::proof_kernel::aver_generated::kernel::rules::conn(
                                        ::aver_rt::str_lit!("Bool.and"),
                                        __arg11,
                                        __arg12,
                                    )
                                }
                            };
                            {
                                let __arg15 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("a"),
                                    );
                                crate::proof_kernel::aver_generated::kernel::rules::plain(
                                    &__arg13, __arg14, __arg15,
                                )
                            }
                        }
                    }
                } else {
                    if &*__dispatch_subject == "bool.and.false_r" {
                        {
                            let __arg18 @ _ = aver_rt::AverList::prepend_owned(
                                crate::proof_kernel::aver_generated::kernel::rules::bo(
                                    ::aver_rt::str_lit!("a"),
                                ),
                                aver_rt::AverList::empty(),
                            );
                            {
                                let __arg19 @ _ = {
                                    let __arg16 @ _ =
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("a"),
                                        );
                                    {
                                        let __arg17 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false));
                                        crate::proof_kernel::aver_generated::kernel::rules::conn(
                                            ::aver_rt::str_lit!("Bool.and"),
                                            __arg16,
                                            __arg17,
                                        )
                                    }
                                };
                                {
                                    let __arg20 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false));
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(
                                        &__arg18, __arg19, __arg20,
                                    )
                                }
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "bool.or.true_l" {
                            {
                                let __arg23 @ _ = aver_rt::AverList::prepend_owned(
                                    crate::proof_kernel::aver_generated::kernel::rules::bo(
                                        ::aver_rt::str_lit!("b"),
                                    ),
                                    aver_rt::AverList::empty(),
                                );
                                {
                                    let __arg24 @ _ = {
                                        let __arg21 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true));
                                        {
                                            let __arg22 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b"));
                                            crate::proof_kernel::aver_generated::kernel::rules::conn(
                                                ::aver_rt::str_lit!("Bool.or"),
                                                __arg21,
                                                __arg22,
                                            )
                                        }
                                    };
                                    {
                                        let __arg25 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true));
                                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                                            &__arg23, __arg24, __arg25,
                                        )
                                    }
                                }
                            }
                        } else {
                            if &*__dispatch_subject == "bool.or.false_l" {
                                {
                                    let __arg28 @ _ = aver_rt::AverList::prepend_owned(
                                        crate::proof_kernel::aver_generated::kernel::rules::bo(
                                            ::aver_rt::str_lit!("b"),
                                        ),
                                        aver_rt::AverList::empty(),
                                    );
                                    {
                                        let __arg29 @ _ = {
                                            let __arg26 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false));
                                            {
                                                let __arg27 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b"));
                                                crate::proof_kernel::aver_generated::kernel::rules::conn(::aver_rt::str_lit!("Bool.or"), __arg26, __arg27)
                                            }
                                        };
                                        {
                                            let __arg30 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b"));
                                            crate::proof_kernel::aver_generated::kernel::rules::plain(&__arg28, __arg29, __arg30)
                                        }
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "bool.or.true_r" {
                                    {
                                        let __arg33 @ _ = aver_rt::AverList::prepend_owned(
                                            crate::proof_kernel::aver_generated::kernel::rules::bo(
                                                ::aver_rt::str_lit!("a"),
                                            ),
                                            aver_rt::AverList::empty(),
                                        );
                                        {
                                            let __arg34 @ _ = {
                                                let __arg31 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"));
                                                {
                                                    let __arg32 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true));
                                                    crate::proof_kernel::aver_generated::kernel::rules::conn(::aver_rt::str_lit!("Bool.or"), __arg31, __arg32)
                                                }
                                            };
                                            {
                                                let __arg35 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true));
                                                crate::proof_kernel::aver_generated::kernel::rules::plain(&__arg33, __arg34, __arg35)
                                            }
                                        }
                                    }
                                } else {
                                    if &*__dispatch_subject == "bool.or.false_r" {
                                        {
                                            let __arg38 @ _ = aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::bo(::aver_rt::str_lit!("a")), aver_rt::AverList::empty());
                                            {
                                                let __arg39 @ _ = {
                                                    let __arg36 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"));
                                                    {
                                                        let __arg37 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false));
                                                        crate::proof_kernel::aver_generated::kernel::rules::conn(::aver_rt::str_lit!("Bool.or"), __arg36, __arg37)
                                                    }
                                                };
                                                {
                                                    let __arg40 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"));
                                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&__arg38, __arg39, __arg40)
                                                }
                                            }
                                        }
                                    } else {
                                        if &*__dispatch_subject == "bool.and.elim_l" {
                                            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::bo(::aver_rt::str_lit!("a")), crate::proof_kernel::aver_generated::kernel::rules::bo(::aver_rt::str_lit!("b"))]), premises: aver_rt::AverList::prepend_owned({ let __arg43 @ _ = { let __arg41 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")); { let __arg42 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b")); crate::proof_kernel::aver_generated::kernel::rules::conn(::aver_rt::str_lit!("Bool.and"), __arg41, __arg42) } }; crate::proof_kernel::aver_generated::kernel::rules::is(__arg43, true) }, aver_rt::AverList::empty()), concl: { let __arg44 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")); crate::proof_kernel::aver_generated::kernel::rules::is(__arg44, true) } })
                                        } else {
                                            if &*__dispatch_subject == "bool.and.elim_r" {
                                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::bo(::aver_rt::str_lit!("a")), crate::proof_kernel::aver_generated::kernel::rules::bo(::aver_rt::str_lit!("b"))]), premises: aver_rt::AverList::prepend_owned({ let __arg47 @ _ = { let __arg45 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")); { let __arg46 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b")); crate::proof_kernel::aver_generated::kernel::rules::conn(::aver_rt::str_lit!("Bool.and"), __arg45, __arg46) } }; crate::proof_kernel::aver_generated::kernel::rules::is(__arg47, true) }, aver_rt::AverList::empty()), concl: { let __arg48 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b")); crate::proof_kernel::aver_generated::kernel::rules::is(__arg48, true) } })
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
        premises: aver_rt::AverList::prepend_owned(
            {
                let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                        p,
                        crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!(
                            "a"
                        )),
                        crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!(
                            "b"
                        )),
                    ),
                );
                crate::proof_kernel::aver_generated::kernel::rules::is(__arg5, pv)
            },
            aver_rt::AverList::empty(),
        ),
        concl: {
            let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    q,
                    crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")),
                    crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b")),
                ),
            );
            crate::proof_kernel::aver_generated::kernel::rules::is(__arg6, qv)
        },
    })
}

/// != on Int as the negation of ==, an Int equality decided by the two orders, == as equality, as reflexive and as symmetric where the type's == is equality, and the product of two nonnegative Ints. A comparison decided by its complement (a > b false gives a <= b true) is a linear step, not a rule.
pub fn comparisonSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let ints @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("a")),
        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("b")),
    ]);
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "int.ne.def" {
            {
                let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                        ::aver_rt::str_lit!("!="),
                        crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!(
                            "a"
                        )),
                        crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!(
                            "b"
                        )),
                    ),
                );
                {
                    let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            ::aver_rt::str_lit!("Bool.not"),
                            aver_rt::AverList::prepend_owned(
                                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                        ::aver_rt::str_lit!("=="),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("a"),
                                        ),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("b"),
                                        ),
                                    ),
                                ),
                                aver_rt::AverList::empty(),
                            ),
                        ),
                    );
                    crate::proof_kernel::aver_generated::kernel::rules::plain(&ints, __arg2, __arg3)
                }
            }
        } else {
            if &*__dispatch_subject == "int.eq.of_le_ge" {
                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                    binders: ints,
                    premises: aver_rt::AverList::from_vec(vec![
                        {
                            let __arg4 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                        ::aver_rt::str_lit!("<="),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("a"),
                                        ),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("b"),
                                        ),
                                    ),
                                );
                            crate::proof_kernel::aver_generated::kernel::rules::is(__arg4, true)
                        },
                        {
                            let __arg5 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                        ::aver_rt::str_lit!(">="),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("a"),
                                        ),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("b"),
                                        ),
                                    ),
                                );
                            crate::proof_kernel::aver_generated::kernel::rules::is(__arg5, true)
                        },
                    ]),
                    concl: {
                        let __arg6 @ _ =
                            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                    ::aver_rt::str_lit!("=="),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("a"),
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("b"),
                                    ),
                                ),
                            );
                        crate::proof_kernel::aver_generated::kernel::rules::is(__arg6, true)
                    },
                })
            } else {
                if &*__dispatch_subject == "int.mul_nonneg" {
                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                        binders: ints,
                        premises: aver_rt::AverList::from_vec(vec![
                            {
                                let __arg7 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"))));
                                crate::proof_kernel::aver_generated::kernel::rules::is(__arg7, true)
                            },
                            {
                                let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b"))));
                                crate::proof_kernel::aver_generated::kernel::rules::is(__arg8, true)
                            },
                        ]),
                        concl: {
                            let __arg9 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("*"), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b"))))));
                            crate::proof_kernel::aver_generated::kernel::rules::is(__arg9, true)
                        },
                    })
                } else {
                    if &*__dispatch_subject == "int.eq.false_of_lt" {
                        crate::proof_kernel::aver_generated::kernel::rules::complement(
                            ::aver_rt::str_lit!("<"),
                            true,
                            ::aver_rt::str_lit!("=="),
                            false,
                            &ints,
                        )
                    } else {
                        if &*__dispatch_subject == "int.eq.false_of_gt" {
                            crate::proof_kernel::aver_generated::kernel::rules::complement(
                                ::aver_rt::str_lit!(">"),
                                true,
                                ::aver_rt::str_lit!("=="),
                                false,
                                &ints,
                            )
                        } else {
                            if &*__dispatch_subject == "int.eq.of_beq" {
                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                                    binders: aver_rt::AverList::from_vec(vec![
                                        crate::proof_kernel::aver_generated::kernel::rules::same(
                                            ::aver_rt::str_lit!("a"),
                                        ),
                                        crate::proof_kernel::aver_generated::kernel::rules::same(
                                            ::aver_rt::str_lit!("b"),
                                        ),
                                    ]),
                                    premises: aver_rt::AverList::prepend_owned(
                                        {
                                            let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b"))));
                                            crate::proof_kernel::aver_generated::kernel::rules::is(
                                                __arg10, true,
                                            )
                                        },
                                        aver_rt::AverList::empty(),
                                    ),
                                    concl: {
                                        let __arg11 @ _ =
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("a"),
                                            );
                                        {
                                            let __arg12 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b"));
                                            crate::proof_kernel::aver_generated::kernel::rules::eq(
                                                __arg11, __arg12,
                                            )
                                        }
                                    },
                                })
                            } else {
                                if &*__dispatch_subject == "bool.beq.refl" {
                                    {
                                        let __arg13 @ _ = aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::same(::aver_rt::str_lit!("a")), aver_rt::AverList::empty());
                                        {
                                            let __arg14 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"))));
                                            {
                                                let __arg15 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true));
                                                crate::proof_kernel::aver_generated::kernel::rules::plain(&__arg13, __arg14, __arg15)
                                            }
                                        }
                                    }
                                } else {
                                    if &*__dispatch_subject == "bool.beq.symm" {
                                        {
                                            let __arg16 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::same(::aver_rt::str_lit!("a")), crate::proof_kernel::aver_generated::kernel::rules::same(::aver_rt::str_lit!("b"))]);
                                            {
                                                let __arg17 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b"))));
                                                {
                                                    let __arg18 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"))));
                                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&__arg16, __arg17, __arg18)
                                                }
                                            }
                                        }
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
            ::aver_rt::str_lit!("__int_div_euclid"),
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
            ::aver_rt::str_lit!("__int_mod_euclid"),
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
    let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
            ::aver_rt::str_lit!("<="),
            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                    aver_rt::AverInt::from_i64(0),
                ),
            ),
            t.clone(),
        ),
    );
    let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
            ::aver_rt::str_lit!("<"),
            t,
            bound,
        ),
    );
    crate::proof_kernel::aver_generated::kernel::rules::conn(
        ::aver_rt::str_lit!("Bool.and"),
        __arg2,
        __arg3,
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
                    crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("a")),
                    crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("k")),
                ]),
                premises: aver_rt::AverList::prepend_owned(
                    {
                        let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0)))));
                        crate::proof_kernel::aver_generated::kernel::rules::is(__arg1, false)
                    },
                    aver_rt::AverList::empty(),
                ),
                concl: {
                    let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            ::aver_rt::str_lit!("Int.div"),
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    ::aver_rt::str_lit!("a"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    ::aver_rt::str_lit!("k"),
                                ),
                            ]),
                        ),
                    );
                    {
                        let __arg5 @ _ =
                            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                                    ::aver_rt::str_lit!("Result.Ok"),
                                    aver_rt::AverList::prepend_owned(
                                        {
                                            let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"));
                                            {
                                                let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k"));
                                                crate::proof_kernel::aver_generated::kernel::rules::divE(__arg2, __arg3)
                                            }
                                        },
                                        aver_rt::AverList::empty(),
                                    ),
                                ),
                            );
                        crate::proof_kernel::aver_generated::kernel::rules::eq(__arg4, __arg5)
                    }
                },
            })
        } else {
            if &*__dispatch_subject == "int.mod.ok" {
                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                    binders: aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!(
                            "a"
                        )),
                        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!(
                            "k"
                        )),
                    ]),
                    premises: aver_rt::AverList::prepend_owned(
                        {
                            let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0)))));
                            crate::proof_kernel::aver_generated::kernel::rules::is(__arg6, false)
                        },
                        aver_rt::AverList::empty(),
                    ),
                    concl: {
                        let __arg9 @ _ =
                            crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                                    ::aver_rt::str_lit!("Int.mod"),
                                    aver_rt::AverList::from_vec(vec![
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("a"),
                                        ),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("k"),
                                        ),
                                    ]),
                                ),
                            );
                        {
                            let __arg10 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                                        ::aver_rt::str_lit!("Result.Ok"),
                                        aver_rt::AverList::prepend_owned(
                                            {
                                                let __arg7 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"));
                                                {
                                                    let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k"));
                                                    crate::proof_kernel::aver_generated::kernel::rules::modE(__arg7, __arg8)
                                                }
                                            },
                                            aver_rt::AverList::empty(),
                                        ),
                                    ),
                                );
                            crate::proof_kernel::aver_generated::kernel::rules::eq(__arg9, __arg10)
                        }
                    },
                })
            } else {
                if &*__dispatch_subject == "int.div_mod_recompose" {
                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                        binders: aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::i(
                                ::aver_rt::str_lit!("a"),
                            ),
                            crate::proof_kernel::aver_generated::kernel::rules::i(
                                ::aver_rt::str_lit!("k"),
                            ),
                        ]),
                        premises: aver_rt::AverList::prepend_owned(
                            {
                                let __arg11 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!(">"), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0)))));
                                crate::proof_kernel::aver_generated::kernel::rules::is(
                                    __arg11, true,
                                )
                            },
                            aver_rt::AverList::empty(),
                        ),
                        concl: {
                            let __arg16 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("+"), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("*"), { let __arg12 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")); { let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")); crate::proof_kernel::aver_generated::kernel::rules::divE(__arg12, __arg13) } }, crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")))), { let __arg14 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")); { let __arg15 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")); crate::proof_kernel::aver_generated::kernel::rules::modE(__arg14, __arg15) } }));
                            {
                                let __arg17 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("a"),
                                    );
                                crate::proof_kernel::aver_generated::kernel::rules::eq(
                                    __arg16, __arg17,
                                )
                            }
                        },
                    })
                } else {
                    if &*__dispatch_subject == "int.mod_range" {
                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                            binders: aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    ::aver_rt::str_lit!("a"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    ::aver_rt::str_lit!("k"),
                                ),
                            ]),
                            premises: aver_rt::AverList::prepend_owned(
                                {
                                    let __arg18 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!(">"), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0)))));
                                    crate::proof_kernel::aver_generated::kernel::rules::is(
                                        __arg18, true,
                                    )
                                },
                                aver_rt::AverList::empty(),
                            ),
                            concl: {
                                let __arg23 @ _ = {
                                    let __arg21 @ _ = {
                                        let __arg19 @ _ =
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("a"),
                                            );
                                        {
                                            let __arg20 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k"));
                                            crate::proof_kernel::aver_generated::kernel::rules::modE(
                                                __arg19, __arg20,
                                            )
                                        }
                                    };
                                    {
                                        let __arg22 @ _ =
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("k"),
                                            );
                                        crate::proof_kernel::aver_generated::kernel::rules::range(
                                            __arg21, __arg22,
                                        )
                                    }
                                };
                                crate::proof_kernel::aver_generated::kernel::rules::is(
                                    __arg23, true,
                                )
                            },
                        })
                    } else {
                        if &*__dispatch_subject == "int.div_range" {
                            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                                binders: aver_rt::AverList::from_vec(vec![
                                    crate::proof_kernel::aver_generated::kernel::rules::i(
                                        ::aver_rt::str_lit!("a"),
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::rules::i(
                                        ::aver_rt::str_lit!("k"),
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::rules::i(
                                        ::aver_rt::str_lit!("m"),
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::rules::i(
                                        ::aver_rt::str_lit!("n"),
                                    ),
                                ]),
                                premises: aver_rt::AverList::from_vec(vec![
                                    {
                                        let __arg26 @ _ = {
                                            let __arg24 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"));
                                            {
                                                let __arg25 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("m"));
                                                crate::proof_kernel::aver_generated::kernel::rules::range(__arg24, __arg25)
                                            }
                                        };
                                        crate::proof_kernel::aver_generated::kernel::rules::is(
                                            __arg26, true,
                                        )
                                    },
                                    {
                                        let __arg29 @ _ = {
                                            let __arg27 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!(">"), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0)))));
                                            {
                                                let __arg28 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("m")), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("*"), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k"))))));
                                                crate::proof_kernel::aver_generated::kernel::rules::conn(::aver_rt::str_lit!("Bool.and"), __arg27, __arg28)
                                            }
                                        };
                                        crate::proof_kernel::aver_generated::kernel::rules::is(
                                            __arg29, true,
                                        )
                                    },
                                ]),
                                concl: {
                                    let __arg34 @ _ = {
                                        let __arg32 @ _ = {
                                            let __arg30 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"));
                                            {
                                                let __arg31 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k"));
                                                crate::proof_kernel::aver_generated::kernel::rules::divE(__arg30, __arg31)
                                            }
                                        };
                                        {
                                            let __arg33 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n"));
                                            crate::proof_kernel::aver_generated::kernel::rules::range(__arg32, __arg33)
                                        }
                                    };
                                    crate::proof_kernel::aver_generated::kernel::rules::is(
                                        __arg34, true,
                                    )
                                },
                            })
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
            ::aver_rt::str_lit!("__bits_shl"),
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
    let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
            ::aver_rt::str_lit!("<="),
            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                    aver_rt::AverInt::from_i64(0),
                ),
            ),
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n")),
        ),
    );
    let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
            ::aver_rt::str_lit!("=="),
            k,
            {
                let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                        aver_rt::AverInt::from_i64(1),
                    ),
                );
                {
                    let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                        ::aver_rt::str_lit!("n"),
                    );
                    crate::proof_kernel::aver_generated::kernel::rules::shl(__arg1, __arg2)
                }
            },
        ),
    );
    crate::proof_kernel::aver_generated::kernel::rules::conn(
        ::aver_rt::str_lit!("Bool.and"),
        __arg3,
        __arg4,
    )
}

/// The bit-level view of Int, infinite two's complement: complement, a shift left, a low mask 2^n - 1 and a one-bit mask 2^n.
#[inline(always)]
pub fn bitsSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "bits.not" {
            {
                let __arg1 @ _ = aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("a")),
                    aver_rt::AverList::empty(),
                );
                {
                    let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            ::aver_rt::str_lit!("Bits.not"),
                            aver_rt::AverList::prepend_owned(
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    ::aver_rt::str_lit!("a"),
                                ),
                                aver_rt::AverList::empty(),
                            ),
                        ),
                    );
                    {
                        let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("-"), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("-"), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")))), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(1)))));
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &__arg1, __arg2, __arg3,
                        )
                    }
                }
            }
        } else {
            if &*__dispatch_subject == "bits.shl" {
                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                    binders: aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!(
                            "a"
                        )),
                        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!(
                            "n"
                        )),
                        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!(
                            "k"
                        )),
                    ]),
                    premises: aver_rt::AverList::prepend_owned(
                        {
                            let __arg5 @ _ = {
                                let __arg4 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("k"),
                                    );
                                crate::proof_kernel::aver_generated::kernel::rules::powerOfTwo(
                                    __arg4,
                                )
                            };
                            crate::proof_kernel::aver_generated::kernel::rules::is(__arg5, true)
                        },
                        aver_rt::AverList::empty(),
                    ),
                    concl: {
                        let __arg8 @ _ = {
                            let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                                ::aver_rt::str_lit!("a"),
                            );
                            {
                                let __arg7 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("n"),
                                    );
                                crate::proof_kernel::aver_generated::kernel::rules::shl(
                                    __arg6, __arg7,
                                )
                            }
                        };
                        {
                            let __arg9 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                        ::aver_rt::str_lit!("*"),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("a"),
                                        ),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("k"),
                                        ),
                                    ),
                                );
                            crate::proof_kernel::aver_generated::kernel::rules::eq(__arg8, __arg9)
                        }
                    },
                })
            } else {
                if &*__dispatch_subject == "bits.and_low" {
                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                        binders: aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::i(
                                ::aver_rt::str_lit!("a"),
                            ),
                            crate::proof_kernel::aver_generated::kernel::rules::i(
                                ::aver_rt::str_lit!("m"),
                            ),
                            crate::proof_kernel::aver_generated::kernel::rules::i(
                                ::aver_rt::str_lit!("k"),
                            ),
                            crate::proof_kernel::aver_generated::kernel::rules::i(
                                ::aver_rt::str_lit!("n"),
                            ),
                        ]),
                        premises: aver_rt::AverList::prepend_owned(
                            {
                                let __arg16 @ _ = {
                                    let __arg14 @ _ = {
                                        let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n"))));
                                        {
                                            let __arg11 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("+"), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("m")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(1))))), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k"))));
                                            crate::proof_kernel::aver_generated::kernel::rules::conn(
                                                ::aver_rt::str_lit!("Bool.and"),
                                                __arg10,
                                                __arg11,
                                            )
                                        }
                                    };
                                    {
                                        let __arg15 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")), { let __arg12 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(1))); { let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n")); crate::proof_kernel::aver_generated::kernel::rules::shl(__arg12, __arg13) } }));
                                        crate::proof_kernel::aver_generated::kernel::rules::conn(
                                            ::aver_rt::str_lit!("Bool.and"),
                                            __arg14,
                                            __arg15,
                                        )
                                    }
                                };
                                crate::proof_kernel::aver_generated::kernel::rules::is(
                                    __arg16, true,
                                )
                            },
                            aver_rt::AverList::empty(),
                        ),
                        concl: {
                            let __arg19 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                                        ::aver_rt::str_lit!("Bits.and"),
                                        aver_rt::AverList::from_vec(vec![
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("a"),
                                            ),
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("m"),
                                            ),
                                        ]),
                                    ),
                                );
                            {
                                let __arg20 @ _ = {
                                    let __arg17 @ _ =
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("a"),
                                        );
                                    {
                                        let __arg18 @ _ =
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("k"),
                                            );
                                        crate::proof_kernel::aver_generated::kernel::rules::modE(
                                            __arg17, __arg18,
                                        )
                                    }
                                };
                                crate::proof_kernel::aver_generated::kernel::rules::eq(
                                    __arg19, __arg20,
                                )
                            }
                        },
                    })
                } else {
                    if &*__dispatch_subject == "bits.and_bit" {
                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                            binders: aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    ::aver_rt::str_lit!("a"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    ::aver_rt::str_lit!("k"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    ::aver_rt::str_lit!("n"),
                                ),
                            ]),
                            premises: aver_rt::AverList::prepend_owned(
                                {
                                    let __arg22 @ _ = {
                                        let __arg21 @ _ =
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("k"),
                                            );
                                        crate::proof_kernel::aver_generated::kernel::rules::powerOfTwo(__arg21)
                                    };
                                    crate::proof_kernel::aver_generated::kernel::rules::is(
                                        __arg22, true,
                                    )
                                },
                                aver_rt::AverList::empty(),
                            ),
                            concl: {
                                let __arg27 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("Bits.and"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k"))])));
                                {
                                    let __arg28 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("*"), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")), { let __arg25 @ _ = { let __arg23 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")); { let __arg24 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")); crate::proof_kernel::aver_generated::kernel::rules::divE(__arg23, __arg24) } }; { let __arg26 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(2))); crate::proof_kernel::aver_generated::kernel::rules::modE(__arg25, __arg26) } }));
                                    crate::proof_kernel::aver_generated::kernel::rules::eq(
                                        __arg27, __arg28,
                                    )
                                }
                            },
                        })
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
            ::aver_rt::str_lit!("List.prepend"),
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
                    __b.push_str(&::aver_rt::str_lit!("List."));
                    __b
                };
                __b.push_display(&(name));
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
        {
            let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    ::aver_rt::str_lit!(">"),
                    crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n")),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                ),
            );
            crate::proof_kernel::aver_generated::kernel::rules::is(__arg5, true)
        }
    } else {
        {
            let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    ::aver_rt::str_lit!("<="),
                    crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n")),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                ),
            );
            crate::proof_kernel::aver_generated::kernel::rules::is(__arg6, true)
        }
    };
    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
        binders: binders.clone(),
        premises: aver_rt::AverList::prepend_owned(premise, aver_rt::AverList::empty()),
        concl: crate::proof_kernel::aver_generated::kernel::rules::eq(l, r),
    })
}

/// Each list builtin on an element in front of a list, and on the empty list where it has another argument; List.len([]) and List.reverse([]) are closed, so compute shows them.
pub fn listSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let xa @ _ = {
        let __arg5 @ _ =
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x"));
        {
            let __arg6 @ _ =
                crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"));
            crate::proof_kernel::aver_generated::kernel::rules::cons(__arg5, __arg6)
        }
    };
    let nil @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TList(
        crate::proof_kernel::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty()),
    );
    let less @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
            ::aver_rt::str_lit!("-"),
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n")),
            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                    aver_rt::AverInt::from_i64(1),
                ),
            ),
        ),
    );
    let xan @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::any(::aver_rt::str_lit!("x")),
        crate::proof_kernel::aver_generated::kernel::rules::lt(::aver_rt::str_lit!("a")),
        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("n")),
    ]);
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "list.concat.nil" {
            {
                let __arg8 @ _ = aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::rules::lt(::aver_rt::str_lit!(
                        "b"
                    )),
                    aver_rt::AverList::empty(),
                );
                {
                    let __arg9 @ _ = {
                        let __arg7 @ _ = aver_rt::AverList::from_vec(vec![
                            nil,
                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                ::aver_rt::str_lit!("b"),
                            ),
                        ]);
                        crate::proof_kernel::aver_generated::kernel::rules::lb(
                            ::aver_rt::str_lit!("concat"),
                            &__arg7,
                        )
                    };
                    {
                        let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                            ::aver_rt::str_lit!("b"),
                        );
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &__arg8, __arg9, __arg10,
                        )
                    }
                }
            }
        } else {
            if &*__dispatch_subject == "list.concat.cons" {
                {
                    let __arg15 @ _ = aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::any(
                            ::aver_rt::str_lit!("x"),
                        ),
                        crate::proof_kernel::aver_generated::kernel::rules::lt(
                            ::aver_rt::str_lit!("a"),
                        ),
                        crate::proof_kernel::aver_generated::kernel::rules::lt(
                            ::aver_rt::str_lit!("b"),
                        ),
                    ]);
                    {
                        let __arg16 @ _ = {
                            let __arg11 @ _ = aver_rt::AverList::from_vec(vec![
                                xa,
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    ::aver_rt::str_lit!("b"),
                                ),
                            ]);
                            crate::proof_kernel::aver_generated::kernel::rules::lb(
                                ::aver_rt::str_lit!("concat"),
                                &__arg11,
                            )
                        };
                        {
                            let __arg17 @ _ = {
                                let __arg13 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("x"),
                                    );
                                {
                                    let __arg14 @ _ = {
                                        let __arg12 @ _ = aver_rt::AverList::from_vec(vec![
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("a"),
                                            ),
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("b"),
                                            ),
                                        ]);
                                        crate::proof_kernel::aver_generated::kernel::rules::lb(
                                            ::aver_rt::str_lit!("concat"),
                                            &__arg12,
                                        )
                                    };
                                    crate::proof_kernel::aver_generated::kernel::rules::cons(
                                        __arg13, __arg14,
                                    )
                                }
                            };
                            crate::proof_kernel::aver_generated::kernel::rules::plain(
                                &__arg15, __arg16, __arg17,
                            )
                        }
                    }
                }
            } else {
                if &*__dispatch_subject == "list.len.cons" {
                    {
                        let __arg20 @ _ = aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::any(
                                ::aver_rt::str_lit!("x"),
                            ),
                            crate::proof_kernel::aver_generated::kernel::rules::lt(
                                ::aver_rt::str_lit!("a"),
                            ),
                        ]);
                        {
                            let __arg21 @ _ = {
                                let __arg18 @ _ = aver_rt::AverList::prepend_owned(
                                    xa,
                                    aver_rt::AverList::empty(),
                                );
                                crate::proof_kernel::aver_generated::kernel::rules::lb(
                                    ::aver_rt::str_lit!("len"),
                                    &__arg18,
                                )
                            };
                            {
                                let __arg22 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("+"), { let __arg19 @ _ = aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")), aver_rt::AverList::empty()); crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("len"), &__arg19) }, crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(1)))));
                                crate::proof_kernel::aver_generated::kernel::rules::plain(
                                    &__arg20, __arg21, __arg22,
                                )
                            }
                        }
                    }
                } else {
                    if &*__dispatch_subject == "list.take.nil" {
                        {
                            let __arg24 @ _ = aver_rt::AverList::prepend_owned(
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    ::aver_rt::str_lit!("n"),
                                ),
                                aver_rt::AverList::empty(),
                            );
                            {
                                let __arg25 @ _ = {
                                    let __arg23 @ _ = aver_rt::AverList::from_vec(vec![
                                        nil.clone(),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("n"),
                                        ),
                                    ]);
                                    crate::proof_kernel::aver_generated::kernel::rules::lb(
                                        ::aver_rt::str_lit!("take"),
                                        &__arg23,
                                    )
                                };
                                crate::proof_kernel::aver_generated::kernel::rules::plain(
                                    &__arg24, __arg25, nil,
                                )
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "list.take.cons_le" {
                            {
                                let __arg27 @ _ = {
                                    let __arg26 @ _ = aver_rt::AverList::from_vec(vec![
                                        xa,
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("n"),
                                        ),
                                    ]);
                                    crate::proof_kernel::aver_generated::kernel::rules::lb(
                                        ::aver_rt::str_lit!("take"),
                                        &__arg26,
                                    )
                                };
                                crate::proof_kernel::aver_generated::kernel::rules::counted(
                                    &xan, false, __arg27, nil,
                                )
                            }
                        } else {
                            if &*__dispatch_subject == "list.take.cons_gt" {
                                {
                                    let __arg32 @ _ = {
                                        let __arg28 @ _ = aver_rt::AverList::from_vec(vec![
                                            xa,
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("n"),
                                            ),
                                        ]);
                                        crate::proof_kernel::aver_generated::kernel::rules::lb(
                                            ::aver_rt::str_lit!("take"),
                                            &__arg28,
                                        )
                                    };
                                    {
                                        let __arg33 @ _ = {
                                            let __arg30 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x"));
                                            {
                                                let __arg31 @ _ = {
                                                    let __arg29 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")), less]);
                                                    crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("take"), &__arg29)
                                                };
                                                crate::proof_kernel::aver_generated::kernel::rules::cons(__arg30, __arg31)
                                            }
                                        };
                                        crate::proof_kernel::aver_generated::kernel::rules::counted(
                                            &xan, true, __arg32, __arg33,
                                        )
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "list.drop.nil" {
                                    {
                                        let __arg35 @ _ = aver_rt::AverList::prepend_owned(
                                            crate::proof_kernel::aver_generated::kernel::rules::i(
                                                ::aver_rt::str_lit!("n"),
                                            ),
                                            aver_rt::AverList::empty(),
                                        );
                                        {
                                            let __arg36 @ _ = {
                                                let __arg34 @ _ = aver_rt::AverList::from_vec(vec![nil.clone(), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n"))]);
                                                crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("drop"), &__arg34)
                                            };
                                            crate::proof_kernel::aver_generated::kernel::rules::plain(&__arg35, __arg36, nil)
                                        }
                                    }
                                } else {
                                    if &*__dispatch_subject == "list.drop.cons_le" {
                                        {
                                            let __arg38 @ _ = {
                                                let __arg37 @ _ = aver_rt::AverList::from_vec(vec![xa.clone(), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n"))]);
                                                crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("drop"), &__arg37)
                                            };
                                            crate::proof_kernel::aver_generated::kernel::rules::counted(&xan, false, __arg38, xa)
                                        }
                                    } else {
                                        if &*__dispatch_subject == "list.drop.cons_gt" {
                                            {
                                                let __arg41 @ _ = {
                                                    let __arg39 @ _ = aver_rt::AverList::from_vec(vec![xa, crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n"))]);
                                                    crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("drop"), &__arg39)
                                                };
                                                {
                                                    let __arg42 @ _ = {
                                                        let __arg40 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")), less]);
                                                        crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("drop"), &__arg40)
                                                    };
                                                    crate::proof_kernel::aver_generated::kernel::rules::counted(&xan, true, __arg41, __arg42)
                                                }
                                            }
                                        } else {
                                            if &*__dispatch_subject == "list.reverse.cons" {
                                                {
                                                    let __arg47 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::any(::aver_rt::str_lit!("x")), crate::proof_kernel::aver_generated::kernel::rules::lt(::aver_rt::str_lit!("a"))]);
                                                    {
                                                        let __arg48 @ _ = {
                                                            let __arg43 @ _ =
                                                                aver_rt::AverList::prepend_owned(
                                                                    xa,
                                                                    aver_rt::AverList::empty(),
                                                                );
                                                            crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("reverse"), &__arg43)
                                                        };
                                                        {
                                                            let __arg49 @ _ = {
                                                                let __arg46 @ _ =
                                                                    aver_rt::AverList::from_vec(
                                                                        vec![
                                                                            {
                                                                                let __arg44 @ _ = aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")), aver_rt::AverList::empty());
                                                                                crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("reverse"), &__arg44)
                                                                            },
                                                                            {
                                                                                let __arg45 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x"));
                                                                                crate::proof_kernel::aver_generated::kernel::rules::cons(__arg45, nil)
                                                                            },
                                                                        ],
                                                                    );
                                                                crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("concat"), &__arg46)
                                                            };
                                                            crate::proof_kernel::aver_generated::kernel::rules::plain(&__arg47, __arg48, __arg49)
                                                        }
                                                    }
                                                }
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
                    __b.push_str(&::aver_rt::str_lit!("Map."));
                    __b
                };
                __b.push_display(&(name));
                __b
            }),
            args.clone(),
        ),
    )
}

/// Each Map read on the empty map and on a set: at the key set, at another key, and the size by membership.
pub fn mapSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let set @ _ = {
        let __arg6 @ _ = aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("m")),
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")),
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("v")),
        ]);
        crate::proof_kernel::aver_generated::kernel::rules::mb(::aver_rt::str_lit!("set"), &__arg6)
    };
    let empty @ _ = {
        let __arg7 @ _ =
            <aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>>::empty();
        crate::proof_kernel::aver_generated::kernel::rules::mb(
            ::aver_rt::str_lit!("empty"),
            &__arg7,
        )
    };
    let other @ _ = aver_rt::AverList::prepend_owned(
        {
            let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    ::aver_rt::str_lit!("!="),
                    crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k")),
                    crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!(
                        "k2"
                    )),
                ),
            );
            crate::proof_kernel::aver_generated::kernel::rules::is(__arg8, true)
        },
        aver_rt::AverList::empty(),
    );
    let mkv @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::kv(::aver_rt::str_lit!("m")),
        crate::proof_kernel::aver_generated::kernel::rules::key(::aver_rt::str_lit!("k")),
        crate::proof_kernel::aver_generated::kernel::rules::val(::aver_rt::str_lit!("v")),
    ]);
    let mkv2 @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::kv(::aver_rt::str_lit!("m")),
        crate::proof_kernel::aver_generated::kernel::rules::key(::aver_rt::str_lit!("k")),
        crate::proof_kernel::aver_generated::kernel::rules::val(::aver_rt::str_lit!("v")),
        crate::proof_kernel::aver_generated::kernel::rules::key(::aver_rt::str_lit!("k2")),
    ]);
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "map.get.empty" {
            {
                let __arg10 @ _ = aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::rules::key(::aver_rt::str_lit!(
                        "k"
                    )),
                    aver_rt::AverList::empty(),
                );
                {
                    let __arg11 @ _ = {
                        let __arg9 @ _ = aver_rt::AverList::from_vec(vec![
                            empty,
                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                ::aver_rt::str_lit!("k"),
                            ),
                        ]);
                        crate::proof_kernel::aver_generated::kernel::rules::mb(
                            ::aver_rt::str_lit!("get"),
                            &__arg9,
                        )
                    };
                    {
                        let __arg12 @ _ =
                            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                                    ::aver_rt::str_lit!("Option.None"),
                                    aver_rt::AverList::empty(),
                                ),
                            );
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &__arg10, __arg11, __arg12,
                        )
                    }
                }
            }
        } else {
            if &*__dispatch_subject == "map.get.set_same" {
                {
                    let __arg14 @ _ = {
                        let __arg13 @ _ = aver_rt::AverList::from_vec(vec![
                            set,
                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                ::aver_rt::str_lit!("k"),
                            ),
                        ]);
                        crate::proof_kernel::aver_generated::kernel::rules::mb(
                            ::aver_rt::str_lit!("get"),
                            &__arg13,
                        )
                    };
                    {
                        let __arg15 @ _ =
                            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                                    ::aver_rt::str_lit!("Option.Some"),
                                    aver_rt::AverList::prepend_owned(
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("v"),
                                        ),
                                        aver_rt::AverList::empty(),
                                    ),
                                ),
                            );
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &mkv, __arg14, __arg15,
                        )
                    }
                }
            } else {
                if &*__dispatch_subject == "map.get.set_other" {
                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                        binders: mkv2,
                        premises: other,
                        concl: {
                            let __arg18 @ _ = {
                                let __arg16 @ _ = aver_rt::AverList::from_vec(vec![
                                    set,
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("k2"),
                                    ),
                                ]);
                                crate::proof_kernel::aver_generated::kernel::rules::mb(
                                    ::aver_rt::str_lit!("get"),
                                    &__arg16,
                                )
                            };
                            {
                                let __arg19 @ _ = {
                                    let __arg17 @ _ = aver_rt::AverList::from_vec(vec![
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("m"),
                                        ),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("k2"),
                                        ),
                                    ]);
                                    crate::proof_kernel::aver_generated::kernel::rules::mb(
                                        ::aver_rt::str_lit!("get"),
                                        &__arg17,
                                    )
                                };
                                crate::proof_kernel::aver_generated::kernel::rules::eq(
                                    __arg18, __arg19,
                                )
                            }
                        },
                    })
                } else {
                    if &*__dispatch_subject == "map.has.empty" {
                        {
                            let __arg21 @ _ = aver_rt::AverList::prepend_owned(
                                crate::proof_kernel::aver_generated::kernel::rules::key(
                                    ::aver_rt::str_lit!("k"),
                                ),
                                aver_rt::AverList::empty(),
                            );
                            {
                                let __arg22 @ _ = {
                                    let __arg20 @ _ = aver_rt::AverList::from_vec(vec![
                                        empty,
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("k"),
                                        ),
                                    ]);
                                    crate::proof_kernel::aver_generated::kernel::rules::mb(
                                        ::aver_rt::str_lit!("has"),
                                        &__arg20,
                                    )
                                };
                                {
                                    let __arg23 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false));
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(
                                        &__arg21, __arg22, __arg23,
                                    )
                                }
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "map.has.set_same" {
                            {
                                let __arg25 @ _ = {
                                    let __arg24 @ _ = aver_rt::AverList::from_vec(vec![
                                        set,
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("k"),
                                        ),
                                    ]);
                                    crate::proof_kernel::aver_generated::kernel::rules::mb(
                                        ::aver_rt::str_lit!("has"),
                                        &__arg24,
                                    )
                                };
                                {
                                    let __arg26 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true));
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(
                                        &mkv, __arg25, __arg26,
                                    )
                                }
                            }
                        } else {
                            if &*__dispatch_subject == "map.has.set_other" {
                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                                    binders: mkv2,
                                    premises: other,
                                    concl: {
                                        let __arg29 @ _ = {
                                            let __arg27 @ _ = aver_rt::AverList::from_vec(vec![set, crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k2"))]);
                                            crate::proof_kernel::aver_generated::kernel::rules::mb(
                                                ::aver_rt::str_lit!("has"),
                                                &__arg27,
                                            )
                                        };
                                        {
                                            let __arg30 @ _ = {
                                                let __arg28 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("m")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k2"))]);
                                                crate::proof_kernel::aver_generated::kernel::rules::mb(::aver_rt::str_lit!("has"), &__arg28)
                                            };
                                            crate::proof_kernel::aver_generated::kernel::rules::eq(
                                                __arg29, __arg30,
                                            )
                                        }
                                    },
                                })
                            } else {
                                if &*__dispatch_subject == "map.len.empty" {
                                    {
                                        let __arg32 @ _ = <aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>>::empty();
                                        {
                                            let __arg33 @ _ = {
                                                let __arg31 @ _ = aver_rt::AverList::prepend_owned(
                                                    empty,
                                                    aver_rt::AverList::empty(),
                                                );
                                                crate::proof_kernel::aver_generated::kernel::rules::mb(::aver_rt::str_lit!("len"), &__arg31)
                                            };
                                            {
                                                let __arg34 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0)));
                                                crate::proof_kernel::aver_generated::kernel::rules::plain(&__arg32, __arg33, __arg34)
                                            }
                                        }
                                    }
                                } else {
                                    if &*__dispatch_subject == "map.len.set_present" {
                                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: mkv, premises: aver_rt::AverList::prepend_owned({ let __arg36 @ _ = { let __arg35 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("m")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k"))]); crate::proof_kernel::aver_generated::kernel::rules::mb(::aver_rt::str_lit!("has"), &__arg35) }; crate::proof_kernel::aver_generated::kernel::rules::is(__arg36, true) }, aver_rt::AverList::empty()), concl: { let __arg39 @ _ = { let __arg37 @ _ = aver_rt::AverList::prepend_owned(set, aver_rt::AverList::empty()); crate::proof_kernel::aver_generated::kernel::rules::mb(::aver_rt::str_lit!("len"), &__arg37) }; { let __arg40 @ _ = { let __arg38 @ _ = aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("m")), aver_rt::AverList::empty()); crate::proof_kernel::aver_generated::kernel::rules::mb(::aver_rt::str_lit!("len"), &__arg38) }; crate::proof_kernel::aver_generated::kernel::rules::eq(__arg39, __arg40) } } })
                                    } else {
                                        if &*__dispatch_subject == "map.len.set_absent" {
                                            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: mkv, premises: aver_rt::AverList::prepend_owned({ let __arg42 @ _ = { let __arg41 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("m")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("k"))]); crate::proof_kernel::aver_generated::kernel::rules::mb(::aver_rt::str_lit!("has"), &__arg41) }; crate::proof_kernel::aver_generated::kernel::rules::is(__arg42, false) }, aver_rt::AverList::empty()), concl: { let __arg45 @ _ = { let __arg43 @ _ = aver_rt::AverList::prepend_owned(set, aver_rt::AverList::empty()); crate::proof_kernel::aver_generated::kernel::rules::mb(::aver_rt::str_lit!("len"), &__arg43) }; { let __arg46 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("+"), { let __arg44 @ _ = aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("m")), aver_rt::AverList::empty()); crate::proof_kernel::aver_generated::kernel::rules::mb(::aver_rt::str_lit!("len"), &__arg44) }, crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(1))))); crate::proof_kernel::aver_generated::kernel::rules::eq(__arg45, __arg46) } } })
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
                    __b.push_str(&::aver_rt::str_lit!("Vector."));
                    __b
                };
                __b.push_display(&(name));
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
    let __arg4 @ _ = {
        let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                ::aver_rt::str_lit!("<="),
                crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                        aver_rt::AverInt::from_i64(0),
                    ),
                ),
                at.clone(),
            ),
        );
        {
            let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    ::aver_rt::str_lit!("<"),
                    at,
                    end,
                ),
            );
            crate::proof_kernel::aver_generated::kernel::rules::conn(
                ::aver_rt::str_lit!("Bool.and"),
                __arg2,
                __arg3,
            )
        }
    };
    crate::proof_kernel::aver_generated::kernel::rules::is(__arg4, true)
}

/// A vector as the list it holds; reads out of range; a read and the length after a write; a literal-size Vector.new.
pub fn vectorSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let len @ _ = {
        let __arg6 @ _ = aver_rt::AverList::prepend_owned(
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("v")),
            aver_rt::AverList::empty(),
        );
        crate::proof_kernel::aver_generated::kernel::rules::vb(::aver_rt::str_lit!("len"), &__arg6)
    };
    let written @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            ::aver_rt::str_lit!("Option.withDefault"),
            aver_rt::AverList::from_vec(vec![
                {
                    let __arg7 @ _ = aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!(
                            "v"
                        )),
                        crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!(
                            "i"
                        )),
                        crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!(
                            "x"
                        )),
                    ]);
                    crate::proof_kernel::aver_generated::kernel::rules::vb(
                        ::aver_rt::str_lit!("set"),
                        &__arg7,
                    )
                },
                crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("v")),
            ]),
        ),
    );
    let made @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            ::aver_rt::str_lit!("__vector_new"),
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n")),
                crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x")),
            ]),
        ),
    );
    let vi @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::vec(::aver_rt::str_lit!("v")),
        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("i")),
    ]);
    let vix @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::rules::vec(::aver_rt::str_lit!("v")),
        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("i")),
        crate::proof_kernel::aver_generated::kernel::rules::any(::aver_rt::str_lit!("x")),
    ]);
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "vector.to_list.of_list" {
            {
                let __arg9 @ _ = aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::rules::lt(::aver_rt::str_lit!(
                        "l"
                    )),
                    aver_rt::AverList::empty(),
                );
                {
                    let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            ::aver_rt::str_lit!("List.fromVector"),
                            aver_rt::AverList::prepend_owned(
                                {
                                    let __arg8 @ _ = aver_rt::AverList::prepend_owned(
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("l"),
                                        ),
                                        aver_rt::AverList::empty(),
                                    );
                                    crate::proof_kernel::aver_generated::kernel::rules::vb(
                                        ::aver_rt::str_lit!("fromList"),
                                        &__arg8,
                                    )
                                },
                                aver_rt::AverList::empty(),
                            ),
                        ),
                    );
                    {
                        let __arg11 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                            ::aver_rt::str_lit!("l"),
                        );
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &__arg9, __arg10, __arg11,
                        )
                    }
                }
            }
        } else {
            if &*__dispatch_subject == "vector.of_list.to_list" {
                {
                    let __arg13 @ _ = aver_rt::AverList::prepend_owned(
                        crate::proof_kernel::aver_generated::kernel::rules::vec(
                            ::aver_rt::str_lit!("v"),
                        ),
                        aver_rt::AverList::empty(),
                    );
                    {
                        let __arg14 @ _ = {
                            let __arg12 @ _ = aver_rt::AverList::prepend_owned(
                                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                                        ::aver_rt::str_lit!("List.fromVector"),
                                        aver_rt::AverList::prepend_owned(
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("v"),
                                            ),
                                            aver_rt::AverList::empty(),
                                        ),
                                    ),
                                ),
                                aver_rt::AverList::empty(),
                            );
                            crate::proof_kernel::aver_generated::kernel::rules::vb(
                                ::aver_rt::str_lit!("fromList"),
                                &__arg12,
                            )
                        };
                        {
                            let __arg15 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                                ::aver_rt::str_lit!("v"),
                            );
                            crate::proof_kernel::aver_generated::kernel::rules::plain(
                                &__arg13, __arg14, __arg15,
                            )
                        }
                    }
                }
            } else {
                if &*__dispatch_subject == "vector.len.to_list" {
                    {
                        let __arg16 @ _ = aver_rt::AverList::prepend_owned(
                            crate::proof_kernel::aver_generated::kernel::rules::vec(
                                ::aver_rt::str_lit!("v"),
                            ),
                            aver_rt::AverList::empty(),
                        );
                        {
                            let __arg17 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.len"), aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.fromVector"), aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("v")), aver_rt::AverList::empty()))), aver_rt::AverList::empty())));
                            crate::proof_kernel::aver_generated::kernel::rules::plain(
                                &__arg16, len, __arg17,
                            )
                        }
                    }
                } else {
                    if &*__dispatch_subject == "vector.get.negative" {
                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                            binders: vi,
                            premises: aver_rt::AverList::prepend_owned(
                                {
                                    let __arg18 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("<"), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0)))));
                                    crate::proof_kernel::aver_generated::kernel::rules::is(
                                        __arg18, true,
                                    )
                                },
                                aver_rt::AverList::empty(),
                            ),
                            concl: {
                                let __arg20 @ _ = {
                                    let __arg19 @ _ = aver_rt::AverList::from_vec(vec![
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("v"),
                                        ),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("i"),
                                        ),
                                    ]);
                                    crate::proof_kernel::aver_generated::kernel::rules::vb(
                                        ::aver_rt::str_lit!("get"),
                                        &__arg19,
                                    )
                                };
                                {
                                    let __arg21 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(::aver_rt::str_lit!("Option.None"), aver_rt::AverList::empty()));
                                    crate::proof_kernel::aver_generated::kernel::rules::eq(
                                        __arg20, __arg21,
                                    )
                                }
                            },
                        })
                    } else {
                        if &*__dispatch_subject == "vector.get.past_end" {
                            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                                binders: vi,
                                premises: aver_rt::AverList::prepend_owned(
                                    {
                                        let __arg22 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!(">="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i")), len));
                                        crate::proof_kernel::aver_generated::kernel::rules::is(
                                            __arg22, true,
                                        )
                                    },
                                    aver_rt::AverList::empty(),
                                ),
                                concl: {
                                    let __arg24 @ _ = {
                                        let __arg23 @ _ = aver_rt::AverList::from_vec(vec![
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("v"),
                                            ),
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("i"),
                                            ),
                                        ]);
                                        crate::proof_kernel::aver_generated::kernel::rules::vb(
                                            ::aver_rt::str_lit!("get"),
                                            &__arg23,
                                        )
                                    };
                                    {
                                        let __arg25 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(::aver_rt::str_lit!("Option.None"), aver_rt::AverList::empty()));
                                        crate::proof_kernel::aver_generated::kernel::rules::eq(
                                            __arg24, __arg25,
                                        )
                                    }
                                },
                            })
                        } else {
                            if &*__dispatch_subject == "vector.set.out_of_range" {
                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                                    binders: vix,
                                    premises: aver_rt::AverList::prepend_owned(
                                        {
                                            let __arg28 @ _ = {
                                                let __arg26 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("<"), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i")), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0)))));
                                                {
                                                    let __arg27 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!(">="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i")), len));
                                                    crate::proof_kernel::aver_generated::kernel::rules::conn(::aver_rt::str_lit!("Bool.or"), __arg26, __arg27)
                                                }
                                            };
                                            crate::proof_kernel::aver_generated::kernel::rules::is(
                                                __arg28, true,
                                            )
                                        },
                                        aver_rt::AverList::empty(),
                                    ),
                                    concl: {
                                        let __arg30 @ _ = {
                                            let __arg29 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("v")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x"))]);
                                            crate::proof_kernel::aver_generated::kernel::rules::vb(
                                                ::aver_rt::str_lit!("set"),
                                                &__arg29,
                                            )
                                        };
                                        {
                                            let __arg31 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(::aver_rt::str_lit!("Option.None"), aver_rt::AverList::empty()));
                                            crate::proof_kernel::aver_generated::kernel::rules::eq(
                                                __arg30, __arg31,
                                            )
                                        }
                                    },
                                })
                            } else {
                                if &*__dispatch_subject == "vector.get.set_same" {
                                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: vix, premises: aver_rt::AverList::prepend_owned({ let __arg32 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i")); crate::proof_kernel::aver_generated::kernel::rules::inRange(__arg32, len) }, aver_rt::AverList::empty()), concl: { let __arg34 @ _ = { let __arg33 @ _ = aver_rt::AverList::from_vec(vec![written, crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i"))]); crate::proof_kernel::aver_generated::kernel::rules::vb(::aver_rt::str_lit!("get"), &__arg33) }; { let __arg35 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(::aver_rt::str_lit!("Option.Some"), aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x")), aver_rt::AverList::empty()))); crate::proof_kernel::aver_generated::kernel::rules::eq(__arg34, __arg35) } } })
                                } else {
                                    if &*__dispatch_subject == "vector.get.set_other" {
                                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::vec(::aver_rt::str_lit!("v")), crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("i")), crate::proof_kernel::aver_generated::kernel::rules::any(::aver_rt::str_lit!("x")), crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("j"))]), premises: aver_rt::AverList::from_vec(vec![{ let __arg36 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i")); crate::proof_kernel::aver_generated::kernel::rules::inRange(__arg36, len) }, { let __arg37 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("!="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("j")))); crate::proof_kernel::aver_generated::kernel::rules::is(__arg37, true) }]), concl: { let __arg40 @ _ = { let __arg38 @ _ = aver_rt::AverList::from_vec(vec![written, crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("j"))]); crate::proof_kernel::aver_generated::kernel::rules::vb(::aver_rt::str_lit!("get"), &__arg38) }; { let __arg41 @ _ = { let __arg39 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("v")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("j"))]); crate::proof_kernel::aver_generated::kernel::rules::vb(::aver_rt::str_lit!("get"), &__arg39) }; crate::proof_kernel::aver_generated::kernel::rules::eq(__arg40, __arg41) } } })
                                    } else {
                                        if &*__dispatch_subject == "vector.len.set" {
                                            {
                                                let __arg43 @ _ = {
                                                    let __arg42 @ _ =
                                                        aver_rt::AverList::prepend_owned(
                                                            written,
                                                            aver_rt::AverList::empty(),
                                                        );
                                                    crate::proof_kernel::aver_generated::kernel::rules::vb(::aver_rt::str_lit!("len"), &__arg42)
                                                };
                                                crate::proof_kernel::aver_generated::kernel::rules::plain(&vix, __arg43, len)
                                            }
                                        } else {
                                            if &*__dispatch_subject == "vector.len.new" {
                                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("n")), crate::proof_kernel::aver_generated::kernel::rules::any(::aver_rt::str_lit!("x"))]), premises: aver_rt::AverList::prepend_owned({ let __arg44 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n")))); crate::proof_kernel::aver_generated::kernel::rules::is(__arg44, true) }, aver_rt::AverList::empty()), concl: { let __arg46 @ _ = { let __arg45 @ _ = aver_rt::AverList::prepend_owned(made, aver_rt::AverList::empty()); crate::proof_kernel::aver_generated::kernel::rules::vb(::aver_rt::str_lit!("len"), &__arg45) }; { let __arg47 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n")); crate::proof_kernel::aver_generated::kernel::rules::eq(__arg46, __arg47) } } })
                                            } else {
                                                if &*__dispatch_subject == "vector.get.new" {
                                                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("n")), crate::proof_kernel::aver_generated::kernel::rules::any(::aver_rt::str_lit!("x")), crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("i"))]), premises: aver_rt::AverList::prepend_owned({ let __arg48 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i")); { let __arg49 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("n")); crate::proof_kernel::aver_generated::kernel::rules::inRange(__arg48, __arg49) } }, aver_rt::AverList::empty()), concl: { let __arg51 @ _ = { let __arg50 @ _ = aver_rt::AverList::from_vec(vec![made, crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("i"))]); crate::proof_kernel::aver_generated::kernel::rules::vb(::aver_rt::str_lit!("get"), &__arg50) }; { let __arg52 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(::aver_rt::str_lit!("Option.Some"), aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x")), aver_rt::AverList::empty()))); crate::proof_kernel::aver_generated::kernel::rules::eq(__arg51, __arg52) } } })
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
        premises: aver_rt::AverList::prepend_owned(
            crate::proof_kernel::aver_generated::kernel::rules::is(test, holds),
            aver_rt::AverList::empty(),
        ),
        concl: crate::proof_kernel::aver_generated::kernel::rules::eq(l, r),
    })
}

/// Int.min and Int.max by a <= b, Int.abs by a < 0; List.contains on [], on an element in front that the value looked for is (found) and past one it is not, by y == x as the builtin compares. Both read == only at a type whose == is equality, where x == x is true of every value.
#[inline(always)]
pub fn pickSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let le @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
            ::aver_rt::str_lit!("<="),
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")),
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b")),
        ),
    );
    let neg @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
            ::aver_rt::str_lit!("<"),
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")),
            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                    aver_rt::AverInt::from_i64(0),
                ),
            ),
        ),
    );
    let min @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            ::aver_rt::str_lit!("Int.min"),
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")),
                crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b")),
            ]),
        ),
    );
    let max @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            ::aver_rt::str_lit!("Int.max"),
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")),
                crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("b")),
            ]),
        ),
    );
    let abs @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
            ::aver_rt::str_lit!("Int.abs"),
            aver_rt::AverList::prepend_owned(
                crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")),
                aver_rt::AverList::empty(),
            ),
        ),
    );
    let found @ _ = {
        let __arg7 @ _ = aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")),
            crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("y")),
        ]);
        crate::proof_kernel::aver_generated::kernel::rules::lb(
            ::aver_rt::str_lit!("contains"),
            &__arg7,
        )
    };
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "int.min.le" {
            {
                let __arg8 @ _ = aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("a")),
                    crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!("b")),
                ]);
                {
                    let __arg9 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                        ::aver_rt::str_lit!("a"),
                    );
                    crate::proof_kernel::aver_generated::kernel::rules::picked(
                        &__arg8, le, true, min, __arg9,
                    )
                }
            }
        } else {
            if &*__dispatch_subject == "int.min.gt" {
                {
                    let __arg10 @ _ = aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!(
                            "a"
                        )),
                        crate::proof_kernel::aver_generated::kernel::rules::i(::aver_rt::str_lit!(
                            "b"
                        )),
                    ]);
                    {
                        let __arg11 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                            ::aver_rt::str_lit!("b"),
                        );
                        crate::proof_kernel::aver_generated::kernel::rules::picked(
                            &__arg10, le, false, min, __arg11,
                        )
                    }
                }
            } else {
                if &*__dispatch_subject == "int.max.le" {
                    {
                        let __arg12 @ _ = aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::i(
                                ::aver_rt::str_lit!("a"),
                            ),
                            crate::proof_kernel::aver_generated::kernel::rules::i(
                                ::aver_rt::str_lit!("b"),
                            ),
                        ]);
                        {
                            let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                                ::aver_rt::str_lit!("b"),
                            );
                            crate::proof_kernel::aver_generated::kernel::rules::picked(
                                &__arg12, le, true, max, __arg13,
                            )
                        }
                    }
                } else {
                    if &*__dispatch_subject == "int.max.gt" {
                        {
                            let __arg14 @ _ = aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    ::aver_rt::str_lit!("a"),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::i(
                                    ::aver_rt::str_lit!("b"),
                                ),
                            ]);
                            {
                                let __arg15 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("a"),
                                    );
                                crate::proof_kernel::aver_generated::kernel::rules::picked(
                                    &__arg14, le, false, max, __arg15,
                                )
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "int.abs.neg" {
                            {
                                let __arg16 @ _ = aver_rt::AverList::prepend_owned(
                                    crate::proof_kernel::aver_generated::kernel::rules::i(
                                        ::aver_rt::str_lit!("a"),
                                    ),
                                    aver_rt::AverList::empty(),
                                );
                                {
                                    let __arg17 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("-"), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a"))));
                                    crate::proof_kernel::aver_generated::kernel::rules::picked(
                                        &__arg16, neg, true, abs, __arg17,
                                    )
                                }
                            }
                        } else {
                            if &*__dispatch_subject == "int.abs.nonneg" {
                                {
                                    let __arg18 @ _ = aver_rt::AverList::prepend_owned(
                                        crate::proof_kernel::aver_generated::kernel::rules::i(
                                            ::aver_rt::str_lit!("a"),
                                        ),
                                        aver_rt::AverList::empty(),
                                    );
                                    {
                                        let __arg19 @ _ =
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("a"),
                                            );
                                        crate::proof_kernel::aver_generated::kernel::rules::picked(
                                            &__arg18, neg, false, abs, __arg19,
                                        )
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "list.contains.nil" {
                                    {
                                        let __arg21 @ _ = aver_rt::AverList::prepend_owned(
                                            crate::proof_kernel::aver_generated::kernel::rules::any(
                                                ::aver_rt::str_lit!("y"),
                                            ),
                                            aver_rt::AverList::empty(),
                                        );
                                        {
                                            let __arg22 @ _ = {
                                                let __arg20 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TList(crate::proof_kernel::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty())), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("y"))]);
                                                crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("contains"), &__arg20)
                                            };
                                            {
                                                let __arg23 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false));
                                                crate::proof_kernel::aver_generated::kernel::rules::plain(&__arg21, __arg22, __arg23)
                                            }
                                        }
                                    }
                                } else {
                                    if &*__dispatch_subject == "list.contains.cons_found" {
                                        {
                                            let __arg27 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::same(::aver_rt::str_lit!("x")), crate::proof_kernel::aver_generated::kernel::rules::lt(::aver_rt::str_lit!("a")), crate::proof_kernel::aver_generated::kernel::rules::same(::aver_rt::str_lit!("y"))]);
                                            {
                                                let __arg28 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("y")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x"))));
                                                {
                                                    let __arg29 @ _ = {
                                                        let __arg26 @ _ = aver_rt::AverList::from_vec(vec![{ let __arg24 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x")); { let __arg25 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")); crate::proof_kernel::aver_generated::kernel::rules::cons(__arg24, __arg25) } }, crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("y"))]);
                                                        crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("contains"), &__arg26)
                                                    };
                                                    {
                                                        let __arg30 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true));
                                                        crate::proof_kernel::aver_generated::kernel::rules::picked(&__arg27, __arg28, true, __arg29, __arg30)
                                                    }
                                                }
                                            }
                                        }
                                    } else {
                                        if &*__dispatch_subject == "list.contains.cons_other" {
                                            {
                                                let __arg34 @ _ = aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::same(::aver_rt::str_lit!("x")), crate::proof_kernel::aver_generated::kernel::rules::lt(::aver_rt::str_lit!("a")), crate::proof_kernel::aver_generated::kernel::rules::same(::aver_rt::str_lit!("y"))]);
                                                {
                                                    let __arg35 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("=="), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("y")), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x"))));
                                                    {
                                                        let __arg36 @ _ = {
                                                            let __arg33 @ _ = aver_rt::AverList::from_vec(vec![{ let __arg31 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x")); { let __arg32 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("a")); crate::proof_kernel::aver_generated::kernel::rules::cons(__arg31, __arg32) } }, crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("y"))]);
                                                            crate::proof_kernel::aver_generated::kernel::rules::lb(::aver_rt::str_lit!("contains"), &__arg33)
                                                        };
                                                        crate::proof_kernel::aver_generated::kernel::rules::picked(&__arg34, __arg35, false, __arg36, found)
                                                    }
                                                }
                                            }
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
    let err @ _ = {
        let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
            crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(
                ::aver_rt::str_lit!("e"),
                false,
            ),
        );
        crate::proof_kernel::aver_generated::kernel::term::typed(::aver_rt::str_lit!("e"), __arg2)
    };
    {
        let __dispatch_subject = id;
        if &*__dispatch_subject == "option.with_default.some" {
            {
                let __arg3 @ _ = aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::any(::aver_rt::str_lit!(
                        "x"
                    )),
                    crate::proof_kernel::aver_generated::kernel::rules::any(::aver_rt::str_lit!(
                        "d"
                    )),
                ]);
                {
                    let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            ::aver_rt::str_lit!("Option.withDefault"),
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                                        ::aver_rt::str_lit!("Option.Some"),
                                        aver_rt::AverList::prepend_owned(
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                ::aver_rt::str_lit!("x"),
                                            ),
                                            aver_rt::AverList::empty(),
                                        ),
                                    ),
                                ),
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    ::aver_rt::str_lit!("d"),
                                ),
                            ]),
                        ),
                    );
                    {
                        let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                            ::aver_rt::str_lit!("x"),
                        );
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &__arg3, __arg4, __arg5,
                        )
                    }
                }
            }
        } else {
            if &*__dispatch_subject == "option.with_default.none" {
                {
                    let __arg6 @ _ = aver_rt::AverList::prepend_owned(
                        crate::proof_kernel::aver_generated::kernel::rules::any(
                            ::aver_rt::str_lit!("d"),
                        ),
                        aver_rt::AverList::empty(),
                    );
                    {
                        let __arg7 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("Option.withDefault"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(::aver_rt::str_lit!("Option.None"), aver_rt::AverList::empty())), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("d"))])));
                        {
                            let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::rules::v(
                                ::aver_rt::str_lit!("d"),
                            );
                            crate::proof_kernel::aver_generated::kernel::rules::plain(
                                &__arg6, __arg7, __arg8,
                            )
                        }
                    }
                }
            } else {
                if &*__dispatch_subject == "result.with_default.ok" {
                    {
                        let __arg9 @ _ = aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::any(
                                ::aver_rt::str_lit!("x"),
                            ),
                            crate::proof_kernel::aver_generated::kernel::rules::any(
                                ::aver_rt::str_lit!("d"),
                            ),
                        ]);
                        {
                            let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("Result.withDefault"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(::aver_rt::str_lit!("Result.Ok"), aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("x")), aver_rt::AverList::empty()))), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("d"))])));
                            {
                                let __arg11 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        ::aver_rt::str_lit!("x"),
                                    );
                                crate::proof_kernel::aver_generated::kernel::rules::plain(
                                    &__arg9, __arg10, __arg11,
                                )
                            }
                        }
                    }
                } else {
                    if &*__dispatch_subject == "result.with_default.err" {
                        {
                            let __arg12 @ _ = aver_rt::AverList::from_vec(vec![
                                err,
                                crate::proof_kernel::aver_generated::kernel::rules::any(
                                    ::aver_rt::str_lit!("d"),
                                ),
                            ]);
                            {
                                let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("Result.withDefault"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(::aver_rt::str_lit!("Result.Err"), aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("e")), aver_rt::AverList::empty()))), crate::proof_kernel::aver_generated::kernel::rules::v(::aver_rt::str_lit!("d"))])));
                                {
                                    let __arg14 @ _ =
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            ::aver_rt::str_lit!("d"),
                                        );
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(
                                        &__arg12, __arg13, __arg14,
                                    )
                                }
                            }
                        }
                    } else {
                        None
                    }
                }
            }
        }
    }
}
