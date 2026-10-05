#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Schema {
    pub binders: aver_rt::AverList<AverStr>,
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
    crate::proof_kernel::aver_generated::kernel::term::Term::TVar(n)
}

/// An equation.
pub fn eq(
    l @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    r @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Eqn {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Eqn {
        lhs: l.clone(),
        rhs: r.clone(),
    }
}

/// A Bool term equals a literal.
#[inline(always)]
pub fn is(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    value @ _: bool,
) -> crate::proof_kernel::aver_generated::kernel::term::Eqn {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::rules::eq(
        t,
        &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(value),
    )
}

/// Bool.and or Bool.or.
pub fn conn(
    name @ _: AverStr,
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    b @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        name,
        aver_rt::AverList::from_vec(vec![a.clone(), b.clone()]),
    )
}

/// A rule without premises.
pub fn plain(
    binders @ _: &aver_rt::AverList<AverStr>,
    l @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    r @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
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
                &aver_rt::AverList::from_vec(vec![AverStr::from("b")]),
                &crate::proof_kernel::aver_generated::kernel::rules::conn(
                    AverStr::from("Bool.and"),
                    &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true),
                    &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                ),
                &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
            )
        } else {
            if &*__dispatch_subject == "bool.and.false_l" {
                crate::proof_kernel::aver_generated::kernel::rules::plain(
                    &aver_rt::AverList::from_vec(vec![AverStr::from("b")]),
                    &crate::proof_kernel::aver_generated::kernel::rules::conn(
                        AverStr::from("Bool.and"),
                        &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false),
                        &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                    ),
                    &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false),
                )
            } else {
                if &*__dispatch_subject == "bool.and.true_r" {
                    crate::proof_kernel::aver_generated::kernel::rules::plain(
                        &aver_rt::AverList::from_vec(vec![AverStr::from("a")]),
                        &crate::proof_kernel::aver_generated::kernel::rules::conn(
                            AverStr::from("Bool.and"),
                            &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "a",
                            )),
                            &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true),
                        ),
                        &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
                    )
                } else {
                    if &*__dispatch_subject == "bool.and.false_r" {
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &aver_rt::AverList::from_vec(vec![AverStr::from("a")]),
                            &crate::proof_kernel::aver_generated::kernel::rules::conn(
                                AverStr::from("Bool.and"),
                                &crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("a"),
                                ),
                                &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                    false,
                                ),
                            ),
                            &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false),
                        )
                    } else {
                        if &*__dispatch_subject == "bool.or.true_l" {
                            crate::proof_kernel::aver_generated::kernel::rules::plain(
                                &aver_rt::AverList::from_vec(vec![AverStr::from("b")]),
                                &crate::proof_kernel::aver_generated::kernel::rules::conn(
                                    AverStr::from("Bool.or"),
                                    &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                        true,
                                    ),
                                    &crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("b"),
                                    ),
                                ),
                                &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                    true,
                                ),
                            )
                        } else {
                            if &*__dispatch_subject == "bool.or.false_l" {
                                crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("b")]), &crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.or"), &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b"))), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")))
                            } else {
                                if &*__dispatch_subject == "bool.or.true_r" {
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("a")]), &crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.or"), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true)), &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true))
                                } else {
                                    if &*__dispatch_subject == "bool.or.false_r" {
                                        crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("a")]), &crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.or"), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false)), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))
                                    } else {
                                        if &*__dispatch_subject == "bool.not.true" {
                                            crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::empty(), &crate::proof_kernel::aver_generated::kernel::term::Term::TBi(AverStr::from("Bool.not"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true)])), &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false))
                                        } else {
                                            if &*__dispatch_subject == "bool.not.false" {
                                                crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::empty(), &crate::proof_kernel::aver_generated::kernel::term::Term::TBi(AverStr::from("Bool.not"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false)])), &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true))
                                            } else {
                                                if &*__dispatch_subject == "bool.and.elim_l" {
                                                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("a"), AverStr::from("b")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.and"), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b"))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), true) })
                                                } else {
                                                    if &*__dispatch_subject == "bool.and.elim_r" {
                                                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("a"), AverStr::from("b")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.and"), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b"))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")), true) })
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
    }
}

/// A decided comparison decides its complement.
pub fn complement(p @ _: AverStr, pv @ _: bool, q @ _: AverStr, qv @ _: bool) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
        binders: aver_rt::AverList::from_vec(vec![AverStr::from("a"), AverStr::from("b")]),
        premises: aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::rules::is(
                &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    p,
                    std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                        AverStr::from("a"),
                    )),
                    std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                        AverStr::from("b"),
                    )),
                ),
                pv,
            ),
        ]),
        concl: crate::proof_kernel::aver_generated::kernel::rules::is(
            &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                q,
                std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                    AverStr::from("a"),
                )),
                std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                    AverStr::from("b"),
                )),
            ),
            qv,
        ),
    })
}

/// The comparison complement rules.
#[inline(always)]
pub fn comparisonSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "int.le.of_not_gt" {
            crate::proof_kernel::aver_generated::kernel::rules::complement(
                AverStr::from(">"),
                false,
                AverStr::from("<="),
                true,
            )
        } else {
            if &*__dispatch_subject == "int.le.false_of_gt" {
                crate::proof_kernel::aver_generated::kernel::rules::complement(
                    AverStr::from(">"),
                    true,
                    AverStr::from("<="),
                    false,
                )
            } else {
                if &*__dispatch_subject == "int.ge.of_not_lt" {
                    crate::proof_kernel::aver_generated::kernel::rules::complement(
                        AverStr::from("<"),
                        false,
                        AverStr::from(">="),
                        true,
                    )
                } else {
                    if &*__dispatch_subject == "int.ge.false_of_lt" {
                        crate::proof_kernel::aver_generated::kernel::rules::complement(
                            AverStr::from("<"),
                            true,
                            AverStr::from(">="),
                            false,
                        )
                    } else {
                        if &*__dispatch_subject == "int.lt.of_not_ge" {
                            crate::proof_kernel::aver_generated::kernel::rules::complement(
                                AverStr::from(">="),
                                false,
                                AverStr::from("<"),
                                true,
                            )
                        } else {
                            if &*__dispatch_subject == "int.lt.false_of_ge" {
                                crate::proof_kernel::aver_generated::kernel::rules::complement(
                                    AverStr::from(">="),
                                    true,
                                    AverStr::from("<"),
                                    false,
                                )
                            } else {
                                if &*__dispatch_subject == "int.gt.of_not_le" {
                                    crate::proof_kernel::aver_generated::kernel::rules::complement(
                                        AverStr::from("<="),
                                        false,
                                        AverStr::from(">"),
                                        true,
                                    )
                                } else {
                                    if &*__dispatch_subject == "int.gt.false_of_le" {
                                        crate::proof_kernel::aver_generated::kernel::rules::complement(AverStr::from("<="), true, AverStr::from(">"), false)
                                    } else {
                                        if &*__dispatch_subject == "int.eq.of_not_ne" {
                                            crate::proof_kernel::aver_generated::kernel::rules::complement(AverStr::from("!="), false, AverStr::from("=="), true)
                                        } else {
                                            if &*__dispatch_subject == "int.eq.false_of_ne" {
                                                crate::proof_kernel::aver_generated::kernel::rules::complement(AverStr::from("!="), true, AverStr::from("=="), false)
                                            } else {
                                                if &*__dispatch_subject == "int.ne.of_not_eq" {
                                                    crate::proof_kernel::aver_generated::kernel::rules::complement(AverStr::from("=="), false, AverStr::from("!="), true)
                                                } else {
                                                    if &*__dispatch_subject == "int.ne.false_of_eq"
                                                    {
                                                        crate::proof_kernel::aver_generated::kernel::rules::complement(AverStr::from("=="), true, AverStr::from("!="), false)
                                                    } else {
                                                        crate::proof_kernel::aver_generated::kernel::rules::ringSchema(id)
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
}

/// Ring laws of Int.
#[inline(always)]
pub fn ringSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "int.add_comm" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &aver_rt::AverList::from_vec(vec![AverStr::from("a"), AverStr::from("b")]),
                &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    AverStr::from("+"),
                    std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                        AverStr::from("a"),
                    )),
                    std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                        AverStr::from("b"),
                    )),
                ),
                &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    AverStr::from("+"),
                    std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                        AverStr::from("b"),
                    )),
                    std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                        AverStr::from("a"),
                    )),
                ),
            )
        } else {
            if &*__dispatch_subject == "int.mul_comm" {
                crate::proof_kernel::aver_generated::kernel::rules::plain(
                    &aver_rt::AverList::from_vec(vec![AverStr::from("a"), AverStr::from("b")]),
                    &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                        AverStr::from("*"),
                        std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                            AverStr::from("a"),
                        )),
                        std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                            AverStr::from("b"),
                        )),
                    ),
                    &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                        AverStr::from("*"),
                        std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                            AverStr::from("b"),
                        )),
                        std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                            AverStr::from("a"),
                        )),
                    ),
                )
            } else {
                if &*__dispatch_subject == "int.add_assoc" {
                    crate::proof_kernel::aver_generated::kernel::rules::plain(
                        &aver_rt::AverList::from_vec(vec![
                            AverStr::from("a"),
                            AverStr::from("b"),
                            AverStr::from("c"),
                        ]),
                        &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            AverStr::from("+"),
                            std::sync::Arc::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                    AverStr::from("+"),
                                    std::sync::Arc::new(
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("a"),
                                        ),
                                    ),
                                    std::sync::Arc::new(
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("b"),
                                        ),
                                    ),
                                ),
                            ),
                            std::sync::Arc::new(
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("c"),
                                ),
                            ),
                        ),
                        &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            AverStr::from("+"),
                            std::sync::Arc::new(
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("a"),
                                ),
                            ),
                            std::sync::Arc::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                    AverStr::from("+"),
                                    std::sync::Arc::new(
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("b"),
                                        ),
                                    ),
                                    std::sync::Arc::new(
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("c"),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    )
                } else {
                    if &*__dispatch_subject == "int.mul_assoc" {
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &aver_rt::AverList::from_vec(vec![
                                AverStr::from("a"),
                                AverStr::from("b"),
                                AverStr::from("c"),
                            ]),
                            &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                AverStr::from("*"),
                                std::sync::Arc::new(
                                    crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                        AverStr::from("*"),
                                        std::sync::Arc::new(
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                AverStr::from("a"),
                                            ),
                                        ),
                                        std::sync::Arc::new(
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                AverStr::from("b"),
                                            ),
                                        ),
                                    ),
                                ),
                                std::sync::Arc::new(
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("c"),
                                    ),
                                ),
                            ),
                            &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                AverStr::from("*"),
                                std::sync::Arc::new(
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("a"),
                                    ),
                                ),
                                std::sync::Arc::new(
                                    crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                        AverStr::from("*"),
                                        std::sync::Arc::new(
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                AverStr::from("b"),
                                            ),
                                        ),
                                        std::sync::Arc::new(
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                AverStr::from("c"),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        )
                    } else {
                        if &*__dispatch_subject == "int.add_zero" {
                            crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("a")]), &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("+"), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a"))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(0)))), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))
                        } else {
                            if &*__dispatch_subject == "int.zero_add" {
                                crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("a")]), &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("+"), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(0))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))
                            } else {
                                if &*__dispatch_subject == "int.mul_one" {
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("a")]), &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("*"), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a"))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(1)))), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))
                                } else {
                                    if &*__dispatch_subject == "int.one_mul" {
                                        crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("a")]), &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("*"), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(1))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))
                                    } else {
                                        if &*__dispatch_subject == "int.sub_zero" {
                                            crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("a")]), &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("-"), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a"))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(0)))), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")))
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
}

/// Euclidean division by a literal, as the compiler writes it.
pub fn divE(
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    k @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        AverStr::from("__int_div_euclid"),
        aver_rt::AverList::from_vec(vec![a.clone(), k.clone()]),
    )
}

/// Euclidean remainder by a literal.
pub fn modE(
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    k @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        AverStr::from("__int_mod_euclid"),
        aver_rt::AverList::from_vec(vec![a.clone(), k.clone()]),
    )
}

/// 0 <= t and t < bound.
#[inline(always)]
pub fn range(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    bound @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::rules::conn(
        AverStr::from("Bool.and"),
        &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
            AverStr::from("<="),
            std::sync::Arc::new(
                crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                    aver_rt::AverInt::from_i64(0),
                ),
            ),
            std::sync::Arc::new(t.clone()),
        ),
        &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
            AverStr::from("<"),
            std::sync::Arc::new(t.clone()),
            std::sync::Arc::new(bound.clone()),
        ),
    )
}

/// Euclidean division by a positive divisor.
#[inline(always)]
pub fn divisionSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "int.div_mod_recompose" {
            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                binders: aver_rt::AverList::from_vec(vec![AverStr::from("a"), AverStr::from("k")]),
                premises: aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::rules::is(
                        &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            AverStr::from(">"),
                            std::sync::Arc::new(
                                crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("k"),
                                ),
                            ),
                            std::sync::Arc::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                                    aver_rt::AverInt::from_i64(0),
                                ),
                            ),
                        ),
                        true,
                    ),
                ]),
                concl: crate::proof_kernel::aver_generated::kernel::rules::eq(
                    &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                        AverStr::from("+"),
                        std::sync::Arc::new(
                            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                AverStr::from("*"),
                                std::sync::Arc::new(
                                    crate::proof_kernel::aver_generated::kernel::rules::divE(
                                        &crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("a"),
                                        ),
                                        &crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("k"),
                                        ),
                                    ),
                                ),
                                std::sync::Arc::new(
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("k"),
                                    ),
                                ),
                            ),
                        ),
                        std::sync::Arc::new(
                            crate::proof_kernel::aver_generated::kernel::rules::modE(
                                &crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("a"),
                                ),
                                &crate::proof_kernel::aver_generated::kernel::rules::v(
                                    AverStr::from("k"),
                                ),
                            ),
                        ),
                    ),
                    &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
                ),
            })
        } else {
            if &*__dispatch_subject == "int.div_range" {
                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("a"), AverStr::from("k"), AverStr::from("m"), AverStr::from("n")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::rules::range(&crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m"))), true), crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.and"), &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from(">"), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(0)))), &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("=="), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m"))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("*"), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))))))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::rules::range(&crate::proof_kernel::aver_generated::kernel::rules::divE(&crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))), true) })
            } else {
                crate::proof_kernel::aver_generated::kernel::rules::listSchema(id)
            }
        }
    }
}

/// An element in front of a list.
pub fn cons(
    x @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    xs @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        AverStr::from("List.prepend"),
        aver_rt::AverList::from_vec(vec![x.clone(), xs.clone()]),
    )
}

/// A list builtin applied to its arguments.
pub fn lb(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
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
    )
}

/// A rule on a count n that holds when n > 0 (positive) or when n <= 0.
pub fn counted(
    binders @ _: &aver_rt::AverList<AverStr>,
    positive @ _: bool,
    l @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    r @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let premise @ _ = if positive {
        crate::proof_kernel::aver_generated::kernel::rules::is(
            &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                AverStr::from(">"),
                std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                    AverStr::from("n"),
                )),
                std::sync::Arc::new(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                        aver_rt::AverInt::from_i64(0),
                    ),
                ),
            ),
            true,
        )
    } else {
        crate::proof_kernel::aver_generated::kernel::rules::is(
            &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                AverStr::from("<="),
                std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                    AverStr::from("n"),
                )),
                std::sync::Arc::new(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                        aver_rt::AverInt::from_i64(0),
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

/// Each list builtin on the empty list and on an element in front of a list.
#[inline(always)]
pub fn listSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let xa @ _ = crate::proof_kernel::aver_generated::kernel::rules::cons(
        &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")),
        &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")),
    );
    let nil @ _ =
        crate::proof_kernel::aver_generated::kernel::term::Term::TList(aver_rt::AverList::empty());
    let less @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        AverStr::from("-"),
        std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
            AverStr::from("n"),
        )),
        std::sync::Arc::new(
            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                aver_rt::AverInt::from_i64(1),
            ),
        ),
    );
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "list.concat.nil" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &aver_rt::AverList::from_vec(vec![AverStr::from("b")]),
                &crate::proof_kernel::aver_generated::kernel::rules::lb(
                    AverStr::from("concat"),
                    &aver_rt::AverList::from_vec(vec![
                        nil,
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
                    ]),
                ),
                &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("b")),
            )
        } else {
            if &*__dispatch_subject == "list.concat.cons" {
                crate::proof_kernel::aver_generated::kernel::rules::plain(
                    &aver_rt::AverList::from_vec(vec![
                        AverStr::from("x"),
                        AverStr::from("a"),
                        AverStr::from("b"),
                    ]),
                    &crate::proof_kernel::aver_generated::kernel::rules::lb(
                        AverStr::from("concat"),
                        &aver_rt::AverList::from_vec(vec![
                            xa,
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "b",
                            )),
                        ]),
                    ),
                    &crate::proof_kernel::aver_generated::kernel::rules::cons(
                        &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")),
                        &crate::proof_kernel::aver_generated::kernel::rules::lb(
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
                if &*__dispatch_subject == "list.len.nil" {
                    crate::proof_kernel::aver_generated::kernel::rules::plain(
                        &aver_rt::AverList::empty(),
                        &crate::proof_kernel::aver_generated::kernel::rules::lb(
                            AverStr::from("len"),
                            &aver_rt::AverList::from_vec(vec![nil]),
                        ),
                        &crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                            aver_rt::AverInt::from_i64(0),
                        ),
                    )
                } else {
                    if &*__dispatch_subject == "list.len.cons" {
                        crate::proof_kernel::aver_generated::kernel::rules::plain(
                            &aver_rt::AverList::from_vec(vec![
                                AverStr::from("x"),
                                AverStr::from("a"),
                            ]),
                            &crate::proof_kernel::aver_generated::kernel::rules::lb(
                                AverStr::from("len"),
                                &aver_rt::AverList::from_vec(vec![xa]),
                            ),
                            &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                AverStr::from("+"),
                                std::sync::Arc::new(
                                    crate::proof_kernel::aver_generated::kernel::rules::lb(
                                        AverStr::from("len"),
                                        &aver_rt::AverList::from_vec(vec![
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                AverStr::from("a"),
                                            ),
                                        ]),
                                    ),
                                ),
                                std::sync::Arc::new(
                                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                                        aver_rt::AverInt::from_i64(1),
                                    ),
                                ),
                            ),
                        )
                    } else {
                        if &*__dispatch_subject == "list.take.nil" {
                            crate::proof_kernel::aver_generated::kernel::rules::plain(
                                &aver_rt::AverList::from_vec(vec![AverStr::from("n")]),
                                &crate::proof_kernel::aver_generated::kernel::rules::lb(
                                    AverStr::from("take"),
                                    &aver_rt::AverList::from_vec(vec![
                                        nil.clone(),
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("n"),
                                        ),
                                    ]),
                                ),
                                &nil,
                            )
                        } else {
                            if &*__dispatch_subject == "list.take.cons_le" {
                                crate::proof_kernel::aver_generated::kernel::rules::counted(
                                    &aver_rt::AverList::from_vec(vec![
                                        AverStr::from("x"),
                                        AverStr::from("a"),
                                        AverStr::from("n"),
                                    ]),
                                    false,
                                    &crate::proof_kernel::aver_generated::kernel::rules::lb(
                                        AverStr::from("take"),
                                        &aver_rt::AverList::from_vec(vec![
                                            xa,
                                            crate::proof_kernel::aver_generated::kernel::rules::v(
                                                AverStr::from("n"),
                                            ),
                                        ]),
                                    ),
                                    &nil,
                                )
                            } else {
                                if &*__dispatch_subject == "list.take.cons_gt" {
                                    crate::proof_kernel::aver_generated::kernel::rules::counted(&aver_rt::AverList::from_vec(vec![AverStr::from("x"), AverStr::from("a"), AverStr::from("n")]), true, &crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("take"), &aver_rt::AverList::from_vec(vec![xa, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))])), &crate::proof_kernel::aver_generated::kernel::rules::cons(&crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")), &crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("take"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), less]))))
                                } else {
                                    if &*__dispatch_subject == "list.drop.nil" {
                                        crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("n")]), &crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("drop"), &aver_rt::AverList::from_vec(vec![nil.clone(), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))])), &nil)
                                    } else {
                                        if &*__dispatch_subject == "list.drop.cons_le" {
                                            crate::proof_kernel::aver_generated::kernel::rules::counted(&aver_rt::AverList::from_vec(vec![AverStr::from("x"), AverStr::from("a"), AverStr::from("n")]), false, &crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("drop"), &aver_rt::AverList::from_vec(vec![xa.clone(), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))])), &xa)
                                        } else {
                                            if &*__dispatch_subject == "list.drop.cons_gt" {
                                                crate::proof_kernel::aver_generated::kernel::rules::counted(&aver_rt::AverList::from_vec(vec![AverStr::from("x"), AverStr::from("a"), AverStr::from("n")]), true, &crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("drop"), &aver_rt::AverList::from_vec(vec![xa, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))])), &crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("drop"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a")), less])))
                                            } else {
                                                if &*__dispatch_subject == "list.reverse.nil" {
                                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::empty(), &crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("reverse"), &aver_rt::AverList::from_vec(vec![nil.clone()])), &nil)
                                                } else {
                                                    if &*__dispatch_subject == "list.reverse.cons" {
                                                        crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("x"), AverStr::from("a")]), &crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("reverse"), &aver_rt::AverList::from_vec(vec![xa])), &crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("concat"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::lb(AverStr::from("reverse"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("a"))])), crate::proof_kernel::aver_generated::kernel::rules::cons(&crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")), &nil)])))
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
    }
}

/// A Map builtin applied to its arguments; the empty map is Map.empty().
pub fn mb(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
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
            &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                AverStr::from("!="),
                std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                    AverStr::from("k"),
                )),
                std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(
                    AverStr::from("k2"),
                )),
            ),
            true,
        ),
    ]);
    {
        let __dispatch_subject = id.clone();
        if &*__dispatch_subject == "map.get.empty" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &aver_rt::AverList::from_vec(vec![AverStr::from("k")]),
                &crate::proof_kernel::aver_generated::kernel::rules::mb(
                    AverStr::from("get"),
                    &aver_rt::AverList::from_vec(vec![
                        empty,
                        crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k")),
                    ]),
                ),
                &crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                    AverStr::from("Option.None"),
                    aver_rt::AverList::empty(),
                ),
            )
        } else {
            if &*__dispatch_subject == "map.get.set_same" {
                crate::proof_kernel::aver_generated::kernel::rules::plain(
                    &aver_rt::AverList::from_vec(vec![
                        AverStr::from("m"),
                        AverStr::from("k"),
                        AverStr::from("v"),
                    ]),
                    &crate::proof_kernel::aver_generated::kernel::rules::mb(
                        AverStr::from("get"),
                        &aver_rt::AverList::from_vec(vec![
                            set,
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "k",
                            )),
                        ]),
                    ),
                    &crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                        AverStr::from("Option.Some"),
                        aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from(
                                "v",
                            )),
                        ]),
                    ),
                )
            } else {
                if &*__dispatch_subject == "map.get.set_other" {
                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema {
                        binders: aver_rt::AverList::from_vec(vec![
                            AverStr::from("m"),
                            AverStr::from("k"),
                            AverStr::from("v"),
                            AverStr::from("k2"),
                        ]),
                        premises: other,
                        concl: crate::proof_kernel::aver_generated::kernel::rules::eq(
                            &crate::proof_kernel::aver_generated::kernel::rules::mb(
                                AverStr::from("get"),
                                &aver_rt::AverList::from_vec(vec![
                                    set,
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("k2"),
                                    ),
                                ]),
                            ),
                            &crate::proof_kernel::aver_generated::kernel::rules::mb(
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
                            &aver_rt::AverList::from_vec(vec![AverStr::from("k")]),
                            &crate::proof_kernel::aver_generated::kernel::rules::mb(
                                AverStr::from("has"),
                                &aver_rt::AverList::from_vec(vec![
                                    empty,
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("k"),
                                    ),
                                ]),
                            ),
                            &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false),
                        )
                    } else {
                        if &*__dispatch_subject == "map.has.set_same" {
                            crate::proof_kernel::aver_generated::kernel::rules::plain(
                                &aver_rt::AverList::from_vec(vec![
                                    AverStr::from("m"),
                                    AverStr::from("k"),
                                    AverStr::from("v"),
                                ]),
                                &crate::proof_kernel::aver_generated::kernel::rules::mb(
                                    AverStr::from("has"),
                                    &aver_rt::AverList::from_vec(vec![
                                        set,
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("k"),
                                        ),
                                    ]),
                                ),
                                &crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                    true,
                                ),
                            )
                        } else {
                            if &*__dispatch_subject == "map.has.set_other" {
                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("m"), AverStr::from("k"), AverStr::from("v"), AverStr::from("k2")]), premises: other, concl: crate::proof_kernel::aver_generated::kernel::rules::eq(&crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("has"), &aver_rt::AverList::from_vec(vec![set, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k2"))])), &crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("has"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k2"))]))) })
                            } else {
                                if &*__dispatch_subject == "map.len.empty" {
                                    crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::empty(), &crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![empty])), &crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(0)))
                                } else {
                                    if &*__dispatch_subject == "map.len.set_present" {
                                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("m"), AverStr::from("k"), AverStr::from("v")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("has"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))])), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(&crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![set])), &crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m"))]))) })
                                    } else {
                                        if &*__dispatch_subject == "map.len.set_absent" {
                                            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("m"), AverStr::from("k"), AverStr::from("v")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("has"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("k"))])), false)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(&crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![set])), &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("+"), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::mb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("m"))]))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(1))))) })
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
    )
}

/// 0 <= at and at < end, as a premise.
#[inline(always)]
pub fn inRange(
    at @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    end @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::term::Eqn {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::rules::is(
        &crate::proof_kernel::aver_generated::kernel::rules::conn(
            AverStr::from("Bool.and"),
            &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                AverStr::from("<="),
                std::sync::Arc::new(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                        aver_rt::AverInt::from_i64(0),
                    ),
                ),
                std::sync::Arc::new(at.clone()),
            ),
            &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                AverStr::from("<"),
                std::sync::Arc::new(at.clone()),
                std::sync::Arc::new(end.clone()),
            ),
        ),
        true,
    )
}

/// A vector as the list it holds; reads out of range; a read and the length after a write; a literal-size Vector.new.
#[inline(always)]
pub fn vectorSchema(id @ _: AverStr) -> Option<Schema> {
    crate::proof_kernel::cancel_checkpoint();
    let len @ _ = crate::proof_kernel::aver_generated::kernel::rules::vb(
        AverStr::from("len"),
        &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(
            AverStr::from("v"),
        )]),
    );
    let written @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
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
    );
    let made @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
        AverStr::from("__vector_new"),
        aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")),
            crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x")),
        ]),
    );
    {
        let __dispatch_subject = id;
        if &*__dispatch_subject == "vector.to_list.of_list" {
            crate::proof_kernel::aver_generated::kernel::rules::plain(
                &aver_rt::AverList::from_vec(vec![AverStr::from("l")]),
                &crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
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
                &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("l")),
            )
        } else {
            if &*__dispatch_subject == "vector.of_list.to_list" {
                crate::proof_kernel::aver_generated::kernel::rules::plain(
                    &aver_rt::AverList::from_vec(vec![AverStr::from("v")]),
                    &crate::proof_kernel::aver_generated::kernel::rules::vb(
                        AverStr::from("fromList"),
                        &aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                                AverStr::from("List.fromVector"),
                                aver_rt::AverList::from_vec(vec![
                                    crate::proof_kernel::aver_generated::kernel::rules::v(
                                        AverStr::from("v"),
                                    ),
                                ]),
                            ),
                        ]),
                    ),
                    &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")),
                )
            } else {
                if &*__dispatch_subject == "vector.len.to_list" {
                    crate::proof_kernel::aver_generated::kernel::rules::plain(
                        &aver_rt::AverList::from_vec(vec![AverStr::from("v")]),
                        &len,
                        &crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                            AverStr::from("List.len"),
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                                    AverStr::from("List.fromVector"),
                                    aver_rt::AverList::from_vec(vec![
                                        crate::proof_kernel::aver_generated::kernel::rules::v(
                                            AverStr::from("v"),
                                        ),
                                    ]),
                                ),
                            ]),
                        ),
                    )
                } else {
                    if &*__dispatch_subject == "vector.get.negative" {
                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("v"), AverStr::from("i")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("<"), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(0)))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(&crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))])), &crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(AverStr::from("Option.None"), aver_rt::AverList::empty())) })
                    } else {
                        if &*__dispatch_subject == "vector.get.past_end" {
                            Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("v"), AverStr::from("i")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from(">="), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))), std::sync::Arc::new(len)), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(&crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))])), &crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(AverStr::from("Option.None"), aver_rt::AverList::empty())) })
                        } else {
                            if &*__dispatch_subject == "vector.set.out_of_range" {
                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("v"), AverStr::from("i"), AverStr::from("x")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::rules::conn(AverStr::from("Bool.or"), &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("<"), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(0)))), &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from(">="), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))), std::sync::Arc::new(len))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(&crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("set"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x"))])), &crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(AverStr::from("Option.None"), aver_rt::AverList::empty())) })
                            } else {
                                if &*__dispatch_subject == "vector.get.set_same" {
                                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("v"), AverStr::from("i"), AverStr::from("x")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::inRange(&crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), &len)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(&crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![written, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))])), &crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(AverStr::from("Option.Some"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x"))]))) })
                                } else {
                                    if &*__dispatch_subject == "vector.get.set_other" {
                                        Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("v"), AverStr::from("i"), AverStr::from("x"), AverStr::from("j")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::inRange(&crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), &len), crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("!="), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("j")))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(&crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![written, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("j"))])), &crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("v")), crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("j"))]))) })
                                    } else {
                                        if &*__dispatch_subject == "vector.len.set" {
                                            crate::proof_kernel::aver_generated::kernel::rules::plain(&aver_rt::AverList::from_vec(vec![AverStr::from("v"), AverStr::from("i"), AverStr::from("x")]), &crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![written])), &len)
                                        } else {
                                            if &*__dispatch_subject == "vector.len.new" {
                                                Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("n"), AverStr::from("x")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::is(&crate::proof_kernel::aver_generated::kernel::term::Term::TOp(AverStr::from("<="), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(0))), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")))), true)]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(&crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("len"), &aver_rt::AverList::from_vec(vec![made])), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n"))) })
                                            } else {
                                                if &*__dispatch_subject == "vector.get.new" {
                                                    Some(crate::proof_kernel::aver_generated::kernel::rules::Schema { binders: aver_rt::AverList::from_vec(vec![AverStr::from("n"), AverStr::from("x"), AverStr::from("i")]), premises: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::inRange(&crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i")), &crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("n")))]), concl: crate::proof_kernel::aver_generated::kernel::rules::eq(&crate::proof_kernel::aver_generated::kernel::rules::vb(AverStr::from("get"), &aver_rt::AverList::from_vec(vec![made, crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("i"))])), &crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(AverStr::from("Option.Some"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::rules::v(AverStr::from("x"))]))) })
                                                } else {
                                                    None
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
