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
        let __dispatch_subject = id;
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
                None
            }
        }
    }
}
