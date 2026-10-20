#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

/// The goal a step script states, its proof left out. A goal the compiler poses has no hypotheses: the law's `when` stays its premise.
pub fn goal(
    text @ _: AverStr,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    Ok(crate::proof_std::aver_generated::kernel::wire::goalOf(
        &crate::proof_std::aver_generated::kernel::decode::script(
            &crate::proof_std::aver_generated::kernel::sexp::readAll(text)?,
        )?,
    ))
}

/// The goal of a read script: everything but its proof, no hypotheses yet, each given listed by what its type is.
#[inline(always)]
pub fn goalOf(
    s @ _: &crate::proof_std::aver_generated::kernel::proof::Script,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::wire::withGivens(
        crate::proof_std::aver_generated::kernel::proof::Goal {
            obligation: crate::proof_std::aver_generated::kernel::proof::Law {
                givens: aver_rt::AverList::empty(),
                ..s.obligation.clone()
            },
            finite: aver_rt::AverList::empty(),
            lists: aver_rt::AverList::empty(),
            ints: aver_rt::AverList::empty(),
            defs: s.defs.clone(),
            consts: s.consts.clone(),
            sums: s.sums.clone(),
            recs: s.recs.clone(),
            laws: s.laws.clone(),
            facts: s.facts.clone(),
            hyps: aver_rt::AverList::empty(),
        },
        s.obligation.givens.clone(),
    )
}

/// The goal with these givens after its own, in order.
#[inline(always)]
pub fn withGivens(
    mut g0 @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    mut gs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(gs, [] => { return g0; }, [g, rest] => { {
            let __tco0 = crate::proof_std::aver_generated::kernel::wire::withGiven(&g0, &g);
            let __tco1 = rest;
            g0 = __tco0;
            gs = __tco1;
            continue;
        } })
    }
}

/// The goal with one more given, also listed among its list, Int or finite givens when its type is one of those: the views a plan reads to choose a split.
pub fn withGiven(
    g0 @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
    g @ _: &crate::proof_std::aver_generated::kernel::term::TypedVar,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    crate::proof_std::cancel_checkpoint();
    let base @ _ = crate::proof_std::aver_generated::kernel::proof::Goal {
        obligation: crate::proof_std::aver_generated::kernel::proof::Law {
            givens: aver_rt::AverList::concat(
                &g0.obligation.givens.clone(),
                &aver_rt::AverList::from_vec(vec![g.clone()]),
            ),
            ..g0.obligation.clone()
        },
        ..g0.clone()
    };
    match (
        g.ty.clone(),
        crate::proof_std::aver_generated::kernel::types::finOf(
            &g.ty,
            &crate::proof_std::aver_generated::kernel::types::TyEnv {
                vars: aver_rt::AverList::empty(),
                defs: aver_rt::AverList::empty(),
                consts: aver_rt::AverList::empty(),
                sums: g0.sums.clone(),
                recs: g0.recs.clone(),
                sigs: aver_rt::AverList::empty(),
            },
            &aver_rt::AverList::empty(),
        ),
    ) {
        (crate::proof_std::aver_generated::kernel::term::Ty::TyList(__e_fields), _) => {
            let e = __e_fields.0.clone();
            crate::proof_std::aver_generated::kernel::proof::Goal {
                lists: aver_rt::AverList::concat(
                    &base.lists,
                    &aver_rt::AverList::from_vec(vec![g.name.clone()]),
                ),
                ..base
            }
        }
        (crate::proof_std::aver_generated::kernel::term::Ty::TyInt, _) => {
            crate::proof_std::aver_generated::kernel::proof::Goal {
                ints: aver_rt::AverList::concat(
                    &base.ints,
                    &aver_rt::AverList::from_vec(vec![g.name.clone()]),
                ),
                ..base
            }
        }
        (_, Some(f)) => crate::proof_std::aver_generated::kernel::proof::Goal {
            finite: aver_rt::AverList::concat(
                &base.finite,
                &aver_rt::AverList::from_vec(vec![
                    crate::proof_std::aver_generated::kernel::proof::Given {
                        name: g.name.clone(),
                        fin: f,
                    },
                ]),
            ),
            ..base
        },
        _ => base,
    }
}

/// A type in the step grammar; a type the grammar cannot state is _.
pub fn tyText(t @ _: &crate::proof_std::aver_generated::kernel::term::Ty) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyInt => AverStr::from("int"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyBool => AverStr::from("bool"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyStr => AverStr::from("str"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyFloat => AverStr::from("float"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyUnit => AverStr::from("unit"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyList(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(list "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::tyText(&a)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyOption(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(25)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(option "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::tyText(&a)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyVector(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(25)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(vector "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::tyText(&a)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyResult(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(42)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("(result "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::tyText(&a)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::tyText(&b)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyMap(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("(map "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::tyText(&a)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::tyText(&b)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                AverStr::from("tuple"),
                &crate::proof_std::aver_generated::kernel::wire::tyTexts(&xs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(__n_fields) => {
            let n = __n_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(named "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyRigid(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            AverStr::from("_")
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyVar(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            if eq {
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(25)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("(var "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" eq)"));
                    __b
                })
            } else {
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("(var "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    };
                    __b.push_str(&AverStr::from(")"));
                    __b
                })
            }
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyAny => AverStr::from("_"),
    }
}

/// Each type in the step grammar.
#[inline(always)]
pub fn tyTexts(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => aver_rt::AverList::empty(), [t, rest] => aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::wire::tyText(&t), &crate::proof_std::aver_generated::kernel::wire::tyTexts(&rest)))
}

/// What the compiler reads back from a plan: `proof` and the proof, or `refused` and the plan's reason, each on the line after the word.
#[inline(always)]
pub fn answer(
    r @ _: &Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr>,
) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match r.clone() {
        Ok(p @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("proof\n"));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::proofText(&p)),
            )));
            __b
        }),
        Err(why @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("refused\n"));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
            __b
        }),
    }
}

/// A text in quotes; a quote, a backslash and a line break are escaped.
#[inline(always)]
pub fn quote(s @ _: AverStr) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    aver_rt::AverStr::from({
        let mut __b = {
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("\""));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::escaped__cursor(
                    s,
                    aver_rt::AverInt::from_i64(0),
                    AverStr::from(""),
                )),
            )));
            __b
        };
        __b.push_str(&AverStr::from("\""));
        __b
    })
}

/// The characters with quotes, backslashes and line breaks escaped.
#[inline(always)]
pub fn escaped(mut cs @ _: aver_rt::AverList<AverStr>, mut acc @ _: AverStr) -> AverStr {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(cs, [] => { return acc; }, [c, rest] => { {
            let __tco0 = rest;
            let __tco1 = (acc + &crate::proof_std::aver_generated::kernel::wire::escapeOne(c));
            cs = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// One character, escaped when it has to be.
#[inline(always)]
pub fn escapeOne(c @ _: AverStr) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
        if __dispatch_subject == aver_rt::AverInt::from_i64(34) {
            AverStr::from("\\\"")
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(92) {
                AverStr::from("\\\\")
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(10) {
                    AverStr::from("\\n")
                } else {
                    c
                }
            }
        }
    }
}

/// Items separated by single spaces.
#[inline(always)]
pub fn spaced(xs @ _: &aver_rt::AverList<AverStr>) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    (aver_rt::string_join(&xs, &AverStr::from(" "))).into_aver()
}

/// (HEAD X…), or (HEAD) when there is nothing after the head.
pub fn headed(head @ _: AverStr, xs @ _: &aver_rt::AverList<AverStr>) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = xs;
        if __list_subject.is_empty() {
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(head))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        } else {
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(35)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(head))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::spaced(xs)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
    }
}

/// One term in the step grammar.
pub fn termText(t @ _: &crate::proof_std::aver_generated::kernel::term::Term) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match t {
        crate::proof_std::aver_generated::kernel::term::Term::TInt(__i_fields) => {
            let i = __i_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(i "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Term::TBool(__b_fields) => {
            let b = __b_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(b "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(b))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Term::TStr(__s_fields) => {
            let s = __s_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(s "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::quote(s)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Term::TUnit => AverStr::from("(unit)"),
        crate::proof_std::aver_generated::kernel::term::Term::TVar(__n_fields) => {
            let n = __n_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(v "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Term::THole => AverStr::from("(hole)"),
        crate::proof_std::aver_generated::kernel::term::Term::TGet(__o_fields) => {
            let o = __o_fields.0.clone();
            let f = __o_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("(get "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(&o)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Term::TCall(__f_fields) => {
            let f = __f_fields.0.clone();
            let xs = __f_fields.1.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("call "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::termTexts(&xs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TBi(__f_fields) => {
            let f = __f_fields.0.clone();
            let xs = __f_fields.1.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(19)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("bi "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::termTexts(&xs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TCtor(__c_fields) => {
            let c = __c_fields.0.clone();
            let xs = __c_fields.1.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("ctor "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c))));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::termTexts(&xs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TOp(__o_fields) => {
            let o = __o_fields.0.clone();
            let a = __o_fields.1.clone();
            let b = __o_fields.2.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(55))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("(op "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(o),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(&a)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&b)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Term::TNeg(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(neg "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&a)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Term::TMatch(__s_fields) => {
            let s = __s_fields.0.clone();
            let arms = __s_fields.1.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("match "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&s)),
                    )));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::armTexts(&arms),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TParts(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                AverStr::from("str"),
                &crate::proof_std::aver_generated::kernel::wire::termTexts(&xs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TList(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                AverStr::from("list"),
                &crate::proof_std::aver_generated::kernel::wire::termTexts(&xs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TTuple(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                AverStr::from("tuple"),
                &crate::proof_std::aver_generated::kernel::wire::termTexts(&xs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TRec(__ty_fields) => {
            let ty = __ty_fields.0.clone();
            let fs = __ty_fields.1.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("rec "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(ty))));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::fieldTexts(&fs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TUpd(__ty_fields) => {
            let ty = __ty_fields.0.clone();
            let b = __ty_fields.1.clone();
            let fs = __ty_fields.2.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(37)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("upd "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(ty))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&b)),
                    )));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::fieldTexts(&fs),
            )
        }
    }
}

/// Each term in the step grammar.
#[inline(always)]
pub fn termTexts(
    xs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(xs.clone(), [] => aver_rt::AverList::empty(), [x, rest] => aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::wire::termText(&x), &crate::proof_std::aver_generated::kernel::wire::termTexts(&rest)))
}

/// (FIELD TERM) for each field.
#[inline(always)]
pub fn fieldTexts(
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => aver_rt::AverList::empty(), [f, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(35)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("(")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f.name)))); __b }; __b.push_str(&AverStr::from(" ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&f.value))))); __b }; __b.push_str(&AverStr::from(")")); __b }), &crate::proof_std::aver_generated::kernel::wire::fieldTexts(&rest)) })
}

/// (arm PAT TERM) for each arm.
#[inline(always)]
pub fn armTexts(
    arms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(arms.clone(), [] => aver_rt::AverList::empty(), [a, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("(arm ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::patText(&a.pattern))))); __b }; __b.push_str(&AverStr::from(" ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&a.body))))); __b }; __b.push_str(&AverStr::from(")")); __b }), &crate::proof_std::aver_generated::kernel::wire::armTexts(&rest)) })
}

/// One pattern in the step grammar.
pub fn patText(p @ _: &crate::proof_std::aver_generated::kernel::term::Pat) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match p {
        crate::proof_std::aver_generated::kernel::term::Pat::PWild => AverStr::from("(pw)"),
        crate::proof_std::aver_generated::kernel::term::Pat::PVar(n) => {
            let n = n.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(pv "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Pat::PLit(t) => {
            let t = t.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(pl "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&t)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Pat::PNil => AverStr::from("(pnil)"),
        crate::proof_std::aver_generated::kernel::term::Pat::PCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("(pcons "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(t))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Pat::PTuple(ps) => {
            let ps = ps.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(pt "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::spaced(
                            &crate::proof_std::aver_generated::kernel::wire::patTexts(&ps),
                        )),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
            let c = c.clone();
            let ns = ns.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(19)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("pc "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c))));
                    __b
                }),
                &ns,
            )
        }
    }
}

/// Each pattern.
#[inline(always)]
pub fn patTexts(
    ps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Pat>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ps.clone(), [] => aver_rt::AverList::empty(), [p, rest] => aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::wire::patText(&p), &crate::proof_std::aver_generated::kernel::wire::patTexts(&rest)))
}

/// ((NAME TERM)…)
#[inline(always)]
pub fn bindingsText(
    bs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    aver_rt::AverStr::from({
        let mut __b = {
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("("));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::spaced(
                    &crate::proof_std::aver_generated::kernel::wire::bindingTexts(bs),
                )),
            )));
            __b
        };
        __b.push_str(&AverStr::from(")"));
        __b
    })
}

/// (NAME TERM) for each binding.
#[inline(always)]
pub fn bindingTexts(
    bs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(bs.clone(), [] => aver_rt::AverList::empty(), [b, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(35)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("(")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(b.name)))); __b }; __b.push_str(&AverStr::from(" ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&b.value))))); __b }; __b.push_str(&AverStr::from(")")); __b }), &crate::proof_std::aver_generated::kernel::wire::bindingTexts(&rest)) })
}

/// One proof in the step grammar.
pub fn proofText(p @ _: &crate::proof_std::aver_generated::kernel::proof::Proof) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match p {
        crate::proof_std::aver_generated::kernel::proof::Proof::PRefl(__t_fields) => {
            let t = __t_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(refl "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&t)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PSymm(__q_fields) => {
            let q = __q_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(symm "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::proofText(&q)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PTrans(__ts_fields) => {
            let ts = __ts_fields.0.clone();
            let ps = __ts_fields.1.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("trans ("));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                &crate::proof_std::aver_generated::kernel::wire::termTexts(&ts),
                            )),
                        )));
                        __b
                    };
                    __b.push_str(&AverStr::from(")"));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::proofTexts(&ps),
            )
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(__c_fields) => {
            let c = __c_fields.0.clone();
            let q = __c_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("(congr "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(&c)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::proofText(&q)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PUnfold(__f_fields) => {
            let f = __f_fields.0.clone();
            let k = __f_fields.1.clone();
            let xs = __f_fields.2.clone();
            let ys = __f_fields.3.clone();
            let pre = __f_fields.4.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = aver_rt::Buffer::with_capacity(
                                                        (aver_rt::AverInt::from_i64(78))
                                                            .to_usize()
                                                            .unwrap_or(0),
                                                    );
                                                    __b.push_str(&AverStr::from("unfold "));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(&(f)),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&AverStr::from(" "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(k)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&AverStr::from(" ("));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                        &crate::proof_std::aver_generated::kernel::wire::termTexts(
                                            &xs,
                                        ),
                                    )),
                                )));
                                __b
                            };
                            __b.push_str(&AverStr::from(") ("));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                &crate::proof_std::aver_generated::kernel::wire::termTexts(&ys),
                            )),
                        )));
                        __b
                    };
                    __b.push_str(&AverStr::from(")"));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::proofTexts(&pre),
            )
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PConst(__n_fields) => {
            let n = __n_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(const "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PArm(__k_fields) => {
            let k = __k_fields.0.clone();
            let ys = __k_fields.1.clone();
            let t = __k_fields.2.clone();
            let q = __k_fields.3.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(75))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&AverStr::from("(arm "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(k)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(" ("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&crate::proof_std::aver_generated::kernel::wire::termTexts(&ys))))));
                                    __b
                                };
                                __b.push_str(&AverStr::from(") "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(&t)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::proofText(&q)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PProj(__t_fields) => {
            let t = __t_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(proj "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&t)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PCell(__t_fields) => {
            let t = __t_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(cell "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&t)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(__h_fields) => {
            let h = __h_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("(hyp "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PRule(__id_fields) => {
            let id = __id_fields.0.clone();
            let bs = __id_fields.1.clone();
            let ps = __id_fields.2.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(38)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("rule "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(id))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::bindingsText(&bs)),
                    )));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::proofTexts(&ps),
            )
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PLaw(__k_fields) => {
            let k = __k_fields.0.clone();
            let bs = __k_fields.1.clone();
            let ps = __k_fields.2.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(37)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("law "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::bindingsText(&bs)),
                    )));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::proofTexts(&ps),
            )
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("(compute "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(&a)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&b)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PCases(__on_fields) => {
            let on = __on_fields.0.clone();
            let h = __on_fields.1.clone();
            let t = __on_fields.2.clone();
            let f = __on_fields.3.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(75))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&AverStr::from("(cases "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&on)))));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(" "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(h),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::proofText(&t)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::proofText(&f)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PSplit(__on_fields) => {
            let on = __on_fields.0.clone();
            let h = __on_fields.1.clone();
            let ks = __on_fields.2.clone();
            let cs = __on_fields.3.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(58))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&AverStr::from("split "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&on)))));
                                        __b
                                    };
                                    __b.push_str(&AverStr::from(" "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                                __b
                            };
                            __b.push_str(&AverStr::from(" ("));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                &crate::proof_std::aver_generated::kernel::wire::termTexts(&ks),
                            )),
                        )));
                        __b
                    };
                    __b.push_str(&AverStr::from(")"));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::splitCaseTexts(&cs),
            )
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PHave(__n_fields) => {
            let n = __n_fields.0.clone();
            let fact = __n_fields.1.clone();
            let q = __n_fields.2.clone();
            let body = __n_fields.3.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(74))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&AverStr::from("(have "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(n)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(" "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(crate::proof_std::aver_generated::kernel::wire::termText(
                                            &fact,
                                        )),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::proofText(&q)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::proofText(&body)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PEnum(__v_fields) => {
            let v = __v_fields.0.clone();
            let l = __v_fields.1.clone();
            let r = __v_fields.2.clone();
            let cs = __v_fields.3.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(55))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("enum "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(v),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(&l)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&r)),
                    )));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::proofTexts(&cs),
            )
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PAbsurd(__q_fields) => {
            let q = __q_fields.0.clone();
            let l = __q_fields.1.clone();
            let r = __q_fields.2.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(59))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("(absurd "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::proofText(&q)))));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(&l)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&r)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PInduct(__f_fields) => {
            let f = __f_fields.0.clone();
            let xs = __f_fields.1.clone();
            let l = __f_fields.2.clone();
            let r = __f_fields.3.clone();
            let carried = __f_fields.4.clone();
            let cs = __f_fields.5.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = aver_rt::Buffer::with_capacity(
                                                        (aver_rt::AverInt::from_i64(92))
                                                            .to_usize()
                                                            .unwrap_or(0),
                                                    );
                                                    __b.push_str(&AverStr::from("induct "));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(&(f)),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&AverStr::from(" ("));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&crate::proof_std::aver_generated::kernel::wire::termTexts(&xs))))));
                                        __b
                                    };
                                    __b.push_str(&AverStr::from(") "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::termText(&l)),
                                )));
                                __b
                            };
                            __b.push_str(&AverStr::from(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(&r)),
                        )));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::carryText(&carried)),
                    )));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::inductCaseTexts(&cs),
            )
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PListInduct(__v_fields) => {
            let v = __v_fields.0.clone();
            let l = __v_fields.1.clone();
            let r = __v_fields.2.clone();
            let carried = __v_fields.3.clone();
            let n = __v_fields.4.clone();
            let h = __v_fields.5.clone();
            let t = __v_fields.6.clone();
            let general = __v_fields.7.clone();
            let ihs = __v_fields.8.clone();
            let c = __v_fields.9.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b = {
                                                            let mut __b = {
                                                                let mut __b = {
                                                                    let mut __b = {
                                                                        let mut __b = {
                                                                            let mut __b = {
                                                                                let mut __b = {
                                                                                    let mut __b = {
                                                                                        let mut __b = {
                                                                                            let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(187)).to_usize().unwrap_or(0));
                                                                                            __b.push_str(&AverStr::from("(listinduct "));
                                                                                            __b
                                                                                        };
                                                                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                                                                        __b
                                                                                    };
                                                                                    __b.push_str(&AverStr::from(" "));
                                                                                    __b
                                                                                };
                                                                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&l)))));
                                                                                __b
                                                                            };
                                                                            __b.push_str(
                                                                                &AverStr::from(" "),
                                                                            );
                                                                            __b
                                                                        };
                                                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&r)))));
                                                                        __b
                                                                    };
                                                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::carryText(&carried)))));
                                                                    __b
                                                                };
                                                                __b.push_str(&AverStr::from(" "));
                                                                __b
                                                            };
                                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::proofText(&n)))));
                                                            __b
                                                        };
                                                        __b.push_str(&AverStr::from(" ("));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(h)),
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&AverStr::from(" "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(t)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(") ("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                            &general,
                                        )),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(") ("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                    &crate::proof_std::aver_generated::kernel::wire::ihAtTexts(
                                        &ihs,
                                    ),
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(") "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::proofText(&c)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PListCases(__v_fields) => {
            let v = __v_fields.0.clone();
            let l = __v_fields.1.clone();
            let r = __v_fields.2.clone();
            let n = __v_fields.3.clone();
            let h = __v_fields.4.clone();
            let t = __v_fields.5.clone();
            let c = __v_fields.6.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b = {
                                                            let mut __b = {
                                                                let mut __b = {
                                                                    let mut __b = {
                                                                        let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(132)).to_usize().unwrap_or(0));
                                                                        __b.push_str(
                                                                            &AverStr::from(
                                                                                "(listcases ",
                                                                            ),
                                                                        );
                                                                        __b
                                                                    };
                                                                    __b.push_str(
                                                                        &aver_rt::AverStr::from(
                                                                            aver_rt::aver_display(
                                                                                &(v),
                                                                            ),
                                                                        ),
                                                                    );
                                                                    __b
                                                                };
                                                                __b.push_str(&AverStr::from(" "));
                                                                __b
                                                            };
                                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&l)))));
                                                            __b
                                                        };
                                                        __b.push_str(&AverStr::from(" "));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&r)))));
                                                    __b
                                                };
                                                __b.push_str(&AverStr::from(" "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::proofText(&n)))));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(" ("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(h),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(t))));
                            __b
                        };
                        __b.push_str(&AverStr::from(") "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::proofText(&c)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PLenInduct(__v_fields) => {
            let v = __v_fields.0.clone();
            let l = __v_fields.1.clone();
            let r = __v_fields.2.clone();
            let carried = __v_fields.3.clone();
            let general = __v_fields.4.clone();
            let n = __v_fields.5.clone();
            let hn = __v_fields.6.clone();
            let ih = __v_fields.7.clone();
            let c = __v_fields.8.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b = {
                                                            let mut __b = {
                                                                let mut __b = {
                                                                    let mut __b = {
                                                                        let mut __b = {
                                                                            let mut __b = {
                                                                                let mut __b = {
                                                                                    let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(167)).to_usize().unwrap_or(0));
                                                                                    __b.push_str(&AverStr::from("(leninduct "));
                                                                                    __b
                                                                                };
                                                                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                                                                __b
                                                                            };
                                                                            __b.push_str(
                                                                                &AverStr::from(" "),
                                                                            );
                                                                            __b
                                                                        };
                                                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&l)))));
                                                                        __b
                                                                    };
                                                                    __b.push_str(&AverStr::from(
                                                                        " ",
                                                                    ));
                                                                    __b
                                                                };
                                                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&r)))));
                                                                __b
                                                            };
                                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::carryText(&carried)))));
                                                            __b
                                                        };
                                                        __b.push_str(&AverStr::from(" ("));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&general)))));
                                                    __b
                                                };
                                                __b.push_str(&AverStr::from(") ("));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(n)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(" "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(hn),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(ih))));
                            __b
                        };
                        __b.push_str(&AverStr::from(") "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::proofText(&c)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PRing(__l_fields) => {
            let l = __l_fields.0.clone();
            let r = __l_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("(ring "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(&l)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&r)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PLinear(__g_fields) => {
            let g = __g_fields.0.clone();
            let v = __g_fields.1.clone();
            let hs = __g_fields.2.clone();
            let ws = __g_fields.3.clone();
            let cs = __g_fields.4.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(96))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&AverStr::from("(linear "));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(&g)))));
                                                    __b
                                                };
                                                __b.push_str(&AverStr::from(" "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(v)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(" ("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                            &hs,
                                        )),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(") ("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                    &crate::proof_std::aver_generated::kernel::wire::intTexts(&ws),
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(")"));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::cutTexts(&cs)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
    }
}

/// Each cut of a linear step, a space before each.
#[inline(always)]
pub fn cutTexts(
    cs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Cut>,
) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => AverStr::from(""), [c, rest] => { aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(58)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from(" (cut ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.divisor)))); __b }; __b.push_str(&AverStr::from(" (")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&crate::proof_std::aver_generated::kernel::wire::intTexts(&c.weights)))))); __b }; __b.push_str(&AverStr::from("))")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::cutTexts(&rest))))); __b }) })
}

/// Each proof.
#[inline(always)]
pub fn proofTexts(
    ps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Proof>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ps.clone(), [] => aver_rt::AverList::empty(), [p, rest] => aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::wire::proofText(&p), &crate::proof_std::aver_generated::kernel::wire::proofTexts(&rest)))
}

/// Each integer as an atom.
#[inline(always)]
pub fn intTexts(ws @ _: &aver_rt::AverIntList) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ws.clone(), [] => aver_rt::AverList::empty(), [w, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(16)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(w)))); __b }), &crate::proof_std::aver_generated::kernel::wire::intTexts(&rest)) })
}

/// (case CTOR (NAME…) PROOF) for each case.
#[inline(always)]
pub fn splitCaseTexts(
    cs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::SplitCase>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => aver_rt::AverList::empty(), [c, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(59)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("(case ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.ctor)))); __b }; __b.push_str(&AverStr::from(" (")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&c.binders))))); __b }; __b.push_str(&AverStr::from(") ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::proofText(&c.proof))))); __b }; __b.push_str(&AverStr::from(")")); __b }), &crate::proof_std::aver_generated::kernel::wire::splitCaseTexts(&rest)) })
}

/// ` (carry NAME…)` when an induction carries hypotheses, nothing otherwise.
pub fn carryText(ns @ _: &aver_rt::AverList<AverStr>) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ns;
        if __list_subject.is_empty() {
            AverStr::from("")
        } else {
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(25)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from(" (carry "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::spaced(ns)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
    }
}

/// (case (NAME…) (IH…) ((K IH (TERM…) (PROOF…))…) PROOF) for each case.
#[inline(always)]
pub fn inductCaseTexts(
    cs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Case>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => aver_rt::AverList::empty(), [c, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(80)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("(case (")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&c.binders))))); __b }; __b.push_str(&AverStr::from(") (")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&crate::proof_std::aver_generated::kernel::wire::ihTexts(&c.ihs, &c.carry)))))); __b }; __b.push_str(&AverStr::from(") (")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&crate::proof_std::aver_generated::kernel::wire::moreTexts(&c.more)))))); __b }; __b.push_str(&AverStr::from(") ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::proofText(&c.proof))))); __b }; __b.push_str(&AverStr::from(")")); __b }), &crate::proof_std::aver_generated::kernel::wire::inductCaseTexts(&rest)) })
}

/// Each hypothesis name, with the proofs of its carried hypotheses when it has any.
#[inline(always)]
pub fn ihTexts(
    ihs @ _: &aver_rt::AverList<AverStr>,
    carry @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Proof>,
    >,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ihs.clone(), [] => aver_rt::AverList::empty(), [h, rest] => aver_list_match!(carry.clone(), [] => aver_rt::AverList::prepend(h, &crate::proof_std::aver_generated::kernel::wire::ihTexts(&rest, &aver_rt::AverList::empty())), [ps, more] => aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::wire::ihText(h, &ps), &crate::proof_std::aver_generated::kernel::wire::ihTexts(&rest, &more))))
}

/// NAME, or (NAME PROOF…).
pub fn ihText(
    h @ _: AverStr,
    ps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Proof>,
) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ps;
        if __list_subject.is_empty() {
            h
        } else {
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(35)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::spaced(
                            &crate::proof_std::aver_generated::kernel::wire::proofTexts(ps),
                        )),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
    }
}

/// (K IH (TERM…) (PROOF…)) for each further hypothesis.
#[inline(always)]
pub fn moreTexts(
    ms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::More>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ms.clone(), [] => aver_rt::AverList::empty(), [m, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(35)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("(")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(m.call)))); __b }; __b.push_str(&AverStr::from(" ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::ihAtBody(&m.ih))))); __b }; __b.push_str(&AverStr::from(")")); __b }), &crate::proof_std::aver_generated::kernel::wire::moreTexts(&rest)) })
}

/// (IH (TERM…) (PROOF…)) for each hypothesis.
#[inline(always)]
pub fn ihAtTexts(
    ihs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::IhAt>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ihs.clone(), [] => aver_rt::AverList::empty(), [ih, rest] => { aver_rt::AverList::prepend(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("(")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::ihAtBody(&ih))))); __b }; __b.push_str(&AverStr::from(")")); __b }), &crate::proof_std::aver_generated::kernel::wire::ihAtTexts(&rest)) })
}

/// IH (TERM…) (PROOF…), without the outer parentheses.
#[inline(always)]
pub fn ihAtBody(ih @ _: &crate::proof_std::aver_generated::kernel::proof::IhAt) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    aver_rt::AverStr::from({
        let mut __b = {
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(54)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(ih.name),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" ("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::spaced(
                            &crate::proof_std::aver_generated::kernel::wire::termTexts(&ih.at),
                        )),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(") ("));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::spaced(
                    &crate::proof_std::aver_generated::kernel::wire::proofTexts(&ih.carry),
                )),
            )));
            __b
        };
        __b.push_str(&AverStr::from(")"));
        __b
    })
}

/// Synthesized cursor variant of `escaped`. Call sites that hand it `String.chars(s)` walk `s` directly — a byte offset stepped one codepoint at a time — instead of materialising the list of one-character strings.
#[inline(always)]
pub fn escaped__cursor(
    mut __cur_s @ _: AverStr,
    mut __cur_i @ _: aver_rt::AverInt,
    mut acc @ _: AverStr,
) -> AverStr {
    loop {
        crate::proof_std::cancel_checkpoint();
        if aver_rt::str_cursor_end(&__cur_s, (__cur_i).to_usize().unwrap_or(usize::MAX)) {
            return acc;
        } else {
            {
                let c = aver_rt::AverStr::from(aver_rt::str_cursor_head(
                    &__cur_s,
                    (__cur_i).to_usize().unwrap_or(usize::MAX),
                ));
                {
                    let __cur_i1 = aver_rt::AverInt::from_i64(aver_rt::str_cursor_next(
                        &__cur_s,
                        (__cur_i).to_usize().unwrap_or(usize::MAX),
                    ) as i64);
                    {
                        let __tco1 = __cur_i1;
                        let __tco2 =
                            (acc + &crate::proof_std::aver_generated::kernel::wire::escapeOne(c));
                        __cur_i = __tco1;
                        acc = __tco2;
                        continue;
                    }
                }
            }
        }
    }
}
