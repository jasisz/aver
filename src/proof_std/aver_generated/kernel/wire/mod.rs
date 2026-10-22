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
        Box::new(
            crate::proof_std::aver_generated::kernel::proof::Goal__Fields {
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
        ),
        &s.obligation.givens,
    )
}

/// The goal with these givens after its own, in order.
#[inline(always)]
pub fn withGivens(
    mut g0 @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    let mut __hold1: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>;
    let mut gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar> =
        gs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = gs;
            if __list_subject.is_empty() {
                return g0;
            } else {
                if let (Some(g), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 =
                            crate::proof_std::aver_generated::kernel::wire::withGiven(&g0, g);
                        let __tco1 = rest;
                        g0 = __tco0;
                        __hold1 = __tco1;
                        gs = &__hold1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The goal with one more given, also listed among its list, Int or finite givens when its type is one of those: the views a plan reads to choose a split.
pub fn withGiven(
    g0 @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
    g @ _: &crate::proof_std::aver_generated::kernel::term::TypedVar,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    crate::proof_std::cancel_checkpoint();
    let base @ _ = {
        let mut __record0 = g0.clone();
        __record0.obligation = crate::proof_std::aver_generated::kernel::proof::Law {
            givens: aver_rt::AverList::concat(
                &g0.obligation.givens.clone(),
                &aver_rt::AverList::from_vec(vec![g.clone()]),
            ),
            ..g0.obligation.clone()
        };
        __record0
    };
    {
        let __t = (
            g.ty.clone(),
            crate::proof_std::aver_generated::kernel::types::finOf(
                &g.ty,
                &Box::new(
                    crate::proof_std::aver_generated::kernel::types::TyEnv__Fields {
                        vars: aver_rt::AverList::empty(),
                        defs: aver_rt::AverList::empty(),
                        consts: aver_rt::AverList::empty(),
                        sums: g0.sums.clone(),
                        recs: g0.recs.clone(),
                        sigs: aver_rt::AverList::empty(),
                    },
                ),
                &aver_rt::AverList::empty(),
            ),
        );
        match (__t.0.view(), &__t.1) {
            (crate::proof_std::aver_generated::kernel::term::Ty__view::TyList(__e_fields), _) => {
                let e = __e_fields.0.clone();
                Box::new(
                    crate::proof_std::aver_generated::kernel::proof::Goal__Fields {
                        lists: aver_rt::AverList::concat(
                            &base.lists,
                            &aver_rt::AverList::from_vec(vec![g.name.clone()]),
                        ),
                        ..*base
                    },
                )
            }
            (crate::proof_std::aver_generated::kernel::term::Ty__view::TyInt, _) => Box::new(
                crate::proof_std::aver_generated::kernel::proof::Goal__Fields {
                    ints: aver_rt::AverList::concat(
                        &base.ints,
                        &aver_rt::AverList::from_vec(vec![g.name.clone()]),
                    ),
                    ..*base
                },
            ),
            (_, Some(f)) => {
                let f = f.clone();
                Box::new(
                    crate::proof_std::aver_generated::kernel::proof::Goal__Fields {
                        finite: aver_rt::AverList::concat(
                            &base.finite,
                            &aver_rt::AverList::from_vec(vec![
                                crate::proof_std::aver_generated::kernel::proof::Given {
                                    name: g.name.clone(),
                                    fin: f,
                                },
                            ]),
                        ),
                        ..*base
                    },
                )
            }
            _ => base,
        }
    }
}

/// A type in the step grammar; a type the grammar cannot state is _.
pub fn tyText(t @ _: &crate::proof_std::aver_generated::kernel::term::Ty) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyInt => {
                ::aver_rt::str_lit!("int")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyBool => {
                ::aver_rt::str_lit!("bool")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyStr => {
                ::aver_rt::str_lit!("str")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyFloat => {
                ::aver_rt::str_lit!("float")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyUnit => {
                ::aver_rt::str_lit!("unit")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyList(__a_fields) => {
                let a = &__a_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(list "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::tyText(a)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyOption(__a_fields) => {
                let a = &__a_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(25)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(option "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::tyText(a)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVector(__a_fields) => {
                let a = &__a_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(25)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(vector "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::tyText(a)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyResult(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(42)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("(result "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::tyText(a)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::tyText(b)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyMap(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("(map "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::tyText(a)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::tyText(b)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyTuple(__xs_fields) => {
                let xs = &__xs_fields.0;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    ::aver_rt::str_lit!("tuple"),
                    &crate::proof_std::aver_generated::kernel::wire::tyTexts(xs),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyNamed(__n_fields) => {
                let n = __n_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(named "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyRigid(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                ::aver_rt::str_lit!("_")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVar(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                if eq {
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(25)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("(var "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" eq)"));
                        __b
                    })
                } else {
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("(var "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    })
                }
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyAny => {
                ::aver_rt::str_lit!("_")
            }
        }
    }
}

/// Each type in the step grammar.
#[inline(always)]
pub fn tyTexts(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    crate::proof_std::aver_generated::kernel::wire::tyText(t),
                    &crate::proof_std::aver_generated::kernel::wire::tyTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
                __b.push_str(&::aver_rt::str_lit!("proof\n"));
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
                __b.push_str(&::aver_rt::str_lit!("refused\n"));
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
                __b.push_str(&::aver_rt::str_lit!("\""));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::escaped__cursor(
                    s,
                    aver_rt::AverInt::from_i64(0),
                    ::aver_rt::str_lit!(""),
                )),
            )));
            __b
        };
        __b.push_str(&::aver_rt::str_lit!("\""));
        __b
    })
}

/// The characters with quotes, backslashes and line breaks escaped.
#[inline(always)]
pub fn escaped(cs @ _: &aver_rt::AverList<AverStr>, mut acc @ _: AverStr) -> AverStr {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut cs @ _: &aver_rt::AverList<AverStr> = cs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = cs;
            if __list_subject.is_empty() {
                return acc;
            } else {
                if let Some((c, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    {
                        let __tco0 = rest;
                        let __tco1 =
                            (acc + &crate::proof_std::aver_generated::kernel::wire::escapeOne(c));
                        __hold0 = __tco0;
                        cs = &__hold0;
                        acc = __tco1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// One character, escaped when it has to be.
#[inline(always)]
pub fn escapeOne(c @ _: AverStr) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
        if __dispatch_subject == aver_rt::AverInt::from_i64(34) {
            ::aver_rt::str_lit!("\\\"")
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(92) {
                ::aver_rt::str_lit!("\\\\")
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(10) {
                    ::aver_rt::str_lit!("\\n")
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
    (aver_rt::string_join(&xs, &::aver_rt::str_lit!(" "))).into_aver()
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
                        __b.push_str(&::aver_rt::str_lit!("("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(head))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(")"));
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
                                __b.push_str(&::aver_rt::str_lit!("("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(head))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::spaced(xs)),
                    )));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(")"));
                __b
            })
        }
    }
}

/// One term in the step grammar.
pub fn termText(t @ _: &crate::proof_std::aver_generated::kernel::term::Term) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TInt(__i_fields) => {
                let i = __i_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(i "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TBool(__b_fields) => {
                let b = __b_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(b "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(b))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TStr(__s_fields) => {
                let s = __s_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(s "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::quote(s)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TUnit => {
                ::aver_rt::str_lit!("(unit)")
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TVar(__n_fields) => {
                let n = __n_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(v "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::THole => {
                ::aver_rt::str_lit!("(hole)")
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TGet(__o_fields) => {
                let o = &__o_fields.0;
                let f = __o_fields.1.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("(get "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::termText(o)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TCall(__f_fields) => {
                let f = __f_fields.0.clone();
                let xs = &__f_fields.1;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("call "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::termTexts(xs),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TBi(__f_fields) => {
                let f = __f_fields.0.clone();
                let xs = &__f_fields.1;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(19)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("bi "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::termTexts(xs),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TCtor(__c_fields) => {
                let c = __c_fields.0.clone();
                let xs = &__c_fields.1;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("ctor "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c))));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::termTexts(xs),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                let o = __o_fields.0.clone();
                let a = &__o_fields.1;
                let b = &__o_fields.2;
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
                                            __b.push_str(&::aver_rt::str_lit!("(op "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(o)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::termText(a)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(b)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                let a = &__a_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(neg "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(a)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TMatch(__s_fields) => {
                let s = &__s_fields.0;
                let arms = &__s_fields.1;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("match "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(s)),
                        )));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::armTexts(arms),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TParts(__xs_fields) => {
                let xs = &__xs_fields.0;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    ::aver_rt::str_lit!("str"),
                    &crate::proof_std::aver_generated::kernel::wire::termTexts(xs),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TList(__xs_fields) => {
                let xs = &__xs_fields.0;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    ::aver_rt::str_lit!("list"),
                    &crate::proof_std::aver_generated::kernel::wire::termTexts(xs),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TTuple(__xs_fields) => {
                let xs = &__xs_fields.0;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    ::aver_rt::str_lit!("tuple"),
                    &crate::proof_std::aver_generated::kernel::wire::termTexts(xs),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TRec(__ty_fields) => {
                let ty = __ty_fields.0.clone();
                let fs = &__ty_fields.1;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("rec "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(ty))));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::fieldTexts(fs),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TUpd(__ty_fields) => {
                let ty = __ty_fields.0.clone();
                let b = &__ty_fields.1;
                let fs = &__ty_fields.2;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(37)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("upd "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(ty))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(b)),
                        )));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::fieldTexts(fs),
                )
            }
        }
    }
}

/// Each term in the step grammar.
#[inline(always)]
pub fn termTexts(
    xs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = xs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    crate::proof_std::aver_generated::kernel::wire::termText(x),
                    &crate::proof_std::aver_generated::kernel::wire::termTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (FIELD TERM) for each field.
#[inline(always)]
pub fn fieldTexts(
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = fs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(35))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(f.name),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(
                                    &f.value,
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::fieldTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (arm PAT TERM) for each arm.
#[inline(always)]
pub fn armTexts(
    arms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = arms;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(39))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("(arm "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(crate::proof_std::aver_generated::kernel::wire::patText(
                                            &a.pattern,
                                        )),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(
                                    &a.body,
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::armTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// One pattern in the step grammar.
pub fn patText(p @ _: &crate::proof_std::aver_generated::kernel::term::Pat) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match p {
        crate::proof_std::aver_generated::kernel::term::Pat::PWild => {
            ::aver_rt::str_lit!("(pw)")
        }
        crate::proof_std::aver_generated::kernel::term::Pat::PVar(n) => {
            let n = n.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("(pv "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Pat::PLit(t) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("(pl "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(crate::proof_std::aver_generated::kernel::wire::termText(t)),
                )));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(")"));
            __b
        }),
        crate::proof_std::aver_generated::kernel::term::Pat::PNil => {
            ::aver_rt::str_lit!("(pnil)")
        }
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
                                __b.push_str(&::aver_rt::str_lit!("(pcons "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(t))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Pat::PTuple(ps) => {
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("(pt "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::spaced(
                            &crate::proof_std::aver_generated::kernel::wire::patTexts(ps),
                        )),
                    )));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
            let c = c.clone();
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(19)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("pc "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c))));
                    __b
                }),
                ns,
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
    {
        let __list_subject = ps;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    crate::proof_std::aver_generated::kernel::wire::patText(p),
                    &crate::proof_std::aver_generated::kernel::wire::patTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
                __b.push_str(&::aver_rt::str_lit!("("));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::spaced(
                    &crate::proof_std::aver_generated::kernel::wire::bindingTexts(bs),
                )),
            )));
            __b
        };
        __b.push_str(&::aver_rt::str_lit!(")"));
        __b
    })
}

/// (NAME TERM) for each binding.
#[inline(always)]
pub fn bindingTexts(
    bs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = bs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(b), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(35))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(b.name),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(
                                    &b.value,
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::bindingTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// One proof in the step grammar.
pub fn proofText(p @ _: &crate::proof_std::aver_generated::kernel::proof::Proof) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = p;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PRefl(__t_fields) => {
                let t = &__t_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(refl "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(t)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PSymm(__q_fields) => {
                let q = &__q_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(symm "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::proofText(q)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PTrans(__ts_fields) => {
                let ts = &__ts_fields.0;
                let ps = &__ts_fields.1;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("trans ("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                    &crate::proof_std::aver_generated::kernel::wire::termTexts(ts),
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::proofTexts(ps),
                )
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PCongr(__c_fields) => {
                let c = &__c_fields.0;
                let q = &__c_fields.1;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("(congr "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::termText(c)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::proofText(q)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PUnfold(__f_fields) => {
                let f = __f_fields.0.clone();
                let k = __f_fields.1.clone();
                let xs = &__f_fields.2;
                let ys = &__f_fields.3;
                let pre = &__f_fields.4;
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
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(78))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&::aver_rt::str_lit!(
                                                            "unfold "
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(f)),
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&::aver_rt::str_lit!(" "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(k)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" ("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&crate::proof_std::aver_generated::kernel::wire::termTexts(xs))))));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(") ("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                    &crate::proof_std::aver_generated::kernel::wire::termTexts(ys),
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::proofTexts(pre),
                )
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PConst(__n_fields) => {
                let n = __n_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(const "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PArm(__k_fields) => {
                let k = __k_fields.0.clone();
                let ys = &__k_fields.1;
                let t = &__k_fields.2;
                let q = &__k_fields.3;
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
                                                    __b.push_str(&::aver_rt::str_lit!("(arm "));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(&(k)),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" ("));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&crate::proof_std::aver_generated::kernel::wire::termTexts(ys))))));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(") "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::termText(t)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::proofText(q)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PProj(__t_fields) => {
                let t = &__t_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(proj "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(t)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PCell(__t_fields) => {
                let t = &__t_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(cell "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(t)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PHyp(__h_fields) => {
                let h = __h_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("(hyp "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PRule(__id_fields) => {
                let id = __id_fields.0.clone();
                let bs = &__id_fields.1;
                let ps = &__id_fields.2;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(38)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("rule "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(id))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::bindingsText(bs)),
                        )));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::proofTexts(ps),
                )
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PLaw(__k_fields) => {
                let k = __k_fields.0.clone();
                let bs = &__k_fields.1;
                let ps = &__k_fields.2;
                crate::proof_std::aver_generated::kernel::wire::headed(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(37)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("law "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::bindingsText(bs)),
                        )));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::proofTexts(ps),
                )
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PCompute(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("(compute "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::termText(a)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(b)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PCases(__on_fields) => {
                let on = &__on_fields.0;
                let h = __on_fields.1.clone();
                let t = &__on_fields.2;
                let f = &__on_fields.3;
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
                                                    __b.push_str(&::aver_rt::str_lit!("(cases "));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(on)))));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(h)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::proofText(t)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::proofText(f)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PSplit(__on_fields) => {
                let on = &__on_fields.0;
                let h = __on_fields.1.clone();
                let ks = &__on_fields.2;
                let cs = &__on_fields.3;
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
                                                __b.push_str(&::aver_rt::str_lit!("split "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(on)))));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(h),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" ("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                    &crate::proof_std::aver_generated::kernel::wire::termTexts(ks),
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::splitCaseTexts(cs),
                )
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PHave(__n_fields) => {
                let n = __n_fields.0.clone();
                let fact = &__n_fields.1;
                let q = &__n_fields.2;
                let body = &__n_fields.3;
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
                                                    __b.push_str(&::aver_rt::str_lit!("(have "));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(&(n)),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(fact)))));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::proofText(q)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::proofText(body)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PEnum(__v_fields) => {
                let v = __v_fields.0.clone();
                let l = &__v_fields.1;
                let r = &__v_fields.2;
                let cs = &__v_fields.3;
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
                                            __b.push_str(&::aver_rt::str_lit!("enum "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(v)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::termText(l)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(r)),
                        )));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::proofTexts(cs),
                )
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PAbsurd(__q_fields) => {
                let q = &__q_fields.0;
                let l = &__q_fields.1;
                let r = &__q_fields.2;
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
                                            __b.push_str(&::aver_rt::str_lit!("(absurd "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::proofText(q)))));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::termText(l)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(r)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PInduct(__f_fields) => {
                let f = __f_fields.0.clone();
                let xs = &__f_fields.1;
                let l = &__f_fields.2;
                let r = &__f_fields.3;
                let carried = &__f_fields.4;
                let cs = &__f_fields.5;
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
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(92))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&::aver_rt::str_lit!(
                                                            "induct "
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(f)),
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&::aver_rt::str_lit!(" ("));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&crate::proof_std::aver_generated::kernel::wire::termTexts(xs))))));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(") "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(crate::proof_std::aver_generated::kernel::wire::termText(
                                            l,
                                        )),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::termText(r)),
                            )));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::carryText(carried)),
                        )));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::inductCaseTexts(cs),
                )
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PListInduct(
                __v_fields,
            ) => {
                let v = __v_fields.0.clone();
                let l = &__v_fields.1;
                let r = &__v_fields.2;
                let carried = &__v_fields.3;
                let n = &__v_fields.4;
                let h = __v_fields.5.clone();
                let t = __v_fields.6.clone();
                let general = &__v_fields.7;
                let ihs = &__v_fields.8;
                let c = &__v_fields.9;
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
                                                                                            let mut
                                                                                            __b = {
                                                                                                let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(187)).to_usize().unwrap_or(0));
                                                                                                __b.push_str(&::aver_rt::str_lit!("(listinduct "));
                                                                                                __b
                                                                                            };
                                                                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                                                                            __b
                                                                                        };
                                                                                        __b.push_str(&::aver_rt::str_lit!(" "));
                                                                                        __b
                                                                                    };
                                                                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(l)))));
                                                                                    __b
                                                                                };
                                                                                __b.push_str(&::aver_rt::str_lit!(" "));
                                                                                __b
                                                                            };
                                                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(r)))));
                                                                            __b
                                                                        };
                                                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::carryText(carried)))));
                                                                        __b
                                                                    };
                                                                    __b.push_str(
                                                                        &::aver_rt::str_lit!(" "),
                                                                    );
                                                                    __b
                                                                };
                                                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::proofText(n)))));
                                                                __b
                                                            };
                                                            __b.push_str(&::aver_rt::str_lit!(
                                                                " ("
                                                            ));
                                                            __b
                                                        };
                                                        __b.push_str(&aver_rt::AverStr::from(
                                                            aver_rt::aver_display(&(h)),
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(&(t)),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(") ("));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(general)))));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(") ("));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                        &crate::proof_std::aver_generated::kernel::wire::ihAtTexts(
                                            ihs,
                                        ),
                                    )),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(") "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::proofText(c)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PListCases(
                __v_fields,
            ) => {
                let v = __v_fields.0.clone();
                let l = &__v_fields.1;
                let r = &__v_fields.2;
                let n = &__v_fields.3;
                let h = __v_fields.4.clone();
                let t = __v_fields.5.clone();
                let c = &__v_fields.6;
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
                                                                            __b.push_str(&::aver_rt::str_lit!("(listcases "));
                                                                            __b
                                                                        };
                                                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                                                        __b
                                                                    };
                                                                    __b.push_str(
                                                                        &::aver_rt::str_lit!(" "),
                                                                    );
                                                                    __b
                                                                };
                                                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(l)))));
                                                                __b
                                                            };
                                                            __b.push_str(&::aver_rt::str_lit!(" "));
                                                            __b
                                                        };
                                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(r)))));
                                                        __b
                                                    };
                                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::proofText(n)))));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" ("));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(h)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(t))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(") "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::proofText(c)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PLenInduct(
                __v_fields,
            ) => {
                let v = __v_fields.0.clone();
                let l = &__v_fields.1;
                let r = &__v_fields.2;
                let carried = &__v_fields.3;
                let general = &__v_fields.4;
                let n = __v_fields.5.clone();
                let hn = __v_fields.6.clone();
                let ih = __v_fields.7.clone();
                let c = &__v_fields.8;
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
                                                                                        __b.push_str(&::aver_rt::str_lit!("(leninduct "));
                                                                                        __b
                                                                                    };
                                                                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                                                                    __b
                                                                                };
                                                                                __b.push_str(&::aver_rt::str_lit!(" "));
                                                                                __b
                                                                            };
                                                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(l)))));
                                                                            __b
                                                                        };
                                                                        __b.push_str(
                                                                            &::aver_rt::str_lit!(
                                                                                " "
                                                                            ),
                                                                        );
                                                                        __b
                                                                    };
                                                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(r)))));
                                                                    __b
                                                                };
                                                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::carryText(carried)))));
                                                                __b
                                                            };
                                                            __b.push_str(&::aver_rt::str_lit!(
                                                                " ("
                                                            ));
                                                            __b
                                                        };
                                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(general)))));
                                                        __b
                                                    };
                                                    __b.push_str(&::aver_rt::str_lit!(") ("));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(&(n)),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(hn)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(ih))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(") "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::proofText(c)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PRing(__l_fields) => {
                let l = &__l_fields.0;
                let r = &__l_fields.1;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("(ring "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::termText(l)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::termText(r)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PLinear(__g_fields) => {
                let g = &__g_fields.0;
                let v = __g_fields.1.clone();
                let hs = &__g_fields.2;
                let ws = &__g_fields.3;
                let cs = &__g_fields.4;
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
                                                                    (aver_rt::AverInt::from_i64(
                                                                        96,
                                                                    ))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                                );
                                                            __b.push_str(&::aver_rt::str_lit!(
                                                                "(linear "
                                                            ));
                                                            __b
                                                        };
                                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::termText(g)))));
                                                        __b
                                                    };
                                                    __b.push_str(&::aver_rt::str_lit!(" "));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(&(v)),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" ("));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(hs)))));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(") ("));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                        &crate::proof_std::aver_generated::kernel::wire::intTexts(
                                            ws,
                                        ),
                                    )),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(")"));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::wire::cutTexts(cs)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
        }
    }
}

/// Each cut of a linear step, a space before each.
#[inline(always)]
pub fn cutTexts(
    cs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Cut>,
) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            ::aver_rt::str_lit!("")
        } else {
            if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverStr::from({
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
                                        __b.push_str(&::aver_rt::str_lit!(" (cut "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(c.divisor),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" ("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                    &crate::proof_std::aver_generated::kernel::wire::intTexts(
                                        &c.weights,
                                    ),
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!("))"));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::cutTexts(&rest)),
                    )));
                    __b
                })
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Each proof.
#[inline(always)]
pub fn proofTexts(
    ps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Proof>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ps;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    crate::proof_std::aver_generated::kernel::wire::proofText(p),
                    &crate::proof_std::aver_generated::kernel::wire::proofTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Each integer as an atom.
#[inline(always)]
pub fn intTexts(ws @ _: &aver_rt::AverIntList) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ws;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let Some((w, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverList::prepend(
                    aver_rt::AverStr::from({
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(16)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(w))));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::intTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (case CTOR (NAME…) PROOF) for each case.
#[inline(always)]
pub fn splitCaseTexts(
    cs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::SplitCase>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
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
                                                __b.push_str(&::aver_rt::str_lit!("(case "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(c.ctor)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" ("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(crate::proof_std::aver_generated::kernel::wire::spaced(
                                            &c.binders,
                                        )),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(") "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::proofText(
                                    &c.proof,
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::splitCaseTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// ` (carry NAME…)` when an induction carries hypotheses, nothing otherwise.
pub fn carryText(ns @ _: &aver_rt::AverList<AverStr>) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ns;
        if __list_subject.is_empty() {
            ::aver_rt::str_lit!("")
        } else {
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(25)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!(" (carry "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::spaced(ns)),
                    )));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(")"));
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
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    aver_rt::AverStr::from({
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
                                                                (aver_rt::AverInt::from_i64(80))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&::aver_rt::str_lit!(
                                                            "(case ("
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&c.binders)))));
                                                    __b
                                                };
                                                __b.push_str(&::aver_rt::str_lit!(") ("));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&crate::proof_std::aver_generated::kernel::wire::ihTexts(&c.ihs, &c.carry))))));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(") ("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::wire::spaced(&crate::proof_std::aver_generated::kernel::wire::moreTexts(&c.more))))));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(") "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::proofText(
                                    &c.proof,
                                )),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::inductCaseTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
    {
        let __list_subject = ihs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let Some((h, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                {
                    let __list_subject = carry;
                    if __list_subject.is_empty() {
                        aver_rt::AverList::prepend(
                            h,
                            &crate::proof_std::aver_generated::kernel::wire::ihTexts(
                                &rest,
                                &aver_rt::AverList::empty(),
                            ),
                        )
                    } else {
                        if let (Some(ps), Some(more)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            aver_rt::AverList::prepend(
                                crate::proof_std::aver_generated::kernel::wire::ihText(h, ps),
                                &crate::proof_std::aver_generated::kernel::wire::ihTexts(
                                    &rest, &more,
                                ),
                            )
                        } else {
                            panic!("Aver Rust codegen: non-exhaustive list match")
                        }
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
                                __b.push_str(&::aver_rt::str_lit!("("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::spaced(
                            &crate::proof_std::aver_generated::kernel::wire::proofTexts(ps),
                        )),
                    )));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(")"));
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
    {
        let __list_subject = ms;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(m), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(35))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("("));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(m.call),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::ihAtBody(&m.ih)),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::moreTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (IH (TERM…) (PROOF…)) for each hypothesis.
#[inline(always)]
pub fn ihAtTexts(
    ihs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::IhAt>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ihs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(ih), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::ihAtBody(ih)),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    }),
                    &crate::proof_std::aver_generated::kernel::wire::ihAtTexts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
                        __b.push_str(&::aver_rt::str_lit!(" ("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::spaced(
                            &crate::proof_std::aver_generated::kernel::wire::termTexts(&ih.at),
                        )),
                    )));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(") ("));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::spaced(
                    &crate::proof_std::aver_generated::kernel::wire::proofTexts(&ih.carry),
                )),
            )));
            __b
        };
        __b.push_str(&::aver_rt::str_lit!(")"));
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
