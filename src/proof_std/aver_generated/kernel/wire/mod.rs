#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

/// The goal a step script states, its proof left out. The compiler wraps the script as (goal (elems (LIST TYPE)…) SCRIPT) to say what each list given holds; a bare script holds no such word. A goal the compiler poses has no hypotheses: the law's `when` stays its premise.
pub fn goal(
    text @ _: AverStr,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __pat0 = crate::proof_std::aver_generated::kernel::sexp::readAll(text)?;
        match __pat0.clone() {
            crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                let __list_subject = __pat1;
                if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    match __pat2 {
                        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat4) => {
                            match &*__pat4 {
                                "goal" => {
                                    let __list_subject = __pat3;
                                    if let Some((__pat5, __pat6)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        match __pat5 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat7) => {
            { let __list_subject = __pat7; if let Some((__pat8, es)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat8 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat9) => {
            match &*__pat9 {
        "elems" => {
            { let __list_subject = __pat6; if let Some((__pat10, __pat11)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat11; if __list_subject.is_empty() { crate::proof_std::aver_generated::kernel::wire::goalOf(&crate::proof_std::aver_generated::kernel::decode::script(&__pat10)?, &crate::proof_std::aver_generated::kernel::wire::listElems(&es)?) } else { crate::proof_std::aver_generated::kernel::wire::goalOf(&crate::proof_std::aver_generated::kernel::decode::script(&__pat0)?, &aver_rt::AverList::empty()) } } } else { crate::proof_std::aver_generated::kernel::wire::goalOf(&crate::proof_std::aver_generated::kernel::decode::script(&__pat0)?, &aver_rt::AverList::empty()) } }
        },
        _ => {
            crate::proof_std::aver_generated::kernel::wire::goalOf(&crate::proof_std::aver_generated::kernel::decode::script(&__pat0)?, &aver_rt::AverList::empty())
        }
    }
        },
        _ => {
            crate::proof_std::aver_generated::kernel::wire::goalOf(&crate::proof_std::aver_generated::kernel::decode::script(&__pat0)?, &aver_rt::AverList::empty())
        }
    } } else { crate::proof_std::aver_generated::kernel::wire::goalOf(&crate::proof_std::aver_generated::kernel::decode::script(&__pat0)?, &aver_rt::AverList::empty()) } }
        },
        _ => {
            crate::proof_std::aver_generated::kernel::wire::goalOf(&crate::proof_std::aver_generated::kernel::decode::script(&__pat0)?, &aver_rt::AverList::empty())
        }
    }
                                    } else {
                                        crate::proof_std::aver_generated::kernel::wire::goalOf(&crate::proof_std::aver_generated::kernel::decode::script(&__pat0)?, &aver_rt::AverList::empty())
                                    }
                                }
                                _ => crate::proof_std::aver_generated::kernel::wire::goalOf(
                                    &crate::proof_std::aver_generated::kernel::decode::script(
                                        &__pat0,
                                    )?,
                                    &aver_rt::AverList::empty(),
                                ),
                            }
                        }
                        _ => crate::proof_std::aver_generated::kernel::wire::goalOf(
                            &crate::proof_std::aver_generated::kernel::decode::script(&__pat0)?,
                            &aver_rt::AverList::empty(),
                        ),
                    }
                } else {
                    crate::proof_std::aver_generated::kernel::wire::goalOf(
                        &crate::proof_std::aver_generated::kernel::decode::script(&__pat0)?,
                        &aver_rt::AverList::empty(),
                    )
                }
            }
            _ => crate::proof_std::aver_generated::kernel::wire::goalOf(
                &crate::proof_std::aver_generated::kernel::decode::script(&__pat0)?,
                &aver_rt::AverList::empty(),
            ),
        }
    }
}

/// The goal of a read script: everything but its proof, no hypotheses yet.
pub fn goalOf(
    s @ _: &crate::proof_std::aver_generated::kernel::proof::Script,
    elems @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::ListElem>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    Ok(crate::proof_std::aver_generated::kernel::proof::Goal {
        obligation: s.obligation.clone(),
        finite: s.finite.clone(),
        lists: s.lists.clone(),
        ints: s.ints.clone(),
        defs: s.defs.clone(),
        consts: s.consts.clone(),
        sums: s.sums.clone(),
        laws: s.laws.clone(),
        facts: s.facts.clone(),
        hyps: aver_rt::AverList::empty(),
        listElems: elems.clone(),
    })
}

/// (LIST TYPE)…: what each list given holds.
#[inline(always)]
pub fn listElems(
    es @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::ListElem>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(es.clone(), [] => Ok(aver_rt::AverList::empty()), [__pat0, rest] => { match __pat0 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
            { let __list_subject = __pat1; if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat2 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            { let __list_subject = __pat3; if let Some((t, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat4; if __list_subject.is_empty() { Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::proof::ListElem { list: n, elem: crate::proof_std::aver_generated::kernel::wire::elemTy(&t)? }, &crate::proof_std::aver_generated::kernel::wire::listElems(&rest)?)) } else { Err(AverStr::from("malformed list element type")) } } } else { Err(AverStr::from("malformed list element type")) } }
        },
        _ => {
            Err(AverStr::from("malformed list element type"))
        }
    } } else { Err(AverStr::from("malformed list element type")) } }
        },
        _ => {
            Err(AverStr::from("malformed list element type"))
        }
    } })
}

/// An element type: (tint), (tlist TYPE), (tother), or a finite type in the kernel's grammar.
pub fn elemTy(
    s @ _: &crate::proof_std::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match s.clone() {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __dispatch_subject = __pat3;
                        if &*__dispatch_subject == "tint" {
                            {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyInt)
                                } else {
                                    Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyFin(
                                        crate::proof_std::aver_generated::kernel::decode::fin(s)?,
                                    ))
                                }
                            }
                        } else {
                            if &*__dispatch_subject == "tother" {
                                {
                                    let __list_subject = __pat2;
                                    if __list_subject.is_empty() {
                                        Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyOther)
                                    } else {
                                        Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyFin(crate::proof_std::aver_generated::kernel::decode::fin(s)?))
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "tlist" {
                                    {
                                        let __list_subject = __pat2;
                                        if let Some((t, __pat4)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if __list_subject.is_empty() {
                                                    Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyList(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::wire::elemTy(&t)?)))
                                                } else {
                                                    Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyFin(crate::proof_std::aver_generated::kernel::decode::fin(s)?))
                                                }
                                            }
                                        } else {
                                            Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyFin(crate::proof_std::aver_generated::kernel::decode::fin(s)?))
                                        }
                                    }
                                } else {
                                    Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyFin(
                                        crate::proof_std::aver_generated::kernel::decode::fin(s)?,
                                    ))
                                }
                            }
                        }
                    }
                    _ => Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyFin(
                        crate::proof_std::aver_generated::kernel::decode::fin(s)?,
                    )),
                }
            } else {
                Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyFin(
                    crate::proof_std::aver_generated::kernel::decode::fin(s)?,
                ))
            }
        }
        _ => Ok(crate::proof_std::aver_generated::kernel::proof::Ty::TyFin(
            crate::proof_std::aver_generated::kernel::decode::fin(s)?,
        )),
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
    match t.clone() {
        crate::proof_std::aver_generated::kernel::term::Term::TInt(i) => aver_rt::AverStr::from({
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
        }),
        crate::proof_std::aver_generated::kernel::term::Term::TBool(b) => aver_rt::AverStr::from({
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
        }),
        crate::proof_std::aver_generated::kernel::term::Term::TStr(s) => aver_rt::AverStr::from({
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
        }),
        crate::proof_std::aver_generated::kernel::term::Term::TUnit => AverStr::from("(unit)"),
        crate::proof_std::aver_generated::kernel::term::Term::TVar(n) => aver_rt::AverStr::from({
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
        }),
        crate::proof_std::aver_generated::kernel::term::Term::THole => AverStr::from("(hole)"),
        crate::proof_std::aver_generated::kernel::term::Term::TGet(o, f) => {
            let o = (*o).clone();
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
        crate::proof_std::aver_generated::kernel::term::Term::TCall(f, xs) => {
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
        crate::proof_std::aver_generated::kernel::term::Term::TBi(f, xs) => {
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
        crate::proof_std::aver_generated::kernel::term::Term::TCtor(c, xs) => {
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
        crate::proof_std::aver_generated::kernel::term::Term::TOp(o, a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
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
        crate::proof_std::aver_generated::kernel::term::Term::TNeg(a) => {
            let a = (*a).clone();
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
        crate::proof_std::aver_generated::kernel::term::Term::TMatch(s, arms) => {
            let s = (*s).clone();
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
        crate::proof_std::aver_generated::kernel::term::Term::TParts(xs) => {
            crate::proof_std::aver_generated::kernel::wire::headed(
                AverStr::from("str"),
                &crate::proof_std::aver_generated::kernel::wire::termTexts(&xs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TList(xs) => {
            crate::proof_std::aver_generated::kernel::wire::headed(
                AverStr::from("list"),
                &crate::proof_std::aver_generated::kernel::wire::termTexts(&xs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TTuple(xs) => {
            crate::proof_std::aver_generated::kernel::wire::headed(
                AverStr::from("tuple"),
                &crate::proof_std::aver_generated::kernel::wire::termTexts(&xs),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TRec(ty, fs) => {
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
        crate::proof_std::aver_generated::kernel::term::Term::TUpd(ty, b, fs) => {
            let b = (*b).clone();
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
    match p.clone() {
        crate::proof_std::aver_generated::kernel::term::Pat::PWild => AverStr::from("(pw)"),
        crate::proof_std::aver_generated::kernel::term::Pat::PVar(n) => aver_rt::AverStr::from({
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
        }),
        crate::proof_std::aver_generated::kernel::term::Pat::PLit(t) => aver_rt::AverStr::from({
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
        }),
        crate::proof_std::aver_generated::kernel::term::Pat::PNil => AverStr::from("(pnil)"),
        crate::proof_std::aver_generated::kernel::term::Pat::PCons(h, t) => {
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
    match p.clone() {
        crate::proof_std::aver_generated::kernel::proof::Proof::PRefl(t) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PSymm(q) => {
            let q = (*q).clone();
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PTrans(ts, ps) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(c, q) => {
            let q = (*q).clone();
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PUnfold(f, k, xs, ys, pre) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PConst(n) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PArm(k, ys, t, q) => {
            let q = (*q).clone();
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PProj(t) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PCell(t) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(h) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PRule(id, bs, ps) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PLaw(k, bs, ps) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(a, b) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PCases(on, h, t, f) => {
            let t = (*t).clone();
            let f = (*f).clone();
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PSplit(f, xs, on, h, cs) => {
            crate::proof_std::aver_generated::kernel::wire::headed(
                aver_rt::AverStr::from({
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
                                                __b.push_str(&AverStr::from("split "));
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
                                &(crate::proof_std::aver_generated::kernel::wire::termText(&on)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                    __b
                }),
                &crate::proof_std::aver_generated::kernel::wire::splitCaseTexts(&cs),
            )
        }
        crate::proof_std::aver_generated::kernel::proof::Proof::PHave(n, fact, q, body) => {
            let q = (*q).clone();
            let body = (*body).clone();
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PEnum(v, l, r, cs) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PAbsurd(q, l, r) => {
            let q = (*q).clone();
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PInduct(
            f,
            xs,
            l,
            r,
            carried,
            cs,
        ) => crate::proof_std::aver_generated::kernel::wire::headed(
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
        ),
        crate::proof_std::aver_generated::kernel::proof::Proof::PListInduct(
            v,
            l,
            r,
            n,
            h,
            t,
            general,
            ihs,
            c,
        ) => {
            let n = (*n).clone();
            let c = (*c).clone();
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
                                                                                        let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(171)).to_usize().unwrap_or(0));
                                                                                        __b.push_str(&AverStr::from("(listinduct "));
                                                                                        __b
                                                                                    };
                                                                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                                                                    __b
                                                                                };
                                                                                __b.push_str(
                                                                                    &AverStr::from(
                                                                                        " ",
                                                                                    ),
                                                                                );
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PListCases(v, l, r, n, h, t, c) => {
            let n = (*n).clone();
            let c = (*c).clone();
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PRing(l, r) => {
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
        crate::proof_std::aver_generated::kernel::proof::Proof::PLinear(g, v, hs, ws) => {
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
                                                    (aver_rt::AverInt::from_i64(80))
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
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(v),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" ("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::wire::spaced(&hs)),
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
                __b.push_str(&AverStr::from("))"));
                __b
            })
        }
    }
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
