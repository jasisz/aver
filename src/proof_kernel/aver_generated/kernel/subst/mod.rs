#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    Capture(aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>),
    CaptureOne(
        crate::proof_kernel::aver_generated::kernel::term::Binding,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
    used @ _: &aver_rt::AverList<AverStr>,
    bound @ _: &aver_rt::AverList<AverStr>,
) -> Result<(), AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::Capture(mut bs @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                aver_list_match!(bs, [] => { return Ok(()) }, [b, rest] => __MutualTco1::CaptureOne(b, rest))
            }
            __MutualTco1::CaptureOne(mut b @ _, mut rest @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                if used.contains(&b.name) {
                    match crate::proof_kernel::aver_generated::kernel::subst::firstShared(
                        crate::proof_kernel::aver_generated::kernel::subst::freeVars(b.value),
                        (*bound).clone(),
                    ) {
                        Some(c @ _) => {
                            return Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(60))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&AverStr::from("substituting "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(b.name)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&AverStr::from(" would capture "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c))));
                                __b
                            }));
                        }
                        None => __MutualTco1::Capture(rest),
                    }
                } else {
                    __MutualTco1::Capture(rest)
                }
            }
        };
    }
}

/// Refuse a binding that is used under the arm and would bring in a bound name.
pub fn capture(
    bs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    used @ _: &aver_rt::AverList<AverStr>,
    bound @ _: &aver_rt::AverList<AverStr>,
) -> Result<(), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::Capture(bs), &used, &bound)
}

/// Check one binding, then the rest.
pub fn captureOne(
    b @ _: crate::proof_kernel::aver_generated::kernel::term::Binding,
    used @ _: &aver_rt::AverList<AverStr>,
    bound @ _: &aver_rt::AverList<AverStr>,
    rest @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<(), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::CaptureOne(b, rest), &used, &bound)
}

/// The term bound to a name, if any.
#[inline(always)]
pub fn lookup(
    mut name @ _: AverStr,
    mut bs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Term> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(bs, [] => { return None; }, [b, rest] => { if (b.name == name) { return Some(b.value); } else { {
            let __tco1 = rest;
            bs = __tco1;
            continue;
        } } })
    }
}

/// The names a pattern binds, wildcards excluded.
pub fn patNames(
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match p {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PVar(n) => {
            let n = n.clone();
            crate::proof_kernel::aver_generated::kernel::subst::dropWild(
                aver_rt::AverList::from_vec(vec![n]),
            )
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            crate::proof_kernel::aver_generated::kernel::subst::dropWild(
                aver_rt::AverList::from_vec(vec![h, t]),
            )
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
            let c = c.clone();
            let ns = ns.clone();
            crate::proof_kernel::aver_generated::kernel::subst::dropWild(ns)
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PTuple(ps) => {
            let ps = ps.clone();
            crate::proof_kernel::aver_generated::kernel::subst::patNamesAll(&ps)
        }
        _ => aver_rt::AverList::empty(),
    }
}

/// Names bound by several patterns.
#[inline(always)]
pub fn patNamesAll(
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Pat>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ps.clone(), [] => aver_rt::AverList::empty(), [p, rest] => aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::subst::patNames(&p), &crate::proof_kernel::aver_generated::kernel::subst::patNamesAll(&rest)))
}

/// Names other than the wildcard.
#[inline(always)]
pub fn dropWild(mut ns @ _: aver_rt::AverList<AverStr>) -> aver_rt::AverList<AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(ns, [] => { return aver_rt::AverList::empty(); }, [n, rest] => { if (&*n == "_") { {
            let __tco0 = rest;
            ns = __tco0;
            continue;
        } } else { return aver_rt::AverList::prepend(n, &crate::proof_kernel::aver_generated::kernel::subst::dropWild(rest)); } })
    }
}

/// Variables of a term not bound by a match arm inside it (with repeats); the hole counts as one.
pub fn freeVars(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> aver_rt::AverList<AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __t = t;
            match __t.view() {
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TVar(__n_fields) => {
                    let n = __n_fields.0.clone();
                    return aver_rt::AverList::from_vec(vec![n]);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::THole => {
                    return aver_rt::AverList::from_vec(vec![AverStr::from("□")]);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TGet(__o_fields) => {
                    let o = __o_fields.0.clone();
                    let f = __o_fields.1.clone();
                    {
                        let __tco0 = o;
                        t = __tco0;
                        continue;
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TCall(
                    __f_fields,
                ) => {
                    let f = __f_fields.0.clone();
                    let xs = __f_fields.1.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(__f_fields) => {
                    let f = __f_fields.0.clone();
                    let xs = __f_fields.1.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                    let o = __o_fields.0.clone();
                    let a = __o_fields.1.clone();
                    let b = __o_fields.2.clone();
                    return aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(a),
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(b),
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                    let a = __a_fields.0.clone();
                    {
                        let __tco0 = a;
                        t = __tco0;
                        continue;
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TCtor(
                    __c_fields,
                ) => {
                    let c = __c_fields.0.clone();
                    let xs = __c_fields.1.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TMatch(
                    __s_fields,
                ) => {
                    let s = __s_fields.0.clone();
                    let arms = __s_fields.1.clone();
                    return aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(s),
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVarsArms(&arms),
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TParts(
                    __xs_fields,
                ) => {
                    let xs = __xs_fields.0.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TList(
                    __xs_fields,
                ) => {
                    let xs = __xs_fields.0.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TTuple(
                    __xs_fields,
                ) => {
                    let xs = __xs_fields.0.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TRec(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let fs = __n_fields.1.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::freeVarsFields(&fs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TUpd(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let b = __n_fields.1.clone();
                    let fs = __n_fields.2.clone();
                    return aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(b),
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVarsFields(&fs),
                    );
                }
                _ => {
                    return aver_rt::AverList::empty();
                }
            }
        }
    }
}

/// Free variables of several terms.
#[inline(always)]
pub fn freeVarsAll(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(xs.clone(), [] => aver_rt::AverList::empty(), [x, rest] => aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::subst::freeVars(x), &crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(&rest)))
}

/// Free variables of record fields.
#[inline(always)]
pub fn freeVarsFields(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => aver_rt::AverList::empty(), [f, rest] => aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::subst::freeVars(f.value), &crate::proof_kernel::aver_generated::kernel::subst::freeVarsFields(&rest)))
}

/// Free variables of match arms, without each arm's own binders.
#[inline(always)]
pub fn freeVarsArms(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(arms.clone(), [] => aver_rt::AverList::empty(), [a, rest] => aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::subst::without(crate::proof_kernel::aver_generated::kernel::subst::freeVars(a.body), crate::proof_kernel::aver_generated::kernel::subst::patNames(&a.pattern)), &crate::proof_kernel::aver_generated::kernel::subst::freeVarsArms(&rest)))
}

/// Names not in the dropped list.
#[inline(always)]
pub fn without(
    mut ns @ _: aver_rt::AverList<AverStr>,
    drop @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<AverStr> {
    let drop @ _ = aver_rt::Shared::new(drop);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(ns, [] => { return aver_rt::AverList::empty(); }, [n, rest] => { if drop.contains(&n) { {
            let __tco0 = rest;
            ns = __tco0;
            continue;
        } } else { return aver_rt::AverList::prepend(n, &crate::proof_kernel::aver_generated::kernel::subst::without(rest, (*drop).clone())); } })
    }
}

/// How many holes a term holds.
pub fn holeCount(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> aver_rt::AverInt {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __t = t;
            match __t.view() {
                crate::proof_kernel::aver_generated::kernel::term::Term__view::THole => {
                    return aver_rt::AverInt::from_i64(1);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TGet(__o_fields) => {
                    let o = __o_fields.0.clone();
                    let f = __o_fields.1.clone();
                    {
                        let __tco0 = o;
                        t = __tco0;
                        continue;
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TCall(
                    __f_fields,
                ) => {
                    let f = __f_fields.0.clone();
                    let xs = __f_fields.1.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCountAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(__f_fields) => {
                    let f = __f_fields.0.clone();
                    let xs = __f_fields.1.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCountAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                    let o = __o_fields.0.clone();
                    let a = __o_fields.1.clone();
                    let b = __o_fields.2.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCount(a)
                        .add(&crate::proof_kernel::aver_generated::kernel::subst::holeCount(b));
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                    let a = __a_fields.0.clone();
                    {
                        let __tco0 = a;
                        t = __tco0;
                        continue;
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TCtor(
                    __c_fields,
                ) => {
                    let c = __c_fields.0.clone();
                    let xs = __c_fields.1.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCountAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TMatch(
                    __s_fields,
                ) => {
                    let s = __s_fields.0.clone();
                    let arms = __s_fields.1.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCount(s).add(
                        &crate::proof_kernel::aver_generated::kernel::subst::holeCountArms(&arms),
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TParts(
                    __xs_fields,
                ) => {
                    let xs = __xs_fields.0.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCountAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TList(
                    __xs_fields,
                ) => {
                    let xs = __xs_fields.0.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCountAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TTuple(
                    __xs_fields,
                ) => {
                    let xs = __xs_fields.0.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCountAll(&xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TRec(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let fs = __n_fields.1.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCountFields(
                        &fs,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TUpd(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let b = __n_fields.1.clone();
                    let fs = __n_fields.2.clone();
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCount(b).add(
                        &crate::proof_kernel::aver_generated::kernel::subst::holeCountFields(&fs),
                    );
                }
                _ => {
                    return aver_rt::AverInt::from_i64(0);
                }
            }
        }
    }
}

/// Holes in several terms.
#[inline(always)]
pub fn holeCountAll(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverInt {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(xs.clone(), [] => aver_rt::AverInt::from_i64(0), [x, rest] => crate::proof_kernel::aver_generated::kernel::subst::holeCount(x).add(&crate::proof_kernel::aver_generated::kernel::subst::holeCountAll(&rest)))
}

/// Holes in record fields.
#[inline(always)]
pub fn holeCountFields(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
) -> aver_rt::AverInt {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => aver_rt::AverInt::from_i64(0), [f, rest] => crate::proof_kernel::aver_generated::kernel::subst::holeCount(f.value).add(&crate::proof_kernel::aver_generated::kernel::subst::holeCountFields(&rest)))
}

/// Holes in match arm bodies.
#[inline(always)]
pub fn holeCountArms(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> aver_rt::AverInt {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(arms.clone(), [] => aver_rt::AverInt::from_i64(0), [a, rest] => crate::proof_kernel::aver_generated::kernel::subst::holeCount(a.body).add(&crate::proof_kernel::aver_generated::kernel::subst::holeCountArms(&rest)))
}

/// Replace free variables (and the hole, bound under the name of the hole) by terms.
pub fn subst(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = &t;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TVar(__n_fields) => {
                let n = __n_fields.0.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::subst::lookup(n, bs.clone())
                        .unwrap_or(t),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::THole => {
                Ok(crate::proof_kernel::aver_generated::kernel::subst::lookup(
                    AverStr::from("□"),
                    bs.clone(),
                )
                .unwrap_or(t))
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TGet(__o_fields) => {
                let o = __o_fields.0.clone();
                let f = __o_fields.1.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TGet(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TGet(
                            crate::proof_kernel::aver_generated::kernel::subst::subst(o, bs)?,
                            f,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TCall(__f_fields) => {
                let f = __f_fields.0.clone();
                let xs = __f_fields.1.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TCall(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TCall(
                            f,
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(&xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(__f_fields) => {
                let f = __f_fields.0.clone();
                let xs = __f_fields.1.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            f,
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(&xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                let o = __o_fields.0.clone();
                let a = __o_fields.1.clone();
                let b = __o_fields.2.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                            o,
                            crate::proof_kernel::aver_generated::kernel::subst::subst(a, bs)?,
                            crate::proof_kernel::aver_generated::kernel::subst::subst(b, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                let a = __a_fields.0.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TNeg(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TNeg(
                            crate::proof_kernel::aver_generated::kernel::subst::subst(a, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TCtor(__c_fields) => {
                let c = __c_fields.0.clone();
                let xs = __c_fields.1.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                            c,
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(&xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TMatch(__s_fields) => {
                let s = __s_fields.0.clone();
                let arms = __s_fields.1.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TMatch(
                            crate::proof_kernel::aver_generated::kernel::subst::subst(s, bs)?,
                            crate::proof_kernel::aver_generated::kernel::subst::substArms(
                                &arms, bs,
                            )?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TParts(__xs_fields) => {
                let xs = __xs_fields.0.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TParts(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TParts(
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(&xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TList(__xs_fields) => {
                let xs = __xs_fields.0.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TList(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TList(
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(&xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TTuple(__xs_fields) => {
                let xs = __xs_fields.0.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TTuple(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TTuple(
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(&xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TRec(__n_fields) => {
                let n = __n_fields.0.clone();
                let fs = __n_fields.1.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TRec(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TRec(
                            n,
                            crate::proof_kernel::aver_generated::kernel::subst::substFields(
                                &fs, bs,
                            )?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TUpd(__n_fields) => {
                let n = __n_fields.0.clone();
                let b = __n_fields.1.clone();
                let fs = __n_fields.2.clone();
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TUpd(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TUpd(
                            n,
                            crate::proof_kernel::aver_generated::kernel::subst::subst(b, bs)?,
                            crate::proof_kernel::aver_generated::kernel::subst::substFields(
                                &fs, bs,
                            )?,
                        ),
                    ),
                )
            }
            _ => Ok(t),
        }
    }
}

/// Substitute in several terms.
#[inline(always)]
pub fn substAll(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(xs.clone(), [] => Ok(aver_rt::AverList::empty()), [x, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::subst::subst(x, bs)?, &crate::proof_kernel::aver_generated::kernel::subst::substAll(&rest, bs)?)))
}

/// Substitute in record fields.
#[inline(always)]
pub fn substFields(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => Ok(aver_rt::AverList::empty()), [f, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::Field { name: f.name, value: crate::proof_kernel::aver_generated::kernel::subst::subst(f.value, bs)? }, &crate::proof_kernel::aver_generated::kernel::subst::substFields(&rest, bs)?)))
}

/// Substitute under each arm, minus the names the arm binds.
#[inline(always)]
pub fn substArms(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(arms.clone(), [] => Ok(aver_rt::AverList::empty()), [a, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::subst::substArm(a, bs)?, &crate::proof_kernel::aver_generated::kernel::subst::substArms(&rest, bs)?)))
}

/// Substitute in one arm, refusing a capture.
pub fn substArm(
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Arm,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Arm, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let bound @ _ = crate::proof_kernel::aver_generated::kernel::subst::patNames(&a.pattern);
    let inner @ _ =
        crate::proof_kernel::aver_generated::kernel::subst::unbind(bs.clone(), bound.clone());
    crate::proof_kernel::aver_generated::kernel::subst::capture(
        inner.clone(),
        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(a.body.clone()),
        &bound,
    )?;
    Ok(crate::proof_kernel::aver_generated::kernel::term::Arm {
        pattern: a.pattern,
        body: crate::proof_kernel::aver_generated::kernel::subst::subst(a.body, &inner)?,
    })
}

/// Drop bindings for names a pattern shadows.
#[inline(always)]
pub fn unbind(
    mut bs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    names @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding> {
    let names @ _ = aver_rt::Shared::new(names);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(bs, [] => { return aver_rt::AverList::empty(); }, [b, rest] => { if names.contains(&b.name) { {
            let __tco0 = rest;
            bs = __tco0;
            continue;
        } } else { return aver_rt::AverList::prepend(b, &crate::proof_kernel::aver_generated::kernel::subst::unbind(rest, (*names).clone())); } })
    }
}

/// The first name also in the other list.
#[inline(always)]
pub fn firstShared(
    mut ns @ _: aver_rt::AverList<AverStr>,
    others @ _: aver_rt::AverList<AverStr>,
) -> Option<AverStr> {
    let others @ _ = aver_rt::Shared::new(others);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(ns, [] => { return None; }, [n, rest] => { if others.contains(&n) { return Some(n); } else { {
            let __tco0 = rest;
            ns = __tco0;
            continue;
        } } })
    }
}
