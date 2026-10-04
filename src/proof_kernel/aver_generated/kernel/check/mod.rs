#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Env {
    pub defs: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    pub consts: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    pub laws: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
    pub hyps: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    pub finite: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Given>,
}

impl PartialOrd for Env {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Env {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.consts.cmp(&other.consts))
            .then_with(|| self.defs.cmp(&other.defs))
            .then_with(|| self.finite.cmp(&other.finite))
            .then_with(|| self.hyps.cmp(&other.hyps))
            .then_with(|| self.laws.cmp(&other.laws))
    }
}

impl aver_rt::AverDisplay for Env {
    fn aver_display(&self) -> String {
        format!(
            "Env({})",
            vec![
                format!("defs: {}", self.defs.aver_display_inner()),
                format!("consts: {}", self.consts.aver_display_inner()),
                format!("laws: {}", self.laws.aver_display_inner()),
                format!("hyps: {}", self.hyps.aver_display_inner()),
                format!("finite: {}", self.finite.aver_display_inner())
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
    EnumCases(
        AverStr,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
        AverStr,
        aver_rt::AverInt,
    ),
    EnumCase(
        AverStr,
        crate::proof_kernel::aver_generated::kernel::term::Term,
        crate::proof_kernel::aver_generated::kernel::proof::Proof,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
        AverStr,
        aver_rt::AverInt,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    env @ _: &Env,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::EnumCases(
                mut v @ _,
                mut vals @ _,
                mut cs @ _,
                mut path @ _,
                mut i @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                {
                    let __int_match_subject = (vals, cs);
                    let (__lit0, __lit1) = &__int_match_subject;
                    if (*__lit0).is_empty() && (*__lit1).is_empty() {
                        return Ok((*claim).clone());
                    } else if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
                        let Some((x, xs)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        let Some((c, rest)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        __MutualTco1::EnumCase(v, x, c, xs, rest, path, i)
                    } else {
                        return crate::proof_kernel::aver_generated::kernel::check::refuse(
                            path,
                            aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(53)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&AverStr::from(
                                        "the cases do not match the values of ",
                                    ));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                __b
                            }),
                        );
                    }
                }
            }
            __MutualTco1::EnumCase(
                mut v @ _,
                mut x @ _,
                mut c @ _,
                mut xs @ _,
                mut rest @ _,
                mut path @ _,
                mut i @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                let at @ _ = aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::term::bind(v.clone(), &x),
                ]);
                let here @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(38)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                            __b
                        };
                        __b.push_str(&AverStr::from("/enum."));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
                    __b
                });
                let got @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
                    &c,
                    &crate::proof_kernel::aver_generated::kernel::check::withHyps(
                        &*env,
                        &crate::proof_kernel::aver_generated::kernel::check::substHyps(
                            &env.hyps,
                            &at,
                            path.clone(),
                        )?,
                    ),
                    here.clone(),
                )?;
                let want @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(
                    &*claim,
                    &at,
                    path.clone(),
                )?;
                if (got == want) {
                    __MutualTco1::EnumCases(
                        v,
                        xs,
                        rest,
                        path,
                        i.add(&aver_rt::AverInt::from_i64(1)),
                    )
                } else {
                    return crate::proof_kernel::aver_generated::kernel::check::refuse(
                        here,
                        AverStr::from("the case proves a different equation"),
                    );
                }
            }
        };
    }
}

/// One case per value, in order, and nothing else.
pub fn enumCases(
    v @ _: AverStr,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vals @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
    i @ _: aver_rt::AverInt,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EnumCases(v, vals, cs, path, i), &claim, &env)
}

/// The claim at one value, under the hypotheses at that value.
pub fn enumCase(
    v @ _: AverStr,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    x @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    c @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    xs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    rest @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
    i @ _: aver_rt::AverInt,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EnumCase(v, x, c, xs, rest, path, i),
        &claim,
        &env,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    PremisesHold(
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
        AverStr,
        aver_rt::AverInt,
    ),
    PremiseHolds(
        crate::proof_kernel::aver_generated::kernel::term::Eqn,
        crate::proof_kernel::aver_generated::kernel::proof::Proof,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
        AverStr,
        aver_rt::AverInt,
    ),
}

fn __mutual_tco_trampoline_2(
    mut __state: __MutualTco2,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
) -> Result<(), AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::PremisesHold(mut wanted @ _, mut ps @ _, mut path @ _, mut i @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                {
                    let __int_match_subject = (wanted, ps);
                    let (__lit0, __lit1) = &__int_match_subject;
                    if (*__lit0).is_empty() && (*__lit1).is_empty() {
                        return Ok(());
                    } else if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
                        let Some((w, ws)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        let Some((p, rest)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        __MutualTco2::PremiseHolds(w, p, ws, rest, path, i)
                    } else {
                        return Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&AverStr::from("step "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&AverStr::from(": wrong number of premises"));
                            __b
                        }));
                    }
                }
            }
            __MutualTco2::PremiseHolds(
                mut w @ _,
                mut p @ _,
                mut ws @ _,
                mut rest @ _,
                mut path @ _,
                mut i @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                let want @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(
                    &w,
                    &*bs,
                    path.clone(),
                )?;
                let got @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
                    &p,
                    &*env,
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&AverStr::from("/premise."));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
                        __b
                    }),
                )?;
                if (got == want) {
                    __MutualTco2::PremisesHold(
                        ws,
                        rest,
                        path,
                        i.add(&aver_rt::AverInt::from_i64(1)),
                    )
                } else {
                    return Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(62))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("step "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(path),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(": premise "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" does not match"));
                        __b
                    }));
                }
            }
        };
    }
}

/// Each premise proof proves its premise at the substitution.
pub fn premisesHold(
    wanted @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    ps @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
    path @ _: AverStr,
    i @ _: aver_rt::AverInt,
) -> Result<(), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::PremisesHold(wanted, ps, path, i), &bs, &env)
}

/// One premise, then the rest.
pub fn premiseHolds(
    w @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    p @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    ws @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    rest @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
    path @ _: AverStr,
    i @ _: aver_rt::AverInt,
) -> Result<(), AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::PremiseHolds(w, p, ws, rest, path, i),
        &bs,
        &env,
    )
}

/// No definitions, laws or hypotheses.
pub fn emptyEnv() -> Env {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: aver_rt::AverList::empty(),
        consts: aver_rt::AverList::empty(),
        laws: aver_rt::AverList::empty(),
        hyps: aver_rt::AverList::empty(),
        finite: aver_rt::AverList::empty(),
    }
}

/// A refusal that names the step.
pub fn refuse(
    path @ _: AverStr,
    why @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    Err(aver_rt::AverStr::from({
        let mut __b = {
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from(": "));
            __b
        };
        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
        __b
    }))
}

/// The equation a step proves, or the refusal of the first wrong step under it.
pub fn conclude(
    p @ _: &crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match p.clone() {
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PRefl(t) => {
            Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                lhs: t.clone(),
                rhs: t,
            })
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PSymm(q) => {
            let q = (*q).clone();
            crate::proof_kernel::aver_generated::kernel::check::flip(
                &crate::proof_kernel::aver_generated::kernel::check::conclude(
                    &q,
                    env,
                    (path + &AverStr::from("/symm")),
                )?,
            )
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PTrans(ts, ps) => {
            crate::proof_kernel::aver_generated::kernel::check::trans(&ts, &ps, env, path)
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PCongr(c, q) => {
            let q = (*q).clone();
            crate::proof_kernel::aver_generated::kernel::check::congr(&c, &q, env, path)
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PUnfold(f, k, xs, ys, pre) => {
            crate::proof_kernel::aver_generated::kernel::check::unfold(
                f, k, &xs, &ys, &pre, env, path,
            )
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PConst(n) => {
            crate::proof_kernel::aver_generated::kernel::check::constant(
                n,
                env.consts.clone(),
                path,
            )
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PArm(k, ys, t, q) => {
            let q = (*q).clone();
            crate::proof_kernel::aver_generated::kernel::check::armStep(k, &ys, &t, &q, env, path)
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PProj(t) => {
            crate::proof_kernel::aver_generated::kernel::check::proj(t, path)
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PHyp(h) => {
            crate::proof_kernel::aver_generated::kernel::check::hyp(h, env.hyps.clone(), path)
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PRule(id, bs, ps) => {
            crate::proof_kernel::aver_generated::kernel::check::rule(id, &bs, &ps, env, path)
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PLaw(k, bs, ps) => {
            crate::proof_kernel::aver_generated::kernel::check::lawStep(k, &bs, &ps, env, path)
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PCompute(a, b) => {
            crate::proof_kernel::aver_generated::kernel::check::compute(&a, &b, path)
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PCases(on, h, t, f) => {
            let t = (*t).clone();
            let f = (*f).clone();
            crate::proof_kernel::aver_generated::kernel::check::cases(&on, h, &t, &f, env, path)
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PEnum(v, l, r, cs) => {
            crate::proof_kernel::aver_generated::kernel::check::enumStep(v, &l, &r, &cs, env, path)
        }
        crate::proof_kernel::aver_generated::kernel::proof::Proof::PAbsurd(q, l, r) => {
            let q = (*q).clone();
            crate::proof_kernel::aver_generated::kernel::check::absurd(
                &crate::proof_kernel::aver_generated::kernel::check::conclude(
                    &q,
                    env,
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                            __b
                        };
                        __b.push_str(&AverStr::from("/absurd"));
                        __b
                    }),
                )?,
                &l,
                &r,
                path,
            )
        }
    }
}

/// Whether a result is a refusal that begins with the step it names.
#[inline(always)]
pub fn namesStep(
    r @ _: &Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr>,
    path @ _: AverStr,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match r.clone() {
        Ok(e @ _) => false,
        Err(why @ _) => why.starts_with(&*aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from(": "));
            __b
        })),
    }
}

/// Symmetry.
pub fn flip(
    e @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
        lhs: e.rhs.clone(),
        rhs: e.lhs.clone(),
    })
}

/// A chain t0 = t1 = … = tn, each link proved by its step.
#[inline(always)]
pub fn trans(
    ts @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (aver_rt::AverInt::from_i64(ts.len() as i64)
        == aver_rt::AverInt::from_i64(ps.len() as i64).add(&aver_rt::AverInt::from_i64(1)))
    {
        crate::proof_kernel::aver_generated::kernel::check::transFrom(
            ts,
            ps,
            env,
            path,
            aver_rt::AverInt::from_i64(0),
        )
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("trans needs one more term than steps"),
        )
    }
}

/// Check link i and those after it; the result spans the whole chain.
pub fn transFrom(
    ts @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
    i @ _: aver_rt::AverInt,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (ts.clone(), ps.clone());
        {
            let __list_subject = __pat0;
            if let Some((a, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                aver_list_match!(__pat2, [] => { { let __list_subject = __pat1; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: a.clone(), rhs: a }) } else { crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("trans needs at least one step")) } } }, [b, moreTerms] => { { let __list_subject = __pat1; if let Some((p, moreSteps)) = aver_rt::list_uncons_cloned(&__list_subject) { crate::proof_kernel::aver_generated::kernel::check::transLink(&a, &b.clone(), &p, &aver_rt::AverList::prepend(b, &moreTerms), &moreSteps, env, path, i) } else { crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("trans needs at least one step")) } } })
            } else {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    AverStr::from("trans needs at least one step"),
                )
            }
        }
    }
}

/// One link of a chain, then the rest.
pub fn transLink(
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    b @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    p @ _: &crate::proof_kernel::aver_generated::kernel::proof::Proof,
    rest @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    steps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
    i @ _: aver_rt::AverInt,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let here @ _ = aver_rt::AverStr::from({
        let mut __b = {
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from("/trans."));
            __b
        };
        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
        __b
    });
    let e @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(p, env, here.clone())?;
    if (e
        == crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: a.clone(),
            rhs: b.clone(),
        })
    {
        crate::proof_kernel::aver_generated::kernel::check::joinRest(
            a,
            &crate::proof_kernel::aver_generated::kernel::check::transFrom(
                rest,
                steps,
                env,
                path,
                i.add(&aver_rt::AverInt::from_i64(1)),
            )?,
        )
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            here,
            AverStr::from("the step does not join the written terms"),
        )
    }
}

/// The chain from a to the end of the rest.
pub fn joinRest(
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    rest @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
        lhs: a.clone(),
        rhs: rest.rhs.clone(),
    })
}

/// From a = b, c[a] = c[b]; the context holds exactly one hole.
#[inline(always)]
pub fn congr(
    c @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    q @ _: &crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (crate::proof_kernel::aver_generated::kernel::subst::holeCount(c.clone())
        == aver_rt::AverInt::from_i64(1))
    {
        crate::proof_kernel::aver_generated::kernel::check::plugBoth(
            c,
            &crate::proof_kernel::aver_generated::kernel::check::conclude(
                q,
                env,
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&AverStr::from("/congr"));
                    __b
                }),
            )?,
            path,
        )
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("a congruence context needs exactly one hole"),
        )
    }
}

/// Put both sides of an equation into the hole.
pub fn plugBoth(
    c @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    e @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        crate::proof_kernel::aver_generated::kernel::subst::subst(
            c,
            &aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::term::bind(AverStr::from("□"), &e.lhs),
            ]),
        ),
        crate::proof_kernel::aver_generated::kernel::subst::subst(
            c,
            &aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::term::bind(AverStr::from("□"), &e.rhs),
            ]),
        ),
    ) {
        (Ok(l), Ok(r)) => {
            Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r })
        }
        (Err(why), _) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
        (_, Err(why)) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
    }
}

/// A record literal's field.
pub fn proj(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TGet(__pat0, f) => {
            let __pat0 = (*__pat0).clone();
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::term::Term::TRec(ty, fs) => {
                    crate::proof_kernel::aver_generated::kernel::check::projField(t, fs, f, path)
                }
                _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    AverStr::from("a projection must read a field of a record literal"),
                ),
            }
        }
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("a projection must read a field of a record literal"),
        ),
    }
}

/// Find the field among the literal's fields.
#[inline(always)]
pub fn projField(
    t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut fs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
    mut f @ _: AverStr,
    mut path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    let t @ _ = std::sync::Arc::new(t);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(fs, [] => { return crate::proof_kernel::aver_generated::kernel::check::refuse(path, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("the record has no field ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f)))); __b })); }, [x, rest] => { if (x.name == f) { return Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: (*t).clone(), rhs: x.value }); } else { {
            let __tco1 = rest;
            fs = __tco1;
            continue;
        } } })
    }
}

/// A hypothesis in scope; the innermost one of a name wins.
#[inline(always)]
pub fn hyp(
    mut h @ _: AverStr,
    mut hs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    mut path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(hs, [] => { return crate::proof_kernel::aver_generated::kernel::check::refuse(path, aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("hypothesis ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h)))); __b }; __b.push_str(&AverStr::from(" is not in scope")); __b })); }, [x, rest] => { if (x.name == h) { return Ok(x.eqn); } else { {
            let __tco1 = rest;
            hs = __tco1;
            continue;
        } } })
    }
}

/// Two closed terms with the same value.
pub fn compute(
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    b @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a),
        crate::proof_kernel::aver_generated::kernel::eval::evalClosed(b),
    ) {
        (Some(x), Some(y)) => {
            if (x == y) {
                Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                    lhs: a.clone(),
                    rhs: b.clone(),
                })
            } else {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    AverStr::from("the two sides compute to different values"),
                )
            }
        }
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("compute needs closed terms"),
        ),
    }
}

/// Both branches of a Bool split must prove the same equation.
pub fn cases(
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
    t @ _: &crate::proof_kernel::aver_generated::kernel::proof::Proof,
    f @ _: &crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let whenTrue @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
        t,
        &crate::proof_kernel::aver_generated::kernel::check::withHyp(env, h.clone(), on, true),
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(27)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from("/cases.true"));
            __b
        }),
    )?;
    let whenFalse @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
        f,
        &crate::proof_kernel::aver_generated::kernel::check::withHyp(env, h, on, false),
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(28)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from("/cases.false"));
            __b
        }),
    )?;
    if (whenTrue == whenFalse) {
        Ok(whenTrue)
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the two branches prove different equations"),
        )
    }
}

/// The environment with one more hypothesis in front.
pub fn withHyp(
    env @ _: &Env,
    h @ _: AverStr,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    value @ _: bool,
) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: env.defs.clone(),
        consts: env.consts.clone(),
        laws: env.laws.clone(),
        hyps: aver_rt::AverList::prepend(
            crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                name: h,
                eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
                    lhs: on.clone(),
                    rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(value),
                },
            },
            &env.hyps.clone(),
        ),
        finite: env.finite.clone(),
    }
}

/// Any equation, once true has been shown equal to false: the case cannot happen.
pub fn absurd(
    e @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    l @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    r @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (e.lhs.clone(), e.rhs.clone()) {
        (
            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(a),
            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(b),
        ) => {
            if (a == b) {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    AverStr::from("absurd needs true equal to false"),
                )
            } else {
                Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                    lhs: l.clone(),
                    rhs: r.clone(),
                })
            }
        }
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("absurd needs true equal to false"),
        ),
    }
}

/// A split of a given of finite type into all its values: case i proves the claim at value i.
#[inline(always)]
pub fn enumStep(
    v @ _: AverStr,
    l @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    r @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::check::findGiven(
        v.clone(),
        env.finite.clone(),
    ) {
        None => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(46)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                    __b
                };
                __b.push_str(&AverStr::from(" is not a given of finite type"));
                __b
            }),
        ),
        Some(f @ _) => crate::proof_kernel::aver_generated::kernel::check::enumCases(
            v,
            &crate::proof_kernel::aver_generated::kernel::term::Eqn {
                lhs: l.clone(),
                rhs: r.clone(),
            },
            crate::proof_kernel::aver_generated::kernel::finite::values(&f),
            cs.clone(),
            env,
            path,
            aver_rt::AverInt::from_i64(0),
        ),
    }
}

/// The finite type of a given.
#[inline(always)]
pub fn findGiven(
    mut v @ _: AverStr,
    mut gs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Given>,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Fin> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(gs, [] => { return None; }, [g, rest] => { if (g.name == v) { return Some(g.fin); } else { {
            let __tco1 = rest;
            gs = __tco1;
            continue;
        } } })
    }
}

/// Every hypothesis at a substitution.
#[inline(always)]
pub fn substHyps(
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    at @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => Ok(aver_rt::AverList::empty()), [h, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: h.name, eqn: crate::proof_kernel::aver_generated::kernel::check::instantiate(&h.eqn, at, path.clone())? }, &crate::proof_kernel::aver_generated::kernel::check::substHyps(&rest, at, path)?)))
}

/// The environment with its hypotheses replaced.
pub fn withHyps(
    env @ _: &Env,
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: env.defs.clone(),
        consts: env.consts.clone(),
        laws: env.laws.clone(),
        hyps: hs.clone(),
        finite: env.finite.clone(),
    }
}

/// A definition by name.
#[inline(always)]
pub fn findDef(
    mut name @ _: AverStr,
    mut ds @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Option<crate::proof_kernel::aver_generated::kernel::proof::Def> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(ds, [] => { return None; }, [d, rest] => { if (d.name == name) { return Some(d); } else { {
            let __tco1 = rest;
            ds = __tco1;
            continue;
        } } })
    }
}

/// A module-level binding's value: the variable that reads it equals the value the script carries.
#[inline(always)]
pub fn constant(
    mut n @ _: AverStr,
    mut cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    mut path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(cs, [] => { return crate::proof_kernel::aver_generated::kernel::check::refuse(path, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("no module-level binding ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n)))); __b })); }, [c, rest] => { if (c.name == n) { return Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: crate::proof_kernel::aver_generated::kernel::term::Term::TVar(n), rhs: c.value }); } else { {
            let __tco1 = rest;
            cs = __tco1;
            continue;
        } } })
    }
}

/// Pair names with terms, as far as both go.
pub fn zipBind(
    names @ _: &aver_rt::AverList<AverStr>,
    values @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (names.clone(), values.clone());
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((n, ns)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((v, vs)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            aver_rt::AverList::prepend(
                crate::proof_kernel::aver_generated::kernel::term::bind(n, &v),
                &crate::proof_kernel::aver_generated::kernel::check::zipBind(&ns, &vs),
            )
        } else {
            aver_rt::AverList::empty()
        }
    }
}

/// Equation k of a definition at explicit arguments: 0 is the whole body, k is arm k of its match.
#[inline(always)]
pub fn unfold(
    f @ _: AverStr,
    k @ _: aver_rt::AverInt,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    pre @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::check::findDef(f.clone(), env.defs.clone()) {
        None => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("no definition of "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                __b
            }),
        ),
        Some(d @ _) => {
            if (aver_rt::AverInt::from_i64(d.params.len() as i64)
                == aver_rt::AverInt::from_i64(xs.len() as i64))
            {
                crate::proof_kernel::aver_generated::kernel::check::unfoldArm(
                    &d, k, xs, ys, pre, env, path,
                )
            } else {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(49)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(f),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" takes "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(aver_rt::AverInt::from_i64(d.params.len() as i64)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" arguments"));
                        __b
                    }),
                )
            }
        }
    }
}

/// The whole body, or one arm under its premise; the local bindings are substituted first, in order.
pub fn unfoldArm(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    k @ _: aver_rt::AverInt,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    pre @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let outer @ _ = crate::proof_kernel::aver_generated::kernel::check::bindLets(
        d.lets.clone(),
        crate::proof_kernel::aver_generated::kernel::check::zipBind(&d.params, xs).reverse(),
    )?;
    let lhs @ _ =
        crate::proof_kernel::aver_generated::kernel::term::Term::TCall(d.name.clone(), xs.clone());
    {
        let __int_match_subject = (k.clone(), ys.clone(), pre.clone());
        let (__lit0, __lit1, __lit2) = &__int_match_subject;
        if &(*__lit0) == &aver_rt::AverInt::from_i64(0)
            && (*__lit1).is_empty()
            && (*__lit2).is_empty()
        {
            Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                lhs: lhs,
                rhs: crate::proof_kernel::aver_generated::kernel::subst::subst(&d.body, &outer)?,
            })
        } else if &(*__lit0) == &aver_rt::AverInt::from_i64(0) {
            crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                AverStr::from("equation 0 takes no binders and no premise"),
            )
        } else {
            crate::proof_kernel::aver_generated::kernel::check::unfoldMatch(
                &d.body, k, &outer, ys, pre, &lhs, env, path,
            )
        }
    }
}

/// Each local binding, in order, bound to its value under everything before it; the latest name comes first.
#[inline(always)]
pub fn bindLets(
    mut lets @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    mut outer @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>, AverStr>
{
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(lets, [] => { return Ok(outer); }, [b, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::bind(b.name, &crate::proof_kernel::aver_generated::kernel::subst::subst(&b.value, &outer)?), &outer);
            lets = __tco0;
            outer = __tco1;
            continue;
        } })
    }
}

/// Arm k of the body's match.
pub fn unfoldMatch(
    body @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    k @ _: aver_rt::AverInt,
    outer @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    pre @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    lhs @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match body.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(s, arms) => {
            let s = (*s).clone();
            crate::proof_kernel::aver_generated::kernel::check::selected(
                &s, &arms, k, outer, ys, pre, lhs, env, path,
            )
        }
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the body is not a match"),
        ),
    }
}

/// Arm k of an explicit match term, under its premise.
pub fn armStep(
    k @ _: aver_rt::AverInt,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    q @ _: &crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(s, arms) => {
            let s = (*s).clone();
            crate::proof_kernel::aver_generated::kernel::check::selected(
                &s,
                &arms,
                k,
                &aver_rt::AverList::empty(),
                ys,
                &aver_rt::AverList::from_vec(vec![q.clone()]),
                t,
                env,
                path,
            )
        }
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("not a match"),
        ),
    }
}

/// lhs = arm k, once the premise shows the subject has arm k's pattern.
pub fn selected(
    s @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    k @ _: aver_rt::AverInt,
    outer @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    pre @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    lhs @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (
            crate::proof_kernel::aver_generated::kernel::check::armAt(arms.clone(), k.clone()),
            pre.clone(),
        );
        match __pat0 {
            None => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(32)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("there is no arm "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
                    __b
                }),
            ),
            _ => {
                let __list_subject = __pat1;
                if let Some((q, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat2;
                        if __list_subject.is_empty() {
                            crate::proof_kernel::aver_generated::kernel::check::selectedWith(
                                s, arms, k, outer, ys, &q, lhs, env, path,
                            )
                        } else {
                            crate::proof_kernel::aver_generated::kernel::check::refuse(
                                path,
                                AverStr::from("an arm needs exactly one premise"),
                            )
                        }
                    }
                } else {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        AverStr::from("an arm needs exactly one premise"),
                    )
                }
            }
        }
    }
}

/// Check the arm is the first that can match, then the premise.
pub fn selectedWith(
    s @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    k @ _: aver_rt::AverInt,
    outer @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    q @ _: &crate::proof_kernel::aver_generated::kernel::proof::Proof,
    lhs @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let chosen @ _ =
        crate::proof_kernel::aver_generated::kernel::check::armAt(arms.clone(), k.clone())
            .unwrap_or(crate::proof_kernel::aver_generated::kernel::term::Arm {
                pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PWild,
                body: crate::proof_kernel::aver_generated::kernel::term::Term::TUnit,
            });
    {
        let (__pat0, __pat1) = (
            crate::proof_kernel::aver_generated::kernel::check::isCatchAll(&chosen.pattern),
            ys.clone(),
        );
        if __pat0 {
            {
                let __list_subject = __pat1;
                if let Some((v, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat2;
                        if __list_subject.is_empty() {
                            crate::proof_kernel::aver_generated::kernel::check::catchAllArm(
                                s, arms, k, &chosen, outer, &v, q, lhs, env, path,
                            )
                        } else {
                            crate::proof_kernel::aver_generated::kernel::check::refuse(
                                path,
                                AverStr::from("a catch-all arm takes the value it is chosen for"),
                            )
                        }
                    }
                } else {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        AverStr::from("a catch-all arm takes the value it is chosen for"),
                    )
                }
            }
        } else {
            if crate::proof_kernel::aver_generated::kernel::check::earlierExclude(
                arms,
                k.clone(),
                &chosen.pattern,
            ) {
                crate::proof_kernel::aver_generated::kernel::check::armEquation(
                    s, &chosen, outer, ys, q, lhs, env, path,
                )
            } else {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(50)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("an earlier arm can also match arm "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
                        __b
                    }),
                )
            }
        }
    }
}

/// A pattern every value matches: the wildcard or a bare name.
pub fn isCatchAll(p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match p.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PWild => true,
        crate::proof_kernel::aver_generated::kernel::term::Pat::PVar(n) => true,
        _ => false,
    }
}

/// A catch-all arm chosen for one value: every earlier arm excludes it, the premise shows the subject is it, and a named catch-all binds it.
#[inline(always)]
pub fn catchAllArm(
    s @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    k @ _: aver_rt::AverInt,
    chosen @ _: &crate::proof_kernel::aver_generated::kernel::term::Arm,
    outer @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    v @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    q @ _: &crate::proof_kernel::aver_generated::kernel::proof::Proof,
    lhs @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if crate::proof_kernel::aver_generated::kernel::check::earlierExcludeValue(arms, k.clone(), v) {
        crate::proof_kernel::aver_generated::kernel::check::catchAllBody(
            chosen,
            &aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::check::zipBind(
                    &crate::proof_kernel::aver_generated::kernel::subst::patNames(&chosen.pattern),
                    &aver_rt::AverList::from_vec(vec![v.clone()]),
                ),
                &outer.clone(),
            ),
            &crate::proof_kernel::aver_generated::kernel::term::Eqn {
                lhs: crate::proof_kernel::aver_generated::kernel::subst::subst(s, outer)?,
                rhs: v.clone(),
            },
            &crate::proof_kernel::aver_generated::kernel::check::conclude(
                q,
                env,
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&AverStr::from("/premise"));
                    __b
                }),
            )?,
            lhs,
            path,
        )
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(50)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("an earlier arm can also match arm "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
                __b
            }),
        )
    }
}

/// The arm's body once the premise is the one it needs.
#[inline(always)]
pub fn catchAllBody(
    chosen @ _: &crate::proof_kernel::aver_generated::kernel::term::Arm,
    inner @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    needed @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    got @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    lhs @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (got == needed) {
        Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: lhs.clone(),
            rhs: crate::proof_kernel::aver_generated::kernel::subst::subst(&chosen.body, inner)?,
        })
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the premise does not select this arm"),
        )
    }
}

/// Every arm before arm k certainly does not match the value.
pub fn earlierExcludeValue(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    k @ _: aver_rt::AverInt,
    v @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (arms.clone(), (k > aver_rt::AverInt::from_i64(1)));
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && (*__lit1) == true {
            let Some((a, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            (crate::proof_kernel::aver_generated::kernel::check::excludes(&a.pattern, v)
                && crate::proof_kernel::aver_generated::kernel::check::earlierExcludeValue(
                    &rest,
                    k.sub(&aver_rt::AverInt::from_i64(1)),
                    v,
                ))
        } else {
            true
        }
    }
}

/// Whether a pattern certainly does not match a value: another literal, another constructor, the other list shape.
pub fn excludes(
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
    v @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (p.clone(), v.clone());
        match __pat0 {
            crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(x) => {
                (crate::proof_kernel::aver_generated::kernel::check::isLiteral(v)
                    && (crate::proof_kernel::aver_generated::kernel::check::sameKind(&x, v)
                        && (&(x) != v)))
            }
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, xs) => match __pat1 {
                crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(d, ys) => (c != d),
                _ => false,
            },
            crate::proof_kernel::aver_generated::kernel::term::Pat::PNil => match __pat1 {
                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(__pat2, __pat3) => {
                    match &*__pat2 {
                        "List.prepend" => {
                            let __list_subject = __pat3;
                            if let Some((h, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat4;
                                    if let Some((t, __pat5)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        {
                                            let __list_subject = __pat5;
                                            if __list_subject.is_empty() {
                                                true
                                            } else {
                                                false
                                            }
                                        }
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
                _ => false,
            },
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => match __pat1 {
                crate::proof_kernel::aver_generated::kernel::term::Term::TList(__pat6) => {
                    let __list_subject = __pat6;
                    if __list_subject.is_empty() {
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            },
            _ => false,
        }
    }
}

/// An Int, Bool, text or unit literal.
pub fn isLiteral(t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TInt(n) => true,
        crate::proof_kernel::aver_generated::kernel::term::Term::TBool(b) => true,
        crate::proof_kernel::aver_generated::kernel::term::Term::TStr(s) => true,
        crate::proof_kernel::aver_generated::kernel::term::Term::TUnit => true,
        _ => false,
    }
}

/// Two literals of the same type.
pub fn sameKind(
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    b @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match (a.clone(), b.clone()) {
        (
            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(x),
            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(y),
        ) => true,
        (
            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(x),
            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(y),
        ) => true,
        (
            crate::proof_kernel::aver_generated::kernel::term::Term::TStr(x),
            crate::proof_kernel::aver_generated::kernel::term::Term::TStr(y),
        ) => true,
        (
            crate::proof_kernel::aver_generated::kernel::term::Term::TUnit,
            crate::proof_kernel::aver_generated::kernel::term::Term::TUnit,
        ) => true,
        _ => false,
    }
}

/// subject = pattern must be what the premise proves; then lhs equals the arm's body.
pub fn armEquation(
    s @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    chosen @ _: &crate::proof_kernel::aver_generated::kernel::term::Arm,
    outer @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    q @ _: &crate::proof_kernel::aver_generated::kernel::proof::Proof,
    lhs @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let shape @ _ = crate::proof_kernel::aver_generated::kernel::check::patTerm(
        &chosen.pattern,
        ys,
        path.clone(),
    )?;
    let names @ _ = crate::proof_kernel::aver_generated::kernel::subst::patNames(&chosen.pattern);
    let inner @ _ = aver_rt::AverList::concat(
        &crate::proof_kernel::aver_generated::kernel::check::zipBind(&names, ys),
        &outer.clone(),
    );
    let needed @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn {
        lhs: crate::proof_kernel::aver_generated::kernel::subst::subst(s, outer)?,
        rhs: shape,
    };
    let got @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
        q,
        env,
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from("/premise"));
            __b
        }),
    )?;
    if (got == needed) {
        Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: lhs.clone(),
            rhs: crate::proof_kernel::aver_generated::kernel::subst::subst(&chosen.body, &inner)?,
        })
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the premise does not select this arm"),
        )
    }
}

/// Arm k, counting from 1.
pub fn armAt(
    mut arms @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    mut k @ _: aver_rt::AverInt,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Arm> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __int_match_subject = (arms, k.clone());
            let (__lit0, __lit1) = &__int_match_subject;
            if !(*__lit0).is_empty() && &(*__lit1) == &aver_rt::AverInt::from_i64(1) {
                let Some((a, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                return Some(a);
            } else if !(*__lit0).is_empty() {
                let Some((a, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                if (k > aver_rt::AverInt::from_i64(1)) {
                    {
                        let __tco0 = rest;
                        let __tco1 = k.sub(&aver_rt::AverInt::from_i64(1));
                        arms = __tco0;
                        k = __tco1;
                        continue;
                    }
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
    }
}

/// Every arm before arm k has a head pattern p cannot share.
pub fn earlierExclude(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    k @ _: aver_rt::AverInt,
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (arms.clone(), (k > aver_rt::AverInt::from_i64(1)));
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && (*__lit1) == true {
            let Some((a, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            (crate::proof_kernel::aver_generated::kernel::check::headsDiffer(&a.pattern, p)
                && crate::proof_kernel::aver_generated::kernel::check::earlierExclude(
                    &rest,
                    k.sub(&aver_rt::AverInt::from_i64(1)),
                    p,
                ))
        } else {
            true
        }
    }
}

/// Two patterns no value can both match.
pub fn headsDiffer(
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
    b @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match (a.clone(), b.clone()) {
        (
            crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(x),
            crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(y),
        ) => (x != y),
        (
            crate::proof_kernel::aver_generated::kernel::term::Pat::PNil,
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t),
        ) => true,
        (
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t),
            crate::proof_kernel::aver_generated::kernel::term::Pat::PNil,
        ) => true,
        (
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, xs),
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(d, ys),
        ) => (c != d),
        _ => false,
    }
}

/// The value a pattern denotes once its names are bound to ys.
pub fn patTerm(
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (p.clone(), ys.clone());
        match __pat0 {
            crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(t) => {
                let __list_subject = __pat1;
                if __list_subject.is_empty() {
                    Ok(t)
                } else {
                    Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(74)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("step "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                            __b
                        };
                        __b.push_str(&AverStr::from(
                            ": this pattern has no term to equate the subject with",
                        ));
                        __b
                    }))
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Pat::PNil => {
                let __list_subject = __pat1;
                if __list_subject.is_empty() {
                    Ok(
                        crate::proof_kernel::aver_generated::kernel::term::Term::TList(
                            aver_rt::AverList::empty(),
                        ),
                    )
                } else {
                    Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(74)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("step "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                            __b
                        };
                        __b.push_str(&AverStr::from(
                            ": this pattern has no term to equate the subject with",
                        ));
                        __b
                    }))
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
                let __list_subject = __pat1;
                if let Some((x, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat2;
                        if let Some((y, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat3;
                                if __list_subject.is_empty() {
                                    crate::proof_kernel::aver_generated::kernel::check::consTerm(
                                        h, t, &x, &y, path,
                                    )
                                } else {
                                    Err(aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(74))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&AverStr::from("step "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(path)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from(
                                            ": this pattern has no term to equate the subject with",
                                        ));
                                        __b
                                    }))
                                }
                            }
                        } else {
                            Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(74))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("step "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(path),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(
                                    ": this pattern has no term to equate the subject with",
                                ));
                                __b
                            }))
                        }
                    }
                } else {
                    Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(74)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("step "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                            __b
                        };
                        __b.push_str(&AverStr::from(
                            ": this pattern has no term to equate the subject with",
                        ));
                        __b
                    }))
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, names) => {
                crate::proof_kernel::aver_generated::kernel::check::ctorTerm(c, &names, ys, path)
            }
            _ => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(74)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("step "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&AverStr::from(
                    ": this pattern has no term to equate the subject with",
                ));
                __b
            })),
        }
    }
}

/// A cons pattern with both parts named.
#[inline(always)]
pub fn consTerm(
    h @ _: AverStr,
    t @ _: AverStr,
    x @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    y @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if ((&*h == "_") || (&*t == "_")) {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(50)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from(": a wildcard part has no term"));
            __b
        }))
    } else {
        Ok(
            crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                AverStr::from("List.prepend"),
                aver_rt::AverList::from_vec(vec![x.clone(), y.clone()]),
            ),
        )
    }
}

/// A constructor pattern with every field named.
#[inline(always)]
pub fn ctorTerm(
    c @ _: AverStr,
    names @ _: &aver_rt::AverList<AverStr>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if ((aver_rt::AverInt::from_i64(names.len() as i64)
        == aver_rt::AverInt::from_i64(ys.len() as i64))
        && (!names.contains(&AverStr::from("_"))))
    {
        Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(c, ys.clone()))
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(69)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from(
                ": the binders do not fit the constructor pattern",
            ));
            __b
        }))
    }
}

/// The names a substitution binds, in order.
#[inline(always)]
pub fn boundNames(
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(bs.clone(), [] => aver_rt::AverList::empty(), [b, rest] => aver_rt::AverList::prepend(b.name, &crate::proof_kernel::aver_generated::kernel::check::boundNames(&rest)))
}

/// An equation at a substitution.
pub fn instantiate(
    e @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        crate::proof_kernel::aver_generated::kernel::subst::subst(&e.lhs, bs),
        crate::proof_kernel::aver_generated::kernel::subst::subst(&e.rhs, bs),
    ) {
        (Ok(l), Ok(r)) => {
            Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r })
        }
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the substitution captures a bound name"),
        ),
    }
}

/// A wall rule at an explicit substitution of all its binders.
#[inline(always)]
pub fn rule(
    id @ _: AverStr,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::rules::schema(id.clone()) {
        None => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(29)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("unknown rule "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(id))));
                __b
            }),
        ),
        Some(s @ _) => {
            if (crate::proof_kernel::aver_generated::kernel::check::boundNames(bs) == s.binders) {
                crate::proof_kernel::aver_generated::kernel::check::ruleConclusion(
                    &s.premises,
                    &s.concl,
                    bs,
                    ps,
                    env,
                    path,
                )
            } else {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(44)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&AverStr::from("rule "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(id))));
                                __b
                            };
                            __b.push_str(&AverStr::from(" binds "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_kernel::aver_generated::kernel::check::commaJoined(
                                &s.binders,
                            )),
                        )));
                        __b
                    }),
                )
            }
        }
    }
}

/// Premises first, then the conclusion.
pub fn ruleConclusion(
    wanted @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    concl @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::premisesHold(
        wanted.clone(),
        ps.clone(),
        bs,
        env,
        path.clone(),
        aver_rt::AverInt::from_i64(0),
    )?;
    crate::proof_kernel::aver_generated::kernel::check::instantiate(concl, bs, path)
}

/// A cited law by key.
#[inline(always)]
pub fn findLaw(
    mut key @ _: AverStr,
    mut ls @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
) -> Option<crate::proof_kernel::aver_generated::kernel::proof::Law> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(ls, [] => { return None; }, [l, rest] => { if (l.key == key) { return Some(l); } else { {
            let __tco1 = rest;
            ls = __tco1;
            continue;
        } } })
    }
}

/// An earlier law at an explicit substitution of all its givens, its when proved.
#[inline(always)]
pub fn lawStep(
    key @ _: AverStr,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::check::findLaw(key.clone(), env.laws.clone())
    {
        None => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("law "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(key))));
                    __b
                };
                __b.push_str(&AverStr::from(" is not cited"));
                __b
            }),
        ),
        Some(l @ _) => {
            if (crate::proof_kernel::aver_generated::kernel::check::boundNames(bs) == l.givens) {
                crate::proof_kernel::aver_generated::kernel::check::ruleConclusion(
                    &crate::proof_kernel::aver_generated::kernel::check::lawPremises(&l),
                    &crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: l.lhs.clone(),
                        rhs: l.rhs.clone(),
                    },
                    bs,
                    ps,
                    env,
                    path,
                )
            } else {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&AverStr::from("law "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(key),
                                )));
                                __b
                            };
                            __b.push_str(&AverStr::from(" quantifies "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_kernel::aver_generated::kernel::check::commaJoined(
                                &l.givens,
                            )),
                        )));
                        __b
                    }),
                )
            }
        }
    }
}

/// A law's when as a premise equation.
#[inline(always)]
pub fn lawPremises(
    l @ _: &crate::proof_kernel::aver_generated::kernel::proof::Law,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(l.premise.clone(), [] => aver_rt::AverList::empty(), [p, rest] => aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: p, rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true) }]))
}

/// The proof must prove exactly the obligation, under its when.
pub fn checkScript(
    s @ _: &crate::proof_kernel::aver_generated::kernel::proof::Script,
) -> Result<AverStr, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let env @ _ = crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: s.defs.clone(),
        consts: s.consts.clone(),
        laws: s.laws.clone(),
        hyps: crate::proof_kernel::aver_generated::kernel::check::lawHyps(&s.obligation),
        finite: s.finite.clone(),
    };
    let e @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
        &s.proof,
        &env,
        AverStr::from("proof"),
    )?;
    if (e
        == crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: s.obligation.lhs.clone(),
            rhs: s.obligation.rhs.clone(),
        })
    {
        Ok(s.obligation.key.clone())
    } else {
        Err(AverStr::from(
            "step proof: the proof ends at a different equation than the claim",
        ))
    }
}

/// The obligation's when, as hypothesis `when`.
#[inline(always)]
pub fn lawHyps(
    ob @ _: &crate::proof_kernel::aver_generated::kernel::proof::Law,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ob.premise.clone(), [] => aver_rt::AverList::empty(), [p, rest] => aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: AverStr::from("when"), eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: p, rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true) } }]))
}

/// Names separated by commas.
#[inline(always)]
pub fn commaJoined(ns @ _: &aver_rt::AverList<AverStr>) -> AverStr {
    crate::proof_kernel::cancel_checkpoint();
    (aver_rt::string_join(&ns, &AverStr::from(","))).into_aver()
}
