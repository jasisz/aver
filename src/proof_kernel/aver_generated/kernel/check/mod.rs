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
    pub lists: aver_rt::AverList<AverStr>,
    pub ints: aver_rt::AverList<AverStr>,
    pub givens: aver_rt::AverList<AverStr>,
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
            .then_with(|| self.givens.cmp(&other.givens))
            .then_with(|| self.hyps.cmp(&other.hyps))
            .then_with(|| self.ints.cmp(&other.ints))
            .then_with(|| self.laws.cmp(&other.laws))
            .then_with(|| self.lists.cmp(&other.lists))
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
                format!("finite: {}", self.finite.aver_display_inner()),
                format!("lists: {}", self.lists.aver_display_inner()),
                format!("ints: {}", self.ints.aver_display_inner()),
                format!("givens: {}", self.givens.aver_display_inner())
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
    Conclude(
        crate::proof_kernel::aver_generated::kernel::proof::Proof,
        Env,
        AverStr,
    ),
    Have(
        AverStr,
        crate::proof_kernel::aver_generated::kernel::term::Term,
        crate::proof_kernel::aver_generated::kernel::proof::Proof,
        crate::proof_kernel::aver_generated::kernel::proof::Proof,
        Env,
        AverStr,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::Conclude(mut p @ _, mut env @ _, mut path @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                match p {
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PRefl(t) => {
                        return Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                            lhs: t.clone(),
                            rhs: t,
                        });
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PSymm(q) => {
                        let q = (*q).clone();
                        return crate::proof_kernel::aver_generated::kernel::check::flip(
                            &crate::proof_kernel::aver_generated::kernel::check::conclude(
                                q,
                                env,
                                (path + &AverStr::from("/symm")),
                            )?,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PTrans(ts, ps) => {
                        return crate::proof_kernel::aver_generated::kernel::check::trans(
                            &ts, &ps, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PCongr(c, q) => {
                        let q = (*q).clone();
                        return crate::proof_kernel::aver_generated::kernel::check::congr(
                            &c, q, env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PUnfold(
                        f,
                        k,
                        xs,
                        ys,
                        pre,
                    ) => {
                        return crate::proof_kernel::aver_generated::kernel::check::unfold(
                            f, k, &xs, &ys, &pre, env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PConst(n) => {
                        return crate::proof_kernel::aver_generated::kernel::check::constant(
                            n, env.consts, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PArm(
                        k,
                        ys,
                        t,
                        q,
                    ) => {
                        let q = (*q).clone();
                        return crate::proof_kernel::aver_generated::kernel::check::armStep(
                            k, &ys, &t, &q, env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PProj(t) => {
                        return crate::proof_kernel::aver_generated::kernel::check::proj(t, path);
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PCell(t) => {
                        return crate::proof_kernel::aver_generated::kernel::check::cell(&t, path);
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PHyp(h) => {
                        return crate::proof_kernel::aver_generated::kernel::check::hyp(
                            h, env.hyps, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PRule(
                        id,
                        bs,
                        ps,
                    ) => {
                        return crate::proof_kernel::aver_generated::kernel::check::rule(
                            id, &bs, &ps, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PLaw(k, bs, ps) => {
                        return crate::proof_kernel::aver_generated::kernel::check::lawStep(
                            k, &bs, &ps, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PCompute(a, b) => {
                        return crate::proof_kernel::aver_generated::kernel::check::compute(
                            &a, &b, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PCases(
                        on,
                        h,
                        t,
                        f,
                    ) => {
                        let t = (*t).clone();
                        let f = (*f).clone();
                        return crate::proof_kernel::aver_generated::kernel::check::cases(
                            &on, h, t, f, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PHave(
                        h,
                        fact,
                        q,
                        body,
                    ) => {
                        let q = (*q).clone();
                        let body = (*body).clone();
                        __MutualTco1::Have(h, fact, q, body, env, path)
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PEnum(
                        v,
                        l,
                        r,
                        cs,
                    ) => {
                        return crate::proof_kernel::aver_generated::kernel::check::enumStep(
                            v, &l, &r, &cs, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PAbsurd(q, l, r) => {
                        let q = (*q).clone();
                        return crate::proof_kernel::aver_generated::kernel::check::absurd(
                            &crate::proof_kernel::aver_generated::kernel::check::conclude(
                                q,
                                env,
                                aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(23))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(path)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&AverStr::from("/absurd"));
                                    __b
                                }),
                            )?,
                            &l,
                            &r,
                            path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PInduct(
                        f,
                        xs,
                        l,
                        r,
                        carry,
                        cs,
                    ) => {
                        return crate::proof_kernel::aver_generated::kernel::check::induct(
                            f,
                            &xs,
                            crate::proof_kernel::aver_generated::kernel::term::Eqn {
                                lhs: l,
                                rhs: r,
                            },
                            &carry,
                            &cs,
                            &env,
                            path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PListInduct(
                        v,
                        l,
                        r,
                        n,
                        h,
                        t,
                        ih,
                        c,
                    ) => {
                        let n = (*n).clone();
                        let c = (*c).clone();
                        return crate::proof_kernel::aver_generated::kernel::check::listInduct(
                            v,
                            &crate::proof_kernel::aver_generated::kernel::term::Eqn {
                                lhs: l,
                                rhs: r,
                            },
                            n,
                            &aver_rt::AverList::from_vec(vec![h, t, ih]),
                            c,
                            &env,
                            path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PIntInduct(
                        v,
                        l,
                        r,
                        g,
                        b,
                        names,
                        carry,
                        ih,
                        st,
                    ) => {
                        let b = (*b).clone();
                        let st = (*st).clone();
                        return crate::proof_kernel::aver_generated::kernel::check::intInduct(
                            v,
                            &crate::proof_kernel::aver_generated::kernel::term::Eqn {
                                lhs: l,
                                rhs: r,
                            },
                            g,
                            b,
                            &names,
                            &carry,
                            ih,
                            st,
                            &env,
                            path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PRing(l, r) => {
                        return crate::proof_kernel::aver_generated::kernel::check::ring(
                            &l, &r, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PLinear(
                        g,
                        v,
                        hs,
                        ws,
                    ) => {
                        return crate::proof_kernel::aver_generated::kernel::check::linear(
                            &g, v, &hs, &ws, &env, path,
                        );
                    }
                }
            }
            __MutualTco1::Have(
                mut h @ _,
                mut fact @ _,
                mut q @ _,
                mut body @ _,
                mut env @ _,
                mut path @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                let proved @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
                    q,
                    env.clone(),
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(38)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&AverStr::from("/have."));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                        __b
                    }),
                )?;
                if (proved
                    == crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: fact.clone(),
                        rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true),
                    })
                {
                    __MutualTco1::Conclude(
                        body,
                        crate::proof_kernel::aver_generated::kernel::check::withHyp(
                            &env, h, &fact, true,
                        ),
                        path,
                    )
                } else {
                    return crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(72)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&AverStr::from("the proof of "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                                __b
                            };
                            __b.push_str(&AverStr::from(
                                " ends at a different equation than its fact",
                            ));
                            __b
                        }),
                    );
                }
            }
        };
    }
}

/// The equation a step proves, or the refusal of the first wrong step under it.
pub fn conclude(
    p @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::Conclude(p, env, path))
}

/// A cut: q proves fact = true in the current scope, then body is checked with it as hypothesis h.
pub fn have(
    h @ _: AverStr,
    fact @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    body @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::Have(h, fact, q, body, env, path))
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
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

fn __mutual_tco_trampoline_2(
    mut __state: __MutualTco2,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    env @ _: &Env,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::EnumCases(
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
                        __MutualTco2::EnumCase(v, x, c, xs, rest, path, i)
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
            __MutualTco2::EnumCase(
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
                    c,
                    crate::proof_kernel::aver_generated::kernel::check::withHyps(
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
                    __MutualTco2::EnumCases(
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
    __mutual_tco_trampoline_2(__MutualTco2::EnumCases(v, vals, cs, path, i), &claim, &env)
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
    __mutual_tco_trampoline_2(
        __MutualTco2::EnumCase(v, x, c, xs, rest, path, i),
        &claim,
        &env,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
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

fn __mutual_tco_trampoline_3(
    mut __state: __MutualTco3,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
) -> Result<(), AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::PremisesHold(mut wanted @ _, mut ps @ _, mut path @ _, mut i @ _) => {
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
                        __MutualTco3::PremiseHolds(w, p, ws, rest, path, i)
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
            __MutualTco3::PremiseHolds(
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
                    p,
                    (*env).clone(),
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
                    __MutualTco3::PremisesHold(
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
    __mutual_tco_trampoline_3(__MutualTco3::PremisesHold(wanted, ps, path, i), &bs, &env)
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
    __mutual_tco_trampoline_3(
        __MutualTco3::PremiseHolds(w, p, ws, rest, path, i),
        &bs,
        &env,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco4 {
    InductCases(
        aver_rt::AverInt,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
        AverStr,
        aver_rt::AverInt,
    ),
    InductNext(
        Result<(), AverStr>,
        aver_rt::AverInt,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
        AverStr,
        aver_rt::AverInt,
    ),
}

fn __mutual_tco_trampoline_4(
    mut __state: __MutualTco4,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    at @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    taken @ _: &aver_rt::AverList<AverStr>,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    env @ _: &Env,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    loop {
        __state = match __state {
            __MutualTco4::InductCases(
                mut j @ _,
                mut arms @ _,
                mut cs @ _,
                mut path @ _,
                mut i @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                {
                    let __int_match_subject = (arms, cs);
                    let (__lit0, __lit1) = &__int_match_subject;
                    if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
                        let Some((a, moreArms)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        let Some((c, moreCases)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        __MutualTco4::InductNext(
                            crate::proof_kernel::aver_generated::kernel::check::inductCase(
                                &*d,
                                j.clone(),
                                a,
                                &c,
                                &*at,
                                &*claim,
                                &*vr,
                                &*taken,
                                &*carried,
                                &*env,
                                aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(38))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(path)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from("/case."));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(i),
                                    )));
                                    __b
                                }),
                            ),
                            j,
                            moreArms,
                            moreCases,
                            path,
                            i.add(&aver_rt::AverInt::from_i64(1)),
                        )
                    } else {
                        return Ok((*claim).clone());
                    }
                }
            }
            __MutualTco4::InductNext(
                mut done @ _,
                mut j @ _,
                mut arms @ _,
                mut cs @ _,
                mut path @ _,
                mut i @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                done?;
                __MutualTco4::InductCases(j, arms, cs, path, i)
            }
        };
    }
}

/// Each arm with its case, in order; the result is the claim.
pub fn inductCases(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    j @ _: aver_rt::AverInt,
    arms @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
    at @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    taken @ _: &aver_rt::AverList<AverStr>,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    env @ _: &Env,
    path @ _: AverStr,
    i @ _: aver_rt::AverInt,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    __mutual_tco_trampoline_4(
        __MutualTco4::InductCases(j, arms, cs, path, i),
        &d,
        &at,
        &claim,
        &vr,
        &taken,
        &carried,
        &env,
    )
}

/// The rest of the cases, once this one checked.
pub fn inductNext(
    done @ _: Result<(), AverStr>,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    j @ _: aver_rt::AverInt,
    arms @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
    at @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    taken @ _: &aver_rt::AverList<AverStr>,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    env @ _: &Env,
    path @ _: AverStr,
    i @ _: aver_rt::AverInt,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    __mutual_tco_trampoline_4(
        __MutualTco4::InductNext(done, j, arms, cs, path, i),
        &d,
        &at,
        &claim,
        &vr,
        &taken,
        &carried,
        &env,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco5 {
    CarriedHold(
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
        AverStr,
    ),
    CarriedThen(
        Result<(), AverStr>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
        AverStr,
    ),
}

fn __mutual_tco_trampoline_5(
    mut __state: __MutualTco5,
    down @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
) -> Result<(), AverStr> {
    loop {
        __state = match __state {
            __MutualTco5::CarriedHold(mut cs @ _, mut proofs @ _, mut path @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                {
                    let __int_match_subject = (cs, proofs);
                    let (__lit0, __lit1) = &__int_match_subject;
                    if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
                        let Some((c, more)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        let Some((q, qs)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        __MutualTco5::CarriedThen(
                            crate::proof_kernel::aver_generated::kernel::check::caseProves(
                                q,
                                &crate::proof_kernel::aver_generated::kernel::check::instantiate(
                                    &c.eqn,
                                    &*down,
                                    path.clone(),
                                )?,
                                (*env).clone(),
                                aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(39))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(path)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&AverStr::from("/carry."));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(c.name),
                                    )));
                                    __b
                                }),
                            ),
                            more,
                            qs,
                            path,
                        )
                    } else {
                        return Ok(());
                    }
                }
            }
            __MutualTco5::CarriedThen(mut done @ _, mut cs @ _, mut proofs @ _, mut path @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                done?;
                __MutualTco5::CarriedHold(cs, proofs, path)
            }
        };
    }
}

/// Each carried hypothesis, at v - 1, proved by its proof.
pub fn carriedHold(
    cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    proofs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    down @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::CarriedHold(cs, proofs, path), &down, &env)
}

/// The rest of the carried hypotheses, once this one held.
pub fn carriedThen(
    done @ _: Result<(), AverStr>,
    cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    proofs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    down @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    __mutual_tco_trampoline_5(
        __MutualTco5::CarriedThen(done, cs, proofs, path),
        &down,
        &env,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco6 {
    CheckFacts(
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
    ),
    CheckFactThen(
        Result<AverStr, AverStr>,
        crate::proof_kernel::aver_generated::kernel::proof::Law,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
    ),
}

fn __mutual_tco_trampoline_6(mut __state: __MutualTco6) -> Result<(), AverStr> {
    loop {
        __state = match __state {
            __MutualTco6::CheckFacts(mut fs @ _, mut earlier @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                aver_list_match!(fs, [] => { return Ok(()) }, [f, rest] => __MutualTco6::CheckFactThen(crate::proof_kernel::aver_generated::kernel::check::checkScript(crate::proof_kernel::aver_generated::kernel::proof::Script { obligation: f.law.clone(), finite: aver_rt::AverList::empty(), lists: f.lists, ints: aver_rt::AverList::empty(), defs: aver_rt::AverList::empty(), consts: aver_rt::AverList::empty(), laws: earlier.clone(), facts: aver_rt::AverList::empty(), proof: f.proof }), f.law.clone(), rest, earlier))
            }
            __MutualTco6::CheckFactThen(
                mut done @ _,
                mut law @ _,
                mut rest @ _,
                mut earlier @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                match done {
                    Ok(k @ _) => __MutualTco6::CheckFacts(
                        rest,
                        aver_rt::AverList::concat(
                            &earlier,
                            &aver_rt::AverList::from_vec(vec![law]),
                        ),
                    ),
                    Err(why @ _) => {
                        return Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(39))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("fact "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(law.key),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(": "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                            __b
                        }));
                    }
                }
            }
        };
    }
}

/// Each builtin fact proves its statement over builtins and the facts before it: no definitions, no other laws, no when.
pub fn checkFacts(
    fs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
    earlier @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
) -> Result<(), AverStr> {
    __mutual_tco_trampoline_6(__MutualTco6::CheckFacts(fs, earlier))
}

/// The rest of the facts, once this one checked; they may cite it.
pub fn checkFactThen(
    done @ _: Result<AverStr, AverStr>,
    law @ _: crate::proof_kernel::aver_generated::kernel::proof::Law,
    rest @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
    earlier @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
) -> Result<(), AverStr> {
    __mutual_tco_trampoline_6(__MutualTco6::CheckFactThen(done, law, rest, earlier))
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
        lists: aver_rt::AverList::empty(),
        ints: aver_rt::AverList::empty(),
        givens: aver_rt::AverList::empty(),
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
                aver_list_match!(__pat2, [] => { { let __list_subject = __pat1; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: a.clone(), rhs: a }) } else { crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("trans needs at least one step")) } } }, [b, moreTerms] => { { let __list_subject = __pat1; if let Some((p, moreSteps)) = aver_rt::list_uncons_cloned(&__list_subject) { crate::proof_kernel::aver_generated::kernel::check::transLink(&a, &b.clone(), p, &aver_rt::AverList::prepend(b, &moreTerms), &moreSteps, env, path, i) } else { crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("trans needs at least one step")) } } })
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
    mut p @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
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
    let e @ _ =
        crate::proof_kernel::aver_generated::kernel::check::conclude(p, env.clone(), here.clone())?;
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
    mut q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut env @ _: Env,
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

/// A list literal with an element is that element in front of the rest.
pub fn cell(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TList(__pat0) => {
            let __list_subject = __pat0;
            if let Some((x, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
                Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                    lhs: t.clone(),
                    rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        AverStr::from("List.prepend"),
                        aver_rt::AverList::from_vec(vec![
                            x,
                            crate::proof_kernel::aver_generated::kernel::term::Term::TList(rest),
                        ]),
                    ),
                })
            } else {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    AverStr::from("a cell step needs a list literal with an element"),
                )
            }
        }
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("a cell step needs a list literal with an element"),
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

/// Both branches of a Bool split must prove the same equation; the term split on must be a Bool.
#[inline(always)]
pub fn cases(
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
    mut t @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut f @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if crate::proof_kernel::aver_generated::kernel::check::isBool(on, env) {
        crate::proof_kernel::aver_generated::kernel::check::casesOfBool(on, h, t, f, env, path)
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the term split on is not a Bool"),
        )
    }
}

/// Whether a term is a Bool by its shape: a literal, a comparison, a Bool connective, a call of a definition that returns a Bool, or a given of type Bool or a Bool field of one.
pub fn isBool(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TBool(b) => true,
        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(o, a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            aver_rt::AverList::from_vec(vec![
                AverStr::from("=="),
                AverStr::from("!="),
                AverStr::from("<"),
                AverStr::from("<="),
                AverStr::from(">"),
                AverStr::from(">="),
                AverStr::from("==."),
                AverStr::from("!=."),
                AverStr::from("<."),
                AverStr::from("<=."),
                AverStr::from(">."),
                AverStr::from(">=."),
            ])
            .contains(&o)
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TBi(n, xs) => {
            aver_rt::AverList::from_vec(vec![
                AverStr::from("Bool.and"),
                AverStr::from("Bool.or"),
                AverStr::from("Bool.not"),
            ])
            .contains(&n)
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TCall(f, xs) => {
            crate::proof_kernel::aver_generated::kernel::check::returnsBool(f, env.defs.clone())
        }
        _ => {
            (crate::proof_kernel::aver_generated::kernel::check::finOf(t, &env.finite)
                == Some(crate::proof_kernel::aver_generated::kernel::term::Fin::FBool))
        }
    }
}

/// The finite type of a given, or of a field of one.
pub fn finOf(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    gs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Given>,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Fin> {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(v) => {
            crate::proof_kernel::aver_generated::kernel::check::findGiven(v, gs.clone())
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TGet(r, f) => {
            let r = (*r).clone();
            match crate::proof_kernel::aver_generated::kernel::check::finOf(&r, gs) {
                Some(__pat0) => match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::term::Fin::FRec(n, fs) => {
                        crate::proof_kernel::aver_generated::kernel::check::fieldFin(f, fs)
                    }
                    _ => None,
                },
                _ => None,
            }
        }
        _ => None,
    }
}

/// The finite type of a named field.
#[inline(always)]
pub fn fieldFin(
    mut f @ _: AverStr,
    mut fs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::FinField>,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Fin> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(fs, [] => { return None; }, [x, rest] => { if (x.name == f) { return Some(x.fin); } else { {
            let __tco1 = rest;
            fs = __tco1;
            continue;
        } } })
    }
}

/// Whether f is a definition in scope marked as returning a Bool.
#[inline(always)]
pub fn returnsBool(
    mut f @ _: AverStr,
    mut ds @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> bool {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(ds, [] => { return false; }, [d, rest] => { if (d.name == f) { return d.returnsBool; } else { {
            let __tco1 = rest;
            ds = __tco1;
            continue;
        } } })
    }
}

/// The two branches of a split on a Bool.
pub fn casesOfBool(
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
    mut t @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut f @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let whenTrue @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
        t,
        crate::proof_kernel::aver_generated::kernel::check::withHyp(env, h.clone(), on, true),
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
        crate::proof_kernel::aver_generated::kernel::check::withHyp(env, h, on, false),
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
        lists: env.lists.clone(),
        ints: env.ints.clone(),
        givens: env.givens.clone(),
    }
}

/// A comparison decided by linear arithmetic: its opposite and the named hypotheses, weighted, add up to a negative constant.
#[inline(always)]
pub fn linear(
    g @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    v @ _: bool,
    hs @ _: &aver_rt::AverList<AverStr>,
    ws @ _: &aver_rt::AverIntList,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::check::hypEqns(hs, &env.hyps, path.clone()) {
        Err(why @ _) => Err(why),
        Ok(facts @ _) => {
            if crate::proof_kernel::aver_generated::kernel::ring::linearContradiction(
                &aver_rt::AverList::prepend(
                    crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: g.clone(),
                        rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool((!v)),
                    },
                    &facts,
                ),
                ws,
            ) {
                Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                    lhs: g.clone(),
                    rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(v),
                })
            } else {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    AverStr::from("the weights do not add up to a contradiction"),
                )
            }
        }
    }
}

/// The equations of the named hypotheses.
#[inline(always)]
pub fn hypEqns(
    hs @ _: &aver_rt::AverList<AverStr>,
    scope @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => Ok(aver_rt::AverList::empty()), [h, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::check::hyp(h, scope.clone(), path.clone())?, &crate::proof_kernel::aver_generated::kernel::check::hypEqns(&rest, scope, path)?)))
}

/// Two Int terms that are the same polynomial over their atoms.
#[inline(always)]
pub fn ring(
    l @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    r @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if crate::proof_kernel::aver_generated::kernel::ring::samePolynomial(l, r) {
        Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: l.clone(),
            rhs: r.clone(),
        })
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the two sides are different polynomials"),
        )
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
        lists: env.lists.clone(),
        ints: env.ints.clone(),
        givens: env.givens.clone(),
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
    mut env @ _: Env,
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
            match (
                (aver_rt::AverInt::from_i64(d.params.len() as i64)
                    == aver_rt::AverInt::from_i64(xs.len() as i64)),
                crate::proof_kernel::aver_generated::kernel::induct::openGate(&d),
            ) {
                (_, Err(why)) => {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, why)
                }
                (false, _) => crate::proof_kernel::aver_generated::kernel::check::refuse(
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
                ),
                (true, _) => crate::proof_kernel::aver_generated::kernel::check::unfoldArm(
                    &d, k, xs, ys, pre, env, path,
                ),
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
    mut env @ _: Env,
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
    mut env @ _: Env,
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
    mut env @ _: Env,
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
    mut env @ _: Env,
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
                                s, arms, k, outer, ys, q, lhs, env, path,
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
    mut q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    lhs @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    mut env @ _: Env,
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
    mut q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    lhs @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    mut env @ _: Env,
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
    mut q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    lhs @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    mut env @ _: Env,
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

/// The proof must prove exactly the obligation, under its when; every cited builtin fact is checked first.
pub fn checkScript(
    mut s @ _: crate::proof_kernel::aver_generated::kernel::proof::Script,
) -> Result<AverStr, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::induct::refuseMutualRecursion(&s.defs)?;
    crate::proof_kernel::aver_generated::kernel::check::checkFacts(
        s.facts.clone(),
        aver_rt::AverList::empty(),
    )?;
    let env @ _ = crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: s.defs,
        consts: s.consts,
        laws: aver_rt::AverList::concat(
            &s.laws,
            &crate::proof_kernel::aver_generated::kernel::check::factLaws(&s.facts),
        ),
        hyps: crate::proof_kernel::aver_generated::kernel::check::lawHyps(&s.obligation),
        finite: s.finite,
        lists: s.lists,
        ints: s.ints,
        givens: s.obligation.givens.clone(),
    };
    let e @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
        s.proof,
        env,
        AverStr::from("proof"),
    )?;
    if (e
        == crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: s.obligation.lhs,
            rhs: s.obligation.rhs,
        })
    {
        Ok(s.obligation.key)
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

/// Induction along the recursion of f at arguments xs: one case per arm of its match, each with one hypothesis per recursive call in the arm. The hypotheses named in carry hold at each case's pattern; a call's hypothesis holds once they are proved at the call's arguments.
#[inline(always)]
pub fn induct(
    f @ _: AverStr,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carry @ _: &aver_rt::AverList<AverStr>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
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
            let (__pat0, __pat1) = (
                crate::proof_kernel::aver_generated::kernel::induct::structuralParam(&d),
                d.body.clone(),
            );
            match __pat0 {
                Err(why @ _) => {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, why)
                }
                Ok(__pat2 @ _) => match __pat2 {
                    None => crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                                __b
                            };
                            __b.push_str(&AverStr::from(" does not recurse"));
                            __b
                        }),
                    ),
                    Some(j @ _) => match __pat1 {
                        crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(
                            s,
                            arms,
                        ) => {
                            let s = (*s).clone();
                            crate::proof_kernel::aver_generated::kernel::check::inductOn(
                                &d, j, &arms, xs, claim, carry, cs, env, path,
                            )
                        }
                        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                            path,
                            aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(43))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("the body of "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(f),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" is not a match"));
                                __b
                            }),
                        ),
                    },
                },
            }
        }
    }
}

/// The arguments, the varied givens and the shape of the arms, then every case.
pub fn inductOn(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    j @ _: aver_rt::AverInt,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carry @ _: &aver_rt::AverList<AverStr>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        (aver_rt::AverInt::from_i64(xs.len() as i64)
            == aver_rt::AverInt::from_i64(d.params.len() as i64)),
        crate::proof_kernel::aver_generated::kernel::induct::varied(xs, j.clone(), &env.givens),
    ) {
        (false, _) => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(49)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name))));
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
        ),
        (_, Err(why)) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
        (true, Ok(vr)) => crate::proof_kernel::aver_generated::kernel::check::inductChecked(
            d,
            j,
            arms,
            xs,
            claim,
            &crate::proof_kernel::aver_generated::kernel::check::namedHyps(
                carry,
                &env.hyps,
                path.clone(),
            )?,
            cs,
            &vr,
            env,
            path,
        ),
    }
}

/// One case per arm; the arms are one per constructor. The cases see the carried hypotheses and those that mention nothing the induction varies.
pub fn inductChecked(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    j @ _: aver_rt::AverInt,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let varying @ _ = aver_rt::AverList::prepend(
        vr.v.clone(),
        &crate::proof_kernel::aver_generated::kernel::check::placeNames(&vr.general),
    );
    match (
        (aver_rt::AverInt::from_i64(arms.len() as i64)
            == aver_rt::AverInt::from_i64(cs.len() as i64)),
        crate::proof_kernel::aver_generated::kernel::check::armsAreConstructors(arms),
    ) {
        (false, _) => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(45)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(aver_rt::AverInt::from_i64(arms.len() as i64)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" arms, "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(aver_rt::AverInt::from_i64(cs.len() as i64)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(" cases"));
                __b
            }),
        ),
        (_, false) => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the arms are not one per constructor"),
        ),
        _ => crate::proof_kernel::aver_generated::kernel::check::inductCases(
            d,
            j,
            arms.clone(),
            cs.clone(),
            &crate::proof_kernel::aver_generated::kernel::induct::Call {
                args: xs.clone(),
                inner: aver_rt::AverList::empty(),
            },
            &claim.clone(),
            vr,
            &crate::proof_kernel::aver_generated::kernel::check::takenNames(claim, &env.hyps),
            carried,
            &crate::proof_kernel::aver_generated::kernel::check::withHyps(
                env,
                &crate::proof_kernel::aver_generated::kernel::check::notMentioningAny(
                    env.hyps.clone(),
                    varying,
                ),
            ),
            path,
            aver_rt::AverInt::from_i64(0),
        ),
    }
}

/// The hypotheses that mention none of the names.
#[inline(always)]
pub fn notMentioningAny(
    mut hs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    names @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> {
    let names @ _ = std::sync::Arc::new(names);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(hs, [] => { return aver_rt::AverList::empty(); }, [h, rest] => { if crate::proof_kernel::aver_generated::kernel::check::hypsMention(&aver_rt::AverList::from_vec(vec![h.clone()]), &*names) { {
            let __tco0 = rest;
            hs = __tco0;
            continue;
        } } else { return aver_rt::AverList::prepend(h, &crate::proof_kernel::aver_generated::kernel::check::notMentioningAny(rest, (*names).clone())); } })
    }
}

/// The names of the varied places.
#[inline(always)]
pub fn placeNames(
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Place>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ps.clone(), [] => aver_rt::AverList::empty(), [p, rest] => aver_rt::AverList::prepend(p.name, &crate::proof_kernel::aver_generated::kernel::check::placeNames(&rest)))
}

/// Whether some hypothesis mentions one of the names.
#[inline(always)]
pub fn hypsMention(
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    names @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => false, [h, rest] => (crate::proof_kernel::aver_generated::kernel::check::anyIn(&aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::subst::freeVars(h.eqn.lhs), &crate::proof_kernel::aver_generated::kernel::subst::freeVars(h.eqn.rhs)), names) || crate::proof_kernel::aver_generated::kernel::check::hypsMention(&rest, names)))
}

/// Whether one of ns is among names.
#[inline(always)]
pub fn anyIn(ns @ _: &aver_rt::AverList<AverStr>, names @ _: &aver_rt::AverList<AverStr>) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ns.clone(), [] => false, [n, rest] => (names.contains(&n) || crate::proof_kernel::aver_generated::kernel::check::anyIn(&rest, names)))
}

/// Names a case may not take for its pattern variables: those of the claim and of the hypotheses.
#[inline(always)]
pub fn takenNames(
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_rt::AverList::concat(
        &aver_rt::AverList::concat(
            &crate::proof_kernel::aver_generated::kernel::subst::freeVars(claim.lhs),
            &crate::proof_kernel::aver_generated::kernel::subst::freeVars(claim.rhs),
        ),
        &crate::proof_kernel::aver_generated::kernel::check::hypNames(hs),
    )
}

/// Variables the hypotheses mention.
#[inline(always)]
pub fn hypNames(
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => aver_rt::AverList::empty(), [h, rest] => aver_rt::AverList::concat(&aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::subst::freeVars(h.eqn.lhs), &crate::proof_kernel::aver_generated::kernel::subst::freeVars(h.eqn.rhs)), &crate::proof_kernel::aver_generated::kernel::check::hypNames(&rest)))
}

/// Every pattern is [], [h, ..t] or a constructor, and [] comes with [h, ..t].
#[inline(always)]
pub fn armsAreConstructors(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    (crate::proof_kernel::aver_generated::kernel::check::allConstructorPatterns(arms)
        && (crate::proof_kernel::aver_generated::kernel::check::hasNil(arms)
            == crate::proof_kernel::aver_generated::kernel::check::hasCons(arms)))
}

/// No literal, wildcard or name pattern.
#[inline(always)]
pub fn allConstructorPatterns(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(arms.clone(), [] => true, [a, rest] => (crate::proof_kernel::aver_generated::kernel::check::isConstructorPattern(&a.pattern) && crate::proof_kernel::aver_generated::kernel::check::allConstructorPatterns(&rest)))
}

/// [], [h, ..t] or a constructor.
pub fn isConstructorPattern(
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match p.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PNil => true,
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => true,
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, ns) => true,
        _ => false,
    }
}

/// An arm for the empty list.
#[inline(always)]
pub fn hasNil(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(arms.clone(), [] => false, [a, rest] => ((a.pattern == crate::proof_kernel::aver_generated::kernel::term::Pat::PNil) || crate::proof_kernel::aver_generated::kernel::check::hasNil(&rest)))
}

/// An arm for a nonempty list.
#[inline(always)]
pub fn hasCons(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(arms.clone(), [] => false, [a, rest] => (crate::proof_kernel::aver_generated::kernel::check::isCons(&a.pattern) || crate::proof_kernel::aver_generated::kernel::check::hasCons(&rest)))
}

/// A [h, ..t] pattern.
pub fn isCons(p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match p.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => true,
        _ => false,
    }
}

/// Fresh names for the arm's pattern; the claim at the pattern, under one hypothesis per recursive call at that call's arguments.
pub fn inductCase(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    j @ _: aver_rt::AverInt,
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Arm,
    c @ _: &crate::proof_kernel::aver_generated::kernel::proof::Case,
    at @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    taken @ _: &aver_rt::AverList<AverStr>,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let names @ _ = crate::proof_kernel::aver_generated::kernel::subst::patNames(&a.pattern);
    match (
        (aver_rt::AverInt::from_i64(names.len() as i64)
            == aver_rt::AverInt::from_i64(c.binders.len() as i64)),
        crate::proof_kernel::aver_generated::kernel::check::freshAll(
            &c.binders,
            taken,
            env,
            &aver_rt::AverList::empty(),
        ),
    ) {
        (false, _) => crate::proof_kernel::aver_generated::kernel::check::refuseCase(
            path,
            AverStr::from("wrong number of names"),
        ),
        (_, false) => crate::proof_kernel::aver_generated::kernel::check::refuseCase(
            path,
            AverStr::from("the names are not fresh"),
        ),
        _ => crate::proof_kernel::aver_generated::kernel::check::inductCaseNamed(
            d,
            j,
            a,
            c.clone(),
            &crate::proof_kernel::aver_generated::kernel::check::namesToVars(&c.binders),
            at,
            claim,
            vr,
            carried,
            env,
            path,
        ),
    }
}

/// A refusal of an induction case, naming it.
pub fn refuseCase(path @ _: AverStr, why @ _: AverStr) -> Result<(), AverStr> {
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

/// No name is taken, a given, a module-level binding, or repeated.
#[inline(always)]
pub fn freshAll(
    bs @ _: &aver_rt::AverList<AverStr>,
    taken @ _: &aver_rt::AverList<AverStr>,
    env @ _: &Env,
    before @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(bs.clone(), [] => true, [b, rest] => ((!((taken.contains(&b) || env.givens.contains(&b)) || (crate::proof_kernel::aver_generated::kernel::check::isConst(b.clone(), &env.consts) || before.contains(&b)))) && crate::proof_kernel::aver_generated::kernel::check::freshAll(&rest, taken, env, &aver_rt::AverList::prepend(b, &before.clone()))))
}

/// Whether n names a module-level binding.
#[inline(always)]
pub fn isConst(
    n @ _: AverStr,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => false, [c, rest] => ((c.name == n) || crate::proof_kernel::aver_generated::kernel::check::isConst(n, &rest)))
}

/// Each name as a variable.
#[inline(always)]
pub fn namesToVars(
    ns @ _: &aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ns.clone(), [] => aver_rt::AverList::empty(), [n, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::Term::TVar(n), &crate::proof_kernel::aver_generated::kernel::check::namesToVars(&rest)))
}

/// The goal and the carried hypotheses at the arm's pattern, and the hypotheses of its recursive calls; then the case's proof must prove the goal.
pub fn inductCaseNamed(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    j @ _: aver_rt::AverInt,
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Arm,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Case,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    at @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let value @ _ =
        crate::proof_kernel::aver_generated::kernel::check::patTerm(&a.pattern, ys, path.clone())?;
    let here @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::term::bind(vr.v.clone(), &value),
    ]);
    let goal @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(
        claim,
        &here,
        path.clone(),
    )?;
    let scope @ _ = crate::proof_kernel::aver_generated::kernel::check::withHyps(
        env,
        &aver_rt::AverList::concat(
            &crate::proof_kernel::aver_generated::kernel::check::hypsAt(
                carried,
                &here,
                path.clone(),
            )?,
            &env.hyps.clone(),
        ),
    );
    let names @ _ = crate::proof_kernel::aver_generated::kernel::subst::patNames(&a.pattern);
    let rename @ _ = aver_rt::AverList::concat(
        &crate::proof_kernel::aver_generated::kernel::check::zipBind(&names, ys),
        &crate::proof_kernel::aver_generated::kernel::check::withoutNames(
            crate::proof_kernel::aver_generated::kernel::check::zipBind(&d.params, &at.args),
            names,
        ),
    );
    let calls @ _ = crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
        a.body,
        d.name.clone(),
        aver_rt::AverList::empty(),
    );
    match (
        (aver_rt::AverInt::from_i64(calls.len() as i64)
            == aver_rt::AverInt::from_i64(c.ihs.len() as i64)),
        (aver_rt::AverInt::from_i64(c.carry.len() as i64)
            == aver_rt::AverInt::from_i64(c.ihs.len() as i64)),
    ) {
        (false, _) => crate::proof_kernel::aver_generated::kernel::check::refuseCase(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(61)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(aver_rt::AverInt::from_i64(calls.len() as i64)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" recursive calls, "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(aver_rt::AverInt::from_i64(c.ihs.len() as i64)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(" hypotheses"));
                __b
            }),
        ),
        (_, false) => crate::proof_kernel::aver_generated::kernel::check::refuseCase(
            path,
            AverStr::from("one list of carried proofs per hypothesis"),
        ),
        _ => crate::proof_kernel::aver_generated::kernel::check::caseProves(
            c.proof,
            &goal,
            crate::proof_kernel::aver_generated::kernel::check::withHyps(
                &scope.clone(),
                &aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::check::ihHyps(
                        &calls,
                        &c.ihs,
                        &c.carry,
                        &rename,
                        j,
                        claim,
                        vr,
                        carried,
                        &scope,
                        path.clone(),
                    )?,
                    &scope.hyps.clone(),
                ),
            ),
            path,
        ),
    }
}

/// Each hypothesis at a substitution, under its own name.
#[inline(always)]
pub fn hypsAt(
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => Ok(aver_rt::AverList::empty()), [h, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: h.name, eqn: crate::proof_kernel::aver_generated::kernel::check::instantiate(&h.eqn, bs, path.clone())? }, &crate::proof_kernel::aver_generated::kernel::check::hypsAt(&rest, bs, path)?)))
}

/// Bindings for names a pattern does not shadow.
#[inline(always)]
pub fn withoutNames(
    mut bs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    names @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding> {
    let names @ _ = std::sync::Arc::new(names);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(bs, [] => { return aver_rt::AverList::empty(); }, [b, rest] => { if names.contains(&b.name) { {
            let __tco0 = rest;
            bs = __tco0;
            continue;
        } } else { return aver_rt::AverList::prepend(b, &crate::proof_kernel::aver_generated::kernel::check::withoutNames(rest, (*names).clone())); } })
    }
}

/// One hypothesis per recursive call, later ones in front: the claim at that call's arguments, once each carried hypothesis is proved there. A call named _ gives none.
pub fn ihHyps(
    calls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Call>,
    ihs @ _: &aver_rt::AverList<AverStr>,
    carry @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    >,
    rename @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    j @ _: aver_rt::AverInt,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    scope @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (calls.clone(), ihs.clone(), carry.clone());
        let (__lit0, __lit1, __lit2) = &__int_match_subject;
        if !(*__lit0).is_empty() && !(*__lit1).is_empty() && !(*__lit2).is_empty() {
            let Some((c, moreCalls)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((h, moreNames)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((ps, moreProofs)) = aver_rt::list_uncons_cloned(&(*__lit2)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            Ok(aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::check::ihHyps(
                    &moreCalls,
                    &moreNames,
                    &moreProofs,
                    rename,
                    j.clone(),
                    claim,
                    vr,
                    carried,
                    scope,
                    path.clone(),
                )?,
                &crate::proof_kernel::aver_generated::kernel::check::ihTaken(
                    &c, h, &ps, rename, j, claim, vr, carried, scope, path,
                )?,
            ))
        } else {
            Ok(aver_rt::AverList::empty())
        }
    }
}

/// The hypothesis of one call, unless it is named _.
pub fn ihTaken(
    c @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    h @ _: AverStr,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    rename @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    j @ _: aver_rt::AverInt,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    scope @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        (&*h == "_"),
        (aver_rt::AverInt::from_i64(ps.len() as i64)
            == aver_rt::AverInt::from_i64(carried.len() as i64)),
    ) {
        (true, _) => Ok(aver_rt::AverList::empty()),
        (_, false) => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(55)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from(": one proof per carried hypothesis"));
            __b
        })),
        _ => crate::proof_kernel::aver_generated::kernel::check::ihHolds(
            &crate::proof_kernel::aver_generated::kernel::check::ihAt(
                c,
                rename,
                j,
                vr,
                path.clone(),
            )?,
            h,
            ps,
            claim,
            carried,
            scope,
            path,
        ),
    }
}

/// Each carried hypothesis proved at the call's arguments; then the claim there.
pub fn ihHolds(
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    h @ _: AverStr,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    scope @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::carriedHold(
        carried.clone(),
        ps.clone(),
        bs,
        scope,
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&AverStr::from("/"));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
            __b
        }),
    )?;
    Ok(aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::proof::Hyp {
            name: h,
            eqn: crate::proof_kernel::aver_generated::kernel::check::instantiate(claim, bs, path)?,
        },
    ]))
}

/// The varied givens bound to the call's arguments.
#[inline(always)]
pub fn ihAt(
    c @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    rename @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    j @ _: aver_rt::AverInt,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    if crate::proof_kernel::aver_generated::kernel::check::anyIn(
        &crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(
            &crate::proof_kernel::aver_generated::kernel::check::readArgs(
                c.args.clone(),
                j.clone(),
                vr.general.clone(),
                aver_rt::AverInt::from_i64(0),
            ),
        ),
        &c.inner,
    ) {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(73)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from(
                ": a recursive call reads a name an inner match binds",
            ));
            __b
        }))
    } else {
        crate::proof_kernel::aver_generated::kernel::check::ihBindings(
            &crate::proof_kernel::aver_generated::kernel::subst::substAll(&c.args, rename)?,
            j,
            vr,
            path,
        )
    }
}

/// The arguments a hypothesis reads: the one at place j and those at the varied places.
#[inline(always)]
pub fn readArgs(
    mut xs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut j @ _: aver_rt::AverInt,
    ps @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Place>,
    mut k @ _: aver_rt::AverInt,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    let ps @ _ = std::sync::Arc::new(ps);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(xs, [] => { return aver_rt::AverList::empty(); }, [x, rest] => { if ((k == j) || crate::proof_kernel::aver_generated::kernel::check::atPlace(&*ps, k.clone())) { return aver_rt::AverList::prepend(x, &crate::proof_kernel::aver_generated::kernel::check::readArgs(rest, j, (*ps).clone(), k.add(&aver_rt::AverInt::from_i64(1)))); } else { {
            let __tco0 = rest;
            let __tco3 = k.add(&aver_rt::AverInt::from_i64(1));
            xs = __tco0;
            k = __tco3;
            continue;
        } } })
    }
}

/// Whether a varied given sits at place k.
#[inline(always)]
pub fn atPlace(
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Place>,
    k @ _: aver_rt::AverInt,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ps.clone(), [] => false, [p, rest] => ((p.at == k) || crate::proof_kernel::aver_generated::kernel::check::atPlace(&rest, k)))
}

/// The given at the matched place and the varied givens, each bound to the call's argument at its place.
#[inline(always)]
pub fn ihBindings(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    j @ _: aver_rt::AverInt,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::induct::nthTerm(args.clone(), j) {
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(61)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from(": a recursive call has too few arguments"));
            __b
        })),
        Some(at @ _) => Ok(aver_rt::AverList::prepend(
            crate::proof_kernel::aver_generated::kernel::term::bind(vr.v.clone(), &at),
            &crate::proof_kernel::aver_generated::kernel::check::placeBindings(&vr.general, args),
        )),
    }
}

/// Each varied given bound to the call's argument at its place.
#[inline(always)]
pub fn placeBindings(
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Place>,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ps.clone(), [] => aver_rt::AverList::empty(), [p, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::bind(p.name.clone(), &crate::proof_kernel::aver_generated::kernel::induct::nthTerm(args.clone(), p.at).unwrap_or(crate::proof_kernel::aver_generated::kernel::term::Term::TVar(p.name.clone()))), &crate::proof_kernel::aver_generated::kernel::check::placeBindings(&rest, args)))
}

/// The case's proof proves exactly the goal.
pub fn caseProves(
    mut p @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    goal @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut env @ _: Env,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let got @ _ =
        crate::proof_kernel::aver_generated::kernel::check::conclude(p, env, path.clone())?;
    if (&(got) == goal) {
        Ok(())
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(59)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from(": the case proves a different equation"));
            __b
        }))
    }
}

/// Induction on a given of list type: the claim at [], and at List.prepend(head, tail) under a hypothesis, the claim at tail.
pub fn listInduct(
    v @ _: AverStr,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut n @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    names @ _: &aver_rt::AverList<AverStr>,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1, __pat2) = (
            env.lists.contains(&v),
            crate::proof_kernel::aver_generated::kernel::check::hypsMention(
                &env.hyps,
                &aver_rt::AverList::from_vec(vec![v.clone()]),
            ),
            names.clone(),
        );
        match __pat0 {
            false => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(44)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" is not a given of list type"));
                    __b
                }),
            ),
            _ => {
                if __pat1 {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("a hypothesis in scope mentions "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                            __b
                        }),
                    )
                } else {
                    {
                        let __list_subject = __pat2;
                        if let Some((h, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat3;
                                if let Some((t, __pat4)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    {
                                        let __list_subject = __pat4;
                                        if let Some((ih, __pat5)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            {
                                                let __list_subject = __pat5;
                                                if __list_subject.is_empty() {
                                                    if crate::proof_kernel::aver_generated::kernel::check::freshAll(&aver_rt::AverList::from_vec(vec![h.clone(), t.clone()]), &crate::proof_kernel::aver_generated::kernel::check::takenNames(claim.clone(), &env.hyps), env, &aver_rt::AverList::empty()) { crate::proof_kernel::aver_generated::kernel::check::listCases(v, claim, n, h, t, ih, c, env, path) } else { crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("the names are not fresh")) }
                                                } else {
                                                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("malformed list induction"))
                                                }
                                            }
                                        } else {
                                            crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("malformed list induction"))
                                        }
                                    }
                                } else {
                                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                                        path,
                                        AverStr::from("malformed list induction"),
                                    )
                                }
                            }
                        } else {
                            crate::proof_kernel::aver_generated::kernel::check::refuse(
                                path,
                                AverStr::from("malformed list induction"),
                            )
                        }
                    }
                }
            }
        }
    }
}

/// The empty-list case, then the cell case under the claim at the tail.
pub fn listCases(
    v @ _: AverStr,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut n @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    h @ _: AverStr,
    t @ _: AverStr,
    ih @ _: AverStr,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::caseProves(
        n,
        &crate::proof_kernel::aver_generated::kernel::check::instantiate(
            claim,
            &aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::term::bind(
                    v.clone(),
                    &crate::proof_kernel::aver_generated::kernel::term::Term::TList(
                        aver_rt::AverList::empty(),
                    ),
                ),
            ]),
            path.clone(),
        )?,
        env.clone(),
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from("/nil"));
            __b
        }),
    )?;
    let atTail @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(
        claim,
        &aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::term::bind(
                v.clone(),
                &crate::proof_kernel::aver_generated::kernel::term::Term::TVar(t.clone()),
            ),
        ]),
        path.clone(),
    )?;
    let goal @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(
        claim,
        &aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::term::bind(
                v,
                &crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                    AverStr::from("List.prepend"),
                    aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(h),
                        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(t),
                    ]),
                ),
            ),
        ]),
        path.clone(),
    )?;
    crate::proof_kernel::aver_generated::kernel::check::caseProves(
        c,
        &goal,
        crate::proof_kernel::aver_generated::kernel::check::withHyps(
            env,
            &aver_rt::AverList::prepend(
                crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                    name: ih,
                    eqn: atTail,
                },
                &env.hyps.clone(),
            ),
        ),
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from("/cons"));
            __b
        }),
    )?;
    Ok(claim.clone())
}

/// Induction on a given of type Int down to zero: the claim where v <= 0 (hypothesis g), and where v > 0 under hypothesis ih, the claim at v - 1. The hypotheses carried hold at v - 1 first; any other one that mentions v is out of scope.
pub fn intInduct(
    v @ _: AverStr,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    g @ _: AverStr,
    mut base @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    names @ _: &aver_rt::AverList<AverStr>,
    carry @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    ih @ _: AverStr,
    mut step @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        env.ints.contains(&v),
        (aver_rt::AverInt::from_i64(names.len() as i64)
            == aver_rt::AverInt::from_i64(carry.len() as i64)),
    ) {
        (false, _) => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                    __b
                };
                __b.push_str(&AverStr::from(" is not a given of type Int"));
                __b
            }),
        ),
        (_, false) => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("one proof per carried hypothesis"),
        ),
        _ => crate::proof_kernel::aver_generated::kernel::check::intCases(
            v,
            claim,
            g,
            base,
            &crate::proof_kernel::aver_generated::kernel::check::namedHyps(
                names,
                &env.hyps,
                path.clone(),
            )?,
            carry,
            ih,
            step,
            env,
            path,
        ),
    }
}

/// No definitions, laws or hypotheses, and these givens of type Int.
pub fn intsEnv(ns @ _: &aver_rt::AverList<AverStr>) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: aver_rt::AverList::empty(),
        consts: aver_rt::AverList::empty(),
        laws: aver_rt::AverList::empty(),
        hyps: aver_rt::AverList::empty(),
        finite: aver_rt::AverList::empty(),
        lists: aver_rt::AverList::empty(),
        ints: ns.clone(),
        givens: ns.clone(),
    }
}

/// The case v <= 0; then, where v > 0, each carried hypothesis at v - 1, and the claim under the claim at v - 1. A case sees the carried hypotheses (the last named innermost), then g, then those that do not mention v.
pub fn intCases(
    v @ _: AverStr,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    g @ _: AverStr,
    mut base @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    carry @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    ih @ _: AverStr,
    mut step @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let guard @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
        AverStr::from("<="),
        std::sync::Arc::new(
            crate::proof_kernel::aver_generated::kernel::term::Term::TVar(v.clone()),
        ),
        std::sync::Arc::new(
            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                aver_rt::AverInt::from_i64(0),
            ),
        ),
    );
    let down @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::term::bind(
            v.clone(),
            &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                AverStr::from("-"),
                std::sync::Arc::new(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TVar(v.clone()),
                ),
                std::sync::Arc::new(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                        aver_rt::AverInt::from_i64(1),
                    ),
                ),
            ),
        ),
    ]);
    let kept @ _ =
        crate::proof_kernel::aver_generated::kernel::check::notMentioning(env.hyps.clone(), v);
    let inner @ _ = cs.reverse();
    crate::proof_kernel::aver_generated::kernel::check::caseProves(
        base,
        claim,
        crate::proof_kernel::aver_generated::kernel::check::withHyps(
            env,
            &aver_rt::AverList::concat(
                &inner.clone(),
                &aver_rt::AverList::prepend(
                    crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                        name: g.clone(),
                        eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
                            lhs: guard.clone(),
                            rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                true,
                            ),
                        },
                    },
                    &kept.clone(),
                ),
            ),
        ),
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from("/base"));
            __b
        }),
    )?;
    let above @ _ = crate::proof_kernel::aver_generated::kernel::check::withHyps(
        env,
        &aver_rt::AverList::concat(
            &inner,
            &aver_rt::AverList::prepend(
                crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                    name: g,
                    eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: guard,
                        rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false),
                    },
                },
                &kept,
            ),
        ),
    );
    crate::proof_kernel::aver_generated::kernel::check::carriedHold(
        cs.clone(),
        carry.clone(),
        &down,
        &above,
        path.clone(),
    )?;
    crate::proof_kernel::aver_generated::kernel::check::caseProves(
        step,
        claim,
        crate::proof_kernel::aver_generated::kernel::check::withHyps(
            &above.clone(),
            &aver_rt::AverList::prepend(
                crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                    name: ih,
                    eqn: crate::proof_kernel::aver_generated::kernel::check::instantiate(
                        claim,
                        &down,
                        path.clone(),
                    )?,
                },
                &above.hyps.clone(),
            ),
        ),
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from("/step"));
            __b
        }),
    )?;
    Ok(claim.clone())
}

/// The hypotheses of these names, as they stand in scope.
#[inline(always)]
pub fn namedHyps(
    names @ _: &aver_rt::AverList<AverStr>,
    scope @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(names.clone(), [] => Ok(aver_rt::AverList::empty()), [n, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: n.clone(), eqn: crate::proof_kernel::aver_generated::kernel::check::hyp(n, scope.clone(), path.clone())? }, &crate::proof_kernel::aver_generated::kernel::check::namedHyps(&rest, scope, path)?)))
}

/// The hypotheses that do not mention v.
#[inline(always)]
pub fn notMentioning(
    mut hs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    mut v @ _: AverStr,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(hs, [] => { return aver_rt::AverList::empty(); }, [h, rest] => { if crate::proof_kernel::aver_generated::kernel::check::hypsMention(&aver_rt::AverList::from_vec(vec![h.clone()]), &aver_rt::AverList::from_vec(vec![v.clone()])) { {
            let __tco0 = rest;
            hs = __tco0;
            continue;
        } } else { return aver_rt::AverList::prepend(h, &crate::proof_kernel::aver_generated::kernel::check::notMentioning(rest, v)); } })
    }
}

/// The statements of checked facts, as laws a step may cite.
#[inline(always)]
pub fn factLaws(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => aver_rt::AverList::empty(), [f, rest] => aver_rt::AverList::prepend(f.law, &crate::proof_kernel::aver_generated::kernel::check::factLaws(&rest)))
}
