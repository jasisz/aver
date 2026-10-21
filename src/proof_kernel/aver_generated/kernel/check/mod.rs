#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Env {
    pub defs: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    pub consts: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    pub laws: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
    pub hyps: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    pub vars: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    pub sums: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Sum>,
    pub recs: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Rec>,
    pub sigs: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::FnSig>,
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
            .then_with(|| self.hyps.cmp(&other.hyps))
            .then_with(|| self.laws.cmp(&other.laws))
            .then_with(|| self.recs.cmp(&other.recs))
            .then_with(|| self.sigs.cmp(&other.sigs))
            .then_with(|| self.sums.cmp(&other.sums))
            .then_with(|| self.vars.cmp(&other.vars))
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
                format!("vars: {}", self.vars.aver_display_inner()),
                format!("sums: {}", self.sums.aver_display_inner()),
                format!("recs: {}", self.recs.aver_display_inner()),
                format!("sigs: {}", self.sigs.aver_display_inner())
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
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PRefl(
                        __t_fields,
                    ) => {
                        let t = __t_fields.0.clone();
                        return Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                            lhs: t.clone(),
                            rhs: t,
                        });
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PSymm(
                        __q_fields,
                    ) => {
                        let q = __q_fields.0.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::flip(
                            &crate::proof_kernel::aver_generated::kernel::check::conclude(
                                q,
                                env,
                                (path + &AverStr::from("/symm")),
                            )?,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PTrans(
                        __ts_fields,
                    ) => {
                        let ts = __ts_fields.0.clone();
                        let ps = __ts_fields.1.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::trans(
                            &ts, &ps, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PCongr(
                        __c_fields,
                    ) => {
                        let c = __c_fields.0.clone();
                        let q = __c_fields.1.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::congr(
                            c, q, env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PUnfold(
                        __f_fields,
                    ) => {
                        let f = __f_fields.0.clone();
                        let k = __f_fields.1.clone();
                        let xs = __f_fields.2.clone();
                        let ys = __f_fields.3.clone();
                        let pre = __f_fields.4.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::unfold(
                            f, k, &xs, &ys, &pre, env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PConst(
                        __n_fields,
                    ) => {
                        let n = __n_fields.0.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::constant(
                            n, env.consts, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PArm(__k_fields) => {
                        let k = __k_fields.0.clone();
                        let ys = __k_fields.1.clone();
                        let t = __k_fields.2.clone();
                        let q = __k_fields.3.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::armStep(
                            k, &ys, t, q, env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PProj(
                        __t_fields,
                    ) => {
                        let t = __t_fields.0.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::proj(
                            t, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PCell(
                        __t_fields,
                    ) => {
                        let t = __t_fields.0.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::cell(t, path);
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PHyp(__h_fields) => {
                        let h = __h_fields.0.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::hyp(
                            h, env.hyps, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PRule(
                        __id_fields,
                    ) => {
                        let id = __id_fields.0.clone();
                        let bs = __id_fields.1.clone();
                        let ps = __id_fields.2.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::rule(
                            id, &bs, &ps, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PLaw(__k_fields) => {
                        let k = __k_fields.0.clone();
                        let bs = __k_fields.1.clone();
                        let ps = __k_fields.2.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::lawStep(
                            k, &bs, &ps, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PCompute(
                        __a_fields,
                    ) => {
                        let a = __a_fields.0.clone();
                        let b = __a_fields.1.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::compute(
                            a, b, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PCases(
                        __on_fields,
                    ) => {
                        let on = __on_fields.0.clone();
                        let h = __on_fields.1.clone();
                        let t = __on_fields.2.clone();
                        let f = __on_fields.3.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::cases(
                            on, h, t, f, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PSplit(
                        __on_fields,
                    ) => {
                        let on = __on_fields.0.clone();
                        let h = __on_fields.1.clone();
                        let lits = __on_fields.2.clone();
                        let cs = __on_fields.3.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::split(
                            &on, h, &lits, &cs, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PHave(
                        __h_fields,
                    ) => {
                        let h = __h_fields.0.clone();
                        let fact = __h_fields.1.clone();
                        let q = __h_fields.2.clone();
                        let body = __h_fields.3.clone();
                        __MutualTco1::Have(h, fact, q, body, env, path)
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PEnum(
                        __v_fields,
                    ) => {
                        let v = __v_fields.0.clone();
                        let l = __v_fields.1.clone();
                        let r = __v_fields.2.clone();
                        let cs = __v_fields.3.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::enumStep(
                            v, l, r, &cs, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PAbsurd(
                        __q_fields,
                    ) => {
                        let q = __q_fields.0.clone();
                        let l = __q_fields.1.clone();
                        let r = __q_fields.2.clone();
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
                            l,
                            r,
                            path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PInduct(
                        __f_fields,
                    ) => {
                        let f = __f_fields.0.clone();
                        let xs = __f_fields.1.clone();
                        let l = __f_fields.2.clone();
                        let r = __f_fields.3.clone();
                        let carry = __f_fields.4.clone();
                        let cs = __f_fields.5.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::induct(
                            f,
                            &xs,
                            &crate::proof_kernel::aver_generated::kernel::term::Eqn {
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
                        __v_fields,
                    ) => {
                        let v = __v_fields.0.clone();
                        let l = __v_fields.1.clone();
                        let r = __v_fields.2.clone();
                        let carry = __v_fields.3.clone();
                        let n = __v_fields.4.clone();
                        let h = __v_fields.5.clone();
                        let t = __v_fields.6.clone();
                        let general = __v_fields.7.clone();
                        let ihs = __v_fields.8.clone();
                        let c = __v_fields.9.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::listInduct(
                            v,
                            crate::proof_kernel::aver_generated::kernel::term::Eqn {
                                lhs: l,
                                rhs: r,
                            },
                            &carry,
                            n,
                            h,
                            t,
                            &general,
                            &ihs,
                            c,
                            &env,
                            path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PListCases(
                        __v_fields,
                    ) => {
                        let v = __v_fields.0.clone();
                        let l = __v_fields.1.clone();
                        let r = __v_fields.2.clone();
                        let n = __v_fields.3.clone();
                        let h = __v_fields.4.clone();
                        let t = __v_fields.5.clone();
                        let c = __v_fields.6.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::splitList(
                            v,
                            crate::proof_kernel::aver_generated::kernel::term::Eqn {
                                lhs: l,
                                rhs: r,
                            },
                            n,
                            h,
                            t,
                            c,
                            &env,
                            path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PLenInduct(
                        __v_fields,
                    ) => {
                        let v = __v_fields.0.clone();
                        let l = __v_fields.1.clone();
                        let r = __v_fields.2.clone();
                        let carry = __v_fields.3.clone();
                        let general = __v_fields.4.clone();
                        let n = __v_fields.5.clone();
                        let hn = __v_fields.6.clone();
                        let ih = __v_fields.7.clone();
                        let c = __v_fields.8.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::lenInduct(
                            v,
                            &crate::proof_kernel::aver_generated::kernel::term::Eqn {
                                lhs: l,
                                rhs: r,
                            },
                            &carry,
                            &general,
                            &aver_rt::AverList::from_vec(vec![n, hn, ih]),
                            c,
                            &env,
                            path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PRing(
                        __l_fields,
                    ) => {
                        let l = __l_fields.0.clone();
                        let r = __l_fields.1.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::ring(
                            l, r, &env, path,
                        );
                    }
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PLinear(
                        __g_fields,
                    ) => {
                        let g = __g_fields.0.clone();
                        let v = __g_fields.1.clone();
                        let hs = __g_fields.2.clone();
                        let ws = __g_fields.3.clone();
                        let cs = __g_fields.4.clone();
                        return crate::proof_kernel::aver_generated::kernel::check::linear(
                            g, v, &hs, &ws, &cs, env, path,
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
                        rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                            aver_rt::Shared::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                    true,
                                ),
                            ),
                        ),
                    })
                {
                    __MutualTco1::Conclude(
                        body,
                        crate::proof_kernel::aver_generated::kernel::check::withHyp(
                            &env, h, fact, true,
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
    SplitCases(
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
        aver_rt::AverList<
            aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
        >,
        aver_rt::AverList<
            aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
        >,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
        AverStr,
    ),
    SplitNext(
        crate::proof_kernel::aver_generated::kernel::term::Eqn,
        crate::proof_kernel::aver_generated::kernel::proof::SplitCase,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
        aver_rt::AverList<
            aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
        >,
        aver_rt::AverList<
            aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
        >,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
        AverStr,
    ),
}

fn __mutual_tco_trampoline_2(
    mut __state: __MutualTco2,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::SplitCases(
                mut cs @ _,
                mut hss @ _,
                mut vss @ _,
                mut before @ _,
                mut path @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                aver_list_match!(cs, [] => { return crate::proof_kernel::aver_generated::kernel::check::splitDone(&before, path) }, [c, rest] => { if crate::proof_kernel::aver_generated::kernel::check::freshAll(&c.binders, &aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::subst::freeVars((*on).clone()), &crate::proof_kernel::aver_generated::kernel::check::hypNames(&env.hyps)), &*env, &aver_rt::AverList::empty()) { __MutualTco2::SplitNext(crate::proof_kernel::aver_generated::kernel::check::conclude(c.proof.clone(), crate::proof_kernel::aver_generated::kernel::check::withVars(&crate::proof_kernel::aver_generated::kernel::check::withHyps(&*env, &aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::check::firstHyps(&hss), &env.hyps.clone())), &crate::proof_kernel::aver_generated::kernel::check::firstVars(&vss)), aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&AverStr::from("/split.")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.ctor)))); __b }))?, c, rest, { let __n = aver_rt::clamp_list_count(&(aver_rt::AverInt::from_i64(1))); (hss).drop_first(__n) }, { let __n = aver_rt::clamp_list_count(&(aver_rt::AverInt::from_i64(1))); (vss).drop_first(__n) }, before, path) } else { return crate::proof_kernel::aver_generated::kernel::check::refuse(path, aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("the names of case ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.ctor)))); __b }; __b.push_str(&AverStr::from(" are not fresh")); __b })) } })
            }
            __MutualTco2::SplitNext(
                mut e @ _,
                mut c @ _,
                mut rest @ _,
                mut hss @ _,
                mut vss @ _,
                mut before @ _,
                mut path @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                match (
                    crate::proof_kernel::aver_generated::kernel::check::anyIn(
                        &aver_rt::AverList::concat(
                            &crate::proof_kernel::aver_generated::kernel::subst::freeVars(
                                e.lhs.clone(),
                            ),
                            &crate::proof_kernel::aver_generated::kernel::subst::freeVars(
                                e.rhs.clone(),
                            ),
                        ),
                        &c.binders,
                    ),
                    ((aver_rt::AverInt::from_i64(before.len() as i64)
                        == aver_rt::AverInt::from_i64(0))
                        || (before == aver_rt::AverList::from_vec(vec![e.clone()]))),
                ) {
                    (true, _) => {
                        return crate::proof_kernel::aver_generated::kernel::check::refuse(
                            path,
                            aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(60))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("case "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(c.ctor),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(
                                    " proves an equation about its own names",
                                ));
                                __b
                            }),
                        );
                    }
                    (_, false) => {
                        return crate::proof_kernel::aver_generated::kernel::check::refuse(
                            path,
                            AverStr::from("the cases prove different equations"),
                        );
                    }
                    _ => __MutualTco2::SplitCases(
                        rest,
                        hss,
                        vss,
                        aver_rt::AverList::from_vec(vec![e]),
                        path,
                    ),
                }
            }
        };
    }
}

/// Each case under its hypotheses and its typed names, fresh for on, the hypotheses, the variables and the constants; every case proves the same equation, which names none of them.
pub fn splitCases(
    cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    hss @ _: aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    >,
    vss @ _: aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    >,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    before @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::SplitCases(cs, hss, vss, before, path),
        &on,
        &env,
    )
}

/// One case's equation: it names none of the case's names, and it is the one every earlier case proved.
pub fn splitNext(
    e @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    c @ _: crate::proof_kernel::aver_generated::kernel::proof::SplitCase,
    rest @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    hss @ _: aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    >,
    vss @ _: aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    >,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    before @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::SplitNext(e, c, rest, hss, vss, before, path),
        &on,
        &env,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
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

fn __mutual_tco_trampoline_3(
    mut __state: __MutualTco3,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    env @ _: &Env,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::EnumCases(
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
                        __MutualTco3::EnumCase(v, x, c, xs, rest, path, i)
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
            __MutualTco3::EnumCase(
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
                    crate::proof_kernel::aver_generated::kernel::term::bind(v.clone(), x),
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
                    (*claim).clone(),
                    &at,
                    path.clone(),
                )?;
                if (got == want) {
                    __MutualTco3::EnumCases(
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
    __mutual_tco_trampoline_3(__MutualTco3::EnumCases(v, vals, cs, path, i), &claim, &env)
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
    __mutual_tco_trampoline_3(
        __MutualTco3::EnumCase(v, x, c, xs, rest, path, i),
        &claim,
        &env,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco4 {
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

fn __mutual_tco_trampoline_4(
    mut __state: __MutualTco4,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
) -> Result<(), AverStr> {
    loop {
        __state = match __state {
            __MutualTco4::PremisesHold(mut wanted @ _, mut ps @ _, mut path @ _, mut i @ _) => {
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
                        __MutualTco4::PremiseHolds(w, p, ws, rest, path, i)
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
            __MutualTco4::PremiseHolds(
                mut w @ _,
                mut p @ _,
                mut ws @ _,
                mut rest @ _,
                mut path @ _,
                mut i @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                let want @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(
                    w,
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
                    __MutualTco4::PremisesHold(
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
    __mutual_tco_trampoline_4(__MutualTco4::PremisesHold(wanted, ps, path, i), &bs, &env)
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
    __mutual_tco_trampoline_4(
        __MutualTco4::PremiseHolds(w, p, ws, rest, path, i),
        &bs,
        &env,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco5 {
    InductCases(
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::ArmCalls>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
        AverStr,
        aver_rt::AverInt,
    ),
    InductNext(
        Result<(), AverStr>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::ArmCalls>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
        AverStr,
        aver_rt::AverInt,
    ),
}

fn __mutual_tco_trampoline_5(
    mut __state: __MutualTco5,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    at @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    taken @ _: &aver_rt::AverList<AverStr>,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    env @ _: &Env,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    loop {
        __state = match __state {
            __MutualTco5::InductCases(mut acs @ _, mut cs @ _, mut path @ _, mut i @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                {
                    let __int_match_subject = (acs, cs);
                    let (__lit0, __lit1) = &__int_match_subject;
                    if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
                        let Some((a, moreArms)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        let Some((c, moreCases)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                            unreachable!("Aver Rust codegen: tuple element list mismatch")
                        };
                        __MutualTco5::InductNext(
                            crate::proof_kernel::aver_generated::kernel::check::inductCase(
                                &*d,
                                &*r,
                                &a,
                                c,
                                &*at,
                                (*claim).clone(),
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
            __MutualTco5::InductNext(
                mut done @ _,
                mut acs @ _,
                mut cs @ _,
                mut path @ _,
                mut i @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                done?;
                __MutualTco5::InductCases(acs, cs, path, i)
            }
        };
    }
}

/// Each arm with its case, in order; the result is the claim.
pub fn inductCases(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    acs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::ArmCalls>,
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
    __mutual_tco_trampoline_5(
        __MutualTco5::InductCases(acs, cs, path, i),
        &d,
        &r,
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
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    acs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::ArmCalls>,
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
    __mutual_tco_trampoline_5(
        __MutualTco5::InductNext(done, acs, cs, path, i),
        &d,
        &r,
        &at,
        &claim,
        &vr,
        &taken,
        &carried,
        &env,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco6 {
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

fn __mutual_tco_trampoline_6(
    mut __state: __MutualTco6,
    down @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
) -> Result<(), AverStr> {
    loop {
        __state = match __state {
            __MutualTco6::CarriedHold(mut cs @ _, mut proofs @ _, mut path @ _) => {
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
                        __MutualTco6::CarriedThen(
                            crate::proof_kernel::aver_generated::kernel::check::caseProves(
                                q,
                                &crate::proof_kernel::aver_generated::kernel::check::instantiate(
                                    c.eqn,
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
            __MutualTco6::CarriedThen(mut done @ _, mut cs @ _, mut proofs @ _, mut path @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                done?;
                __MutualTco6::CarriedHold(cs, proofs, path)
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
    __mutual_tco_trampoline_6(__MutualTco6::CarriedHold(cs, proofs, path), &down, &env)
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
    __mutual_tco_trampoline_6(
        __MutualTco6::CarriedThen(done, cs, proofs, path),
        &down,
        &env,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco7 {
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

fn __mutual_tco_trampoline_7(mut __state: __MutualTco7) -> Result<(), AverStr> {
    loop {
        __state = match __state {
            __MutualTco7::CheckFacts(mut fs @ _, mut earlier @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                aver_list_match!(fs, [] => { return Ok(()) }, [f, rest] => __MutualTco7::CheckFactThen(crate::proof_kernel::aver_generated::kernel::check::checkScript(&crate::proof_kernel::aver_generated::kernel::proof::Script { obligation: f.law.clone(), defs: aver_rt::AverList::empty(), consts: aver_rt::AverList::empty(), sums: aver_rt::AverList::empty(), recs: aver_rt::AverList::empty(), sigs: aver_rt::AverList::empty(), laws: earlier.clone(), facts: aver_rt::AverList::empty(), proof: f.proof }), f.law.clone(), rest, earlier))
            }
            __MutualTco7::CheckFactThen(
                mut done @ _,
                mut law @ _,
                mut rest @ _,
                mut earlier @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                match done {
                    Ok(k @ _) => __MutualTco7::CheckFacts(
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
    __mutual_tco_trampoline_7(__MutualTco7::CheckFacts(fs, earlier))
}

/// The rest of the facts, once this one checked; they may cite it.
pub fn checkFactThen(
    done @ _: Result<AverStr, AverStr>,
    law @ _: crate::proof_kernel::aver_generated::kernel::proof::Law,
    rest @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
    earlier @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
) -> Result<(), AverStr> {
    __mutual_tco_trampoline_7(__MutualTco7::CheckFactThen(done, law, rest, earlier))
}

/// No definitions, laws or hypotheses.
pub fn emptyEnv() -> Env {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: aver_rt::AverList::empty(),
        consts: aver_rt::AverList::empty(),
        laws: aver_rt::AverList::empty(),
        hyps: aver_rt::AverList::empty(),
        vars: aver_rt::AverList::empty(),
        sums: aver_rt::AverList::empty(),
        recs: aver_rt::AverList::empty(),
        sigs: aver_rt::AverList::empty(),
    }
}

/// What typing a term needs from the environment.
pub fn tyEnv(env @ _: &Env) -> crate::proof_kernel::aver_generated::kernel::types::TyEnv {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::types::TyEnv {
        vars: env.vars.clone(),
        defs: env.defs.clone(),
        consts: env.consts.clone(),
        sums: env.sums.clone(),
        recs: env.recs.clone(),
        sigs: env.sigs.clone(),
    }
}

/// The environment with these variables in front.
pub fn withVars(
    env @ _: &Env,
    vs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: env.defs.clone(),
        consts: env.consts.clone(),
        laws: env.laws.clone(),
        hyps: env.hyps.clone(),
        vars: aver_rt::AverList::concat(&vs.clone(), &env.vars.clone()),
        sums: env.sums.clone(),
        recs: env.recs.clone(),
        sigs: env.sigs.clone(),
    }
}

/// The type of a term, a refusal naming the step when it has none.
#[inline(always)]
pub fn typeOf(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::types::typeOf(
        t,
        crate::proof_kernel::aver_generated::kernel::check::tyEnv(env),
    ) {
        Ok(ty @ _) => Ok(ty),
        Err(why @ _) => Err(aver_rt::AverStr::from({
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
        })),
    }
}

/// The one check of every substitution a proof writes: each term fits the declared type of the variable it stands for.
#[inline(always)]
pub fn bound(
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    declared @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    env @ _: &Env,
    what @ _: AverStr,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::types::bindTyped(
        bs.clone(),
        declared.clone(),
        aver_rt::AverList::empty(),
        crate::proof_kernel::aver_generated::kernel::check::tyEnv(env),
        what,
    ) {
        Ok(b @ _) => Ok(()),
        Err(why @ _) => Err(aver_rt::AverStr::from({
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
        })),
    }
}

/// A definition's arguments: one per parameter, each of its parameter's type.
#[inline(always)]
pub fn argsFit(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (aver_rt::AverInt::from_i64(d.params.len() as i64)
        == aver_rt::AverInt::from_i64(xs.len() as i64))
    {
        crate::proof_kernel::aver_generated::kernel::check::bound(
            &crate::proof_kernel::aver_generated::kernel::check::zipBind(
                &crate::proof_kernel::aver_generated::kernel::types::paramsOf(d),
                xs,
            ),
            &d.params,
            env,
            d.name.clone(),
            path,
        )
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(72)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&AverStr::from("step "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&AverStr::from(": "));
                            __b
                        };
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
        }))
    }
}

/// The variables a pattern binds over a subject of this type, a refusal naming the step when it does not fit.
#[inline(always)]
pub fn patTypes(
    mut p @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
    mut subject @ _: crate::proof_kernel::aver_generated::kernel::term::Ty,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::types::patVars(
        p,
        subject,
        crate::proof_kernel::aver_generated::kernel::check::tyEnv(env),
    ) {
        Ok(vs @ _) => Ok(vs),
        Err(why @ _) => Err(aver_rt::AverStr::from({
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
        })),
    }
}

/// What a variable of list type holds, if it is one.
#[inline(always)]
pub fn listVar(
    v @ _: AverStr,
    env @ _: &Env,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Ty> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::types::typeOf(
        &crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(
            crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v),
        )),
        crate::proof_kernel::aver_generated::kernel::check::tyEnv(env),
    ) {
        Ok(t @ _) => crate::proof_kernel::aver_generated::kernel::types::elemOf(&t),
        Err(why @ _) => None,
    }
}

/// The head and the tail of a list of e.
pub fn cellVars(
    h @ _: AverStr,
    t @ _: AverStr,
    mut e @ _: crate::proof_kernel::aver_generated::kernel::term::Ty,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar> {
    crate::proof_kernel::cancel_checkpoint();
    aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::term::typed(h, e.clone()),
        crate::proof_kernel::aver_generated::kernel::term::typed(
            t,
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyList(aver_rt::Shared::new(
                crate::proof_kernel::aver_generated::kernel::term::Ty__TyList(e),
            )),
        ),
    ])
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
                aver_list_match!(__pat2, [] => { { let __list_subject = __pat1; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: a.clone(), rhs: a }) } else { crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("trans needs at least one step")) } } }, [b, moreTerms] => { { let __list_subject = __pat1; if let Some((p, moreSteps)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __arg13 @ _ = aver_rt::AverList::prepend(b.clone(), &moreTerms); crate::proof_kernel::aver_generated::kernel::check::transLink(a, b, p, &__arg13, &moreSteps, env, path, i) } } else { crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("trans needs at least one step")) } } })
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
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut b @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
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
            rhs: b,
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
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    rest @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
        lhs: a,
        rhs: rest.rhs.clone(),
    })
}

/// From a = b, c[a] = c[b]; the context holds exactly one hole.
#[inline(always)]
pub fn congr(
    mut c @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
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
            crate::proof_kernel::aver_generated::kernel::check::conclude(
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
    mut c @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut e @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        crate::proof_kernel::aver_generated::kernel::subst::subst(
            c.clone(),
            &aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::term::bind(AverStr::from("□"), e.lhs),
            ]),
        ),
        crate::proof_kernel::aver_generated::kernel::subst::subst(
            c,
            &aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::term::bind(AverStr::from("□"), e.rhs),
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

/// A record literal's field; a field of an update, which is the value written to it, or the base's field when the update does not write it.
pub fn proj(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match &t {
        crate::proof_kernel::aver_generated::kernel::term::Term::TGet(____pat0_fields) => {
            let __pat0 = ____pat0_fields.0.clone();
            let f = ____pat0_fields.1.clone();
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::term::Term::TRec(__ty_fields) => {
                    let ty = __ty_fields.0.clone();
                    let fs = __ty_fields.1.clone();
                    crate::proof_kernel::aver_generated::kernel::check::projField(t, fs, f, path)
                }
                crate::proof_kernel::aver_generated::kernel::term::Term::TUpd(__ty_fields) => {
                    let ty = __ty_fields.0.clone();
                    let base = __ty_fields.1.clone();
                    let fs = __ty_fields.2.clone();
                    {
                        let __arg10 @ _ =
                            crate::proof_kernel::aver_generated::kernel::check::typeOf(
                                &t, env, path,
                            );
                        crate::proof_kernel::aver_generated::kernel::check::projUpdate(
                            t, &__arg10, base, &fs, f,
                        )
                    }
                }
                _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    AverStr::from(
                        "a projection must read a field of a record literal or of an update",
                    ),
                ),
            }
        }
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("a projection must read a field of a record literal or of an update"),
        ),
    }
}

/// Once the read has a type, so the base is of the updated record and f one of its fields: the value written to f, or the base's f.
pub fn projUpdate(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    ty @ _: &Result<crate::proof_kernel::aver_generated::kernel::term::Ty, AverStr>,
    mut base @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
    f @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    ty.clone()?;
    match crate::proof_kernel::aver_generated::kernel::check::findField(fs.clone(), f.clone()) {
        Some(x @ _) => {
            Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: t, rhs: x })
        }
        None => Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: t,
            rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TGet(
                aver_rt::Shared::new(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TGet(base, f),
                ),
            ),
        }),
    }
}

/// The value the first field named f holds.
#[inline(always)]
pub fn findField(
    mut fs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
    mut f @ _: AverStr,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Term> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(fs, [] => { return None; }, [x, rest] => { if (x.name == f) { return Some(x.value); } else { {
            let __tco0 = rest;
            fs = __tco0;
            continue;
        } } })
    }
}

/// A list literal with an element is that element in front of the rest.
pub fn cell(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match &t {
        crate::proof_kernel::aver_generated::kernel::term::Term::TList(____pat0_fields) => {
            let __pat0 = ____pat0_fields.0.clone();
            {
                let __list_subject = __pat0;
                if let Some((x, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: t, rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBi(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("List.prepend"), aver_rt::AverList::from_vec(vec![x, crate::proof_kernel::aver_generated::kernel::term::Term::TList(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TList(rest)))])))) })
                } else {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        AverStr::from("a cell step needs a list literal with an element"),
                    )
                }
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
    let t @ _ = aver_rt::Shared::new(t);
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
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut b @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a),
        crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&b),
    ) {
        (Some(x), Some(y)) => {
            if (x == y) {
                Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: a, rhs: b })
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
pub fn cases(
    mut on @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
    mut t @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut f @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::check::typeOf(&on, env, path.clone()) {
        Ok(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyBool => {
                crate::proof_kernel::aver_generated::kernel::check::casesOfBool(
                    on, h, t, f, env, path,
                )
            }
            _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                AverStr::from("the term split on is not a Bool"),
            ),
        },
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the term split on is not a Bool"),
        ),
    }
}

/// The two branches of a split on a Bool.
pub fn casesOfBool(
    mut on @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
    mut t @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut f @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let whenTrue @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
        t,
        crate::proof_kernel::aver_generated::kernel::check::withHyp(
            env,
            h.clone(),
            on.clone(),
            true,
        ),
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

/// A split on any term: one case per constructor of its type ([] and [h, ..t] for a list, every constructor of a sum in order, each with one name per field); or, given literals of its type, one case per literal and a last one where it is none of them.
pub fn split(
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
    lits @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (
            crate::proof_kernel::aver_generated::kernel::check::typeOf(on, env, path.clone()),
            lits.clone(),
        );
        let (__lit0, __lit1) = &__int_match_subject;
        if (*__lit0).is_err() {
            let why = if let Err(why) = &(*__lit0) {
                why.clone()
            } else {
                unreachable!("Aver Rust codegen: tuple element tag mismatch")
            };
            Err(why)
        } else if (*__lit0).is_ok() && (*__lit1).is_empty() {
            let t = if let Ok(t) = &(*__lit0) {
                t.clone()
            } else {
                unreachable!("Aver Rust codegen: tuple element tag mismatch")
            };
            crate::proof_kernel::aver_generated::kernel::check::splitConstructors(
                &t, on, h, cs, env, path,
            )
        } else if (*__lit0).is_ok() {
            let t = if let Ok(t) = &(*__lit0) {
                t.clone()
            } else {
                unreachable!("Aver Rust codegen: tuple element tag mismatch")
            };
            crate::proof_kernel::aver_generated::kernel::check::splitLiterals(
                &t, lits, on, h, cs, env, path,
            )
        } else {
            unreachable!("Aver Rust codegen: non-exhaustive guard-chain match")
        }
    }
}

/// A split along literals of the term's type: one case per literal under h : on = k, then one naming one hypothesis per literal, (on == k) = false.
#[inline(always)]
pub fn splitLiterals(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
    lits @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (crate::proof_kernel::aver_generated::kernel::check::literalsOf(lits, t, env, path.clone())
        && ((crate::proof_kernel::aver_generated::kernel::check::splitCtors(cs)
            == crate::proof_kernel::aver_generated::kernel::check::litCtors(lits))
            && (crate::proof_kernel::aver_generated::kernel::check::binderCounts(cs)
                == crate::proof_kernel::aver_generated::kernel::check::litCounts(
                    lits,
                    aver_rt::AverInt::from_i64(0),
                ))))
    {
        crate::proof_kernel::aver_generated::kernel::check::splitCases(
            cs.clone(),
            crate::proof_kernel::aver_generated::kernel::check::litHyps(
                lits,
                cs,
                on,
                h,
                &aver_rt::AverList::empty(),
            ),
            crate::proof_kernel::aver_generated::kernel::check::noVars(cs),
            on,
            aver_rt::AverList::empty(),
            env,
            path,
        )
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the cases are not the literals and one more"),
        )
    }
}

/// Every one an Int, Bool, text or unit literal of type t.
#[inline(always)]
pub fn literalsOf(
    lits @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
    env @ _: &Env,
    path @ _: AverStr,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(lits.clone(), [] => true, [k, rest] => ((crate::proof_kernel::aver_generated::kernel::check::isLiteral(&k) && (crate::proof_kernel::aver_generated::kernel::check::typeOf(&k, env, path.clone()) == Ok(t.clone()))) && crate::proof_kernel::aver_generated::kernel::check::literalsOf(&rest, t, env, path)))
}

/// A split along the constructors of the subject's type: every one in order, each case naming one variable per field.
pub fn splitConstructors(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        (crate::proof_kernel::aver_generated::kernel::types::ctorsOf(
            t,
            &crate::proof_kernel::aver_generated::kernel::check::tyEnv(env),
        ) == crate::proof_kernel::aver_generated::kernel::check::splitCtors(cs)),
        crate::proof_kernel::aver_generated::kernel::check::caseVars(cs, t, env),
    ) {
        (false, _) => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(54)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("the cases are not the constructors of "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(crate::proof_kernel::aver_generated::kernel::types::show(t)),
                )));
                __b
            }),
        ),
        (_, Err(why)) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
        (true, Ok(vss)) => crate::proof_kernel::aver_generated::kernel::check::splitCases(
            cs.clone(),
            crate::proof_kernel::aver_generated::kernel::check::ctorHyps(cs, on, h),
            vss,
            on,
            aver_rt::AverList::empty(),
            env,
            path,
        ),
    }
}

/// A given x of a declared sum C with bare constructors C.A and C.B.
pub fn sumEnv() -> Env {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: aver_rt::AverList::empty(),
        consts: aver_rt::AverList::empty(),
        laws: aver_rt::AverList::empty(),
        hyps: aver_rt::AverList::empty(),
        vars: aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::term::typed(
                AverStr::from("x"),
                crate::proof_kernel::aver_generated::kernel::term::Ty::TyNamed(
                    aver_rt::Shared::new(
                        crate::proof_kernel::aver_generated::kernel::term::Ty__TyNamed(
                            AverStr::from("C"),
                        ),
                    ),
                ),
            ),
        ]),
        sums: aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::proof::Sum {
                name: AverStr::from("C"),
                eq: false,
                variants: aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::proof::Variant {
                        name: AverStr::from("C.A"),
                        fields: aver_rt::AverList::empty(),
                    },
                    crate::proof_kernel::aver_generated::kernel::proof::Variant {
                        name: AverStr::from("C.B"),
                        fields: aver_rt::AverList::empty(),
                    },
                ]),
            },
        ]),
        recs: aver_rt::AverList::empty(),
        sigs: aver_rt::AverList::empty(),
    }
}

/// For each case, its names with the types of its constructor's fields over the type split on.
#[inline(always)]
pub fn caseVars(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
    env @ _: &Env,
) -> Result<
    aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    >,
    AverStr,
> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => Ok(aver_rt::AverList::empty()), [c, rest] => { match crate::proof_kernel::aver_generated::kernel::types::ctorFields(c.ctor.clone(), t, &crate::proof_kernel::aver_generated::kernel::check::tyEnv(env)) { Err(why @ _) => { Err(why) }, Ok(fs @ _) => { if (aver_rt::AverInt::from_i64(fs.len() as i64) == aver_rt::AverInt::from_i64(c.binders.len() as i64)) { Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::types::namedVars(&c.binders, &fs), &crate::proof_kernel::aver_generated::kernel::check::caseVars(&rest, t, env)?)) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(78)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("case ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.ctor)))); __b }; __b.push_str(&AverStr::from(" binds ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(aver_rt::AverInt::from_i64(c.binders.len() as i64))))); __b }; __b.push_str(&AverStr::from(" names for ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(aver_rt::AverInt::from_i64(fs.len() as i64))))); __b }; __b.push_str(&AverStr::from(" fields")); __b })) } } } })
}

/// No variables for any case: a literal split names hypotheses, not values.
#[inline(always)]
pub fn noVars(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>>
{
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => aver_rt::AverList::empty(), [c, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::check::firstVars(&aver_rt::AverList::empty()), &crate::proof_kernel::aver_generated::kernel::check::noVars(&rest)))
}

/// The constructor of each case.
#[inline(always)]
pub fn splitCtors(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => aver_rt::AverList::empty(), [c, rest] => aver_rt::AverList::prepend(c.ctor, &crate::proof_kernel::aver_generated::kernel::check::splitCtors(&rest)))
}

/// How many names each case binds.
#[inline(always)]
pub fn binderCounts(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
) -> aver_rt::AverIntList {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => aver_rt::AverIntList::empty(), [c, rest] => aver_rt::AverIntList::prepend(aver_rt::AverInt::from_i64(c.binders.len() as i64), &crate::proof_kernel::aver_generated::kernel::check::binderCounts(&rest)))
}

/// The constructor of a case applied to its names.
pub fn splitValue(
    c @ _: &crate::proof_kernel::aver_generated::kernel::proof::SplitCase,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (c.ctor.clone(), c.binders.clone());
        {
            let __dispatch_subject = __pat0;
            if &*__dispatch_subject == "nil" {
                crate::proof_kernel::aver_generated::kernel::term::Term::TList(
                    aver_rt::Shared::new(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TList(
                            aver_rt::AverList::empty(),
                        ),
                    ),
                )
            } else {
                if &*__dispatch_subject == "cons" {
                    {
                        let __list_subject = __pat1;
                        if let Some((a, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat2;
                                if let Some((b, __pat3)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    {
                                        let __list_subject = __pat3;
                                        if __list_subject.is_empty() {
                                            crate::proof_kernel::aver_generated::kernel::term::Term::TBi(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("List.prepend"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(a))), crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(b)))]))))
                                        } else {
                                            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(c.ctor.clone(), crate::proof_kernel::aver_generated::kernel::check::namesToVars(&c.binders))))
                                        }
                                    }
                                } else {
                                    crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(c.ctor.clone(), crate::proof_kernel::aver_generated::kernel::check::namesToVars(&c.binders))))
                                }
                            }
                        } else {
                            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(c.ctor.clone(), crate::proof_kernel::aver_generated::kernel::check::namesToVars(&c.binders))))
                        }
                    }
                } else {
                    crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                        aver_rt::Shared::new(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                                c.ctor.clone(),
                                crate::proof_kernel::aver_generated::kernel::check::namesToVars(
                                    &c.binders,
                                ),
                            ),
                        ),
                    )
                }
            }
        }
    }
}

/// The case of each literal, lit, then else for the last one.
#[inline(always)]
pub fn litCtors(
    lits @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(lits.clone(), [] => aver_rt::AverList::from_vec(vec![AverStr::from("else")]), [k, rest] => aver_rt::AverList::prepend(AverStr::from("lit"), &crate::proof_kernel::aver_generated::kernel::check::litCtors(&rest)))
}

/// How many names each case of a literal split binds: none for a literal, one per literal for the last one.
#[inline(always)]
pub fn litCounts(
    lits @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    n @ _: aver_rt::AverInt,
) -> aver_rt::AverIntList {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(lits.clone(), [] => aver_rt::AverIntList::from_vec(vec![n]), [k, rest] => aver_rt::AverIntList::prepend(aver_rt::AverInt::from_i64(0), &crate::proof_kernel::aver_generated::kernel::check::litCounts(&rest, n.add(&aver_rt::AverInt::from_i64(1)))))
}

/// For each case of a split on a constructor, h : on = the constructor at the case's names.
#[inline(always)]
pub fn ctorHyps(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => aver_rt::AverList::empty(), [c, rest] => aver_rt::AverList::prepend(aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: h.clone(), eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: on.clone(), rhs: crate::proof_kernel::aver_generated::kernel::check::splitValue(&c) } }]), &crate::proof_kernel::aver_generated::kernel::check::ctorHyps(&rest, on, h)))
}

/// Each case's hypotheses: h : on = k for a literal k, and for the last one (on == k) = false per literal, named by the case's names.
pub fn litHyps(
    lits @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
    seen @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (lits.clone(), cs.clone());
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((k, ks)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((c, more)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            aver_rt::AverList::prepend(
                aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                        name: h.clone(),
                        eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
                            lhs: on.clone(),
                            rhs: k.clone(),
                        },
                    },
                ]),
                &crate::proof_kernel::aver_generated::kernel::check::litHyps(
                    &ks,
                    &more,
                    on,
                    h,
                    &aver_rt::AverList::concat(
                        &seen.clone(),
                        &aver_rt::AverList::from_vec(vec![k]),
                    ),
                ),
            )
        } else if (*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((c, more)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::check::unequal(&c.binders, seen, on),
            ])
        } else {
            aver_rt::AverList::empty()
        }
    }
}

/// Each name stating (on == k) = false for its literal k.
pub fn unequal(
    names @ _: &aver_rt::AverList<AverStr>,
    lits @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (names.clone(), lits.clone());
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((n, ns)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((k, ks)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            aver_rt::AverList::prepend(
                crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                    name: n,
                    eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            aver_rt::Shared::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                    AverStr::from("=="),
                                    on.clone(),
                                    k,
                                ),
                            ),
                        ),
                        rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                            aver_rt::Shared::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                    false,
                                ),
                            ),
                        ),
                    },
                },
                &crate::proof_kernel::aver_generated::kernel::check::unequal(&ns, &ks, on),
            )
        } else {
            aver_rt::AverList::empty()
        }
    }
}

/// The typed names of the first case.
#[inline(always)]
pub fn firstVars(
    vss @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    >,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(vss.clone(), [] => aver_rt::AverList::empty(), [vs, rest] => vs)
}

/// The hypotheses of the first case.
#[inline(always)]
pub fn firstHyps(
    hss @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    >,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(hss.clone(), [] => aver_rt::AverList::empty(), [hs, rest] => hs)
}

/// The equation every case proved.
#[inline(always)]
pub fn splitDone(
    before @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(before.clone(), [] => crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("a split with no cases")), [e, rest] => Ok(e))
}

/// The environment with one more hypothesis in front.
pub fn withHyp(
    env @ _: &Env,
    h @ _: AverStr,
    mut on @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
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
                    lhs: on,
                    rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                        aver_rt::Shared::new(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TBool(value),
                        ),
                    ),
                },
            },
            &env.hyps.clone(),
        ),
        vars: env.vars.clone(),
        sums: env.sums.clone(),
        recs: env.recs.clone(),
        sigs: env.sigs.clone(),
    }
}

/// A comparison decided by linear arithmetic over the integers: its opposite and the named hypotheses, then the rows the cuts round from them, weighted, add up to a negative constant. Every row compares two Int terms.
pub fn linear(
    mut g @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    v @ _: bool,
    hs @ _: &aver_rt::AverList<AverStr>,
    ws @ _: &aver_rt::AverIntList,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Cut>,
    mut env @ _: Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let facts @ _ =
        crate::proof_kernel::aver_generated::kernel::check::hypEqns(hs, &env.hyps, path.clone())?;
    crate::proof_kernel::aver_generated::kernel::check::intComparisons(
        aver_rt::AverList::prepend(
            g.clone(),
            &crate::proof_kernel::aver_generated::kernel::check::lhsOf(&facts),
        ),
        env,
        path.clone(),
    )?;
    crate::proof_kernel::aver_generated::kernel::check::linearOver(g, v, &facts, ws, cs, path)
}

/// Each term compares two Int terms: what linear arithmetic may read as a row.
#[inline(always)]
pub fn intComparisons(
    mut ts @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    env @ _: Env,
    mut path @ _: AverStr,
) -> Result<(), AverStr> {
    let env @ _ = aver_rt::Shared::new(env);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(ts, [] => { return Ok(()); }, [__pat0, rest] => { match __pat0 {
        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(__o_fields) => {
            let o = __o_fields.0.clone();
            let a = __o_fields.1.clone();
            let b = __o_fields.2.clone();
            match (crate::proof_kernel::aver_generated::kernel::check::typeOf(&a, &*env, path.clone())?, crate::proof_kernel::aver_generated::kernel::check::typeOf(&b, &*env, path.clone())?) {
        (crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt, crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt) => {
            {
            let __tco0 = rest;
            ts = __tco0;
            continue;
        }
        },
        _ => {
            return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(71)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&AverStr::from(": linear arithmetic reads comparisons of Int terms")); __b }));
        }
    }
        },
        _ => {
            return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(71)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&AverStr::from(": linear arithmetic reads comparisons of Int terms")); __b }));
        }
    } })
    }
}

/// The left side of each equation.
#[inline(always)]
pub fn lhsOf(
    es @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(es.clone(), [] => aver_rt::AverList::empty(), [e, rest] => aver_rt::AverList::prepend(e.lhs, &crate::proof_kernel::aver_generated::kernel::check::lhsOf(&rest)))
}

/// The contradiction the weights add up to.
#[inline(always)]
pub fn linearOver(
    mut g @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    v @ _: bool,
    facts @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    ws @ _: &aver_rt::AverIntList,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Cut>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if crate::proof_kernel::aver_generated::kernel::ring::linearContradiction(
        &aver_rt::AverList::prepend(
            crate::proof_kernel::aver_generated::kernel::term::Eqn {
                lhs: g.clone(),
                rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                    aver_rt::Shared::new(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBool((!v)),
                    ),
                ),
            },
            &facts.clone(),
        ),
        cs,
        ws,
    ) {
        Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: g,
            rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                aver_rt::Shared::new(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TBool(v),
                ),
            ),
        })
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the weights do not add up to a contradiction"),
        )
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
pub fn ring(
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        crate::proof_kernel::aver_generated::kernel::check::typeOf(&l, env, path.clone())?,
        crate::proof_kernel::aver_generated::kernel::check::typeOf(&r, env, path.clone())?,
        crate::proof_kernel::aver_generated::kernel::ring::samePolynomial(l.clone(), r.clone()),
    ) {
        (
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt,
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt,
            true,
        ) => Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r }),
        (
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt,
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt,
            false,
        ) => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the two sides are different polynomials"),
        ),
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("ring needs two Int terms"),
        ),
    }
}

/// Any equation, once true has been shown equal to false: the case cannot happen.
pub fn absurd(
    e @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (e.lhs.clone(), e.rhs.clone()) {
        (
            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(__a_fields),
            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(__b_fields),
        ) => {
            let a = __a_fields.0.clone();
            let b = __b_fields.0.clone();
            if (a == b) {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    AverStr::from("absurd needs true equal to false"),
                )
            } else {
                Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r })
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
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::check::finOfVar(v.clone(), env) {
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
            &crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r },
            crate::proof_kernel::aver_generated::kernel::finite::values(&f),
            cs.clone(),
            env,
            path,
            aver_rt::AverInt::from_i64(0),
        ),
    }
}

/// The finite type of a variable, if its type is one.
#[inline(always)]
pub fn finOfVar(
    v @ _: AverStr,
    env @ _: &Env,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Fin> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::types::typeOf(
        &crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(
            crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v),
        )),
        crate::proof_kernel::aver_generated::kernel::check::tyEnv(env),
    ) {
        Ok(t @ _) => crate::proof_kernel::aver_generated::kernel::types::finOf(
            &t,
            &crate::proof_kernel::aver_generated::kernel::check::tyEnv(env),
            &aver_rt::AverList::empty(),
        ),
        Err(why @ _) => None,
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
    aver_list_match!(hs.clone(), [] => Ok(aver_rt::AverList::empty()), [h, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: h.name, eqn: crate::proof_kernel::aver_generated::kernel::check::instantiate(h.eqn, at, path.clone())? }, &crate::proof_kernel::aver_generated::kernel::check::substHyps(&rest, at, path)?)))
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
        vars: env.vars.clone(),
        sums: env.sums.clone(),
        recs: env.recs.clone(),
        sigs: env.sigs.clone(),
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
        aver_list_match!(cs, [] => { return crate::proof_kernel::aver_generated::kernel::check::refuse(path, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("no module-level binding ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n)))); __b })); }, [c, rest] => { if (c.name == n) { return Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(n))), rhs: c.value }); } else { {
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
                crate::proof_kernel::aver_generated::kernel::term::bind(n, v),
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
    match crate::proof_kernel::aver_generated::kernel::proof::findDef(f.clone(), env.defs.clone()) {
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
                crate::proof_kernel::aver_generated::kernel::check::argsFit(
                    &d,
                    xs,
                    &env,
                    path.clone(),
                ),
                crate::proof_kernel::aver_generated::kernel::induct::openGate(&d, &env.defs),
            ) {
                (_, Err(why)) => {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, why)
                }
                (Err(why), _) => Err(why),
                _ => crate::proof_kernel::aver_generated::kernel::check::unfoldArm(
                    d, k, xs, ys, pre, env, path,
                ),
            }
        }
    }
}

/// The whole body, or one arm under its premise; the local bindings are substituted first, in order.
pub fn unfoldArm(
    mut d @ _: crate::proof_kernel::aver_generated::kernel::proof::Def,
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
        crate::proof_kernel::aver_generated::kernel::check::zipBind(
            &crate::proof_kernel::aver_generated::kernel::types::paramsOf(&d),
            xs,
        )
        .reverse(),
    )?;
    let lhs @ _ =
        crate::proof_kernel::aver_generated::kernel::term::Term::TCall(aver_rt::Shared::new(
            crate::proof_kernel::aver_generated::kernel::term::Term__TCall(d.name, xs.clone()),
        ));
    {
        let __int_match_subject = (k.clone(), ys.clone(), pre.clone());
        let (__lit0, __lit1, __lit2) = &__int_match_subject;
        if &(*__lit0) == &aver_rt::AverInt::from_i64(0)
            && (*__lit1).is_empty()
            && (*__lit2).is_empty()
        {
            Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                lhs: lhs,
                rhs: crate::proof_kernel::aver_generated::kernel::subst::subst(d.body, &outer)?,
            })
        } else if &(*__lit0) == &aver_rt::AverInt::from_i64(0) {
            crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                AverStr::from("equation 0 takes no binders and no premise"),
            )
        } else {
            crate::proof_kernel::aver_generated::kernel::check::unfoldMatch(
                &d.body, k, &outer, ys, pre, lhs, env, path,
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
            let __tco1 = aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::bind(b.name, crate::proof_kernel::aver_generated::kernel::subst::subst(b.value, &outer)?), &outer);
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
    mut lhs @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut env @ _: Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match body {
        crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(__s_fields) => {
            let s = __s_fields.0.clone();
            let arms = __s_fields.1.clone();
            crate::proof_kernel::aver_generated::kernel::check::selected(
                s, &arms, k, outer, ys, pre, lhs, env, path,
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
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut env @ _: Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match &t {
        crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(__s_fields) => {
            let s = __s_fields.0.clone();
            let arms = __s_fields.1.clone();
            crate::proof_kernel::aver_generated::kernel::check::selected(
                s,
                &arms,
                k,
                &aver_rt::AverList::empty(),
                ys,
                &aver_rt::AverList::from_vec(vec![q]),
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
    mut s @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    k @ _: aver_rt::AverInt,
    outer @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    pre @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    mut lhs @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
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
    mut s @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    k @ _: aver_rt::AverInt,
    outer @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut lhs @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
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
                                s, arms, k, chosen, outer, v, q, lhs, env, path,
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
                    s, chosen, outer, ys, q, lhs, env, path,
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
    match p {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PWild => true,
        crate::proof_kernel::aver_generated::kernel::term::Pat::PVar(n) => {
            let n = n.clone();
            true
        }
        _ => false,
    }
}

/// A catch-all arm chosen for one value: every earlier arm excludes it, the premise shows the subject is it, and a named catch-all binds it.
pub fn catchAllArm(
    mut s @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    k @ _: aver_rt::AverInt,
    mut chosen @ _: crate::proof_kernel::aver_generated::kernel::term::Arm,
    outer @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    mut v @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut lhs @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut env @ _: Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let subject @ _ = crate::proof_kernel::aver_generated::kernel::subst::subst(s, outer)?;
    crate::proof_kernel::aver_generated::kernel::check::bound(
        &crate::proof_kernel::aver_generated::kernel::check::zipBind(
            &crate::proof_kernel::aver_generated::kernel::subst::patNames(&chosen.pattern),
            &aver_rt::AverList::from_vec(vec![v.clone()]),
        ),
        &crate::proof_kernel::aver_generated::kernel::types::namedVars(
            &crate::proof_kernel::aver_generated::kernel::subst::patNames(&chosen.pattern),
            &aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::check::typeOf(
                    &subject,
                    &env,
                    path.clone(),
                )?,
            ]),
        ),
        &env,
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("arm "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
            __b
        }),
        path.clone(),
    )?;
    if crate::proof_kernel::aver_generated::kernel::check::earlierExcludeValue(
        arms,
        k.clone(),
        &v,
        &env.hyps,
    ) {
        {
            let __arg12 @ _ = aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::check::zipBind(
                    &crate::proof_kernel::aver_generated::kernel::subst::patNames(&chosen.pattern),
                    &aver_rt::AverList::from_vec(vec![v.clone()]),
                ),
                &outer.clone(),
            );
            crate::proof_kernel::aver_generated::kernel::check::catchAllBody(
                chosen,
                &__arg12,
                &crate::proof_kernel::aver_generated::kernel::term::Eqn {
                    lhs: subject,
                    rhs: v,
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
        }
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
    mut chosen @ _: crate::proof_kernel::aver_generated::kernel::term::Arm,
    inner @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    needed @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    got @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut lhs @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (got == needed) {
        Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: lhs,
            rhs: crate::proof_kernel::aver_generated::kernel::subst::subst(chosen.body, inner)?,
        })
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the premise does not select this arm"),
        )
    }
}

/// Every arm before arm k certainly does not match the value: by its shape, or a literal arm by a hypothesis (v == k) = false in scope.
pub fn earlierExcludeValue(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    k @ _: aver_rt::AverInt,
    v @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (arms.clone(), (k > aver_rt::AverInt::from_i64(1)));
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && (*__lit1) == true {
            let Some((a, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            ((crate::proof_kernel::aver_generated::kernel::check::excludes(a.pattern.clone(), v)
                || crate::proof_kernel::aver_generated::kernel::check::ruledOut(
                    &a.pattern,
                    v.clone(),
                    hs,
                ))
                && crate::proof_kernel::aver_generated::kernel::check::earlierExcludeValue(
                    &rest,
                    k.sub(&aver_rt::AverInt::from_i64(1)),
                    v,
                    hs,
                ))
        } else {
            true
        }
    }
}

/// A literal pattern k that a hypothesis in scope states the value is not: (v == k) = false.
pub fn ruledOut(
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
    mut v @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match p {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(k) => {
            let k = k.clone();
            (crate::proof_kernel::aver_generated::kernel::check::isLiteral(&k)
                && crate::proof_kernel::aver_generated::kernel::check::stated(
                    &crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            aver_rt::Shared::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                    AverStr::from("=="),
                                    v,
                                    k,
                                ),
                            ),
                        ),
                        rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                            aver_rt::Shared::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                    false,
                                ),
                            ),
                        ),
                    },
                    hs,
                ))
        }
        _ => false,
    }
}

/// Some hypothesis in scope states exactly e.
#[inline(always)]
pub fn stated(
    e @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => false, [h, rest] => ((&(h.eqn) == e) || crate::proof_kernel::aver_generated::kernel::check::stated(e, &rest)))
}

/// Whether a pattern certainly does not match a value: another literal, another constructor, the other list shape.
pub fn excludes(
    mut p @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
    v @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (p, v.clone());
        match __pat0 {
            crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(x) => {
                (crate::proof_kernel::aver_generated::kernel::check::isLiteral(v) && (&(x) != v))
            }
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, xs) => match __pat1 {
                crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(__d_fields) => {
                    let d = __d_fields.0.clone();
                    let ys = __d_fields.1.clone();
                    (c != d)
                }
                _ => false,
            },
            crate::proof_kernel::aver_generated::kernel::term::Pat::PNil => match __pat1 {
                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(____pat2_fields) => {
                    let __pat2 = ____pat2_fields.0.clone();
                    let __pat3 = ____pat2_fields.1.clone();
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
                crate::proof_kernel::aver_generated::kernel::term::Term::TList(____pat6_fields) => {
                    let __pat6 = ____pat6_fields.0.clone();
                    {
                        let __list_subject = __pat6;
                        if __list_subject.is_empty() {
                            true
                        } else {
                            false
                        }
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
    match t {
        crate::proof_kernel::aver_generated::kernel::term::Term::TInt(__n_fields) => {
            let n = __n_fields.0.clone();
            true
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TBool(__b_fields) => {
            let b = __b_fields.0.clone();
            true
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TStr(__s_fields) => {
            let s = __s_fields.0.clone();
            true
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TUnit => true,
        _ => false,
    }
}

/// subject = pattern must be what the premise proves; then lhs equals the arm's body.
pub fn armEquation(
    mut s @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut chosen @ _: crate::proof_kernel::aver_generated::kernel::term::Arm,
    outer @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut lhs @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut env @ _: Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let shape @ _ = crate::proof_kernel::aver_generated::kernel::check::patTerm(
        chosen.pattern.clone(),
        ys,
        path.clone(),
    )?;
    let subject @ _ = crate::proof_kernel::aver_generated::kernel::subst::subst(s, outer)?;
    let parts @ _ = crate::proof_kernel::aver_generated::kernel::check::withoutNames(
        crate::proof_kernel::aver_generated::kernel::check::zipBind(
            &crate::proof_kernel::aver_generated::kernel::check::patFields(&chosen.pattern),
            ys,
        ),
        aver_rt::AverList::from_vec(vec![AverStr::from("_")]),
    );
    crate::proof_kernel::aver_generated::kernel::check::bound(
        &parts,
        &crate::proof_kernel::aver_generated::kernel::check::patTypes(
            chosen.pattern,
            crate::proof_kernel::aver_generated::kernel::check::typeOf(
                &subject,
                &env,
                path.clone(),
            )?,
            &env,
            path.clone(),
        )?,
        &env,
        AverStr::from("the arm"),
        path.clone(),
    )?;
    let inner @ _ = aver_rt::AverList::concat(&parts, &outer.clone());
    let needed @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn {
        lhs: subject,
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
            lhs: lhs,
            rhs: crate::proof_kernel::aver_generated::kernel::subst::subst(chosen.body, &inner)?,
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
            (crate::proof_kernel::aver_generated::kernel::check::headsDiffer(a.pattern, p.clone())
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
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
    mut b @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match (a, b) {
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
    mut p @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (p, ys.clone());
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
                            aver_rt::Shared::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TList(
                                    aver_rt::AverList::empty(),
                                ),
                            ),
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
                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TBi(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("List.prepend"), aver_rt::AverList::from_vec(vec![x, y])))))
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

/// A constructor pattern with one term per field, a wildcard field as well.
#[inline(always)]
pub fn ctorTerm(
    c @ _: AverStr,
    names @ _: &aver_rt::AverList<AverStr>,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (aver_rt::AverInt::from_i64(names.len() as i64)
        == aver_rt::AverInt::from_i64(ys.len() as i64))
    {
        Ok(
            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(aver_rt::Shared::new(
                crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(c, ys.clone()),
            )),
        )
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

/// The names a pattern's parts take, in order, a wildcard part as _.
pub fn patFields(
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match p {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            aver_rt::AverList::from_vec(vec![h, t])
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, names) => {
            let c = c.clone();
            let names = names.clone();
            names
        }
        _ => crate::proof_kernel::aver_generated::kernel::subst::patNames(p),
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
    mut e @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        crate::proof_kernel::aver_generated::kernel::subst::subst(e.lhs, bs),
        crate::proof_kernel::aver_generated::kernel::subst::subst(e.rhs, bs),
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
            if (crate::proof_kernel::aver_generated::kernel::check::boundNames(bs)
                == crate::proof_kernel::aver_generated::kernel::proof::givenNames(&s.binders))
            {
                crate::proof_kernel::aver_generated::kernel::check::ruleConclusion(
                    &crate::proof_kernel::aver_generated::kernel::check::bound(
                        bs,
                        &s.binders,
                        env,
                        aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("rule "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(id))));
                            __b
                        }),
                        path.clone(),
                    ),
                    &s.premises,
                    s.concl,
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
                                &crate::proof_kernel::aver_generated::kernel::proof::givenNames(
                                    &s.binders,
                                ),
                            )),
                        )));
                        __b
                    }),
                )
            }
        }
    }
}

/// Once the binders are typed: premises first, then the conclusion.
pub fn ruleConclusion(
    typed @ _: &Result<(), AverStr>,
    wanted @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    mut concl @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    typed.clone()?;
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
            if (crate::proof_kernel::aver_generated::kernel::check::boundNames(bs)
                == crate::proof_kernel::aver_generated::kernel::proof::givenNames(&l.givens))
            {
                crate::proof_kernel::aver_generated::kernel::check::ruleConclusion(
                    &crate::proof_kernel::aver_generated::kernel::check::bound(
                        bs,
                        &l.givens,
                        env,
                        aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("law "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(key))));
                            __b
                        }),
                        path.clone(),
                    ),
                    &crate::proof_kernel::aver_generated::kernel::check::lawPremises(&l),
                    crate::proof_kernel::aver_generated::kernel::term::Eqn {
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
                                &crate::proof_kernel::aver_generated::kernel::proof::givenNames(
                                    &l.givens,
                                ),
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
    aver_list_match!(l.premise.clone(), [] => aver_rt::AverList::empty(), [p, rest] => aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: p, rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true))) }]))
}

/// The proof must prove exactly the obligation, under its when; every cited builtin fact is checked first.
pub fn checkScript(
    s @ _: &crate::proof_kernel::aver_generated::kernel::proof::Script,
) -> Result<AverStr, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::induct::refuseMutualRecursion(&s.defs)?;
    crate::proof_kernel::aver_generated::kernel::check::checkFacts(
        s.facts.clone(),
        aver_rt::AverList::empty(),
    )?;
    let env @ _ = crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: s.defs.clone(),
        consts: s.consts.clone(),
        laws: aver_rt::AverList::concat(
            &s.laws.clone(),
            &crate::proof_kernel::aver_generated::kernel::check::factLaws(&s.facts),
        ),
        hyps: crate::proof_kernel::aver_generated::kernel::check::lawHyps(&s.obligation),
        vars: crate::proof_kernel::aver_generated::kernel::types::rigidVars(&s.obligation.givens),
        sums: s.sums.clone(),
        recs: s.recs.clone(),
        sigs: s.sigs.clone(),
    };
    crate::proof_kernel::aver_generated::kernel::check::statementTyped(&s.obligation, &env)?;
    let e @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
        s.proof.clone(),
        env,
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

/// The claim's two sides have one type and its when is a Bool: a statement written wrongly is refused before its proof is read.
pub fn statementTyped(
    ob @ _: &crate::proof_kernel::aver_generated::kernel::proof::Law,
    env @ _: &Env,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (
            crate::proof_kernel::aver_generated::kernel::types::join(
                crate::proof_kernel::aver_generated::kernel::check::typeOf(
                    &ob.lhs,
                    env,
                    AverStr::from("proof"),
                )?,
                crate::proof_kernel::aver_generated::kernel::check::typeOf(
                    &ob.rhs,
                    env,
                    AverStr::from("proof"),
                )?,
            ),
            crate::proof_kernel::aver_generated::kernel::check::typesOf(
                &ob.premise,
                env,
                AverStr::from("proof"),
            )?,
        );
        match __pat0 {
            Err(why) => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("step proof: the claim compares "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                __b
            })),
            _ => {
                aver_list_match!(__pat1, [] => Ok(()), [__pat2, __pat3] => { match __pat2 {
        crate::proof_kernel::aver_generated::kernel::term::Ty::TyBool => {
            { let __list_subject = __pat3; if __list_subject.is_empty() { Ok(()) } else { Err(AverStr::from("step proof: the when is not a Bool")) } }
        },
        _ => {
            Err(AverStr::from("step proof: the when is not a Bool"))
        }
    } })
            }
        }
    }
}

/// The type of each term.
#[inline(always)]
pub fn typesOf(
    ts @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Ty>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => Ok(aver_rt::AverList::empty()), [t, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::check::typeOf(&t, env, path.clone())?, &crate::proof_kernel::aver_generated::kernel::check::typesOf(&rest, env, path)?)))
}

/// The obligation's when, as hypothesis `when`.
#[inline(always)]
pub fn lawHyps(
    ob @ _: &crate::proof_kernel::aver_generated::kernel::proof::Law,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ob.premise.clone(), [] => aver_rt::AverList::empty(), [p, rest] => aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: AverStr::from("when"), eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: p, rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true))) } }]))
}

/// Names separated by commas.
#[inline(always)]
pub fn commaJoined(ns @ _: &aver_rt::AverList<AverStr>) -> AverStr {
    crate::proof_kernel::cancel_checkpoint();
    (aver_rt::string_join(&ns, &AverStr::from(","))).into_aver()
}

/// Induction along the recursion of f at arguments xs, as its gate checked it: one case per arm of its match, each with one hypothesis per recursive call in the arm. The hypotheses named in carry hold in each case; a call's hypothesis holds once they are proved at the call's arguments.
#[inline(always)]
pub fn induct(
    f @ _: AverStr,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carry @ _: &aver_rt::AverList<AverStr>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::proof::findDef(f.clone(), env.defs.clone()) {
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
            match crate::proof_kernel::aver_generated::kernel::induct::recursion(&d, &env.defs) {
                Err(why @ _) => {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, why)
                }
                Ok(__pat0 @ _) => match __pat0 {
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
                    Some(r @ _) => crate::proof_kernel::aver_generated::kernel::check::inductOn(
                        &d, &r, xs, claim, carry, cs, env, path,
                    ),
                },
            }
        }
    }
}

/// The arguments, the varied givens and the shape of the arms, then every case.
pub fn inductOn(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carry @ _: &aver_rt::AverList<AverStr>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        crate::proof_kernel::aver_generated::kernel::check::argsFit(d, xs, env, path.clone()),
        crate::proof_kernel::aver_generated::kernel::induct::varied(
            xs,
            r.at.clone(),
            &crate::proof_kernel::aver_generated::kernel::proof::givenNames(&env.vars),
        ),
    ) {
        (Err(why), _) => Err(why),
        (_, Err(why)) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
        (_, Ok(vr)) => crate::proof_kernel::aver_generated::kernel::check::inductChecked(
            d,
            r,
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

/// One case per arm, the arms of the shape the descent needs. The cases see the carried hypotheses and those that mention nothing the induction varies.
pub fn inductChecked(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
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
        (aver_rt::AverInt::from_i64(r.arms.len() as i64)
            == aver_rt::AverInt::from_i64(cs.len() as i64)),
        crate::proof_kernel::aver_generated::kernel::check::measured(r, vr.v.clone(), env),
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
                                &(aver_rt::AverInt::from_i64(r.arms.len() as i64)),
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
        (_, Err(why)) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
        _ => crate::proof_kernel::aver_generated::kernel::check::inductCases(
            d,
            r,
            r.arms.clone(),
            cs.clone(),
            &crate::proof_kernel::aver_generated::kernel::induct::Call {
                args: xs.clone(),
                inner: aver_rt::AverList::empty(),
            },
            claim,
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

/// A match on the value itself needs one arm per constructor; a count toward zero needs v to be a given of type Int.
#[inline(always)]
pub fn measured(
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    v @ _: AverStr,
    env @ _: &Env,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match r.guard.clone() {
        None => {
            if crate::proof_kernel::aver_generated::kernel::check::armsAreConstructors(
                &crate::proof_kernel::aver_generated::kernel::check::armsOf(&r.arms),
            ) {
                Ok(())
            } else {
                Err(AverStr::from("the arms are not one per constructor"))
            }
        }
        Some(g @ _) => {
            match crate::proof_kernel::aver_generated::kernel::types::typeOf(
                &crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                    aver_rt::Shared::new(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v.clone()),
                    ),
                ),
                crate::proof_kernel::aver_generated::kernel::check::tyEnv(env),
            ) {
                Ok(__pat0) => match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt => Ok(()),
                    _ => Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" is not a given of type Int"));
                        __b
                    })),
                },
                _ => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" is not a given of type Int"));
                    __b
                })),
            }
        }
    }
}

/// The arms, without their calls.
#[inline(always)]
pub fn armsOf(
    acs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::ArmCalls>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(acs.clone(), [] => aver_rt::AverList::empty(), [a, rest] => aver_rt::AverList::prepend(a.arm, &crate::proof_kernel::aver_generated::kernel::check::armsOf(&rest)))
}

/// The hypotheses that mention none of the names.
#[inline(always)]
pub fn notMentioningAny(
    mut hs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    names @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> {
    let names @ _ = aver_rt::Shared::new(names);
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
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_rt::AverList::concat(
        &aver_rt::AverList::concat(
            &crate::proof_kernel::aver_generated::kernel::subst::freeVars(claim.lhs.clone()),
            &crate::proof_kernel::aver_generated::kernel::subst::freeVars(claim.rhs.clone()),
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
    match p {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PNil => true,
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            true
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
            let c = c.clone();
            let ns = ns.clone();
            true
        }
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
    match p {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            true
        }
        _ => false,
    }
}

/// Fresh names for what the case introduces: the arm's pattern variables, or in a count toward zero the hypothesis that the comparison has the arm's value. Then the claim in the case, under one hypothesis per recursive call at that call's arguments.
pub fn inductCase(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    a @ _: &crate::proof_kernel::aver_generated::kernel::induct::ArmCalls,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Case,
    at @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    taken @ _: &aver_rt::AverList<AverStr>,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        (aver_rt::AverInt::from_i64(c.binders.len() as i64)
            == crate::proof_kernel::aver_generated::kernel::check::caseNames(r, &a.arm)),
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
        _ => {
            let __arg11 @ _ =
                crate::proof_kernel::aver_generated::kernel::check::namesToVars(&c.binders);
            crate::proof_kernel::aver_generated::kernel::check::inductCaseNamed(
                d, r, a, c, &__arg11, at, claim, vr, carried, env, path,
            )
        }
    }
}

/// How many names a case introduces: one per part of the pattern, a wildcard part as well, or one hypothesis in a count toward zero.
#[inline(always)]
pub fn caseNames(
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Arm,
) -> aver_rt::AverInt {
    crate::proof_kernel::cancel_checkpoint();
    match r.guard.clone() {
        None => aver_rt::AverInt::from_i64(
            crate::proof_kernel::aver_generated::kernel::check::patFields(&a.pattern).len() as i64,
        ),
        Some(g @ _) => aver_rt::AverInt::from_i64(1),
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

/// No name is taken, a variable in scope (a given, or a name an earlier case introduced), a module-level binding, or repeated.
#[inline(always)]
pub fn freshAll(
    bs @ _: &aver_rt::AverList<AverStr>,
    taken @ _: &aver_rt::AverList<AverStr>,
    env @ _: &Env,
    before @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(bs.clone(), [] => true, [b, rest] => ((!(((taken.contains(&b) || (&*b == "_")) || crate::proof_kernel::aver_generated::kernel::proof::givenNames(&env.vars).contains(&b)) || (crate::proof_kernel::aver_generated::kernel::check::isConst(b.clone(), &env.consts) || before.contains(&b)))) && crate::proof_kernel::aver_generated::kernel::check::freshAll(&rest, taken, env, &aver_rt::AverList::prepend(b, &before.clone()))))
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
    aver_list_match!(ns.clone(), [] => aver_rt::AverList::empty(), [n, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(n))), &crate::proof_kernel::aver_generated::kernel::check::namesToVars(&rest)))
}

/// The goal, the carried hypotheses and in a count toward zero the arm's value of the comparison, then the hypotheses of the arm's recursive calls, those the gate checked to descend; the case's proof must prove the goal.
pub fn inductCaseNamed(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    a @ _: &crate::proof_kernel::aver_generated::kernel::induct::ArmCalls,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Case,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    at @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let names @ _ = crate::proof_kernel::aver_generated::kernel::subst::patNames(&a.arm.pattern);
    let rename @ _ = aver_rt::AverList::concat(
        &crate::proof_kernel::aver_generated::kernel::check::withoutNames(
            crate::proof_kernel::aver_generated::kernel::check::zipBind(
                &crate::proof_kernel::aver_generated::kernel::check::patFields(&a.arm.pattern),
                ys,
            ),
            aver_rt::AverList::from_vec(vec![AverStr::from("_")]),
        ),
        &crate::proof_kernel::aver_generated::kernel::check::withoutNames(
            crate::proof_kernel::aver_generated::kernel::check::zipBind(
                &crate::proof_kernel::aver_generated::kernel::types::paramsOf(d),
                &at.args,
            ),
            names,
        ),
    );
    let here @ _ = crate::proof_kernel::aver_generated::kernel::check::caseValue(
        &r.guard,
        a.arm.pattern.clone(),
        vr.v.clone(),
        ys,
        path.clone(),
    )?;
    let goal @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(
        claim.clone(),
        &here,
        path.clone(),
    )?;
    let scope @ _ = crate::proof_kernel::aver_generated::kernel::check::withVars(
        &crate::proof_kernel::aver_generated::kernel::check::withHyps(
            env,
            &aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::check::caseGuard(
                    &r.guard,
                    a.arm.pattern.clone(),
                    &c.binders,
                    &rename,
                    path.clone(),
                )?,
                &aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::check::hypsAt(
                        carried,
                        &here,
                        path.clone(),
                    )?,
                    &env.hyps.clone(),
                ),
            ),
        ),
        &crate::proof_kernel::aver_generated::kernel::check::caseVarsOf(
            &r.guard,
            a.arm.pattern.clone(),
            &c.binders,
            vr.v.clone(),
            env,
            path.clone(),
        )?,
    );
    match (
        (aver_rt::AverInt::from_i64(a.calls.len() as i64)
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
                                &(aver_rt::AverInt::from_i64(a.calls.len() as i64)),
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
                    &crate::proof_kernel::aver_generated::kernel::check::moreHyps(
                        &c.more,
                        &a.calls,
                        &rename,
                        r.at.clone(),
                        claim.clone(),
                        vr,
                        carried,
                        &scope,
                        path.clone(),
                    )?,
                    &aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::check::ihHyps(
                            &a.calls,
                            &c.ihs,
                            &c.carry,
                            &rename,
                            r.at.clone(),
                            claim,
                            vr,
                            carried,
                            &scope,
                            path.clone(),
                        )?,
                        &scope.hyps.clone(),
                    ),
                ),
            ),
            path,
        ),
    }
}

/// The case's names with the types of the pattern's parts over the given inducted on; none in a count toward zero, whose one name is a hypothesis.
#[inline(always)]
pub fn caseVarsOf(
    guard @ _: &Option<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut p @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
    binders @ _: &aver_rt::AverList<AverStr>,
    v @ _: AverStr,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    match guard.clone() {
        Some(g @ _) => Ok(aver_rt::AverList::empty()),
        None => crate::proof_kernel::aver_generated::kernel::check::patTypes(
            crate::proof_kernel::aver_generated::kernel::check::namedParts(p, binders),
            crate::proof_kernel::aver_generated::kernel::check::typeOf(
                &crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                    aver_rt::Shared::new(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v),
                    ),
                ),
                env,
                path.clone(),
            )?,
            env,
            path,
        ),
    }
}

/// The pattern with its parts, a wildcard part as well, taking these names in order.
pub fn namedParts(
    mut p @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
    names @ _: &aver_rt::AverList<AverStr>,
) -> crate::proof_kernel::aver_generated::kernel::term::Pat {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (p.clone(), names.clone());
        match __pat0 {
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
                let __list_subject = __pat1;
                if let Some((x, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat2;
                        if let Some((y, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat3;
                                if __list_subject.is_empty() {
                                    crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(
                                        x, y,
                                    )
                                } else {
                                    p
                                }
                            }
                        } else {
                            p
                        }
                    }
                } else {
                    p
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
                crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, names.clone())
            }
            _ => p,
        }
    }
}

/// Where the case puts the given inducted on: at the arm's pattern over the case's names; left as it is in a count toward zero.
#[inline(always)]
pub fn caseValue(
    guard @ _: &Option<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut p @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
    v @ _: AverStr,
    ys @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    match guard.clone() {
        None => Ok(aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::term::bind(
                v,
                crate::proof_kernel::aver_generated::kernel::check::patTerm(p, ys, path)?,
            ),
        ])),
        Some(g @ _) => Ok(aver_rt::AverList::empty()),
    }
}

/// In a count toward zero, the hypothesis the case's one name stands for: the comparison at the claim's arguments has the value of the arm's pattern.
pub fn caseGuard(
    guard @ _: &Option<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut p @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
    binders @ _: &aver_rt::AverList<AverStr>,
    rename @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1, __pat2) = (guard.clone(), p, binders.clone());
        match __pat0 {
            None => Ok(aver_rt::AverList::empty()),
            Some(g @ _) => match __pat1 {
                crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(__pat3) => {
                    match __pat3 {
                        crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                            __b_fields,
                        ) => {
                            let b = __b_fields.0.clone();
                            {
                                let __list_subject = __pat2;
                                if let Some((h, __pat4)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    {
                                        let __list_subject = __pat4;
                                        if __list_subject.is_empty() {
                                            Ok(aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: h, eqn: crate::proof_kernel::aver_generated::kernel::check::instantiate(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: g, rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(b))) }, rename, path)? }]))
                                        } else {
                                            Err(aver_rt::AverStr::from({
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(65))
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
                                                    ": the arm is not one value of the comparison",
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
                                                    (aver_rt::AverInt::from_i64(65))
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
                                            ": the arm is not one value of the comparison",
                                        ));
                                        __b
                                    }))
                                }
                            }
                        }
                        _ => Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(65)).to_usize().unwrap_or(0),
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
                                ": the arm is not one value of the comparison",
                            ));
                            __b
                        })),
                    }
                }
                _ => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(65)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&AverStr::from(
                        ": the arm is not one value of the comparison",
                    ));
                    __b
                })),
            },
        }
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
    aver_list_match!(hs.clone(), [] => Ok(aver_rt::AverList::empty()), [h, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: h.name, eqn: crate::proof_kernel::aver_generated::kernel::check::instantiate(h.eqn, bs, path.clone())? }, &crate::proof_kernel::aver_generated::kernel::check::hypsAt(&rest, bs, path)?)))
}

/// Bindings for names a pattern does not shadow.
#[inline(always)]
pub fn withoutNames(
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
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
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
                    claim.clone(),
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

/// One hypothesis per entry, later ones in front: the claim at the part recursive call K recurses on and at the entry's values of the varied givens, once each carried hypothesis is proved there.
#[inline(always)]
pub fn moreHyps(
    ms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::More>,
    calls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Call>,
    rename @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    j @ _: aver_rt::AverInt,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    scope @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ms.clone(), [] => Ok(aver_rt::AverList::empty()), [m, rest] => { match (crate::proof_kernel::aver_generated::kernel::check::callAt(calls.clone(), m.call.clone()), (aver_rt::AverInt::from_i64(m.ih.at.len() as i64) == aver_rt::AverInt::from_i64(vr.general.len() as i64)), (aver_rt::AverInt::from_i64(m.ih.carry.len() as i64) == aver_rt::AverInt::from_i64(carried.len() as i64))) {
        (None, _, _) => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(57)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&AverStr::from(": no recursive call ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(m.call)))); __b }))
        },
        (_, false, _) => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(104)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&AverStr::from(": ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(m.ih.name)))); __b }; __b.push_str(&AverStr::from(" gives ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(aver_rt::AverInt::from_i64(m.ih.at.len() as i64))))); __b }; __b.push_str(&AverStr::from(" values for ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(aver_rt::AverInt::from_i64(vr.general.len() as i64))))); __b }; __b.push_str(&AverStr::from(" varied givens")); __b }))
        },
        (_, _, false) => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(55)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&AverStr::from(": one proof per carried hypothesis")); __b }))
        },
        (Some(c), true, true) => {
            Ok(aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::check::moreHyps(&rest, calls, rename, j.clone(), claim.clone(), vr, carried, scope, path.clone())?, &crate::proof_kernel::aver_generated::kernel::check::ihHolds(&aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::check::partBinding(&crate::proof_kernel::aver_generated::kernel::check::ihAt(&c, rename, j, vr, path.clone())?, path.clone())?, &crate::proof_kernel::aver_generated::kernel::check::zipBind(&crate::proof_kernel::aver_generated::kernel::check::placeNames(&vr.general), &m.ih.at)), m.ih.name, &m.ih.carry, claim, carried, scope, path)?))
        }
    } })
}

/// The recursive call at place k.
#[inline(always)]
pub fn callAt(
    mut calls @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Call>,
    mut k @ _: aver_rt::AverInt,
) -> Option<crate::proof_kernel::aver_generated::kernel::induct::Call> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(calls, [] => { return None; }, [c, rest] => { if (k == aver_rt::AverInt::from_i64(0)) { return Some(c); } else { {
            let __tco0 = rest;
            let __tco1 = k.sub(&aver_rt::AverInt::from_i64(1));
            calls = __tco0;
            k = __tco1;
            continue;
        } } })
    }
}

/// The binding of the given inducted on, which ihAt puts first.
#[inline(always)]
pub fn partBinding(
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Binding, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(bs.clone(), [] => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(53)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&AverStr::from(": a recursive call binds nothing")); __b })) }, [b, rest] => Ok(b))
}

/// The hypothesis of one call, unless it is named _.
pub fn ihTaken(
    c @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    h @ _: AverStr,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    rename @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    j @ _: aver_rt::AverInt,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
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

/// The call's arguments fit the types of the variables they stand for; each carried hypothesis is proved at them; then the claim there.
pub fn ihHolds(
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    h @ _: AverStr,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    scope @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::bound(
        bs,
        &scope.vars,
        scope,
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(27)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("hypothesis "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
            __b
        }),
        path.clone(),
    )?;
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
    let ps @ _ = aver_rt::Shared::new(ps);
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
            crate::proof_kernel::aver_generated::kernel::term::bind(vr.v.clone(), at),
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
    aver_list_match!(ps.clone(), [] => aver_rt::AverList::empty(), [p, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::bind(p.name.clone(), crate::proof_kernel::aver_generated::kernel::induct::nthTerm(args.clone(), p.at).unwrap_or(crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(p.name.clone()))))), &crate::proof_kernel::aver_generated::kernel::check::placeBindings(&rest, args)))
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

/// Induction on a given of list type, for every value of the names in general: the claim at [], and at List.prepend(h, t) under one hypothesis per entry of ihs, the claim at t and that entry's values of general. The hypotheses named in carry hold in each case; an entry's hypothesis holds once they are proved at its values. Any other hypothesis that mentions v or what it generalises is out of scope.
pub fn listInduct(
    v @ _: AverStr,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carry @ _: &aver_rt::AverList<AverStr>,
    mut n @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    h @ _: AverStr,
    t @ _: AverStr,
    general @ _: &aver_rt::AverList<AverStr>,
    ihs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::IhAt>,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let carried @ _ = crate::proof_kernel::aver_generated::kernel::check::namedHyps(
        carry,
        &env.hyps,
        path.clone(),
    )?;
    match (
        crate::proof_kernel::aver_generated::kernel::check::listVar(v.clone(), env),
        general.contains(&v),
    ) {
        (None, _) => crate::proof_kernel::aver_generated::kernel::check::refuse(
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
        (_, true) => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(52)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                    __b
                };
                __b.push_str(&AverStr::from(" is both inducted on and generalised"));
                __b
            }),
        ),
        (Some(e), false) => {
            if crate::proof_kernel::aver_generated::kernel::check::freshAll(
                &aver_rt::AverList::from_vec(vec![h.clone(), t.clone()]),
                &crate::proof_kernel::aver_generated::kernel::check::takenNames(&claim, &env.hyps),
                env,
                &aver_rt::AverList::empty(),
            ) {
                crate::proof_kernel::aver_generated::kernel::check::listCases(
                    v.clone(),
                    e,
                    claim,
                    &carried.clone(),
                    n,
                    h,
                    t,
                    general,
                    ihs,
                    c,
                    &crate::proof_kernel::aver_generated::kernel::check::withHyps(
                        env,
                        &aver_rt::AverList::concat(
                            &carried,
                            &crate::proof_kernel::aver_generated::kernel::check::notMentioningAny(
                                env.hyps.clone(),
                                aver_rt::AverList::prepend(v, &general.clone()),
                            ),
                        ),
                    ),
                    path,
                )
            } else {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    AverStr::from("the names are not fresh"),
                )
            }
        }
    }
}

/// Induction on the length of a given v of list type, for every value of the names in general. With names [n, hn, ih] fresh for everything in scope, c proves the claim as it stands under the carried hypotheses, each a fact, those that mention neither v nor what it generalises, an Int n, hn saying v is at most n long, and the law ih: the claim at every v shorter than n and every value of general where the carried hypotheses hold. Then the claim holds whenever v is at most n long, for every n by induction on n, so for every v.
pub fn lenInduct(
    v @ _: AverStr,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carry @ _: &aver_rt::AverList<AverStr>,
    general @ _: &aver_rt::AverList<AverStr>,
    names @ _: &aver_rt::AverList<AverStr>,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let carried @ _ = crate::proof_kernel::aver_generated::kernel::check::namedHyps(
        carry,
        &env.hyps,
        path.clone(),
    )?;
    {
        let (__pat0, __pat1, __pat2) = (
            crate::proof_kernel::aver_generated::kernel::check::listVar(v.clone(), env),
            general.contains(&v),
            names.clone(),
        );
        match __pat0 {
            None => crate::proof_kernel::aver_generated::kernel::check::refuse(
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
                                    (aver_rt::AverInt::from_i64(52)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                __b
                            };
                            __b.push_str(&AverStr::from(" is both inducted on and generalised"));
                            __b
                        }),
                    )
                } else {
                    {
                        let __list_subject = __pat2;
                        if let Some((n, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat3;
                                if let Some((hn, __pat4)) =
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
                                                    if crate::proof_kernel::aver_generated::kernel::check::freshAll(names, &aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::check::takenNames(claim, &env.hyps), &crate::proof_kernel::aver_generated::kernel::check::hypLabels(&env.hyps)), env, &aver_rt::AverList::empty()) { crate::proof_kernel::aver_generated::kernel::check::lenCase(v.clone(), claim.clone(), &carried.clone(), general, n.clone(), hn, &crate::proof_kernel::aver_generated::kernel::proof::Law { key: ih, givens: crate::proof_kernel::aver_generated::kernel::check::typedNames(&aver_rt::AverList::prepend(v.clone(), &general.clone()), env, path.clone())?, premise: aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::check::conjoined(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("<"), crate::proof_kernel::aver_generated::kernel::term::Term::TBi(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("List.len"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v)))])))), crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(n)))))), &crate::proof_kernel::aver_generated::kernel::check::factsOf(&carried, path.clone())?)]), lhs: claim.lhs.clone(), rhs: claim.rhs.clone() }, c, env, path) } else { crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("the names are not fresh")) }
                                                } else {
                                                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("a length induction names a bound, its hypothesis and its law"))
                                                }
                                            }
                                        } else {
                                            crate::proof_kernel::aver_generated::kernel::check::refuse(path, AverStr::from("a length induction names a bound, its hypothesis and its law"))
                                        }
                                    }
                                } else {
                                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                                        path,
                                        AverStr::from(
                                            "a length induction names a bound, its hypothesis and its law",
                                        ),
                                    )
                                }
                            }
                        } else {
                            crate::proof_kernel::aver_generated::kernel::check::refuse(
                                path,
                                AverStr::from(
                                    "a length induction names a bound, its hypothesis and its law",
                                ),
                            )
                        }
                    }
                }
            }
        }
    }
}

/// The one case of a length induction: c proves the claim with the Int n and the hypothesis hn in scope, and the law ih cited as any other.
pub fn lenCase(
    v @ _: AverStr,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    general @ _: &aver_rt::AverList<AverStr>,
    n @ _: AverStr,
    hn @ _: AverStr,
    ih @ _: &crate::proof_kernel::aver_generated::kernel::proof::Law,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let bounded @ _ = crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: hn, eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: crate::proof_kernel::aver_generated::kernel::term::Term::TOp(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(AverStr::from("<="), crate::proof_kernel::aver_generated::kernel::term::Term::TBi(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("List.len"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v.clone())))])))), crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(n.clone())))))), rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true))) } };
    let hyps @ _ = aver_rt::AverList::prepend(
        bounded,
        &aver_rt::AverList::concat(
            &carried.clone(),
            &crate::proof_kernel::aver_generated::kernel::check::notMentioningAny(
                env.hyps.clone(),
                aver_rt::AverList::prepend(v, &general.clone()),
            ),
        ),
    );
    crate::proof_kernel::aver_generated::kernel::check::caseProves(
        c,
        &claim,
        crate::proof_kernel::aver_generated::kernel::check::Env {
            defs: env.defs.clone(),
            consts: env.consts.clone(),
            laws: aver_rt::AverList::prepend(ih.clone(), &env.laws.clone()),
            hyps: hyps,
            vars: aver_rt::AverList::prepend(
                crate::proof_kernel::aver_generated::kernel::term::typed(
                    n,
                    crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt,
                ),
                &env.vars.clone(),
            ),
            sums: env.sums.clone(),
            recs: env.recs.clone(),
            sigs: env.sigs.clone(),
        },
        aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&AverStr::from("/below"));
            __b
        }),
    )?;
    Ok(claim)
}

/// Each name with the type it has in scope.
#[inline(always)]
pub fn typedNames(
    ns @ _: &aver_rt::AverList<AverStr>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ns.clone(), [] => Ok(aver_rt::AverList::empty()), [x, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::typed(x.clone(), crate::proof_kernel::aver_generated::kernel::check::typeOf(&crate::proof_kernel::aver_generated::kernel::term::Term::TVar(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(x))), env, path.clone())?), &crate::proof_kernel::aver_generated::kernel::check::typedNames(&rest, env, path)?)))
}

/// What each hypothesis says is true; a refusal for one that is no such fact.
#[inline(always)]
pub fn factsOf(
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => Ok(aver_rt::AverList::empty()), [h, rest] => { if (h.eqn.rhs == crate::proof_kernel::aver_generated::kernel::term::Term::TBool(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true)))) { Ok(aver_rt::AverList::prepend(h.eqn.lhs, &crate::proof_kernel::aver_generated::kernel::check::factsOf(&rest, path)?)) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(53)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&AverStr::from(": ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h.name)))); __b }; __b.push_str(&AverStr::from(" is not a fact")); __b })) } })
}

/// t and each of rest, joined by Bool.and to the right.
#[inline(always)]
pub fn conjoined(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    rest @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(rest.clone(), [] => t, [x, more] => crate::proof_kernel::aver_generated::kernel::term::Term::TBi(aver_rt::Shared::new(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(AverStr::from("Bool.and"), aver_rt::AverList::from_vec(vec![t, crate::proof_kernel::aver_generated::kernel::check::conjoined(x, &more)])))))
}

/// The environment with a record R of two Int fields a and b declared.
pub fn withRec(env @ _: &Env) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::Env {
        defs: env.defs.clone(),
        consts: env.consts.clone(),
        laws: env.laws.clone(),
        hyps: env.hyps.clone(),
        vars: env.vars.clone(),
        sums: env.sums.clone(),
        recs: aver_rt::AverList::prepend(
            crate::proof_kernel::aver_generated::kernel::proof::Rec {
                name: AverStr::from("R"),
                eq: true,
                fields: aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::term::typed(
                        AverStr::from("a"),
                        crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt,
                    ),
                    crate::proof_kernel::aver_generated::kernel::term::typed(
                        AverStr::from("b"),
                        crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt,
                    ),
                ]),
            },
            &env.recs.clone(),
        ),
        sigs: env.sigs.clone(),
    }
}

/// No definitions, laws or hypotheses, and these givens of list type.
#[inline(always)]
pub fn listsEnv(ns @ _: &aver_rt::AverList<AverStr>) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::withVars(
        &crate::proof_kernel::aver_generated::kernel::check::emptyEnv(),
        &crate::proof_kernel::aver_generated::kernel::check::typedAll(
            ns,
            &crate::proof_kernel::aver_generated::kernel::term::Ty::TyList(aver_rt::Shared::new(
                crate::proof_kernel::aver_generated::kernel::term::Ty__TyList(
                    crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny,
                ),
            )),
        ),
    )
}

/// Each name with the one type.
#[inline(always)]
pub fn typedAll(
    ns @ _: &aver_rt::AverList<AverStr>,
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ns.clone(), [] => aver_rt::AverList::empty(), [n, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::typed(n, t.clone()), &crate::proof_kernel::aver_generated::kernel::check::typedAll(&rest, t)))
}

/// The empty-list case, then the cell case; each under the hypotheses in scope at its value of v, the head and the tail typed by what the list holds, and the cell case under one hypothesis per entry of ihs once the carried ones are proved there. A split is these cases with no entry.
pub fn listCases(
    v @ _: AverStr,
    mut e @ _: crate::proof_kernel::aver_generated::kernel::term::Ty,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    mut n @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    h @ _: AverStr,
    t @ _: AverStr,
    general @ _: &aver_rt::AverList<AverStr>,
    ihs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::IhAt>,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let atNil @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::term::bind(
            v.clone(),
            crate::proof_kernel::aver_generated::kernel::term::Term::TList(aver_rt::Shared::new(
                crate::proof_kernel::aver_generated::kernel::term::Term__TList(
                    aver_rt::AverList::empty(),
                ),
            )),
        ),
    ]);
    crate::proof_kernel::aver_generated::kernel::check::caseProves(
        n,
        &crate::proof_kernel::aver_generated::kernel::check::instantiate(
            claim.clone(),
            &atNil,
            path.clone(),
        )?,
        crate::proof_kernel::aver_generated::kernel::check::withHyps(
            env,
            &crate::proof_kernel::aver_generated::kernel::check::hypsAt(
                &env.hyps,
                &atNil,
                path.clone(),
            )?,
        ),
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
    let atCell @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_kernel::aver_generated::kernel::term::bind(
            v.clone(),
            crate::proof_kernel::aver_generated::kernel::term::Term::TBi(aver_rt::Shared::new(
                crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                    AverStr::from("List.prepend"),
                    aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                            aver_rt::Shared::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TVar(
                                    h.clone(),
                                ),
                            ),
                        ),
                        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                            aver_rt::Shared::new(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TVar(
                                    t.clone(),
                                ),
                            ),
                        ),
                    ]),
                ),
            )),
        ),
    ]);
    let inCell @ _ = crate::proof_kernel::aver_generated::kernel::check::withVars(
        &crate::proof_kernel::aver_generated::kernel::check::withHyps(
            env,
            &crate::proof_kernel::aver_generated::kernel::check::hypsAt(
                &env.hyps,
                &atCell,
                path.clone(),
            )?,
        ),
        &crate::proof_kernel::aver_generated::kernel::check::cellVars(h, t.clone(), e),
    );
    crate::proof_kernel::aver_generated::kernel::check::caseProves(
        c,
        &crate::proof_kernel::aver_generated::kernel::check::instantiate(
            claim.clone(),
            &atCell,
            path.clone(),
        )?,
        crate::proof_kernel::aver_generated::kernel::check::withHyps(
            &inCell.clone(),
            &aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::check::smallerHyps(
                    v,
                    crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                        aver_rt::Shared::new(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TVar(t),
                        ),
                    ),
                    general,
                    ihs,
                    claim.clone(),
                    carried,
                    &inCell,
                    path.clone(),
                )?,
                &inCell.hyps.clone(),
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
    Ok(claim)
}

/// A split of a list variable (a given of list type, or a tail an earlier split named) into [] and List.prepend(h, t), the names fresh for everything in scope: each case proves the claim with v replaced, in the hypotheses too, and the tail is a list variable in its case.
pub fn splitList(
    v @ _: AverStr,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut n @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    h @ _: AverStr,
    t @ _: AverStr,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match (
        crate::proof_kernel::aver_generated::kernel::check::listVar(v.clone(), env),
        crate::proof_kernel::aver_generated::kernel::check::freshAll(
            &aver_rt::AverList::from_vec(vec![h.clone(), t.clone()]),
            &aver_rt::AverList::prepend(
                v.clone(),
                &aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::check::takenNames(
                        &claim, &env.hyps,
                    ),
                    &crate::proof_kernel::aver_generated::kernel::check::hypLabels(&env.hyps),
                ),
            ),
            env,
            &aver_rt::AverList::empty(),
        ),
    ) {
        (None, _) => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                    __b
                };
                __b.push_str(&AverStr::from(" is not a variable of list type"));
                __b
            }),
        ),
        (_, false) => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            AverStr::from("the names are not fresh"),
        ),
        (Some(e), true) => crate::proof_kernel::aver_generated::kernel::check::listCases(
            v,
            e,
            claim,
            &aver_rt::AverList::empty(),
            n,
            h,
            t,
            &aver_rt::AverList::empty(),
            &aver_rt::AverList::empty(),
            c,
            env,
            path,
        ),
    }
}

/// The names of the hypotheses in scope.
#[inline(always)]
pub fn hypLabels(
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => aver_rt::AverList::empty(), [x, rest] => aver_rt::AverList::prepend(x.name, &crate::proof_kernel::aver_generated::kernel::check::hypLabels(&rest)))
}

/// No definitions, laws or hypotheses, and these givens of type Int.
#[inline(always)]
pub fn intsEnv(ns @ _: &aver_rt::AverList<AverStr>) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::check::withVars(
        &crate::proof_kernel::aver_generated::kernel::check::emptyEnv(),
        &crate::proof_kernel::aver_generated::kernel::check::typedAll(
            ns,
            &crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt,
        ),
    )
}

/// One hypothesis per entry, later ones in front: the claim with v at the smaller value and general at the entry's values, once each carried hypothesis is proved there.
#[inline(always)]
pub fn smallerHyps(
    v @ _: AverStr,
    mut smaller @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    general @ _: &aver_rt::AverList<AverStr>,
    ihs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::IhAt>,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    scope @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ihs.clone(), [] => Ok(aver_rt::AverList::empty()), [i, rest] => { match ((aver_rt::AverInt::from_i64(i.at.len() as i64) == aver_rt::AverInt::from_i64(general.len() as i64)), (aver_rt::AverInt::from_i64(i.carry.len() as i64) == aver_rt::AverInt::from_i64(cs.len() as i64))) {
        (false, _) => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(109)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&AverStr::from(": ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i.name)))); __b }; __b.push_str(&AverStr::from(" gives ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(aver_rt::AverInt::from_i64(i.at.len() as i64))))); __b }; __b.push_str(&AverStr::from(" values for ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(aver_rt::AverInt::from_i64(general.len() as i64))))); __b }; __b.push_str(&AverStr::from(" generalised givens")); __b }))
        },
        (_, false) => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(55)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&AverStr::from(": one proof per carried hypothesis")); __b }))
        },
        _ => {
            Ok(aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::check::smallerHyps(v.clone(), smaller.clone(), general, &rest, claim.clone(), cs, scope, path.clone())?, &crate::proof_kernel::aver_generated::kernel::check::ihHolds(&aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::bind(v, smaller), &crate::proof_kernel::aver_generated::kernel::check::zipBind(general, &i.at)), i.name, &i.carry, claim, cs, scope, path)?))
        }
    } })
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

/// The statements of checked facts, as laws a step may cite.
#[inline(always)]
pub fn factLaws(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => aver_rt::AverList::empty(), [f, rest] => aver_rt::AverList::prepend(f.law, &crate::proof_kernel::aver_generated::kernel::check::factLaws(&rest)))
}
