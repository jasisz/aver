#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

pub type Env = Box<Env__Fields>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Env__Fields {
    pub defs: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    pub consts: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    pub laws: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
    pub hyps: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    pub vars: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    pub sums: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Sum>,
    pub recs: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Rec>,
    pub sigs: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::FnSig>,
}

impl PartialOrd for Env__Fields {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Env__Fields {
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

impl aver_rt::AverDisplay for Env__Fields {
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
                {
                    let __t = p;
                    match __t.view() {
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PRefl(__t_fields) => {
            let t = &__t_fields.0;
            return Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: t.clone(), rhs: t.clone() })
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PSymm(__q_fields) => {
            let q = &__q_fields.0;
            { let __arg89 @ _ = { let __arg88 @ _ = (path + &::aver_rt::str_lit!("/symm")); crate::proof_kernel::aver_generated::kernel::check::conclude(q.clone(), env, __arg88) }?; return crate::proof_kernel::aver_generated::kernel::check::flip(__arg89) }
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PTrans(__ts_fields) => {
            let ts = &__ts_fields.0;
            let ps = &__ts_fields.1;
            return crate::proof_kernel::aver_generated::kernel::check::trans(ts, ps, &env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PCongr(__c_fields) => {
            let c = &__c_fields.0;
            let q = &__c_fields.1;
            return crate::proof_kernel::aver_generated::kernel::check::congr(c.clone(), q.clone(), &env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PUnfold(__f_fields) => {
            let f = __f_fields.0.clone();
            let k = __f_fields.1.clone();
            let xs = &__f_fields.2;
            let ys = &__f_fields.3;
            let pre = &__f_fields.4;
            return crate::proof_kernel::aver_generated::kernel::check::unfold(f, k, xs, ys, pre, env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PConst(__n_fields) => {
            let n = __n_fields.0.clone();
            return crate::proof_kernel::aver_generated::kernel::check::constant(n, &env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PArm(__k_fields) => {
            let k = __k_fields.0.clone();
            let ys = &__k_fields.1;
            let t = &__k_fields.2;
            let q = &__k_fields.3;
            return crate::proof_kernel::aver_generated::kernel::check::armStep(k, ys, t.clone(), q.clone(), env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PProj(__t_fields) => {
            let t = &__t_fields.0;
            return crate::proof_kernel::aver_generated::kernel::check::proj(t.clone(), &env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PCell(__t_fields) => {
            let t = &__t_fields.0;
            return crate::proof_kernel::aver_generated::kernel::check::cell(t.clone(), path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PHyp(__h_fields) => {
            let h = __h_fields.0.clone();
            return crate::proof_kernel::aver_generated::kernel::check::hyp(h, &env.hyps, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PRule(__id_fields) => {
            let id = __id_fields.0.clone();
            let bs = &__id_fields.1;
            let ps = &__id_fields.2;
            return crate::proof_kernel::aver_generated::kernel::check::rule(id, bs, ps, &env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PLaw(__k_fields) => {
            let k = __k_fields.0.clone();
            let bs = &__k_fields.1;
            let ps = &__k_fields.2;
            return crate::proof_kernel::aver_generated::kernel::check::lawStep(k, bs, ps, &env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PCompute(__a_fields) => {
            let a = &__a_fields.0;
            let b = &__a_fields.1;
            return crate::proof_kernel::aver_generated::kernel::check::compute(a.clone(), b.clone(), path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PCases(__on_fields) => {
            let on = &__on_fields.0;
            let h = __on_fields.1.clone();
            let t = &__on_fields.2;
            let f = &__on_fields.3;
            return crate::proof_kernel::aver_generated::kernel::check::cases(on.clone(), h, t.clone(), f.clone(), &env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PSplit(__on_fields) => {
            let on = &__on_fields.0;
            let h = __on_fields.1.clone();
            let lits = &__on_fields.2;
            let cs = &__on_fields.3;
            return crate::proof_kernel::aver_generated::kernel::check::split(on, h, lits, cs, &env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PHave(__h_fields) => {
            let h = __h_fields.0.clone();
            let fact = &__h_fields.1;
            let q = &__h_fields.2;
            let body = &__h_fields.3;
            __MutualTco1::Have(h, fact.clone(), q.clone(), body.clone(), env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PEnum(__v_fields) => {
            let v = __v_fields.0.clone();
            let l = &__v_fields.1;
            let r = &__v_fields.2;
            let cs = &__v_fields.3;
            return crate::proof_kernel::aver_generated::kernel::check::enumStep(v, l.clone(), r.clone(), cs, &env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PAbsurd(__q_fields) => {
            let q = &__q_fields.0;
            let l = &__q_fields.1;
            let r = &__q_fields.2;
            { let __arg91 @ _ = { let __arg90 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&::aver_rt::str_lit!("/absurd")); __b }); crate::proof_kernel::aver_generated::kernel::check::conclude(q.clone(), env, __arg90) }?; return crate::proof_kernel::aver_generated::kernel::check::absurd(__arg91, l.clone(), r.clone(), path) }
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PInduct(__f_fields) => {
            let f = __f_fields.0.clone();
            let xs = &__f_fields.1;
            let l = &__f_fields.2;
            let r = &__f_fields.3;
            let carry = &__f_fields.4;
            let cs = &__f_fields.5;
            { let __arg92 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l.clone(), rhs: r.clone() }; return crate::proof_kernel::aver_generated::kernel::check::induct(f, xs, &__arg92, carry, cs, &env, path) }
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PListInduct(__v_fields) => {
            let v = __v_fields.0.clone();
            let l = &__v_fields.1;
            let r = &__v_fields.2;
            let carry = &__v_fields.3;
            let n = &__v_fields.4;
            let h = __v_fields.5.clone();
            let t = __v_fields.6.clone();
            let general = &__v_fields.7;
            let ihs = &__v_fields.8;
            let c = &__v_fields.9;
            { let __arg93 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l.clone(), rhs: r.clone() }; return crate::proof_kernel::aver_generated::kernel::check::listInduct(v, __arg93, carry, n.clone(), h, t, general, ihs, c.clone(), &env, path) }
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PListCases(__v_fields) => {
            let v = __v_fields.0.clone();
            let l = &__v_fields.1;
            let r = &__v_fields.2;
            let n = &__v_fields.3;
            let h = __v_fields.4.clone();
            let t = __v_fields.5.clone();
            let c = &__v_fields.6;
            { let __arg94 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l.clone(), rhs: r.clone() }; return crate::proof_kernel::aver_generated::kernel::check::splitList(v, __arg94, n.clone(), h, t, c.clone(), &env, path) }
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PLenInduct(__v_fields) => {
            let v = __v_fields.0.clone();
            let l = &__v_fields.1;
            let r = &__v_fields.2;
            let carry = &__v_fields.3;
            let general = &__v_fields.4;
            let n = __v_fields.5.clone();
            let hn = __v_fields.6.clone();
            let ih = __v_fields.7.clone();
            let c = &__v_fields.8;
            { let __arg95 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l.clone(), rhs: r.clone() }; { let __arg96 @ _ = aver_rt::AverList::from_vec(vec![n, hn, ih]); return crate::proof_kernel::aver_generated::kernel::check::lenInduct(v, __arg95, carry, general, &__arg96, c.clone(), &env, path) } }
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PRing(__l_fields) => {
            let l = &__l_fields.0;
            let r = &__l_fields.1;
            return crate::proof_kernel::aver_generated::kernel::check::ring(l.clone(), r.clone(), &env, path)
        },
        crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PLinear(__g_fields) => {
            let g = &__g_fields.0;
            let v = __g_fields.1.clone();
            let hs = &__g_fields.2;
            let ws = &__g_fields.3;
            let cs = &__g_fields.4;
            return crate::proof_kernel::aver_generated::kernel::check::linear(g.clone(), v, hs, ws, cs, &env, path)
        }
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
                let proved @ _ = {
                    let __arg7 @ _ = aver_rt::AverStr::from({
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
                            __b.push_str(&::aver_rt::str_lit!("/have."));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                        __b
                    });
                    crate::proof_kernel::aver_generated::kernel::check::conclude(
                        q,
                        env.clone(),
                        __arg7,
                    )
                }?;
                if (proved
                    == crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: fact.clone(),
                        rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true),
                        ),
                    })
                {
                    {
                        let __arg8 @ _ =
                            crate::proof_kernel::aver_generated::kernel::check::withHyp(
                                &env, h, fact, true,
                            );
                        __MutualTco1::Conclude(body, __arg8, path)
                    }
                } else {
                    {
                        let __arg9 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(72)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("the proof of "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(
                                " ends at a different equation than its fact"
                            ));
                            __b
                        });
                        return crate::proof_kernel::aver_generated::kernel::check::refuse(
                            path, __arg9,
                        );
                    }
                }
            }
        };
    }
}

/// The equation a step proves, or the refusal of the first wrong step under it.
pub fn conclude(
    mut p @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut env @ _: Env,
    mut path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = p;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PRefl(__t_fields) => {
                let t = &__t_fields.0;
                return Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                    lhs: t.clone(),
                    rhs: t.clone(),
                });
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PSymm(__q_fields) => {
                let q = &__q_fields.0;
                {
                    let __arg89 @ _ = {
                        let __arg88 @ _ = (path + &::aver_rt::str_lit!("/symm"));
                        crate::proof_kernel::aver_generated::kernel::check::conclude(
                            q.clone(),
                            env,
                            __arg88,
                        )
                    }?;
                    return crate::proof_kernel::aver_generated::kernel::check::flip(__arg89);
                }
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PTrans(
                __ts_fields,
            ) => {
                let ts = &__ts_fields.0;
                let ps = &__ts_fields.1;
                return crate::proof_kernel::aver_generated::kernel::check::trans(
                    ts, ps, &env, path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PCongr(__c_fields) => {
                let c = &__c_fields.0;
                let q = &__c_fields.1;
                return crate::proof_kernel::aver_generated::kernel::check::congr(
                    c.clone(),
                    q.clone(),
                    &env,
                    path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PUnfold(
                __f_fields,
            ) => {
                let f = __f_fields.0.clone();
                let k = __f_fields.1.clone();
                let xs = &__f_fields.2;
                let ys = &__f_fields.3;
                let pre = &__f_fields.4;
                return crate::proof_kernel::aver_generated::kernel::check::unfold(
                    f, k, xs, ys, pre, env, path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PConst(__n_fields) => {
                let n = __n_fields.0.clone();
                return crate::proof_kernel::aver_generated::kernel::check::constant(n, &env, path);
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PArm(__k_fields) => {
                let k = __k_fields.0.clone();
                let ys = &__k_fields.1;
                let t = &__k_fields.2;
                let q = &__k_fields.3;
                return crate::proof_kernel::aver_generated::kernel::check::armStep(
                    k,
                    ys,
                    t.clone(),
                    q.clone(),
                    env,
                    path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PProj(__t_fields) => {
                let t = &__t_fields.0;
                return crate::proof_kernel::aver_generated::kernel::check::proj(
                    t.clone(),
                    &env,
                    path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PCell(__t_fields) => {
                let t = &__t_fields.0;
                return crate::proof_kernel::aver_generated::kernel::check::cell(t.clone(), path);
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PHyp(__h_fields) => {
                let h = __h_fields.0.clone();
                return crate::proof_kernel::aver_generated::kernel::check::hyp(h, &env.hyps, path);
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PRule(__id_fields) => {
                let id = __id_fields.0.clone();
                let bs = &__id_fields.1;
                let ps = &__id_fields.2;
                return crate::proof_kernel::aver_generated::kernel::check::rule(
                    id, bs, ps, &env, path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PLaw(__k_fields) => {
                let k = __k_fields.0.clone();
                let bs = &__k_fields.1;
                let ps = &__k_fields.2;
                return crate::proof_kernel::aver_generated::kernel::check::lawStep(
                    k, bs, ps, &env, path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PCompute(
                __a_fields,
            ) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return crate::proof_kernel::aver_generated::kernel::check::compute(
                    a.clone(),
                    b.clone(),
                    path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PCases(
                __on_fields,
            ) => {
                let on = &__on_fields.0;
                let h = __on_fields.1.clone();
                let t = &__on_fields.2;
                let f = &__on_fields.3;
                return crate::proof_kernel::aver_generated::kernel::check::cases(
                    on.clone(),
                    h,
                    t.clone(),
                    f.clone(),
                    &env,
                    path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PSplit(
                __on_fields,
            ) => {
                let on = &__on_fields.0;
                let h = __on_fields.1.clone();
                let lits = &__on_fields.2;
                let cs = &__on_fields.3;
                return crate::proof_kernel::aver_generated::kernel::check::split(
                    on, h, lits, cs, &env, path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PHave(__h_fields) => {
                let h = __h_fields.0.clone();
                let fact = &__h_fields.1;
                let q = &__h_fields.2;
                let body = &__h_fields.3;
                return __mutual_tco_trampoline_1(__MutualTco1::Have(
                    h,
                    fact.clone(),
                    q.clone(),
                    body.clone(),
                    env,
                    path,
                ));
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PEnum(__v_fields) => {
                let v = __v_fields.0.clone();
                let l = &__v_fields.1;
                let r = &__v_fields.2;
                let cs = &__v_fields.3;
                return crate::proof_kernel::aver_generated::kernel::check::enumStep(
                    v,
                    l.clone(),
                    r.clone(),
                    cs,
                    &env,
                    path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PAbsurd(
                __q_fields,
            ) => {
                let q = &__q_fields.0;
                let l = &__q_fields.1;
                let r = &__q_fields.2;
                {
                    let __arg91 @ _ = {
                        let __arg90 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!("/absurd"));
                            __b
                        });
                        crate::proof_kernel::aver_generated::kernel::check::conclude(
                            q.clone(),
                            env,
                            __arg90,
                        )
                    }?;
                    return crate::proof_kernel::aver_generated::kernel::check::absurd(
                        __arg91,
                        l.clone(),
                        r.clone(),
                        path,
                    );
                }
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PInduct(
                __f_fields,
            ) => {
                let f = __f_fields.0.clone();
                let xs = &__f_fields.1;
                let l = &__f_fields.2;
                let r = &__f_fields.3;
                let carry = &__f_fields.4;
                let cs = &__f_fields.5;
                {
                    let __arg92 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: l.clone(),
                        rhs: r.clone(),
                    };
                    return crate::proof_kernel::aver_generated::kernel::check::induct(
                        f, xs, &__arg92, carry, cs, &env, path,
                    );
                }
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PListInduct(
                __v_fields,
            ) => {
                let v = __v_fields.0.clone();
                let l = &__v_fields.1;
                let r = &__v_fields.2;
                let carry = &__v_fields.3;
                let n = &__v_fields.4;
                let h = __v_fields.5.clone();
                let t = __v_fields.6.clone();
                let general = &__v_fields.7;
                let ihs = &__v_fields.8;
                let c = &__v_fields.9;
                {
                    let __arg93 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: l.clone(),
                        rhs: r.clone(),
                    };
                    return crate::proof_kernel::aver_generated::kernel::check::listInduct(
                        v,
                        __arg93,
                        carry,
                        n.clone(),
                        h,
                        t,
                        general,
                        ihs,
                        c.clone(),
                        &env,
                        path,
                    );
                }
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PListCases(
                __v_fields,
            ) => {
                let v = __v_fields.0.clone();
                let l = &__v_fields.1;
                let r = &__v_fields.2;
                let n = &__v_fields.3;
                let h = __v_fields.4.clone();
                let t = __v_fields.5.clone();
                let c = &__v_fields.6;
                {
                    let __arg94 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: l.clone(),
                        rhs: r.clone(),
                    };
                    return crate::proof_kernel::aver_generated::kernel::check::splitList(
                        v,
                        __arg94,
                        n.clone(),
                        h,
                        t,
                        c.clone(),
                        &env,
                        path,
                    );
                }
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PLenInduct(
                __v_fields,
            ) => {
                let v = __v_fields.0.clone();
                let l = &__v_fields.1;
                let r = &__v_fields.2;
                let carry = &__v_fields.3;
                let general = &__v_fields.4;
                let n = __v_fields.5.clone();
                let hn = __v_fields.6.clone();
                let ih = __v_fields.7.clone();
                let c = &__v_fields.8;
                {
                    let __arg95 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: l.clone(),
                        rhs: r.clone(),
                    };
                    {
                        let __arg96 @ _ = aver_rt::AverList::from_vec(vec![n, hn, ih]);
                        return crate::proof_kernel::aver_generated::kernel::check::lenInduct(
                            v,
                            __arg95,
                            carry,
                            general,
                            &__arg96,
                            c.clone(),
                            &env,
                            path,
                        );
                    }
                }
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PRing(__l_fields) => {
                let l = &__l_fields.0;
                let r = &__l_fields.1;
                return crate::proof_kernel::aver_generated::kernel::check::ring(
                    l.clone(),
                    r.clone(),
                    &env,
                    path,
                );
            }
            crate::proof_kernel::aver_generated::kernel::proof::Proof__view::PLinear(
                __g_fields,
            ) => {
                let g = &__g_fields.0;
                let v = __g_fields.1.clone();
                let hs = &__g_fields.2;
                let ws = &__g_fields.3;
                let cs = &__g_fields.4;
                return crate::proof_kernel::aver_generated::kernel::check::linear(
                    g.clone(),
                    v,
                    hs,
                    ws,
                    cs,
                    &env,
                    path,
                );
            }
        }
    }
}

/// A cut: q proves fact = true in the current scope, then body is checked with it as hypothesis h.
pub fn have(
    mut h @ _: AverStr,
    mut fact @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut body @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut env @ _: Env,
    mut path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let proved @ _ = {
        let __arg7 @ _ = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(38)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!("/have."));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
            __b
        });
        crate::proof_kernel::aver_generated::kernel::check::conclude(q, env.clone(), __arg7)
    }?;
    if (proved
        == crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: fact.clone(),
            rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true),
            ),
        })
    {
        {
            let __arg8 @ _ =
                crate::proof_kernel::aver_generated::kernel::check::withHyp(&env, h, fact, true);
            return __mutual_tco_trampoline_1(__MutualTco1::Conclude(body, __arg8, path));
        }
    } else {
        {
            let __arg9 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(72)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("the proof of "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(
                    " ends at a different equation than its fact"
                ));
                __b
            });
            return crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg9);
        }
    }
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
                aver_list_match!(cs, [] => { return crate::proof_kernel::aver_generated::kernel::check::splitDone(&before, path) }, [c, rest] => { if { let __arg9 @ _ = aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::subst::freeVars(&*on), &crate::proof_kernel::aver_generated::kernel::check::hypNames(&env.hyps)); { let __arg10 @ _ = <aver_rt::AverList<AverStr>>::empty(); crate::proof_kernel::aver_generated::kernel::check::freshAll(&c.binders, &__arg9, &*env, &__arg10) } } { { let __arg16 @ _ = { let __arg14 @ _ = { let __arg12 @ _ = { let __arg11 @ _ = aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::check::firstHyps(&hss), &env.hyps.clone()); crate::proof_kernel::aver_generated::kernel::check::withHyps(&*env, &__arg11) }; { let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::check::firstVars(&vss); crate::proof_kernel::aver_generated::kernel::check::withVars(&__arg12, &__arg13) } }; { let __arg15 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&::aver_rt::str_lit!("/split.")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.ctor)))); __b }); crate::proof_kernel::aver_generated::kernel::check::conclude(c.proof.clone(), __arg14, __arg15) } }?; { let __arg17 @ _ = { let __n = aver_rt::clamp_list_count(&(aver_rt::AverInt::from_i64(1))); (hss).drop_first(__n) }; { let __arg18 @ _ = { let __n = aver_rt::clamp_list_count(&(aver_rt::AverInt::from_i64(1))); (vss).drop_first(__n) }; __MutualTco2::SplitNext(__arg16, c, rest, __arg17, __arg18, before, path) } } } } else { { let __arg19 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("the names of case ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.ctor)))); __b }; __b.push_str(&::aver_rt::str_lit!(" are not fresh")); __b }); return crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg19) } } })
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
                {
                    let __subject = (
                        {
                            let __arg9 @ _ = aver_rt::AverList::concat(
                                &crate::proof_kernel::aver_generated::kernel::subst::freeVars(
                                    &e.lhs,
                                ),
                                &crate::proof_kernel::aver_generated::kernel::subst::freeVars(
                                    &e.rhs,
                                ),
                            );
                            crate::proof_kernel::aver_generated::kernel::check::anyIn(
                                &__arg9, &c.binders,
                            )
                        },
                        ((aver_rt::AverInt::from_i64(before.len() as i64)
                            == aver_rt::AverInt::from_i64(0))
                            || (before == aver_rt::AverList::from_vec(vec![e.clone()]))),
                    );
                    match __subject {
                        (true, _) => {
                            let __arg10 @ _ = aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(60))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("case "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(c.ctor),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(
                                    " proves an equation about its own names"
                                ));
                                __b
                            });
                            return crate::proof_kernel::aver_generated::kernel::check::refuse(
                                path, __arg10,
                            );
                        }
                        (_, false) => {
                            return crate::proof_kernel::aver_generated::kernel::check::refuse(
                                path,
                                ::aver_rt::str_lit!("the cases prove different equations"),
                            );
                        }
                        _ => {
                            let __arg11 @ _ = aver_rt::AverList::from_vec(vec![e]);
                            __MutualTco2::SplitCases(rest, hss, vss, __arg11, path)
                        }
                    }
                }
            }
        };
    }
}

/// Each case under its hypotheses and its typed names, fresh for on, the hypotheses, the variables and the constants; every case proves the same equation, which names none of them.
pub fn splitCases(
    mut cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    mut hss @ _: aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    >,
    mut vss @ _: aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    >,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    mut before @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    env @ _: &Env,
    mut path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(cs, [] => { return crate::proof_kernel::aver_generated::kernel::check::splitDone(&before, path) }, [c, rest] => { if { let __arg9 @ _ = aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::subst::freeVars(&*on), &crate::proof_kernel::aver_generated::kernel::check::hypNames(&env.hyps)); { let __arg10 @ _ = <aver_rt::AverList<AverStr>>::empty(); crate::proof_kernel::aver_generated::kernel::check::freshAll(&c.binders, &__arg9, &*env, &__arg10) } } { { let __arg16 @ _ = { let __arg14 @ _ = { let __arg12 @ _ = { let __arg11 @ _ = aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::check::firstHyps(&hss), &env.hyps.clone()); crate::proof_kernel::aver_generated::kernel::check::withHyps(&*env, &__arg11) }; { let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::check::firstVars(&vss); crate::proof_kernel::aver_generated::kernel::check::withVars(&__arg12, &__arg13) } }; { let __arg15 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&::aver_rt::str_lit!("/split.")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.ctor)))); __b }); crate::proof_kernel::aver_generated::kernel::check::conclude(c.proof.clone(), __arg14, __arg15) } }?; { let __arg17 @ _ = { let __n = aver_rt::clamp_list_count(&(aver_rt::AverInt::from_i64(1))); (hss).drop_first(__n) }; { let __arg18 @ _ = { let __n = aver_rt::clamp_list_count(&(aver_rt::AverInt::from_i64(1))); (vss).drop_first(__n) }; return __mutual_tco_trampoline_2(__MutualTco2::SplitNext(__arg16, c, rest, __arg17, __arg18, before, path), on, env) } } } } else { { let __arg19 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("the names of case ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.ctor)))); __b }; __b.push_str(&::aver_rt::str_lit!(" are not fresh")); __b }); return crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg19) } } })
}

/// One case's equation: it names none of the case's names, and it is the one every earlier case proved.
pub fn splitNext(
    mut e @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::SplitCase,
    mut rest @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    mut hss @ _: aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    >,
    mut vss @ _: aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    >,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    mut before @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    env @ _: &Env,
    mut path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = (
            {
                let __arg9 @ _ = aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::subst::freeVars(&e.lhs),
                    &crate::proof_kernel::aver_generated::kernel::subst::freeVars(&e.rhs),
                );
                crate::proof_kernel::aver_generated::kernel::check::anyIn(&__arg9, &c.binders)
            },
            ((aver_rt::AverInt::from_i64(before.len() as i64) == aver_rt::AverInt::from_i64(0))
                || (before == aver_rt::AverList::from_vec(vec![e.clone()]))),
        );
        match __subject {
            (true, _) => {
                let __arg10 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(60)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("case "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.ctor))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(
                        " proves an equation about its own names"
                    ));
                    __b
                });
                return crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg10);
            }
            (_, false) => {
                return crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    ::aver_rt::str_lit!("the cases prove different equations"),
                );
            }
            _ => {
                let __arg11 @ _ = aver_rt::AverList::from_vec(vec![e]);
                return __mutual_tco_trampoline_2(
                    __MutualTco2::SplitCases(rest, hss, vss, __arg11, path),
                    on,
                    env,
                );
            }
        }
    }
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
                        {
                            let __arg11 @ _ = aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(53)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!(
                                        "the cases do not match the values of "
                                    ));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                __b
                            });
                            return crate::proof_kernel::aver_generated::kernel::check::refuse(
                                path, __arg11,
                            );
                        }
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
                        __b.push_str(&::aver_rt::str_lit!("/enum."));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
                    __b
                });
                let got @ _ = {
                    let __arg14 @ _ = {
                        let __arg13 @ _ =
                            crate::proof_kernel::aver_generated::kernel::check::substHyps(
                                &env.hyps,
                                &at,
                                path.clone(),
                            )?;
                        crate::proof_kernel::aver_generated::kernel::check::withHyps(
                            &*env, &__arg13,
                        )
                    };
                    crate::proof_kernel::aver_generated::kernel::check::conclude(
                        c,
                        __arg14,
                        here.clone(),
                    )
                }?;
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
                        ::aver_rt::str_lit!("the case proves a different equation"),
                    );
                }
            }
        };
    }
}

/// One case per value, in order, and nothing else.
pub fn enumCases(
    mut v @ _: AverStr,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut vals @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    mut path @ _: AverStr,
    mut i @ _: aver_rt::AverInt,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
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
            return __mutual_tco_trampoline_3(
                __MutualTco3::EnumCase(v, x, c, xs, rest, path, i),
                claim,
                env,
            );
        } else {
            {
                let __arg11 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(53)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!(
                            "the cases do not match the values of "
                        ));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                    __b
                });
                return crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg11);
            }
        }
    }
}

/// The claim at one value, under the hypotheses at that value.
pub fn enumCase(
    mut v @ _: AverStr,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut x @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut c @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut xs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut rest @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    env @ _: &Env,
    mut path @ _: AverStr,
    mut i @ _: aver_rt::AverInt,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
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
            __b.push_str(&::aver_rt::str_lit!("/enum."));
            __b
        };
        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
        __b
    });
    let got @ _ = {
        let __arg14 @ _ = {
            let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::check::substHyps(
                &env.hyps,
                &at,
                path.clone(),
            )?;
            crate::proof_kernel::aver_generated::kernel::check::withHyps(&*env, &__arg13)
        };
        crate::proof_kernel::aver_generated::kernel::check::conclude(c, __arg14, here.clone())
    }?;
    let want @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(
        (*claim).clone(),
        &at,
        path.clone(),
    )?;
    if (got == want) {
        return __mutual_tco_trampoline_3(
            __MutualTco3::EnumCases(v, xs, rest, path, i.add(&aver_rt::AverInt::from_i64(1))),
            claim,
            env,
        );
    } else {
        return crate::proof_kernel::aver_generated::kernel::check::refuse(
            here,
            ::aver_rt::str_lit!("the case proves a different equation"),
        );
    }
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
                                    __b.push_str(&::aver_rt::str_lit!("step "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(": wrong number of premises"));
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
                let got @ _ = {
                    let __arg10 @ _ = aver_rt::AverStr::from({
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
                            __b.push_str(&::aver_rt::str_lit!("/premise."));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
                        __b
                    });
                    crate::proof_kernel::aver_generated::kernel::check::conclude(
                        p,
                        (*env).clone(),
                        __arg10,
                    )
                }?;
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
                                        __b.push_str(&::aver_rt::str_lit!("step "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(path),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(": premise "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" does not match"));
                        __b
                    }));
                }
            }
        };
    }
}

/// Each premise proof proves its premise at the substitution.
pub fn premisesHold(
    mut wanted @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    mut ps @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
    mut path @ _: AverStr,
    mut i @ _: aver_rt::AverInt,
) -> Result<(), AverStr> {
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
            return __mutual_tco_trampoline_4(
                __MutualTco4::PremiseHolds(w, p, ws, rest, path, i),
                bs,
                env,
            );
        } else {
            return Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("step "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(": wrong number of premises"));
                __b
            }));
        }
    }
}

/// One premise, then the rest.
pub fn premiseHolds(
    mut w @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut p @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    mut ws @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    mut rest @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
    mut path @ _: AverStr,
    mut i @ _: aver_rt::AverInt,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let want @ _ =
        crate::proof_kernel::aver_generated::kernel::check::instantiate(w, &*bs, path.clone())?;
    let got @ _ = {
        let __arg10 @ _ = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!("/premise."));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
            __b
        });
        crate::proof_kernel::aver_generated::kernel::check::conclude(p, (*env).clone(), __arg10)
    }?;
    if (got == want) {
        return __mutual_tco_trampoline_4(
            __MutualTco4::PremisesHold(ws, rest, path, i.add(&aver_rt::AverInt::from_i64(1))),
            bs,
            env,
        );
    } else {
        return Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(62)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": premise "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(" does not match"));
            __b
        }));
    }
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
                        {
                            let __arg17 @ _ = {
                                let __arg16 @ _ = aver_rt::AverStr::from({
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
                                        __b.push_str(&::aver_rt::str_lit!("/case."));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(i),
                                    )));
                                    __b
                                });
                                crate::proof_kernel::aver_generated::kernel::check::inductCase(
                                    &*d,
                                    &*r,
                                    a,
                                    c,
                                    &*at,
                                    (*claim).clone(),
                                    &*vr,
                                    &*taken,
                                    &*carried,
                                    &*env,
                                    __arg16,
                                )
                            };
                            __MutualTco5::InductNext(
                                __arg17,
                                moreArms,
                                moreCases,
                                path,
                                i.add(&aver_rt::AverInt::from_i64(1)),
                            )
                        }
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
    mut acs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::ArmCalls>,
    mut cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
    at @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    taken @ _: &aver_rt::AverList<AverStr>,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    env @ _: &Env,
    mut path @ _: AverStr,
    mut i @ _: aver_rt::AverInt,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
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
            {
                let __arg17 @ _ = {
                    let __arg16 @ _ = aver_rt::AverStr::from({
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
                            __b.push_str(&::aver_rt::str_lit!("/case."));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
                        __b
                    });
                    crate::proof_kernel::aver_generated::kernel::check::inductCase(
                        &*d,
                        &*r,
                        a,
                        c,
                        &*at,
                        (*claim).clone(),
                        &*vr,
                        &*taken,
                        &*carried,
                        &*env,
                        __arg16,
                    )
                };
                return __mutual_tco_trampoline_5(
                    __MutualTco5::InductNext(
                        __arg17,
                        moreArms,
                        moreCases,
                        path,
                        i.add(&aver_rt::AverInt::from_i64(1)),
                    ),
                    d,
                    r,
                    at,
                    claim,
                    vr,
                    taken,
                    carried,
                    env,
                );
            }
        } else {
            return Ok((*claim).clone());
        }
    }
}

/// The rest of the cases, once this one checked.
pub fn inductNext(
    mut done @ _: Result<(), AverStr>,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    mut acs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::ArmCalls>,
    mut cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>,
    at @ _: &crate::proof_kernel::aver_generated::kernel::induct::Call,
    claim @ _: &crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    taken @ _: &aver_rt::AverList<AverStr>,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    env @ _: &Env,
    mut path @ _: AverStr,
    mut i @ _: aver_rt::AverInt,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    done?;
    return __mutual_tco_trampoline_5(
        __MutualTco5::InductCases(acs, cs, path, i),
        d,
        r,
        at,
        claim,
        vr,
        taken,
        carried,
        env,
    );
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
                        {
                            let __arg11 @ _ = {
                                let __arg9 @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(c.eqn, &*down, path.clone())?;
                                {
                                    let __arg10 @ _ = aver_rt::AverStr::from({
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
                                            __b.push_str(&::aver_rt::str_lit!("/carry."));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(c.name)),
                                        ));
                                        __b
                                    });
                                    crate::proof_kernel::aver_generated::kernel::check::caseProves(
                                        q,
                                        &__arg9,
                                        (*env).clone(),
                                        __arg10,
                                    )
                                }
                            };
                            __MutualTco6::CarriedThen(__arg11, more, qs, path)
                        }
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
    mut cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    mut proofs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    down @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
    mut path @ _: AverStr,
) -> Result<(), AverStr> {
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
            {
                let __arg11 @ _ = {
                    let __arg9 @ _ =
                        crate::proof_kernel::aver_generated::kernel::check::instantiate(
                            c.eqn,
                            &*down,
                            path.clone(),
                        )?;
                    {
                        let __arg10 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(path),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!("/carry."));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.name))));
                            __b
                        });
                        crate::proof_kernel::aver_generated::kernel::check::caseProves(
                            q,
                            &__arg9,
                            (*env).clone(),
                            __arg10,
                        )
                    }
                };
                return __mutual_tco_trampoline_6(
                    __MutualTco6::CarriedThen(__arg11, more, qs, path),
                    down,
                    env,
                );
            }
        } else {
            return Ok(());
        }
    }
}

/// The rest of the carried hypotheses, once this one held.
pub fn carriedThen(
    mut done @ _: Result<(), AverStr>,
    mut cs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    mut proofs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
    down @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    env @ _: &Env,
    mut path @ _: AverStr,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    done?;
    return __mutual_tco_trampoline_6(__MutualTco6::CarriedHold(cs, proofs, path), down, env);
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
                aver_list_match!(fs, [] => { return Ok(()) }, [f, rest] => { { let __arg5 @ _ = { let __arg4 @ _ = Box::new(crate::proof_kernel::aver_generated::kernel::proof::Script__Fields { obligation: f.law.clone(), defs: aver_rt::AverList::empty(), consts: aver_rt::AverList::empty(), sums: aver_rt::AverList::empty(), recs: aver_rt::AverList::empty(), sigs: aver_rt::AverList::empty(), laws: earlier.clone(), facts: aver_rt::AverList::empty(), proof: f.proof }); crate::proof_kernel::aver_generated::kernel::check::checkScript(__arg4) }; __MutualTco7::CheckFactThen(__arg5, f.law, rest, earlier) } })
            }
            __MutualTco7::CheckFactThen(
                mut done @ _,
                mut law @ _,
                mut rest @ _,
                mut earlier @ _,
            ) => {
                crate::proof_kernel::cancel_checkpoint();
                match done {
                    Ok(k @ _) => {
                        let __arg6 @ _ = aver_rt::AverList::concat(
                            &earlier,
                            &aver_rt::AverList::from_vec(vec![law]),
                        );
                        __MutualTco7::CheckFacts(rest, __arg6)
                    }
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
                                        __b.push_str(&::aver_rt::str_lit!("fact "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(law.key),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(": "));
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
    mut fs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
    mut earlier @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(fs, [] => { return Ok(()) }, [f, rest] => { { let __arg5 @ _ = { let __arg4 @ _ = Box::new(crate::proof_kernel::aver_generated::kernel::proof::Script__Fields { obligation: f.law.clone(), defs: aver_rt::AverList::empty(), consts: aver_rt::AverList::empty(), sums: aver_rt::AverList::empty(), recs: aver_rt::AverList::empty(), sigs: aver_rt::AverList::empty(), laws: earlier.clone(), facts: aver_rt::AverList::empty(), proof: f.proof }); crate::proof_kernel::aver_generated::kernel::check::checkScript(__arg4) }; return __mutual_tco_trampoline_7(__MutualTco7::CheckFactThen(__arg5, f.law, rest, earlier)) } })
}

/// The rest of the facts, once this one checked; they may cite it.
pub fn checkFactThen(
    mut done @ _: Result<AverStr, AverStr>,
    mut law @ _: crate::proof_kernel::aver_generated::kernel::proof::Law,
    mut rest @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
    mut earlier @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match done {
        Ok(k @ _) => {
            let __arg6 @ _ =
                aver_rt::AverList::concat(&earlier, &aver_rt::AverList::from_vec(vec![law]));
            return __mutual_tco_trampoline_7(__MutualTco7::CheckFacts(rest, __arg6));
        }
        Err(why @ _) => {
            return Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("fact "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(law.key))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                __b
            }));
        }
    }
}

/// No definitions, laws or hypotheses.
pub fn emptyEnv() -> Env {
    crate::proof_kernel::cancel_checkpoint();
    Box::new(
        crate::proof_kernel::aver_generated::kernel::check::Env__Fields {
            defs: aver_rt::AverList::empty(),
            consts: aver_rt::AverList::empty(),
            laws: aver_rt::AverList::empty(),
            hyps: aver_rt::AverList::empty(),
            vars: aver_rt::AverList::empty(),
            sums: aver_rt::AverList::empty(),
            recs: aver_rt::AverList::empty(),
            sigs: aver_rt::AverList::empty(),
        },
    )
}

/// What typing a term needs from the environment.
pub fn tyEnv(env @ _: &Env) -> crate::proof_kernel::aver_generated::kernel::types::TyEnv {
    crate::proof_kernel::cancel_checkpoint();
    Box::new(
        crate::proof_kernel::aver_generated::kernel::types::TyEnv__Fields {
            vars: env.vars.clone(),
            defs: env.defs.clone(),
            consts: env.consts.clone(),
            sums: env.sums.clone(),
            recs: env.recs.clone(),
            sigs: env.sigs.clone(),
        },
    )
}

/// The environment with these variables in front.
pub fn withVars(
    env @ _: &Env,
    vs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    Box::new(
        crate::proof_kernel::aver_generated::kernel::check::Env__Fields {
            defs: env.defs.clone(),
            consts: env.consts.clone(),
            laws: env.laws.clone(),
            hyps: env.hyps.clone(),
            vars: aver_rt::AverList::concat(&vs.clone(), &env.vars.clone()),
            sums: env.sums.clone(),
            recs: env.recs.clone(),
            sigs: env.sigs.clone(),
        },
    )
}

/// The type of a term, a refusal naming the step when it has none.
#[inline(always)]
pub fn typeOf(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = {
            let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
            crate::proof_kernel::aver_generated::kernel::types::typeOf(t, &__arg5)
        };
        match __subject {
            Ok(ty @ _) => Ok(ty),
            Err(why @ _) => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                __b
            })),
        }
    }
}

/// The same equation after checking that its two sides have one type.
#[inline(always)]
pub fn typedEquation(
    mut e @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = {
            let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::check::typeOf(
                &e.lhs,
                env,
                path.clone(),
            )?;
            {
                let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::check::typeOf(
                    &e.rhs,
                    env,
                    path.clone(),
                )?;
                crate::proof_kernel::aver_generated::kernel::types::join(__arg5, __arg6)
            }
        };
        match __subject {
            Ok(ty @ _) => Ok(e),
            Err(why @ _) => {
                let __arg7 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(38)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("the equation compares "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg7)
            }
        }
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
    {
        let __subject = {
            let __arg7 @ _ = <aver_rt::AverList<
                crate::proof_kernel::aver_generated::kernel::term::TypedVar,
            >>::empty();
            {
                let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
                crate::proof_kernel::aver_generated::kernel::types::bindTyped(
                    bs, declared, __arg7, &__arg8, what,
                )
            }
        };
        match __subject {
            Ok(b @ _) => Ok(()),
            Err(why @ _) => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                __b
            })),
        }
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
        {
            let __arg5 @ _ = {
                let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::types::paramsOf(d);
                crate::proof_kernel::aver_generated::kernel::check::zipBind(&__arg4, xs)
            };
            crate::proof_kernel::aver_generated::kernel::check::bound(
                &__arg5,
                &d.params,
                env,
                d.name.clone(),
                path,
            )
        }
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
                                    __b.push_str(&::aver_rt::str_lit!("step "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(": "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" takes "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(aver_rt::AverInt::from_i64(d.params.len() as i64)),
                )));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(" arguments"));
            __b
        }))
    }
}

/// Every definition body and constant value fits the type its declaration gives it; theorem givens are not in their scope.
pub fn declarationsTyped(env @ _: &Env, path @ _: AverStr) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let scope @ _ = Box::new(
        crate::proof_kernel::aver_generated::kernel::check::Env__Fields {
            defs: env.defs.clone(),
            consts: env.consts.clone(),
            laws: aver_rt::AverList::empty(),
            hyps: aver_rt::AverList::empty(),
            vars: aver_rt::AverList::empty(),
            sums: env.sums.clone(),
            recs: env.recs.clone(),
            sigs: env.sigs.clone(),
        },
    );
    {
        let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::check::defNames(&scope.defs);
        {
            let __arg14 @ _ = <aver_rt::AverList<AverStr>>::empty();
            {
                let __arg15 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": duplicate definition"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::types::unique(
                    &__arg13, __arg14, __arg15,
                )
            }
        }
    }?;
    {
        let __arg16 @ _ = crate::proof_kernel::aver_generated::kernel::check::sigNames(&scope.sigs);
        {
            let __arg17 @ _ = <aver_rt::AverList<AverStr>>::empty();
            {
                let __arg18 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(42)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": duplicate signature"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::types::unique(
                    &__arg16, __arg17, __arg18,
                )
            }
        }
    }?;
    {
        let __arg19 @ _ = aver_rt::AverList::concat(
            &crate::proof_kernel::aver_generated::kernel::check::defNames(&scope.defs),
            &crate::proof_kernel::aver_generated::kernel::check::sigNames(&scope.sigs),
        );
        {
            let __arg20 @ _ = <aver_rt::AverList<AverStr>>::empty();
            {
                let __arg21 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(58)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(
                        ": definition and signature share name"
                    ));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::types::unique(
                    &__arg19, __arg20, __arg21,
                )
            }
        }
    }?;
    {
        let __arg22 @ _ =
            crate::proof_kernel::aver_generated::kernel::check::constNames(&scope.consts);
        {
            let __arg23 @ _ = <aver_rt::AverList<AverStr>>::empty();
            {
                let __arg24 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(53)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": duplicate module-level binding"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::types::unique(
                    &__arg22, __arg23, __arg24,
                )
            }
        }
    }?;
    {
        let __arg25 @ _ = aver_rt::AverList::concat(
            &crate::proof_kernel::aver_generated::kernel::check::constNames(&scope.consts),
            &crate::proof_kernel::aver_generated::kernel::proof::givenNames(&env.vars),
        );
        {
            let __arg26 @ _ = <aver_rt::AverList<AverStr>>::empty();
            {
                let __arg27 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(67)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(
                        ": module-level binding and variable share name"
                    ));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::types::unique(
                    &__arg25, __arg26, __arg27,
                )
            }
        }
    }?;
    crate::proof_kernel::aver_generated::kernel::check::declarationsAcyclic(
        &scope.consts,
        &scope.defs,
        &scope.consts,
        path.clone(),
    )?;
    {
        let __arg28 @ _ =
            crate::proof_kernel::aver_generated::kernel::check::typeNames(&scope.sums, &scope.recs);
        {
            let __arg29 @ _ = <aver_rt::AverList<AverStr>>::empty();
            {
                let __arg30 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(49)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": duplicate type declaration"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::types::unique(
                    &__arg28, __arg29, __arg30,
                )
            }
        }
    }?;
    {
        let __arg31 @ _ = crate::proof_kernel::aver_generated::kernel::check::tyEnv(&scope);
        crate::proof_kernel::aver_generated::kernel::types::sumVariantsTyped(
            &scope.sums,
            &__arg31,
            path.clone(),
        )
    }?;
    crate::proof_kernel::aver_generated::kernel::types::recordFieldsUnique(
        &scope.recs,
        path.clone(),
    )?;
    crate::proof_kernel::aver_generated::kernel::check::defsTyped(
        &scope.defs,
        &scope,
        path.clone(),
    )?;
    crate::proof_kernel::aver_generated::kernel::check::constsTyped(&scope.consts, &scope, path)
}

/// The names the script's definitions declare, in order.
#[inline(always)]
pub fn defNames(
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ds;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(d), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    d.name.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::defNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The names the script's external signatures declare, in order.
#[inline(always)]
pub fn sigNames(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::FnSig>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    s.name.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::sigNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The names the script's module-level bindings declare, in order.
#[inline(always)]
pub fn constNames(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    c.name.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::constNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The names declared by sums, then by records: one namespace of named types.
#[inline(always)]
pub fn typeNames(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Sum>,
    rs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Rec>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            aver_rt::AverList::prepend_owned(
                s.name.clone(),
                crate::proof_kernel::aver_generated::kernel::check::typeNames(&rest, rs),
            )
        } else {
            if __list_subject.is_empty() {
                {
                    let __list_subject = rs;
                    if __list_subject.is_empty() {
                        aver_rt::AverList::empty()
                    } else {
                        if let (Some(r), Some(more)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            aver_rt::AverList::prepend_owned(r.name.clone(), {
                                let __arg6 @ _ = <aver_rt::AverList<
                                    crate::proof_kernel::aver_generated::kernel::proof::Sum,
                                >>::empty();
                                crate::proof_kernel::aver_generated::kernel::check::typeNames(
                                    &__arg6, &more,
                                )
                            })
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

/// No module-level binding depends on itself through any mixture of bindings and definitions.
#[inline(always)]
pub fn declarationsAcyclic(
    todo @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    mut path @ _: AverStr,
) -> Result<(), AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>;
    let mut todo @ _: &aver_rt::AverList<
        crate::proof_kernel::aver_generated::kernel::proof::Const,
    > = todo;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = todo;
            if __list_subject.is_empty() {
                return Ok(());
            } else {
                if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if {
                        let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::check::constantDeclarationDeps(c.name.clone(), cs, ds, cs);
                        {
                            let __arg7 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TVar(
                                        c.name.clone(),
                                    ),
                                );
                            {
                                let __arg8 @ _ = <aver_rt::AverList<
                                    crate::proof_kernel::aver_generated::kernel::term::Term,
                                >>::empty();
                                {
                                    let __arg9 @ _ = aver_rt::AverInt::from_i64(ds.len() as i64)
                                        .add(&aver_rt::AverInt::from_i64(cs.len() as i64));
                                    crate::proof_kernel::aver_generated::kernel::check::declarationCycle(__arg6, &__arg7, ds, cs, __arg8, __arg9)
                                }
                            }
                        }
                    } {
                        return Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(87))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("step "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(path),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(
                                    ": cyclic declaration through module-level binding "
                                ));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c.name))));
                            __b
                        }));
                    } else {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            todo = &__hold0;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Whether following definition and binding references reaches the target declaration; running out of fuel counts as a cycle.
pub fn declarationCycle(
    mut frontier @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    target @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    mut seen @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut fuel @ _: aver_rt::AverInt,
) -> bool {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __subject = (frontier, (fuel > aver_rt::AverInt::from_i64(0)));
            {
                let __int_match_subject = __subject;
                let (__lit0, __lit1) = &__int_match_subject;
                if (*__lit0).is_empty() {
                    return false;
                } else if (*__lit1) == false {
                    return true;
                } else if !(*__lit0).is_empty() && (*__lit1) == true {
                    let Some((n, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                        unreachable!("Aver Rust codegen: tuple element list mismatch")
                    };
                    if (&(n) == target) {
                        return true;
                    } else {
                        if seen.contains(&n) {
                            {
                                let __tco0 = rest;
                                frontier = __tco0;
                                continue;
                            }
                        } else {
                            {
                                let __tco0 = aver_rt::AverList::concat(&rest, &crate::proof_kernel::aver_generated::kernel::check::declarationDeps(&n, ds, cs));
                                let __tco4 = aver_rt::AverList::prepend_owned(n, seen);
                                let __tco5 = fuel.sub(&aver_rt::AverInt::from_i64(1));
                                frontier = __tco0;
                                seen = __tco4;
                                fuel = __tco5;
                                continue;
                            }
                        }
                    }
                } else {
                    unreachable!("Aver Rust codegen: non-exhaustive guard-chain match")
                }
            }
        }
    }
}

/// The declarations directly read by one constant or definition node.
pub fn declarationDeps(
    node @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = node;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TVar(__n_fields) => {
                let n = __n_fields.0.clone();
                crate::proof_kernel::aver_generated::kernel::check::constantDeclarationDeps(
                    n, cs, ds, cs,
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TCall(__f_fields) => {
                let f = __f_fields.0.clone();
                let __pat0 = &__f_fields.1;
                {
                    let __list_subject = __pat0;
                    if __list_subject.is_empty() {
                        crate::proof_kernel::aver_generated::kernel::check::definitionDeclarationDeps(f, ds, ds, cs)
                    } else {
                        aver_rt::AverList::empty()
                    }
                }
            }
            _ => aver_rt::AverList::empty(),
        }
    }
}

/// The declarations read by a named module-level binding.
#[inline(always)]
pub fn constantDeclarationDeps(
    mut name @ _: AverStr,
    todo @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>;
    let mut todo @ _: &aver_rt::AverList<
        crate::proof_kernel::aver_generated::kernel::proof::Const,
    > = todo;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = todo;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (c.name == name) {
                        return {
                            let __arg6 @ _ = <aver_rt::AverList<AverStr>>::empty();
                            crate::proof_kernel::aver_generated::kernel::check::termDeclarationDeps(
                                &c.value, &__arg6, ds, cs,
                            )
                        };
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            todo = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The declarations read by a named definition.
#[inline(always)]
pub fn definitionDeclarationDeps(
    mut name @ _: AverStr,
    todo @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>;
    let mut todo @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def> =
        todo;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = todo;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let (Some(d), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (d.name == name) {
                        return crate::proof_kernel::aver_generated::kernel::check::definitionDeps(
                            d, ds, cs,
                        );
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            todo = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A definition's dependencies, with parameters and sequential local bindings shadowing constants.
#[inline(always)]
pub fn definitionDeps(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::proof::givenNames(&d.params);
    crate::proof_kernel::aver_generated::kernel::check::letDeclarationDeps(
        &d.lets, &d.body, &__arg3, ds, cs,
    )
}

/// Dependencies of local bindings in scope order, then of the body under all their names.
#[inline(always)]
pub fn letDeclarationDeps(
    lets @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    body @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    locals @ _: &aver_rt::AverList<AverStr>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = lets;
        if __list_subject.is_empty() {
            crate::proof_kernel::aver_generated::kernel::check::termDeclarationDeps(
                body, locals, ds, cs,
            )
        } else {
            if let (Some(b), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::check::termDeclarationDeps(
                        &b.value, locals, ds, cs,
                    ),
                    &{
                        let __arg7 @ _ = aver_rt::AverList::prepend(b.name.clone(), locals);
                        crate::proof_kernel::aver_generated::kernel::check::letDeclarationDeps(
                            &rest, body, &__arg7, ds, cs,
                        )
                    },
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Constant references not shadowed in this scope and calls to script definitions.
#[inline(always)]
pub fn termDeclarationDeps(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    locals @ _: &aver_rt::AverList<AverStr>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    aver_rt::AverList::concat(
        &{
            let __arg5 @ _ = {
                let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::subst::freeVars(t);
                crate::proof_kernel::aver_generated::kernel::subst::without(&__arg4, locals.clone())
            };
            crate::proof_kernel::aver_generated::kernel::check::constDependencyNodes(__arg5, cs)
        },
        &{
            let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::induct::namesCalled(t);
            crate::proof_kernel::aver_generated::kernel::check::definitionDependencyNodes(
                &__arg6, ds,
            )
        },
    )
}

/// References whose names denote module-level bindings, as graph nodes.
#[inline(always)]
pub fn constDependencyNodes(
    mut names @ _: aver_rt::AverList<AverStr>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(names, [] => { return aver_rt::AverList::empty(); }, [n, rest] => { if crate::proof_kernel::aver_generated::kernel::check::isConst(n.clone(), cs) { return aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(n)), crate::proof_kernel::aver_generated::kernel::check::constDependencyNodes(rest, cs)); } else { {
            let __tco0 = rest;
            names = __tco0;
            continue;
        } } })
    }
}

/// Calls whose names denote script definitions, as graph nodes.
#[inline(always)]
pub fn definitionDependencyNodes(
    names @ _: &aver_rt::AverList<AverStr>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut names @ _: &aver_rt::AverList<AverStr> = names;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = names;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    match crate::proof_kernel::aver_generated::kernel::proof::findDef(n.clone(), ds)
                    {
                        Some(d @ _) => {
                            return aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::term::Term::TCall(crate::proof_kernel::aver_generated::kernel::term::Term__TCall(n, aver_rt::AverList::empty())), crate::proof_kernel::aver_generated::kernel::check::definitionDependencyNodes(&rest, ds));
                        }
                        None => {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            names = &__hold0;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Every definition in the script is well typed.
#[inline(always)]
pub fn defsTyped(
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    env @ _: &Env,
    mut path @ _: AverStr,
) -> Result<(), AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>;
    let mut ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def> =
        ds;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = ds;
            if __list_subject.is_empty() {
                return Ok(());
            } else {
                if let (Some(d), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::proof_kernel::aver_generated::kernel::check::definitionTyped(
                        d,
                        env,
                        path.clone(),
                    ) {
                        Err(why @ _) => {
                            return Err(why);
                        }
                        Ok(done @ _) => {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            ds = &__hold0;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A definition body has its declared result type under its parameters and local bindings.
pub fn definitionTyped(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::proof::givenNames(&d.params);
        {
            let __arg6 @ _ = <aver_rt::AverList<AverStr>>::empty();
            {
                let __arg7 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(74)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("step "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(": definition "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" has duplicate parameter"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::types::unique(&__arg5, __arg6, __arg7)
            }
        }
    }?;
    let scope @ _ = {
        let __arg8 @ _ =
            crate::proof_kernel::aver_generated::kernel::check::withVars(env, &d.params);
        crate::proof_kernel::aver_generated::kernel::check::letsTyped(&d.lets, __arg8, path.clone())
    }?;
    let __arg9 @ _ =
        crate::proof_kernel::aver_generated::kernel::check::typeOf(&d.body, &scope, path.clone())?;
    let __arg10 @ _ = aver_rt::AverStr::from({
        let mut __b = {
            let mut __b = aver_rt::Buffer::with_capacity(
                (aver_rt::AverInt::from_i64(27)).to_usize().unwrap_or(0),
            );
            __b.push_str(&::aver_rt::str_lit!("definition "));
            __b
        };
        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name))));
        __b
    });
    crate::proof_kernel::aver_generated::kernel::check::declaredFits(&__arg9, &d.ret, __arg10, path)
}

/// Infer local bindings in source order, making each one visible to those after it and to the body.
#[inline(always)]
pub fn letsTyped(
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    mut env @ _: Env,
    mut path @ _: AverStr,
) -> Result<Env, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>;
    let mut bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding> =
        bs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = bs;
            if __list_subject.is_empty() {
                return Ok(env);
            } else {
                if let (Some(b), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = {
                            let __arg6 @ _ = aver_rt::AverList::from_vec(vec![{
                                let __arg5 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::check::typeOf(
                                        &b.value,
                                        &env,
                                        path.clone(),
                                    )?;
                                crate::proof_kernel::aver_generated::kernel::term::typed(
                                    b.name.clone(),
                                    __arg5,
                                )
                            }]);
                            crate::proof_kernel::aver_generated::kernel::check::withVars(
                                &env, &__arg6,
                            )
                        };
                        __hold0 = __tco0;
                        bs = &__hold0;
                        env = __tco1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Every module constant value has its declared type.
#[inline(always)]
pub fn constsTyped(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    env @ _: &Env,
    mut path @ _: AverStr,
) -> Result<(), AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>;
    let mut cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const> =
        cs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = cs;
            if __list_subject.is_empty() {
                return Ok(());
            } else {
                if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __subject = {
                            let __arg7 @ _ =
                                crate::proof_kernel::aver_generated::kernel::check::typeOf(
                                    &c.value,
                                    env,
                                    path.clone(),
                                )?;
                            {
                                let __arg8 @ _ = aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(25))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("constant "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(c.name),
                                    )));
                                    __b
                                });
                                crate::proof_kernel::aver_generated::kernel::check::declaredFits(
                                    &__arg7,
                                    &c.ty,
                                    __arg8,
                                    path.clone(),
                                )
                            }
                        };
                        match __subject {
                            Err(why @ _) => {
                                return Err(why);
                            }
                            Ok(done @ _) => {
                                let __tco0 = rest;
                                __hold0 = __tco0;
                                cs = &__hold0;
                                continue;
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A declaration's inferred type fits the type it announces.
#[inline(always)]
pub fn declaredFits(
    actual @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
    declared @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
    what @ _: AverStr,
    path @ _: AverStr,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if crate::proof_kernel::aver_generated::kernel::types::fits(actual.clone(), declared.clone()) {
        Ok(())
    } else {
        Err(aver_rt::AverStr::from({
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
                                        __b.push_str(&::aver_rt::str_lit!("step "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(path),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(": "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(what))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" has type "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_kernel::aver_generated::kernel::types::show(actual)),
                    )));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(", declared "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_kernel::aver_generated::kernel::types::show(declared)),
            )));
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
    {
        let __subject = {
            let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
            crate::proof_kernel::aver_generated::kernel::types::patVars(p, subject, &__arg6)
        };
        match __subject {
            Ok(vs @ _) => Ok(vs),
            Err(why @ _) => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                __b
            })),
        }
    }
}

/// What a variable of list type holds, if it is one.
#[inline(always)]
pub fn listVar(
    v @ _: AverStr,
    env @ _: &Env,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Ty> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = {
            let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v),
            );
            {
                let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
                crate::proof_kernel::aver_generated::kernel::types::typeOf(&__arg4, &__arg5)
            }
        };
        match __subject {
            Ok(t @ _) => crate::proof_kernel::aver_generated::kernel::types::elemOf(&t),
            Err(why @ _) => None,
        }
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
        {
            let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyList(
                crate::proof_kernel::aver_generated::kernel::term::Ty__TyList(e),
            );
            crate::proof_kernel::aver_generated::kernel::term::typed(t, __arg3)
        },
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
                    __b.push_str(&::aver_rt::str_lit!("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(": "));
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
    match r {
        Ok(e @ _) => false,
        Err(why @ _) => {
            let why = why.clone();
            why.starts_with(&*aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("step "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(": "));
                __b
            }))
        }
    }
}

/// Symmetry.
pub fn flip(
    mut e @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
        lhs: e.rhs,
        rhs: e.lhs,
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
            ::aver_rt::str_lit!("trans needs one more term than steps"),
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
            if let (Some(a), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                ::aver_rt::aver_list_match_reuse!(__pat2, [] => { { let __list_subject = __pat1; if __list_subject.is_empty() { { let __arg14 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: a.clone(), rhs: a.clone() }; { let __arg15 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&::aver_rt::str_lit!("/trans.")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i)))); __b }); crate::proof_kernel::aver_generated::kernel::check::typedEquation(__arg14, env, __arg15) } } } else { crate::proof_kernel::aver_generated::kernel::check::refuse(path, ::aver_rt::str_lit!("trans needs at least one step")) } } }, [b, moreTerms, __list_slot] => { { let __list_subject = __pat1; if let (Some(p), Some(moreSteps)) = (__list_subject.first(), __list_subject.tail()) { { let __arg13 @ _ = __list_slot.prepend(b.clone(), moreTerms); crate::proof_kernel::aver_generated::kernel::check::transLink(a.clone(), b, p.clone(), &__arg13, &moreSteps, env, path, i) } } else { crate::proof_kernel::aver_generated::kernel::check::refuse(path, ::aver_rt::str_lit!("trans needs at least one step")) } } })
            } else {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    ::aver_rt::str_lit!("trans needs at least one step"),
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
            __b.push_str(&::aver_rt::str_lit!("/trans."));
            __b
        };
        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(i))));
        __b
    });
    let e @ _ = {
        let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
            p,
            env.clone(),
            here.clone(),
        )?;
        crate::proof_kernel::aver_generated::kernel::check::typedEquation(
            __arg10,
            env,
            here.clone(),
        )
    }?;
    if (e
        == crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: a.clone(),
            rhs: b,
        })
    {
        {
            let __arg11 @ _ = crate::proof_kernel::aver_generated::kernel::check::transFrom(
                rest,
                steps,
                env,
                path,
                i.add(&aver_rt::AverInt::from_i64(1)),
            )?;
            crate::proof_kernel::aver_generated::kernel::check::joinRest(a, __arg11)
        }
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            here,
            ::aver_rt::str_lit!("the step does not join the written terms"),
        )
    }
}

/// The chain from a to the end of the rest.
pub fn joinRest(
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut rest @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
        lhs: a,
        rhs: rest.rhs,
    })
}

/// From a = b, c[a] = c[b]; the context holds exactly one hole.
#[inline(always)]
pub fn congr(
    mut c @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut q @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (crate::proof_kernel::aver_generated::kernel::subst::holeCount(&c)
        == aver_rt::AverInt::from_i64(1))
    {
        {
            let __arg8 @ _ = {
                let __arg7 @ _ = {
                    let __arg5 @ _ = {
                        let __arg4 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!("/congr"));
                            __b
                        });
                        crate::proof_kernel::aver_generated::kernel::check::conclude(
                            q,
                            env.clone(),
                            __arg4,
                        )
                    }?;
                    {
                        let __arg6 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!("/congr"));
                            __b
                        });
                        crate::proof_kernel::aver_generated::kernel::check::typedEquation(
                            __arg5, env, __arg6,
                        )
                    }
                }?;
                crate::proof_kernel::aver_generated::kernel::check::plugBoth(
                    c,
                    __arg7,
                    path.clone(),
                )
            }?;
            crate::proof_kernel::aver_generated::kernel::check::typedEquation(__arg8, env, path)
        }
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            ::aver_rt::str_lit!("a congruence context needs exactly one hole"),
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
    {
        let __subject = (
            {
                let __arg7 @ _ = aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::term::bind(
                        ::aver_rt::str_lit!("□"),
                        e.lhs,
                    ),
                ]);
                crate::proof_kernel::aver_generated::kernel::subst::subst(c.clone(), &__arg7)
            },
            {
                let __arg8 @ _ = aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::term::bind(
                        ::aver_rt::str_lit!("□"),
                        e.rhs,
                    ),
                ]);
                crate::proof_kernel::aver_generated::kernel::subst::subst(c, &__arg8)
            },
        );
        match __subject {
            (Ok(l), Ok(r)) => {
                Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r })
            }
            (Err(why), _) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
            (_, Err(why)) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
        }
    }
}

/// A record literal's field; a field of an update, which is the value written to it, or the base's field when the update does not write it.
pub fn proj(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = &t;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TGet(
                ____pat0_fields,
            ) => {
                let __pat0 = ____pat0_fields.0.clone();
                let f = ____pat0_fields.1.clone();
                {
                    let __t = __pat0;
                    match __t.view() {
                        crate::proof_kernel::aver_generated::kernel::term::Term__view::TRec(
                            __ty_fields,
                        ) => {
                            let ty = __ty_fields.0.clone();
                            let fs = &__ty_fields.1;
                            crate::proof_kernel::aver_generated::kernel::check::projField(
                                t, fs, f, path,
                            )
                        }
                        crate::proof_kernel::aver_generated::kernel::term::Term__view::TUpd(
                            __ty_fields,
                        ) => {
                            let ty = __ty_fields.0.clone();
                            let base = &__ty_fields.1;
                            let fs = &__ty_fields.2;
                            {
                                let __arg10 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::check::typeOf(
                                        &t, env, path,
                                    );
                                crate::proof_kernel::aver_generated::kernel::check::projUpdate(
                                    t,
                                    &__arg10,
                                    base.clone(),
                                    fs,
                                    f,
                                )
                            }
                        }
                        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                            path,
                            ::aver_rt::str_lit!(
                                "a projection must read a field of a record literal or of an update"
                            ),
                        ),
                    }
                }
            }
            _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                ::aver_rt::str_lit!(
                    "a projection must read a field of a record literal or of an update"
                ),
            ),
        }
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
    match crate::proof_kernel::aver_generated::kernel::check::findField(fs, f.clone()) {
        Some(x @ _) => {
            Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: t, rhs: x })
        }
        None => Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: t,
            rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TGet(
                crate::proof_kernel::aver_generated::kernel::term::Term__TGet(base, f),
            ),
        }),
    }
}

/// The value the first field named f holds.
#[inline(always)]
pub fn findField(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
    mut f @ _: AverStr,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Term> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>;
    let mut fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field> =
        fs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = fs;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (x.name == f) {
                        return Some(x.value.clone());
                    } else {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            fs = &__hold0;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A list literal with an element is that element in front of the rest.
pub fn cell(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = &t;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TList(
                ____pat0_fields,
            ) => {
                let __pat0 = ____pat0_fields.0.clone();
                {
                    let __list_subject = __pat0;
                    if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                        Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: t, rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.prepend"), aver_rt::AverList::from_vec(vec![x.clone(), crate::proof_kernel::aver_generated::kernel::term::Term::TList(crate::proof_kernel::aver_generated::kernel::term::Term__TList(rest))]))) })
                    } else {
                        crate::proof_kernel::aver_generated::kernel::check::refuse(
                            path,
                            ::aver_rt::str_lit!("a cell step needs a list literal with an element"),
                        )
                    }
                }
            }
            _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                ::aver_rt::str_lit!("a cell step needs a list literal with an element"),
            ),
        }
    }
}

/// Find the field among the literal's fields.
#[inline(always)]
pub fn projField(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
    mut f @ _: AverStr,
    mut path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>;
    let mut fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field> =
        fs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = fs;
            if __list_subject.is_empty() {
                return {
                    let __arg6 @ _ = aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("the record has no field "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                        __b
                    });
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg6)
                };
            } else {
                if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (x.name == f) {
                        return Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                            lhs: t,
                            rhs: x.value.clone(),
                        });
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            fs = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A hypothesis in scope; the innermost one of a name wins.
#[inline(always)]
pub fn hyp(
    mut h @ _: AverStr,
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    mut path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>;
    let mut hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> =
        hs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = hs;
            if __list_subject.is_empty() {
                return {
                    let __arg5 @ _ = aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("hypothesis "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" is not in scope"));
                        __b
                    });
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg5)
                };
            } else {
                if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (x.name == h) {
                        return Ok(x.eqn.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            hs = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Two closed terms with the same value.
pub fn compute(
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut b @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = (
            crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a),
            crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&b),
        );
        match __subject {
            (Some(x), Some(y)) => {
                if (x == y) {
                    Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: a, rhs: b })
                } else {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        ::aver_rt::str_lit!("the two sides compute to different values"),
                    )
                }
            }
            _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                ::aver_rt::str_lit!("compute needs closed terms"),
            ),
        }
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
        Ok(__pat0) => {
            let __t = __pat0;
            match __t.view() {
                crate::proof_kernel::aver_generated::kernel::term::Ty__view::TyBool => {
                    crate::proof_kernel::aver_generated::kernel::check::casesOfBool(
                        on, h, t, f, env, path,
                    )
                }
                _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    ::aver_rt::str_lit!("the term split on is not a Bool"),
                ),
            }
        }
        _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            ::aver_rt::str_lit!("the term split on is not a Bool"),
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
    let whenTrue @ _ = {
        let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::check::withHyp(
            env,
            h.clone(),
            on.clone(),
            true,
        );
        {
            let __arg9 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(27)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!("/cases.true"));
                __b
            });
            crate::proof_kernel::aver_generated::kernel::check::conclude(t, __arg8, __arg9)
        }
    }?;
    let whenFalse @ _ = {
        let __arg10 @ _ =
            crate::proof_kernel::aver_generated::kernel::check::withHyp(env, h, on, false);
        {
            let __arg11 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(28)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!("/cases.false"));
                __b
            });
            crate::proof_kernel::aver_generated::kernel::check::conclude(f, __arg10, __arg11)
        }
    }?;
    if (whenTrue == whenFalse) {
        Ok(whenTrue)
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            ::aver_rt::str_lit!("the two branches prove different equations"),
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
        let __subject = (
            crate::proof_kernel::aver_generated::kernel::check::typeOf(on, env, path.clone()),
            lits.clone(),
        );
        {
            let __int_match_subject = __subject;
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
        {
            let __arg8 @ _ = {
                let __arg7 @ _ = <aver_rt::AverList<
                    crate::proof_kernel::aver_generated::kernel::term::Term,
                >>::empty();
                crate::proof_kernel::aver_generated::kernel::check::litHyps(
                    lits, cs, on, h, &__arg7,
                )
            };
            {
                let __arg9 @ _ = crate::proof_kernel::aver_generated::kernel::check::noVars(cs);
                {
                    let __arg10 @ _ = <aver_rt::AverList<
                        crate::proof_kernel::aver_generated::kernel::term::Eqn,
                    >>::empty();
                    crate::proof_kernel::aver_generated::kernel::check::splitCases(
                        cs.clone(),
                        __arg8,
                        __arg9,
                        on,
                        __arg10,
                        env,
                        path,
                    )
                }
            }
        }
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            ::aver_rt::str_lit!("the cases are not the literals and one more"),
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
    {
        let __list_subject = lits;
        if __list_subject.is_empty() {
            true
        } else {
            if let (Some(k), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                ((crate::proof_kernel::aver_generated::kernel::check::isLiteral(k)
                    && (crate::proof_kernel::aver_generated::kernel::check::typeOf(
                        k,
                        env,
                        path.clone(),
                    ) == Ok(t.clone())))
                    && crate::proof_kernel::aver_generated::kernel::check::literalsOf(
                        &rest, t, env, path,
                    ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
    {
        let __subject = (
            ({
                let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
                crate::proof_kernel::aver_generated::kernel::types::ctorsOf(t, &__arg8)
            } == crate::proof_kernel::aver_generated::kernel::check::splitCtors(cs)),
            crate::proof_kernel::aver_generated::kernel::check::caseVars(cs, t, env),
        );
        match __subject {
            (false, _) => {
                let __arg9 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(54)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!(
                            "the cases are not the constructors of "
                        ));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_kernel::aver_generated::kernel::types::show(t)),
                    )));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg9)
            }
            (_, Err(why)) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
            (true, Ok(vss)) => {
                let __arg10 @ _ =
                    crate::proof_kernel::aver_generated::kernel::check::ctorHyps(cs, on, h);
                {
                    let __arg11 @ _ = <aver_rt::AverList<
                        crate::proof_kernel::aver_generated::kernel::term::Eqn,
                    >>::empty();
                    crate::proof_kernel::aver_generated::kernel::check::splitCases(
                        cs.clone(),
                        __arg10,
                        vss,
                        on,
                        __arg11,
                        env,
                        path,
                    )
                }
            }
        }
    }
}

/// A given x of a declared sum C with bare constructors C.A and C.B.
pub fn sumEnv() -> Env {
    crate::proof_kernel::cancel_checkpoint();
    Box::new(
        crate::proof_kernel::aver_generated::kernel::check::Env__Fields {
            defs: aver_rt::AverList::empty(),
            consts: aver_rt::AverList::empty(),
            laws: aver_rt::AverList::empty(),
            hyps: aver_rt::AverList::empty(),
            vars: aver_rt::AverList::from_vec(vec![{
                let __arg0 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyNamed(
                    crate::proof_kernel::aver_generated::kernel::term::Ty__TyNamed(
                        ::aver_rt::str_lit!("C"),
                    ),
                );
                crate::proof_kernel::aver_generated::kernel::term::typed(
                    ::aver_rt::str_lit!("x"),
                    __arg0,
                )
            }]),
            sums: aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::proof::Sum {
                    name: ::aver_rt::str_lit!("C"),
                    eq: false,
                    variants: aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::proof::Variant {
                            name: ::aver_rt::str_lit!("C.A"),
                            fields: aver_rt::AverList::empty(),
                        },
                        crate::proof_kernel::aver_generated::kernel::proof::Variant {
                            name: ::aver_rt::str_lit!("C.B"),
                            fields: aver_rt::AverList::empty(),
                        },
                    ]),
                },
            ]),
            recs: aver_rt::AverList::empty(),
            sigs: aver_rt::AverList::empty(),
        },
    )
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
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let __subject = {
                        let __arg7 @ _ =
                            crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
                        crate::proof_kernel::aver_generated::kernel::types::ctorFields(
                            c.ctor.clone(),
                            t,
                            &__arg7,
                        )
                    };
                    match __subject {
                        Err(why @ _) => Err(why),
                        Ok(fs @ _) => {
                            if (aver_rt::AverInt::from_i64(fs.len() as i64)
                                == aver_rt::AverInt::from_i64(c.binders.len() as i64))
                            {
                                Ok(aver_rt::AverList::prepend_owned(
                                    crate::proof_kernel::aver_generated::kernel::types::namedVars(
                                        &c.binders, &fs,
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::check::caseVars(
                                        &rest, t, env,
                                    )?,
                                ))
                            } else {
                                Err(aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b = {
                                                            let mut __b =
                                                                aver_rt::Buffer::with_capacity(
                                                                    (aver_rt::AverInt::from_i64(
                                                                        78,
                                                                    ))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                                );
                                                            __b.push_str(&::aver_rt::str_lit!(
                                                                "case "
                                                            ));
                                                            __b
                                                        };
                                                        __b.push_str(&aver_rt::AverStr::from(
                                                            aver_rt::aver_display(&(c.ctor)),
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_str(&::aver_rt::str_lit!(" binds "));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(
                                                        &(aver_rt::AverInt::from_i64(
                                                            c.binders.len() as i64,
                                                        )),
                                                    ),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" names for "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(
                                                &(aver_rt::AverInt::from_i64(fs.len() as i64)),
                                            ),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" fields"));
                                    __b
                                }))
                            }
                        }
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// No variables for any case: a literal split names hypotheses, not values.
#[inline(always)]
pub fn noVars(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>>
{
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let Some((c, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverList::prepend_owned(
                    {
                        let __arg3 @ _ = <aver_rt::AverList<
                            aver_rt::AverList<
                                crate::proof_kernel::aver_generated::kernel::term::TypedVar,
                            >,
                        >>::empty();
                        crate::proof_kernel::aver_generated::kernel::check::firstVars(&__arg3)
                    },
                    crate::proof_kernel::aver_generated::kernel::check::noVars(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The constructor of each case.
#[inline(always)]
pub fn splitCtors(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    c.ctor.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::splitCtors(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// How many names each case binds.
#[inline(always)]
pub fn binderCounts(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
) -> aver_rt::AverIntList {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            aver_rt::AverIntList::empty()
        } else {
            if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverIntList::prepend_owned(
                    aver_rt::AverInt::from_i64(c.binders.len() as i64),
                    crate::proof_kernel::aver_generated::kernel::check::binderCounts(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
                    crate::proof_kernel::aver_generated::kernel::term::Term__TList(
                        aver_rt::AverList::empty(),
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
                                            crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.prepend"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(a)), crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(b))])))
                                        } else {
                                            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(c.ctor.clone(), crate::proof_kernel::aver_generated::kernel::check::namesToVars(&c.binders)))
                                        }
                                    }
                                } else {
                                    crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(c.ctor.clone(), crate::proof_kernel::aver_generated::kernel::check::namesToVars(&c.binders)))
                                }
                            }
                        } else {
                            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                                    c.ctor.clone(),
                                    crate::proof_kernel::aver_generated::kernel::check::namesToVars(
                                        &c.binders,
                                    ),
                                ),
                            )
                        }
                    }
                } else {
                    crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                            c.ctor.clone(),
                            crate::proof_kernel::aver_generated::kernel::check::namesToVars(
                                &c.binders,
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
    {
        let __list_subject = lits;
        if __list_subject.is_empty() {
            aver_rt::AverList::from_vec(vec![::aver_rt::str_lit!("else")])
        } else {
            if let Some((k, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverList::prepend_owned(
                    ::aver_rt::str_lit!("lit"),
                    crate::proof_kernel::aver_generated::kernel::check::litCtors(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// How many names each case of a literal split binds: none for a literal, one per literal for the last one.
#[inline(always)]
pub fn litCounts(
    lits @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    n @ _: aver_rt::AverInt,
) -> aver_rt::AverIntList {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = lits;
        if __list_subject.is_empty() {
            aver_rt::AverIntList::from_vec(vec![n])
        } else {
            if let Some((k, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverIntList::prepend_owned(
                    aver_rt::AverInt::from_i64(0),
                    crate::proof_kernel::aver_generated::kernel::check::litCounts(
                        &rest,
                        n.add(&aver_rt::AverInt::from_i64(1)),
                    ),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// For each case of a split on a constructor, h : on = the constructor at the case's names.
#[inline(always)]
pub fn ctorHyps(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>,
    on @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    h @ _: AverStr,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                            name: h.clone(),
                            eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
                                lhs: on.clone(),
                                rhs: crate::proof_kernel::aver_generated::kernel::check::splitValue(
                                    c,
                                ),
                            },
                        },
                    ]),
                    crate::proof_kernel::aver_generated::kernel::check::ctorHyps(&rest, on, h),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
            aver_rt::AverList::prepend_owned(
                aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                        name: h.clone(),
                        eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
                            lhs: on.clone(),
                            rhs: k.clone(),
                        },
                    },
                ]),
                {
                    let __arg11 @ _ = aver_rt::AverList::concat(
                        &seen.clone(),
                        &aver_rt::AverList::from_vec(vec![k]),
                    );
                    crate::proof_kernel::aver_generated::kernel::check::litHyps(
                        &ks, &more, on, h, &__arg11,
                    )
                },
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
            aver_rt::AverList::prepend_owned(
                crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                    name: n,
                    eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                                ::aver_rt::str_lit!("=="),
                                on.clone(),
                                k,
                            ),
                        ),
                        rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false),
                        ),
                    },
                },
                crate::proof_kernel::aver_generated::kernel::check::unequal(&ns, &ks, on),
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
    {
        let __list_subject = vss;
        if let (Some(vs), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            vs.clone()
        } else {
            if __list_subject.is_empty() {
                aver_rt::AverList::empty()
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The hypotheses of the first case.
#[inline(always)]
pub fn firstHyps(
    hss @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    >,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = hss;
        if let (Some(hs), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            hs.clone()
        } else {
            if __list_subject.is_empty() {
                aver_rt::AverList::empty()
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The equation every case proved.
#[inline(always)]
pub fn splitDone(
    before @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = before;
        if let (Some(e), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            Ok(e.clone())
        } else {
            if __list_subject.is_empty() {
                crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    ::aver_rt::str_lit!("a split with no cases"),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The environment with one more hypothesis in front.
pub fn withHyp(
    env @ _: &Env,
    h @ _: AverStr,
    mut on @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    value @ _: bool,
) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    Box::new(
        crate::proof_kernel::aver_generated::kernel::check::Env__Fields {
            defs: env.defs.clone(),
            consts: env.consts.clone(),
            laws: env.laws.clone(),
            hyps: aver_rt::AverList::prepend(
                crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                    name: h,
                    eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: on,
                        rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TBool(value),
                        ),
                    },
                },
                &env.hyps,
            ),
            vars: env.vars.clone(),
            sums: env.sums.clone(),
            recs: env.recs.clone(),
            sigs: env.sigs.clone(),
        },
    )
}

/// A comparison decided by linear arithmetic over the integers: its opposite and the named hypotheses, then the rows the cuts round from them, weighted, add up to a negative constant. Every row compares two Int terms.
pub fn linear(
    mut g @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    v @ _: bool,
    hs @ _: &aver_rt::AverList<AverStr>,
    ws @ _: &aver_rt::AverIntList,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Cut>,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let facts @ _ =
        crate::proof_kernel::aver_generated::kernel::check::hypEqns(hs, &env.hyps, path.clone())?;
    {
        let __arg9 @ _ = aver_rt::AverList::prepend_owned(
            g.clone(),
            crate::proof_kernel::aver_generated::kernel::check::lhsOf(&facts),
        );
        crate::proof_kernel::aver_generated::kernel::check::intComparisons(
            __arg9,
            env,
            path.clone(),
        )
    }?;
    crate::proof_kernel::aver_generated::kernel::check::linearOver(g, v, &facts, ws, cs, path)
}

/// Each term compares two Int terms: what linear arithmetic may read as a row.
#[inline(always)]
pub fn intComparisons(
    mut ts @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    env @ _: &Env,
    mut path @ _: AverStr,
) -> Result<(), AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(ts, [] => { return Ok(()); }, [__pat0, rest] => { { let __t = __pat0; match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
            let o = __o_fields.0.clone();
            let a = &__o_fields.1;
            let b = &__o_fields.2;
            { let __subject = (crate::proof_kernel::aver_generated::kernel::check::typeOf(a, env, path.clone())?, crate::proof_kernel::aver_generated::kernel::check::typeOf(b, env, path.clone())?); { let __t = __subject; match (__t.0.view(), __t.1.view(),) {
        (crate::proof_kernel::aver_generated::kernel::term::Ty__view::TyInt, crate::proof_kernel::aver_generated::kernel::term::Ty__view::TyInt) => {
            {
            let __tco0 = rest;
            ts = __tco0;
            continue;
        }
        },
        _ => {
            return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(71)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&::aver_rt::str_lit!(": linear arithmetic reads comparisons of Int terms")); __b }));
        }
    } } }
        },
        _ => {
            return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(71)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&::aver_rt::str_lit!(": linear arithmetic reads comparisons of Int terms")); __b }));
        }
    } } })
    }
}

/// The left side of each equation.
#[inline(always)]
pub fn lhsOf(
    es @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Eqn>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = es;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(e), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    e.lhs.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::lhsOf(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
    if {
        let __arg6 @ _ = aver_rt::AverList::prepend(
            crate::proof_kernel::aver_generated::kernel::term::Eqn {
                lhs: g.clone(),
                rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TBool((!v)),
                ),
            },
            facts,
        );
        crate::proof_kernel::aver_generated::kernel::ring::linearContradiction(&__arg6, cs, ws)
    } {
        Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: g,
            rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(v),
            ),
        })
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            ::aver_rt::str_lit!("the weights do not add up to a contradiction"),
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
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let Some((h, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::check::hyp(
                        h,
                        scope,
                        path.clone(),
                    )?,
                    crate::proof_kernel::aver_generated::kernel::check::hypEqns(
                        &rest, scope, path,
                    )?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Two Int terms that are the same polynomial over their atoms.
pub fn ring(
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = (
            crate::proof_kernel::aver_generated::kernel::check::typeOf(&l, env, path.clone())?,
            crate::proof_kernel::aver_generated::kernel::check::typeOf(&r, env, path.clone())?,
            crate::proof_kernel::aver_generated::kernel::ring::samePolynomial(l.clone(), r.clone()),
        );
        {
            let __t = __subject;
            match (__t.0.view(), __t.1.view(), &__t.2) {
                (
                    crate::proof_kernel::aver_generated::kernel::term::Ty__view::TyInt,
                    crate::proof_kernel::aver_generated::kernel::term::Ty__view::TyInt,
                    true,
                ) => Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r }),
                (
                    crate::proof_kernel::aver_generated::kernel::term::Ty__view::TyInt,
                    crate::proof_kernel::aver_generated::kernel::term::Ty__view::TyInt,
                    false,
                ) => crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    ::aver_rt::str_lit!("the two sides are different polynomials"),
                ),
                _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                    path,
                    ::aver_rt::str_lit!("ring needs two Int terms"),
                ),
            }
        }
    }
}

/// Any equation, once true has been shown equal to false: the case cannot happen.
pub fn absurd(
    mut e @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = (e.lhs, e.rhs);
        match (__t.0.view(), __t.1.view()) {
            (
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TBool(__a_fields),
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TBool(__b_fields),
            ) => {
                let a = __a_fields.0.clone();
                let b = __b_fields.0.clone();
                if (a == b) {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        ::aver_rt::str_lit!("absurd needs true equal to false"),
                    )
                } else {
                    Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r })
                }
            }
            _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                ::aver_rt::str_lit!("absurd needs true equal to false"),
            ),
        }
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
        None => {
            let __arg7 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(46)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" is not a given of finite type"));
                __b
            });
            crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg7)
        }
        Some(f @ _) => {
            let __arg8 @ _ =
                crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r };
            {
                let __arg9 @ _ = crate::proof_kernel::aver_generated::kernel::finite::values(&f);
                crate::proof_kernel::aver_generated::kernel::check::enumCases(
                    v,
                    &__arg8,
                    __arg9,
                    cs.clone(),
                    env,
                    path,
                    aver_rt::AverInt::from_i64(0),
                )
            }
        }
    }
}

/// The finite type of a variable, if its type is one.
#[inline(always)]
pub fn finOfVar(
    v @ _: AverStr,
    env @ _: &Env,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Fin> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = {
            let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v),
            );
            {
                let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
                crate::proof_kernel::aver_generated::kernel::types::typeOf(&__arg4, &__arg5)
            }
        };
        match __subject {
            Ok(t @ _) => {
                let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
                {
                    let __arg7 @ _ = <aver_rt::AverList<AverStr>>::empty();
                    crate::proof_kernel::aver_generated::kernel::types::finOf(&t, &__arg6, &__arg7)
                }
            }
            Err(why @ _) => None,
        }
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
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                        name: h.name.clone(),
                        eqn: crate::proof_kernel::aver_generated::kernel::check::instantiate(
                            h.eqn.clone(),
                            at,
                            path.clone(),
                        )?,
                    },
                    crate::proof_kernel::aver_generated::kernel::check::substHyps(&rest, at, path)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The environment with its hypotheses replaced.
pub fn withHyps(
    env @ _: &Env,
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    Box::new(
        crate::proof_kernel::aver_generated::kernel::check::Env__Fields {
            defs: env.defs.clone(),
            consts: env.consts.clone(),
            laws: env.laws.clone(),
            hyps: hs.clone(),
            vars: env.vars.clone(),
            sums: env.sums.clone(),
            recs: env.recs.clone(),
            sigs: env.sigs.clone(),
        },
    )
}

/// A module-level binding's value, unless a variable in the current proof scope shadows its name.
#[inline(always)]
pub fn constant(
    n @ _: AverStr,
    env @ _: &Env,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if crate::proof_kernel::aver_generated::kernel::proof::givenNames(&env.vars).contains(&n) {
        {
            let __arg3 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(63)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("module-level binding "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" is shadowed by a variable"));
                __b
            });
            crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg3)
        }
    } else {
        crate::proof_kernel::aver_generated::kernel::check::constantIn(n, &env.consts, path)
    }
}

/// A module-level binding's declared value.
#[inline(always)]
pub fn constantIn(
    mut n @ _: AverStr,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    mut path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>;
    let mut cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const> =
        cs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = cs;
            if __list_subject.is_empty() {
                return {
                    let __arg5 @ _ = aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("no module-level binding "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    });
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg5)
                };
            } else {
                if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (c.name == n) {
                        return Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
                            lhs: crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TVar(n),
                            ),
                            rhs: c.value.clone(),
                        });
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            cs = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
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
            aver_rt::AverList::prepend_owned(
                crate::proof_kernel::aver_generated::kernel::term::bind(n, v),
                crate::proof_kernel::aver_generated::kernel::check::zipBind(&ns, &vs),
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
    match crate::proof_kernel::aver_generated::kernel::proof::findDef(f.clone(), &env.defs) {
        None => {
            let __arg10 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("no definition of "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                __b
            });
            crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg10)
        }
        Some(d @ _) => {
            let __subject = (
                crate::proof_kernel::aver_generated::kernel::check::argsFit(
                    &d,
                    xs,
                    &env,
                    path.clone(),
                ),
                crate::proof_kernel::aver_generated::kernel::induct::openGate(&d, &env.defs),
            );
            match __subject {
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
    let outer @ _ = {
        let __arg10 @ _ = {
            let __arg9 @ _ = crate::proof_kernel::aver_generated::kernel::types::paramsOf(&d);
            crate::proof_kernel::aver_generated::kernel::check::zipBind(&__arg9, xs)
        }
        .reverse();
        crate::proof_kernel::aver_generated::kernel::check::bindLets(&d.lets, __arg10)
    }?;
    let lhs @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TCall(
        crate::proof_kernel::aver_generated::kernel::term::Term__TCall(d.name, xs.clone()),
    );
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
                ::aver_rt::str_lit!("equation 0 takes no binders and no premise"),
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
    lets @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    mut outer @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>, AverStr>
{
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>;
    let mut lets @ _: &aver_rt::AverList<
        crate::proof_kernel::aver_generated::kernel::term::Binding,
    > = lets;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = lets;
            if __list_subject.is_empty() {
                return Ok(outer);
            } else {
                if let (Some(b), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::AverList::prepend_owned(
                            {
                                let __arg4 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::subst::subst(
                                        b.value.clone(),
                                        &outer,
                                    )?;
                                crate::proof_kernel::aver_generated::kernel::term::bind(
                                    b.name.clone(),
                                    __arg4,
                                )
                            },
                            outer,
                        );
                        __hold0 = __tco0;
                        lets = &__hold0;
                        outer = __tco1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
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
    {
        let __t = body;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TMatch(__s_fields) => {
                let s = &__s_fields.0;
                let arms = &__s_fields.1;
                crate::proof_kernel::aver_generated::kernel::check::selected(
                    s.clone(),
                    arms,
                    k,
                    outer,
                    ys,
                    pre,
                    lhs,
                    env,
                    path,
                )
            }
            _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                ::aver_rt::str_lit!("the body is not a match"),
            ),
        }
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
    {
        let __t = &t;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TMatch(__s_fields) => {
                let s = __s_fields.0.clone();
                let arms = __s_fields.1.clone();
                {
                    let __arg8 @ _ = <aver_rt::AverList<
                        crate::proof_kernel::aver_generated::kernel::term::Binding,
                    >>::empty();
                    {
                        let __arg9 @ _ = aver_rt::AverList::from_vec(vec![q]);
                        crate::proof_kernel::aver_generated::kernel::check::selected(
                            s, &arms, k, &__arg8, ys, &__arg9, t, env, path,
                        )
                    }
                }
            }
            _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                ::aver_rt::str_lit!("not a match"),
            ),
        }
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
            None => {
                let __arg13 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(32)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("there is no arm "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg13)
            }
            _ => {
                let __list_subject = __pat1;
                if let (Some(q), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = __pat2;
                        if __list_subject.is_empty() {
                            crate::proof_kernel::aver_generated::kernel::check::selectedWith(
                                s,
                                arms,
                                k,
                                outer,
                                ys,
                                q.clone(),
                                lhs,
                                env,
                                path,
                            )
                        } else {
                            crate::proof_kernel::aver_generated::kernel::check::refuse(
                                path,
                                ::aver_rt::str_lit!("an arm needs exactly one premise"),
                            )
                        }
                    }
                } else {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        ::aver_rt::str_lit!("an arm needs exactly one premise"),
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
                if let (Some(v), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = __pat2;
                        if __list_subject.is_empty() {
                            crate::proof_kernel::aver_generated::kernel::check::catchAllArm(
                                s,
                                arms,
                                k,
                                chosen,
                                outer,
                                v.clone(),
                                q,
                                lhs,
                                env,
                                path,
                            )
                        } else {
                            crate::proof_kernel::aver_generated::kernel::check::refuse(
                                path,
                                ::aver_rt::str_lit!(
                                    "a catch-all arm takes the value it is chosen for"
                                ),
                            )
                        }
                    }
                } else {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        ::aver_rt::str_lit!("a catch-all arm takes the value it is chosen for"),
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
                {
                    let __arg14 @ _ = aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(50)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!(
                                "an earlier arm can also match arm "
                            ));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
                        __b
                    });
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg14)
                }
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
    {
        let __arg16 @ _ = {
            let __arg12 @ _ =
                crate::proof_kernel::aver_generated::kernel::subst::patNames(&chosen.pattern);
            {
                let __arg13 @ _ = aver_rt::AverList::from_vec(vec![v.clone()]);
                crate::proof_kernel::aver_generated::kernel::check::zipBind(&__arg12, &__arg13)
            }
        };
        {
            let __arg17 @ _ = {
                let __arg14 @ _ =
                    crate::proof_kernel::aver_generated::kernel::subst::patNames(&chosen.pattern);
                {
                    let __arg15 @ _ = aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::check::typeOf(
                            &subject,
                            &env,
                            path.clone(),
                        )?,
                    ]);
                    crate::proof_kernel::aver_generated::kernel::types::namedVars(
                        &__arg14, &__arg15,
                    )
                }
            };
            {
                let __arg18 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("arm "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::bound(
                    &__arg16,
                    &__arg17,
                    &env,
                    __arg18,
                    path.clone(),
                )
            }
        }
    }?;
    if crate::proof_kernel::aver_generated::kernel::check::earlierExcludeValue(
        arms,
        k.clone(),
        &v,
        &env.hyps,
    ) {
        {
            let __arg22 @ _ = aver_rt::AverList::concat(
                &{
                    let __arg19 @ _ = crate::proof_kernel::aver_generated::kernel::subst::patNames(
                        &chosen.pattern,
                    );
                    {
                        let __arg20 @ _ = aver_rt::AverList::from_vec(vec![v.clone()]);
                        crate::proof_kernel::aver_generated::kernel::check::zipBind(
                            &__arg19, &__arg20,
                        )
                    }
                },
                &outer.clone(),
            );
            {
                let __arg23 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn {
                    lhs: subject,
                    rhs: v,
                };
                {
                    let __arg24 @ _ = {
                        let __arg21 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!("/premise"));
                            __b
                        });
                        crate::proof_kernel::aver_generated::kernel::check::conclude(
                            q, env, __arg21,
                        )
                    }?;
                    crate::proof_kernel::aver_generated::kernel::check::catchAllBody(
                        chosen, &__arg22, &__arg23, &__arg24, lhs, path,
                    )
                }
            }
        }
    } else {
        {
            let __arg25 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(50)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("an earlier arm can also match arm "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
                __b
            });
            crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg25)
        }
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
            ::aver_rt::str_lit!("the premise does not select this arm"),
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
        let __subject = (arms.clone(), (k > aver_rt::AverInt::from_i64(1)));
        {
            let __int_match_subject = __subject;
            let (__lit0, __lit1) = &__int_match_subject;
            if !(*__lit0).is_empty() && (*__lit1) == true {
                let Some((a, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                ((crate::proof_kernel::aver_generated::kernel::check::excludes(
                    a.pattern.clone(),
                    v,
                ) || crate::proof_kernel::aver_generated::kernel::check::ruledOut(
                    &a.pattern,
                    v.clone(),
                    hs,
                )) && crate::proof_kernel::aver_generated::kernel::check::earlierExcludeValue(
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
            (crate::proof_kernel::aver_generated::kernel::check::isLiteral(k) && {
                let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn {
                    lhs: crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                            ::aver_rt::str_lit!("=="),
                            v,
                            k.clone(),
                        ),
                    ),
                    rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false),
                    ),
                };
                crate::proof_kernel::aver_generated::kernel::check::stated(&__arg4, hs)
            })
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
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            false
        } else {
            if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                ((&(h.eqn) == e)
                    || crate::proof_kernel::aver_generated::kernel::check::stated(e, &rest))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, xs) => {
                let __t = __pat1;
                match __t.view() {
                    crate::proof_kernel::aver_generated::kernel::term::Term__view::TCtor(
                        __d_fields,
                    ) => {
                        let d = __d_fields.0.clone();
                        let ys = __d_fields.1.clone();
                        (c != d)
                    }
                    _ => false,
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Pat::PNil => {
                let __t = __pat1;
                match __t.view() {
                    crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(
                        ____pat2_fields,
                    ) => {
                        let __pat2 = ____pat2_fields.0.clone();
                        let __pat3 = &____pat2_fields.1;
                        match &*__pat2 {
                            "List.prepend" => {
                                let __list_subject = __pat3;
                                if let Some((h, __pat4)) =
                                    aver_rt::list_uncons_cloned(__list_subject)
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
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
                let __t = __pat1;
                match __t.view() {
                    crate::proof_kernel::aver_generated::kernel::term::Term__view::TList(
                        ____pat6_fields,
                    ) => {
                        let __pat6 = &____pat6_fields.0;
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
                }
            }
            _ => false,
        }
    }
}

/// An Int, Bool, text or unit literal.
pub fn isLiteral(t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TInt(__n_fields) => {
                let n = __n_fields.0.clone();
                true
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TBool(__b_fields) => {
                let b = __b_fields.0.clone();
                true
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TStr(__s_fields) => {
                let s = __s_fields.0.clone();
                true
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TUnit => true,
            _ => false,
        }
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
    let parts @ _ = {
        let __arg16 @ _ = {
            let __arg15 @ _ =
                crate::proof_kernel::aver_generated::kernel::check::patFields(&chosen.pattern);
            crate::proof_kernel::aver_generated::kernel::check::zipBind(&__arg15, ys)
        };
        {
            let __arg17 @ _ = aver_rt::AverList::from_vec(vec![::aver_rt::str_lit!("_")]);
            crate::proof_kernel::aver_generated::kernel::check::withoutNames(__arg16, __arg17)
        }
    };
    {
        let __arg19 @ _ = {
            let __arg18 @ _ = crate::proof_kernel::aver_generated::kernel::check::typeOf(
                &subject,
                &env,
                path.clone(),
            )?;
            crate::proof_kernel::aver_generated::kernel::check::patTypes(
                chosen.pattern,
                __arg18,
                &env,
                path.clone(),
            )
        }?;
        crate::proof_kernel::aver_generated::kernel::check::bound(
            &parts,
            &__arg19,
            &env,
            ::aver_rt::str_lit!("the arm"),
            path.clone(),
        )
    }?;
    let inner @ _ = aver_rt::AverList::concat(&parts, &outer.clone());
    let needed @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn {
        lhs: subject,
        rhs: shape,
    };
    let got @ _ = {
        let __arg20 @ _ = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!("/premise"));
            __b
        });
        crate::proof_kernel::aver_generated::kernel::check::conclude(q, env, __arg20)
    }?;
    if (got == needed) {
        Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: lhs,
            rhs: crate::proof_kernel::aver_generated::kernel::subst::subst(chosen.body, &inner)?,
        })
    } else {
        crate::proof_kernel::aver_generated::kernel::check::refuse(
            path,
            ::aver_rt::str_lit!("the premise does not select this arm"),
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
        let __subject = (arms.clone(), (k > aver_rt::AverInt::from_i64(1)));
        {
            let __int_match_subject = __subject;
            let (__lit0, __lit1) = &__int_match_subject;
            if !(*__lit0).is_empty() && (*__lit1) == true {
                let Some((a, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                (crate::proof_kernel::aver_generated::kernel::check::headsDiffer(
                    a.pattern,
                    p.clone(),
                ) && crate::proof_kernel::aver_generated::kernel::check::earlierExclude(
                    &rest,
                    k.sub(&aver_rt::AverInt::from_i64(1)),
                    p,
                ))
            } else {
                true
            }
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
                                __b.push_str(&::aver_rt::str_lit!("step "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(
                            ": this pattern has no term to equate the subject with"
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
                            crate::proof_kernel::aver_generated::kernel::term::Term__TList(
                                aver_rt::AverList::empty(),
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
                                __b.push_str(&::aver_rt::str_lit!("step "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(
                            ": this pattern has no term to equate the subject with"
                        ));
                        __b
                    }))
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
                let __list_subject = __pat1;
                if let (Some(x), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = __pat2;
                        if let (Some(y), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat3;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.prepend"), aver_rt::AverList::from_vec(vec![x.clone(), y.clone()]))))
                                } else {
                                    Err(aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(74))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&::aver_rt::str_lit!("step "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(path)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(
                                            ": this pattern has no term to equate the subject with"
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
                                        __b.push_str(&::aver_rt::str_lit!("step "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(path),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(
                                    ": this pattern has no term to equate the subject with"
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
                                __b.push_str(&::aver_rt::str_lit!("step "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(
                            ": this pattern has no term to equate the subject with"
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
                        __b.push_str(&::aver_rt::str_lit!("step "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(
                    ": this pattern has no term to equate the subject with"
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
            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(c, ys.clone()),
            ),
        )
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(69)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(
                ": the binders do not fit the constructor pattern"
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
            names.clone()
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
    {
        let __list_subject = bs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(b), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    b.name.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::boundNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// An equation at a substitution.
pub fn instantiate(
    mut e @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Eqn, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = (
            crate::proof_kernel::aver_generated::kernel::subst::subst(e.lhs, bs),
            crate::proof_kernel::aver_generated::kernel::subst::subst(e.rhs, bs),
        );
        match __subject {
            (Ok(l), Ok(r)) => {
                Ok(crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: l, rhs: r })
            }
            _ => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                ::aver_rt::str_lit!("the substitution captures a bound name"),
            ),
        }
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
        None => {
            let __arg6 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(29)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("unknown rule "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(id))));
                __b
            });
            crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg6)
        }
        Some(s @ _) => {
            if (crate::proof_kernel::aver_generated::kernel::check::boundNames(bs)
                == crate::proof_kernel::aver_generated::kernel::proof::givenNames(&s.binders))
            {
                {
                    let __arg8 @ _ = {
                        let __arg7 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("rule "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(id))));
                            __b
                        });
                        crate::proof_kernel::aver_generated::kernel::check::bound(
                            bs,
                            &s.binders,
                            env,
                            __arg7,
                            path.clone(),
                        )
                    };
                    crate::proof_kernel::aver_generated::kernel::check::ruleConclusion(
                        &__arg8,
                        &s.premises,
                        s.concl,
                        bs,
                        ps,
                        env,
                        path,
                    )
                }
            } else {
                {
                    let __arg10 @ _ = aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(44)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("rule "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(id))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" binds "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &({
                                let __arg9 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::proof::givenNames(
                                        &s.binders,
                                    );
                                crate::proof_kernel::aver_generated::kernel::check::commaJoined(
                                    &__arg9,
                                )
                            }),
                        )));
                        __b
                    });
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg10)
                }
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
    ls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
) -> Option<crate::proof_kernel::aver_generated::kernel::proof::Law> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>;
    let mut ls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law> =
        ls;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = ls;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(l), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (l.key == key) {
                        return Some(l.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            ls = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
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
    match crate::proof_kernel::aver_generated::kernel::check::findLaw(key.clone(), &env.laws) {
        None => {
            let __arg6 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("law "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(key))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" is not cited"));
                __b
            });
            crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg6)
        }
        Some(l @ _) => {
            if (crate::proof_kernel::aver_generated::kernel::check::boundNames(bs)
                == crate::proof_kernel::aver_generated::kernel::proof::givenNames(&l.givens))
            {
                {
                    let __arg8 @ _ = {
                        let __arg7 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("law "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(key))));
                            __b
                        });
                        crate::proof_kernel::aver_generated::kernel::check::bound(
                            bs,
                            &l.givens,
                            env,
                            __arg7,
                            path.clone(),
                        )
                    };
                    {
                        let __arg9 @ _ =
                            crate::proof_kernel::aver_generated::kernel::check::lawPremises(&l);
                        {
                            let __arg10 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Eqn {
                                    lhs: l.lhs,
                                    rhs: l.rhs,
                                };
                            crate::proof_kernel::aver_generated::kernel::check::ruleConclusion(
                                &__arg8, &__arg9, __arg10, bs, ps, env, path,
                            )
                        }
                    }
                }
            } else {
                {
                    let __arg12 @ _ = aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("law "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(key),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" quantifies "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &({
                                let __arg11 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::proof::givenNames(
                                        &l.givens,
                                    );
                                crate::proof_kernel::aver_generated::kernel::check::commaJoined(
                                    &__arg11,
                                )
                            }),
                        )));
                        __b
                    });
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg12)
                }
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
    {
        let __list_subject = l.premise.clone();
        if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::term::Eqn {
                    lhs: p.clone(),
                    rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true),
                    ),
                },
            ])
        } else {
            if __list_subject.is_empty() {
                aver_rt::AverList::empty()
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The proof must prove exactly the obligation, under its when; every cited builtin fact is checked first.
pub fn checkScript(
    mut s @ _: crate::proof_kernel::aver_generated::kernel::proof::Script,
) -> Result<AverStr, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::induct::refuseMutualRecursion(&s.defs)?;
    {
        let __arg9 @ _ = {
            let __arg8 @ _ = aver_rt::AverList::concat(
                &s.laws.clone(),
                &crate::proof_kernel::aver_generated::kernel::check::factLaws(&s.facts),
            );
            crate::proof_kernel::aver_generated::kernel::check::lawKeys(&__arg8)
        };
        {
            let __arg10 @ _ = <aver_rt::AverList<AverStr>>::empty();
            crate::proof_kernel::aver_generated::kernel::types::unique(
                &__arg9,
                __arg10,
                ::aver_rt::str_lit!("step proof: cited laws repeat key"),
            )
        }
    }?;
    {
        let __arg11 @ _ =
            <aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>>::empty();
        crate::proof_kernel::aver_generated::kernel::check::checkFacts(s.facts.clone(), __arg11)
    }?;
    let env @ _ = Box::new(
        crate::proof_kernel::aver_generated::kernel::check::Env__Fields {
            defs: s.defs,
            consts: s.consts,
            laws: aver_rt::AverList::concat(
                &s.laws,
                &crate::proof_kernel::aver_generated::kernel::check::factLaws(&s.facts),
            ),
            hyps: crate::proof_kernel::aver_generated::kernel::check::lawHyps(&s.obligation),
            vars: crate::proof_kernel::aver_generated::kernel::types::rigidVars(
                &s.obligation.givens,
            ),
            sums: s.sums,
            recs: s.recs,
            sigs: s.sigs,
        },
    );
    crate::proof_kernel::aver_generated::kernel::check::declarationsTyped(
        &env,
        ::aver_rt::str_lit!("proof"),
    )?;
    crate::proof_kernel::aver_generated::kernel::check::statementTyped(&s.obligation, &env)?;
    let e @ _ = crate::proof_kernel::aver_generated::kernel::check::conclude(
        s.proof,
        env,
        ::aver_rt::str_lit!("proof"),
    )?;
    if (e
        == crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: s.obligation.lhs,
            rhs: s.obligation.rhs,
        })
    {
        Ok(s.obligation.key)
    } else {
        Err(::aver_rt::str_lit!(
            "step proof: the proof ends at a different equation than the claim"
        ))
    }
}

/// The keys of cited laws and checked facts, in order.
#[inline(always)]
pub fn lawKeys(
    ls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ls;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(l), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    l.key.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::lawKeys(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
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
            {
                let __arg7 @ _ = crate::proof_kernel::aver_generated::kernel::check::typeOf(
                    &ob.lhs,
                    env,
                    ::aver_rt::str_lit!("proof"),
                )?;
                {
                    let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::check::typeOf(
                        &ob.rhs,
                        env,
                        ::aver_rt::str_lit!("proof"),
                    )?;
                    crate::proof_kernel::aver_generated::kernel::types::join(__arg7, __arg8)
                }
            },
            crate::proof_kernel::aver_generated::kernel::check::typesOf(
                &ob.premise,
                env,
                ::aver_rt::str_lit!("proof"),
            )?,
        );
        match __pat0 {
            Err(why) => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("step proof: the claim compares "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                __b
            })),
            _ => {
                aver_list_match!(__pat1, [] => Ok(()), [__pat2, __pat3] => { { let __t = __pat2; match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Ty__view::TyBool => {
            { let __list_subject = __pat3; if __list_subject.is_empty() { Ok(()) } else { Err(::aver_rt::str_lit!("step proof: the when is not a Bool")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("step proof: the when is not a Bool"))
        }
    } } })
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
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::check::typeOf(
                        t,
                        env,
                        path.clone(),
                    )?,
                    crate::proof_kernel::aver_generated::kernel::check::typesOf(&rest, env, path)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The obligation's when, as hypothesis `when`.
#[inline(always)]
pub fn lawHyps(
    ob @ _: &crate::proof_kernel::aver_generated::kernel::proof::Law,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ob.premise.clone();
        if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                    name: ::aver_rt::str_lit!("when"),
                    eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
                        lhs: p.clone(),
                        rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true),
                        ),
                    },
                },
            ])
        } else {
            if __list_subject.is_empty() {
                aver_rt::AverList::empty()
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Names separated by commas.
#[inline(always)]
pub fn commaJoined(ns @ _: &aver_rt::AverList<AverStr>) -> AverStr {
    crate::proof_kernel::cancel_checkpoint();
    (aver_rt::string_join(&ns, &::aver_rt::str_lit!(","))).into_aver()
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
    match crate::proof_kernel::aver_generated::kernel::proof::findDef(f.clone(), &env.defs) {
        None => {
            let __arg11 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("no definition of "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                __b
            });
            crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg11)
        }
        Some(d @ _) => {
            match crate::proof_kernel::aver_generated::kernel::induct::recursion(&d, &env.defs) {
                Err(why @ _) => {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, why)
                }
                Ok(__pat0 @ _) => match __pat0 {
                    None => {
                        let __arg12 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" does not recurse"));
                            __b
                        });
                        crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg12)
                    }
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
    {
        let __subject = (
            crate::proof_kernel::aver_generated::kernel::check::argsFit(d, xs, env, path.clone()),
            {
                let __arg11 @ _ =
                    crate::proof_kernel::aver_generated::kernel::proof::givenNames(&env.vars);
                crate::proof_kernel::aver_generated::kernel::induct::varied(
                    xs,
                    r.at.clone(),
                    __arg11,
                )
            },
        );
        match __subject {
            (Err(why), _) => Err(why),
            (_, Err(why)) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
            (_, Ok(vr)) => {
                let __arg12 @ _ = crate::proof_kernel::aver_generated::kernel::check::namedHyps(
                    carry,
                    &env.hyps,
                    path.clone(),
                )?;
                crate::proof_kernel::aver_generated::kernel::check::inductChecked(
                    d, r, xs, claim, &__arg12, cs, &vr, env, path,
                )
            }
        }
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
    let varying @ _ = aver_rt::AverList::prepend_owned(
        vr.v.clone(),
        crate::proof_kernel::aver_generated::kernel::check::placeNames(&vr.general),
    );
    {
        let __subject = (
            (aver_rt::AverInt::from_i64(r.arms.len() as i64)
                == aver_rt::AverInt::from_i64(cs.len() as i64)),
            crate::proof_kernel::aver_generated::kernel::check::measured(r, vr.v.clone(), env),
        );
        match __subject {
            (false, _) => {
                let __arg11 @ _ = aver_rt::AverStr::from({
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
                            __b.push_str(&::aver_rt::str_lit!(" arms, "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(aver_rt::AverInt::from_i64(cs.len() as i64)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" cases"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg11)
            }
            (_, Err(why)) => crate::proof_kernel::aver_generated::kernel::check::refuse(path, why),
            _ => {
                let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::induct::Call {
                    args: xs.clone(),
                    inner: aver_rt::AverList::empty(),
                };
                {
                    let __arg14 @ _ =
                        crate::proof_kernel::aver_generated::kernel::check::takenNames(
                            claim, &env.hyps,
                        );
                    {
                        let __arg15 @ _ = {
                            let __arg12 @ _ = crate::proof_kernel::aver_generated::kernel::check::notMentioningAny(&env.hyps, &varying);
                            crate::proof_kernel::aver_generated::kernel::check::withHyps(
                                env, &__arg12,
                            )
                        };
                        crate::proof_kernel::aver_generated::kernel::check::inductCases(
                            d,
                            r,
                            r.arms.clone(),
                            cs.clone(),
                            &__arg13,
                            claim,
                            vr,
                            &__arg14,
                            carried,
                            &__arg15,
                            path,
                            aver_rt::AverInt::from_i64(0),
                        )
                    }
                }
            }
        }
    }
}

/// A match on the value itself needs exactly one well-shaped arm per constructor of v's type; a count toward zero needs v to be a given of type Int.
#[inline(always)]
pub fn measured(
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    v @ _: AverStr,
    env @ _: &Env,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match r.guard.clone() {
        None => {
            let __subject = {
                let __arg7 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v),
                );
                {
                    let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
                    crate::proof_kernel::aver_generated::kernel::types::typeOf(&__arg7, &__arg8)
                }
            };
            match __subject {
                Ok(t @ _) => {
                    if {
                        let __arg9 @ _ =
                            crate::proof_kernel::aver_generated::kernel::check::armsOf(&r.arms);
                        {
                            let __arg10 @ _ =
                                crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
                            crate::proof_kernel::aver_generated::kernel::check::constructorArmsFit(
                                __arg9, &t, &__arg10,
                            )
                        }
                    } {
                        Ok(())
                    } else {
                        Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(71)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!(
                                    "the induction arms are not exactly the constructors of "
                                ));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_kernel::aver_generated::kernel::types::show(&t)),
                            )));
                            __b
                        }))
                    }
                }
                Err(why @ _) => Err(why),
            }
        }
        Some(g @ _) => {
            let __subject = {
                let __arg11 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v.clone()),
                );
                {
                    let __arg12 @ _ =
                        crate::proof_kernel::aver_generated::kernel::check::tyEnv(env);
                    crate::proof_kernel::aver_generated::kernel::types::typeOf(&__arg11, &__arg12)
                }
            };
            match __subject {
                Ok(__pat0) => {
                    let __t = __pat0;
                    match __t.view() {
                        crate::proof_kernel::aver_generated::kernel::term::Ty__view::TyInt => {
                            Ok(())
                        }
                        _ => Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" is not a given of type Int"));
                            __b
                        })),
                    }
                }
                _ => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" is not a given of type Int"));
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
    {
        let __list_subject = acs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    a.arm.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::armsOf(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The hypotheses that mention none of the names.
#[inline(always)]
pub fn notMentioningAny(
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    names @ _: &aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>;
    let mut hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp> =
        hs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = hs;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if {
                        let __arg4 @ _ = aver_rt::AverList::from_vec(vec![h.clone()]);
                        crate::proof_kernel::aver_generated::kernel::check::hypsMention(
                            &__arg4, names,
                        )
                    } {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            hs = &__hold0;
                            continue;
                        }
                    } else {
                        return aver_rt::AverList::prepend_owned(
                            h.clone(),
                            crate::proof_kernel::aver_generated::kernel::check::notMentioningAny(
                                &rest, names,
                            ),
                        );
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The names of the varied places.
#[inline(always)]
pub fn placeNames(
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Place>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ps;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    p.name.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::placeNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Whether some hypothesis mentions one of the names.
#[inline(always)]
pub fn hypsMention(
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    names @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            false
        } else {
            if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                ({
                    let __arg4 @ _ = aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(&h.eqn.lhs),
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(&h.eqn.rhs),
                    );
                    crate::proof_kernel::aver_generated::kernel::check::anyIn(&__arg4, names)
                } || crate::proof_kernel::aver_generated::kernel::check::hypsMention(&rest, names))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Whether one of ns is among names.
#[inline(always)]
pub fn anyIn(ns @ _: &aver_rt::AverList<AverStr>, names @ _: &aver_rt::AverList<AverStr>) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ns;
        if __list_subject.is_empty() {
            false
        } else {
            if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                (names.contains(&n)
                    || crate::proof_kernel::aver_generated::kernel::check::anyIn(&rest, names))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
            &crate::proof_kernel::aver_generated::kernel::subst::freeVars(&claim.lhs),
            &crate::proof_kernel::aver_generated::kernel::subst::freeVars(&claim.rhs),
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
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::concat(
                    &aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(&h.eqn.lhs),
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(&h.eqn.rhs),
                    ),
                    &crate::proof_kernel::aver_generated::kernel::check::hypNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The arms are a permutation of every constructor of t, without duplicates, and each pattern names exactly the constructor's fields.
#[inline(always)]
pub fn constructorArmsFit(
    mut arms @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
    env @ _: &crate::proof_kernel::aver_generated::kernel::types::TyEnv,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    ({
        let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::types::ctorsOf(t, env);
        {
            let __arg4 @ _ =
                crate::proof_kernel::aver_generated::kernel::check::armConstructorNames(&arms);
            crate::proof_kernel::aver_generated::kernel::check::sameNames(&__arg3, &__arg4)
        }
    } && crate::proof_kernel::aver_generated::kernel::check::constructorFieldsFit(arms, t, env))
}

/// Two lists contain every name the same number of times, in any order.
#[inline(always)]
pub fn sameNames(
    expected @ _: &aver_rt::AverList<AverStr>,
    actual @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    ((aver_rt::AverInt::from_i64(expected.len() as i64)
        == aver_rt::AverInt::from_i64(actual.len() as i64))
        && crate::proof_kernel::aver_generated::kernel::check::sameNameCounts(
            expected, expected, actual,
        ))
}

/// Every name occurring on the left occurs equally often on both sides.
#[inline(always)]
pub fn sameNameCounts(
    left @ _: &aver_rt::AverList<AverStr>,
    allLeft @ _: &aver_rt::AverList<AverStr>,
    right @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = left;
        if __list_subject.is_empty() {
            true
        } else {
            if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                ((crate::proof_kernel::aver_generated::kernel::check::countName(
                    n.clone(),
                    allLeft,
                ) == crate::proof_kernel::aver_generated::kernel::check::countName(n, right))
                    && crate::proof_kernel::aver_generated::kernel::check::sameNameCounts(
                        &rest, allLeft, right,
                    ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// How often want occurs in names.
#[inline(always)]
pub fn countName(
    mut want @ _: AverStr,
    names @ _: &aver_rt::AverList<AverStr>,
) -> aver_rt::AverInt {
    let mut __hold1: aver_rt::AverList<AverStr>;
    let mut names @ _: &aver_rt::AverList<AverStr> = names;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = names;
            if __list_subject.is_empty() {
                return aver_rt::AverInt::from_i64(0);
            } else {
                if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    if (n == want) {
                        return aver_rt::AverInt::from_i64(1).add(
                            &crate::proof_kernel::aver_generated::kernel::check::countName(
                                want, &rest,
                            ),
                        );
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            names = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The constructor selected by each constructor-shaped arm.
#[inline(always)]
pub fn armConstructorNames(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = arms;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::check::patternConstructorName(
                        &a.pattern,
                    ),
                    crate::proof_kernel::aver_generated::kernel::check::armConstructorNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The split name of a constructor-shaped pattern; an empty name for another pattern.
pub fn patternConstructorName(
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> AverStr {
    crate::proof_kernel::cancel_checkpoint();
    match p {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PNil => {
            ::aver_rt::str_lit!("nil")
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            ::aver_rt::str_lit!("cons")
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
            let c = c.clone();
            let ns = ns.clone();
            c
        }
        _ => {
            ::aver_rt::str_lit!("")
        }
    }
}

/// Every arm is constructor-shaped and binds exactly the number of fields that constructor carries.
#[inline(always)]
pub fn constructorFieldsFit(
    mut arms @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
    env @ _: &crate::proof_kernel::aver_generated::kernel::types::TyEnv,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(arms, [] => true, [a, rest] => (crate::proof_kernel::aver_generated::kernel::check::constructorFieldCountFits(&a.pattern, t, env) && crate::proof_kernel::aver_generated::kernel::check::constructorFieldsFit(rest, t, env)))
}

/// A constructor-shaped pattern has one name per declared field.
#[inline(always)]
pub fn constructorFieldCountFits(
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
    env @ _: &crate::proof_kernel::aver_generated::kernel::types::TyEnv,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = {
            let __arg5 @ _ =
                crate::proof_kernel::aver_generated::kernel::check::patternConstructorName(p);
            crate::proof_kernel::aver_generated::kernel::types::ctorFields(__arg5, t, env)
        };
        match __subject {
            Ok(fs @ _) => {
                (aver_rt::AverInt::from_i64(fs.len() as i64)
                    == aver_rt::AverInt::from_i64(
                        crate::proof_kernel::aver_generated::kernel::check::patFields(p).len()
                            as i64,
                    ))
            }
            Err(why @ _) => false,
        }
    }
}

/// Fresh names for what the case introduces: the arm's pattern variables, or in a count toward zero the hypothesis that the comparison has the arm's value. Then the claim in the case, under one hypothesis per recursive call at that call's arguments.
pub fn inductCase(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    mut a @ _: crate::proof_kernel::aver_generated::kernel::induct::ArmCalls,
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
    {
        let __subject = (
            (aver_rt::AverInt::from_i64(c.binders.len() as i64)
                == crate::proof_kernel::aver_generated::kernel::check::caseNames(r, &a.arm)),
            {
                let __arg11 @ _ = <aver_rt::AverList<AverStr>>::empty();
                crate::proof_kernel::aver_generated::kernel::check::freshAll(
                    &c.binders, taken, env, &__arg11,
                )
            },
        );
        match __subject {
            (false, _) => crate::proof_kernel::aver_generated::kernel::check::refuseCase(
                path,
                ::aver_rt::str_lit!("wrong number of names"),
            ),
            (_, false) => crate::proof_kernel::aver_generated::kernel::check::refuseCase(
                path,
                ::aver_rt::str_lit!("the names are not fresh"),
            ),
            _ => {
                let __arg12 @ _ =
                    crate::proof_kernel::aver_generated::kernel::check::namesToVars(&c.binders);
                crate::proof_kernel::aver_generated::kernel::check::inductCaseNamed(
                    d, r, a, c, &__arg12, at, claim, vr, carried, env, path,
                )
            }
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
                    __b.push_str(&::aver_rt::str_lit!("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(": "));
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
    {
        let __list_subject = bs;
        if __list_subject.is_empty() {
            true
        } else {
            if let Some((b, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                ((!(((taken.contains(&b) || (&*b == "_"))
                    || crate::proof_kernel::aver_generated::kernel::proof::givenNames(&env.vars)
                        .contains(&b))
                    || (crate::proof_kernel::aver_generated::kernel::check::isConst(
                        b.clone(),
                        &env.consts,
                    ) || before.contains(&b))))
                    && {
                        let __arg6 @ _ = aver_rt::AverList::prepend(b, before);
                        crate::proof_kernel::aver_generated::kernel::check::freshAll(
                            &rest, taken, env, &__arg6,
                        )
                    })
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Whether n names a module-level binding.
#[inline(always)]
pub fn isConst(
    n @ _: AverStr,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            false
        } else {
            if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                ((c.name == n)
                    || crate::proof_kernel::aver_generated::kernel::check::isConst(n, &rest))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Each name as a variable.
#[inline(always)]
pub fn namesToVars(
    ns @ _: &aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ns;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TVar(n),
                    ),
                    crate::proof_kernel::aver_generated::kernel::check::namesToVars(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The goal, the carried hypotheses and in a count toward zero the arm's value of the comparison, then the hypotheses of the arm's recursive calls, those the gate checked to descend; the case's proof must prove the goal.
pub fn inductCaseNamed(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    r @ _: &crate::proof_kernel::aver_generated::kernel::induct::Recursion,
    mut a @ _: crate::proof_kernel::aver_generated::kernel::induct::ArmCalls,
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
        &{
            let __arg17 @ _ = {
                let __arg16 @ _ =
                    crate::proof_kernel::aver_generated::kernel::check::patFields(&a.arm.pattern);
                crate::proof_kernel::aver_generated::kernel::check::zipBind(&__arg16, ys)
            };
            {
                let __arg18 @ _ = aver_rt::AverList::from_vec(vec![::aver_rt::str_lit!("_")]);
                crate::proof_kernel::aver_generated::kernel::check::withoutNames(__arg17, __arg18)
            }
        },
        &{
            let __arg20 @ _ = {
                let __arg19 @ _ = crate::proof_kernel::aver_generated::kernel::types::paramsOf(d);
                crate::proof_kernel::aver_generated::kernel::check::zipBind(&__arg19, &at.args)
            };
            crate::proof_kernel::aver_generated::kernel::check::withoutNames(__arg20, names)
        },
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
    let scope @ _ = {
        let __arg22 @ _ = {
            let __arg21 @ _ = aver_rt::AverList::concat(
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
            );
            crate::proof_kernel::aver_generated::kernel::check::withHyps(env, &__arg21)
        };
        {
            let __arg23 @ _ = crate::proof_kernel::aver_generated::kernel::check::caseVarsOf(
                &r.guard,
                a.arm.pattern,
                &c.binders,
                vr.v.clone(),
                env,
                path.clone(),
            )?;
            crate::proof_kernel::aver_generated::kernel::check::withVars(&__arg22, &__arg23)
        }
    };
    {
        let __subject = (
            (aver_rt::AverInt::from_i64(a.calls.len() as i64)
                == aver_rt::AverInt::from_i64(c.ihs.len() as i64)),
            (aver_rt::AverInt::from_i64(c.carry.len() as i64)
                == aver_rt::AverInt::from_i64(c.ihs.len() as i64)),
        );
        match __subject {
            (false, _) => {
                let __arg24 @ _ = aver_rt::AverStr::from({
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
                            __b.push_str(&::aver_rt::str_lit!(" recursive calls, "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(aver_rt::AverInt::from_i64(c.ihs.len() as i64)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" hypotheses"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::refuseCase(path, __arg24)
            }
            (_, false) => crate::proof_kernel::aver_generated::kernel::check::refuseCase(
                path,
                ::aver_rt::str_lit!("one list of carried proofs per hypothesis"),
            ),
            _ => {
                let __arg26 @ _ = {
                    let __arg25 @ _ = aver_rt::AverList::concat(
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
                    );
                    crate::proof_kernel::aver_generated::kernel::check::withHyps(&scope, &__arg25)
                };
                crate::proof_kernel::aver_generated::kernel::check::caseProves(
                    c.proof, &goal, __arg26, path,
                )
            }
        }
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
    match guard {
        Some(g @ _) => Ok(aver_rt::AverList::empty()),
        None => {
            let __arg8 @ _ =
                crate::proof_kernel::aver_generated::kernel::check::namedParts(p, binders);
            {
                let __arg9 @ _ = {
                    let __arg7 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v),
                    );
                    crate::proof_kernel::aver_generated::kernel::check::typeOf(
                        &__arg7,
                        env,
                        path.clone(),
                    )
                }?;
                crate::proof_kernel::aver_generated::kernel::check::patTypes(
                    __arg8, __arg9, env, path,
                )
            }
        }
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
    match guard {
        None => Ok(aver_rt::AverList::from_vec(vec![{
            let __arg6 @ _ =
                crate::proof_kernel::aver_generated::kernel::check::patTerm(p, ys, path)?;
            crate::proof_kernel::aver_generated::kernel::term::bind(v, __arg6)
        }])),
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
            Some(g @ _) => {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(__pat3) => {
                        let __t = __pat3;
                        match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TBool(__b_fields) => {
            let b = __b_fields.0.clone();
            { let __list_subject = __pat2; if let Some((h, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat4; if __list_subject.is_empty() { Ok(aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::proof::Hyp { name: h, eqn: { let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::term::Eqn { lhs: g, rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(b)) }; crate::proof_kernel::aver_generated::kernel::check::instantiate(__arg13, rename, path) }? }])) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(65)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&::aver_rt::str_lit!(": the arm is not one value of the comparison")); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(65)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&::aver_rt::str_lit!(": the arm is not one value of the comparison")); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(65)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("step ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path)))); __b }; __b.push_str(&::aver_rt::str_lit!(": the arm is not one value of the comparison")); __b }))
        }
    }
                    }
                    _ => Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(65)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("step "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(
                            ": the arm is not one value of the comparison"
                        ));
                        __b
                    })),
                }
            }
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
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                        name: h.name.clone(),
                        eqn: crate::proof_kernel::aver_generated::kernel::check::instantiate(
                            h.eqn.clone(),
                            bs,
                            path.clone(),
                        )?,
                    },
                    crate::proof_kernel::aver_generated::kernel::check::hypsAt(&rest, bs, path)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Bindings for names a pattern does not shadow.
#[inline(always)]
pub fn withoutNames(
    mut bs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    mut names @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        ::aver_rt::aver_list_match_reuse!(bs, [] => { return aver_rt::AverList::empty(); }, [b, rest, __list_slot] => { if names.contains(&b.name) { {
            let __tco0 = rest;
            bs = __tco0;
            continue;
        } } else { return __list_slot.prepend(b, crate::proof_kernel::aver_generated::kernel::check::withoutNames(rest, names)); } })
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
    {
        let __list_subject = ms;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(m), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let __subject = (
                        crate::proof_kernel::aver_generated::kernel::check::callAt(
                            calls,
                            m.call.clone(),
                        ),
                        (aver_rt::AverInt::from_i64(m.ih.at.len() as i64)
                            == aver_rt::AverInt::from_i64(vr.general.len() as i64)),
                        (aver_rt::AverInt::from_i64(m.ih.carry.len() as i64)
                            == aver_rt::AverInt::from_i64(carried.len() as i64)),
                    );
                    match __subject {
                        (None, _, _) => Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(57))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("step "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(path),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(": no recursive call "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(m.call))));
                            __b
                        })),
                        (_, false, _) => {
                            Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b = {
                                                            let mut __b = {
                                                                let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(104)).to_usize().unwrap_or(0));
                                                                __b.push_str(&::aver_rt::str_lit!(
                                                                    "step "
                                                                ));
                                                                __b
                                                            };
                                                            __b.push_str(&aver_rt::AverStr::from(
                                                                aver_rt::aver_display(&(path)),
                                                            ));
                                                            __b
                                                        };
                                                        __b.push_str(&::aver_rt::str_lit!(": "));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(m.ih.name)),
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&::aver_rt::str_lit!(" gives "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(
                                                    &(aver_rt::AverInt::from_i64(
                                                        m.ih.at.len() as i64
                                                    )),
                                                ),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" values for "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(aver_rt::AverInt::from_i64(vr.general.len() as i64)),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" varied givens"));
                                __b
                            }))
                        }
                        (_, _, false) => Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(55)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("step "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(
                                ": one proof per carried hypothesis"
                            ));
                            __b
                        })),
                        (Some(c), true, true) => {
                            Ok(aver_rt::AverList::concat(
                                &crate::proof_kernel::aver_generated::kernel::check::moreHyps(
                                    &rest,
                                    calls,
                                    rename,
                                    j.clone(),
                                    claim.clone(),
                                    vr,
                                    carried,
                                    scope,
                                    path.clone(),
                                )?,
                                &{
                                    let __arg14 @ _ = aver_rt::AverList::prepend_owned(
                                        {
                                            let __arg12 @ _ = crate::proof_kernel::aver_generated::kernel::check::ihAt(&c, rename, j, vr, path.clone())?;
                                            crate::proof_kernel::aver_generated::kernel::check::partBinding(&__arg12, path.clone())
                                        }?,
                                        {
                                            let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::check::placeNames(&vr.general);
                                            crate::proof_kernel::aver_generated::kernel::check::zipBind(&__arg13, &m.ih.at)
                                        },
                                    );
                                    crate::proof_kernel::aver_generated::kernel::check::ihHolds(
                                        &__arg14,
                                        m.ih.name.clone(),
                                        &m.ih.carry,
                                        claim,
                                        carried,
                                        scope,
                                        path,
                                    )
                                }?,
                            ))
                        }
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The recursive call at place k.
#[inline(always)]
pub fn callAt(
    calls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Call>,
    mut k @ _: aver_rt::AverInt,
) -> Option<crate::proof_kernel::aver_generated::kernel::induct::Call> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Call>;
    let mut calls @ _: &aver_rt::AverList<
        crate::proof_kernel::aver_generated::kernel::induct::Call,
    > = calls;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = calls;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (k == aver_rt::AverInt::from_i64(0)) {
                        return Some(c.clone());
                    } else {
                        {
                            let __tco0 = rest;
                            let __tco1 = k.sub(&aver_rt::AverInt::from_i64(1));
                            __hold0 = __tco0;
                            calls = &__hold0;
                            k = __tco1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The binding of the given inducted on, which ihAt puts first.
#[inline(always)]
pub fn partBinding(
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    path @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Binding, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = bs;
        if let (Some(b), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            Ok(b.clone())
        } else {
            if __list_subject.is_empty() {
                Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(53)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("step "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(": a recursive call binds nothing"));
                    __b
                }))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
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
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
    vr @ _: &crate::proof_kernel::aver_generated::kernel::induct::Varied,
    carried @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    scope @ _: &Env,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = (
            (&*h == "_"),
            (aver_rt::AverInt::from_i64(ps.len() as i64)
                == aver_rt::AverInt::from_i64(carried.len() as i64)),
        );
        match __subject {
            (true, _) => Ok(aver_rt::AverList::empty()),
            (_, false) => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(55)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("step "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(": one proof per carried hypothesis"));
                __b
            })),
            _ => {
                let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::check::ihAt(
                    c,
                    rename,
                    j,
                    vr,
                    path.clone(),
                )?;
                crate::proof_kernel::aver_generated::kernel::check::ihHolds(
                    &__arg10, h, ps, claim, carried, scope, path,
                )
            }
        }
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
    {
        let __arg9 @ _ = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(27)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("hypothesis "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
            __b
        });
        crate::proof_kernel::aver_generated::kernel::check::bound(
            bs,
            &scope.vars,
            scope,
            __arg9,
            path.clone(),
        )
    }?;
    {
        let __arg10 @ _ = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!("/"));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h))));
            __b
        });
        crate::proof_kernel::aver_generated::kernel::check::carriedHold(
            carried.clone(),
            ps.clone(),
            bs,
            scope,
            __arg10,
        )
    }?;
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
    if {
        let __arg6 @ _ = {
            let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::check::readArgs(
                &c.args,
                j.clone(),
                &vr.general,
                aver_rt::AverInt::from_i64(0),
            );
            crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(&__arg5)
        };
        crate::proof_kernel::aver_generated::kernel::check::anyIn(&__arg6, &c.inner)
    } {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(73)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(
                ": a recursive call reads a name an inner match binds"
            ));
            __b
        }))
    } else {
        {
            let __arg7 @ _ =
                crate::proof_kernel::aver_generated::kernel::subst::substAll(&c.args, rename)?;
            crate::proof_kernel::aver_generated::kernel::check::ihBindings(&__arg7, j, vr, path)
        }
    }
}

/// The arguments a hypothesis reads: the one at place j and those at the varied places.
#[inline(always)]
pub fn readArgs(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut j @ _: aver_rt::AverInt,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Place>,
    mut k @ _: aver_rt::AverInt,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>;
    let mut xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> =
        xs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = xs;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if ((k == j)
                        || crate::proof_kernel::aver_generated::kernel::check::atPlace(
                            ps,
                            k.clone(),
                        ))
                    {
                        return aver_rt::AverList::prepend_owned(
                            x.clone(),
                            crate::proof_kernel::aver_generated::kernel::check::readArgs(
                                &rest,
                                j,
                                ps,
                                k.add(&aver_rt::AverInt::from_i64(1)),
                            ),
                        );
                    } else {
                        {
                            let __tco0 = rest;
                            let __tco3 = k.add(&aver_rt::AverInt::from_i64(1));
                            __hold0 = __tco0;
                            xs = &__hold0;
                            k = __tco3;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Whether a varied given sits at place k.
#[inline(always)]
pub fn atPlace(
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::induct::Place>,
    k @ _: aver_rt::AverInt,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ps;
        if __list_subject.is_empty() {
            false
        } else {
            if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                ((p.at == k)
                    || crate::proof_kernel::aver_generated::kernel::check::atPlace(&rest, k))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
    match crate::proof_kernel::aver_generated::kernel::induct::nthTerm(args, j) {
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(61)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(
                ": a recursive call has too few arguments"
            ));
            __b
        })),
        Some(at @ _) => Ok(aver_rt::AverList::prepend_owned(
            crate::proof_kernel::aver_generated::kernel::term::bind(vr.v.clone(), at),
            crate::proof_kernel::aver_generated::kernel::check::placeBindings(&vr.general, args),
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
    {
        let __list_subject = ps;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    {
                        let __arg4 @ _ =
                            crate::proof_kernel::aver_generated::kernel::induct::nthTerm(
                                args,
                                p.at.clone(),
                            )
                            .unwrap_or(
                                crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TVar(
                                        p.name.clone(),
                                    ),
                                ),
                            );
                        crate::proof_kernel::aver_generated::kernel::term::bind(
                            p.name.clone(),
                            __arg4,
                        )
                    },
                    crate::proof_kernel::aver_generated::kernel::check::placeBindings(&rest, args),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
                    __b.push_str(&::aver_rt::str_lit!("step "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(
                ": the case proves a different equation"
            ));
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
    {
        let __subject = (
            crate::proof_kernel::aver_generated::kernel::check::listVar(v.clone(), env),
            general.contains(&v),
        );
        match __subject {
            (None, _) => {
                let __arg13 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(44)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" is not a given of list type"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg13)
            }
            (_, true) => {
                let __arg14 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(52)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" is both inducted on and generalised"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg14)
            }
            (Some(e), false) => {
                if {
                    let __arg15 @ _ = aver_rt::AverList::from_vec(vec![h.clone(), t.clone()]);
                    {
                        let __arg16 @ _ =
                            crate::proof_kernel::aver_generated::kernel::check::takenNames(
                                &claim, &env.hyps,
                            );
                        {
                            let __arg17 @ _ = <aver_rt::AverList<AverStr>>::empty();
                            crate::proof_kernel::aver_generated::kernel::check::freshAll(
                                &__arg15, &__arg16, env, &__arg17,
                            )
                        }
                    }
                } {
                    {
                        let __arg20 @ _ = {
                            let __arg19 @ _ = aver_rt::AverList::concat(&carried.clone(), &{
                                let __arg18 @ _ = aver_rt::AverList::prepend(v.clone(), general);
                                crate::proof_kernel::aver_generated::kernel::check::notMentioningAny(
                                    &env.hyps, &__arg18,
                                )
                            });
                            crate::proof_kernel::aver_generated::kernel::check::withHyps(
                                env, &__arg19,
                            )
                        };
                        crate::proof_kernel::aver_generated::kernel::check::listCases(
                            v, e, claim, &carried, n, h, t, general, ihs, c, &__arg20, path,
                        )
                    }
                } else {
                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                        path,
                        ::aver_rt::str_lit!("the names are not fresh"),
                    )
                }
            }
        }
    }
}

/// Induction on the length of a given v of list type, for every value of the names in general. With names [n, hn, ih] fresh for everything in scope, c proves the claim as it stands under the carried hypotheses, each a fact, those that mention neither v nor what it generalises, an Int n, hn saying v is at most n long, and the law ih: the claim at every v shorter than n and every value of general where the carried hypotheses hold. Then the claim holds whenever v is at most n long, for every n by induction on n, so for every v.
pub fn lenInduct(
    v @ _: AverStr,
    mut claim @ _: crate::proof_kernel::aver_generated::kernel::term::Eqn,
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
            None => {
                let __arg18 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(44)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" is not a given of list type"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg18)
            }
            _ => {
                if __pat1 {
                    {
                        let __arg19 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(52)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(
                                " is both inducted on and generalised"
                            ));
                            __b
                        });
                        crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg19)
                    }
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
                                                    if {
                                                        let __arg20 @ _ = aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::check::takenNames(&claim, &env.hyps), &crate::proof_kernel::aver_generated::kernel::check::hypLabels(&env.hyps));
                                                        {
                                                            let __arg21 @ _ = <aver_rt::AverList<
                                                                AverStr,
                                                            >>::empty(
                                                            );
                                                            crate::proof_kernel::aver_generated::kernel::check::freshAll(names, &__arg20, env, &__arg21)
                                                        }
                                                    } {
                                                        {
                                                            let __arg25 @ _ = crate::proof_kernel::aver_generated::kernel::proof::Law { key: ih, givens: { let __arg22 @ _ = aver_rt::AverList::prepend(v.clone(), general); crate::proof_kernel::aver_generated::kernel::check::typedNames(&__arg22, env, path.clone()) }?, premise: aver_rt::AverList::from_vec(vec![{ let __arg23 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("<"), crate::proof_kernel::aver_generated::kernel::term::Term::TBi(crate::proof_kernel::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.len"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(v.clone()))]))), crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(n.clone())))); { let __arg24 @ _ = crate::proof_kernel::aver_generated::kernel::check::factsOf(&carried, path.clone())?; crate::proof_kernel::aver_generated::kernel::check::conjoined(__arg23, &__arg24) } }]), lhs: claim.lhs.clone(), rhs: claim.rhs.clone() };
                                                            crate::proof_kernel::aver_generated::kernel::check::lenCase(v, claim, &carried, general, n, hn, &__arg25, c, env, path)
                                                        }
                                                    } else {
                                                        crate::proof_kernel::aver_generated::kernel::check::refuse(path, ::aver_rt::str_lit!("the names are not fresh"))
                                                    }
                                                } else {
                                                    crate::proof_kernel::aver_generated::kernel::check::refuse(path, ::aver_rt::str_lit!("a length induction names a bound, its hypothesis and its law"))
                                                }
                                            }
                                        } else {
                                            crate::proof_kernel::aver_generated::kernel::check::refuse(path, ::aver_rt::str_lit!("a length induction names a bound, its hypothesis and its law"))
                                        }
                                    }
                                } else {
                                    crate::proof_kernel::aver_generated::kernel::check::refuse(
                                        path,
                                        ::aver_rt::str_lit!(
                                            "a length induction names a bound, its hypothesis and its law"
                                        ),
                                    )
                                }
                            }
                        } else {
                            crate::proof_kernel::aver_generated::kernel::check::refuse(
                                path,
                                ::aver_rt::str_lit!(
                                    "a length induction names a bound, its hypothesis and its law"
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
    let bounded @ _ = crate::proof_kernel::aver_generated::kernel::proof::Hyp {
        name: hn,
        eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn {
            lhs: crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                    ::aver_rt::str_lit!("<="),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            ::aver_rt::str_lit!("List.len"),
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TVar(
                                        v.clone(),
                                    ),
                                ),
                            ]),
                        ),
                    ),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TVar(n.clone()),
                    ),
                ),
            ),
            rhs: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true),
            ),
        },
    };
    let hyps @ _ = aver_rt::AverList::prepend_owned(
        bounded,
        aver_rt::AverList::concat(&carried.clone(), &{
            let __arg13 @ _ = aver_rt::AverList::prepend(v, general);
            crate::proof_kernel::aver_generated::kernel::check::notMentioningAny(
                &env.hyps, &__arg13,
            )
        }),
    );
    {
        let __arg15 @ _ = Box::new(
            crate::proof_kernel::aver_generated::kernel::check::Env__Fields {
                defs: env.defs.clone(),
                consts: env.consts.clone(),
                laws: aver_rt::AverList::prepend(ih.clone(), &env.laws),
                hyps: hyps,
                vars: aver_rt::AverList::prepend(
                    {
                        let __arg14 @ _ =
                            crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt;
                        crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg14)
                    },
                    &env.vars,
                ),
                sums: env.sums.clone(),
                recs: env.recs.clone(),
                sigs: env.sigs.clone(),
            },
        );
        {
            let __arg16 @ _ = aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!("/below"));
                __b
            });
            crate::proof_kernel::aver_generated::kernel::check::caseProves(
                c, &claim, __arg15, __arg16,
            )
        }
    }?;
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
    {
        let __list_subject = ns;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let Some((x, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                Ok(aver_rt::AverList::prepend_owned(
                    {
                        let __arg6 @ _ = {
                            let __arg5 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TVar(
                                        x.clone(),
                                    ),
                                );
                            crate::proof_kernel::aver_generated::kernel::check::typeOf(
                                &__arg5,
                                env,
                                path.clone(),
                            )
                        }?;
                        crate::proof_kernel::aver_generated::kernel::term::typed(x, __arg6)
                    },
                    crate::proof_kernel::aver_generated::kernel::check::typedNames(
                        &rest, env, path,
                    )?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// What each hypothesis says is true; a refusal for one that is no such fact.
#[inline(always)]
pub fn factsOf(
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                if (h.eqn.rhs
                    == crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true),
                    ))
                {
                    Ok(aver_rt::AverList::prepend_owned(
                        h.eqn.lhs.clone(),
                        crate::proof_kernel::aver_generated::kernel::check::factsOf(&rest, path)?,
                    ))
                } else {
                    Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(53))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("step "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(path),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(": "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h.name))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" is not a fact"));
                        __b
                    }))
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// t and each of rest, joined by Bool.and to the right.
#[inline(always)]
pub fn conjoined(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    rest @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> crate::proof_kernel::aver_generated::kernel::term::Term {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = rest;
        if __list_subject.is_empty() {
            t
        } else {
            if let (Some(x), Some(more)) = (__list_subject.first(), __list_subject.tail()) {
                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                        ::aver_rt::str_lit!("Bool.and"),
                        aver_rt::AverList::from_vec(vec![
                            t,
                            crate::proof_kernel::aver_generated::kernel::check::conjoined(
                                x.clone(),
                                &more,
                            ),
                        ]),
                    ),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The environment with a record R of two Int fields a and b declared.
pub fn withRec(env @ _: &Env) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    Box::new(
        crate::proof_kernel::aver_generated::kernel::check::Env__Fields {
            defs: env.defs.clone(),
            consts: env.consts.clone(),
            laws: env.laws.clone(),
            hyps: env.hyps.clone(),
            vars: env.vars.clone(),
            sums: env.sums.clone(),
            recs: aver_rt::AverList::prepend(
                crate::proof_kernel::aver_generated::kernel::proof::Rec {
                    name: ::aver_rt::str_lit!("R"),
                    eq: true,
                    fields: aver_rt::AverList::from_vec(vec![
                        {
                            let __arg1 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt;
                            crate::proof_kernel::aver_generated::kernel::term::typed(
                                ::aver_rt::str_lit!("a"),
                                __arg1,
                            )
                        },
                        {
                            let __arg2 @ _ =
                                crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt;
                            crate::proof_kernel::aver_generated::kernel::term::typed(
                                ::aver_rt::str_lit!("b"),
                                __arg2,
                            )
                        },
                    ]),
                },
                &env.recs,
            ),
            sigs: env.sigs.clone(),
        },
    )
}

/// No definitions, laws or hypotheses, and these givens of list type.
#[inline(always)]
pub fn listsEnv(ns @ _: &aver_rt::AverList<AverStr>) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::check::emptyEnv();
    let __arg3 @ _ = {
        let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyList(
            crate::proof_kernel::aver_generated::kernel::term::Ty__TyList(
                crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny,
            ),
        );
        crate::proof_kernel::aver_generated::kernel::check::typedAll(ns, &__arg1)
    };
    crate::proof_kernel::aver_generated::kernel::check::withVars(&__arg2, &__arg3)
}

/// Each name with the one type.
#[inline(always)]
pub fn typedAll(
    ns @ _: &aver_rt::AverList<AverStr>,
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Ty,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ns;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::term::typed(n, t.clone()),
                    crate::proof_kernel::aver_generated::kernel::check::typedAll(&rest, t),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
    let atNil @ _ = aver_rt::AverList::from_vec(vec![{
        let __arg17 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TList(
            crate::proof_kernel::aver_generated::kernel::term::Term__TList(
                aver_rt::AverList::empty(),
            ),
        );
        crate::proof_kernel::aver_generated::kernel::term::bind(v.clone(), __arg17)
    }]);
    {
        let __arg19 @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(
            claim.clone(),
            &atNil,
            path.clone(),
        )?;
        {
            let __arg20 @ _ = {
                let __arg18 @ _ = crate::proof_kernel::aver_generated::kernel::check::hypsAt(
                    &env.hyps,
                    &atNil,
                    path.clone(),
                )?;
                crate::proof_kernel::aver_generated::kernel::check::withHyps(env, &__arg18)
            };
            {
                let __arg21 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!("/nil"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::caseProves(
                    n, &__arg19, __arg20, __arg21,
                )
            }
        }
    }?;
    let atCell @ _ = aver_rt::AverList::from_vec(vec![{
        let __arg22 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
            crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                ::aver_rt::str_lit!("List.prepend"),
                aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TVar(h.clone()),
                    ),
                    crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TVar(t.clone()),
                    ),
                ]),
            ),
        );
        crate::proof_kernel::aver_generated::kernel::term::bind(v.clone(), __arg22)
    }]);
    let inCell @ _ = {
        let __arg24 @ _ = {
            let __arg23 @ _ = crate::proof_kernel::aver_generated::kernel::check::hypsAt(
                &env.hyps,
                &atCell,
                path.clone(),
            )?;
            crate::proof_kernel::aver_generated::kernel::check::withHyps(env, &__arg23)
        };
        {
            let __arg25 @ _ =
                crate::proof_kernel::aver_generated::kernel::check::cellVars(h, t.clone(), e);
            crate::proof_kernel::aver_generated::kernel::check::withVars(&__arg24, &__arg25)
        }
    };
    {
        let __arg28 @ _ = crate::proof_kernel::aver_generated::kernel::check::instantiate(
            claim.clone(),
            &atCell,
            path.clone(),
        )?;
        {
            let __arg29 @ _ = {
                let __arg27 @ _ = aver_rt::AverList::concat(
                    &{
                        let __arg26 @ _ =
                            crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TVar(t),
                            );
                        crate::proof_kernel::aver_generated::kernel::check::smallerHyps(
                            v,
                            __arg26,
                            general,
                            ihs,
                            claim.clone(),
                            carried,
                            &inCell,
                            path.clone(),
                        )
                    }?,
                    &inCell.hyps.clone(),
                );
                crate::proof_kernel::aver_generated::kernel::check::withHyps(&inCell, &__arg27)
            };
            {
                let __arg30 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(21)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(path))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!("/cons"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::caseProves(
                    c, &__arg28, __arg29, __arg30,
                )
            }
        }
    }?;
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
    {
        let __subject = (
            crate::proof_kernel::aver_generated::kernel::check::listVar(v.clone(), env),
            {
                let __arg9 @ _ = aver_rt::AverList::from_vec(vec![h.clone(), t.clone()]);
                {
                    let __arg10 @ _ = aver_rt::AverList::prepend_owned(
                        v.clone(),
                        aver_rt::AverList::concat(
                            &crate::proof_kernel::aver_generated::kernel::check::takenNames(
                                &claim, &env.hyps,
                            ),
                            &crate::proof_kernel::aver_generated::kernel::check::hypLabels(
                                &env.hyps,
                            ),
                        ),
                    );
                    {
                        let __arg11 @ _ = <aver_rt::AverList<AverStr>>::empty();
                        crate::proof_kernel::aver_generated::kernel::check::freshAll(
                            &__arg9, &__arg10, env, &__arg11,
                        )
                    }
                }
            },
        );
        match __subject {
            (None, _) => {
                let __arg12 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" is not a variable of list type"));
                    __b
                });
                crate::proof_kernel::aver_generated::kernel::check::refuse(path, __arg12)
            }
            (_, false) => crate::proof_kernel::aver_generated::kernel::check::refuse(
                path,
                ::aver_rt::str_lit!("the names are not fresh"),
            ),
            (Some(e), true) => {
                let __arg13 @ _ = <aver_rt::AverList<
                    crate::proof_kernel::aver_generated::kernel::proof::Hyp,
                >>::empty();
                {
                    let __arg14 @ _ = <aver_rt::AverList<AverStr>>::empty();
                    {
                        let __arg15 @ _ = <aver_rt::AverList<
                            crate::proof_kernel::aver_generated::kernel::proof::IhAt,
                        >>::empty();
                        crate::proof_kernel::aver_generated::kernel::check::listCases(
                            v, e, claim, &__arg13, n, h, t, &__arg14, &__arg15, c, env, path,
                        )
                    }
                }
            }
        }
    }
}

/// The names of the hypotheses in scope.
#[inline(always)]
pub fn hypLabels(
    hs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = hs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    x.name.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::hypLabels(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// No definitions, laws or hypotheses, and these givens of type Int.
#[inline(always)]
pub fn intsEnv(ns @ _: &aver_rt::AverList<AverStr>) -> Env {
    crate::proof_kernel::cancel_checkpoint();
    let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::check::emptyEnv();
    let __arg3 @ _ = {
        let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt;
        crate::proof_kernel::aver_generated::kernel::check::typedAll(ns, &__arg1)
    };
    crate::proof_kernel::aver_generated::kernel::check::withVars(&__arg2, &__arg3)
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
    {
        let __list_subject = ihs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(i), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let __subject = (
                        (aver_rt::AverInt::from_i64(i.at.len() as i64)
                            == aver_rt::AverInt::from_i64(general.len() as i64)),
                        (aver_rt::AverInt::from_i64(i.carry.len() as i64)
                            == aver_rt::AverInt::from_i64(cs.len() as i64)),
                    );
                    match __subject {
                        (false, _) => {
                            Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b = {
                                                            let mut __b = {
                                                                let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(109)).to_usize().unwrap_or(0));
                                                                __b.push_str(&::aver_rt::str_lit!(
                                                                    "step "
                                                                ));
                                                                __b
                                                            };
                                                            __b.push_str(&aver_rt::AverStr::from(
                                                                aver_rt::aver_display(&(path)),
                                                            ));
                                                            __b
                                                        };
                                                        __b.push_str(&::aver_rt::str_lit!(": "));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(i.name)),
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&::aver_rt::str_lit!(" gives "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(aver_rt::AverInt::from_i64(i.at.len() as i64)))));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" values for "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(aver_rt::AverInt::from_i64(general.len() as i64)),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" generalised givens"));
                                __b
                            }))
                        }
                        (_, false) => Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(55)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("step "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(path),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(
                                ": one proof per carried hypothesis"
                            ));
                            __b
                        })),
                        _ => Ok(aver_rt::AverList::concat(
                            &crate::proof_kernel::aver_generated::kernel::check::smallerHyps(
                                v.clone(),
                                smaller.clone(),
                                general,
                                &rest,
                                claim.clone(),
                                cs,
                                scope,
                                path.clone(),
                            )?,
                            &{
                                let __arg10 @ _ = aver_rt::AverList::prepend_owned(
                                    crate::proof_kernel::aver_generated::kernel::term::bind(
                                        v, smaller,
                                    ),
                                    crate::proof_kernel::aver_generated::kernel::check::zipBind(
                                        general, &i.at,
                                    ),
                                );
                                crate::proof_kernel::aver_generated::kernel::check::ihHolds(
                                    &__arg10,
                                    i.name.clone(),
                                    &i.carry,
                                    claim,
                                    cs,
                                    scope,
                                    path,
                                )
                            }?,
                        )),
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The hypotheses of these names, as they stand in scope.
#[inline(always)]
pub fn namedHyps(
    names @ _: &aver_rt::AverList<AverStr>,
    scope @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>,
    path @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = names;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::proof::Hyp {
                        name: n.clone(),
                        eqn: crate::proof_kernel::aver_generated::kernel::check::hyp(
                            n,
                            scope,
                            path.clone(),
                        )?,
                    },
                    crate::proof_kernel::aver_generated::kernel::check::namedHyps(
                        &rest, scope, path,
                    )?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The statements of checked facts, as laws a step may cite.
#[inline(always)]
pub fn factLaws(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = fs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    f.law.clone(),
                    crate::proof_kernel::aver_generated::kernel::check::factLaws(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}
