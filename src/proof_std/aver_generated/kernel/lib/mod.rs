#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Rw {
    pub to: crate::proof_std::aver_generated::kernel::term::Term,
    pub proof: crate::proof_std::aver_generated::kernel::proof::Proof,
}

impl PartialOrd for Rw {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Rw {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.proof.cmp(&other.proof))
            .then_with(|| self.to.cmp(&other.to))
    }
}

impl aver_rt::AverDisplay for Rw {
    fn aver_display(&self) -> String {
        format!(
            "Rw({})",
            vec![
                format!("to: {}", self.to.aver_display_inner()),
                format!("proof: {}", self.proof.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Known {
    pub hyp: AverStr,
    pub name: AverStr,
    pub value: crate::proof_std::aver_generated::kernel::term::Term,
}

impl PartialOrd for Known {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Known {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.hyp.cmp(&other.hyp))
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.value.cmp(&other.value))
    }
}

impl aver_rt::AverDisplay for Known {
    fn aver_display(&self) -> String {
        format!(
            "Known({})",
            vec![
                format!("hyp: {}", self.hyp.aver_display_inner()),
                format!("name: {}", self.name.aver_display_inner()),
                format!("value: {}", self.value.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Scope {
    pub defs: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Def>,
    pub known: aver_rt::AverList<Known>,
}

impl PartialOrd for Scope {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Scope {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.defs.cmp(&other.defs))
            .then_with(|| self.known.cmp(&other.known))
    }
}

impl aver_rt::AverDisplay for Scope {
    fn aver_display(&self) -> String {
        format!(
            "Scope({})",
            vec![
                format!("defs: {}", self.defs.aver_display_inner()),
                format!("known: {}", self.known.aver_display_inner())
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
    SelectFrom(
        aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
        crate::proof_std::aver_generated::kernel::term::Term,
        aver_rt::AverInt,
    ),
    SelectOne(
        crate::proof_std::aver_generated::kernel::term::Arm,
        aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
        crate::proof_std::aver_generated::kernel::term::Term,
        aver_rt::AverInt,
        AverStr,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
) -> Option<(
    aver_rt::AverInt,
    crate::proof_std::aver_generated::kernel::term::Arm,
    aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
)> {
    loop {
        __state = match __state {
            __MutualTco1::SelectFrom(mut arms @ _, mut value @ _, mut k @ _) => {
                crate::proof_std::cancel_checkpoint();
                aver_list_match!(arms, [] => { return None }, [a, rest] => __MutualTco1::SelectOne(a, rest, value.clone(), k, crate::proof_std::aver_generated::kernel::lib::shapeOf(&value)))
            }
            __MutualTco1::SelectOne(
                mut a @ _,
                mut rest @ _,
                mut v @ _,
                mut k @ _,
                mut shape @ _,
            ) => {
                crate::proof_std::cancel_checkpoint();
                match a.pattern.clone() {
                    crate::proof_std::aver_generated::kernel::term::Pat::PWild => {
                        return Some((k, a, aver_rt::AverList::from_vec(vec![v])));
                    }
                    crate::proof_std::aver_generated::kernel::term::Pat::PVar(n) => {
                        return Some((k, a, aver_rt::AverList::from_vec(vec![v])));
                    }
                    crate::proof_std::aver_generated::kernel::term::Pat::PLit(x) => {
                        if (x == v) {
                            return Some((k, a, aver_rt::AverList::empty()));
                        } else {
                            if (crate::proof_std::aver_generated::kernel::lib::isLiteral(&x)
                                && (&*shape == "lit"))
                            {
                                __MutualTco1::SelectFrom(
                                    rest,
                                    v,
                                    k.add(&aver_rt::AverInt::from_i64(1)),
                                )
                            } else {
                                return None;
                            }
                        }
                    }
                    crate::proof_std::aver_generated::kernel::term::Pat::PNil => {
                        let __dispatch_subject = shape;
                        if &*__dispatch_subject == "nil" {
                            return Some((k, a, aver_rt::AverList::empty()));
                        } else {
                            if &*__dispatch_subject == "cons" {
                                __MutualTco1::SelectFrom(
                                    rest,
                                    v,
                                    k.add(&aver_rt::AverInt::from_i64(1)),
                                )
                            } else {
                                return None;
                            }
                        }
                    }
                    crate::proof_std::aver_generated::kernel::term::Pat::PCons(h, t) => {
                        let __dispatch_subject = shape;
                        if &*__dispatch_subject == "cons" {
                            return Some((
                                k,
                                a,
                                crate::proof_std::aver_generated::kernel::lib::cellParts(&v),
                            ));
                        } else {
                            if &*__dispatch_subject == "nil" {
                                __MutualTco1::SelectFrom(
                                    rest,
                                    v,
                                    k.add(&aver_rt::AverInt::from_i64(1)),
                                )
                            } else {
                                return None;
                            }
                        }
                    }
                    crate::proof_std::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
                        match v.clone() {
                            crate::proof_std::aver_generated::kernel::term::Term::TCtor(d, ys) => {
                                if (c == d) {
                                    return Some((k, a, ys));
                                } else {
                                    __MutualTco1::SelectFrom(
                                        rest,
                                        v,
                                        k.add(&aver_rt::AverInt::from_i64(1)),
                                    )
                                }
                            }
                            _ => return None,
                        }
                    }
                    _ => return None,
                }
            }
        };
    }
}

/// selectArm from arm number k on.
pub fn selectFrom(
    arms @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
    value @ _: crate::proof_std::aver_generated::kernel::term::Term,
    k @ _: aver_rt::AverInt,
) -> Option<(
    aver_rt::AverInt,
    crate::proof_std::aver_generated::kernel::term::Arm,
    aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
)> {
    __mutual_tco_trampoline_1(__MutualTco1::SelectFrom(arms, value, k))
}

/// Arm a when it decides v, the arms after it when it rules v out, none otherwise.
pub fn selectOne(
    a @ _: crate::proof_std::aver_generated::kernel::term::Arm,
    rest @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
    v @ _: crate::proof_std::aver_generated::kernel::term::Term,
    k @ _: aver_rt::AverInt,
    shape @ _: AverStr,
) -> Option<(
    aver_rt::AverInt,
    crate::proof_std::aver_generated::kernel::term::Arm,
    aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
)> {
    __mutual_tco_trampoline_1(__MutualTco1::SelectOne(a, rest, v, k, shape))
}

/// How deep evaluate nests before it gives up: a bound against a definition that does not shrink.
pub fn evalDepth() -> aver_rt::AverInt {
    crate::proof_std::cancel_checkpoint();
    aver_rt::AverInt::from_i64(400)
}

/// The definitions of a goal and no case hypotheses yet.
pub fn scope(goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal) -> Scope {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::lib::Scope {
        defs: goal.defs.clone(),
        known: aver_rt::AverList::empty(),
    }
}

/// One more case hypothesis, named hyp, stating that the variable name equals value; evaluate reads name as value.
pub fn assume(
    s @ _: &Scope,
    hyp @ _: AverStr,
    name @ _: AverStr,
    value @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Scope {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::lib::Scope {
        defs: s.defs.clone(),
        known: aver_rt::AverList::prepend(
            crate::proof_std::aver_generated::kernel::lib::Known {
                hyp: hyp,
                name: name,
                value: value.clone(),
            },
            &s.known.clone(),
        ),
    }
}

/// t rewritten to itself, by no step.
pub fn refl(t @ _: &crate::proof_std::aver_generated::kernel::term::Term) -> Rw {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::lib::Rw {
        to: t.clone(),
        proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRefl(t.clone()),
    }
}

/// Whether a proof is a bare reflexivity.
pub fn isRefl(p @ _: &crate::proof_std::aver_generated::kernel::proof::Proof) -> bool {
    crate::proof_std::cancel_checkpoint();
    match p.clone() {
        crate::proof_std::aver_generated::kernel::proof::Proof::PRefl(t) => true,
        _ => false,
    }
}

/// t = a.to = b.to, where a starts at t and b at a.to. A reflexive link is dropped, so the chain never carries one.
pub fn chain(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    a @ _: &Rw,
    b @ _: &Rw,
) -> Rw {
    crate::proof_std::cancel_checkpoint();
    match (
        crate::proof_std::aver_generated::kernel::lib::isRefl(&a.proof),
        crate::proof_std::aver_generated::kernel::lib::isRefl(&b.proof),
    ) {
        (true, _) => b.clone(),
        (false, true) => crate::proof_std::aver_generated::kernel::lib::Rw {
            to: a.to.clone(),
            proof: a.proof.clone(),
        },
        (false, false) => crate::proof_std::aver_generated::kernel::lib::Rw {
            to: b.to.clone(),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PTrans(
                aver_rt::AverList::from_vec(vec![t.clone(), a.to.clone(), b.to.clone()]),
                aver_rt::AverList::from_vec(vec![a.proof.clone(), b.proof.clone()]),
            ),
        },
    }
}

/// terms[0] = terms[n] from n proofs, proof k of terms[k] = terms[k + 1]: the kernel's chain lists one term more than it has steps.
#[inline(always)]
pub fn trans(
    terms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    proofs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Proof>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    if (aver_rt::AverInt::from_i64(terms.len() as i64)
        == aver_rt::AverInt::from_i64(proofs.len() as i64).add(&aver_rt::AverInt::from_i64(1)))
    {
        {
            let __list_subject = proofs.clone();
            if let Some((p, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
                {
                    let __list_subject = __pat0;
                    if __list_subject.is_empty() {
                        Ok(p)
                    } else {
                        Ok(
                            crate::proof_std::aver_generated::kernel::proof::Proof::PTrans(
                                terms.clone(),
                                proofs.clone(),
                            ),
                        )
                    }
                }
            } else {
                Ok(
                    crate::proof_std::aver_generated::kernel::proof::Proof::PTrans(
                        terms.clone(),
                        proofs.clone(),
                    ),
                )
            }
        }
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(84)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("a chain of "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(aver_rt::AverInt::from_i64(proofs.len() as i64)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" steps lists "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(aver_rt::AverInt::from_i64(proofs.len() as i64)
                            .add(&aver_rt::AverInt::from_i64(1))),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(" terms, not "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(aver_rt::AverInt::from_i64(terms.len() as i64)),
            )));
            __b
        }))
    }
}

/// The name of the hypothesis that holds the law's `when`.
pub fn whenName() -> AverStr {
    crate::proof_std::cancel_checkpoint();
    AverStr::from("when")
}

/// The law's `when`, if it has one. Several `when` lines arrive joined by Bool.and.
#[inline(always)]
pub fn whenOf(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::proof::premiseOf(&goal.obligation)
}

/// The proof of `premise = true` for the law's `when`.
pub fn whenHolds() -> crate::proof_std::aver_generated::kernel::proof::Proof {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(AverStr::from("when"))
}

/// The goal's claim in a case where the `when` evaluates to false: true = when = … = false, so the case cannot happen.
#[inline(always)]
pub fn absurdByWhen(
    s @ _: &Scope,
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::whenOf(goal) {
        None => Err(AverStr::from("the law has no `when`")),
        Some(p @ _) => crate::proof_std::aver_generated::kernel::lib::whenRefutes(
            s,
            &p,
            &goal.obligation.lhs,
            &goal.obligation.rhs,
        ),
    }
}

/// lhs = rhs in a case where the `when`, as it reads in that case, evaluates to false: true = premise = … = false, so the case cannot happen. Inside a list split the `when` reads with the split variable replaced, so pass it that way.
pub fn whenRefutes(
    s @ _: &Scope,
    premise @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    lhs @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    rhs @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let r @ _ = crate::proof_std::aver_generated::kernel::lib::evaluate(s, premise)?;
    match r.to {
        crate::proof_std::aver_generated::kernel::term::Term::TBool(__pat0) => match __pat0 {
            false => Ok(
                crate::proof_std::aver_generated::kernel::proof::Proof::PAbsurd(
                    std::sync::Arc::new(
                        crate::proof_std::aver_generated::kernel::proof::Proof::PTrans(
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_std::aver_generated::kernel::term::Term::TBool(true),
                                premise.clone(),
                                crate::proof_std::aver_generated::kernel::term::Term::TBool(false),
                            ]),
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_std::aver_generated::kernel::proof::Proof::PSymm(
                                    std::sync::Arc::new(
                                        crate::proof_std::aver_generated::kernel::lib::whenHolds(),
                                    ),
                                ),
                                r.proof,
                            ]),
                        ),
                    ),
                    lhs.clone(),
                    rhs.clone(),
                ),
            ),
            _ => Err(AverStr::from("the `when` does not evaluate to false here")),
        },
        _ => Err(AverStr::from("the `when` does not evaluate to false here")),
    }
}

/// t with the variable name replaced by value, as the kernel replaces a split variable in a list split's cases.
#[inline(always)]
pub fn substitute(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    name @ _: AverStr,
    value @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<crate::proof_std::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::subst::subst(
        t,
        &aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(
            name, value,
        )]),
    )
}

/// lhs = n = rhs when evaluate takes both sides to the same n.
pub fn bothSides(
    s @ _: &Scope,
    lhs @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    rhs @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let l @ _ = crate::proof_std::aver_generated::kernel::lib::evaluate(s, lhs)?;
    let r @ _ = crate::proof_std::aver_generated::kernel::lib::evaluate(s, rhs)?;
    if (l.to == r.to) {
        crate::proof_std::aver_generated::kernel::lib::trans(
            &aver_rt::AverList::from_vec(vec![lhs.clone(), l.to.clone(), rhs.clone()]),
            &aver_rt::AverList::from_vec(vec![
                l.proof,
                crate::proof_std::aver_generated::kernel::proof::Proof::PSymm(std::sync::Arc::new(
                    r.proof,
                )),
            ]),
        )
    } else {
        Err(AverStr::from("the two sides evaluate to different terms"))
    }
}

/// t evaluated as far as the case hypotheses, list cells, the List.len and List.take equations, computation on literals and unfolding at a decided arm allow.
#[inline(always)]
pub fn evaluate(
    s @ _: &Scope,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::lib::evalWithin(s, t, aver_rt::AverInt::from_i64(400))
}

/// evaluate, nested at most depth deep.
#[inline(always)]
pub fn evalWithin(
    s @ _: &Scope,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    if (depth > aver_rt::AverInt::from_i64(0)) {
        crate::proof_std::aver_generated::kernel::lib::evalNode(
            s,
            t,
            depth.sub(&aver_rt::AverInt::from_i64(1)),
        )
    } else {
        Err(AverStr::from("evaluation nested too deep"))
    }
}

/// One node: its parts first, then one step at its head.
pub fn evalNode(
    s @ _: &Scope,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match t.clone() {
        crate::proof_std::aver_generated::kernel::term::Term::TVar(x) => {
            crate::proof_std::aver_generated::kernel::lib::evalVar(
                s,
                t,
                &crate::proof_std::aver_generated::kernel::lib::lookupKnown(x, s.known.clone()),
                depth,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TList(__pat0) => {
            let __list_subject = __pat0;
            if let Some((x, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
                Ok(crate::proof_std::aver_generated::kernel::lib::chain(
                    t,
                    &crate::proof_std::aver_generated::kernel::lib::Rw {
                        to: crate::proof_std::aver_generated::kernel::lib::cons(
                            &x,
                            &crate::proof_std::aver_generated::kernel::term::Term::TList(
                                rest.clone(),
                            ),
                        ),
                        proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCell(
                            t.clone(),
                        ),
                    },
                    &crate::proof_std::aver_generated::kernel::lib::evalWithin(
                        s,
                        &crate::proof_std::aver_generated::kernel::lib::cons(
                            &x,
                            &crate::proof_std::aver_generated::kernel::term::Term::TList(rest),
                        ),
                        depth,
                    )?,
                ))
            } else {
                Ok(crate::proof_std::aver_generated::kernel::lib::refl(t))
            }
        }
        crate::proof_std::aver_generated::kernel::term::Term::TBi(name, args) => {
            crate::proof_std::aver_generated::kernel::lib::afterArgs(
                s,
                t,
                &crate::proof_std::aver_generated::kernel::lib::evalArgs(
                    s,
                    AverStr::from("bi"),
                    name,
                    &aver_rt::AverList::empty(),
                    &args,
                    depth.clone(),
                )?,
                depth,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TCall(f, args) => {
            crate::proof_std::aver_generated::kernel::lib::afterArgs(
                s,
                t,
                &crate::proof_std::aver_generated::kernel::lib::evalArgs(
                    s,
                    AverStr::from("call"),
                    f,
                    &aver_rt::AverList::empty(),
                    &args,
                    depth.clone(),
                )?,
                depth,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TOp(o, a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::proof_std::aver_generated::kernel::lib::evalOp(s, o, &a, &b, depth)
        }
        crate::proof_std::aver_generated::kernel::term::Term::TMatch(subject, arms) => {
            let subject = (*subject).clone();
            crate::proof_std::aver_generated::kernel::lib::evalMatch(s, t, &subject, &arms, depth)
        }
        crate::proof_std::aver_generated::kernel::term::Term::TGet(r, f) => {
            let r = (*r).clone();
            crate::proof_std::aver_generated::kernel::lib::evalGet(s, t, &r, f, depth)
        }
        _ => Ok(crate::proof_std::aver_generated::kernel::lib::refl(t)),
    }
}

/// A field read: the record evaluated in place, then the field of a record literal by projection.
pub fn evalGet(
    s @ _: &Scope,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    r @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    f @ _: AverStr,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let rr @ _ = crate::proof_std::aver_generated::kernel::lib::evalWithin(s, r, depth.clone())?;
    let inner @ _ = if crate::proof_std::aver_generated::kernel::lib::isRefl(&rr.proof) {
        crate::proof_std::aver_generated::kernel::lib::refl(t)
    } else {
        crate::proof_std::aver_generated::kernel::lib::Rw {
            to: crate::proof_std::aver_generated::kernel::term::Term::TGet(
                std::sync::Arc::new(rr.to.clone()),
                f.clone(),
            ),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
                crate::proof_std::aver_generated::kernel::term::Term::TGet(
                    std::sync::Arc::new(
                        crate::proof_std::aver_generated::kernel::term::Term::THole,
                    ),
                    f.clone(),
                ),
                std::sync::Arc::new(rr.proof),
            ),
        }
    };
    match rr.to {
        crate::proof_std::aver_generated::kernel::term::Term::TRec(ty, fs) => {
            crate::proof_std::aver_generated::kernel::lib::projected(
                s,
                t,
                &inner,
                &crate::proof_std::aver_generated::kernel::lib::fieldOf(fs, f),
                depth,
            )
        }
        _ => Ok(inner),
    }
}

/// t = the read of the literal = its field, evaluated further.
#[inline(always)]
pub fn projected(
    s @ _: &Scope,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    inner @ _: &Rw,
    v @ _: &Option<crate::proof_std::aver_generated::kernel::term::Term>,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match v.clone() {
        None => Ok(inner.clone()),
        Some(x @ _) => Ok(crate::proof_std::aver_generated::kernel::lib::chain(
            t,
            inner,
            &crate::proof_std::aver_generated::kernel::lib::chain(
                &inner.to,
                &crate::proof_std::aver_generated::kernel::lib::Rw {
                    to: x.clone(),
                    proof: crate::proof_std::aver_generated::kernel::proof::Proof::PProj(
                        inner.to.clone(),
                    ),
                },
                &crate::proof_std::aver_generated::kernel::lib::evalWithin(s, &x, depth)?,
            ),
        )),
    }
}

/// A record literal's field by name.
#[inline(always)]
pub fn fieldOf(
    mut fs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
    mut f @ _: AverStr,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(fs, [] => { return None; }, [x, rest] => { if (x.name == f) { return Some(x.value); } else { {
            let __tco0 = rest;
            fs = __tco0;
            continue;
        } } })
    }
}

/// A match term: its subject evaluated, then the arm that value decides, evaluated further.
pub fn evalMatch(
    s @ _: &Scope,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    subject @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    arms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let r @ _ =
        crate::proof_std::aver_generated::kernel::lib::evalWithin(s, subject, depth.clone())?;
    match crate::proof_std::aver_generated::kernel::lib::selectArm(arms, r.to) {
        None => Ok(crate::proof_std::aver_generated::kernel::lib::refl(t)),
        Some(__pat0 @ _) => {
            let (k, arm, parts) = __pat0;
            crate::proof_std::aver_generated::kernel::lib::evalArm(
                s,
                t,
                &crate::proof_std::aver_generated::kernel::subst::subst(
                    &arm.body,
                    &crate::proof_std::aver_generated::kernel::lib::patternBinds(
                        &arm.pattern,
                        &parts,
                    ),
                )?,
                &crate::proof_std::aver_generated::kernel::proof::Proof::PArm(
                    k,
                    parts,
                    t.clone(),
                    std::sync::Arc::new(r.proof),
                ),
                depth,
            )
        }
    }
}

/// t = body by the arm step, then body evaluated.
pub fn evalArm(
    s @ _: &Scope,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    body @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    step @ _: &crate::proof_std::aver_generated::kernel::proof::Proof,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    Ok(crate::proof_std::aver_generated::kernel::lib::chain(
        t,
        &crate::proof_std::aver_generated::kernel::lib::Rw {
            to: body.clone(),
            proof: step.clone(),
        },
        &crate::proof_std::aver_generated::kernel::lib::evalWithin(s, body, depth)?,
    ))
}

/// A variable with a case hypothesis is its value, evaluated further.
#[inline(always)]
pub fn evalVar(
    s @ _: &Scope,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    known @ _: &Option<Known>,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match known.clone() {
        None => Ok(crate::proof_std::aver_generated::kernel::lib::refl(t)),
        Some(k @ _) => Ok(crate::proof_std::aver_generated::kernel::lib::chain(
            t,
            &crate::proof_std::aver_generated::kernel::lib::Rw {
                to: k.value.clone(),
                proof: crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(k.hyp),
            },
            &crate::proof_std::aver_generated::kernel::lib::evalWithin(s, &k.value, depth)?,
        )),
    }
}

/// x in front of xs, as the kernel writes a list cell.
pub fn cons(
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    xs @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> crate::proof_std::aver_generated::kernel::term::Term {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::term::Term::TBi(
        AverStr::from("List.prepend"),
        aver_rt::AverList::from_vec(vec![x.clone(), xs.clone()]),
    )
}

/// The latest case hypothesis about x.
#[inline(always)]
pub fn lookupKnown(mut x @ _: AverStr, mut ks @ _: aver_rt::AverList<Known>) -> Option<Known> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(ks, [] => { return None; }, [k, rest] => { if (k.name == x) { return Some(k); } else { {
            let __tco1 = rest;
            ks = __tco1;
            continue;
        } } })
    }
}

/// A call of a definition or an application of a builtin.
#[inline(always)]
pub fn rebuild(
    kind @ _: AverStr,
    name @ _: AverStr,
    xs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> crate::proof_std::aver_generated::kernel::term::Term {
    crate::proof_std::cancel_checkpoint();
    if (&*kind == "call") {
        crate::proof_std::aver_generated::kernel::term::Term::TCall(name, xs.clone())
    } else {
        crate::proof_std::aver_generated::kernel::term::Term::TBi(name, xs.clone())
    }
}

/// Each argument in turn, rewritten in place; the result starts at the application of done then todo.
#[inline(always)]
pub fn evalArgs(
    s @ _: &Scope,
    kind @ _: AverStr,
    name @ _: AverStr,
    done @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    todo @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(todo.clone(), [] => Ok(crate::proof_std::aver_generated::kernel::lib::refl(&crate::proof_std::aver_generated::kernel::lib::rebuild(kind, name, done))), [x, rest] => crate::proof_std::aver_generated::kernel::lib::evalArg(s, kind, name, done, &x.clone(), &rest, &crate::proof_std::aver_generated::kernel::lib::evalWithin(s, &x, depth.clone())?, depth))
}

/// One argument rewritten in place by congruence, then the arguments after it.
pub fn evalArg(
    s @ _: &Scope,
    kind @ _: AverStr,
    name @ _: AverStr,
    done @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    rest @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    r @ _: &Rw,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let here @ _ = crate::proof_std::aver_generated::kernel::lib::rebuild(
        kind.clone(),
        name.clone(),
        &aver_rt::AverList::concat(
            &done.clone(),
            &aver_rt::AverList::prepend(x.clone(), &rest.clone()),
        ),
    );
    let step @ _ = if crate::proof_std::aver_generated::kernel::lib::isRefl(&r.proof) {
        crate::proof_std::aver_generated::kernel::lib::refl(&here)
    } else {
        crate::proof_std::aver_generated::kernel::lib::Rw {
            to: crate::proof_std::aver_generated::kernel::lib::rebuild(
                kind.clone(),
                name.clone(),
                &aver_rt::AverList::concat(
                    &done.clone(),
                    &aver_rt::AverList::prepend(r.to.clone(), &rest.clone()),
                ),
            ),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
                crate::proof_std::aver_generated::kernel::lib::rebuild(
                    kind.clone(),
                    name.clone(),
                    &aver_rt::AverList::concat(
                        &done.clone(),
                        &aver_rt::AverList::prepend(
                            crate::proof_std::aver_generated::kernel::term::Term::THole,
                            &rest.clone(),
                        ),
                    ),
                ),
                std::sync::Arc::new(r.proof.clone()),
            ),
        }
    };
    Ok(crate::proof_std::aver_generated::kernel::lib::chain(
        &here,
        &step,
        &crate::proof_std::aver_generated::kernel::lib::evalArgs(
            s,
            kind,
            name,
            &aver_rt::AverList::concat(
                &done.clone(),
                &aver_rt::AverList::from_vec(vec![r.to.clone()]),
            ),
            rest,
            depth,
        )?,
    ))
}

/// With the arguments evaluated, one step at the head and the evaluation of what it gives.
#[inline(always)]
pub fn afterArgs(
    s @ _: &Scope,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    args @ _: &Rw,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::headStep(s, &args.to, depth.clone())? {
        None => Ok(args.clone()),
        Some(step @ _) => Ok(crate::proof_std::aver_generated::kernel::lib::chain(
            t,
            args,
            &crate::proof_std::aver_generated::kernel::lib::chain(
                &args.to,
                &step.clone(),
                &crate::proof_std::aver_generated::kernel::lib::evalWithin(s, &step.to, depth)?,
            ),
        )),
    }
}

/// Both operands rewritten in place, then computed once both are literals.
pub fn evalOp(
    s @ _: &Scope,
    o @ _: AverStr,
    a @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    b @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let ra @ _ = crate::proof_std::aver_generated::kernel::lib::evalWithin(s, a, depth.clone())?;
    let rb @ _ = crate::proof_std::aver_generated::kernel::lib::evalWithin(s, b, depth)?;
    let t @ _ = crate::proof_std::aver_generated::kernel::term::Term::TOp(
        o.clone(),
        std::sync::Arc::new(a.clone()),
        std::sync::Arc::new(b.clone()),
    );
    let mid @ _ = crate::proof_std::aver_generated::kernel::term::Term::TOp(
        o.clone(),
        std::sync::Arc::new(ra.to.clone()),
        std::sync::Arc::new(b.clone()),
    );
    let end @ _ = crate::proof_std::aver_generated::kernel::term::Term::TOp(
        o.clone(),
        std::sync::Arc::new(ra.to.clone()),
        std::sync::Arc::new(rb.to),
    );
    let first @ _ = if crate::proof_std::aver_generated::kernel::lib::isRefl(&ra.proof) {
        crate::proof_std::aver_generated::kernel::lib::refl(&t)
    } else {
        crate::proof_std::aver_generated::kernel::lib::Rw {
            to: mid.clone(),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
                crate::proof_std::aver_generated::kernel::term::Term::TOp(
                    o.clone(),
                    std::sync::Arc::new(
                        crate::proof_std::aver_generated::kernel::term::Term::THole,
                    ),
                    std::sync::Arc::new(b.clone()),
                ),
                std::sync::Arc::new(ra.proof),
            ),
        }
    };
    let second @ _ = if crate::proof_std::aver_generated::kernel::lib::isRefl(&rb.proof) {
        crate::proof_std::aver_generated::kernel::lib::refl(&mid)
    } else {
        crate::proof_std::aver_generated::kernel::lib::Rw {
            to: end.clone(),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
                crate::proof_std::aver_generated::kernel::term::Term::TOp(
                    o,
                    std::sync::Arc::new(ra.to),
                    std::sync::Arc::new(
                        crate::proof_std::aver_generated::kernel::term::Term::THole,
                    ),
                ),
                std::sync::Arc::new(rb.proof),
            ),
        }
    };
    Ok(crate::proof_std::aver_generated::kernel::lib::chain(
        &t,
        &crate::proof_std::aver_generated::kernel::lib::chain(&t, &first, &second),
        &crate::proof_std::aver_generated::kernel::lib::compute(&end),
    ))
}

/// An Int operator between two Int literals, by computation; any other term is left as it is, since the kernel computes only closed literals.
#[inline(always)]
pub fn compute(t @ _: &crate::proof_std::aver_generated::kernel::term::Term) -> Rw {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::literalValue(t) {
        Some(v @ _) => crate::proof_std::aver_generated::kernel::lib::Rw {
            to: v.clone(),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(t.clone(), v),
        },
        None => crate::proof_std::aver_generated::kernel::lib::refl(t),
    }
}

/// The value of a closed Int or Bool term, by the kernel's own evaluator for the compute rule (not a copy of it).
pub fn literalValue(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match t.clone() {
        crate::proof_std::aver_generated::kernel::term::Term::TOp(o, __pat0, __pat1) => {
            let __pat0 = (*__pat0).clone();
            let __pat1 = (*__pat1).clone();
            match __pat0 {
                crate::proof_std::aver_generated::kernel::term::Term::TInt(__pat2) => {
                    match __pat1 {
                        crate::proof_std::aver_generated::kernel::term::Term::TInt(__pat3) => {
                            crate::proof_std::aver_generated::kernel::lib::intOp(o, __pat2, __pat3)
                        }
                        _ => crate::proof_std::aver_generated::kernel::lib::scalarOf(
                            &crate::proof_std::aver_generated::kernel::eval::evalClosed(t),
                        ),
                    }
                }
                _ => crate::proof_std::aver_generated::kernel::lib::scalarOf(
                    &crate::proof_std::aver_generated::kernel::eval::evalClosed(t),
                ),
            }
        }
        crate::proof_std::aver_generated::kernel::term::Term::TBi(n, xs) => {
            crate::proof_std::aver_generated::kernel::lib::scalarOf(
                &crate::proof_std::aver_generated::kernel::eval::evalClosed(t),
            )
        }
        _ => None,
    }
}

/// An Int or Bool value as a literal; a list value is left alone, since evaluate writes lists as cells.
pub fn scalarOf(
    v @ _: &Option<crate::proof_std::aver_generated::kernel::eval::Val>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match v.clone() {
        Some(__pat0) => match __pat0 {
            crate::proof_std::aver_generated::kernel::eval::Val::VInt(n) => Some(
                crate::proof_std::aver_generated::kernel::term::Term::TInt(n),
            ),
            crate::proof_std::aver_generated::kernel::eval::Val::VBool(b) => Some(
                crate::proof_std::aver_generated::kernel::term::Term::TBool(b),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// The Int operators compute writes.
#[inline(always)]
pub fn intOp(
    o @ _: AverStr,
    x @ _: aver_rt::AverInt,
    y @ _: aver_rt::AverInt,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    {
        let __dispatch_subject = o;
        if &*__dispatch_subject == "+" {
            Some(crate::proof_std::aver_generated::kernel::term::Term::TInt(
                x.add(&y),
            ))
        } else {
            if &*__dispatch_subject == "-" {
                Some(crate::proof_std::aver_generated::kernel::term::Term::TInt(
                    x.sub(&y),
                ))
            } else {
                if &*__dispatch_subject == "*" {
                    Some(crate::proof_std::aver_generated::kernel::term::Term::TInt(
                        x.mul(&y),
                    ))
                } else {
                    if &*__dispatch_subject == "<=" {
                        Some(crate::proof_std::aver_generated::kernel::term::Term::TBool(
                            (x <= y),
                        ))
                    } else {
                        if &*__dispatch_subject == "<" {
                            Some(crate::proof_std::aver_generated::kernel::term::Term::TBool(
                                (x < y),
                            ))
                        } else {
                            if &*__dispatch_subject == ">=" {
                                Some(crate::proof_std::aver_generated::kernel::term::Term::TBool(
                                    (x >= y),
                                ))
                            } else {
                                if &*__dispatch_subject == ">" {
                                    Some(
                                        crate::proof_std::aver_generated::kernel::term::Term::TBool(
                                            (x > y),
                                        ),
                                    )
                                } else {
                                    if &*__dispatch_subject == "==" {
                                        Some(crate::proof_std::aver_generated::kernel::term::Term::TBool((x == y)))
                                    } else {
                                        if &*__dispatch_subject == "!=" {
                                            Some(crate::proof_std::aver_generated::kernel::term::Term::TBool((x != y)))
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

/// One step at the head: a list equation, or unfolding a definition at the arm its subject decides.
pub fn headStep(
    s @ _: &Scope,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    depth @ _: aver_rt::AverInt,
) -> Result<Option<Rw>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match t.clone() {
        crate::proof_std::aver_generated::kernel::term::Term::TBi(name, args) => Ok(
            crate::proof_std::aver_generated::kernel::lib::builtinStep(t, name, &args),
        ),
        crate::proof_std::aver_generated::kernel::term::Term::TCall(f, xs) => {
            crate::proof_std::aver_generated::kernel::lib::unfoldWithin(s, f, &xs, depth)
        }
        _ => Ok(None),
    }
}

/// A list equation, or, for a builtin other than List.prepend applied to closed values, its value by computation.
#[inline(always)]
pub fn builtinStep(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> Option<Rw> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::listEquation(name.clone(), args) {
        Some(r @ _) => Some(r),
        None => {
            if (&*name == "List.prepend") {
                None
            } else {
                match crate::proof_std::aver_generated::kernel::lib::literalValue(t) {
                    Some(v @ _) => Some(crate::proof_std::aver_generated::kernel::lib::Rw {
                        to: v.clone(),
                        proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(
                            t.clone(),
                            v,
                        ),
                    }),
                    None => crate::proof_std::aver_generated::kernel::lib::boolEquation(name, args),
                }
            }
        }
    }
}

/// Bool.and and Bool.or with one literal side, by the kernel's rules (both sides literal is computed instead).
pub fn boolEquation(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> Option<Rw> {
    crate::proof_std::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (name, args.clone());
        {
            let __dispatch_subject = __pat0;
            if &*__dispatch_subject == "Bool.and" {
                {
                    let __list_subject = __pat1;
                    if let Some((a, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        match a.clone() {
                            crate::proof_std::aver_generated::kernel::term::Term::TBool(__pat3) => {
                                if __pat3 {
                                    {
                                        let __list_subject = __pat2;
                                        if let Some((b, __pat4)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if __list_subject.is_empty() {
                                                    Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: b.clone(), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("bool.and.true_l"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("b"), &b)]), aver_rt::AverList::empty()) })
                                                } else {
                                                    None
                                                }
                                            }
                                        } else {
                                            None
                                        }
                                    }
                                } else {
                                    {
                                        let __list_subject = __pat2;
                                        if let Some((b, __pat5)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            {
                                                let __list_subject = __pat5;
                                                if __list_subject.is_empty() {
                                                    Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TBool(false), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("bool.and.false_l"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("b"), &b)]), aver_rt::AverList::empty()) })
                                                } else {
                                                    None
                                                }
                                            }
                                        } else {
                                            None
                                        }
                                    }
                                }
                            }
                            _ => {
                                let __list_subject = __pat2;
                                if let Some((__pat6, __pat7)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    match __pat6 {
        crate::proof_std::aver_generated::kernel::term::Term::TBool(__pat8) => {
            if __pat8 { { let __list_subject = __pat7; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: a.clone(), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("bool.and.true_r"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("a"), &a)]), aver_rt::AverList::empty()) }) } else { None } } } else { { let __list_subject = __pat7; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TBool(false), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("bool.and.false_r"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("a"), &a)]), aver_rt::AverList::empty()) }) } else { None } } }
        },
        _ => {
            None
        }
    }
                                } else {
                                    None
                                }
                            }
                        }
                    } else {
                        None
                    }
                }
            } else {
                if &*__dispatch_subject == "Bool.or" {
                    {
                        let __list_subject = __pat1;
                        if let Some((a, __pat9)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            match a.clone() {
                                crate::proof_std::aver_generated::kernel::term::Term::TBool(
                                    __pat10,
                                ) => {
                                    if __pat10 {
                                        {
                                            let __list_subject = __pat9;
                                            if let Some((b, __pat11)) =
                                                aver_rt::list_uncons_cloned(&__list_subject)
                                            {
                                                {
                                                    let __list_subject = __pat11;
                                                    if __list_subject.is_empty() {
                                                        Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TBool(true), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("bool.or.true_l"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("b"), &b)]), aver_rt::AverList::empty()) })
                                                    } else {
                                                        None
                                                    }
                                                }
                                            } else {
                                                None
                                            }
                                        }
                                    } else {
                                        {
                                            let __list_subject = __pat9;
                                            if let Some((b, __pat12)) =
                                                aver_rt::list_uncons_cloned(&__list_subject)
                                            {
                                                {
                                                    let __list_subject = __pat12;
                                                    if __list_subject.is_empty() {
                                                        Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: b.clone(), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("bool.or.false_l"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("b"), &b)]), aver_rt::AverList::empty()) })
                                                    } else {
                                                        None
                                                    }
                                                }
                                            } else {
                                                None
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    let __list_subject = __pat9;
                                    if let Some((__pat13, __pat14)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        match __pat13 {
        crate::proof_std::aver_generated::kernel::term::Term::TBool(__pat15) => {
            if __pat15 { { let __list_subject = __pat14; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TBool(true), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("bool.or.true_r"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("a"), &a)]), aver_rt::AverList::empty()) }) } else { None } } } else { { let __list_subject = __pat14; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: a.clone(), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("bool.or.false_r"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("a"), &a)]), aver_rt::AverList::empty()) }) } else { None } } }
        },
        _ => {
            None
        }
    }
                                    } else {
                                        None
                                    }
                                }
                            }
                        } else {
                            None
                        }
                    }
                } else {
                    None
                }
            }
        }
    }
}

/// The equations of List.len, List.take, List.concat and List.reverse on [] and on a cell, as the kernel's rules state them.
pub fn listEquation(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> Option<Rw> {
    crate::proof_std::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (name, args.clone());
        {
            let __dispatch_subject = __pat0;
            if &*__dispatch_subject == "List.len" {
                {
                    let __list_subject = __pat1;
                    if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        match __pat2 {
                            crate::proof_std::aver_generated::kernel::term::Term::TList(__pat4) => {
                                let __list_subject = __pat4;
                                if __list_subject.is_empty() {
                                    {
                                        let __list_subject = __pat3;
                                        if __list_subject.is_empty() {
                                            Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(0)), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("list.len.nil"), aver_rt::AverList::empty(), aver_rt::AverList::empty()) })
                                        } else {
                                            None
                                        }
                                    }
                                } else {
                                    None
                                }
                            }
                            crate::proof_std::aver_generated::kernel::term::Term::TBi(
                                __pat5,
                                __pat6,
                            ) => match &*__pat5 {
                                "List.prepend" => {
                                    let __list_subject = __pat6;
                                    if let Some((x, __pat7)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        {
                                            let __list_subject = __pat7;
                                            if let Some((a, __pat8)) =
                                                aver_rt::list_uncons_cloned(&__list_subject)
                                            {
                                                {
                                                    let __list_subject = __pat8;
                                                    if __list_subject.is_empty() {
                                                        {
                                                            let __list_subject = __pat3;
                                                            if __list_subject.is_empty() {
                                                                Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TOp(AverStr::from("+"), std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Term::TBi(AverStr::from("List.len"), aver_rt::AverList::from_vec(vec![a.clone()]))), std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(1)))), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("list.len.cons"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("x"), &x), crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("a"), &a)]), aver_rt::AverList::empty()) })
                                                            } else {
                                                                None
                                                            }
                                                        }
                                                    } else {
                                                        None
                                                    }
                                                }
                                            } else {
                                                None
                                            }
                                        }
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            },
                            _ => None,
                        }
                    } else {
                        None
                    }
                }
            } else {
                if &*__dispatch_subject == "List.take" {
                    {
                        let __list_subject = __pat1;
                        if let Some((__pat9, __pat10)) =
                            aver_rt::list_uncons_cloned(&__list_subject)
                        {
                            match __pat9 {
                                crate::proof_std::aver_generated::kernel::term::Term::TList(
                                    __pat11,
                                ) => {
                                    let __list_subject = __pat11;
                                    if __list_subject.is_empty() {
                                        {
                                            let __list_subject = __pat10;
                                            if let Some((n, __pat12)) =
                                                aver_rt::list_uncons_cloned(&__list_subject)
                                            {
                                                {
                                                    let __list_subject = __pat12;
                                                    if __list_subject.is_empty() {
                                                        Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TList(aver_rt::AverList::empty()), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("list.take.nil"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("n"), &n)]), aver_rt::AverList::empty()) })
                                                    } else {
                                                        None
                                                    }
                                                }
                                            } else {
                                                None
                                            }
                                        }
                                    } else {
                                        None
                                    }
                                }
                                crate::proof_std::aver_generated::kernel::term::Term::TBi(
                                    __pat13,
                                    __pat14,
                                ) => match &*__pat13 {
                                    "List.prepend" => {
                                        let __list_subject = __pat14;
                                        if let Some((x, __pat15)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            {
                                                let __list_subject = __pat15;
                                                if let Some((a, __pat16)) =
                                                    aver_rt::list_uncons_cloned(&__list_subject)
                                                {
                                                    {
                                                        let __list_subject = __pat16;
                                                        if __list_subject.is_empty() {
                                                            {
                                                                let __list_subject = __pat10;
                                                                if let Some((__pat17, __pat18)) =
                                                                    aver_rt::list_uncons_cloned(
                                                                        &__list_subject,
                                                                    )
                                                                {
                                                                    match __pat17 {
        crate::proof_std::aver_generated::kernel::term::Term::TInt(n) => {
            { let __list_subject = __pat18; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::takeCell(&x, &a, n)) } else { None } }
        },
        _ => {
            None
        }
    }
                                                                } else {
                                                                    None
                                                                }
                                                            }
                                                        } else {
                                                            None
                                                        }
                                                    }
                                                } else {
                                                    None
                                                }
                                            }
                                        } else {
                                            None
                                        }
                                    }
                                    _ => None,
                                },
                                _ => None,
                            }
                        } else {
                            None
                        }
                    }
                } else {
                    if &*__dispatch_subject == "List.concat" {
                        {
                            let __list_subject = __pat1;
                            if let Some((__pat19, __pat20)) =
                                aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                match __pat19 {
                                    crate::proof_std::aver_generated::kernel::term::Term::TList(
                                        __pat21,
                                    ) => {
                                        let __list_subject = __pat21;
                                        if __list_subject.is_empty() {
                                            {
                                                let __list_subject = __pat20;
                                                if let Some((b, __pat22)) =
                                                    aver_rt::list_uncons_cloned(&__list_subject)
                                                {
                                                    {
                                                        let __list_subject = __pat22;
                                                        if __list_subject.is_empty() {
                                                            Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: b.clone(), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("list.concat.nil"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("b"), &b)]), aver_rt::AverList::empty()) })
                                                        } else {
                                                            None
                                                        }
                                                    }
                                                } else {
                                                    None
                                                }
                                            }
                                        } else {
                                            None
                                        }
                                    }
                                    crate::proof_std::aver_generated::kernel::term::Term::TBi(
                                        __pat23,
                                        __pat24,
                                    ) => {
                                        match &*__pat23 {
                                            "List.prepend" => {
                                                let __list_subject = __pat24;
                                                if let Some((x, __pat25)) =
                                                    aver_rt::list_uncons_cloned(&__list_subject)
                                                {
                                                    {
                                                        let __list_subject = __pat25;
                                                        if let Some((a, __pat26)) =
                                                            aver_rt::list_uncons_cloned(
                                                                &__list_subject,
                                                            )
                                                        {
                                                            {
                                                                let __list_subject = __pat26;
                                                                if __list_subject.is_empty() {
                                                                    {
                                                                        let __list_subject =
                                                                            __pat20;
                                                                        if let Some((b, __pat27)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat27; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::lib::cons(&x, &crate::proof_std::aver_generated::kernel::term::Term::TBi(AverStr::from("List.concat"), aver_rt::AverList::from_vec(vec![a.clone(), b.clone()]))), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("list.concat.cons"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("x"), &x), crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("a"), &a), crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("b"), &b)]), aver_rt::AverList::empty()) }) } else { None } } } else { None }
                                                                    }
                                                                } else {
                                                                    None
                                                                }
                                                            }
                                                        } else {
                                                            None
                                                        }
                                                    }
                                                } else {
                                                    None
                                                }
                                            }
                                            _ => None,
                                        }
                                    }
                                    _ => None,
                                }
                            } else {
                                None
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "List.reverse" {
                            {
                                let __list_subject = __pat1;
                                if let Some((__pat28, __pat29)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    match __pat28 {
        crate::proof_std::aver_generated::kernel::term::Term::TList(__pat30) => {
            { let __list_subject = __pat30; if __list_subject.is_empty() { { let __list_subject = __pat29; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TList(aver_rt::AverList::empty()), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("list.reverse.nil"), aver_rt::AverList::empty(), aver_rt::AverList::empty()) }) } else { None } } } else { None } }
        },
        crate::proof_std::aver_generated::kernel::term::Term::TBi(__pat31, __pat32) => {
            match &*__pat31 {
        "List.prepend" => {
            { let __list_subject = __pat32; if let Some((x, __pat33)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat33; if let Some((a, __pat34)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat34; if __list_subject.is_empty() { { let __list_subject = __pat29; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TBi(AverStr::from("List.concat"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::Term::TBi(AverStr::from("List.reverse"), aver_rt::AverList::from_vec(vec![a.clone()])), crate::proof_std::aver_generated::kernel::lib::cons(&x, &crate::proof_std::aver_generated::kernel::term::Term::TList(aver_rt::AverList::empty()))])), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(AverStr::from("list.reverse.cons"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("x"), &x), crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("a"), &a)]), aver_rt::AverList::empty()) }) } else { None } } } else { None } } } else { None } } } else { None } }
        },
        _ => {
            None
        }
    }
        },
        _ => {
            None
        }
    }
                                } else {
                                    None
                                }
                            }
                        } else {
                            None
                        }
                    }
                }
            }
        }
    }
}

/// List.take of a cell, by the sign of a literal count.
pub fn takeCell(
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    a @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    n @ _: aver_rt::AverInt,
) -> Rw {
    crate::proof_std::cancel_checkpoint();
    let bs @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("x"), x),
        crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("a"), a),
        crate::proof_std::aver_generated::kernel::term::bind(
            AverStr::from("n"),
            &crate::proof_std::aver_generated::kernel::term::Term::TInt(n.clone()),
        ),
    ]);
    if (n > aver_rt::AverInt::from_i64(0)) {
        crate::proof_std::aver_generated::kernel::lib::Rw {
            to: crate::proof_std::aver_generated::kernel::lib::cons(
                x,
                &crate::proof_std::aver_generated::kernel::term::Term::TBi(
                    AverStr::from("List.take"),
                    aver_rt::AverList::from_vec(vec![
                        a.clone(),
                        crate::proof_std::aver_generated::kernel::term::Term::TOp(
                            AverStr::from("-"),
                            std::sync::Arc::new(
                                crate::proof_std::aver_generated::kernel::term::Term::TInt(
                                    n.clone(),
                                ),
                            ),
                            std::sync::Arc::new(
                                crate::proof_std::aver_generated::kernel::term::Term::TInt(
                                    aver_rt::AverInt::from_i64(1),
                                ),
                            ),
                        ),
                    ]),
                ),
            ),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(
                AverStr::from("list.take.cons_gt"),
                bs,
                aver_rt::AverList::from_vec(vec![
                    crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(
                        crate::proof_std::aver_generated::kernel::term::Term::TOp(
                            AverStr::from(">"),
                            std::sync::Arc::new(
                                crate::proof_std::aver_generated::kernel::term::Term::TInt(n),
                            ),
                            std::sync::Arc::new(
                                crate::proof_std::aver_generated::kernel::term::Term::TInt(
                                    aver_rt::AverInt::from_i64(0),
                                ),
                            ),
                        ),
                        crate::proof_std::aver_generated::kernel::term::Term::TBool(true),
                    ),
                ]),
            ),
        }
    } else {
        crate::proof_std::aver_generated::kernel::lib::Rw {
            to: crate::proof_std::aver_generated::kernel::term::Term::TList(
                aver_rt::AverList::empty(),
            ),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(
                AverStr::from("list.take.cons_le"),
                bs,
                aver_rt::AverList::from_vec(vec![
                    crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(
                        crate::proof_std::aver_generated::kernel::term::Term::TOp(
                            AverStr::from("<="),
                            std::sync::Arc::new(
                                crate::proof_std::aver_generated::kernel::term::Term::TInt(n),
                            ),
                            std::sync::Arc::new(
                                crate::proof_std::aver_generated::kernel::term::Term::TInt(
                                    aver_rt::AverInt::from_i64(0),
                                ),
                            ),
                        ),
                        crate::proof_std::aver_generated::kernel::term::Term::TBool(true),
                    ),
                ]),
            ),
        }
    }
}

/// A definition by name.
#[inline(always)]
pub fn findDef(
    mut f @ _: AverStr,
    mut ds @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Def>,
) -> Option<crate::proof_std::aver_generated::kernel::proof::Def> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(ds, [] => { return None; }, [d, rest] => { if (d.name == f) { return Some(d); } else { {
            let __tco1 = rest;
            ds = __tco1;
            continue;
        } } })
    }
}

/// Each parameter to its argument.
pub fn zipBind(
    ns @ _: &aver_rt::AverList<AverStr>,
    xs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding> {
    crate::proof_std::cancel_checkpoint();
    {
        let __int_match_subject = (ns.clone(), xs.clone());
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((n, ns2)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((x, xs2)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            aver_rt::AverList::prepend(
                crate::proof_std::aver_generated::kernel::term::bind(n, &x),
                &crate::proof_std::aver_generated::kernel::lib::zipBind(&ns2, &xs2),
            )
        } else {
            aver_rt::AverList::empty()
        }
    }
}

/// What opening d at args binds, in the kernel's order: the parameters, then each local binding under everything before it, the latest first.
#[inline(always)]
pub fn letBindings(
    d @ _: &crate::proof_std::aver_generated::kernel::proof::Def,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    if (aver_rt::AverInt::from_i64(d.params.len() as i64)
        == aver_rt::AverInt::from_i64(args.len() as i64))
    {
        crate::proof_std::aver_generated::kernel::lib::bindLets(
            d.lets.clone(),
            crate::proof_std::aver_generated::kernel::lib::zipBind(
                &crate::proof_std::aver_generated::kernel::proof::givenNames(&d.params),
                args,
            )
            .reverse(),
        )
    } else {
        Err(aver_rt::AverStr::from({
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
        }))
    }
}

/// Each local binding with what comes before it substituted, put in front.
#[inline(always)]
pub fn bindLets(
    mut lets @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    mut outer @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>, AverStr> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(lets, [] => { return Ok(outer); }, [b, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::term::bind(b.name, &crate::proof_std::aver_generated::kernel::subst::subst(&b.value, &outer)?), &outer);
            lets = __tco0;
            outer = __tco1;
            continue;
        } })
    }
}

/// f(args) = the arm its evaluated subject decides, with the subject's evaluation as the premise; none when f has no match body or no arm is decided.
#[inline(always)]
pub fn unfold(
    s @ _: &Scope,
    f @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> Result<Option<Rw>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::lib::unfoldWithin(
        s,
        f,
        args,
        aver_rt::AverInt::from_i64(400),
    )
}

/// unfold, nested at most depth deep.
#[inline(always)]
pub fn unfoldWithin(
    s @ _: &Scope,
    f @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    depth @ _: aver_rt::AverInt,
) -> Result<Option<Rw>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::findDef(f, s.defs.clone()) {
        None => Ok(None),
        Some(d @ _) => match d.body.clone() {
            crate::proof_std::aver_generated::kernel::term::Term::TMatch(subject, arms) => {
                let subject = (*subject).clone();
                crate::proof_std::aver_generated::kernel::lib::unfoldAt(
                    s,
                    &d.clone(),
                    args,
                    &subject,
                    &arms,
                    &crate::proof_std::aver_generated::kernel::lib::letBindings(&d, args)?,
                    depth,
                )
            }
            _ => {
                if crate::proof_std::aver_generated::kernel::lib::callsName(
                    d.body.clone(),
                    d.name.clone(),
                ) {
                    Ok(None)
                } else {
                    Ok(Some(crate::proof_std::aver_generated::kernel::lib::Rw {
                        to: crate::proof_std::aver_generated::kernel::subst::subst(
                            &d.body,
                            &crate::proof_std::aver_generated::kernel::lib::letBindings(&d, args)?,
                        )?,
                        proof: crate::proof_std::aver_generated::kernel::proof::Proof::PUnfold(
                            d.name.clone(),
                            aver_rt::AverInt::from_i64(0),
                            args.clone(),
                            aver_rt::AverList::empty(),
                            aver_rt::AverList::empty(),
                        ),
                    }))
                }
            }
        },
    }
}

/// Whether t calls the definition f anywhere: a body that calls itself is not opened whole.
pub fn callsName(
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut f @ _: AverStr,
) -> bool {
    loop {
        crate::proof_std::cancel_checkpoint();
        match t {
            crate::proof_std::aver_generated::kernel::term::Term::TCall(g, xs) => {
                return ((g == f)
                    || crate::proof_std::aver_generated::kernel::lib::callsAny(&xs, f));
            }
            crate::proof_std::aver_generated::kernel::term::Term::TBi(g, xs) => {
                return crate::proof_std::aver_generated::kernel::lib::callsAny(&xs, f);
            }
            crate::proof_std::aver_generated::kernel::term::Term::TOp(o, a, b) => {
                let a = (*a).clone();
                let b = (*b).clone();
                return (crate::proof_std::aver_generated::kernel::lib::callsName(a, f.clone())
                    || crate::proof_std::aver_generated::kernel::lib::callsName(b, f));
            }
            crate::proof_std::aver_generated::kernel::term::Term::TNeg(a) => {
                let a = (*a).clone();
                {
                    let __tco0 = a;
                    t = __tco0;
                    continue;
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term::TGet(a, n) => {
                let a = (*a).clone();
                {
                    let __tco0 = a;
                    t = __tco0;
                    continue;
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term::TCtor(c, xs) => {
                return crate::proof_std::aver_generated::kernel::lib::callsAny(&xs, f);
            }
            crate::proof_std::aver_generated::kernel::term::Term::TList(xs) => {
                return crate::proof_std::aver_generated::kernel::lib::callsAny(&xs, f);
            }
            crate::proof_std::aver_generated::kernel::term::Term::TTuple(xs) => {
                return crate::proof_std::aver_generated::kernel::lib::callsAny(&xs, f);
            }
            crate::proof_std::aver_generated::kernel::term::Term::TParts(xs) => {
                return crate::proof_std::aver_generated::kernel::lib::callsAny(&xs, f);
            }
            crate::proof_std::aver_generated::kernel::term::Term::TRec(n, fs) => {
                return crate::proof_std::aver_generated::kernel::lib::callsFields(&fs, f);
            }
            crate::proof_std::aver_generated::kernel::term::Term::TUpd(n, a, fs) => {
                let a = (*a).clone();
                return (crate::proof_std::aver_generated::kernel::lib::callsName(a, f.clone())
                    || crate::proof_std::aver_generated::kernel::lib::callsFields(&fs, f));
            }
            crate::proof_std::aver_generated::kernel::term::Term::TMatch(a, arms) => {
                let a = (*a).clone();
                return (crate::proof_std::aver_generated::kernel::lib::callsName(a, f.clone())
                    || crate::proof_std::aver_generated::kernel::lib::callsArms(&arms, f));
            }
            _ => {
                return false;
            }
        }
    }
}

/// Whether any of xs calls f.
#[inline(always)]
pub fn callsAny(
    xs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    f @ _: AverStr,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(xs.clone(), [] => false, [x, rest] => (crate::proof_std::aver_generated::kernel::lib::callsName(x, f.clone()) || crate::proof_std::aver_generated::kernel::lib::callsAny(&rest, f)))
}

/// Whether any field value calls f.
#[inline(always)]
pub fn callsFields(
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
    f @ _: AverStr,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => false, [x, rest] => (crate::proof_std::aver_generated::kernel::lib::callsName(x.value, f.clone()) || crate::proof_std::aver_generated::kernel::lib::callsFields(&rest, f)))
}

/// Whether any arm body calls f.
#[inline(always)]
pub fn callsArms(
    arms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
    f @ _: AverStr,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(arms.clone(), [] => false, [a, rest] => (crate::proof_std::aver_generated::kernel::lib::callsName(a.body, f.clone()) || crate::proof_std::aver_generated::kernel::lib::callsArms(&rest, f)))
}

/// Evaluate the subject, choose the arm, and write the unfold with the subject's evaluation as its premise.
pub fn unfoldAt(
    s @ _: &Scope,
    d @ _: &crate::proof_std::aver_generated::kernel::proof::Def,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    subject @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    arms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
    outer @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    depth @ _: aver_rt::AverInt,
) -> Result<Option<Rw>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let r @ _ = crate::proof_std::aver_generated::kernel::lib::evalWithin(
        s,
        &crate::proof_std::aver_generated::kernel::subst::subst(subject, outer)?,
        depth,
    )?;
    match crate::proof_std::aver_generated::kernel::lib::selectArm(arms, r.to) {
        None => Ok(None),
        Some(__pat0 @ _) => {
            let (k, arm, parts) = __pat0;
            Ok(Some(crate::proof_std::aver_generated::kernel::lib::Rw {
                to: crate::proof_std::aver_generated::kernel::subst::subst(
                    &arm.body,
                    &aver_rt::AverList::concat(
                        &crate::proof_std::aver_generated::kernel::lib::patternBinds(
                            &arm.pattern,
                            &parts,
                        ),
                        &outer.clone(),
                    ),
                )?,
                proof: crate::proof_std::aver_generated::kernel::proof::Proof::PUnfold(
                    d.name.clone(),
                    k,
                    args.clone(),
                    parts,
                    aver_rt::AverList::from_vec(vec![r.proof]),
                ),
            }))
        }
    }
}

/// The arm a literal or a list value decides, counted from 1, with what the kernel's unfold takes as binders: the parts of a cell, nothing for a literal, the value itself for a catch-all; none when an earlier arm cannot be ruled out.
#[inline(always)]
pub fn selectArm(
    arms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
    mut value @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<(
    aver_rt::AverInt,
    crate::proof_std::aver_generated::kernel::term::Arm,
    aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
)> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::lib::selectFrom(
        arms.clone(),
        value,
        aver_rt::AverInt::from_i64(1),
    )
}

/// nil, cons, lit or other.
pub fn shapeOf(v @ _: &crate::proof_std::aver_generated::kernel::term::Term) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match v.clone() {
        crate::proof_std::aver_generated::kernel::term::Term::TList(__pat0) => {
            let __list_subject = __pat0;
            if __list_subject.is_empty() {
                AverStr::from("nil")
            } else {
                if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) {
                    AverStr::from("lit")
                } else {
                    AverStr::from("other")
                }
            }
        }
        crate::proof_std::aver_generated::kernel::term::Term::TBi(__pat1, __pat2) => match &*__pat1
        {
            "List.prepend" => {
                let __list_subject = __pat2;
                if let Some((h, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat3;
                        if let Some((t, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat4;
                                if __list_subject.is_empty() {
                                    AverStr::from("cons")
                                } else {
                                    if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) {
                                        AverStr::from("lit")
                                    } else {
                                        AverStr::from("other")
                                    }
                                }
                            }
                        } else {
                            if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) {
                                AverStr::from("lit")
                            } else {
                                AverStr::from("other")
                            }
                        }
                    }
                } else {
                    if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) {
                        AverStr::from("lit")
                    } else {
                        AverStr::from("other")
                    }
                }
            }
            _ => {
                if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) {
                    AverStr::from("lit")
                } else {
                    AverStr::from("other")
                }
            }
        },
        _ => {
            if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) {
                AverStr::from("lit")
            } else {
                AverStr::from("other")
            }
        }
    }
}

/// The head and tail of a list cell.
pub fn cellParts(
    v @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match v.clone() {
        crate::proof_std::aver_generated::kernel::term::Term::TBi(n, xs) => xs,
        _ => aver_rt::AverList::empty(),
    }
}

/// An Int, Bool or String literal.
pub fn isLiteral(t @ _: &crate::proof_std::aver_generated::kernel::term::Term) -> bool {
    crate::proof_std::cancel_checkpoint();
    match t.clone() {
        crate::proof_std::aver_generated::kernel::term::Term::TInt(i) => true,
        crate::proof_std::aver_generated::kernel::term::Term::TBool(b) => true,
        crate::proof_std::aver_generated::kernel::term::Term::TStr(s) => true,
        _ => false,
    }
}

/// The names a cell pattern or a named catch-all binds.
pub fn patternBinds(
    p @ _: &crate::proof_std::aver_generated::kernel::term::Pat,
    parts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding> {
    crate::proof_std::cancel_checkpoint();
    {
        let (__pat0, ys) = (p.clone(), parts.clone());
        match __pat0 {
            crate::proof_std::aver_generated::kernel::term::Pat::PCons(h, t) => {
                let __list_subject = ys;
                if let Some((x, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat1;
                        if let Some((y, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    aver_rt::AverList::from_vec(vec![
                                        crate::proof_std::aver_generated::kernel::term::bind(h, &x),
                                        crate::proof_std::aver_generated::kernel::term::bind(t, &y),
                                    ])
                                } else {
                                    aver_rt::AverList::empty()
                                }
                            }
                        } else {
                            aver_rt::AverList::empty()
                        }
                    }
                } else {
                    aver_rt::AverList::empty()
                }
            }
            crate::proof_std::aver_generated::kernel::term::Pat::PVar(n) => {
                let __list_subject = ys;
                if let Some((x, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat3;
                        if __list_subject.is_empty() {
                            aver_rt::AverList::from_vec(vec![
                                crate::proof_std::aver_generated::kernel::term::bind(n, &x),
                            ])
                        } else {
                            aver_rt::AverList::empty()
                        }
                    }
                } else {
                    aver_rt::AverList::empty()
                }
            }
            crate::proof_std::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
                crate::proof_std::aver_generated::kernel::lib::fieldBinds(ns, ys)
            }
            _ => aver_rt::AverList::empty(),
        }
    }
}

/// Each named field of a constructor pattern to its part; a wildcard field binds nothing.
pub fn fieldBinds(
    mut ns @ _: aver_rt::AverList<AverStr>,
    mut ys @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding> {
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __int_match_subject = (ns, ys);
            let (__lit0, __lit1) = &__int_match_subject;
            if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
                let Some((n, ns2)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                let Some((y, ys2)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                if (&*n == "_") {
                    {
                        let __tco0 = ns2;
                        let __tco1 = ys2;
                        ns = __tco0;
                        ys = __tco1;
                        continue;
                    }
                } else {
                    return aver_rt::AverList::prepend(
                        crate::proof_std::aver_generated::kernel::term::bind(n, &y),
                        &crate::proof_std::aver_generated::kernel::lib::fieldBinds(ns2, ys2),
                    );
                }
            } else {
                return aver_rt::AverList::empty();
            }
        }
    }
}

/// base, or base followed by the first number that makes a name not in used (often taken(goal)): a hypothesis or binder name must not clash with one in scope.
#[inline(always)]
pub fn fresh(base @ _: AverStr, used @ _: &aver_rt::AverList<AverStr>) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    if used.contains(&base) {
        crate::proof_std::aver_generated::kernel::lib::freshFrom(
            base,
            used.clone(),
            aver_rt::AverInt::from_i64(1),
        )
    } else {
        base
    }
}

/// The first base followed by a number from k on not in used.
#[inline(always)]
pub fn freshFrom(
    mut base @ _: AverStr,
    used @ _: aver_rt::AverList<AverStr>,
    mut k @ _: aver_rt::AverInt,
) -> AverStr {
    let used @ _ = std::sync::Arc::new(used);
    loop {
        crate::proof_std::cancel_checkpoint();
        let name @ _ = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(32)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(base))));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
            __b
        });
        if used.contains(&name) {
            {
                let __tco2 = k.add(&aver_rt::AverInt::from_i64(1));
                k = __tco2;
                continue;
            }
        } else {
            return name;
        }
    }
}

/// Every name a goal already uses, for fresh to avoid: the givens, the variables of the claim and of the `when`, the hypothesis `when` itself, the module-level bindings, the list variables, and each hypothesis's name and variables.
#[inline(always)]
pub fn taken(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    let ob @ _ = goal.obligation.clone();
    let vars @ _ =
        crate::proof_std::aver_generated::kernel::subst::freeVarsAll(&aver_rt::AverList::concat(
            &aver_rt::AverList::from_vec(vec![ob.lhs, ob.rhs]),
            &ob.premise,
        ));
    aver_rt::AverList::concat(
        &aver_rt::AverList::concat(
            &aver_rt::AverList::concat(
                &crate::proof_std::aver_generated::kernel::proof::givenNames(&ob.givens),
                &vars,
            ),
            &aver_rt::AverList::concat(
                &aver_rt::AverList::concat(
                    &crate::proof_std::aver_generated::kernel::lib::whenLabel(goal),
                    &crate::proof_std::aver_generated::kernel::lib::constNames(&goal.consts),
                ),
                &goal.lists.clone(),
            ),
        ),
        &crate::proof_std::aver_generated::kernel::lib::hypNames(&goal.hyps),
    )
}

/// The names of the hypotheses and every variable they mention.
#[inline(always)]
pub fn hypNames(
    hs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => aver_rt::AverList::empty(), [h, rest] => aver_rt::AverList::concat(&aver_rt::AverList::prepend(h.name, &crate::proof_std::aver_generated::kernel::subst::freeVarsAll(&aver_rt::AverList::from_vec(vec![h.eqn.lhs, h.eqn.rhs]))), &crate::proof_std::aver_generated::kernel::lib::hypNames(&rest)))
}

/// The name of the `when` hypothesis, when the law has one.
#[inline(always)]
pub fn whenLabel(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::whenOf(goal) {
        None => aver_rt::AverList::empty(),
        Some(_) => aver_rt::AverList::from_vec(vec![AverStr::from("when")]),
    }
}

/// The names of module-level bindings.
#[inline(always)]
pub fn constNames(
    cs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Const>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => aver_rt::AverList::empty(), [c, rest] => aver_rt::AverList::prepend(c.name, &crate::proof_std::aver_generated::kernel::lib::constNames(&rest)))
}

/// A goal for law with nothing else in it: no definitions, laws or hypotheses, its givens listed by what their types are. For a plan's examples, so they keep reading the same when Goal gains a field.
#[inline(always)]
pub fn goalFor(
    law @ _: &crate::proof_std::aver_generated::kernel::proof::Law,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::wire::withGivens(
        crate::proof_std::aver_generated::kernel::proof::Goal {
            obligation: crate::proof_std::aver_generated::kernel::proof::Law {
                givens: aver_rt::AverList::empty(),
                ..law.clone()
            },
            finite: aver_rt::AverList::empty(),
            lists: aver_rt::AverList::empty(),
            ints: aver_rt::AverList::empty(),
            defs: aver_rt::AverList::empty(),
            consts: aver_rt::AverList::empty(),
            sums: aver_rt::AverList::empty(),
            recs: aver_rt::AverList::empty(),
            laws: aver_rt::AverList::empty(),
            facts: aver_rt::AverList::empty(),
            hyps: aver_rt::AverList::empty(),
        },
        law.givens.clone(),
    )
}

/// The goal step data states, its proof left out: how the compiler hands a plan its goal.
#[inline(always)]
pub fn readGoal(
    text @ _: AverStr,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::wire::goal(text)
}

/// A plan's answer as the compiler reads it back: `proof` and the proof in step data, or `refused` and the reason.
#[inline(always)]
pub fn writeAnswer(
    r @ _: &Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr>,
) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::wire::answer(r)
}
