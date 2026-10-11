#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

/// Largest number of cases a split may write, over all its givens; the automatic producer's bound.
pub fn maxCases() -> aver_rt::AverInt {
    crate::proof_std::cancel_checkpoint();
    aver_rt::AverInt::from_i64(64)
}

/// Split every given of finite type (Bool, a sum whose variants carry nothing, records and tuples of those) into all its values, in the order the kernel's enum step lists them. Each case is closed by the `when` reading false there, or else by close.
pub fn cases(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let n @ _ = crate::proof_std::aver_generated::kernel::std::caseCount(&goal.finite);
    {
        let __list_subject = goal.finite.clone();
        if __list_subject.is_empty() {
            Err(AverStr::from("no given of finite type"))
        } else {
            if (n > aver_rt::AverInt::from_i64(64)) {
                Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(53)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" cases is more than a split may write"));
                    __b
                }))
            } else {
                crate::proof_std::aver_generated::kernel::std::splitAll(goal, &goal.finite)
            }
        }
    }
}

/// How many cases splitting every given writes.
#[inline(always)]
pub fn caseCount(
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
) -> aver_rt::AverInt {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(gs.clone(), [] => aver_rt::AverInt::from_i64(1), [g, rest] => aver_rt::AverInt::from_i64(crate::proof_std::aver_generated::kernel::finite::values(&g.fin).len() as i64).mul(&crate::proof_std::aver_generated::kernel::std::caseCount(&rest)))
}

/// Split the first given into its values and the rest inside each case; with none left, close the case.
#[inline(always)]
pub fn splitAll(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
    gs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(gs.clone(), [] => crate::proof_std::aver_generated::kernel::std::leaf(goal), [g, rest] => Ok(crate::proof_std::aver_generated::kernel::proof::Proof::PEnum(g.name.clone(), goal.obligation.lhs.clone(), goal.obligation.rhs.clone(), crate::proof_std::aver_generated::kernel::std::eachValue(goal, g.name.clone(), &crate::proof_std::aver_generated::kernel::finite::values(&g.fin), &rest)?)))
}

/// One case per value, in the kernel's order: the goal with v replaced by the value.
#[inline(always)]
pub fn eachValue(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
    v @ _: AverStr,
    vals @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    rest @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Proof>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(vals.clone(), [] => Ok(aver_rt::AverList::empty()), [x, xs] => Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::std::splitAll(&crate::proof_std::aver_generated::kernel::std::at(goal.clone(), v.clone(), &x)?, rest)?, &crate::proof_std::aver_generated::kernel::std::eachValue(goal, v, &xs, rest)?)))
}

/// One case: impossible when the `when` reads false in it, otherwise close.
#[inline(always)]
pub fn leaf(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::whenOf(goal) {
        None => crate::proof_std::aver_generated::kernel::std::close(goal),
        Some(p @ _) => {
            match crate::proof_std::aver_generated::kernel::lib::whenRefutes(
                &crate::proof_std::aver_generated::kernel::lib::scope(goal),
                &p,
                &goal.obligation.lhs,
                &goal.obligation.rhs,
            ) {
                Ok(proof @ _) => Ok(proof),
                Err(_) => crate::proof_std::aver_generated::kernel::std::close(goal),
            }
        }
    }
}

/// The goal with v replaced by x in the claim, the `when` and every hypothesis, the way a split's case reads; v is no longer a given to split.
pub fn at(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    v @ _: AverStr,
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Goal, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let ob @ _ = goal.obligation;
    let claim @ _ = crate::proof_std::aver_generated::kernel::proof::Law {
        lhs: crate::proof_std::aver_generated::kernel::lib::substitute(&ob.lhs, v.clone(), x)?,
        rhs: crate::proof_std::aver_generated::kernel::lib::substitute(&ob.rhs, v.clone(), x)?,
        premise: crate::proof_std::aver_generated::kernel::std::substAll(
            &ob.premise,
            v.clone(),
            x,
        )?,
        ..ob
    };
    Ok(crate::proof_std::aver_generated::kernel::proof::Goal {
        obligation: claim,
        finite: crate::proof_std::aver_generated::kernel::std::withoutGiven(goal.finite, v.clone()),
        hyps: crate::proof_std::aver_generated::kernel::std::substHyps(&goal.hyps, v, x)?,
        ..goal
    })
}

/// The finite givens other than v.
#[inline(always)]
pub fn withoutGiven(
    mut gs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given>,
    mut v @ _: AverStr,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Given> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(gs, [] => { return aver_rt::AverList::empty(); }, [g, rest] => { if (g.name == v) { {
            let __tco0 = rest;
            gs = __tco0;
            continue;
        } } else { return aver_rt::AverList::prepend(g, &crate::proof_std::aver_generated::kernel::std::withoutGiven(rest, v)); } })
    }
}

/// Each term with v replaced by x.
#[inline(always)]
pub fn substAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    v @ _: AverStr,
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => Ok(aver_rt::AverList::empty()), [t, rest] => Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::lib::substitute(&t, v.clone(), x)?, &crate::proof_std::aver_generated::kernel::std::substAll(&rest, v, x)?)))
}

/// Each hypothesis with v replaced by x.
#[inline(always)]
pub fn substHyps(
    hs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
    v @ _: AverStr,
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(hs.clone(), [] => Ok(aver_rt::AverList::empty()), [h, rest] => Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::proof::Hyp { name: h.name, eqn: crate::proof_std::aver_generated::kernel::term::Eqn { lhs: crate::proof_std::aver_generated::kernel::lib::substitute(&h.eqn.lhs, v.clone(), x)?, rhs: crate::proof_std::aver_generated::kernel::lib::substitute(&h.eqn.rhs, v.clone(), x)? } }, &crate::proof_std::aver_generated::kernel::std::substHyps(&rest, v, x)?)))
}

/// The goal with one more hypothesis, name: lhs = rhs, in front. Only a step around the proof can bring a hypothesis into scope (a Bool split, an induction); the kernel refuses a proof that cites one it does not have.
pub fn assume(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    name @ _: AverStr,
    lhs @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    rhs @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::proof::Goal {
        hyps: aver_rt::AverList::prepend(
            crate::proof_std::aver_generated::kernel::proof::Hyp {
                name: name,
                eqn: crate::proof_std::aver_generated::kernel::term::Eqn {
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                },
            },
            &goal.hyps,
        ),
        ..goal
    }
}

/// lhs = n = rhs when both sides evaluate to the same n, rewriting by the goal's hypotheses (left side to right side) where evaluation stops.
pub fn close(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let s @ _ = crate::proof_std::aver_generated::kernel::lib::scope(goal);
    let lhs @ _ = goal.obligation.lhs.clone();
    let rhs @ _ = goal.obligation.rhs.clone();
    let l @ _ = crate::proof_std::aver_generated::kernel::std::rounds(
        &s,
        &goal.hyps,
        &lhs,
        aver_rt::AverInt::from_i64(16),
    )?;
    let r @ _ = crate::proof_std::aver_generated::kernel::std::rounds(
        &s,
        &goal.hyps,
        &rhs,
        aver_rt::AverInt::from_i64(16),
    )?;
    if (l.to == r.to) {
        crate::proof_std::aver_generated::kernel::lib::trans(
            &aver_rt::AverList::from_vec(vec![lhs, l.to.clone(), rhs]),
            &aver_rt::AverList::from_vec(vec![
                l.proof,
                crate::proof_std::aver_generated::kernel::proof::Proof::PSymm(std::sync::Arc::new(
                    r.proof,
                )),
            ]),
        )
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(55)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("the sides stop at "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::wire::termText(&l.to)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(" and "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::wire::termText(&r.to)),
            )));
            __b
        }))
    }
}

/// Evaluate, rewrite once by a hypothesis, and repeat while n lasts.
pub fn rounds(
    s @ _: &crate::proof_std::aver_generated::kernel::lib::Scope,
    hyps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    n @ _: aver_rt::AverInt,
) -> Result<crate::proof_std::aver_generated::kernel::lib::Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let r @ _ = crate::proof_std::aver_generated::kernel::lib::evaluate(s, t)?;
    if (n > aver_rt::AverInt::from_i64(0)) {
        match crate::proof_std::aver_generated::kernel::std::rewriteOnce(
            hyps.clone(),
            r.to.clone(),
        )? {
            None => Ok(r),
            Some(step @ _) => Ok(crate::proof_std::aver_generated::kernel::lib::chain(
                t,
                &r,
                &crate::proof_std::aver_generated::kernel::lib::chain(
                    &r.to,
                    &step.clone(),
                    &crate::proof_std::aver_generated::kernel::std::rounds(
                        s,
                        hyps,
                        &step.to,
                        n.sub(&aver_rt::AverInt::from_i64(1)),
                    )?,
                ),
            )),
        }
    } else {
        Ok(r)
    }
}

/// t with its first occurrence of a hypothesis's left side replaced by its right side, by congruence.
#[inline(always)]
pub fn rewriteOnce(
    mut hyps @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Hyp>,
    t @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<Option<crate::proof_std::aver_generated::kernel::lib::Rw>, AverStr> {
    let t @ _ = std::sync::Arc::new(t);
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(hyps, [] => { return Ok(None); }, [h, rest] => { if (h.eqn.lhs == h.eqn.rhs) { {
            let __tco0 = rest;
            hyps = __tco0;
            continue;
        } } else { match crate::proof_std::aver_generated::kernel::std::holeAt(&*t, &h.eqn.lhs) { None => { {
            let __tco0 = rest;
            hyps = __tco0;
            continue;
        } }, Some(c @ _) => { return Ok(Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::subst::subst(&c, &aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(AverStr::from("□"), &h.eqn.rhs)]))?, proof: crate::proof_std::aver_generated::kernel::std::congrHyp(&c, h.name) })); } } } })
    }
}

/// The hypothesis itself at the top, or under the context c.
pub fn congrHyp(
    c @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    name @ _: AverStr,
) -> crate::proof_std::aver_generated::kernel::proof::Proof {
    crate::proof_std::cancel_checkpoint();
    match c {
        crate::proof_std::aver_generated::kernel::term::Term::THole => {
            crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(name)
        }
        _ => crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
            c.clone(),
            std::sync::Arc::new(crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(name)),
        ),
    }
}

/// t with its first occurrence of target, outside match arms, replaced by the hole.
#[inline(always)]
pub fn holeAt(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    target @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    if (t == target) {
        Some(crate::proof_std::aver_generated::kernel::term::Term::THole)
    } else {
        crate::proof_std::aver_generated::kernel::std::holeBelow(t, target)
    }
}

/// holeAt in the parts of t.
pub fn holeBelow(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    target @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match t.clone() {
        crate::proof_std::aver_generated::kernel::term::Term::TCall(f, xs) => {
            crate::proof_std::aver_generated::kernel::std::mapSome(
                &crate::proof_std::aver_generated::kernel::std::holeInList(&xs, target),
                &crate::proof_std::aver_generated::kernel::term::Term::TCall(
                    f,
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TBi(f, xs) => {
            crate::proof_std::aver_generated::kernel::std::mapSome(
                &crate::proof_std::aver_generated::kernel::std::holeInList(&xs, target),
                &crate::proof_std::aver_generated::kernel::term::Term::TBi(
                    f,
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TCtor(c, xs) => {
            crate::proof_std::aver_generated::kernel::std::mapSome(
                &crate::proof_std::aver_generated::kernel::std::holeInList(&xs, target),
                &crate::proof_std::aver_generated::kernel::term::Term::TCtor(
                    c,
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TList(xs) => {
            crate::proof_std::aver_generated::kernel::std::mapSome(
                &crate::proof_std::aver_generated::kernel::std::holeInList(&xs, target),
                &crate::proof_std::aver_generated::kernel::term::Term::TList(
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TTuple(xs) => {
            crate::proof_std::aver_generated::kernel::std::mapSome(
                &crate::proof_std::aver_generated::kernel::std::holeInList(&xs, target),
                &crate::proof_std::aver_generated::kernel::term::Term::TTuple(
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TOp(o, a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::proof_std::aver_generated::kernel::std::opWith(
                o,
                &a,
                &b,
                &crate::proof_std::aver_generated::kernel::std::holeAt(&a, target),
                target,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TNeg(a) => {
            let a = (*a).clone();
            crate::proof_std::aver_generated::kernel::std::negWith(
                &crate::proof_std::aver_generated::kernel::std::holeAt(&a, target),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TGet(a, f) => {
            let a = (*a).clone();
            crate::proof_std::aver_generated::kernel::std::getWith(
                &crate::proof_std::aver_generated::kernel::std::holeAt(&a, target),
                f,
            )
        }
        _ => None,
    }
}

/// shape (a call, builtin, constructor, list or tuple) around parts holding the hole.
#[inline(always)]
pub fn mapSome(
    xs @ _: &Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>>,
    shape @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match xs.clone() {
        None => None,
        Some(ys @ _) => match shape.clone() {
            crate::proof_std::aver_generated::kernel::term::Term::TCall(f, _) => Some(
                crate::proof_std::aver_generated::kernel::term::Term::TCall(f, ys),
            ),
            crate::proof_std::aver_generated::kernel::term::Term::TBi(f, _) => Some(
                crate::proof_std::aver_generated::kernel::term::Term::TBi(f, ys),
            ),
            crate::proof_std::aver_generated::kernel::term::Term::TCtor(c, _) => Some(
                crate::proof_std::aver_generated::kernel::term::Term::TCtor(c, ys),
            ),
            crate::proof_std::aver_generated::kernel::term::Term::TTuple(_) => {
                Some(crate::proof_std::aver_generated::kernel::term::Term::TTuple(ys))
            }
            _ => Some(crate::proof_std::aver_generated::kernel::term::Term::TList(
                ys,
            )),
        },
    }
}

/// A negation around a part holding the hole.
#[inline(always)]
pub fn negWith(
    a @ _: &Option<crate::proof_std::aver_generated::kernel::term::Term>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match a.clone() {
        Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TNeg(
            std::sync::Arc::new(c),
        )),
        None => None,
    }
}

/// A field read around a part holding the hole.
#[inline(always)]
pub fn getWith(
    a @ _: &Option<crate::proof_std::aver_generated::kernel::term::Term>,
    f @ _: AverStr,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match a.clone() {
        Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TGet(
            std::sync::Arc::new(c),
            f,
        )),
        None => None,
    }
}

/// An operator with the hole on the left, else on the right.
#[inline(always)]
pub fn opWith(
    o @ _: AverStr,
    a @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    b @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    left @ _: &Option<crate::proof_std::aver_generated::kernel::term::Term>,
    target @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match left.clone() {
        Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TOp(
            o,
            std::sync::Arc::new(c),
            std::sync::Arc::new(b.clone()),
        )),
        None => match crate::proof_std::aver_generated::kernel::std::holeAt(b, target) {
            Some(c @ _) => Some(crate::proof_std::aver_generated::kernel::term::Term::TOp(
                o,
                std::sync::Arc::new(a.clone()),
                std::sync::Arc::new(c),
            )),
            None => None,
        },
    }
}

/// xs with the hole in the first element that holds target.
#[inline(always)]
pub fn holeInList(
    xs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    target @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(xs.clone(), [] => None, [x, rest] => match crate::proof_std::aver_generated::kernel::std::holeAt(&x, target) { Some(c @ _) => { Some(aver_rt::AverList::prepend(c, &rest)) }, None => { crate::proof_std::aver_generated::kernel::std::prependTo(&x, &crate::proof_std::aver_generated::kernel::std::holeInList(&rest, target)) } })
}

/// x in front of a list holding the hole.
#[inline(always)]
pub fn prependTo(
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    xs @ _: &Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>>,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>> {
    crate::proof_std::cancel_checkpoint();
    match xs.clone() {
        Some(ys @ _) => Some(aver_rt::AverList::prepend(x.clone(), &ys)),
        None => None,
    }
}

/// Split on the Bool term on: next proves the goal where on is true and where it is false, each time with the case as a hypothesis (fresh name: on = true or on = false) that close rewrites by.
pub fn splitBool(
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
    on @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    next @ _: fn(
        crate::proof_std::aver_generated::kernel::proof::Goal,
    ) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr>,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let h @ _ = crate::proof_std::aver_generated::kernel::lib::fresh(
        AverStr::from("c"),
        &crate::proof_std::aver_generated::kernel::lib::taken(&goal),
    );
    let yes @ _ = next(crate::proof_std::aver_generated::kernel::std::assume(
        goal.clone(),
        h.clone(),
        on,
        &crate::proof_std::aver_generated::kernel::term::Term::TBool(true),
    ))?;
    let no @ _ = next(crate::proof_std::aver_generated::kernel::std::assume(
        goal,
        h.clone(),
        on,
        &crate::proof_std::aver_generated::kernel::term::Term::TBool(false),
    ))?;
    Ok(
        crate::proof_std::aver_generated::kernel::proof::Proof::PCases(
            on.clone(),
            h,
            std::sync::Arc::new(yes),
            std::sync::Arc::new(no),
        ),
    )
}
