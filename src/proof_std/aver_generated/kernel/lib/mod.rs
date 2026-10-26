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
                aver_list_match!(arms, [] => { return None }, [a, rest] => { { let __arg5 @ _ = crate::proof_std::aver_generated::kernel::lib::shapeOf(&value); __MutualTco1::SelectOne(a, rest, value, k, __arg5) } })
            }
            __MutualTco1::SelectOne(
                mut a @ _,
                mut rest @ _,
                mut v @ _,
                mut k @ _,
                mut shape @ _,
            ) => {
                crate::proof_std::cancel_checkpoint();
                match &a.pattern {
                    crate::proof_std::aver_generated::kernel::term::Pat::PWild => {
                        return Some((
                            k,
                            a,
                            aver_rt::AverList::prepend_owned(v, aver_rt::AverList::empty()),
                        ));
                    }
                    crate::proof_std::aver_generated::kernel::term::Pat::PVar(n) => {
                        let n = n.clone();
                        return Some((
                            k,
                            a,
                            aver_rt::AverList::prepend_owned(v, aver_rt::AverList::empty()),
                        ));
                    }
                    crate::proof_std::aver_generated::kernel::term::Pat::PLit(x) => {
                        let x = x.clone();
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
                        let h = h.clone();
                        let t = t.clone();
                        {
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
                    }
                    crate::proof_std::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
                        let c = c.clone();
                        let ns = ns.clone();
                        {
                            let __t = &v;
                            match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TCtor(__d_fields) => {
            let d = __d_fields.0.clone();
            let ys = __d_fields.1.clone();
            if (c == d) { return Some((k, a, ys)) } else { __MutualTco1::SelectFrom(rest, v, k.add(&aver_rt::AverInt::from_i64(1))) }
        },
        _ => {
            return None
        }
    }
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
    mut value @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> Scope {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::lib::Scope {
        defs: s.defs.clone(),
        known: aver_rt::AverList::prepend(
            crate::proof_std::aver_generated::kernel::lib::Known {
                hyp: hyp,
                name: name,
                value: value,
            },
            &s.known,
        ),
    }
}

/// t rewritten to itself, by no step.
pub fn refl(mut t @ _: crate::proof_std::aver_generated::kernel::term::Term) -> Rw {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::lib::Rw {
        to: t.clone(),
        proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRefl(
            crate::proof_std::aver_generated::kernel::proof::Proof__PRefl(t),
        ),
    }
}

/// Whether a proof is a bare reflexivity.
pub fn isRefl(p @ _: &crate::proof_std::aver_generated::kernel::proof::Proof) -> bool {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = p;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::proof::Proof__view::PRefl(__t_fields) => {
                let t = __t_fields.0.clone();
                true
            }
            _ => false,
        }
    }
}

/// t = a.to = b.to, where a starts at t and b at a.to. A reflexive link is dropped, so the chain never carries one.
pub fn chain(
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut a @ _: Rw,
    mut b @ _: Rw,
) -> Rw {
    crate::proof_std::cancel_checkpoint();
    {
        let __subject = (
            crate::proof_std::aver_generated::kernel::lib::isRefl(&a.proof),
            crate::proof_std::aver_generated::kernel::lib::isRefl(&b.proof),
        );
        match __subject {
            (true, _) => b,
            (false, true) => crate::proof_std::aver_generated::kernel::lib::Rw {
                to: a.to,
                proof: a.proof,
            },
            (false, false) => crate::proof_std::aver_generated::kernel::lib::Rw {
                to: b.to.clone(),
                proof: crate::proof_std::aver_generated::kernel::proof::Proof::PTrans(
                    crate::proof_std::aver_generated::kernel::proof::Proof__PTrans(
                        aver_rt::AverList::from_vec(vec![t, a.to, b.to]),
                        aver_rt::AverList::from_vec(vec![a.proof, b.proof]),
                    ),
                ),
            },
        }
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
            let __list_subject = proofs;
            match aver_rt::list_uncons_cloned(__list_subject) {
                Some((p, __pat0)) => {
                    let __list_subject = __pat0;
                    if __list_subject.is_empty() {
                        Ok(p)
                    } else {
                        Ok(
                            crate::proof_std::aver_generated::kernel::proof::Proof::PTrans(
                                crate::proof_std::aver_generated::kernel::proof::Proof__PTrans(
                                    terms.clone(),
                                    proofs.clone(),
                                ),
                            ),
                        )
                    }
                }
                None => Ok(
                    crate::proof_std::aver_generated::kernel::proof::Proof::PTrans(
                        crate::proof_std::aver_generated::kernel::proof::Proof__PTrans(
                            terms.clone(),
                            proofs.clone(),
                        ),
                    ),
                ),
            }
        }
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(84usize);
                                __b.push_str(&::aver_rt::str_lit!("a chain of "));
                                __b
                            };
                            __b.push_display(&(aver_rt::AverInt::from_i64(proofs.len() as i64)));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" steps lists "));
                        __b
                    };
                    __b.push_display(
                        &(aver_rt::AverInt::from_i64(proofs.len() as i64)
                            .add(&aver_rt::AverInt::from_i64(1))),
                    );
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" terms, not "));
                __b
            };
            __b.push_display(&(aver_rt::AverInt::from_i64(terms.len() as i64)));
            __b
        }))
    }
}

/// The name of the hypothesis that holds the law's `when`.
pub fn whenName() -> AverStr {
    crate::proof_std::cancel_checkpoint();
    ::aver_rt::str_lit!("when")
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
    crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(
        crate::proof_std::aver_generated::kernel::proof::Proof__PHyp(::aver_rt::str_lit!("when")),
    )
}

/// The goal's claim in a case where the `when` evaluates to false: true = when = … = false, so the case cannot happen.
#[inline(always)]
pub fn absurdByWhen(
    s @ _: &Scope,
    mut goal @ _: crate::proof_std::aver_generated::kernel::proof::Goal,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::whenOf(&goal) {
        None => Err(::aver_rt::str_lit!("the law has no `when`")),
        Some(p @ _) => crate::proof_std::aver_generated::kernel::lib::whenRefutes(
            s,
            p,
            goal.obligation.lhs,
            goal.obligation.rhs,
        ),
    }
}

/// lhs = rhs in a case where the `when`, as it reads in that case, evaluates to false: true = premise = … = false, so the case cannot happen. Inside a list split the `when` reads with the split variable replaced, so pass it that way.
pub fn whenRefutes(
    s @ _: &Scope,
    mut premise @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut lhs @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut rhs @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let r @ _ = crate::proof_std::aver_generated::kernel::lib::evaluate(s, premise.clone())?;
    {
        let __t = r.to;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TBool(____pat0_fields) => {
                let __pat0 = ____pat0_fields.0.clone();
                match __pat0 {
        false => {
            Ok(crate::proof_std::aver_generated::kernel::proof::Proof::PAbsurd(crate::proof_std::aver_generated::kernel::proof::Proof__PAbsurd(crate::proof_std::aver_generated::kernel::proof::Proof::PTrans(crate::proof_std::aver_generated::kernel::proof::Proof__PTrans(aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool(true)), premise, crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool(false))]), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::proof::Proof::PSymm(crate::proof_std::aver_generated::kernel::proof::Proof__PSymm(crate::proof_std::aver_generated::kernel::lib::whenHolds())), r.proof]))), lhs, rhs)))
        },
        _ => {
            Err(::aver_rt::str_lit!("the `when` does not evaluate to false here"))
        }
    }
            }
            _ => Err(::aver_rt::str_lit!(
                "the `when` does not evaluate to false here"
            )),
        }
    }
}

/// t with the variable name replaced by value, as the kernel replaces a split variable in a list split's cases.
#[inline(always)]
pub fn substitute(
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    name @ _: AverStr,
    mut value @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<crate::proof_std::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let __arg3 @ _ = aver_rt::AverList::prepend_owned(
        crate::proof_std::aver_generated::kernel::term::bind(name, value),
        aver_rt::AverList::empty(),
    );
    crate::proof_std::aver_generated::kernel::subst::subst(t, &__arg3)
}

/// lhs = n = rhs when evaluate takes both sides to the same n.
pub fn bothSides(
    s @ _: &Scope,
    mut lhs @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut rhs @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let l @ _ = crate::proof_std::aver_generated::kernel::lib::evaluate(s, lhs.clone())?;
    let r @ _ = crate::proof_std::aver_generated::kernel::lib::evaluate(s, rhs.clone())?;
    if (l.to == r.to) {
        {
            let __arg5 @ _ = aver_rt::AverList::from_vec(vec![lhs, l.to.clone(), rhs]);
            {
                let __arg6 @ _ = aver_rt::AverList::from_vec(vec![
                    l.proof,
                    crate::proof_std::aver_generated::kernel::proof::Proof::PSymm(
                        crate::proof_std::aver_generated::kernel::proof::Proof__PSymm(r.proof),
                    ),
                ]);
                crate::proof_std::aver_generated::kernel::lib::trans(&__arg5, &__arg6)
            }
        }
    } else {
        Err(::aver_rt::str_lit!(
            "the two sides evaluate to different terms"
        ))
    }
}

/// t evaluated as far as the case hypotheses, list cells, the List.len and List.take equations, computation on literals and unfolding at a decided arm allow.
#[inline(always)]
pub fn evaluate(
    s @ _: &Scope,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::lib::evalWithin(s, t, aver_rt::AverInt::from_i64(400))
}

/// evaluate, nested at most depth deep.
#[inline(always)]
pub fn evalWithin(
    s @ _: &Scope,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
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
        Err(::aver_rt::str_lit!("evaluation nested too deep"))
    }
}

/// One node: its parts first, then one step at its head.
pub fn evalNode(
    s @ _: &Scope,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = &t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TVar(__x_fields) => {
                let x = __x_fields.0.clone();
                {
                    let __arg18 @ _ =
                        crate::proof_std::aver_generated::kernel::lib::lookupKnown(x, &s.known);
                    crate::proof_std::aver_generated::kernel::lib::evalVar(
                        s,
                        t,
                        (&__arg18).as_ref(),
                        depth,
                    )
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TList(____pat0_fields) => {
                let __pat0 = ____pat0_fields.0.clone();
                {
                    let __list_subject = __pat0;
                    match aver_rt::list_uncons(&__list_subject) {
                        Some((x, rest)) => Ok({
                            let __arg22 @ _ = crate::proof_std::aver_generated::kernel::lib::Rw { to: { let __arg19 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TList(crate::proof_std::aver_generated::kernel::term::Term__TList(rest.clone())); crate::proof_std::aver_generated::kernel::lib::cons(x.clone(), __arg19) }, proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCell(crate::proof_std::aver_generated::kernel::proof::Proof__PCell(t.clone())) };
                            {
                                let __arg23 @ _ = {
                                    let __arg21 @ _ = {
                                        let __arg20 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TList(crate::proof_std::aver_generated::kernel::term::Term__TList(rest));
                                        crate::proof_std::aver_generated::kernel::lib::cons(
                                            x.clone(),
                                            __arg20,
                                        )
                                    };
                                    crate::proof_std::aver_generated::kernel::lib::evalWithin(
                                        s, __arg21, depth,
                                    )
                                }?;
                                crate::proof_std::aver_generated::kernel::lib::chain(
                                    t, __arg22, __arg23,
                                )
                            }
                        }),
                        None => Ok(crate::proof_std::aver_generated::kernel::lib::refl(t)),
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TBi(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = __name_fields.1.clone();
                {
                    let __arg25 @ _ = {
                        let __arg24 @ _ = <aver_rt::AverList<
                            crate::proof_std::aver_generated::kernel::term::Term,
                        >>::empty();
                        crate::proof_std::aver_generated::kernel::lib::evalArgs(
                            s,
                            ::aver_rt::str_lit!("bi"),
                            name,
                            &__arg24,
                            &args,
                            depth.clone(),
                        )
                    }?;
                    crate::proof_std::aver_generated::kernel::lib::afterArgs(s, t, __arg25, depth)
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TCall(__f_fields) => {
                let f = __f_fields.0.clone();
                let args = __f_fields.1.clone();
                {
                    let __arg27 @ _ = {
                        let __arg26 @ _ = <aver_rt::AverList<
                            crate::proof_std::aver_generated::kernel::term::Term,
                        >>::empty();
                        crate::proof_std::aver_generated::kernel::lib::evalArgs(
                            s,
                            ::aver_rt::str_lit!("call"),
                            f,
                            &__arg26,
                            &args,
                            depth.clone(),
                        )
                    }?;
                    crate::proof_std::aver_generated::kernel::lib::afterArgs(s, t, __arg27, depth)
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                let o = __o_fields.0.clone();
                let a = &__o_fields.1;
                let b = &__o_fields.2;
                crate::proof_std::aver_generated::kernel::lib::evalOp(
                    s,
                    o,
                    a.clone(),
                    b.clone(),
                    depth,
                )
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TMatch(
                __subject_fields,
            ) => {
                let subject = __subject_fields.0.clone();
                let arms = __subject_fields.1.clone();
                crate::proof_std::aver_generated::kernel::lib::evalMatch(
                    s, t, subject, &arms, depth,
                )
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TGet(__r_fields) => {
                let r = __r_fields.0.clone();
                let f = __r_fields.1.clone();
                crate::proof_std::aver_generated::kernel::lib::evalGet(s, t, r, f, depth)
            }
            _ => Ok(crate::proof_std::aver_generated::kernel::lib::refl(t)),
        }
    }
}

/// A field read: the record evaluated in place, then the field of a record literal by projection.
pub fn evalGet(
    s @ _: &Scope,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_std::aver_generated::kernel::term::Term,
    f @ _: AverStr,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let rr @ _ = crate::proof_std::aver_generated::kernel::lib::evalWithin(s, r, depth.clone())?;
    let inner @ _ = if crate::proof_std::aver_generated::kernel::lib::isRefl(&rr.proof) {
        crate::proof_std::aver_generated::kernel::lib::refl(t.clone())
    } else {
        crate::proof_std::aver_generated::kernel::lib::Rw {
            to: crate::proof_std::aver_generated::kernel::term::Term::TGet(
                crate::proof_std::aver_generated::kernel::term::Term__TGet(
                    rr.to.clone(),
                    f.clone(),
                ),
            ),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
                crate::proof_std::aver_generated::kernel::proof::Proof__PCongr(
                    crate::proof_std::aver_generated::kernel::term::Term::TGet(
                        crate::proof_std::aver_generated::kernel::term::Term__TGet(
                            crate::proof_std::aver_generated::kernel::term::Term::THole,
                            f.clone(),
                        ),
                    ),
                    rr.proof,
                ),
            ),
        }
    };
    {
        let __t = rr.to;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TRec(__ty_fields) => {
                let ty = __ty_fields.0.clone();
                let fs = &__ty_fields.1;
                {
                    let __arg9 @ _ = crate::proof_std::aver_generated::kernel::lib::fieldOf(fs, f);
                    crate::proof_std::aver_generated::kernel::lib::projected(
                        s,
                        t,
                        inner,
                        (&__arg9).as_ref(),
                        depth,
                    )
                }
            }
            _ => Ok(inner),
        }
    }
}

/// t = the read of the literal = its field, evaluated further.
#[inline(always)]
pub fn projected(
    s @ _: &Scope,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut inner @ _: Rw,
    v @ _: Option<&crate::proof_std::aver_generated::kernel::term::Term>,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match v {
        None => Ok(inner),
        Some(x @ _) => Ok({
            let __arg8 @ _ = {
                let __arg6 @ _ = crate::proof_std::aver_generated::kernel::lib::Rw {
                    to: x.clone(),
                    proof: crate::proof_std::aver_generated::kernel::proof::Proof::PProj(
                        crate::proof_std::aver_generated::kernel::proof::Proof__PProj(
                            inner.to.clone(),
                        ),
                    ),
                };
                {
                    let __arg7 @ _ = crate::proof_std::aver_generated::kernel::lib::evalWithin(
                        s,
                        x.clone(),
                        depth,
                    )?;
                    crate::proof_std::aver_generated::kernel::lib::chain(
                        inner.to.clone(),
                        __arg6,
                        __arg7,
                    )
                }
            };
            crate::proof_std::aver_generated::kernel::lib::chain(t, inner, __arg8)
        }),
    }
}

/// A record literal's field by name.
#[inline(always)]
pub fn fieldOf(
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
    mut f @ _: AverStr,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>;
    let mut fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field> = fs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = fs;
            match aver_rt::list_uncons(__list_subject) {
                Some((x, rest)) => {
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
                }
                None => {
                    return None;
                }
            }
        }
    }
}

/// A match term: its subject evaluated, then the arm that value decides, evaluated further.
pub fn evalMatch(
    s @ _: &Scope,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut subject @ _: crate::proof_std::aver_generated::kernel::term::Term,
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
            {
                let __arg11 @ _ = {
                    let __arg10 @ _ = crate::proof_std::aver_generated::kernel::lib::patternBinds(
                        arm.pattern,
                        &parts,
                    );
                    crate::proof_std::aver_generated::kernel::subst::subst(arm.body, &__arg10)
                }?;
                {
                    let __arg12 @ _ = crate::proof_std::aver_generated::kernel::proof::Proof::PArm(
                        crate::proof_std::aver_generated::kernel::proof::Proof__PArm(
                            k,
                            parts,
                            t.clone(),
                            r.proof,
                        ),
                    );
                    crate::proof_std::aver_generated::kernel::lib::evalArm(
                        s, t, __arg11, __arg12, depth,
                    )
                }
            }
        }
    }
}

/// t = body by the arm step, then body evaluated.
pub fn evalArm(
    s @ _: &Scope,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut body @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut step @ _: crate::proof_std::aver_generated::kernel::proof::Proof,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    Ok({
        let __arg5 @ _ = crate::proof_std::aver_generated::kernel::lib::Rw {
            to: body.clone(),
            proof: step,
        };
        {
            let __arg6 @ _ =
                crate::proof_std::aver_generated::kernel::lib::evalWithin(s, body, depth)?;
            crate::proof_std::aver_generated::kernel::lib::chain(t, __arg5, __arg6)
        }
    })
}

/// A variable with a case hypothesis is its value, evaluated further.
#[inline(always)]
pub fn evalVar(
    s @ _: &Scope,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    known @ _: Option<&Known>,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match known {
        None => Ok(crate::proof_std::aver_generated::kernel::lib::refl(t)),
        Some(k @ _) => Ok({
            let __arg5 @ _ = crate::proof_std::aver_generated::kernel::lib::Rw {
                to: k.value.clone(),
                proof: crate::proof_std::aver_generated::kernel::proof::Proof::PHyp(
                    crate::proof_std::aver_generated::kernel::proof::Proof__PHyp(k.hyp.clone()),
                ),
            };
            {
                let __arg6 @ _ = crate::proof_std::aver_generated::kernel::lib::evalWithin(
                    s,
                    k.value.clone(),
                    depth,
                )?;
                crate::proof_std::aver_generated::kernel::lib::chain(t, __arg5, __arg6)
            }
        }),
    }
}

/// x in front of xs, as the kernel writes a list cell.
pub fn cons(
    mut x @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut xs @ _: crate::proof_std::aver_generated::kernel::term::Term,
) -> crate::proof_std::aver_generated::kernel::term::Term {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::term::Term::TBi(
        crate::proof_std::aver_generated::kernel::term::Term__TBi(
            ::aver_rt::str_lit!("List.prepend"),
            aver_rt::AverList::from_vec(vec![x, xs]),
        ),
    )
}

/// The latest case hypothesis about x.
#[inline(always)]
pub fn lookupKnown(mut x @ _: AverStr, ks @ _: &aver_rt::AverList<Known>) -> Option<Known> {
    let mut __hold1: aver_rt::AverList<Known>;
    let mut ks @ _: &aver_rt::AverList<Known> = ks;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = ks;
            match aver_rt::list_uncons(__list_subject) {
                Some((k, rest)) => {
                    if (k.name == x) {
                        return Some(k.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            ks = &__hold1;
                            continue;
                        }
                    }
                }
                None => {
                    return None;
                }
            }
        }
    }
}

/// A call of a definition or an application of a builtin.
#[inline(always)]
pub fn rebuild(
    kind @ _: &AverStr,
    name @ _: AverStr,
    xs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> crate::proof_std::aver_generated::kernel::term::Term {
    crate::proof_std::cancel_checkpoint();
    if (&**kind == "call") {
        crate::proof_std::aver_generated::kernel::term::Term::TCall(
            crate::proof_std::aver_generated::kernel::term::Term__TCall(name, xs.clone()),
        )
    } else {
        crate::proof_std::aver_generated::kernel::term::Term::TBi(
            crate::proof_std::aver_generated::kernel::term::Term__TBi(name, xs.clone()),
        )
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
    {
        let __list_subject = todo;
        match aver_rt::list_uncons(__list_subject) {
            Some((x, rest)) => {
                let __arg9 @ _ = crate::proof_std::aver_generated::kernel::lib::evalWithin(
                    s,
                    x.clone(),
                    depth.clone(),
                )?;
                crate::proof_std::aver_generated::kernel::lib::evalArg(
                    s, kind, name, done, x, &rest, __arg9, depth,
                )
            }
            None => Ok({
                let __arg8 @ _ =
                    crate::proof_std::aver_generated::kernel::lib::rebuild(&kind, name, done);
                crate::proof_std::aver_generated::kernel::lib::refl(__arg8)
            }),
        }
    }
}

/// One argument rewritten in place by congruence, then the arguments after it.
pub fn evalArg(
    s @ _: &Scope,
    kind @ _: AverStr,
    name @ _: AverStr,
    done @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    x @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    rest @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    mut r @ _: Rw,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let here @ _ = {
        let __arg10 @ _ =
            aver_rt::AverList::concat(&done.clone(), &aver_rt::AverList::prepend(x.clone(), rest));
        crate::proof_std::aver_generated::kernel::lib::rebuild(&kind, name.clone(), &__arg10)
    };
    let step @ _ = if crate::proof_std::aver_generated::kernel::lib::isRefl(&r.proof) {
        crate::proof_std::aver_generated::kernel::lib::refl(here.clone())
    } else {
        crate::proof_std::aver_generated::kernel::lib::Rw {
            to: {
                let __arg11 @ _ = aver_rt::AverList::concat(
                    &done.clone(),
                    &aver_rt::AverList::prepend(r.to.clone(), rest),
                );
                crate::proof_std::aver_generated::kernel::lib::rebuild(
                    &kind,
                    name.clone(),
                    &__arg11,
                )
            },
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
                crate::proof_std::aver_generated::kernel::proof::Proof__PCongr(
                    {
                        let __arg12 @ _ = aver_rt::AverList::concat(
                            &done.clone(),
                            &aver_rt::AverList::prepend(
                                crate::proof_std::aver_generated::kernel::term::Term::THole,
                                rest,
                            ),
                        );
                        crate::proof_std::aver_generated::kernel::lib::rebuild(
                            &kind,
                            name.clone(),
                            &__arg12,
                        )
                    },
                    r.proof,
                ),
            ),
        }
    };
    Ok({
        let __arg14 @ _ = {
            let __arg13 @ _ = aver_rt::AverList::concat(
                &done.clone(),
                &aver_rt::AverList::prepend_owned(r.to, aver_rt::AverList::empty()),
            );
            crate::proof_std::aver_generated::kernel::lib::evalArgs(
                s, kind, name, &__arg13, rest, depth,
            )
        }?;
        crate::proof_std::aver_generated::kernel::lib::chain(here, step, __arg14)
    })
}

/// With the arguments evaluated, one step at the head and the evaluation of what it gives.
#[inline(always)]
pub fn afterArgs(
    s @ _: &Scope,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut args @ _: Rw,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::headStep(
        s,
        args.to.clone(),
        depth.clone(),
    )? {
        None => Ok(args),
        Some(step @ _) => Ok({
            let __arg6 @ _ = {
                let __arg5 @ _ = crate::proof_std::aver_generated::kernel::lib::evalWithin(
                    s,
                    step.to.clone(),
                    depth,
                )?;
                crate::proof_std::aver_generated::kernel::lib::chain(args.to.clone(), step, __arg5)
            };
            crate::proof_std::aver_generated::kernel::lib::chain(t, args, __arg6)
        }),
    }
}

/// Both operands rewritten in place, then computed once both are literals.
pub fn evalOp(
    s @ _: &Scope,
    o @ _: AverStr,
    mut a @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut b @ _: crate::proof_std::aver_generated::kernel::term::Term,
    depth @ _: aver_rt::AverInt,
) -> Result<Rw, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let ra @ _ =
        crate::proof_std::aver_generated::kernel::lib::evalWithin(s, a.clone(), depth.clone())?;
    let rb @ _ = crate::proof_std::aver_generated::kernel::lib::evalWithin(s, b.clone(), depth)?;
    let t @ _ = crate::proof_std::aver_generated::kernel::term::Term::TOp(
        crate::proof_std::aver_generated::kernel::term::Term__TOp(o.clone(), a, b.clone()),
    );
    let mid @ _ = crate::proof_std::aver_generated::kernel::term::Term::TOp(
        crate::proof_std::aver_generated::kernel::term::Term__TOp(
            o.clone(),
            ra.to.clone(),
            b.clone(),
        ),
    );
    let end @ _ = crate::proof_std::aver_generated::kernel::term::Term::TOp(
        crate::proof_std::aver_generated::kernel::term::Term__TOp(o.clone(), ra.to.clone(), rb.to),
    );
    let first @ _ = if crate::proof_std::aver_generated::kernel::lib::isRefl(&ra.proof) {
        crate::proof_std::aver_generated::kernel::lib::refl(t.clone())
    } else {
        crate::proof_std::aver_generated::kernel::lib::Rw {
            to: mid.clone(),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
                crate::proof_std::aver_generated::kernel::proof::Proof__PCongr(
                    crate::proof_std::aver_generated::kernel::term::Term::TOp(
                        crate::proof_std::aver_generated::kernel::term::Term__TOp(
                            o.clone(),
                            crate::proof_std::aver_generated::kernel::term::Term::THole,
                            b,
                        ),
                    ),
                    ra.proof,
                ),
            ),
        }
    };
    let second @ _ = if crate::proof_std::aver_generated::kernel::lib::isRefl(&rb.proof) {
        crate::proof_std::aver_generated::kernel::lib::refl(mid)
    } else {
        crate::proof_std::aver_generated::kernel::lib::Rw {
            to: end.clone(),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCongr(
                crate::proof_std::aver_generated::kernel::proof::Proof__PCongr(
                    crate::proof_std::aver_generated::kernel::term::Term::TOp(
                        crate::proof_std::aver_generated::kernel::term::Term__TOp(
                            o,
                            ra.to,
                            crate::proof_std::aver_generated::kernel::term::Term::THole,
                        ),
                    ),
                    rb.proof,
                ),
            ),
        }
    };
    Ok({
        let __arg12 @ _ =
            crate::proof_std::aver_generated::kernel::lib::chain(t.clone(), first, second);
        {
            let __arg13 @ _ = crate::proof_std::aver_generated::kernel::lib::compute(end);
            crate::proof_std::aver_generated::kernel::lib::chain(t, __arg12, __arg13)
        }
    })
}

/// An Int operator between two Int literals, by computation; any other term is left as it is, since the kernel computes only closed literals.
#[inline(always)]
pub fn compute(mut t @ _: crate::proof_std::aver_generated::kernel::term::Term) -> Rw {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::literalValue(&t) {
        Some(v @ _) => crate::proof_std::aver_generated::kernel::lib::Rw {
            to: v.clone(),
            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(
                crate::proof_std::aver_generated::kernel::proof::Proof__PCompute(t, v),
            ),
        },
        None => crate::proof_std::aver_generated::kernel::lib::refl(t),
    }
}

/// The value of a closed Int or Bool term, by the kernel's own evaluator for the compute rule (not a copy of it).
pub fn literalValue(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                let o = __o_fields.0.clone();
                let __pat0 = &__o_fields.1;
                let __pat1 = &__o_fields.2;
                {
                    let __t = __pat0;
                    match __t.view() {
                        crate::proof_std::aver_generated::kernel::term::Term__view::TInt(
                            ____pat2_fields,
                        ) => {
                            let __pat2 = ____pat2_fields.0.clone();
                            {
                                let __t = __pat1;
                                match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TInt(____pat3_fields) => {
            let __pat3 = ____pat3_fields.0.clone();
            crate::proof_std::aver_generated::kernel::lib::intOp(o, __pat2, __pat3)
        },
        _ => {
            { let __arg8 @ _ = crate::proof_std::aver_generated::kernel::eval::evalClosed(t); crate::proof_std::aver_generated::kernel::lib::scalarOf((&__arg8).as_ref()) }
        }
    }
                            }
                        }
                        _ => {
                            let __arg9 @ _ =
                                crate::proof_std::aver_generated::kernel::eval::evalClosed(t);
                            crate::proof_std::aver_generated::kernel::lib::scalarOf(
                                (&__arg9).as_ref(),
                            )
                        }
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TBi(__n_fields) => {
                let n = __n_fields.0.clone();
                let xs = __n_fields.1.clone();
                {
                    let __arg10 @ _ = crate::proof_std::aver_generated::kernel::eval::evalClosed(t);
                    crate::proof_std::aver_generated::kernel::lib::scalarOf((&__arg10).as_ref())
                }
            }
            _ => None,
        }
    }
}

/// An Int or Bool value as a literal; a list value is left alone, since evaluate writes lists as cells.
pub fn scalarOf(
    v @ _: Option<&crate::proof_std::aver_generated::kernel::eval::Val>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match v {
        Some(__pat0) => match __pat0 {
            crate::proof_std::aver_generated::kernel::eval::Val::VInt(n) => {
                let n = n.clone();
                Some(crate::proof_std::aver_generated::kernel::term::Term::TInt(
                    crate::proof_std::aver_generated::kernel::term::Term__TInt(n),
                ))
            }
            crate::proof_std::aver_generated::kernel::eval::Val::VBool(b) => {
                let b = b.clone();
                Some(crate::proof_std::aver_generated::kernel::term::Term::TBool(
                    crate::proof_std::aver_generated::kernel::term::Term__TBool(b),
                ))
            }
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
                crate::proof_std::aver_generated::kernel::term::Term__TInt(x.add(&y)),
            ))
        } else {
            if &*__dispatch_subject == "-" {
                Some(crate::proof_std::aver_generated::kernel::term::Term::TInt(
                    crate::proof_std::aver_generated::kernel::term::Term__TInt(x.sub(&y)),
                ))
            } else {
                if &*__dispatch_subject == "*" {
                    Some(crate::proof_std::aver_generated::kernel::term::Term::TInt(
                        crate::proof_std::aver_generated::kernel::term::Term__TInt(x.mul(&y)),
                    ))
                } else {
                    if &*__dispatch_subject == "<=" {
                        Some(crate::proof_std::aver_generated::kernel::term::Term::TBool(
                            crate::proof_std::aver_generated::kernel::term::Term__TBool((x <= y)),
                        ))
                    } else {
                        if &*__dispatch_subject == "<" {
                            Some(crate::proof_std::aver_generated::kernel::term::Term::TBool(
                                crate::proof_std::aver_generated::kernel::term::Term__TBool(
                                    (x < y),
                                ),
                            ))
                        } else {
                            if &*__dispatch_subject == ">=" {
                                Some(crate::proof_std::aver_generated::kernel::term::Term::TBool(
                                    crate::proof_std::aver_generated::kernel::term::Term__TBool(
                                        (x >= y),
                                    ),
                                ))
                            } else {
                                if &*__dispatch_subject == ">" {
                                    Some(crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool((x > y))))
                                } else {
                                    if &*__dispatch_subject == "==" {
                                        Some(crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool((x == y))))
                                    } else {
                                        if &*__dispatch_subject == "!=" {
                                            Some(crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool((x != y))))
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
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
    depth @ _: aver_rt::AverInt,
) -> Result<Option<Rw>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = &t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TBi(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = __name_fields.1.clone();
                Ok(crate::proof_std::aver_generated::kernel::lib::builtinStep(
                    t, name, &args,
                ))
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TCall(__f_fields) => {
                let f = __f_fields.0.clone();
                let xs = &__f_fields.1;
                crate::proof_std::aver_generated::kernel::lib::unfoldWithin(s, f, xs, depth)
            }
            _ => Ok(None),
        }
    }
}

/// A list equation, or, for a builtin other than List.prepend applied to closed values, its value by computation.
#[inline(always)]
pub fn builtinStep(
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Term,
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
                match crate::proof_std::aver_generated::kernel::lib::literalValue(&t) {
                    Some(v @ _) => Some(crate::proof_std::aver_generated::kernel::lib::Rw {
                        to: v.clone(),
                        proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(
                            crate::proof_std::aver_generated::kernel::proof::Proof__PCompute(t, v),
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
                    match aver_rt::list_uncons(&__list_subject) {
                        Some((a, __pat2)) => {
                            let __t = a;
                            match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TBool(____pat3_fields) => {
            let __pat3 = ____pat3_fields.0.clone();
            if __pat3 { { let __list_subject = __pat2; match aver_rt::list_uncons(&__list_subject) { Some((b, __pat4)) => { { let __list_subject = __pat4; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: b.clone(), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("bool.and.true_l"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("b"), b.clone()), aver_rt::AverList::empty()), aver_rt::AverList::empty())) }) } else { None } } }, None => { None } } } } else { { let __list_subject = __pat2; match aver_rt::list_uncons(&__list_subject) { Some((b, __pat5)) => { { let __list_subject = __pat5; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool(false)), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("bool.and.false_l"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("b"), b.clone()), aver_rt::AverList::empty()), aver_rt::AverList::empty())) }) } else { None } } }, None => { None } } } }
        },
        _ => {
            { let __list_subject = __pat2; match aver_rt::list_uncons(&__list_subject) { Some((__pat6, __pat7)) => { { let __t = __pat6; match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TBool(____pat8_fields) => {
            let __pat8 = ____pat8_fields.0.clone();
            if __pat8 { { let __list_subject = __pat7; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: a.clone(), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("bool.and.true_r"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("a"), a.clone()), aver_rt::AverList::empty()), aver_rt::AverList::empty())) }) } else { None } } } else { { let __list_subject = __pat7; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool(false)), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("bool.and.false_r"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("a"), a.clone()), aver_rt::AverList::empty()), aver_rt::AverList::empty())) }) } else { None } } }
        },
        _ => {
            None
        }
    } } }, None => { None } } }
        }
    }
                        }
                        None => None,
                    }
                }
            } else {
                if &*__dispatch_subject == "Bool.or" {
                    {
                        let __list_subject = __pat1;
                        match aver_rt::list_uncons(&__list_subject) {
                            Some((a, __pat9)) => {
                                let __t = a;
                                match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TBool(____pat10_fields) => {
            let __pat10 = ____pat10_fields.0.clone();
            if __pat10 { { let __list_subject = __pat9; match aver_rt::list_uncons(&__list_subject) { Some((b, __pat11)) => { { let __list_subject = __pat11; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool(true)), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("bool.or.true_l"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("b"), b.clone()), aver_rt::AverList::empty()), aver_rt::AverList::empty())) }) } else { None } } }, None => { None } } } } else { { let __list_subject = __pat9; match aver_rt::list_uncons(&__list_subject) { Some((b, __pat12)) => { { let __list_subject = __pat12; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: b.clone(), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("bool.or.false_l"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("b"), b.clone()), aver_rt::AverList::empty()), aver_rt::AverList::empty())) }) } else { None } } }, None => { None } } } }
        },
        _ => {
            { let __list_subject = __pat9; match aver_rt::list_uncons(&__list_subject) { Some((__pat13, __pat14)) => { { let __t = __pat13; match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TBool(____pat15_fields) => {
            let __pat15 = ____pat15_fields.0.clone();
            if __pat15 { { let __list_subject = __pat14; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool(true)), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("bool.or.true_r"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("a"), a.clone()), aver_rt::AverList::empty()), aver_rt::AverList::empty())) }) } else { None } } } else { { let __list_subject = __pat14; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: a.clone(), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("bool.or.false_r"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("a"), a.clone()), aver_rt::AverList::empty()), aver_rt::AverList::empty())) }) } else { None } } }
        },
        _ => {
            None
        }
    } } }, None => { None } } }
        }
    }
                            }
                            None => None,
                        }
                    }
                } else {
                    None
                }
            }
        }
    }
}

/// The equations of List.len, List.take, List.concat and List.reverse on [] and on a cell, as the kernel's rules state them, and as compute shows them where the call is closed.
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
                    match aver_rt::list_uncons(&__list_subject) {
                        Some((__pat2, __pat3)) => {
                            let __t = __pat2;
                            match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TList(____pat4_fields) => {
            let __pat4 = &____pat4_fields.0;
            { let __list_subject = __pat4; if __list_subject.is_empty() { { let __list_subject = __pat3; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TInt(crate::proof_std::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(crate::proof_std::aver_generated::kernel::proof::Proof__PCompute(crate::proof_std::aver_generated::kernel::term::Term::TBi(crate::proof_std::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.len"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::Term::TList(crate::proof_std::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty())), aver_rt::AverList::empty()))), crate::proof_std::aver_generated::kernel::term::Term::TInt(crate::proof_std::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))) }) } else { None } } } else { None } }
        },
        crate::proof_std::aver_generated::kernel::term::Term__view::TBi(____pat5_fields) => {
            let __pat5 = ____pat5_fields.0.clone();
            let __pat6 = &____pat5_fields.1;
            match &*__pat5 {
        "List.prepend" => {
            { let __list_subject = __pat6; match aver_rt::list_uncons(__list_subject) { Some((x, __pat7)) => { { let __list_subject = __pat7; match aver_rt::list_uncons(&__list_subject) { Some((a, __pat8)) => { { let __list_subject = __pat8; if __list_subject.is_empty() { { let __list_subject = __pat3; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TOp(crate::proof_std::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("+"), crate::proof_std::aver_generated::kernel::term::Term::TBi(crate::proof_std::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.len"), aver_rt::AverList::prepend_owned(a.clone(), aver_rt::AverList::empty()))), crate::proof_std::aver_generated::kernel::term::Term::TInt(crate::proof_std::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(1))))), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("list.len.cons"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("x"), x.clone()), crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("a"), a.clone())]), aver_rt::AverList::empty())) }) } else { None } } } else { None } } }, None => { None } } } }, None => { None } } }
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
                        }
                        None => None,
                    }
                }
            } else {
                if &*__dispatch_subject == "List.take" {
                    {
                        let __list_subject = __pat1;
                        match aver_rt::list_uncons(&__list_subject) {
                            Some((__pat9, __pat10)) => {
                                let __t = __pat9;
                                match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TList(____pat11_fields) => {
            let __pat11 = &____pat11_fields.0;
            { let __list_subject = __pat11; if __list_subject.is_empty() { { let __list_subject = __pat10; match aver_rt::list_uncons(&__list_subject) { Some((n, __pat12)) => { { let __list_subject = __pat12; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TList(crate::proof_std::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty())), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("list.take.nil"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("n"), n.clone()), aver_rt::AverList::empty()), aver_rt::AverList::empty())) }) } else { None } } }, None => { None } } } } else { None } }
        },
        crate::proof_std::aver_generated::kernel::term::Term__view::TBi(____pat13_fields) => {
            let __pat13 = ____pat13_fields.0.clone();
            let __pat14 = &____pat13_fields.1;
            match &*__pat13 {
        "List.prepend" => {
            { let __list_subject = __pat14; match aver_rt::list_uncons(__list_subject) { Some((x, __pat15)) => { { let __list_subject = __pat15; match aver_rt::list_uncons(&__list_subject) { Some((a, __pat16)) => { { let __list_subject = __pat16; if __list_subject.is_empty() { { let __list_subject = __pat10; match aver_rt::list_uncons(&__list_subject) { Some((__pat17, __pat18)) => { { let __t = __pat17; match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TInt(__n_fields) => {
            let n = __n_fields.0.clone();
            { let __list_subject = __pat18; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::takeCell(x.clone(), a.clone(), n)) } else { None } }
        },
        _ => {
            None
        }
    } } }, None => { None } } } } else { None } } }, None => { None } } } }, None => { None } } }
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
                            }
                            None => None,
                        }
                    }
                } else {
                    if &*__dispatch_subject == "List.concat" {
                        {
                            let __list_subject = __pat1;
                            match aver_rt::list_uncons(&__list_subject) {
                                Some((__pat19, __pat20)) => {
                                    let __t = __pat19;
                                    match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TList(____pat21_fields) => {
            let __pat21 = &____pat21_fields.0;
            { let __list_subject = __pat21; if __list_subject.is_empty() { { let __list_subject = __pat20; match aver_rt::list_uncons(&__list_subject) { Some((b, __pat22)) => { { let __list_subject = __pat22; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: b.clone(), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("list.concat.nil"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("b"), b.clone()), aver_rt::AverList::empty()), aver_rt::AverList::empty())) }) } else { None } } }, None => { None } } } } else { None } }
        },
        crate::proof_std::aver_generated::kernel::term::Term__view::TBi(____pat23_fields) => {
            let __pat23 = ____pat23_fields.0.clone();
            let __pat24 = &____pat23_fields.1;
            match &*__pat23 {
        "List.prepend" => {
            { let __list_subject = __pat24; match aver_rt::list_uncons(__list_subject) { Some((x, __pat25)) => { { let __list_subject = __pat25; match aver_rt::list_uncons(&__list_subject) { Some((a, __pat26)) => { { let __list_subject = __pat26; if __list_subject.is_empty() { { let __list_subject = __pat20; match aver_rt::list_uncons(&__list_subject) { Some((b, __pat27)) => { { let __list_subject = __pat27; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: { let __arg49 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TBi(crate::proof_std::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.concat"), aver_rt::AverList::from_vec(vec![a.clone(), b.clone()]))); crate::proof_std::aver_generated::kernel::lib::cons(x.clone(), __arg49) }, proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("list.concat.cons"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("x"), x.clone()), crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("a"), a.clone()), crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("b"), b.clone())]), aver_rt::AverList::empty())) }) } else { None } } }, None => { None } } } } else { None } } }, None => { None } } } }, None => { None } } }
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
                                }
                                None => None,
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "List.reverse" {
                            {
                                let __list_subject = __pat1;
                                match aver_rt::list_uncons(&__list_subject) {
                                    Some((__pat28, __pat29)) => {
                                        let __t = __pat28;
                                        match __t.view() {
        crate::proof_std::aver_generated::kernel::term::Term__view::TList(____pat30_fields) => {
            let __pat30 = &____pat30_fields.0;
            { let __list_subject = __pat30; if __list_subject.is_empty() { { let __list_subject = __pat29; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TList(crate::proof_std::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty())), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(crate::proof_std::aver_generated::kernel::proof::Proof__PCompute(crate::proof_std::aver_generated::kernel::term::Term::TBi(crate::proof_std::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.reverse"), aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::term::Term::TList(crate::proof_std::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty())), aver_rt::AverList::empty()))), crate::proof_std::aver_generated::kernel::term::Term::TList(crate::proof_std::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty())))) }) } else { None } } } else { None } }
        },
        crate::proof_std::aver_generated::kernel::term::Term__view::TBi(____pat31_fields) => {
            let __pat31 = ____pat31_fields.0.clone();
            let __pat32 = &____pat31_fields.1;
            match &*__pat31 {
        "List.prepend" => {
            { let __list_subject = __pat32; match aver_rt::list_uncons(__list_subject) { Some((x, __pat33)) => { { let __list_subject = __pat33; match aver_rt::list_uncons(&__list_subject) { Some((a, __pat34)) => { { let __list_subject = __pat34; if __list_subject.is_empty() { { let __list_subject = __pat29; if __list_subject.is_empty() { Some(crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TBi(crate::proof_std::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.concat"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::Term::TBi(crate::proof_std::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.reverse"), aver_rt::AverList::prepend_owned(a.clone(), aver_rt::AverList::empty()))), { let __arg50 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TList(crate::proof_std::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty())); crate::proof_std::aver_generated::kernel::lib::cons(x.clone(), __arg50) }]))), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("list.reverse.cons"), aver_rt::AverList::from_vec(vec![crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("x"), x.clone()), crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("a"), a.clone())]), aver_rt::AverList::empty())) }) } else { None } } } else { None } } }, None => { None } } } }, None => { None } } }
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
                                    }
                                    None => None,
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
    mut x @ _: crate::proof_std::aver_generated::kernel::term::Term,
    mut a @ _: crate::proof_std::aver_generated::kernel::term::Term,
    n @ _: aver_rt::AverInt,
) -> Rw {
    crate::proof_std::cancel_checkpoint();
    let bs @ _ = aver_rt::AverList::from_vec(vec![
        crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("x"), x.clone()),
        crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("a"), a.clone()),
        {
            let __arg4 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TInt(
                crate::proof_std::aver_generated::kernel::term::Term__TInt(n.clone()),
            );
            crate::proof_std::aver_generated::kernel::term::bind(::aver_rt::str_lit!("n"), __arg4)
        },
    ]);
    if (n > aver_rt::AverInt::from_i64(0)) {
        crate::proof_std::aver_generated::kernel::lib::Rw { to: { let __arg5 @ _ = crate::proof_std::aver_generated::kernel::term::Term::TBi(crate::proof_std::aver_generated::kernel::term::Term__TBi(::aver_rt::str_lit!("List.take"), aver_rt::AverList::from_vec(vec![a, crate::proof_std::aver_generated::kernel::term::Term::TOp(crate::proof_std::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("-"), crate::proof_std::aver_generated::kernel::term::Term::TInt(crate::proof_std::aver_generated::kernel::term::Term__TInt(n.clone())), crate::proof_std::aver_generated::kernel::term::Term::TInt(crate::proof_std::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(1)))))]))); crate::proof_std::aver_generated::kernel::lib::cons(x, __arg5) }, proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("list.take.cons_gt"), bs, aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(crate::proof_std::aver_generated::kernel::proof::Proof__PCompute(crate::proof_std::aver_generated::kernel::term::Term::TOp(crate::proof_std::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!(">"), crate::proof_std::aver_generated::kernel::term::Term::TInt(crate::proof_std::aver_generated::kernel::term::Term__TInt(n)), crate::proof_std::aver_generated::kernel::term::Term::TInt(crate::proof_std::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))), crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool(true)))), aver_rt::AverList::empty()))) }
    } else {
        crate::proof_std::aver_generated::kernel::lib::Rw { to: crate::proof_std::aver_generated::kernel::term::Term::TList(crate::proof_std::aver_generated::kernel::term::Term__TList(aver_rt::AverList::empty())), proof: crate::proof_std::aver_generated::kernel::proof::Proof::PRule(crate::proof_std::aver_generated::kernel::proof::Proof__PRule(::aver_rt::str_lit!("list.take.cons_le"), bs, aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::proof::Proof::PCompute(crate::proof_std::aver_generated::kernel::proof::Proof__PCompute(crate::proof_std::aver_generated::kernel::term::Term::TOp(crate::proof_std::aver_generated::kernel::term::Term__TOp(::aver_rt::str_lit!("<="), crate::proof_std::aver_generated::kernel::term::Term::TInt(crate::proof_std::aver_generated::kernel::term::Term__TInt(n)), crate::proof_std::aver_generated::kernel::term::Term::TInt(crate::proof_std::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))), crate::proof_std::aver_generated::kernel::term::Term::TBool(crate::proof_std::aver_generated::kernel::term::Term__TBool(true)))), aver_rt::AverList::empty()))) }
    }
}

/// A definition by name.
#[inline(always)]
pub fn findDef(
    mut f @ _: AverStr,
    ds @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Def>,
) -> Option<crate::proof_std::aver_generated::kernel::proof::Def> {
    let mut __hold1: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Def>;
    let mut ds @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Def> = ds;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = ds;
            match aver_rt::list_uncons(__list_subject) {
                Some((d, rest)) => {
                    if (d.name == f) {
                        return Some(d.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            ds = &__hold1;
                            continue;
                        }
                    }
                }
                None => {
                    return None;
                }
            }
        }
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
            aver_rt::AverList::prepend_owned(
                crate::proof_std::aver_generated::kernel::term::bind(n, x),
                crate::proof_std::aver_generated::kernel::lib::zipBind(&ns2, &xs2),
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
        {
            let __arg3 @ _ = ({
                let __arg2 @ _ =
                    crate::proof_std::aver_generated::kernel::proof::givenNames(&d.params);
                crate::proof_std::aver_generated::kernel::lib::zipBind(&__arg2, args)
            })
            .reverse_owned();
            crate::proof_std::aver_generated::kernel::lib::bindLets(&d.lets, __arg3)
        }
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(49usize);
                        __b.push_display(&(d.name));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" takes "));
                    __b
                };
                __b.push_display(&(aver_rt::AverInt::from_i64(d.params.len() as i64)));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(" arguments"));
            __b
        }))
    }
}

/// Each local binding with what comes before it substituted, put in front.
#[inline(always)]
pub fn bindLets(
    lets @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    mut outer @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>;
    let mut lets @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding> =
        lets;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = lets;
            match aver_rt::list_uncons(__list_subject) {
                Some((b, rest)) => {
                    let __tco0 = rest;
                    let __tco1 = aver_rt::AverList::prepend_owned(
                        {
                            let __arg4 @ _ =
                                crate::proof_std::aver_generated::kernel::subst::subst(
                                    b.value.clone(),
                                    &outer,
                                )?;
                            crate::proof_std::aver_generated::kernel::term::bind(
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
                None => {
                    return Ok(outer);
                }
            }
        }
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
    match crate::proof_std::aver_generated::kernel::lib::findDef(f, &s.defs) {
        None => Ok(None),
        Some(d @ _) => {
            let __t = &d.body;
            match __t.view() {
                crate::proof_std::aver_generated::kernel::term::Term__view::TMatch(
                    __subject_fields,
                ) => {
                    let subject = __subject_fields.0.clone();
                    let arms = __subject_fields.1.clone();
                    {
                        let __arg7 @ _ =
                            crate::proof_std::aver_generated::kernel::lib::letBindings(&d, args)?;
                        crate::proof_std::aver_generated::kernel::lib::unfoldAt(
                            s, &d, args, subject, &arms, &__arg7, depth,
                        )
                    }
                }
                _ => {
                    if crate::proof_std::aver_generated::kernel::lib::callsName(
                        &d.body,
                        d.name.clone(),
                    ) {
                        Ok(None)
                    } else {
                        Ok(Some(crate::proof_std::aver_generated::kernel::lib::Rw {
                            to: {
                                let __arg8 @ _ =
                                    crate::proof_std::aver_generated::kernel::lib::letBindings(
                                        &d, args,
                                    )?;
                                crate::proof_std::aver_generated::kernel::subst::subst(
                                    d.body.clone(),
                                    &__arg8,
                                )
                            }?,
                            proof: crate::proof_std::aver_generated::kernel::proof::Proof::PUnfold(
                                crate::proof_std::aver_generated::kernel::proof::Proof__PUnfold(
                                    d.name,
                                    aver_rt::AverInt::from_i64(0),
                                    args.clone(),
                                    aver_rt::AverList::empty(),
                                    aver_rt::AverList::empty(),
                                ),
                            ),
                        }))
                    }
                }
            }
        }
    }
}

/// Whether t calls the definition f anywhere: a body that calls itself is not opened whole.
pub fn callsName(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    mut f @ _: AverStr,
) -> bool {
    let mut __hold0: crate::proof_std::aver_generated::kernel::term::Term;
    let mut t @ _: &crate::proof_std::aver_generated::kernel::term::Term = t;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __t = t;
            match __t.view() {
                crate::proof_std::aver_generated::kernel::term::Term__view::TCall(__g_fields) => {
                    let g = __g_fields.0.clone();
                    let xs = &__g_fields.1;
                    return ((g == f)
                        || crate::proof_std::aver_generated::kernel::lib::callsAny(xs, f));
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TBi(__g_fields) => {
                    let g = __g_fields.0.clone();
                    let xs = &__g_fields.1;
                    return crate::proof_std::aver_generated::kernel::lib::callsAny(xs, f);
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                    let o = __o_fields.0.clone();
                    let a = &__o_fields.1;
                    let b = &__o_fields.2;
                    return (crate::proof_std::aver_generated::kernel::lib::callsName(
                        a,
                        f.clone(),
                    ) || crate::proof_std::aver_generated::kernel::lib::callsName(b, f));
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                    let a = &__a_fields.0;
                    {
                        let __tco0 = a.clone();
                        __hold0 = __tco0;
                        t = &__hold0;
                        continue;
                    }
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TGet(__a_fields) => {
                    let a = &__a_fields.0;
                    let n = __a_fields.1.clone();
                    {
                        let __tco0 = a.clone();
                        __hold0 = __tco0;
                        t = &__hold0;
                        continue;
                    }
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TCtor(__c_fields) => {
                    let c = __c_fields.0.clone();
                    let xs = &__c_fields.1;
                    return crate::proof_std::aver_generated::kernel::lib::callsAny(xs, f);
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TList(__xs_fields) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_std::aver_generated::kernel::lib::callsAny(xs, f);
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TTuple(__xs_fields) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_std::aver_generated::kernel::lib::callsAny(xs, f);
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TParts(__xs_fields) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_std::aver_generated::kernel::lib::callsAny(xs, f);
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TRec(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let fs = &__n_fields.1;
                    return crate::proof_std::aver_generated::kernel::lib::callsFields(fs, f);
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TUpd(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let a = &__n_fields.1;
                    let fs = &__n_fields.2;
                    return (crate::proof_std::aver_generated::kernel::lib::callsName(
                        a,
                        f.clone(),
                    ) || crate::proof_std::aver_generated::kernel::lib::callsFields(
                        fs, f,
                    ));
                }
                crate::proof_std::aver_generated::kernel::term::Term__view::TMatch(__a_fields) => {
                    let a = &__a_fields.0;
                    let arms = &__a_fields.1;
                    return (crate::proof_std::aver_generated::kernel::lib::callsName(
                        a,
                        f.clone(),
                    ) || crate::proof_std::aver_generated::kernel::lib::callsArms(
                        arms, f,
                    ));
                }
                _ => {
                    return false;
                }
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
    {
        let __list_subject = xs;
        match aver_rt::list_uncons(__list_subject) {
            Some((x, rest)) => {
                (crate::proof_std::aver_generated::kernel::lib::callsName(x, f.clone())
                    || crate::proof_std::aver_generated::kernel::lib::callsAny(&rest, f))
            }
            None => false,
        }
    }
}

/// Whether any field value calls f.
#[inline(always)]
pub fn callsFields(
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
    f @ _: AverStr,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = fs;
        match aver_rt::list_uncons(__list_subject) {
            Some((x, rest)) => {
                (crate::proof_std::aver_generated::kernel::lib::callsName(&x.value, f.clone())
                    || crate::proof_std::aver_generated::kernel::lib::callsFields(&rest, f))
            }
            None => false,
        }
    }
}

/// Whether any arm body calls f.
#[inline(always)]
pub fn callsArms(
    arms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
    f @ _: AverStr,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = arms;
        match aver_rt::list_uncons(__list_subject) {
            Some((a, rest)) => {
                (crate::proof_std::aver_generated::kernel::lib::callsName(&a.body, f.clone())
                    || crate::proof_std::aver_generated::kernel::lib::callsArms(&rest, f))
            }
            None => false,
        }
    }
}

/// Evaluate the subject, choose the arm, and write the unfold with the subject's evaluation as its premise.
pub fn unfoldAt(
    s @ _: &Scope,
    d @ _: &crate::proof_std::aver_generated::kernel::proof::Def,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    mut subject @ _: crate::proof_std::aver_generated::kernel::term::Term,
    arms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
    outer @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    depth @ _: aver_rt::AverInt,
) -> Result<Option<Rw>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let r @ _ = {
        let __arg12 @ _ = crate::proof_std::aver_generated::kernel::subst::subst(subject, outer)?;
        crate::proof_std::aver_generated::kernel::lib::evalWithin(s, __arg12, depth)
    }?;
    match crate::proof_std::aver_generated::kernel::lib::selectArm(arms, r.to) {
        None => Ok(None),
        Some(__pat0 @ _) => {
            let (k, arm, parts) = __pat0;
            Ok(Some(crate::proof_std::aver_generated::kernel::lib::Rw {
                to: {
                    let __arg13 @ _ = aver_rt::AverList::concat(
                        &crate::proof_std::aver_generated::kernel::lib::patternBinds(
                            arm.pattern,
                            &parts,
                        ),
                        &outer.clone(),
                    );
                    crate::proof_std::aver_generated::kernel::subst::subst(arm.body, &__arg13)
                }?,
                proof: crate::proof_std::aver_generated::kernel::proof::Proof::PUnfold(
                    crate::proof_std::aver_generated::kernel::proof::Proof__PUnfold(
                        d.name.clone(),
                        k,
                        args.clone(),
                        parts,
                        aver_rt::AverList::prepend_owned(r.proof, aver_rt::AverList::empty()),
                    ),
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
    {
        let __t = v;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TList(____pat0_fields) => {
                let __pat0 = &____pat0_fields.0;
                {
                    let __list_subject = __pat0;
                    if __list_subject.is_empty() {
                        ::aver_rt::str_lit!("nil")
                    } else {
                        if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) {
                            ::aver_rt::str_lit!("lit")
                        } else {
                            ::aver_rt::str_lit!("other")
                        }
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TBi(____pat1_fields) => {
                let __pat1 = ____pat1_fields.0.clone();
                let __pat2 = &____pat1_fields.1;
                match &*__pat1 {
                    "List.prepend" => {
                        let __list_subject = __pat2;
                        match aver_rt::list_uncons_cloned(__list_subject) {
                            Some((h, __pat3)) => {
                                let __list_subject = __pat3;
                                match aver_rt::list_uncons_cloned(&__list_subject) {
                                    Some((t, __pat4)) => {
                                        let __list_subject = __pat4;
                                        if __list_subject.is_empty() {
                                            ::aver_rt::str_lit!("cons")
                                        } else {
                                            if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) { ::aver_rt::str_lit!("lit") } else { ::aver_rt::str_lit!("other") }
                                        }
                                    }
                                    None => {
                                        if crate::proof_std::aver_generated::kernel::lib::isLiteral(
                                            v,
                                        ) {
                                            ::aver_rt::str_lit!("lit")
                                        } else {
                                            ::aver_rt::str_lit!("other")
                                        }
                                    }
                                }
                            }
                            None => {
                                if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) {
                                    ::aver_rt::str_lit!("lit")
                                } else {
                                    ::aver_rt::str_lit!("other")
                                }
                            }
                        }
                    }
                    _ => {
                        if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) {
                            ::aver_rt::str_lit!("lit")
                        } else {
                            ::aver_rt::str_lit!("other")
                        }
                    }
                }
            }
            _ => {
                if crate::proof_std::aver_generated::kernel::lib::isLiteral(v) {
                    ::aver_rt::str_lit!("lit")
                } else {
                    ::aver_rt::str_lit!("other")
                }
            }
        }
    }
}

/// The head and tail of a list cell.
pub fn cellParts(
    v @ _: &crate::proof_std::aver_generated::kernel::term::Term,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TBi(__n_fields) => {
                let n = __n_fields.0.clone();
                let xs = &__n_fields.1;
                xs.clone()
            }
            _ => aver_rt::AverList::empty(),
        }
    }
}

/// An Int, Bool or String literal.
pub fn isLiteral(t @ _: &crate::proof_std::aver_generated::kernel::term::Term) -> bool {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TInt(__i_fields) => {
                let i = __i_fields.0.clone();
                true
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TBool(__b_fields) => {
                let b = __b_fields.0.clone();
                true
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TStr(__s_fields) => {
                let s = __s_fields.0.clone();
                true
            }
            _ => false,
        }
    }
}

/// The names a cell pattern or a named catch-all binds.
pub fn patternBinds(
    mut p @ _: crate::proof_std::aver_generated::kernel::term::Pat,
    parts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding> {
    crate::proof_std::cancel_checkpoint();
    {
        let (__pat0, ys) = (p, parts.clone());
        match __pat0 {
            crate::proof_std::aver_generated::kernel::term::Pat::PCons(h, t) => {
                let __list_subject = ys;
                match aver_rt::list_uncons(&__list_subject) {
                    Some((x, __pat1)) => {
                        let __list_subject = __pat1;
                        match aver_rt::list_uncons(&__list_subject) {
                            Some((y, __pat2)) => {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    aver_rt::AverList::from_vec(vec![
                                        crate::proof_std::aver_generated::kernel::term::bind(
                                            h,
                                            x.clone(),
                                        ),
                                        crate::proof_std::aver_generated::kernel::term::bind(
                                            t,
                                            y.clone(),
                                        ),
                                    ])
                                } else {
                                    aver_rt::AverList::empty()
                                }
                            }
                            None => aver_rt::AverList::empty(),
                        }
                    }
                    None => aver_rt::AverList::empty(),
                }
            }
            crate::proof_std::aver_generated::kernel::term::Pat::PVar(n) => {
                let __list_subject = ys;
                match aver_rt::list_uncons(&__list_subject) {
                    Some((x, __pat3)) => {
                        let __list_subject = __pat3;
                        if __list_subject.is_empty() {
                            aver_rt::AverList::prepend_owned(
                                crate::proof_std::aver_generated::kernel::term::bind(n, x.clone()),
                                aver_rt::AverList::empty(),
                            )
                        } else {
                            aver_rt::AverList::empty()
                        }
                    }
                    None => aver_rt::AverList::empty(),
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
                    return aver_rt::AverList::prepend_owned(
                        crate::proof_std::aver_generated::kernel::term::bind(n, y),
                        crate::proof_std::aver_generated::kernel::lib::fieldBinds(ns2, ys2),
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
    mut used @ _: aver_rt::AverList<AverStr>,
    mut k @ _: aver_rt::AverInt,
) -> AverStr {
    loop {
        crate::proof_std::cancel_checkpoint();
        let name @ _ = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(32usize);
                __b.push_display(&(base));
                __b
            };
            __b.push_display(&(k));
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
    let vars @ _ = {
        let __arg3 @ _ = aver_rt::AverList::concat(
            &aver_rt::AverList::from_vec(vec![ob.lhs, ob.rhs]),
            &ob.premise,
        );
        crate::proof_std::aver_generated::kernel::subst::freeVarsAll(&__arg3)
    };
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
    {
        let __list_subject = hs;
        match aver_rt::list_uncons(__list_subject) {
            Some((h, rest)) => aver_rt::AverList::concat(
                &aver_rt::AverList::prepend_owned(h.name.clone(), {
                    let __arg3 @ _ =
                        aver_rt::AverList::from_vec(vec![h.eqn.lhs.clone(), h.eqn.rhs.clone()]);
                    crate::proof_std::aver_generated::kernel::subst::freeVarsAll(&__arg3)
                }),
                &crate::proof_std::aver_generated::kernel::lib::hypNames(&rest),
            ),
            None => aver_rt::AverList::empty(),
        }
    }
}

/// The name of the `when` hypothesis, when the law has one.
#[inline(always)]
pub fn whenLabel(
    goal @ _: &crate::proof_std::aver_generated::kernel::proof::Goal,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::lib::whenOf(goal) {
        None => aver_rt::AverList::empty(),
        Some(_) => aver_rt::AverList::prepend_owned(
            ::aver_rt::str_lit!("when"),
            aver_rt::AverList::empty(),
        ),
    }
}

/// The names of module-level bindings.
#[inline(always)]
pub fn constNames(
    cs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Const>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = cs;
        match aver_rt::list_uncons(__list_subject) {
            Some((c, rest)) => aver_rt::AverList::prepend_owned(
                c.name.clone(),
                crate::proof_std::aver_generated::kernel::lib::constNames(&rest),
            ),
            None => aver_rt::AverList::empty(),
        }
    }
}

/// A goal for law with nothing else in it: no definitions, laws or hypotheses, its givens listed by what their types are. For a plan's examples, so they keep reading the same when Goal gains a field.
#[inline(always)]
pub fn goalFor(
    law @ _: &crate::proof_std::aver_generated::kernel::proof::Law,
) -> crate::proof_std::aver_generated::kernel::proof::Goal {
    crate::proof_std::cancel_checkpoint();
    let __arg1 @ _ = Box::new(
        crate::proof_std::aver_generated::kernel::proof::Goal__Fields {
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
    );
    crate::proof_std::aver_generated::kernel::wire::withGivens(__arg1, &law.givens)
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
