#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Call {
    pub args: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    pub inner: aver_rt::AverList<AverStr>,
}

impl PartialOrd for Call {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Call {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.args.cmp(&other.args))
            .then_with(|| self.inner.cmp(&other.inner))
    }
}

impl aver_rt::AverDisplay for Call {
    fn aver_display(&self) -> String {
        format!(
            "Call({})",
            vec![
                format!("args: {}", self.args.aver_display_inner()),
                format!("inner: {}", self.inner.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ArmCalls {
    pub arm: crate::proof_kernel::aver_generated::kernel::term::Arm,
    pub calls: aver_rt::AverList<Call>,
}

impl PartialOrd for ArmCalls {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ArmCalls {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.arm.cmp(&other.arm))
            .then_with(|| self.calls.cmp(&other.calls))
    }
}

impl aver_rt::AverDisplay for ArmCalls {
    fn aver_display(&self) -> String {
        format!(
            "ArmCalls({})",
            vec![
                format!("arm: {}", self.arm.aver_display_inner()),
                format!("calls: {}", self.calls.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Recursion {
    pub at: aver_rt::AverInt,
    pub guard: Option<crate::proof_kernel::aver_generated::kernel::term::Term>,
    pub arms: aver_rt::AverList<ArmCalls>,
}

impl PartialOrd for Recursion {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Recursion {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.arms.cmp(&other.arms))
            .then_with(|| self.at.cmp(&other.at))
            .then_with(|| self.guard.cmp(&other.guard))
    }
}

impl aver_rt::AverDisplay for Recursion {
    fn aver_display(&self) -> String {
        format!(
            "Recursion({})",
            vec![
                format!("at: {}", self.at.aver_display_inner()),
                format!("guard: {}", self.guard.aver_display_inner()),
                format!("arms: {}", self.arms.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Place {
    pub at: aver_rt::AverInt,
    pub name: AverStr,
}

impl PartialOrd for Place {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Place {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.at.cmp(&other.at))
            .then_with(|| self.name.cmp(&other.name))
    }
}

impl aver_rt::AverDisplay for Place {
    fn aver_display(&self) -> String {
        format!(
            "Place({})",
            vec![
                format!("at: {}", self.at.aver_display_inner()),
                format!("name: {}", self.name.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Varied {
    pub v: AverStr,
    pub general: aver_rt::AverList<Place>,
}

impl PartialOrd for Varied {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Varied {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.general.cmp(&other.general))
            .then_with(|| self.v.cmp(&other.v))
    }
}

impl aver_rt::AverDisplay for Varied {
    fn aver_display(&self) -> String {
        format!(
            "Varied({})",
            vec![
                format!("v: {}", self.v.aver_display_inner()),
                format!("general: {}", self.general.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Shrink {
    pub f: AverStr,
    pub p: AverStr,
    pub j: aver_rt::AverInt,
    pub n: aver_rt::AverInt,
}

impl PartialOrd for Shrink {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Shrink {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.f.cmp(&other.f))
            .then_with(|| self.j.cmp(&other.j))
            .then_with(|| self.n.cmp(&other.n))
            .then_with(|| self.p.cmp(&other.p))
    }
}

impl aver_rt::AverDisplay for Shrink {
    fn aver_display(&self) -> String {
        format!(
            "Shrink({})",
            vec![
                format!("f: {}", self.f.aver_display_inner()),
                format!("p: {}", self.p.aver_display_inner()),
                format!("j: {}", self.j.aver_display_inner()),
                format!("n: {}", self.n.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

/// The calls of f in t, a call before the calls in its arguments, a match's subject before its arms, each with the names inner arms bind around it. A call of another definition of ds that calls back into f stands for the calls of f its body makes, read at the call's arguments, before the calls in its arguments; fuel bounds how many such bodies one call goes through.
pub fn selfCalls(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    mut f @ _: AverStr,
    bound @ _: &aver_rt::AverList<AverStr>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    mut fuel @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverList<Call>, AverStr> {
    let mut __hold0: crate::proof_kernel::aver_generated::kernel::term::Term;
    let mut t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term = t;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __t = t;
            match __t.view() {
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TCall(
                    __g_fields,
                ) => {
                    let g = __g_fields.0.clone();
                    let xs = &__g_fields.1;
                    if (g == f) {
                        return Ok(aver_rt::AverList::prepend_owned(
                            crate::proof_kernel::aver_generated::kernel::induct::Call {
                                args: xs.clone(),
                                inner: bound.clone(),
                            },
                            crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                                xs, f, bound, ds, fuel,
                            )?,
                        ));
                    } else {
                        return Ok(aver_rt::AverList::concat(
                            &crate::proof_kernel::aver_generated::kernel::induct::helperCalls(
                                g,
                                xs,
                                f.clone(),
                                bound,
                                ds,
                                fuel.clone(),
                            )?,
                            &crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                                xs, f, bound, ds, fuel,
                            )?,
                        ));
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TMatch(
                    __s_fields,
                ) => {
                    let s = &__s_fields.0;
                    let arms = &__s_fields.1;
                    return Ok(aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                            s,
                            f.clone(),
                            bound,
                            ds,
                            fuel.clone(),
                        )?,
                        &crate::proof_kernel::aver_generated::kernel::induct::callsArms(
                            arms, f, bound, ds, fuel,
                        )?,
                    ));
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TGet(__o_fields) => {
                    let o = &__o_fields.0;
                    let n = __o_fields.1.clone();
                    {
                        let __tco0 = o.clone();
                        __hold0 = __tco0;
                        t = &__hold0;
                        continue;
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let xs = &__n_fields.1;
                    return crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                        xs, f, bound, ds, fuel,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                    let o = __o_fields.0.clone();
                    let a = &__o_fields.1;
                    let b = &__o_fields.2;
                    return Ok(aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                            a,
                            f.clone(),
                            bound,
                            ds,
                            fuel.clone(),
                        )?,
                        &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                            b, f, bound, ds, fuel,
                        )?,
                    ));
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                    let a = &__a_fields.0;
                    {
                        let __tco0 = a.clone();
                        __hold0 = __tco0;
                        t = &__hold0;
                        continue;
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TCtor(
                    __c_fields,
                ) => {
                    let c = __c_fields.0.clone();
                    let xs = &__c_fields.1;
                    return crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                        xs, f, bound, ds, fuel,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TParts(
                    __xs_fields,
                ) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                        xs, f, bound, ds, fuel,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TList(
                    __xs_fields,
                ) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                        xs, f, bound, ds, fuel,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TTuple(
                    __xs_fields,
                ) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                        xs, f, bound, ds, fuel,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TRec(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let fs = &__n_fields.1;
                    return crate::proof_kernel::aver_generated::kernel::induct::callsFields(
                        fs, f, bound, ds, fuel,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TUpd(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let b = &__n_fields.1;
                    let fs = &__n_fields.2;
                    return Ok(aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                            b,
                            f.clone(),
                            bound,
                            ds,
                            fuel.clone(),
                        )?,
                        &crate::proof_kernel::aver_generated::kernel::induct::callsFields(
                            fs, f, bound, ds, fuel,
                        )?,
                    ));
                }
                _ => {
                    return Ok(aver_rt::AverList::empty());
                }
            }
        }
    }
}

/// Calls of f in several terms, in order.
#[inline(always)]
pub fn callsAll(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    f @ _: AverStr,
    bound @ _: &aver_rt::AverList<AverStr>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    fuel @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverList<Call>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = xs;
        match aver_rt::list_uncons(__list_subject) {
            Some((x, rest)) => Ok(aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                    x,
                    f.clone(),
                    bound,
                    ds,
                    fuel.clone(),
                )?,
                &crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                    &rest, f, bound, ds, fuel,
                )?,
            )),
            None => Ok(aver_rt::AverList::empty()),
        }
    }
}

/// Calls of f in record fields, in order.
#[inline(always)]
pub fn callsFields(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
    f @ _: AverStr,
    bound @ _: &aver_rt::AverList<AverStr>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    fuel @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverList<Call>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = fs;
        match aver_rt::list_uncons(__list_subject) {
            Some((x, rest)) => Ok(aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                    &x.value,
                    f.clone(),
                    bound,
                    ds,
                    fuel.clone(),
                )?,
                &crate::proof_kernel::aver_generated::kernel::induct::callsFields(
                    &rest, f, bound, ds, fuel,
                )?,
            )),
            None => Ok(aver_rt::AverList::empty()),
        }
    }
}

/// Calls of f in match arms, each arm's pattern names added to what is bound.
#[inline(always)]
pub fn callsArms(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    f @ _: AverStr,
    bound @ _: &aver_rt::AverList<AverStr>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    fuel @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverList<Call>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = arms;
        match aver_rt::list_uncons(__list_subject) {
            Some((a, rest)) => Ok(aver_rt::AverList::concat(
                &{
                    let __arg7 @ _ = aver_rt::AverList::concat(
                        &bound.clone(),
                        &crate::proof_kernel::aver_generated::kernel::subst::patNames(&a.pattern),
                    );
                    crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                        &a.body,
                        f.clone(),
                        &__arg7,
                        ds,
                        fuel.clone(),
                    )
                }?,
                &crate::proof_kernel::aver_generated::kernel::induct::callsArms(
                    &rest, f, bound, ds, fuel,
                )?,
            )),
            None => Ok(aver_rt::AverList::empty()),
        }
    }
}

/// The calls of f a call of g at xs makes: none unless g is another definition of ds that calls back into f, and then those of g's body read at xs. A chain of such bodies longer than fuel goes through one of them twice without f: a recursion of its own, refused.
pub fn helperCalls(
    g @ _: AverStr,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    f @ _: AverStr,
    bound @ _: &aver_rt::AverList<AverStr>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    fuel @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverList<Call>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = (
            crate::proof_kernel::aver_generated::kernel::induct::reachesBack(
                g.clone(),
                f.clone(),
                ds,
            ),
            crate::proof_kernel::aver_generated::kernel::induct::defNamed(g.clone(), ds),
        );
        match __subject {
            (true, Some(h)) => {
                if (fuel > aver_rt::AverInt::from_i64(0)) {
                    crate::proof_kernel::aver_generated::kernel::induct::helperBody(
                        &h, xs, f, bound, ds, fuel,
                    )
                } else {
                    Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(97usize);
                                        __b.push_display(&(f));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" recurses through "));
                                    __b
                                };
                                __b.push_display(&(g));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(", which reaches itself without "));
                            __b
                        };
                        __b.push_display(&(f));
                        __b
                    }))
                }
            }
            _ => Ok(aver_rt::AverList::empty()),
        }
    }
}

/// A helper called with every argument: each call of f in its body, its local bindings substituted in order, read at xs. No local binding may call f: a call there would run whatever the body does with it.
pub fn helperBody(
    h @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    f @ _: AverStr,
    bound @ _: &aver_rt::AverList<AverStr>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    fuel @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverList<Call>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = (
            (aver_rt::AverInt::from_i64(
                crate::proof_kernel::aver_generated::kernel::proof::givenNames(&h.params).len()
                    as i64,
            ) == aver_rt::AverInt::from_i64(xs.len() as i64)),
            (crate::proof_kernel::aver_generated::kernel::induct::letCalls(
                &h.lets,
                f.clone(),
                ds,
                fuel.sub(&aver_rt::AverInt::from_i64(1)),
            )? == aver_rt::AverInt::from_i64(0)),
        );
        match __subject {
            (false, _) => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(49usize);
                            __b.push_display(&(h.name));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" takes "));
                        __b
                    };
                    __b.push_display(&(aver_rt::AverInt::from_i64(h.params.len() as i64)));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" arguments"));
                __b
            })),
            (_, false) => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(66usize);
                        __b.push_display(&(f));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" recurses from a local binding of "));
                    __b
                };
                __b.push_display(&(h.name));
                __b
            })),
            _ => {
                let __arg10 @ _ = {
                    let __arg8 @ _ = {
                        let __arg7 @ _ = {
                            let __arg6 @ _ = <aver_rt::AverList<
                                crate::proof_kernel::aver_generated::kernel::term::Binding,
                            >>::empty();
                            crate::proof_kernel::aver_generated::kernel::induct::letsInlined(
                                &h.lets, __arg6,
                            )
                        }?;
                        crate::proof_kernel::aver_generated::kernel::subst::subst(
                            h.body.clone(),
                            &__arg7,
                        )
                    }?;
                    {
                        let __arg9 @ _ = <aver_rt::AverList<AverStr>>::empty();
                        crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                            &__arg8,
                            f,
                            &__arg9,
                            ds,
                            fuel.sub(&aver_rt::AverInt::from_i64(1)),
                        )
                    }
                }?;
                crate::proof_kernel::aver_generated::kernel::induct::readAt(&__arg10, h, xs, bound)
            }
        }
    }
}

/// Each local binding, in order, bound to its value with the bindings before it substituted; the latest name comes first.
#[inline(always)]
pub fn letsInlined(
    lets @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    mut done @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
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
            match aver_rt::list_uncons(__list_subject) {
                Some((b, rest)) => {
                    let __tco0 = rest;
                    let __tco1 = aver_rt::AverList::prepend_owned(
                        {
                            let __arg4 @ _ =
                                crate::proof_kernel::aver_generated::kernel::subst::subst(
                                    b.value.clone(),
                                    &done,
                                )?;
                            crate::proof_kernel::aver_generated::kernel::term::bind(
                                b.name.clone(),
                                __arg4,
                            )
                        },
                        done,
                    );
                    __hold0 = __tco0;
                    lets = &__hold0;
                    done = __tco1;
                    continue;
                }
                None => {
                    return Ok(done);
                }
            }
        }
    }
}

/// Each call of f in h's body at the arguments h is called with.
#[inline(always)]
pub fn readAt(
    cs @ _: &aver_rt::AverList<Call>,
    h @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    bound @ _: &aver_rt::AverList<AverStr>,
) -> Result<aver_rt::AverList<Call>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = cs;
        match aver_rt::list_uncons(__list_subject) {
            Some((c, rest)) => Ok(aver_rt::AverList::prepend_owned(
                crate::proof_kernel::aver_generated::kernel::induct::readOne(c, h, xs, bound)?,
                crate::proof_kernel::aver_generated::kernel::induct::readAt(&rest, h, xs, bound)?,
            )),
            None => Ok(aver_rt::AverList::empty()),
        }
    }
}

/// One call of f in h's body at the arguments h is called with: h's parameters that no arm of h rebinds around it replaced, refusing a capture. It may read nothing else of h's scope. The names bound around the call of h come before those h binds around it.
pub fn readOne(
    c @ _: &Call,
    h @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    bound @ _: &aver_rt::AverList<AverStr>,
) -> Result<Call, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let used @ _ = crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(&c.args);
    let bs @ _ = {
        let __arg9 @ _ = {
            let __arg8 @ _ =
                crate::proof_kernel::aver_generated::kernel::proof::givenNames(&h.params);
            crate::proof_kernel::aver_generated::kernel::induct::paramsAt(&__arg8, xs)
        };
        crate::proof_kernel::aver_generated::kernel::subst::unbind(&__arg9, c.inner.clone())
    };
    {
        let __arg11 @ _ = {
            let __arg10 @ _ = aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::proof::givenNames(&h.params),
                &c.inner.clone(),
            );
            crate::proof_kernel::aver_generated::kernel::subst::without(&used, __arg10)
        };
        crate::proof_kernel::aver_generated::kernel::induct::unread(__arg11, h.name.clone())
    }?;
    crate::proof_kernel::aver_generated::kernel::subst::capture(bs.clone(), &used, &c.inner)?;
    Ok(crate::proof_kernel::aver_generated::kernel::induct::Call {
        args: crate::proof_kernel::aver_generated::kernel::subst::substAll(&c.args, &bs)?,
        inner: aver_rt::AverList::concat(&bound.clone(), &c.inner.clone()),
    })
}

/// No name is left that the helper's call does not give.
#[inline(always)]
pub fn unread(mut ns @ _: aver_rt::AverList<AverStr>, h @ _: AverStr) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ns, [] => Ok(()), [n, rest] => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity(85usize); __b.push_display(&(h)); __b }; __b.push_str(&::aver_rt::str_lit!(" passes ")); __b }; __b.push_display(&(n)); __b }; __b.push_str(&::aver_rt::str_lit!(", which it does not bind, to a recursive call")); __b })) })
}

/// Each parameter bound to its argument.
pub fn paramsAt(
    ps @ _: &aver_rt::AverList<AverStr>,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (ps.clone(), xs.clone());
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((p, morePs)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((x, moreXs)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            aver_rt::AverList::prepend_owned(
                crate::proof_kernel::aver_generated::kernel::term::bind(p, x),
                crate::proof_kernel::aver_generated::kernel::induct::paramsAt(&morePs, &moreXs),
            )
        } else {
            aver_rt::AverList::empty()
        }
    }
}

/// Whether g is another definition of ds from which f is reachable.
#[inline(always)]
pub fn reachesBack(
    g @ _: AverStr,
    f @ _: AverStr,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    ((g != f) && {
        let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::induct::calleesOf(g, ds, ds);
        {
            let __arg4 @ _ = <aver_rt::AverList<AverStr>>::empty();
            {
                let __arg5 @ _ = aver_rt::AverInt::from_i64(ds.len() as i64);
                crate::proof_kernel::aver_generated::kernel::induct::reaches(
                    __arg3, f, ds, __arg4, __arg5,
                )
            }
        }
    })
}

/// The definition of ds named n.
#[inline(always)]
pub fn defNamed(
    mut n @ _: AverStr,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Option<crate::proof_kernel::aver_generated::kernel::proof::Def> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>;
    let mut ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def> =
        ds;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = ds;
            match aver_rt::list_uncons(__list_subject) {
                Some((d, rest)) => {
                    if (d.name == n) {
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

/// How many calls of f the local bindings make; fuel bounds the helper bodies a call goes through, as in selfCalls.
#[inline(always)]
pub fn letCalls(
    ls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    f @ _: AverStr,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    fuel @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverInt, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ls;
        match aver_rt::list_uncons(__list_subject) {
            Some((b, rest)) => Ok(aver_rt::AverInt::from_i64(
                {
                    let __arg6 @ _ = <aver_rt::AverList<AverStr>>::empty();
                    crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                        &b.value,
                        f.clone(),
                        &__arg6,
                        ds,
                        fuel.clone(),
                    )
                }?
                .len() as i64,
            )
            .add(
                &crate::proof_kernel::aver_generated::kernel::induct::letCalls(&rest, f, ds, fuel)?,
            )),
            None => Ok(aver_rt::AverInt::from_i64(0)),
        }
    }
}

/// The recursive calls of d in t, read through the definitions of ds that call back into d.
#[inline(always)]
pub fn calls(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<aver_rt::AverList<Call>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let __arg3 @ _ = <aver_rt::AverList<AverStr>>::empty();
    let __arg4 @ _ = aver_rt::AverInt::from_i64(ds.len() as i64);
    crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
        t,
        d.name.clone(),
        &__arg3,
        ds,
        __arg4,
    )
}

/// None when d does not call itself, directly or through other definitions of ds that call back into it. Otherwise what the gate checked, which opening d and an induction along it both read: the place of the parameter d recurses on, the comparison its match splits on when it counts that Int toward zero (none when it matches the value itself), and each arm with its recursive calls, every one of them checked to descend. A refusal when one does not.
pub fn recursion(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<Option<Recursion>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = (
            ({
                let __arg2 @ _ = aver_rt::AverInt::from_i64(ds.len() as i64);
                crate::proof_kernel::aver_generated::kernel::induct::letCalls(
                    &d.lets,
                    d.name.clone(),
                    ds,
                    __arg2,
                )
            }? == aver_rt::AverInt::from_i64(0)),
            crate::proof_kernel::aver_generated::kernel::induct::calls(&d.body, d, ds)?,
        );
        {
            let __int_match_subject = __subject;
            let (__lit0, __lit1) = &__int_match_subject;
            if (*__lit0) == true && (*__lit1).is_empty() {
                Ok(None)
            } else {
                Ok(Some(
                    crate::proof_kernel::aver_generated::kernel::induct::recursive(d, ds)?,
                ))
            }
        }
    }
}

/// The gate for a definition that does call itself: no local bindings, and a body that matches a parameter or compares one with zero.
pub fn recursive(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<Recursion, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (d.lets.clone(), d.body.clone());
        {
            let __list_subject = __pat0;
            if __list_subject.is_empty() {
                {
                    let __t = __pat1;
                    match __t.view() {
                        crate::proof_kernel::aver_generated::kernel::term::Term__view::TMatch(
                            ____pat2_fields,
                        ) => {
                            let __pat2 = &____pat2_fields.0;
                            let arms = &____pat2_fields.1;
                            {
                                let __t = __pat2;
                                match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TVar(__p_fields) => {
            let p = __p_fields.0.clone();
            crate::proof_kernel::aver_generated::kernel::induct::onParts(d, ds, { let __arg12 @ _ = crate::proof_kernel::aver_generated::kernel::proof::givenNames(&d.params); crate::proof_kernel::aver_generated::kernel::induct::indexOf(__arg12, p, aver_rt::AverInt::from_i64(0)) }, arms)
        },
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(__op_fields) => {
            let op = __op_fields.0.clone();
            let __pat3 = &__op_fields.1;
            let __pat4 = &__op_fields.2;
            { let __t = __pat3; match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TVar(__p_fields) => {
            let p = __p_fields.0.clone();
            { let __t = __pat4; match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TInt(____pat5_fields) => {
            let __pat5 = ____pat5_fields.0.clone();
            { let __int_match_subject = __pat5; if __int_match_subject == aver_rt::AverInt::from_i64(0) { { let __arg14 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(op, crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(p.clone())), crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))))); crate::proof_kernel::aver_generated::kernel::induct::towardZero(d, ds, __arg14, p.clone(), { let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::proof::givenNames(&d.params); crate::proof_kernel::aver_generated::kernel::induct::indexOf(__arg13, p, aver_rt::AverInt::from_i64(0)) }, arms) } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity(56usize); __b.push_display(&(d.name)); __b }; __b.push_str(&::aver_rt::str_lit!(" recurses outside a match on a parameter")); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity(56usize); __b.push_display(&(d.name)); __b }; __b.push_str(&::aver_rt::str_lit!(" recurses outside a match on a parameter")); __b }))
        }
    } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity(56usize); __b.push_display(&(d.name)); __b }; __b.push_str(&::aver_rt::str_lit!(" recurses outside a match on a parameter")); __b }))
        }
    } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity(56usize); __b.push_display(&(d.name)); __b }; __b.push_str(&::aver_rt::str_lit!(" recurses outside a match on a parameter")); __b }))
        }
    }
                            }
                        }
                        _ => Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(56usize);
                                __b.push_display(&(d.name));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(
                                " recurses outside a match on a parameter"
                            ));
                            __b
                        })),
                    }
                }
            } else {
                Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(46usize);
                        __b.push_display(&(d.name));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" recurses from a local binding"));
                    __b
                }))
            }
        }
    }
}

/// Every recursive call passes, at place j, a name its arm's pattern binds and no inner arm rebinds, or List.drop of one: a value smaller than the one matched.
#[inline(always)]
pub fn onParts(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    j @ _: aver_rt::AverInt,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> Result<Recursion, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (j < aver_rt::AverInt::from_i64(0)) {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(69usize);
                __b.push_display(&(d.name));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(
                " recurses on a match whose subject is not a parameter"
            ));
            __b
        }))
    } else {
        if crate::proof_kernel::aver_generated::kernel::induct::armsShrink(arms, d, ds, j.clone())?
        {
            Ok(
                crate::proof_kernel::aver_generated::kernel::induct::Recursion {
                    at: j,
                    guard: None,
                    arms: crate::proof_kernel::aver_generated::kernel::induct::armCalls(
                        arms, d, ds,
                    )?,
                },
            )
        } else {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(81usize);
                    __b.push_display(&(d.name));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(
                    " recurses on something that is not a part of the value it matches"
                ));
                __b
            }))
        }
    }
}

/// Each arm with the recursive calls in it, as selfCalls lists them.
#[inline(always)]
pub fn armCalls(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<aver_rt::AverList<ArmCalls>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = arms;
        match aver_rt::list_uncons(__list_subject) {
            Some((a, rest)) => Ok(aver_rt::AverList::prepend_owned(
                crate::proof_kernel::aver_generated::kernel::induct::ArmCalls {
                    arm: a.clone(),
                    calls: crate::proof_kernel::aver_generated::kernel::induct::calls(
                        &a.body, d, ds,
                    )?,
                },
                crate::proof_kernel::aver_generated::kernel::induct::armCalls(&rest, d, ds)?,
            )),
            None => Ok(aver_rt::AverList::empty()),
        }
    }
}

/// match p <= 0 or match p > 0, one arm per truth value: the arm where the comparison says p is at most 0 makes no recursive call, and every call in the other passes p - 1 or p / k at p's place, for a literal k of at least 2, p not rebound around it. Where p > 0 both are at least 0 and below p, so the recursion stops for every Int.
pub fn towardZero(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    mut guard @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    p @ _: AverStr,
    j @ _: aver_rt::AverInt,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> Result<Recursion, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1, __pat2) = (
            (j < aver_rt::AverInt::from_i64(0)),
            crate::proof_kernel::aver_generated::kernel::induct::stopsAt(&guard),
            arms.clone(),
        );
        if __pat0 {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(69usize);
                    __b.push_display(&(d.name));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(
                    " recurses on a match whose subject is not a parameter"
                ));
                __b
            }))
        } else {
            match __pat1 {
                Some(stop) => {
                    let __list_subject = __pat2;
                    match aver_rt::list_uncons(&__list_subject) {
                        Some((a, __pat3)) => {
                            let __list_subject = __pat3;
                            match aver_rt::list_uncons(&__list_subject) {
                                Some((b, __pat4)) => {
                                    let __list_subject = __pat4;
                                    if __list_subject.is_empty() {
                                        if (crate::proof_kernel::aver_generated::kernel::induct::oneEach(a.pattern.clone(), b.pattern.clone()) && (crate::proof_kernel::aver_generated::kernel::induct::armDescends(a, stop.clone(), d, ds, p.clone(), j.clone())? && crate::proof_kernel::aver_generated::kernel::induct::armDescends(b, stop, d, ds, p.clone(), j.clone())?)) { Ok(crate::proof_kernel::aver_generated::kernel::induct::Recursion { at: j, guard: Some(guard), arms: crate::proof_kernel::aver_generated::kernel::induct::armCalls(arms, d, ds)? }) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity(61usize); __b.push_display(&(d.name)); __b }; __b.push_str(&::aver_rt::str_lit!(" does not count ")); __b }; __b.push_display(&(p)); __b }; __b.push_str(&::aver_rt::str_lit!(" down to zero")); __b })) }
                                    } else {
                                        Err(aver_rt::AverStr::from({
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(61usize);
                                                        __b.push_display(&(d.name));
                                                        __b
                                                    };
                                                    __b.push_str(&::aver_rt::str_lit!(
                                                        " does not count "
                                                    ));
                                                    __b
                                                };
                                                __b.push_display(&(p));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" down to zero"));
                                            __b
                                        }))
                                    }
                                }
                                None => Err(aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b =
                                                    aver_rt::Buffer::with_capacity(61usize);
                                                __b.push_display(&(d.name));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" does not count "));
                                            __b
                                        };
                                        __b.push_display(&(p));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" down to zero"));
                                    __b
                                })),
                            }
                        }
                        None => Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(61usize);
                                        __b.push_display(&(d.name));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" does not count "));
                                    __b
                                };
                                __b.push_display(&(p));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" down to zero"));
                            __b
                        })),
                    }
                }
                _ => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(61usize);
                                __b.push_display(&(d.name));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" does not count "));
                            __b
                        };
                        __b.push_display(&(p));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" down to zero"));
                    __b
                })),
            }
        }
    }
}

/// The value of the comparison where p is at most 0: true for p <= 0, false for p > 0; none for any other comparison.
pub fn stopsAt(
    guard @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> Option<bool> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __t = guard;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(____pat0_fields) => {
                let __pat0 = ____pat0_fields.0.clone();
                {
                    let __dispatch_subject = __pat0;
                    if &*__dispatch_subject == "<=" {
                        Some(true)
                    } else {
                        if &*__dispatch_subject == ">" {
                            Some(false)
                        } else {
                            None
                        }
                    }
                }
            }
            _ => None,
        }
    }
}

/// One arm true, the other false.
pub fn oneEach(
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
    mut b @ _: crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (a, b);
        match __pat0 {
            crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(__pat2) => {
                let __t = __pat2;
                match __t.view() {
                    crate::proof_kernel::aver_generated::kernel::term::Term__view::TBool(
                        __x_fields,
                    ) => {
                        let x = __x_fields.0.clone();
                        match __pat1 {
                            crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(
                                __pat3,
                            ) => {
                                let __t = __pat3;
                                match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TBool(__y_fields) => {
            let y = __y_fields.0.clone();
            (x != y)
        },
        _ => {
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
            _ => false,
        }
    }
}

/// The arm where p is at most 0 makes no recursive call; each call in the other descends at place j.
#[inline(always)]
pub fn armDescends(
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Arm,
    stop @ _: bool,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    p @ _: AverStr,
    j @ _: aver_rt::AverInt,
) -> Result<bool, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (a.pattern
        == crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(
            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(stop),
            ),
        ))
    {
        Ok((aver_rt::AverInt::from_i64(
            crate::proof_kernel::aver_generated::kernel::induct::calls(&a.body, d, ds)?.len()
                as i64,
        ) == aver_rt::AverInt::from_i64(0)))
    } else {
        Ok({
            let __arg6 @ _ =
                crate::proof_kernel::aver_generated::kernel::induct::calls(&a.body, d, ds)?;
            {
                let __arg7 @ _ = aver_rt::AverInt::from_i64(d.params.len() as i64);
                crate::proof_kernel::aver_generated::kernel::induct::callsDescend(
                    &__arg6, p, j, __arg7,
                )
            }
        })
    }
}

/// Each call has every argument and passes p - 1 or p / k at place j, p not rebound around it.
#[inline(always)]
pub fn callsDescend(
    cs @ _: &aver_rt::AverList<Call>,
    p @ _: AverStr,
    j @ _: aver_rt::AverInt,
    n @ _: aver_rt::AverInt,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = cs;
        match aver_rt::list_uncons(__list_subject) {
            Some((c, rest)) => {
                (((aver_rt::AverInt::from_i64(c.args.len() as i64) == n)
                    && ({
                        let __arg6 @ _ =
                            crate::proof_kernel::aver_generated::kernel::induct::nthTerm(
                                &c.args,
                                j.clone(),
                            );
                        crate::proof_kernel::aver_generated::kernel::induct::descends(
                            (&__arg6).as_ref(),
                            &p,
                        )
                    } && (!c.inner.contains(&p))))
                    && crate::proof_kernel::aver_generated::kernel::induct::callsDescend(
                        &rest, p, j, n,
                    ))
            }
            None => true,
        }
    }
}

/// Whether t is p - 1, or p / k for a literal k of at least 2: for p > 0 each is at least 0 and below p.
pub fn descends(
    t @ _: Option<&crate::proof_kernel::aver_generated::kernel::term::Term>,
    p @ _: &AverStr,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match t {
        Some(__pat0) => {
            let __t = __pat0;
            match __t.view() {
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(
                    ____pat1_fields,
                ) => {
                    let __pat1 = ____pat1_fields.0.clone();
                    let __pat2 = &____pat1_fields.1;
                    let __pat3 = &____pat1_fields.2;
                    {
                        let __int_match_subject =
                            aver_rt::AverInt::from_i64(aver_rt::str_code1(&__pat1));
                        if __int_match_subject == aver_rt::AverInt::from_i64(45) {
                            {
                                let __t = __pat2;
                                match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TVar(__q_fields) => {
            let q = __q_fields.0.clone();
            { let __t = __pat3; match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TInt(____pat4_fields) => {
            let __pat4 = ____pat4_fields.0.clone();
            { let __int_match_subject = __pat4; if __int_match_subject == aver_rt::AverInt::from_i64(1) { (&(q) == p) } else { false } }
        },
        _ => {
            false
        }
    } }
        },
        _ => {
            false
        }
    }
                            }
                        } else {
                            false
                        }
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(
                    ____pat5_fields,
                ) => {
                    let __pat5 = ____pat5_fields.0.clone();
                    let __pat6 = &____pat5_fields.1;
                    match &*__pat5 {
                        "__int_div_euclid" => {
                            let __list_subject = __pat6;
                            match aver_rt::list_uncons(__list_subject) {
                                Some((__pat7, __pat8)) => {
                                    let __t = __pat7;
                                    match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TVar(__q_fields) => {
            let q = __q_fields.0.clone();
            { let __list_subject = __pat8; match aver_rt::list_uncons(&__list_subject) { Some((__pat9, __pat10)) => { { let __t = __pat9; match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TInt(__k_fields) => {
            let k = __k_fields.0.clone();
            { let __list_subject = __pat10; if __list_subject.is_empty() { ((&(q) == p) && (k >= aver_rt::AverInt::from_i64(2))) } else { false } }
        },
        _ => {
            false
        }
    } } }, None => { false } } }
        },
        _ => {
            false
        }
    }
                                }
                                None => false,
                            }
                        }
                        _ => false,
                    }
                }
                _ => false,
            }
        }
        _ => false,
    }
}

/// Whether steps may open d: its recursion, read through the definitions of ds it calls back through, passes the gate; or every cycle through d passes one whose recursion does (rooted); or every call shortens a list (shortens).
#[inline(always)]
pub fn openGate(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::induct::recursion(d, ds) {
        Ok(_) => Ok(()),
        Err(why @ _) => {
            if (crate::proof_kernel::aver_generated::kernel::induct::rooted(d, ds, ds)
                || crate::proof_kernel::aver_generated::kernel::induct::shortens(d, ds))
            {
                Ok(())
            } else {
                Err(why)
            }
        }
    }
}

/// Whether d calls itself only directly, never back through another definition of ds, and at one place every eager local binding and the body, with local bindings substituted, pass a shorter list at every call (shorter). That length falls at every call, so the recursion stops.
#[inline(always)]
pub fn shortens(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = {
            let __arg4 @ _ = <aver_rt::AverList<
                crate::proof_kernel::aver_generated::kernel::term::Binding,
            >>::empty();
            crate::proof_kernel::aver_generated::kernel::induct::letsInlined(&d.lets, __arg4)
        };
        match __subject {
            Err(_) => false,
            Ok(ls @ _) => {
                match crate::proof_kernel::aver_generated::kernel::subst::subst(d.body.clone(), &ls)
                {
                    Err(_) => false,
                    Ok(body @ _) => {
                        ((!{
                            let __arg5 @ _ =
                                crate::proof_kernel::aver_generated::kernel::induct::callees(d, ds);
                            crate::proof_kernel::aver_generated::kernel::induct::anyBack(
                                __arg5,
                                d.name.clone(),
                                ds,
                            )
                        }) && {
                            let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::term::Term::TTuple(crate::proof_kernel::aver_generated::kernel::term::Term__TTuple(aver_rt::AverList::prepend_owned(body, crate::proof_kernel::aver_generated::kernel::induct::bindingValues(&ls))));
                            {
                                let __arg7 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::proof::givenNames(
                                        &d.params,
                                    );
                                crate::proof_kernel::aver_generated::kernel::induct::shorterSomewhere(&__arg6, d.name.clone(), &__arg7, aver_rt::AverInt::from_i64(0))
                            }
                        })
                    }
                }
            }
        }
    }
}

/// Every eager local binding value after earlier bindings have been substituted into it.
#[inline(always)]
pub fn bindingValues(
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = bs;
        match aver_rt::list_uncons(__list_subject) {
            Some((b, rest)) => aver_rt::AverList::prepend_owned(
                b.value.clone(),
                crate::proof_kernel::aver_generated::kernel::induct::bindingValues(&rest),
            ),
            None => aver_rt::AverList::empty(),
        }
    }
}

/// Whether one of gs reaches back to f.
#[inline(always)]
pub fn anyBack(
    mut gs @ _: aver_rt::AverList<AverStr>,
    f @ _: AverStr,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(gs, [] => false, [g, rest] => (crate::proof_kernel::aver_generated::kernel::induct::reachesBack(g, f.clone(), ds) || crate::proof_kernel::aver_generated::kernel::induct::anyBack(rest, f, ds)))
}

/// Whether some place j of the parameters ps is one where every call of f in body is shorter.
#[inline(always)]
pub fn shorterSomewhere(
    body @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    f @ _: AverStr,
    ps @ _: &aver_rt::AverList<AverStr>,
    j @ _: aver_rt::AverInt,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ps;
        match aver_rt::list_uncons_cloned(__list_subject) {
            Some((p, rest)) => {
                ({
                    let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::induct::Shrink {
                        f: f.clone(),
                        p: p,
                        j: j.clone(),
                        n: j.add(&aver_rt::AverInt::from_i64(ps.len() as i64)),
                    };
                    {
                        let __arg7 @ _ = <aver_rt::AverList<AverStr>>::empty();
                        crate::proof_kernel::aver_generated::kernel::induct::shorter(
                            body, &__arg6, true, &__arg7,
                        )
                    }
                } || crate::proof_kernel::aver_generated::kernel::induct::shorterSomewhere(
                    body,
                    f,
                    &rest,
                    j.add(&aver_rt::AverInt::from_i64(1)),
                ))
            }
            None => false,
        }
    }
}

/// Every call of m.f in t has m.n arguments and passes at place m.j one of parts or List.drop of something shorter (smaller). A part is the tail of a cell of the list m.p names while live, or of a list shorter than it, or a name a catch-all arm binds to a shorter list: each strictly shorter than the list at m.j.
pub fn shorter(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    m @ _: &Shrink,
    mut live @ _: bool,
    parts @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    let mut __hold0: crate::proof_kernel::aver_generated::kernel::term::Term;
    let mut t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term = t;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __t = t;
            match __t.view() {
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TCall(
                    __g_fields,
                ) => {
                    let g = __g_fields.0.clone();
                    let xs = &__g_fields.1;
                    return (((g != m.f)
                        || ((aver_rt::AverInt::from_i64(xs.len() as i64) == m.n) && {
                            let __arg26 @ _ =
                                crate::proof_kernel::aver_generated::kernel::induct::nthTerm(
                                    xs,
                                    m.j.clone(),
                                );
                            {
                                let __arg27 @ _ = <aver_rt::AverList<AverStr>>::empty();
                                crate::proof_kernel::aver_generated::kernel::induct::smaller(
                                    __arg26,
                                    parts.clone(),
                                    __arg27,
                                )
                            }
                        }))
                        && crate::proof_kernel::aver_generated::kernel::induct::shorterAll(
                            xs, m, live, parts,
                        ));
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TMatch(
                    __s_fields,
                ) => {
                    let s = &__s_fields.0;
                    let arms = &__s_fields.1;
                    return (crate::proof_kernel::aver_generated::kernel::induct::shorter(
                        s, m, live, parts,
                    ) && crate::proof_kernel::aver_generated::kernel::induct::shorterArms(
                        arms, s, m, live, parts,
                    ));
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TGet(__o_fields) => {
                    let o = &__o_fields.0;
                    let k = __o_fields.1.clone();
                    {
                        let __tco0 = o.clone();
                        __hold0 = __tco0;
                        t = &__hold0;
                        continue;
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(__k_fields) => {
                    let k = __k_fields.0.clone();
                    let xs = &__k_fields.1;
                    return crate::proof_kernel::aver_generated::kernel::induct::shorterAll(
                        xs, m, live, parts,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                    let o = __o_fields.0.clone();
                    let a = &__o_fields.1;
                    let b = &__o_fields.2;
                    return (crate::proof_kernel::aver_generated::kernel::induct::shorter(
                        a, m, live, parts,
                    ) && crate::proof_kernel::aver_generated::kernel::induct::shorter(
                        b, m, live, parts,
                    ));
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                    let a = &__a_fields.0;
                    {
                        let __tco0 = a.clone();
                        __hold0 = __tco0;
                        t = &__hold0;
                        continue;
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TCtor(
                    __c_fields,
                ) => {
                    let c = __c_fields.0.clone();
                    let xs = &__c_fields.1;
                    return crate::proof_kernel::aver_generated::kernel::induct::shorterAll(
                        xs, m, live, parts,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TParts(
                    __xs_fields,
                ) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_kernel::aver_generated::kernel::induct::shorterAll(
                        xs, m, live, parts,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TList(
                    __xs_fields,
                ) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_kernel::aver_generated::kernel::induct::shorterAll(
                        xs, m, live, parts,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TTuple(
                    __xs_fields,
                ) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_kernel::aver_generated::kernel::induct::shorterAll(
                        xs, m, live, parts,
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TRec(__k_fields) => {
                    let k = __k_fields.0.clone();
                    let fs = &__k_fields.1;
                    return {
                        let __arg28 @ _ =
                            crate::proof_kernel::aver_generated::kernel::induct::fieldValues(fs);
                        crate::proof_kernel::aver_generated::kernel::induct::shorterAll(
                            &__arg28, m, live, parts,
                        )
                    };
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TUpd(__k_fields) => {
                    let k = __k_fields.0.clone();
                    let b = &__k_fields.1;
                    let fs = &__k_fields.2;
                    return {
                        let __arg29 @ _ = aver_rt::AverList::prepend_owned(
                            b.clone(),
                            crate::proof_kernel::aver_generated::kernel::induct::fieldValues(fs),
                        );
                        crate::proof_kernel::aver_generated::kernel::induct::shorterAll(
                            &__arg29, m, live, parts,
                        )
                    };
                }
                _ => {
                    return true;
                }
            }
        }
    }
}

/// shorter for each term.
#[inline(always)]
pub fn shorterAll(
    ts @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    m @ _: &Shrink,
    live @ _: bool,
    parts @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ts;
        match aver_rt::list_uncons(__list_subject) {
            Some((t, rest)) => {
                (crate::proof_kernel::aver_generated::kernel::induct::shorter(t, m, live, parts)
                    && crate::proof_kernel::aver_generated::kernel::induct::shorterAll(
                        &rest, m, live, parts,
                    ))
            }
            None => true,
        }
    }
}

/// shorter in each arm of a match on s: the names the arm binds are no longer parts, nor is m.p the parameter once rebound; the tail of a cell of a list at most as long as the one at m.j, or a name bound to a shorter list, is a part.
#[inline(always)]
pub fn shorterArms(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    s @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    m @ _: &Shrink,
    live @ _: bool,
    parts @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = arms;
        match aver_rt::list_uncons(__list_subject) {
            Some((a, rest)) => {
                ({
                    let __arg8 @ _ = (live
                        && (!crate::proof_kernel::aver_generated::kernel::subst::patNames(
                            &a.pattern,
                        )
                        .contains(&m.p)));
                    {
                        let __arg9 @ _ = aver_rt::AverList::concat(
                            &{
                                let __arg7 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::subst::patNames(
                                        &a.pattern,
                                    );
                                crate::proof_kernel::aver_generated::kernel::subst::without(
                                    parts, __arg7,
                                )
                            },
                            &crate::proof_kernel::aver_generated::kernel::induct::armParts(
                                &a.pattern,
                                s.clone(),
                                m,
                                live,
                                parts,
                            ),
                        );
                        crate::proof_kernel::aver_generated::kernel::induct::shorter(
                            &a.body, m, __arg8, &__arg9,
                        )
                    }
                } && crate::proof_kernel::aver_generated::kernel::induct::shorterArms(
                    &rest, s, m, live, parts,
                ))
            }
            None => true,
        }
    }
}

/// What an arm adds to the parts: the tail of [h, ..t] over m.p while live or over something shorter, or the name of a catch-all over something shorter.
pub fn armParts(
    pat @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
    mut s @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    m @ _: &Shrink,
    live @ _: bool,
    parts @ _: &aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match pat {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, tl) => {
            let h = h.clone();
            let tl = tl.clone();
            if ((live
                && (s
                    == crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TVar(m.p.clone()),
                    )))
                || {
                    let __arg8 @ _ = Some(s);
                    {
                        let __arg9 @ _ = <aver_rt::AverList<AverStr>>::empty();
                        crate::proof_kernel::aver_generated::kernel::induct::smaller(
                            __arg8,
                            parts.clone(),
                            __arg9,
                        )
                    }
                })
            {
                aver_rt::AverList::prepend_owned(tl, aver_rt::AverList::empty())
            } else {
                aver_rt::AverList::empty()
            }
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PVar(x) => {
            let x = x.clone();
            if {
                let __arg10 @ _ = Some(s);
                {
                    let __arg11 @ _ = <aver_rt::AverList<AverStr>>::empty();
                    crate::proof_kernel::aver_generated::kernel::induct::smaller(
                        __arg10,
                        parts.clone(),
                        __arg11,
                    )
                }
            } {
                aver_rt::AverList::prepend_owned(x, aver_rt::AverList::empty())
            } else {
                aver_rt::AverList::empty()
            }
        }
        _ => aver_rt::AverList::empty(),
    }
}

/// The values of record fields.
#[inline(always)]
pub fn fieldValues(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = fs;
        match aver_rt::list_uncons(__list_subject) {
            Some((x, rest)) => aver_rt::AverList::prepend_owned(
                x.value.clone(),
                crate::proof_kernel::aver_generated::kernel::induct::fieldValues(&rest),
            ),
            None => aver_rt::AverList::empty(),
        }
    }
}

/// Whether some definition r of ds reaches d and is reached from it, and r's recursion, read through the others, passes the gate. Every cycle through d then passes r, each time on a smaller value, so d stops.
#[inline(always)]
pub fn rooted(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    todo @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> bool {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>;
    let mut todo @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def> =
        todo;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = todo;
            match aver_rt::list_uncons(__list_subject) {
                Some((r, rest)) => {
                    if ((crate::proof_kernel::aver_generated::kernel::induct::reachesFrom(
                        d.name.clone(),
                        r.name.clone(),
                        ds,
                    ) && crate::proof_kernel::aver_generated::kernel::induct::reachesFrom(
                        r.name.clone(),
                        d.name.clone(),
                        ds,
                    )) && crate::proof_kernel::aver_generated::kernel::induct::descendsThrough(
                        r, ds,
                    )) {
                        return true;
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            todo = &__hold1;
                            continue;
                        }
                    }
                }
                None => {
                    return false;
                }
            }
        }
    }
}

/// Whether b is a or reachable from a through the definitions of ds.
#[inline(always)]
pub fn reachesFrom(
    a @ _: AverStr,
    b @ _: AverStr,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    ((a == b) || {
        let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::induct::calleesOf(a, ds, ds);
        {
            let __arg4 @ _ = <aver_rt::AverList<AverStr>>::empty();
            {
                let __arg5 @ _ = aver_rt::AverInt::from_i64(ds.len() as i64);
                crate::proof_kernel::aver_generated::kernel::induct::reaches(
                    __arg3, b, ds, __arg4, __arg5,
                )
            }
        }
    })
}

/// Whether r calls itself and its recursion passes the gate.
pub fn descendsThrough(
    r @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::induct::recursion(r, ds) {
        Ok(__pat0) => match __pat0 {
            Some(_) => true,
            _ => false,
        },
        _ => false,
    }
}

/// A fixture for the tests: any(xs) matches a list and calls orOn on its head and tail.
pub fn listAny() -> crate::proof_kernel::aver_generated::kernel::proof::Def {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::proof::Def { name: ::aver_rt::str_lit!("any"), params: aver_rt::AverList::prepend_owned({ let __arg0 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny; crate::proof_kernel::aver_generated::kernel::term::typed(::aver_rt::str_lit!("xs"), __arg0) }, aver_rt::AverList::empty()), ret: crate::proof_kernel::aver_generated::kernel::term::Ty::TyBool, lets: aver_rt::AverList::empty(), body: crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(crate::proof_kernel::aver_generated::kernel::term::Term__TMatch(crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(::aver_rt::str_lit!("xs"))), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Arm { pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PNil, body: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false)) }, crate::proof_kernel::aver_generated::kernel::term::Arm { pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(::aver_rt::str_lit!("h"), ::aver_rt::str_lit!("t")), body: crate::proof_kernel::aver_generated::kernel::term::Term::TCall(crate::proof_kernel::aver_generated::kernel::term::Term__TCall(::aver_rt::str_lit!("orOn"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(::aver_rt::str_lit!("h"))), crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(::aver_rt::str_lit!("t")))]))) }]))) }
}

/// A fixture for the tests: walk(xs) matches a list and hands its head and tail to skip.
pub fn walks() -> crate::proof_kernel::aver_generated::kernel::proof::Def {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::proof::Def { name: ::aver_rt::str_lit!("walk"), params: aver_rt::AverList::prepend_owned({ let __arg0 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny; crate::proof_kernel::aver_generated::kernel::term::typed(::aver_rt::str_lit!("xs"), __arg0) }, aver_rt::AverList::empty()), ret: crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny, lets: aver_rt::AverList::empty(), body: crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(crate::proof_kernel::aver_generated::kernel::term::Term__TMatch(crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(::aver_rt::str_lit!("xs"))), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Arm { pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PNil, body: crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))) }, crate::proof_kernel::aver_generated::kernel::term::Arm { pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(::aver_rt::str_lit!("h"), ::aver_rt::str_lit!("t")), body: crate::proof_kernel::aver_generated::kernel::term::Term::TCall(crate::proof_kernel::aver_generated::kernel::term::Term__TCall(::aver_rt::str_lit!("skip"), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(::aver_rt::str_lit!("h"))), crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(::aver_rt::str_lit!("t")))]))) }]))) }
}

/// A fixture for the tests: skip(n, rest) with these local bindings and this body.
pub fn skipping(
    lets @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    mut body @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::proof::Def {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::proof::Def {
        name: ::aver_rt::str_lit!("skip"),
        params: aver_rt::AverList::from_vec(vec![
            {
                let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny;
                crate::proof_kernel::aver_generated::kernel::term::typed(
                    ::aver_rt::str_lit!("n"),
                    __arg2,
                )
            },
            {
                let __arg3 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny;
                crate::proof_kernel::aver_generated::kernel::term::typed(
                    ::aver_rt::str_lit!("rest"),
                    __arg3,
                )
            },
        ]),
        ret: crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny,
        lets: lets.clone(),
        body: body,
    }
}

/// A fixture for the tests: f(xs, k) matches a list and calls itself on next in its cell arm.
pub fn dropsBy(
    mut next @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::proof::Def {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::proof::Def { name: ::aver_rt::str_lit!("f"), params: aver_rt::AverList::from_vec(vec![{ let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny; crate::proof_kernel::aver_generated::kernel::term::typed(::aver_rt::str_lit!("xs"), __arg1) }, { let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny; crate::proof_kernel::aver_generated::kernel::term::typed(::aver_rt::str_lit!("k"), __arg2) }]), ret: crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny, lets: aver_rt::AverList::empty(), body: crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(crate::proof_kernel::aver_generated::kernel::term::Term__TMatch(crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(::aver_rt::str_lit!("xs"))), aver_rt::AverList::from_vec(vec![crate::proof_kernel::aver_generated::kernel::term::Arm { pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PNil, body: crate::proof_kernel::aver_generated::kernel::term::Term::TInt(crate::proof_kernel::aver_generated::kernel::term::Term__TInt(aver_rt::AverInt::from_i64(0))) }, crate::proof_kernel::aver_generated::kernel::term::Arm { pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(::aver_rt::str_lit!("h"), ::aver_rt::str_lit!("t")), body: crate::proof_kernel::aver_generated::kernel::term::Term::TCall(crate::proof_kernel::aver_generated::kernel::term::Term__TCall(::aver_rt::str_lit!("f"), aver_rt::AverList::from_vec(vec![next, crate::proof_kernel::aver_generated::kernel::term::Term::TVar(crate::proof_kernel::aver_generated::kernel::term::Term__TVar(::aver_rt::str_lit!("k")))]))) }]))) }
}

/// A fixture for the tests: orOn(first, rest) is true when first is not 0, and back otherwise.
pub fn orOn(
    mut back @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::proof::Def {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::proof::Def {
        name: ::aver_rt::str_lit!("orOn"),
        params: aver_rt::AverList::from_vec(vec![
            {
                let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny;
                crate::proof_kernel::aver_generated::kernel::term::typed(
                    ::aver_rt::str_lit!("first"),
                    __arg1,
                )
            },
            {
                let __arg2 @ _ = crate::proof_kernel::aver_generated::kernel::term::Ty::TyAny;
                crate::proof_kernel::aver_generated::kernel::term::typed(
                    ::aver_rt::str_lit!("rest"),
                    __arg2,
                )
            },
        ]),
        ret: crate::proof_kernel::aver_generated::kernel::term::Ty::TyBool,
        lets: aver_rt::AverList::empty(),
        body: crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(
            crate::proof_kernel::aver_generated::kernel::term::Term__TMatch(
                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TOp(
                        ::aver_rt::str_lit!("!="),
                        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TVar(
                                ::aver_rt::str_lit!("first"),
                            ),
                        ),
                        crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                                aver_rt::AverInt::from_i64(0),
                            ),
                        ),
                    ),
                ),
                aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::term::Arm {
                        pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(
                            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                    true,
                                ),
                            ),
                        ),
                        body: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true),
                        ),
                    },
                    crate::proof_kernel::aver_generated::kernel::term::Arm {
                        pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(
                            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                crate::proof_kernel::aver_generated::kernel::term::Term__TBool(
                                    false,
                                ),
                            ),
                        ),
                        body: back,
                    },
                ]),
            ),
        ),
    }
}

/// Every arm passes, at place j of each recursive call, a proper part bound below a constructor and no inner arm rebinds.
#[inline(always)]
pub fn armsShrink(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    j @ _: aver_rt::AverInt,
) -> Result<bool, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = arms;
        match aver_rt::list_uncons(__list_subject) {
            Some((a, rest)) => Ok(({
                let __arg6 @ _ =
                    crate::proof_kernel::aver_generated::kernel::induct::calls(&a.body, d, ds)?;
                {
                    let __arg7 @ _ =
                        crate::proof_kernel::aver_generated::kernel::induct::structuralParts(
                            &a.pattern,
                        );
                    {
                        let __arg8 @ _ = aver_rt::AverInt::from_i64(d.params.len() as i64);
                        crate::proof_kernel::aver_generated::kernel::induct::callsShrink(
                            &__arg6,
                            &__arg7,
                            j.clone(),
                            __arg8,
                        )
                    }
                }
            }
                && crate::proof_kernel::aver_generated::kernel::induct::armsShrink(
                    &rest, d, ds, j,
                )?)),
            None => Ok(true),
        }
    }
}

/// Names bound below a constructor, list cell or tuple; a catch-all binds the whole value, not a smaller part.
pub fn structuralParts(
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match p {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            crate::proof_kernel::aver_generated::kernel::subst::patNames(p)
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
            let c = c.clone();
            let ns = ns.clone();
            crate::proof_kernel::aver_generated::kernel::subst::patNames(p)
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PTuple(ps) => {
            let ps = ps.clone();
            crate::proof_kernel::aver_generated::kernel::subst::patNames(p)
        }
        _ => aver_rt::AverList::empty(),
    }
}

/// Each call passes one of the parts at place j.
#[inline(always)]
pub fn callsShrink(
    cs @ _: &aver_rt::AverList<Call>,
    parts @ _: &aver_rt::AverList<AverStr>,
    j @ _: aver_rt::AverInt,
    n @ _: aver_rt::AverInt,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = cs;
        match aver_rt::list_uncons(__list_subject) {
            Some((c, rest)) => {
                (((aver_rt::AverInt::from_i64(c.args.len() as i64) == n) && {
                    let __arg6 @ _ = crate::proof_kernel::aver_generated::kernel::induct::nthTerm(
                        &c.args,
                        j.clone(),
                    );
                    crate::proof_kernel::aver_generated::kernel::induct::smaller(
                        __arg6,
                        parts.clone(),
                        c.inner.clone(),
                    )
                }) && crate::proof_kernel::aver_generated::kernel::induct::callsShrink(
                    &rest, parts, j, n,
                ))
            }
            None => true,
        }
    }
}

/// A name the arm's pattern binds, not rebound inside; or List.drop of something smaller, which is never longer than the list it drops from, whatever the count.
pub fn smaller(
    mut t @ _: Option<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut parts @ _: aver_rt::AverList<AverStr>,
    mut inner @ _: aver_rt::AverList<AverStr>,
) -> bool {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        match t {
            Some(__pat0) => {
                let __t = __pat0;
                match __t.view() {
                    crate::proof_kernel::aver_generated::kernel::term::Term__view::TVar(
                        __b_fields,
                    ) => {
                        let b = __b_fields.0.clone();
                        return (parts.contains(&b) && (!inner.contains(&b)));
                    }
                    crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(
                        ____pat1_fields,
                    ) => {
                        let __pat1 = ____pat1_fields.0.clone();
                        let __pat2 = &____pat1_fields.1;
                        match &*__pat1 {
                            "List.drop" => {
                                let __list_subject = __pat2;
                                match aver_rt::list_uncons(__list_subject) {
                                    Some((l, __pat3)) => {
                                        let __list_subject = __pat3;
                                        match aver_rt::list_uncons_cloned(&__list_subject) {
                                            Some((n, __pat4)) => {
                                                let __list_subject = __pat4;
                                                if __list_subject.is_empty() {
                                                    {
                                                        let __tco0 = Some(l.clone());
                                                        t = __tco0;
                                                        continue;
                                                    }
                                                } else {
                                                    return false;
                                                }
                                            }
                                            None => {
                                                return false;
                                            }
                                        }
                                    }
                                    None => {
                                        return false;
                                    }
                                }
                            }
                            _ => {
                                return false;
                            }
                        }
                    }
                    _ => {
                        return false;
                    }
                }
            }
            _ => {
                return false;
            }
        }
    }
}

/// Where x is in xs, from i; -1 when it is not.
#[inline(always)]
pub fn indexOf(
    mut xs @ _: aver_rt::AverList<AverStr>,
    mut x @ _: AverStr,
    mut i @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(xs, [] => { return aver_rt::AverInt::from_i64(-1); }, [y, rest] => { if (y == x) { return i; } else { {
            let __tco0 = rest;
            let __tco2 = i.add(&aver_rt::AverInt::from_i64(1));
            xs = __tco0;
            i = __tco2;
            continue;
        } } })
    }
}

/// The term at place i.
#[inline(always)]
pub fn nthTerm(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut i @ _: aver_rt::AverInt,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Term> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>;
    let mut xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> =
        xs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = xs;
            match aver_rt::list_uncons(__list_subject) {
                Some((x, rest)) => {
                    if (i == aver_rt::AverInt::from_i64(0)) {
                        return Some(x.clone());
                    } else {
                        {
                            let __tco0 = rest;
                            let __tco1 = i.sub(&aver_rt::AverInt::from_i64(1));
                            __hold0 = __tco0;
                            xs = &__hold0;
                            i = __tco1;
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

/// No definition reaches itself through another definition of the script, unless every such cycle passes one whose recursion, read through the others, passes the gate (rooted).
pub fn refuseMutualRecursion(
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ds;
        if __list_subject.is_empty() {
            Ok(())
        } else {
            crate::proof_kernel::aver_generated::kernel::induct::eachNotMutual(ds, ds)
        }
    }
}

/// Check each definition in turn.
#[inline(always)]
pub fn eachNotMutual(
    todo @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<(), AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>;
    let mut todo @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def> =
        todo;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = todo;
            match aver_rt::list_uncons(__list_subject) {
                Some((d, rest)) => {
                    if ({
                        let __arg4 @ _ =
                            crate::proof_kernel::aver_generated::kernel::induct::callees(d, ds);
                        {
                            let __arg5 @ _ = <aver_rt::AverList<AverStr>>::empty();
                            {
                                let __arg6 @ _ = aver_rt::AverInt::from_i64(ds.len() as i64);
                                crate::proof_kernel::aver_generated::kernel::induct::reaches(
                                    __arg4,
                                    d.name.clone(),
                                    ds,
                                    __arg5,
                                    __arg6,
                                )
                            }
                        }
                    } && (!crate::proof_kernel::aver_generated::kernel::induct::rooted(
                        d, ds, ds,
                    ))) {
                        return Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(46usize);
                                __b.push_display(&(d.name));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" is part of a mutual recursion"));
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
                }
                None => {
                    return Ok(());
                }
            }
        }
    }
}

/// Whether target is among the definitions reachable from the frontier; fuel is the number of definitions, enough for any path that does not repeat.
pub fn reaches(
    mut frontier @ _: aver_rt::AverList<AverStr>,
    mut target @ _: AverStr,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    mut seen @ _: aver_rt::AverList<AverStr>,
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
                    return false;
                } else if !(*__lit0).is_empty() && (*__lit1) == true {
                    let Some((g, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                        unreachable!("Aver Rust codegen: tuple element list mismatch")
                    };
                    if (g == target) {
                        return true;
                    } else {
                        if seen.contains(&g) {
                            {
                                let __tco0 = rest;
                                frontier = __tco0;
                                continue;
                            }
                        } else {
                            {
                                let __tco0 = aver_rt::AverList::concat(
                                    &rest,
                                    &crate::proof_kernel::aver_generated::kernel::induct::calleesOf(
                                        g.clone(),
                                        ds,
                                        ds,
                                    ),
                                );
                                let __tco3 = aver_rt::AverList::prepend_owned(g, seen);
                                let __tco4 = fuel.sub(&aver_rt::AverInt::from_i64(1));
                                frontier = __tco0;
                                seen = __tco3;
                                fuel = __tco4;
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

/// The definitions of ds that the one named name calls.
#[inline(always)]
pub fn calleesOf(
    mut name @ _: AverStr,
    todo @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> aver_rt::AverList<AverStr> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>;
    let mut todo @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def> =
        todo;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = todo;
            match aver_rt::list_uncons(__list_subject) {
                Some((d, rest)) => {
                    if (d.name == name) {
                        return crate::proof_kernel::aver_generated::kernel::induct::callees(d, ds);
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            todo = &__hold1;
                            continue;
                        }
                    }
                }
                None => {
                    return aver_rt::AverList::empty();
                }
            }
        }
    }
}

/// The other definitions of the script d calls.
#[inline(always)]
pub fn callees(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::AverList::concat(
        &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(&d.body),
        &crate::proof_kernel::aver_generated::kernel::induct::letNames(&d.lets),
    );
    crate::proof_kernel::aver_generated::kernel::induct::onlyDefs(&__arg2, ds, d.name.clone())
}

/// Functions the local bindings call.
#[inline(always)]
pub fn letNames(
    ls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ls;
        match aver_rt::list_uncons(__list_subject) {
            Some((b, rest)) => aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(&b.value),
                &crate::proof_kernel::aver_generated::kernel::induct::letNames(&rest),
            ),
            None => aver_rt::AverList::empty(),
        }
    }
}

/// The names that are other definitions of the script.
#[inline(always)]
pub fn onlyDefs(
    ns @ _: &aver_rt::AverList<AverStr>,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    mut own @ _: AverStr,
) -> aver_rt::AverList<AverStr> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut ns @ _: &aver_rt::AverList<AverStr> = ns;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = ns;
            match aver_rt::list_uncons_cloned(__list_subject) {
                Some((n, rest)) => {
                    if ((n != own)
                        && crate::proof_kernel::aver_generated::kernel::induct::isDef(
                            n.clone(),
                            ds,
                        ))
                    {
                        return aver_rt::AverList::prepend_owned(
                            n,
                            crate::proof_kernel::aver_generated::kernel::induct::onlyDefs(
                                &rest, ds, own,
                            ),
                        );
                    } else {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            ns = &__hold0;
                            continue;
                        }
                    }
                }
                None => {
                    return aver_rt::AverList::empty();
                }
            }
        }
    }
}

/// Whether the script defines n.
#[inline(always)]
pub fn isDef(
    n @ _: AverStr,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ds;
        match aver_rt::list_uncons(__list_subject) {
            Some((d, rest)) => {
                ((d.name == n)
                    || crate::proof_kernel::aver_generated::kernel::induct::isDef(n, &rest))
            }
            None => false,
        }
    }
}

/// Every function a term calls.
pub fn namesCalled(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> aver_rt::AverList<AverStr> {
    let mut __hold0: crate::proof_kernel::aver_generated::kernel::term::Term;
    let mut t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term = t;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __t = t;
            match __t.view() {
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TCall(
                    __g_fields,
                ) => {
                    let g = __g_fields.0.clone();
                    let xs = &__g_fields.1;
                    return aver_rt::AverList::prepend_owned(
                        g,
                        crate::proof_kernel::aver_generated::kernel::induct::namesAll(xs),
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TMatch(
                    __s_fields,
                ) => {
                    let s = &__s_fields.0;
                    let arms = &__s_fields.1;
                    return aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(s),
                        &crate::proof_kernel::aver_generated::kernel::induct::namesArms(arms),
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TGet(__o_fields) => {
                    let o = &__o_fields.0;
                    let n = __o_fields.1.clone();
                    {
                        let __tco0 = o.clone();
                        __hold0 = __tco0;
                        t = &__hold0;
                        continue;
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let xs = &__n_fields.1;
                    return crate::proof_kernel::aver_generated::kernel::induct::namesAll(xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                    let o = __o_fields.0.clone();
                    let a = &__o_fields.1;
                    let b = &__o_fields.2;
                    return aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(a),
                        &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(b),
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                    let a = &__a_fields.0;
                    {
                        let __tco0 = a.clone();
                        __hold0 = __tco0;
                        t = &__hold0;
                        continue;
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TCtor(
                    __c_fields,
                ) => {
                    let c = __c_fields.0.clone();
                    let xs = &__c_fields.1;
                    return crate::proof_kernel::aver_generated::kernel::induct::namesAll(xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TParts(
                    __xs_fields,
                ) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_kernel::aver_generated::kernel::induct::namesAll(xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TList(
                    __xs_fields,
                ) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_kernel::aver_generated::kernel::induct::namesAll(xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TTuple(
                    __xs_fields,
                ) => {
                    let xs = &__xs_fields.0;
                    return crate::proof_kernel::aver_generated::kernel::induct::namesAll(xs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TRec(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let fs = &__n_fields.1;
                    return crate::proof_kernel::aver_generated::kernel::induct::namesFields(fs);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TUpd(__n_fields) => {
                    let n = __n_fields.0.clone();
                    let b = &__n_fields.1;
                    let fs = &__n_fields.2;
                    return aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(b),
                        &crate::proof_kernel::aver_generated::kernel::induct::namesFields(fs),
                    );
                }
                _ => {
                    return aver_rt::AverList::empty();
                }
            }
        }
    }
}

/// Functions several terms call.
#[inline(always)]
pub fn namesAll(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = xs;
        match aver_rt::list_uncons(__list_subject) {
            Some((x, rest)) => aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(x),
                &crate::proof_kernel::aver_generated::kernel::induct::namesAll(&rest),
            ),
            None => aver_rt::AverList::empty(),
        }
    }
}

/// Functions record fields call.
#[inline(always)]
pub fn namesFields(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = fs;
        match aver_rt::list_uncons(__list_subject) {
            Some((x, rest)) => aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(&x.value),
                &crate::proof_kernel::aver_generated::kernel::induct::namesFields(&rest),
            ),
            None => aver_rt::AverList::empty(),
        }
    }
}

/// Functions match arms call.
#[inline(always)]
pub fn namesArms(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = arms;
        match aver_rt::list_uncons(__list_subject) {
            Some((a, rest)) => aver_rt::AverList::concat(
                &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(&a.body),
                &crate::proof_kernel::aver_generated::kernel::induct::namesArms(&rest),
            ),
            None => aver_rt::AverList::empty(),
        }
    }
}

/// The given at the matched place, and the other givens among the arguments that vary with it, each at the first place it appears.
pub fn varied(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    j @ _: aver_rt::AverInt,
    mut givens @ _: aver_rt::AverList<AverStr>,
) -> Result<Varied, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::induct::nthTerm(xs, j.clone()) {
        Some(__pat0) => {
            let __t = __pat0;
            match __t.view() {
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TVar(__v_fields) => {
                    let v = __v_fields.0.clone();
                    if givens.contains(&v) {
                        Ok(
                            crate::proof_kernel::aver_generated::kernel::induct::Varied {
                                v: v.clone(),
                                general: {
                                    let __arg5 @ _ = <aver_rt::AverList<AverStr>>::empty();
                                    crate::proof_kernel::aver_generated::kernel::induct::generalised(
                                        xs,
                                        aver_rt::AverInt::from_i64(0),
                                        j,
                                        v,
                                        givens,
                                        __arg5,
                                    )
                                },
                            },
                        )
                    } else {
                        Err(::aver_rt::str_lit!(
                            "the argument at the matched place must be a given"
                        ))
                    }
                }
                _ => Err(::aver_rt::str_lit!(
                    "the argument at the matched place must be a given"
                )),
            }
        }
        _ => Err(::aver_rt::str_lit!(
            "the argument at the matched place must be a given"
        )),
    }
}

/// Places other than j holding a given other than v not seen before.
#[inline(always)]
pub fn generalised(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut k @ _: aver_rt::AverInt,
    mut j @ _: aver_rt::AverInt,
    mut v @ _: AverStr,
    mut givens @ _: aver_rt::AverList<AverStr>,
    mut seen @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<Place> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>;
    let mut xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> =
        xs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = xs;
            match aver_rt::list_uncons(__list_subject) {
                Some((x, rest)) => {
                    let __t = x;
                    match __t.view() {
                        crate::proof_kernel::aver_generated::kernel::term::Term__view::TVar(
                            __n_fields,
                        ) => {
                            let n = __n_fields.0.clone();
                            if (((k != j) && (n != v))
                                && (givens.contains(&n) && (!seen.contains(&n))))
                            {
                                return aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::induct::Place { at: k.clone(), name: n.clone() }, crate::proof_kernel::aver_generated::kernel::induct::generalised(&rest, k.add(&aver_rt::AverInt::from_i64(1)), j, v, givens, aver_rt::AverList::prepend_owned(n, seen)));
                            } else {
                                {
                                    let __tco0 = rest;
                                    let __tco1 = k.add(&aver_rt::AverInt::from_i64(1));
                                    __hold0 = __tco0;
                                    xs = &__hold0;
                                    k = __tco1;
                                    continue;
                                }
                            }
                        }
                        _ => {
                            let __tco0 = rest;
                            let __tco1 = k.add(&aver_rt::AverInt::from_i64(1));
                            __hold0 = __tco0;
                            xs = &__hold0;
                            k = __tco1;
                            continue;
                        }
                    }
                }
                None => {
                    return aver_rt::AverList::empty();
                }
            }
        }
    }
}
