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

/// The calls of f in t, a call before the calls in its arguments, a match's subject before its arms, each with the names inner arms bind around it. A call of another definition of ds that calls back into f stands for the calls of f its body makes, read at the call's arguments, before the calls in its arguments; fuel bounds how many such bodies one call goes through.
pub fn selfCalls(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut f @ _: AverStr,
    bound @ _: aver_rt::AverList<AverStr>,
    ds @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    mut fuel @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverList<Call>, AverStr> {
    let bound @ _ = std::sync::Arc::new(bound);
    let ds @ _ = std::sync::Arc::new(ds);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        match t {
            crate::proof_kernel::aver_generated::kernel::term::Term::TCall(g, xs) => {
                if (g == f) {
                    return Ok(aver_rt::AverList::prepend(
                        crate::proof_kernel::aver_generated::kernel::induct::Call {
                            args: xs.clone(),
                            inner: (*bound).clone(),
                        },
                        &crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                            &xs, f, &*bound, &*ds, fuel,
                        )?,
                    ));
                } else {
                    return Ok(aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::induct::helperCalls(
                            g,
                            &xs,
                            f.clone(),
                            &*bound,
                            &*ds,
                            fuel.clone(),
                        )?,
                        &crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                            &xs, f, &*bound, &*ds, fuel,
                        )?,
                    ));
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(s, arms) => {
                let s = (*s).clone();
                return Ok(aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                        s,
                        f.clone(),
                        (*bound).clone(),
                        (*ds).clone(),
                        fuel.clone(),
                    )?,
                    &crate::proof_kernel::aver_generated::kernel::induct::callsArms(
                        &arms, f, &*bound, &*ds, fuel,
                    )?,
                ));
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TGet(o, n) => {
                let o = (*o).clone();
                {
                    let __tco0 = o;
                    t = __tco0;
                    continue;
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TBi(n, xs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                    &xs, f, &*bound, &*ds, fuel,
                );
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(o, a, b) => {
                let a = (*a).clone();
                let b = (*b).clone();
                return Ok(aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                        a,
                        f.clone(),
                        (*bound).clone(),
                        (*ds).clone(),
                        fuel.clone(),
                    )?,
                    &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                        b,
                        f,
                        (*bound).clone(),
                        (*ds).clone(),
                        fuel,
                    )?,
                ));
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TNeg(a) => {
                let a = (*a).clone();
                {
                    let __tco0 = a;
                    t = __tco0;
                    continue;
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(c, xs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                    &xs, f, &*bound, &*ds, fuel,
                );
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TParts(xs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                    &xs, f, &*bound, &*ds, fuel,
                );
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TList(xs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                    &xs, f, &*bound, &*ds, fuel,
                );
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TTuple(xs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::callsAll(
                    &xs, f, &*bound, &*ds, fuel,
                );
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TRec(n, fs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::callsFields(
                    &fs, f, &*bound, &*ds, fuel,
                );
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TUpd(n, b, fs) => {
                let b = (*b).clone();
                return Ok(aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                        b,
                        f.clone(),
                        (*bound).clone(),
                        (*ds).clone(),
                        fuel.clone(),
                    )?,
                    &crate::proof_kernel::aver_generated::kernel::induct::callsFields(
                        &fs, f, &*bound, &*ds, fuel,
                    )?,
                ));
            }
            _ => {
                return Ok(aver_rt::AverList::empty());
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
    aver_list_match!(xs.clone(), [] => Ok(aver_rt::AverList::empty()), [x, rest] => Ok(aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::induct::selfCalls(x, f.clone(), bound.clone(), ds.clone(), fuel.clone())?, &crate::proof_kernel::aver_generated::kernel::induct::callsAll(&rest, f, bound, ds, fuel)?)))
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
    aver_list_match!(fs.clone(), [] => Ok(aver_rt::AverList::empty()), [x, rest] => Ok(aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::induct::selfCalls(x.value, f.clone(), bound.clone(), ds.clone(), fuel.clone())?, &crate::proof_kernel::aver_generated::kernel::induct::callsFields(&rest, f, bound, ds, fuel)?)))
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
    aver_list_match!(arms.clone(), [] => Ok(aver_rt::AverList::empty()), [a, rest] => Ok(aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::induct::selfCalls(a.body, f.clone(), aver_rt::AverList::concat(&bound.clone(), &crate::proof_kernel::aver_generated::kernel::subst::patNames(&a.pattern)), ds.clone(), fuel.clone())?, &crate::proof_kernel::aver_generated::kernel::induct::callsArms(&rest, f, bound, ds, fuel)?)))
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
    match (
        crate::proof_kernel::aver_generated::kernel::induct::reachesBack(g.clone(), f.clone(), ds),
        crate::proof_kernel::aver_generated::kernel::induct::defNamed(g.clone(), ds.clone()),
    ) {
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
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(97)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(f),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(" recurses through "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(g))));
                            __b
                        };
                        __b.push_str(&AverStr::from(", which reaches itself without "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                    __b
                }))
            }
        }
        _ => Ok(aver_rt::AverList::empty()),
    }
}

/// A helper without local bindings, called with every argument: each call of f in its body, read at xs.
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
        let __int_match_subject = (
            h.lets.clone(),
            (aver_rt::AverInt::from_i64(h.params.len() as i64)
                == aver_rt::AverInt::from_i64(xs.len() as i64)),
        );
        let (__lit0, __lit1) = &__int_match_subject;
        if (*__lit0).is_empty() && (*__lit1) == true {
            crate::proof_kernel::aver_generated::kernel::induct::readAt(
                &crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
                    h.body.clone(),
                    f,
                    aver_rt::AverList::empty(),
                    ds.clone(),
                    fuel.sub(&aver_rt::AverInt::from_i64(1)),
                )?,
                h,
                xs,
                bound,
            )
        } else if (*__lit0).is_empty() && (*__lit1) == false {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(49)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h.name))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" takes "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(aver_rt::AverInt::from_i64(h.params.len() as i64)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(" arguments"));
                __b
            }))
        } else {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(76)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" recurses through "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h.name))));
                    __b
                };
                __b.push_str(&AverStr::from(", which has local bindings"));
                __b
            }))
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
    aver_list_match!(cs.clone(), [] => Ok(aver_rt::AverList::empty()), [c, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::induct::readOne(&c, h, xs, bound)?, &crate::proof_kernel::aver_generated::kernel::induct::readAt(&rest, h, xs, bound)?)))
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
    let bs @ _ = crate::proof_kernel::aver_generated::kernel::subst::unbind(
        crate::proof_kernel::aver_generated::kernel::induct::paramsAt(&h.params, xs),
        c.inner.clone(),
    );
    crate::proof_kernel::aver_generated::kernel::induct::unread(
        &crate::proof_kernel::aver_generated::kernel::subst::without(
            used.clone(),
            aver_rt::AverList::concat(&h.params.clone(), &c.inner.clone()),
        ),
        h.name.clone(),
    )?;
    crate::proof_kernel::aver_generated::kernel::subst::capture(bs.clone(), &used, &c.inner)?;
    Ok(crate::proof_kernel::aver_generated::kernel::induct::Call {
        args: crate::proof_kernel::aver_generated::kernel::subst::substAll(&c.args, &bs)?,
        inner: aver_rt::AverList::concat(&bound.clone(), &c.inner.clone()),
    })
}

/// No name is left that the helper's call does not give.
#[inline(always)]
pub fn unread(ns @ _: &aver_rt::AverList<AverStr>, h @ _: AverStr) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ns.clone(), [] => Ok(()), [n, rest] => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(85)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(h)))); __b }; __b.push_str(&AverStr::from(" passes ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n)))); __b }; __b.push_str(&AverStr::from(", which it does not bind, to a recursive call")); __b })) })
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
            aver_rt::AverList::prepend(
                crate::proof_kernel::aver_generated::kernel::term::bind(p, &x),
                &crate::proof_kernel::aver_generated::kernel::induct::paramsAt(&morePs, &moreXs),
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
    ((g != f)
        && crate::proof_kernel::aver_generated::kernel::induct::reaches(
            crate::proof_kernel::aver_generated::kernel::induct::calleesOf(
                g,
                ds.clone(),
                ds.clone(),
            ),
            f,
            ds.clone(),
            aver_rt::AverList::empty(),
            aver_rt::AverInt::from_i64(ds.len() as i64),
        ))
}

/// The definition of ds named n.
#[inline(always)]
pub fn defNamed(
    mut n @ _: AverStr,
    mut ds @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Option<crate::proof_kernel::aver_generated::kernel::proof::Def> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(ds, [] => { return None; }, [d, rest] => { if (d.name == n) { return Some(d); } else { {
            let __tco1 = rest;
            ds = __tco1;
            continue;
        } } })
    }
}

/// How many calls of f the local bindings make.
#[inline(always)]
pub fn letCalls(
    ls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    f @ _: AverStr,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<aver_rt::AverInt, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ls.clone(), [] => Ok(aver_rt::AverInt::from_i64(0)), [b, rest] => Ok(aver_rt::AverInt::from_i64(crate::proof_kernel::aver_generated::kernel::induct::selfCalls(b.value, f.clone(), aver_rt::AverList::empty(), ds.clone(), aver_rt::AverInt::from_i64(ds.len() as i64))?.len() as i64).add(&crate::proof_kernel::aver_generated::kernel::induct::letCalls(&rest, f, ds)?)))
}

/// The recursive calls of d in t, read through the definitions of ds that call back into d.
#[inline(always)]
pub fn calls(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<aver_rt::AverList<Call>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::induct::selfCalls(
        t,
        d.name.clone(),
        aver_rt::AverList::empty(),
        ds.clone(),
        aver_rt::AverInt::from_i64(ds.len() as i64),
    )
}

/// None when d does not call itself, directly or through other definitions of ds that call back into it. Otherwise what the gate checked, which opening d and an induction along it both read: the place of the parameter d recurses on, the comparison its match splits on when it counts that Int toward zero (none when it matches the value itself), and each arm with its recursive calls, every one of them checked to descend. A refusal when one does not.
pub fn recursion(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<Option<Recursion>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __int_match_subject = (
            (crate::proof_kernel::aver_generated::kernel::induct::letCalls(
                &d.lets,
                d.name.clone(),
                ds,
            )? == aver_rt::AverInt::from_i64(0)),
            crate::proof_kernel::aver_generated::kernel::induct::calls(d.body.clone(), d, ds)?,
        );
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
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(
                        __pat2,
                        arms,
                    ) => {
                        let __pat2 = (*__pat2).clone();
                        match __pat2 {
                            crate::proof_kernel::aver_generated::kernel::term::Term::TVar(p) => {
                                crate::proof_kernel::aver_generated::kernel::induct::onParts(
                                    d,
                                    ds,
                                    crate::proof_kernel::aver_generated::kernel::induct::indexOf(
                                        d.params.clone(),
                                        p,
                                        aver_rt::AverInt::from_i64(0),
                                    ),
                                    &arms,
                                )
                            }
                            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                                op,
                                __pat3,
                                __pat4,
                            ) => {
                                let __pat3 = (*__pat3).clone();
                                let __pat4 = (*__pat4).clone();
                                match __pat3 {
        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(p) => {
            match __pat4 {
        crate::proof_kernel::aver_generated::kernel::term::Term::TInt(__pat5) => {
            { let __int_match_subject = __pat5; if __int_match_subject == aver_rt::AverInt::from_i64(0) { crate::proof_kernel::aver_generated::kernel::induct::towardZero(d, ds, &crate::proof_kernel::aver_generated::kernel::term::Term::TOp(op, std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TVar(p.clone())), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::term::Term::TInt(aver_rt::AverInt::from_i64(0)))), p.clone(), crate::proof_kernel::aver_generated::kernel::induct::indexOf(d.params.clone(), p, aver_rt::AverInt::from_i64(0)), &arms) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(56)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name)))); __b }; __b.push_str(&AverStr::from(" recurses outside a match on a parameter")); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(56)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name)))); __b }; __b.push_str(&AverStr::from(" recurses outside a match on a parameter")); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(56)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name)))); __b }; __b.push_str(&AverStr::from(" recurses outside a match on a parameter")); __b }))
        }
    }
                            }
                            _ => Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(56)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(d.name),
                                    )));
                                    __b
                                };
                                __b.push_str(&AverStr::from(
                                    " recurses outside a match on a parameter",
                                ));
                                __b
                            })),
                        }
                    }
                    _ => Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(56)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" recurses outside a match on a parameter"));
                        __b
                    })),
                }
            } else {
                Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(46)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" recurses from a local binding"));
                    __b
                }))
            }
        }
    }
}

/// Every recursive call passes, at place j, a name its arm's pattern binds and no inner arm rebinds: a strict part of the value matched.
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
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(69)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name))));
                __b
            };
            __b.push_str(&AverStr::from(
                " recurses on a match whose subject is not a parameter",
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
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(81)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name))));
                    __b
                };
                __b.push_str(&AverStr::from(
                    " recurses on something that is not a part of the value it matches",
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
    aver_list_match!(arms.clone(), [] => Ok(aver_rt::AverList::empty()), [a, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::induct::ArmCalls { arm: a.clone(), calls: crate::proof_kernel::aver_generated::kernel::induct::calls(a.body.clone(), d, ds)? }, &crate::proof_kernel::aver_generated::kernel::induct::armCalls(&rest, d, ds)?)))
}

/// match p <= 0 or match p > 0, one arm per truth value: the arm where the comparison says p is at most 0 makes no recursive call, and every call in the other passes p - 1 or p / k at p's place, for a literal k of at least 2, p not rebound around it. Where p > 0 both are at least 0 and below p, so the recursion stops for every Int.
pub fn towardZero(
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    guard @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    p @ _: AverStr,
    j @ _: aver_rt::AverInt,
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> Result<Recursion, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1, __pat2) = (
            (j < aver_rt::AverInt::from_i64(0)),
            crate::proof_kernel::aver_generated::kernel::induct::stopsAt(guard),
            arms.clone(),
        );
        if __pat0 {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(69)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name))));
                    __b
                };
                __b.push_str(&AverStr::from(
                    " recurses on a match whose subject is not a parameter",
                ));
                __b
            }))
        } else {
            match __pat1 {
                Some(stop) => {
                    let __list_subject = __pat2;
                    if let Some((a, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = __pat3;
                            if let Some((b, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat4;
                                    if __list_subject.is_empty() {
                                        if (crate::proof_kernel::aver_generated::kernel::induct::oneEach(&a.pattern, &b.pattern) && (crate::proof_kernel::aver_generated::kernel::induct::armDescends(a, stop.clone(), d, ds, p.clone(), j.clone())? && crate::proof_kernel::aver_generated::kernel::induct::armDescends(b, stop, d, ds, p.clone(), j.clone())?)) { Ok(crate::proof_kernel::aver_generated::kernel::induct::Recursion { at: j, guard: Some(guard.clone()), arms: crate::proof_kernel::aver_generated::kernel::induct::armCalls(arms, d, ds)? }) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(61)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name)))); __b }; __b.push_str(&AverStr::from(" does not count ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(p)))); __b }; __b.push_str(&AverStr::from(" down to zero")); __b })) }
                                    } else {
                                        Err(aver_rt::AverStr::from({
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(61))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&aver_rt::AverStr::from(
                                                            aver_rt::aver_display(&(d.name)),
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_str(&AverStr::from(
                                                        " does not count ",
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(&(p)),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&AverStr::from(" down to zero"));
                                            __b
                                        }))
                                    }
                                }
                            } else {
                                Err(aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(61))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(&(d.name)),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&AverStr::from(" does not count "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(p)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&AverStr::from(" down to zero"));
                                    __b
                                }))
                            }
                        }
                    } else {
                        Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(61))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(d.name)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&AverStr::from(" does not count "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(p))));
                                __b
                            };
                            __b.push_str(&AverStr::from(" down to zero"));
                            __b
                        }))
                    }
                }
                _ => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(61)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(d.name),
                                )));
                                __b
                            };
                            __b.push_str(&AverStr::from(" does not count "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(p))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" down to zero"));
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
    match guard.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(__pat0, _, _) => {
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
        _ => None,
    }
}

/// One arm true, the other false.
pub fn oneEach(
    a @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
    b @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (a.clone(), b.clone());
        match __pat0 {
            crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(__pat2) => match __pat2 {
                crate::proof_kernel::aver_generated::kernel::term::Term::TBool(x) => match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(__pat3) => {
                        match __pat3 {
                            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(y) => {
                                (x != y)
                            }
                            _ => false,
                        }
                    }
                    _ => false,
                },
                _ => false,
            },
            _ => false,
        }
    }
}

/// The arm where p is at most 0 makes no recursive call; each call in the other descends at place j.
#[inline(always)]
pub fn armDescends(
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Arm,
    stop @ _: bool,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    p @ _: AverStr,
    j @ _: aver_rt::AverInt,
) -> Result<bool, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if (a.pattern
        == crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(
            crate::proof_kernel::aver_generated::kernel::term::Term::TBool(stop),
        ))
    {
        Ok((aver_rt::AverInt::from_i64(
            crate::proof_kernel::aver_generated::kernel::induct::calls(a.body, d, ds)?.len() as i64,
        ) == aver_rt::AverInt::from_i64(0)))
    } else {
        Ok(
            crate::proof_kernel::aver_generated::kernel::induct::callsDescend(
                &crate::proof_kernel::aver_generated::kernel::induct::calls(a.body, d, ds)?,
                p,
                j,
                aver_rt::AverInt::from_i64(d.params.len() as i64),
            ),
        )
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
    aver_list_match!(cs.clone(), [] => true, [c, rest] => (((aver_rt::AverInt::from_i64(c.args.len() as i64) == n) && (crate::proof_kernel::aver_generated::kernel::induct::descends(&crate::proof_kernel::aver_generated::kernel::induct::nthTerm(c.args.clone(), j.clone()), p.clone()) && (!c.inner.contains(&p)))) && crate::proof_kernel::aver_generated::kernel::induct::callsDescend(&rest, p, j, n)))
}

/// Whether t is p - 1, or p / k for a literal k of at least 2: for p > 0 each is at least 0 and below p.
pub fn descends(
    t @ _: &Option<crate::proof_kernel::aver_generated::kernel::term::Term>,
    p @ _: AverStr,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        Some(__pat0) => {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    __pat1,
                    __pat2,
                    __pat3,
                ) => {
                    let __pat2 = (*__pat2).clone();
                    let __pat3 = (*__pat3).clone();
                    {
                        let __int_match_subject =
                            aver_rt::AverInt::from_i64(aver_rt::str_code1(&__pat1));
                        if __int_match_subject == aver_rt::AverInt::from_i64(45) {
                            match __pat2 {
        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(q) => {
            match __pat3 {
        crate::proof_kernel::aver_generated::kernel::term::Term::TInt(__pat4) => {
            { let __int_match_subject = __pat4; if __int_match_subject == aver_rt::AverInt::from_i64(1) { (q == p) } else { false } }
        },
        _ => {
            false
        }
    }
        },
        _ => {
            false
        }
    }
                        } else {
                            false
                        }
                    }
                }
                crate::proof_kernel::aver_generated::kernel::term::Term::TBi(__pat5, __pat6) => {
                    match &*__pat5 {
                        "__int_div_euclid" => {
                            let __list_subject = __pat6;
                            if let Some((__pat7, __pat8)) =
                                aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                match __pat7 {
        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(q) => {
            { let __list_subject = __pat8; if let Some((__pat9, __pat10)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat9 {
        crate::proof_kernel::aver_generated::kernel::term::Term::TInt(k) => {
            { let __list_subject = __pat10; if __list_subject.is_empty() { ((q == p) && (k >= aver_rt::AverInt::from_i64(2))) } else { false } }
        },
        _ => {
            false
        }
    } } else { false } }
        },
        _ => {
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
        _ => false,
    }
}

/// Whether steps may open d: its recursion, read through the definitions of ds it calls back through, passes the gate; or every cycle through d passes one whose recursion does (rooted).
#[inline(always)]
pub fn openGate(
    mut d @ _: crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<(), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::induct::recursion(&d, ds) {
        Ok(_) => Ok(()),
        Err(why @ _) => {
            if crate::proof_kernel::aver_generated::kernel::induct::rooted(
                d,
                ds.clone(),
                ds.clone(),
            ) {
                Ok(())
            } else {
                Err(why)
            }
        }
    }
}

/// Whether some definition r of ds reaches d and is reached from it, and r's recursion, read through the others, passes the gate. Every cycle through d then passes r, each time on a smaller value, so d stops.
#[inline(always)]
pub fn rooted(
    d @ _: crate::proof_kernel::aver_generated::kernel::proof::Def,
    mut todo @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    ds @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> bool {
    let d @ _ = std::sync::Arc::new(d);
    let ds @ _ = std::sync::Arc::new(ds);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(todo, [] => { return false; }, [r, rest] => { if ((crate::proof_kernel::aver_generated::kernel::induct::reachesFrom(d.name.clone(), r.name.clone(), &*ds) && crate::proof_kernel::aver_generated::kernel::induct::reachesFrom(r.name.clone(), d.name.clone(), &*ds)) && crate::proof_kernel::aver_generated::kernel::induct::descendsThrough(&r, &*ds)) { return true; } else { {
            let __tco1 = rest;
            todo = __tco1;
            continue;
        } } })
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
    ((a == b)
        || crate::proof_kernel::aver_generated::kernel::induct::reaches(
            crate::proof_kernel::aver_generated::kernel::induct::calleesOf(
                a,
                ds.clone(),
                ds.clone(),
            ),
            b,
            ds.clone(),
            aver_rt::AverList::empty(),
            aver_rt::AverInt::from_i64(ds.len() as i64),
        ))
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
    crate::proof_kernel::aver_generated::kernel::proof::Def {
        name: AverStr::from("any"),
        returnsBool: true,
        params: aver_rt::AverList::from_vec(vec![AverStr::from("xs")]),
        lets: aver_rt::AverList::empty(),
        body: crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(
            std::sync::Arc::new(
                crate::proof_kernel::aver_generated::kernel::term::Term::TVar(AverStr::from("xs")),
            ),
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::term::Arm {
                    pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PNil,
                    body: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false),
                },
                crate::proof_kernel::aver_generated::kernel::term::Arm {
                    pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(
                        AverStr::from("h"),
                        AverStr::from("t"),
                    ),
                    body: crate::proof_kernel::aver_generated::kernel::term::Term::TCall(
                        AverStr::from("orOn"),
                        aver_rt::AverList::from_vec(vec![
                            crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                                AverStr::from("h"),
                            ),
                            crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                                AverStr::from("t"),
                            ),
                        ]),
                    ),
                },
            ]),
        ),
    }
}

/// A fixture for the tests: orOn(first, rest) is true when first is not 0, and back otherwise.
pub fn orOn(
    back @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> crate::proof_kernel::aver_generated::kernel::proof::Def {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::proof::Def {
        name: AverStr::from("orOn"),
        returnsBool: true,
        params: aver_rt::AverList::from_vec(vec![AverStr::from("first"), AverStr::from("rest")]),
        lets: aver_rt::AverList::empty(),
        body: crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(
            std::sync::Arc::new(
                crate::proof_kernel::aver_generated::kernel::term::Term::TOp(
                    AverStr::from("!="),
                    std::sync::Arc::new(
                        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                            AverStr::from("first"),
                        ),
                    ),
                    std::sync::Arc::new(
                        crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                ),
            ),
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::term::Arm {
                    pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(
                        crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true),
                    ),
                    body: crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true),
                },
                crate::proof_kernel::aver_generated::kernel::term::Arm {
                    pattern: crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(
                        crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false),
                    ),
                    body: back.clone(),
                },
            ]),
        ),
    }
}

/// Every arm passes, at place j of each recursive call, a name its pattern binds and no inner arm rebinds.
#[inline(always)]
pub fn armsShrink(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    d @ _: &crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    j @ _: aver_rt::AverInt,
) -> Result<bool, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(arms.clone(), [] => Ok(true), [a, rest] => Ok((crate::proof_kernel::aver_generated::kernel::induct::callsShrink(&crate::proof_kernel::aver_generated::kernel::induct::calls(a.body, d, ds)?, &crate::proof_kernel::aver_generated::kernel::subst::patNames(&a.pattern), j.clone(), aver_rt::AverInt::from_i64(d.params.len() as i64)) && crate::proof_kernel::aver_generated::kernel::induct::armsShrink(&rest, d, ds, j)?)))
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
    aver_list_match!(cs.clone(), [] => true, [c, rest] => (((aver_rt::AverInt::from_i64(c.args.len() as i64) == n) && crate::proof_kernel::aver_generated::kernel::induct::smaller(&crate::proof_kernel::aver_generated::kernel::induct::nthTerm(c.args.clone(), j.clone()), parts, &c.inner)) && crate::proof_kernel::aver_generated::kernel::induct::callsShrink(&rest, parts, j, n)))
}

/// A name the arm's pattern binds, not rebound inside.
pub fn smaller(
    t @ _: &Option<crate::proof_kernel::aver_generated::kernel::term::Term>,
    parts @ _: &aver_rt::AverList<AverStr>,
    inner @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        Some(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::term::Term::TVar(b) => {
                (parts.contains(&b) && (!inner.contains(&b)))
            }
            _ => false,
        },
        _ => false,
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
    mut xs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut i @ _: aver_rt::AverInt,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Term> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(xs, [] => { return None; }, [x, rest] => { if (i == aver_rt::AverInt::from_i64(0)) { return Some(x); } else { {
            let __tco0 = rest;
            let __tco1 = i.sub(&aver_rt::AverInt::from_i64(1));
            xs = __tco0;
            i = __tco1;
            continue;
        } } })
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
            crate::proof_kernel::aver_generated::kernel::induct::eachNotMutual(
                ds.clone(),
                ds.clone(),
            )
        }
    }
}

/// Check each definition in turn.
#[inline(always)]
pub fn eachNotMutual(
    mut todo @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    ds @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> Result<(), AverStr> {
    let ds @ _ = std::sync::Arc::new(ds);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(todo, [] => { return Ok(()); }, [d, rest] => { if (crate::proof_kernel::aver_generated::kernel::induct::reaches(crate::proof_kernel::aver_generated::kernel::induct::callees(d.clone(), &*ds), d.name.clone(), (*ds).clone(), aver_rt::AverList::empty(), aver_rt::AverInt::from_i64(ds.len() as i64)) && (!crate::proof_kernel::aver_generated::kernel::induct::rooted(d.clone(), (*ds).clone(), (*ds).clone()))) { return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(46)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(d.name)))); __b }; __b.push_str(&AverStr::from(" is part of a mutual recursion")); __b })); } else { {
            let __tco0 = rest;
            todo = __tco0;
            continue;
        } } })
    }
}

/// Whether target is among the definitions reachable from the frontier; fuel is the number of definitions, enough for any path that does not repeat.
pub fn reaches(
    mut frontier @ _: aver_rt::AverList<AverStr>,
    mut target @ _: AverStr,
    ds @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    mut seen @ _: aver_rt::AverList<AverStr>,
    mut fuel @ _: aver_rt::AverInt,
) -> bool {
    let ds @ _ = std::sync::Arc::new(ds);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __int_match_subject = (frontier, (fuel > aver_rt::AverInt::from_i64(0)));
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
                                    (*ds).clone(),
                                    (*ds).clone(),
                                ),
                            );
                            let __tco3 = aver_rt::AverList::prepend(g, &seen);
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

/// The definitions of ds that the one named name calls.
#[inline(always)]
pub fn calleesOf(
    mut name @ _: AverStr,
    mut todo @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    ds @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> aver_rt::AverList<AverStr> {
    let ds @ _ = std::sync::Arc::new(ds);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(todo, [] => { return aver_rt::AverList::empty(); }, [d, rest] => { if (d.name == name) { return crate::proof_kernel::aver_generated::kernel::induct::callees(d, &*ds); } else { {
            let __tco1 = rest;
            todo = __tco1;
            continue;
        } } })
    }
}

/// The other definitions of the script d calls.
#[inline(always)]
pub fn callees(
    mut d @ _: crate::proof_kernel::aver_generated::kernel::proof::Def,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::induct::onlyDefs(
        aver_rt::AverList::concat(
            &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(d.body),
            &crate::proof_kernel::aver_generated::kernel::induct::letNames(&d.lets),
        ),
        ds.clone(),
        d.name,
    )
}

/// Functions the local bindings call.
#[inline(always)]
pub fn letNames(
    ls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ls.clone(), [] => aver_rt::AverList::empty(), [b, rest] => aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::induct::namesCalled(b.value), &crate::proof_kernel::aver_generated::kernel::induct::letNames(&rest)))
}

/// The names that are other definitions of the script.
#[inline(always)]
pub fn onlyDefs(
    mut ns @ _: aver_rt::AverList<AverStr>,
    ds @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    mut own @ _: AverStr,
) -> aver_rt::AverList<AverStr> {
    let ds @ _ = std::sync::Arc::new(ds);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(ns, [] => { return aver_rt::AverList::empty(); }, [n, rest] => { if ((n != own) && crate::proof_kernel::aver_generated::kernel::induct::isDef(n.clone(), &*ds)) { return aver_rt::AverList::prepend(n, &crate::proof_kernel::aver_generated::kernel::induct::onlyDefs(rest, (*ds).clone(), own)); } else { {
            let __tco0 = rest;
            ns = __tco0;
            continue;
        } } })
    }
}

/// Whether the script defines n.
#[inline(always)]
pub fn isDef(
    n @ _: AverStr,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ds.clone(), [] => false, [d, rest] => ((d.name == n) || crate::proof_kernel::aver_generated::kernel::induct::isDef(n, &rest)))
}

/// Every function a term calls.
pub fn namesCalled(
    mut t @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> aver_rt::AverList<AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        match t {
            crate::proof_kernel::aver_generated::kernel::term::Term::TCall(g, xs) => {
                return aver_rt::AverList::prepend(
                    g,
                    &crate::proof_kernel::aver_generated::kernel::induct::namesAll(&xs),
                );
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(s, arms) => {
                let s = (*s).clone();
                return aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(s),
                    &crate::proof_kernel::aver_generated::kernel::induct::namesArms(&arms),
                );
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TGet(o, n) => {
                let o = (*o).clone();
                {
                    let __tco0 = o;
                    t = __tco0;
                    continue;
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TBi(n, xs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::namesAll(&xs);
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TOp(o, a, b) => {
                let a = (*a).clone();
                let b = (*b).clone();
                return aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(a),
                    &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(b),
                );
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TNeg(a) => {
                let a = (*a).clone();
                {
                    let __tco0 = a;
                    t = __tco0;
                    continue;
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(c, xs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::namesAll(&xs);
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TParts(xs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::namesAll(&xs);
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TList(xs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::namesAll(&xs);
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TTuple(xs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::namesAll(&xs);
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TRec(n, fs) => {
                return crate::proof_kernel::aver_generated::kernel::induct::namesFields(&fs);
            }
            crate::proof_kernel::aver_generated::kernel::term::Term::TUpd(n, b, fs) => {
                let b = (*b).clone();
                return aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::induct::namesCalled(b),
                    &crate::proof_kernel::aver_generated::kernel::induct::namesFields(&fs),
                );
            }
            _ => {
                return aver_rt::AverList::empty();
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
    aver_list_match!(xs.clone(), [] => aver_rt::AverList::empty(), [x, rest] => aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::induct::namesCalled(x), &crate::proof_kernel::aver_generated::kernel::induct::namesAll(&rest)))
}

/// Functions record fields call.
#[inline(always)]
pub fn namesFields(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => aver_rt::AverList::empty(), [x, rest] => aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::induct::namesCalled(x.value), &crate::proof_kernel::aver_generated::kernel::induct::namesFields(&rest)))
}

/// Functions match arms call.
#[inline(always)]
pub fn namesArms(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(arms.clone(), [] => aver_rt::AverList::empty(), [a, rest] => aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::induct::namesCalled(a.body), &crate::proof_kernel::aver_generated::kernel::induct::namesArms(&rest)))
}

/// The given at the matched place, and the other givens among the arguments that vary with it, each at the first place it appears.
pub fn varied(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    j @ _: aver_rt::AverInt,
    givens @ _: &aver_rt::AverList<AverStr>,
) -> Result<Varied, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::induct::nthTerm(xs.clone(), j.clone()) {
        Some(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::term::Term::TVar(v) => {
                if givens.contains(&v) {
                    Ok(
                        crate::proof_kernel::aver_generated::kernel::induct::Varied {
                            v: v.clone(),
                            general:
                                crate::proof_kernel::aver_generated::kernel::induct::generalised(
                                    xs.clone(),
                                    aver_rt::AverInt::from_i64(0),
                                    j,
                                    v,
                                    givens.clone(),
                                    aver_rt::AverList::empty(),
                                ),
                        },
                    )
                } else {
                    Err(AverStr::from(
                        "the argument at the matched place must be a given",
                    ))
                }
            }
            _ => Err(AverStr::from(
                "the argument at the matched place must be a given",
            )),
        },
        _ => Err(AverStr::from(
            "the argument at the matched place must be a given",
        )),
    }
}

/// Places other than j holding a given other than v not seen before.
#[inline(always)]
pub fn generalised(
    mut xs @ _: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut k @ _: aver_rt::AverInt,
    mut j @ _: aver_rt::AverInt,
    mut v @ _: AverStr,
    givens @ _: aver_rt::AverList<AverStr>,
    seen @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<Place> {
    let givens @ _ = std::sync::Arc::new(givens);
    let seen @ _ = std::sync::Arc::new(seen);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(xs, [] => { return aver_rt::AverList::empty(); }, [x, rest] => { match x {
        crate::proof_kernel::aver_generated::kernel::term::Term::TVar(n) => {
            if (((k != j) && (n != v)) && (givens.contains(&n) && (!seen.contains(&n)))) { return aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::induct::Place { at: k.clone(), name: n.clone() }, &crate::proof_kernel::aver_generated::kernel::induct::generalised(rest, k.add(&aver_rt::AverInt::from_i64(1)), j, v, (*givens).clone(), aver_rt::AverList::prepend(n, &(*seen).clone()))); } else { {
            let __tco0 = rest;
            let __tco1 = k.add(&aver_rt::AverInt::from_i64(1));
            xs = __tco0;
            k = __tco1;
            continue;
        } }
        },
        _ => {
            {
            let __tco0 = rest;
            let __tco1 = k.add(&aver_rt::AverInt::from_i64(1));
            xs = __tco0;
            k = __tco1;
            continue;
        }
        }
    } })
    }
}
