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
                {
                    let __list_subject = bs;
                    if __list_subject.is_empty() {
                        return Ok(());
                    } else {
                        if let (Some(b), Some(rest)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            __MutualTco1::CaptureOne(b.clone(), rest)
                        } else {
                            panic!("Aver Rust codegen: non-exhaustive list match")
                        }
                    }
                }
            }
            __MutualTco1::CaptureOne(mut b @ _, mut rest @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                if used.contains(&b.name) {
                    {
                        let __subject = {
                            let __arg5 @ _ =
                                crate::proof_kernel::aver_generated::kernel::subst::freeVars(
                                    &b.value,
                                );
                            crate::proof_kernel::aver_generated::kernel::subst::firstShared(
                                &__arg5,
                                (*bound).clone(),
                            )
                        };
                        match __subject {
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
                                                __b.push_str(&::aver_rt::str_lit!("substituting "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(b.name)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" would capture "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(c),
                                    )));
                                    __b
                                }));
                            }
                            None => __MutualTco1::Capture(rest),
                        }
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
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Option<crate::proof_kernel::aver_generated::kernel::term::Term> {
    let mut __hold1: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>;
    let mut bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding> =
        bs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = bs;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(b), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (b.name == name) {
                        return Some(b.value.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            bs = &__hold1;
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

/// The names a pattern binds, wildcards excluded.
pub fn patNames(
    p @ _: &crate::proof_kernel::aver_generated::kernel::term::Pat,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match p {
        crate::proof_kernel::aver_generated::kernel::term::Pat::PVar(n) => {
            let n = n.clone();
            {
                let __arg7 @ _ = aver_rt::AverList::from_vec(vec![n]);
                crate::proof_kernel::aver_generated::kernel::subst::dropWild(&__arg7)
            }
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            {
                let __arg8 @ _ = aver_rt::AverList::from_vec(vec![h, t]);
                crate::proof_kernel::aver_generated::kernel::subst::dropWild(&__arg8)
            }
        }
        crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(c, ns) => {
            let c = c.clone();
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
    {
        let __list_subject = ps;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::subst::patNames(p),
                    &crate::proof_kernel::aver_generated::kernel::subst::patNamesAll(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Names other than the wildcard.
#[inline(always)]
pub fn dropWild(ns @ _: &aver_rt::AverList<AverStr>) -> aver_rt::AverList<AverStr> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut ns @ _: &aver_rt::AverList<AverStr> = ns;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = ns;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    if (&*n == "_") {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            ns = &__hold0;
                            continue;
                        }
                    } else {
                        return aver_rt::AverList::prepend(
                            n,
                            &crate::proof_kernel::aver_generated::kernel::subst::dropWild(&rest),
                        );
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Variables of a term not bound by a match arm inside it (with repeats); the hole counts as one.
pub fn freeVars(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> aver_rt::AverList<AverStr> {
    let mut __hold0: crate::proof_kernel::aver_generated::kernel::term::Term;
    let mut t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term = t;
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
                    return aver_rt::AverList::from_vec(vec![::aver_rt::str_lit!("□")]);
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TGet(__o_fields) => {
                    let o = __o_fields.0.clone();
                    let f = __o_fields.1.clone();
                    {
                        let __tco0 = o;
                        __hold0 = __tco0;
                        t = &__hold0;
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
                    let a = &__o_fields.1;
                    let b = &__o_fields.2;
                    return aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(a),
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(b),
                    );
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                    let a = __a_fields.0.clone();
                    {
                        let __tco0 = a;
                        __hold0 = __tco0;
                        t = &__hold0;
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
                    let s = &__s_fields.0;
                    let arms = &__s_fields.1;
                    return aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(s),
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVarsArms(arms),
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
                    let b = &__n_fields.1;
                    let fs = &__n_fields.2;
                    return aver_rt::AverList::concat(
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVars(b),
                        &crate::proof_kernel::aver_generated::kernel::subst::freeVarsFields(fs),
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
    {
        let __list_subject = xs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::subst::freeVars(x),
                    &crate::proof_kernel::aver_generated::kernel::subst::freeVarsAll(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Free variables of record fields.
#[inline(always)]
pub fn freeVarsFields(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = fs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::concat(
                    &crate::proof_kernel::aver_generated::kernel::subst::freeVars(&f.value),
                    &crate::proof_kernel::aver_generated::kernel::subst::freeVarsFields(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Free variables of match arms, without each arm's own binders.
#[inline(always)]
pub fn freeVarsArms(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = arms;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::concat(
                    &{
                        let __arg3 @ _ =
                            crate::proof_kernel::aver_generated::kernel::subst::freeVars(&a.body);
                        {
                            let __arg4 @ _ =
                                crate::proof_kernel::aver_generated::kernel::subst::patNames(
                                    &a.pattern,
                                );
                            crate::proof_kernel::aver_generated::kernel::subst::without(
                                &__arg3, __arg4,
                            )
                        }
                    },
                    &crate::proof_kernel::aver_generated::kernel::subst::freeVarsArms(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Names not in the dropped list.
#[inline(always)]
pub fn without(
    ns @ _: &aver_rt::AverList<AverStr>,
    mut drop @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<AverStr> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut ns @ _: &aver_rt::AverList<AverStr> = ns;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = ns;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    if drop.contains(&n) {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            ns = &__hold0;
                            continue;
                        }
                    } else {
                        return aver_rt::AverList::prepend(
                            n,
                            &crate::proof_kernel::aver_generated::kernel::subst::without(
                                &rest, drop,
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

/// How many holes a term holds.
pub fn holeCount(
    t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> aver_rt::AverInt {
    let mut __hold0: crate::proof_kernel::aver_generated::kernel::term::Term;
    let mut t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term = t;
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
                        __hold0 = __tco0;
                        t = &__hold0;
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
                    let a = &__o_fields.1;
                    let b = &__o_fields.2;
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCount(a)
                        .add(&crate::proof_kernel::aver_generated::kernel::subst::holeCount(b));
                }
                crate::proof_kernel::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                    let a = __a_fields.0.clone();
                    {
                        let __tco0 = a;
                        __hold0 = __tco0;
                        t = &__hold0;
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
                    let s = &__s_fields.0;
                    let arms = &__s_fields.1;
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCount(s).add(
                        &crate::proof_kernel::aver_generated::kernel::subst::holeCountArms(arms),
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
                    let b = &__n_fields.1;
                    let fs = &__n_fields.2;
                    return crate::proof_kernel::aver_generated::kernel::subst::holeCount(b).add(
                        &crate::proof_kernel::aver_generated::kernel::subst::holeCountFields(fs),
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
    {
        let __list_subject = xs;
        if __list_subject.is_empty() {
            aver_rt::AverInt::from_i64(0)
        } else {
            if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                crate::proof_kernel::aver_generated::kernel::subst::holeCount(x)
                    .add(&crate::proof_kernel::aver_generated::kernel::subst::holeCountAll(&rest))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Holes in record fields.
#[inline(always)]
pub fn holeCountFields(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
) -> aver_rt::AverInt {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = fs;
        if __list_subject.is_empty() {
            aver_rt::AverInt::from_i64(0)
        } else {
            if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                crate::proof_kernel::aver_generated::kernel::subst::holeCount(&f.value).add(
                    &crate::proof_kernel::aver_generated::kernel::subst::holeCountFields(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Holes in match arm bodies.
#[inline(always)]
pub fn holeCountArms(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
) -> aver_rt::AverInt {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = arms;
        if __list_subject.is_empty() {
            aver_rt::AverInt::from_i64(0)
        } else {
            if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                crate::proof_kernel::aver_generated::kernel::subst::holeCount(&a.body)
                    .add(&crate::proof_kernel::aver_generated::kernel::subst::holeCountArms(&rest))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
                Ok(crate::proof_kernel::aver_generated::kernel::subst::lookup(n, bs).unwrap_or(t))
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::THole => {
                Ok(crate::proof_kernel::aver_generated::kernel::subst::lookup(
                    ::aver_rt::str_lit!("□"),
                    bs,
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
                let xs = &__f_fields.1;
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TCall(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TCall(
                            f,
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(__f_fields) => {
                let f = __f_fields.0.clone();
                let xs = &__f_fields.1;
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            f,
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(xs, bs)?,
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
                let xs = &__c_fields.1;
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                            c,
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TMatch(__s_fields) => {
                let s = __s_fields.0.clone();
                let arms = &__s_fields.1;
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TMatch(
                            crate::proof_kernel::aver_generated::kernel::subst::subst(s, bs)?,
                            crate::proof_kernel::aver_generated::kernel::subst::substArms(
                                arms, bs,
                            )?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TParts(__xs_fields) => {
                let xs = &__xs_fields.0;
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TParts(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TParts(
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TList(__xs_fields) => {
                let xs = &__xs_fields.0;
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TList(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TList(
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TTuple(__xs_fields) => {
                let xs = &__xs_fields.0;
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TTuple(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TTuple(
                            crate::proof_kernel::aver_generated::kernel::subst::substAll(xs, bs)?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TRec(__n_fields) => {
                let n = __n_fields.0.clone();
                let fs = &__n_fields.1;
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TRec(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TRec(
                            n,
                            crate::proof_kernel::aver_generated::kernel::subst::substFields(
                                fs, bs,
                            )?,
                        ),
                    ),
                )
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TUpd(__n_fields) => {
                let n = __n_fields.0.clone();
                let b = __n_fields.1.clone();
                let fs = &__n_fields.2;
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TUpd(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TUpd(
                            n,
                            crate::proof_kernel::aver_generated::kernel::subst::subst(b, bs)?,
                            crate::proof_kernel::aver_generated::kernel::subst::substFields(
                                fs, bs,
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
    {
        let __list_subject = xs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend(
                    crate::proof_kernel::aver_generated::kernel::subst::subst(x.clone(), bs)?,
                    &crate::proof_kernel::aver_generated::kernel::subst::substAll(&rest, bs)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Substitute in record fields.
#[inline(always)]
pub fn substFields(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = fs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend(
                    crate::proof_kernel::aver_generated::kernel::term::Field {
                        name: f.name.clone(),
                        value: crate::proof_kernel::aver_generated::kernel::subst::subst(
                            f.value.clone(),
                            bs,
                        )?,
                    },
                    &crate::proof_kernel::aver_generated::kernel::subst::substFields(&rest, bs)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Substitute under each arm, minus the names the arm binds.
#[inline(always)]
pub fn substArms(
    arms @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = arms;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend(
                    crate::proof_kernel::aver_generated::kernel::subst::substArm(a.clone(), bs)?,
                    &crate::proof_kernel::aver_generated::kernel::subst::substArms(&rest, bs)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Substitute in one arm, refusing a capture.
pub fn substArm(
    mut a @ _: crate::proof_kernel::aver_generated::kernel::term::Arm,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Arm, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let bound @ _ = crate::proof_kernel::aver_generated::kernel::subst::patNames(&a.pattern);
    let inner @ _ = crate::proof_kernel::aver_generated::kernel::subst::unbind(bs, bound.clone());
    {
        let __arg5 @ _ = crate::proof_kernel::aver_generated::kernel::subst::freeVars(&a.body);
        crate::proof_kernel::aver_generated::kernel::subst::capture(inner.clone(), &__arg5, &bound)
    }?;
    Ok(crate::proof_kernel::aver_generated::kernel::term::Arm {
        pattern: a.pattern,
        body: crate::proof_kernel::aver_generated::kernel::subst::subst(a.body, &inner)?,
    })
}

/// Drop bindings for names a pattern shadows.
#[inline(always)]
pub fn unbind(
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    mut names @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding> {
    let mut __hold0: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>;
    let mut bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding> =
        bs;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = bs;
            if __list_subject.is_empty() {
                return aver_rt::AverList::empty();
            } else {
                if let (Some(b), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if names.contains(&b.name) {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            bs = &__hold0;
                            continue;
                        }
                    } else {
                        return aver_rt::AverList::prepend(
                            b.clone(),
                            &crate::proof_kernel::aver_generated::kernel::subst::unbind(
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

/// The first name also in the other list.
#[inline(always)]
pub fn firstShared(
    ns @ _: &aver_rt::AverList<AverStr>,
    mut others @ _: aver_rt::AverList<AverStr>,
) -> Option<AverStr> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut ns @ _: &aver_rt::AverList<AverStr> = ns;
    loop {
        crate::proof_kernel::cancel_checkpoint();
        {
            let __list_subject = ns;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    if others.contains(&n) {
                        return Some(n);
                    } else {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            ns = &__hold0;
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
