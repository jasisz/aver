#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    MatchPatTupleItemsAcc(
        aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
        aver_rt::AverList<crate::aver_generated::domain::value::Val>,
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    ),
    MatchPatTupleOne(
        crate::aver_generated::domain::ast::Pattern,
        crate::aver_generated::domain::value::Val,
        aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
        aver_rt::AverList<crate::aver_generated::domain::value::Val>,
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::MatchPatTupleItemsAcc(mut pats @ _, mut items @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                aver_list_match!(pats, [] => { return Ok(acc) }, [pat, restPats] => { aver_list_match!(items, [] => { return Err(AverStr::from("no match")) }, [item, restItems] => __MutualTco1::MatchPatTupleOne(pat, item, restPats, restItems, acc)) })
            }
            __MutualTco1::MatchPatTupleOne(
                mut pat @ _,
                mut item @ _,
                mut restPats @ _,
                mut restItems @ _,
                mut acc @ _,
            ) => {
                crate::cancel_checkpoint();
                let bindings @ _ =
                    crate::aver_generated::domain::match_mod::matchPattern(&pat, &item)?;
                __MutualTco1::MatchPatTupleItemsAcc(
                    restPats,
                    restItems,
                    aver_rt::AverList::concat(&acc, &bindings),
                )
            }
        };
    }
}

/// Accumulate bindings from tuple pattern matching.
pub fn matchPatTupleItemsAcc(
    pats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    acc @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::MatchPatTupleItemsAcc(
        pats.clone(),
        items.clone(),
        acc.clone(),
    ))
}

/// Match one tuple element and continue with accumulated bindings.
pub fn matchPatTupleOne(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
    item @ _: &crate::aver_generated::domain::value::Val,
    restPats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    restItems @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    acc @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::MatchPatTupleOne(
        pat.clone(),
        item.clone(),
        restPats.clone(),
        restItems.clone(),
        acc.clone(),
    ))
}

/// Try to match a value against a pattern. Returns bindings on success.
pub fn matchPattern(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match pat.clone() {
        crate::aver_generated::domain::ast::Pattern::PatWild => Ok(aver_rt::AverList::empty()),
        crate::aver_generated::domain::ast::Pattern::PatInt(n) => {
            crate::aver_generated::domain::match_mod::matchPatInt(n, v)
        }
        crate::aver_generated::domain::ast::Pattern::PatFloat(f) => {
            crate::aver_generated::domain::match_mod::matchPatFloat(f, v)
        }
        crate::aver_generated::domain::ast::Pattern::PatBool(b) => {
            crate::aver_generated::domain::match_mod::matchPatBool(b, v)
        }
        crate::aver_generated::domain::ast::Pattern::PatStr(s) => {
            crate::aver_generated::domain::match_mod::matchPatStr(s, v)
        }
        crate::aver_generated::domain::ast::Pattern::PatEmpty => {
            crate::aver_generated::domain::match_mod::matchPatEmpty(v)
        }
        crate::aver_generated::domain::ast::Pattern::PatCons(h, t) => {
            crate::aver_generated::domain::match_mod::matchPatCons(h, t, v)
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructor(ctorName, bindings) => {
            crate::aver_generated::domain::match_mod::matchPatConstructor(ctorName, &bindings, v)
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructorId(tag, ctorName, bindings) => {
            crate::aver_generated::domain::match_mod::matchPatConstructorById(
                tag, ctorName, &bindings, v,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatTuple(pats) => {
            crate::aver_generated::domain::match_mod::matchPatTuple(&pats, v)
        }
        crate::aver_generated::domain::ast::Pattern::PatVar(name) => {
            Ok(aver_rt::AverList::from_vec(vec![(name, v.clone())]))
        }
    }
}

/// Match a PatInt pattern against a value.
pub fn matchPatInt(
    n @ _: aver_rt::AverInt,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValInt(m) => {
            if (n == m) {
                Ok(aver_rt::AverList::empty())
            } else {
                Err(AverStr::from("no match"))
            }
        }
        _ => Err(AverStr::from("no match")),
    }
}

/// Match a PatFloat pattern against a value.
pub fn matchPatFloat(
    f @ _: f64,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValFloat(vf) => {
            if (f == vf) {
                Ok(aver_rt::AverList::empty())
            } else {
                Err(AverStr::from("no match"))
            }
        }
        _ => Err(AverStr::from("no match")),
    }
}

/// Match a PatBool pattern against a value.
pub fn matchPatBool(
    b @ _: bool,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValBool(vb) => {
            if (b == vb) {
                Ok(aver_rt::AverList::empty())
            } else {
                Err(AverStr::from("no match"))
            }
        }
        _ => Err(AverStr::from("no match")),
    }
}

/// Match a string literal pattern.
pub fn matchPatStr(
    s @ _: AverStr,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValStr(vs) => {
            if (s == vs) {
                Ok(aver_rt::AverList::empty())
            } else {
                Err(AverStr::from("no match"))
            }
        }
        _ => Err(AverStr::from("no match")),
    }
}

/// Match empty list pattern [].
pub fn matchPatEmpty(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValList(items) => {
            let __list_subject = items;
            if __list_subject.is_empty() {
                Ok(aver_rt::AverList::empty())
            } else {
                Err(AverStr::from("no match"))
            }
        }
        _ => Err(AverStr::from("no match")),
    }
}

/// Match cons pattern [h, ..t].
pub fn matchPatCons(
    h @ _: AverStr,
    t @ _: AverStr,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValList(items) => {
            aver_list_match!(items, [] => Err(AverStr::from("no match")), [head, tail] => Ok(aver_rt::AverList::from_vec(vec![(h, head), (t, crate::aver_generated::domain::value::Val::ValList(tail))])))
        }
        _ => Err(AverStr::from("no match")),
    }
}

/// Match constructor pattern. Checks tag name and extracts inner values.
#[inline(always)]
pub fn matchPatConstructor(
    ctorName @ _: AverStr,
    bindings @ _: &aver_rt::AverList<AverStr>,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = ctorName.clone();
        if &*__dispatch_subject == "Result.Ok" {
            crate::aver_generated::domain::match_mod::matchWrapperPat(
                v,
                AverStr::from("Ok"),
                bindings,
            )
        } else {
            if &*__dispatch_subject == "Result.Err" {
                crate::aver_generated::domain::match_mod::matchWrapperPat(
                    v,
                    AverStr::from("Err"),
                    bindings,
                )
            } else {
                if &*__dispatch_subject == "Option.Some" {
                    crate::aver_generated::domain::match_mod::matchWrapperPat(
                        v,
                        AverStr::from("Some"),
                        bindings,
                    )
                } else {
                    if &*__dispatch_subject == "Option.None" {
                        crate::aver_generated::domain::match_mod::matchNonePat(v)
                    } else {
                        crate::aver_generated::domain::match_mod::matchGenericConstructor(
                            ctorName, bindings, v,
                        )
                    }
                }
            }
        }
    }
}

/// Match tuple pattern with nested patterns.
pub fn matchPatTuple(
    pats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValTuple(items) => {
            crate::aver_generated::domain::match_mod::matchPatTupleItems(pats, &items)
        }
        _ => Err(AverStr::from("no match")),
    }
}

/// Match each pattern against corresponding tuple item.
pub fn matchPatTupleItems(
    pats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::match_mod::matchPatTupleItemsAcc(
        pats,
        items,
        &aver_rt::AverList::empty(),
    )
}

/// Match a generic variant constructor pattern using the full name stored in the value.
pub fn matchGenericConstructor(
    ctorName @ _: AverStr,
    bindings @ _: &aver_rt::AverList<AverStr>,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValVariant(_, fullName, fields) => {
            crate::aver_generated::domain::match_mod::matchGenericCtorCheck(
                ctorName, fullName, bindings, &fields,
            )
        }
        _ => Err(AverStr::from("no match")),
    }
}

/// Check if variant full name matches constructor name.
#[inline(always)]
pub fn matchGenericCtorCheck(
    ctorName @ _: AverStr,
    fullName @ _: AverStr,
    bindings @ _: &aver_rt::AverList<AverStr>,
    fields @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    if (fullName == ctorName) {
        Ok(crate::aver_generated::domain::match_mod::zipBindings(
            bindings, fields,
        ))
    } else {
        Err(AverStr::from("no match"))
    }
}

/// Match constructor pattern by integer tag ID. Builtins dispatch directly; user variants confirm both tag and full name.
#[inline(always)]
pub fn matchPatConstructorById(
    tag @ _: aver_rt::AverInt,
    ctorName @ _: AverStr,
    bindings @ _: &aver_rt::AverList<AverStr>,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = tag.clone();
        if __dispatch_subject == aver_rt::AverInt::from_i64(1) {
            crate::aver_generated::domain::match_mod::matchWrapperPatDirect(
                v,
                aver_rt::AverInt::from_i64(1),
                bindings,
            )
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(2) {
                crate::aver_generated::domain::match_mod::matchWrapperPatDirect(
                    v,
                    aver_rt::AverInt::from_i64(2),
                    bindings,
                )
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(3) {
                    crate::aver_generated::domain::match_mod::matchWrapperPatDirect(
                        v,
                        aver_rt::AverInt::from_i64(3),
                        bindings,
                    )
                } else {
                    if __dispatch_subject == aver_rt::AverInt::from_i64(4) {
                        crate::aver_generated::domain::match_mod::matchNonePat(v)
                    } else {
                        crate::aver_generated::domain::match_mod::matchGenericCtorById(
                            tag, ctorName, bindings, v,
                        )
                    }
                }
            }
        }
    }
}

/// Match Ok/Err/Some wrapper by integer tag. 1=Ok, 2=Err, 3=Some.
#[inline(always)]
pub fn matchWrapperPatDirect(
    v @ _: &crate::aver_generated::domain::value::Val,
    tag @ _: aver_rt::AverInt,
    bindings @ _: &aver_rt::AverList<AverStr>,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = tag;
        if __dispatch_subject == aver_rt::AverInt::from_i64(1) {
            match v.clone() {
                crate::aver_generated::domain::value::Val::ValOk(inner) => {
                    let inner = (*inner).clone();
                    Ok(crate::aver_generated::domain::match_mod::zipBindings(
                        bindings,
                        &aver_rt::AverList::from_vec(vec![inner]),
                    ))
                }
                _ => Err(AverStr::from("no match")),
            }
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(2) {
                match v.clone() {
                    crate::aver_generated::domain::value::Val::ValErr(inner) => {
                        let inner = (*inner).clone();
                        Ok(crate::aver_generated::domain::match_mod::zipBindings(
                            bindings,
                            &aver_rt::AverList::from_vec(vec![inner]),
                        ))
                    }
                    _ => Err(AverStr::from("no match")),
                }
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(3) {
                    match v.clone() {
                        crate::aver_generated::domain::value::Val::ValSome(inner) => {
                            let inner = (*inner).clone();
                            Ok(crate::aver_generated::domain::match_mod::zipBindings(
                                bindings,
                                &aver_rt::AverList::from_vec(vec![inner]),
                            ))
                        }
                        _ => Err(AverStr::from("no match")),
                    }
                } else {
                    Err(AverStr::from("no match"))
                }
            }
        }
    }
}

/// Match a user-defined variant by integer tag first, then full constructor name for collision safety.
pub fn matchGenericCtorById(
    tag @ _: aver_rt::AverInt,
    ctorName @ _: AverStr,
    bindings @ _: &aver_rt::AverList<AverStr>,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValVariant(vTag, fullName, fields) => {
            if (vTag == tag) {
                if (fullName == ctorName) {
                    Ok(crate::aver_generated::domain::match_mod::zipBindings(
                        bindings, &fields,
                    ))
                } else {
                    Err(AverStr::from("no match"))
                }
            } else {
                Err(AverStr::from("no match"))
            }
        }
        _ => Err(AverStr::from("no match")),
    }
}

/// Match Option.None pattern.
pub fn matchNonePat(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v {
        crate::aver_generated::domain::value::Val::ValNone => Ok(aver_rt::AverList::empty()),
        _ => Err(AverStr::from("no match")),
    }
}

/// Match Ok/Err/Some wrapper and bind inner value.
#[inline(always)]
pub fn matchWrapperPat(
    v @ _: &crate::aver_generated::domain::value::Val,
    tag @ _: AverStr,
    bindings @ _: &aver_rt::AverList<AverStr>,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = tag;
        if &*__dispatch_subject == "Ok" {
            match v.clone() {
                crate::aver_generated::domain::value::Val::ValOk(inner) => {
                    let inner = (*inner).clone();
                    Ok(crate::aver_generated::domain::match_mod::zipBindings(
                        bindings,
                        &aver_rt::AverList::from_vec(vec![inner]),
                    ))
                }
                _ => Err(AverStr::from("no match")),
            }
        } else {
            if &*__dispatch_subject == "Err" {
                match v.clone() {
                    crate::aver_generated::domain::value::Val::ValErr(inner) => {
                        let inner = (*inner).clone();
                        Ok(crate::aver_generated::domain::match_mod::zipBindings(
                            bindings,
                            &aver_rt::AverList::from_vec(vec![inner]),
                        ))
                    }
                    _ => Err(AverStr::from("no match")),
                }
            } else {
                if &*__dispatch_subject == "Some" {
                    match v.clone() {
                        crate::aver_generated::domain::value::Val::ValSome(inner) => {
                            let inner = (*inner).clone();
                            Ok(crate::aver_generated::domain::match_mod::zipBindings(
                                bindings,
                                &aver_rt::AverList::from_vec(vec![inner]),
                            ))
                        }
                        _ => Err(AverStr::from("no match")),
                    }
                } else {
                    Err(AverStr::from("no match"))
                }
            }
        }
    }
}

/// Pair binding names with values.
#[inline(always)]
pub fn zipBindings(
    names @ _: &aver_rt::AverList<AverStr>,
    vals @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::match_mod::zipBindingsAcc(
        names.clone(),
        vals.clone(),
        aver_rt::AverList::empty(),
    )
}

/// Accumulate binding pairs in reverse, then reverse at end.
#[inline(always)]
pub fn zipBindingsAcc(
    mut names @ _: aver_rt::AverList<AverStr>,
    mut vals @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    loop {
        crate::cancel_checkpoint();
        let reversed @ _ = acc.reverse();
        aver_list_match!(names, [] => { return reversed; }, [n, ns] => { aver_list_match!(vals, [] => { return reversed; }, [v, vs] => { {
            let __tco0 = ns;
            let __tco1 = vs;
            let __tco2 = aver_rt::AverList::prepend((n, v), &acc);
            names = __tco0;
            vals = __tco1;
            acc = __tco2;
            continue;
        } }) })
    }
}
