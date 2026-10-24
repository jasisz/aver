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
                aver_list_match!(pats, [] => { return Ok(acc) }, [pat, restPats] => { aver_list_match!(items, [] => { return Err(::aver_rt::str_lit!("no match")) }, [item, restItems] => __MutualTco1::MatchPatTupleOne(pat, item, restPats, restItems, acc)) })
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
                    crate::aver_generated::domain::match_mod::matchPattern(&pat, item)?;
                let __arg6 @ _ = aver_rt::AverList::concat(&acc, &bindings);
                __MutualTco1::MatchPatTupleItemsAcc(restPats, restItems, __arg6)
            }
        };
    }
}

/// Accumulate bindings from tuple pattern matching.
pub fn matchPatTupleItemsAcc(
    mut pats @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    mut items @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(pats, [] => { return Ok(acc) }, [pat, restPats] => { aver_list_match!(items, [] => { return Err(::aver_rt::str_lit!("no match")) }, [item, restItems] => { return __mutual_tco_trampoline_1(__MutualTco1::MatchPatTupleOne(pat, item, restPats, restItems, acc)) }) })
}

/// Match one tuple element and continue with accumulated bindings.
pub fn matchPatTupleOne(
    mut pat @ _: crate::aver_generated::domain::ast::Pattern,
    mut item @ _: crate::aver_generated::domain::value::Val,
    mut restPats @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    mut restItems @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    let bindings @ _ = crate::aver_generated::domain::match_mod::matchPattern(&pat, item)?;
    let __arg6 @ _ = aver_rt::AverList::concat(&acc, &bindings);
    return __mutual_tco_trampoline_1(__MutualTco1::MatchPatTupleItemsAcc(
        restPats, restItems, __arg6,
    ));
}

/// Try to match a value against a pattern. Returns bindings on success.
pub fn matchPattern(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
    mut v @ _: crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    match pat {
        crate::aver_generated::domain::ast::Pattern::PatWild => Ok(aver_rt::AverList::empty()),
        crate::aver_generated::domain::ast::Pattern::PatInt(n) => {
            let n = n.clone();
            crate::aver_generated::domain::match_mod::matchPatInt(n, &v)
        }
        crate::aver_generated::domain::ast::Pattern::PatFloat(f) => {
            let f = f.clone();
            crate::aver_generated::domain::match_mod::matchPatFloat(f, &v)
        }
        crate::aver_generated::domain::ast::Pattern::PatBool(b) => {
            let b = b.clone();
            crate::aver_generated::domain::match_mod::matchPatBool(b, &v)
        }
        crate::aver_generated::domain::ast::Pattern::PatStr(s) => {
            let s = s.clone();
            crate::aver_generated::domain::match_mod::matchPatStr(&s, &v)
        }
        crate::aver_generated::domain::ast::Pattern::PatEmpty => {
            crate::aver_generated::domain::match_mod::matchPatEmpty(&v)
        }
        crate::aver_generated::domain::ast::Pattern::PatCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            crate::aver_generated::domain::match_mod::matchPatCons(h, t, &v)
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructor(ctorName, bindings) => {
            let ctorName = ctorName.clone();
            crate::aver_generated::domain::match_mod::matchPatConstructor(ctorName, bindings, &v)
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructorId(tag, ctorName, bindings) => {
            let tag = tag.clone();
            let ctorName = ctorName.clone();
            crate::aver_generated::domain::match_mod::matchPatConstructorById(
                tag, ctorName, bindings, &v,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatTuple(pats) => {
            crate::aver_generated::domain::match_mod::matchPatTuple(pats, &v)
        }
        crate::aver_generated::domain::ast::Pattern::PatVar(name) => {
            let name = name.clone();
            Ok(aver_rt::AverList::from_vec(vec![(name, v)]))
        }
    }
}

/// Match a PatInt pattern against a value.
pub fn matchPatInt(
    n @ _: aver_rt::AverInt,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValInt(__m_fields) => {
                let m = __m_fields.0.clone();
                if (n == m) {
                    Ok(aver_rt::AverList::empty())
                } else {
                    Err(::aver_rt::str_lit!("no match"))
                }
            }
            _ => Err(::aver_rt::str_lit!("no match")),
        }
    }
}

/// Match a PatFloat pattern against a value.
pub fn matchPatFloat(
    f @ _: f64,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValFloat(__vf_fields) => {
                let vf = __vf_fields.0.clone();
                if (f == vf) {
                    Ok(aver_rt::AverList::empty())
                } else {
                    Err(::aver_rt::str_lit!("no match"))
                }
            }
            _ => Err(::aver_rt::str_lit!("no match")),
        }
    }
}

/// Match a PatBool pattern against a value.
pub fn matchPatBool(
    b @ _: bool,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValBool(__vb_fields) => {
                let vb = __vb_fields.0.clone();
                if (b == vb) {
                    Ok(aver_rt::AverList::empty())
                } else {
                    Err(::aver_rt::str_lit!("no match"))
                }
            }
            _ => Err(::aver_rt::str_lit!("no match")),
        }
    }
}

/// Match a string literal pattern.
pub fn matchPatStr(
    s @ _: &AverStr,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValStr(__vs_fields) => {
                let vs = __vs_fields.0.clone();
                if (s == &(vs)) {
                    Ok(aver_rt::AverList::empty())
                } else {
                    Err(::aver_rt::str_lit!("no match"))
                }
            }
            _ => Err(::aver_rt::str_lit!("no match")),
        }
    }
}

/// Match empty list pattern [].
pub fn matchPatEmpty(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValList(__items_fields) => {
                let items = &__items_fields.0;
                {
                    let __list_subject = items;
                    if __list_subject.is_empty() {
                        Ok(aver_rt::AverList::empty())
                    } else {
                        Err(::aver_rt::str_lit!("no match"))
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("no match")),
        }
    }
}

/// Match cons pattern [h, ..t].
pub fn matchPatCons(
    h @ _: AverStr,
    t @ _: AverStr,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValList(__items_fields) => {
                let items = &__items_fields.0;
                {
                    let __list_subject = items;
                    if __list_subject.is_empty() {
                        Err(::aver_rt::str_lit!("no match"))
                    } else {
                        if let (Some(head), Some(tail)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            Ok(aver_rt::AverList::from_vec(vec![
                                (h, head.clone()),
                                (
                                    t,
                                    crate::aver_generated::domain::value::Val::ValList(
                                        crate::aver_generated::domain::value::Val__ValList(tail),
                                    ),
                                ),
                            ]))
                        } else {
                            panic!("Aver Rust codegen: non-exhaustive list match")
                        }
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("no match")),
        }
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
                ::aver_rt::str_lit!("Ok"),
                bindings,
            )
        } else {
            if &*__dispatch_subject == "Result.Err" {
                crate::aver_generated::domain::match_mod::matchWrapperPat(
                    v,
                    ::aver_rt::str_lit!("Err"),
                    bindings,
                )
            } else {
                if &*__dispatch_subject == "Option.Some" {
                    crate::aver_generated::domain::match_mod::matchWrapperPat(
                        v,
                        ::aver_rt::str_lit!("Some"),
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
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValTuple(__items_fields) => {
                let items = &__items_fields.0;
                crate::aver_generated::domain::match_mod::matchPatTupleItems(pats, items)
            }
            _ => Err(::aver_rt::str_lit!("no match")),
        }
    }
}

/// Match each pattern against corresponding tuple item.
pub fn matchPatTupleItems(
    pats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    let __arg2 @ _ =
        <aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>>::empty();
    crate::aver_generated::domain::match_mod::matchPatTupleItemsAcc(
        pats.clone(),
        items.clone(),
        __arg2,
    )
}

/// Match a generic variant constructor pattern using the full name stored in the value.
pub fn matchGenericConstructor(
    ctorName @ _: AverStr,
    bindings @ _: &aver_rt::AverList<AverStr>,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValVariant(__fullName_fields) => {
                let fullName = __fullName_fields.1.clone();
                let fields = &__fullName_fields.2;
                crate::aver_generated::domain::match_mod::matchGenericCtorCheck(
                    &ctorName, &fullName, bindings, fields,
                )
            }
            _ => Err(::aver_rt::str_lit!("no match")),
        }
    }
}

/// Check if variant full name matches constructor name.
#[inline(always)]
pub fn matchGenericCtorCheck(
    ctorName @ _: &AverStr,
    fullName @ _: &AverStr,
    bindings @ _: &aver_rt::AverList<AverStr>,
    fields @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    if (fullName == ctorName) {
        Ok(crate::aver_generated::domain::match_mod::zipBindings(
            bindings, fields,
        ))
    } else {
        Err(::aver_rt::str_lit!("no match"))
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
                            tag, &ctorName, bindings, v,
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
            {
                let __t = v;
                match __t.view() {
                    crate::aver_generated::domain::value::Val__view::ValOk(__inner_fields) => {
                        let inner = &__inner_fields.0;
                        Ok({
                            let __arg6 @ _ = aver_rt::AverList::from_vec(vec![inner.clone()]);
                            crate::aver_generated::domain::match_mod::zipBindings(bindings, &__arg6)
                        })
                    }
                    _ => Err(::aver_rt::str_lit!("no match")),
                }
            }
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(2) {
                {
                    let __t = v;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValErr(__inner_fields) => {
                            let inner = &__inner_fields.0;
                            Ok({
                                let __arg7 @ _ = aver_rt::AverList::from_vec(vec![inner.clone()]);
                                crate::aver_generated::domain::match_mod::zipBindings(
                                    bindings, &__arg7,
                                )
                            })
                        }
                        _ => Err(::aver_rt::str_lit!("no match")),
                    }
                }
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(3) {
                    {
                        let __t = v;
                        match __t.view() {
                            crate::aver_generated::domain::value::Val__view::ValSome(
                                __inner_fields,
                            ) => {
                                let inner = &__inner_fields.0;
                                Ok({
                                    let __arg8 @ _ =
                                        aver_rt::AverList::from_vec(vec![inner.clone()]);
                                    crate::aver_generated::domain::match_mod::zipBindings(
                                        bindings, &__arg8,
                                    )
                                })
                            }
                            _ => Err(::aver_rt::str_lit!("no match")),
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("no match"))
                }
            }
        }
    }
}

/// Match a user-defined variant by integer tag first, then full constructor name for collision safety.
pub fn matchGenericCtorById(
    tag @ _: aver_rt::AverInt,
    ctorName @ _: &AverStr,
    bindings @ _: &aver_rt::AverList<AverStr>,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValVariant(__vTag_fields) => {
                let vTag = __vTag_fields.0.clone();
                let fullName = __vTag_fields.1.clone();
                let fields = &__vTag_fields.2;
                if (vTag == tag) {
                    if (&(fullName) == ctorName) {
                        Ok(crate::aver_generated::domain::match_mod::zipBindings(
                            bindings, fields,
                        ))
                    } else {
                        Err(::aver_rt::str_lit!("no match"))
                    }
                } else {
                    Err(::aver_rt::str_lit!("no match"))
                }
            }
            _ => Err(::aver_rt::str_lit!("no match")),
        }
    }
}

/// Match Option.None pattern.
pub fn matchNonePat(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValNone => {
                Ok(aver_rt::AverList::empty())
            }
            _ => Err(::aver_rt::str_lit!("no match")),
        }
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
            {
                let __t = v;
                match __t.view() {
                    crate::aver_generated::domain::value::Val__view::ValOk(__inner_fields) => {
                        let inner = &__inner_fields.0;
                        Ok({
                            let __arg6 @ _ = aver_rt::AverList::from_vec(vec![inner.clone()]);
                            crate::aver_generated::domain::match_mod::zipBindings(bindings, &__arg6)
                        })
                    }
                    _ => Err(::aver_rt::str_lit!("no match")),
                }
            }
        } else {
            if &*__dispatch_subject == "Err" {
                {
                    let __t = v;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValErr(__inner_fields) => {
                            let inner = &__inner_fields.0;
                            Ok({
                                let __arg7 @ _ = aver_rt::AverList::from_vec(vec![inner.clone()]);
                                crate::aver_generated::domain::match_mod::zipBindings(
                                    bindings, &__arg7,
                                )
                            })
                        }
                        _ => Err(::aver_rt::str_lit!("no match")),
                    }
                }
            } else {
                if &*__dispatch_subject == "Some" {
                    {
                        let __t = v;
                        match __t.view() {
                            crate::aver_generated::domain::value::Val__view::ValSome(
                                __inner_fields,
                            ) => {
                                let inner = &__inner_fields.0;
                                Ok({
                                    let __arg8 @ _ =
                                        aver_rt::AverList::from_vec(vec![inner.clone()]);
                                    crate::aver_generated::domain::match_mod::zipBindings(
                                        bindings, &__arg8,
                                    )
                                })
                            }
                            _ => Err(::aver_rt::str_lit!("no match")),
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("no match"))
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
    let __arg2 @ _ =
        <aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>>::empty();
    crate::aver_generated::domain::match_mod::zipBindingsAcc(names, vals, __arg2)
}

/// Accumulate binding pairs in reverse, then reverse at end.
#[inline(always)]
pub fn zipBindingsAcc(
    names @ _: &aver_rt::AverList<AverStr>,
    vals @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut names @ _: &aver_rt::AverList<AverStr> = names;
    let mut __hold1: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut vals @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = vals;
    loop {
        crate::cancel_checkpoint();
        let reversed @ _ = acc.reverse();
        {
            let __list_subject = names;
            if __list_subject.is_empty() {
                return reversed;
            } else {
                if let Some((n, ns)) = aver_rt::list_uncons_cloned(__list_subject) {
                    {
                        let __list_subject = vals;
                        if __list_subject.is_empty() {
                            return reversed;
                        } else {
                            if let (Some(v), Some(vs)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                {
                                    let __tco0 = ns;
                                    let __tco1 = vs;
                                    let __tco2 =
                                        aver_rt::AverList::prepend_owned((n, v.clone()), acc);
                                    __hold0 = __tco0;
                                    names = &__hold0;
                                    __hold1 = __tco1;
                                    vals = &__hold1;
                                    acc = __tco2;
                                    continue;
                                }
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
}
