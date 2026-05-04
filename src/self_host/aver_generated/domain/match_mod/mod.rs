#[allow(unused_imports)]
use crate::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    MatchPatTupleItemsAcc(aver_rt::AverList<Pattern>, aver_rt::AverList<Val>, aver_rt::AverList<(AverStr, Val)>),
    MatchPatTupleOne(Pattern, Val, aver_rt::AverList<Pattern>, aver_rt::AverList<Val>, aver_rt::AverList<(AverStr, Val)>),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::MatchPatTupleItemsAcc(mut pats, mut items, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(pats, [] => { return Ok(acc) }, [pat, restPats] => { aver_list_match!(items, [] => { return Err(AverStr::from("no match")) }, [item, restItems] => __MutualTco1::MatchPatTupleOne(pat, item, restPats, restItems, acc)) })
            }
            __MutualTco1::MatchPatTupleOne(mut pat, mut item, mut restPats, mut restItems, mut acc) => {
                crate::cancel_checkpoint();
                let bindings = matchPattern(&pat, &item)?;
                __MutualTco1::MatchPatTupleItemsAcc(restPats, restItems, aver_rt::AverList::concat(&acc, &bindings))
            }
        };
    }
}

/// Accumulate bindings from tuple pattern matching.
pub fn matchPatTupleItemsAcc(pats: &aver_rt::AverList<Pattern>, items: &aver_rt::AverList<Val>, acc: &aver_rt::AverList<(AverStr, Val)>) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::MatchPatTupleItemsAcc(pats.clone(), items.clone(), acc.clone()))
}

/// Match one tuple element and continue with accumulated bindings.
pub fn matchPatTupleOne(pat: &Pattern, item: &Val, restPats: &aver_rt::AverList<Pattern>, restItems: &aver_rt::AverList<Val>, acc: &aver_rt::AverList<(AverStr, Val)>) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::MatchPatTupleOne(pat.clone(), item.clone(), restPats.clone(), restItems.clone(), acc.clone()))
}

/// Try to match a value against a pattern. Returns bindings on success.
pub fn matchPattern(pat: &Pattern, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match pat.clone() {
        Pattern::PatWild => {
            Ok(aver_rt::AverList::empty())
        },
        Pattern::PatInt(n) => {
            matchPatInt(n, v)
        },
        Pattern::PatFloat(f) => {
            matchPatFloat(f, v)
        },
        Pattern::PatBool(b) => {
            matchPatBool(b, v)
        },
        Pattern::PatStr(s) => {
            matchPatStr(s, v)
        },
        Pattern::PatEmpty => {
            matchPatEmpty(v)
        },
        Pattern::PatCons(h, t) => {
            matchPatCons(h, t, v)
        },
        Pattern::PatConstructor(ctorName, bindings) => {
            matchPatConstructor(ctorName, &bindings, v)
        },
        Pattern::PatConstructorId(tag, ctorName, bindings) => {
            matchPatConstructorById(tag, ctorName, &bindings, v)
        },
        Pattern::PatTuple(pats) => {
            matchPatTuple(&pats, v)
        },
        Pattern::PatVar(name) => {
            Ok(aver_rt::AverList::from_vec(vec![(name, v.clone())]))
        }
    }
}

/// Match a PatInt pattern against a value.
pub fn matchPatInt(n: i64, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValInt(m) => {
            if (n == m) { Ok(aver_rt::AverList::empty()) } else { Err(AverStr::from("no match")) }
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    }
}

/// Match a PatFloat pattern against a value.
pub fn matchPatFloat(f: f64, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValFloat(vf) => {
            if (f == vf) { Ok(aver_rt::AverList::empty()) } else { Err(AverStr::from("no match")) }
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    }
}

/// Match a PatBool pattern against a value.
pub fn matchPatBool(b: bool, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValBool(vb) => {
            if (b == vb) { Ok(aver_rt::AverList::empty()) } else { Err(AverStr::from("no match")) }
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    }
}

/// Match a string literal pattern.
pub fn matchPatStr(s: AverStr, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValStr(vs) => {
            if (s == vs) { Ok(aver_rt::AverList::empty()) } else { Err(AverStr::from("no match")) }
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    }
}

/// Match empty list pattern [].
pub fn matchPatEmpty(v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValList(items) => {
            { let __list_subject = items; if __list_subject.is_empty() { Ok(aver_rt::AverList::empty()) } else { Err(AverStr::from("no match")) } }
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    }
}

/// Match cons pattern [h, ..t].
pub fn matchPatCons(h: AverStr, t: AverStr, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValList(items) => {
            aver_list_match!(items, [] => Err(AverStr::from("no match")), [head, tail] => Ok(aver_rt::AverList::from_vec(vec![(h, head), (t, Val::ValList(tail))])))
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    }
}

/// Match constructor pattern. Checks tag name and extracts inner values.
#[inline(always)]
pub fn matchPatConstructor(ctorName: AverStr, bindings: &aver_rt::AverList<AverStr>, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    { let __dispatch_subject = ctorName.clone(); if &*__dispatch_subject == "Result.Ok" { matchWrapperPat(v, AverStr::from("Ok"), bindings) } else { if &*__dispatch_subject == "Result.Err" { matchWrapperPat(v, AverStr::from("Err"), bindings) } else { if &*__dispatch_subject == "Option.Some" { matchWrapperPat(v, AverStr::from("Some"), bindings) } else { if &*__dispatch_subject == "Option.None" { matchNonePat(v) } else { matchGenericConstructor(ctorName, bindings, v) } } } } }
}

/// Match tuple pattern with nested patterns.
pub fn matchPatTuple(pats: &aver_rt::AverList<Pattern>, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValTuple(items) => {
            matchPatTupleItems(pats, &items)
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    }
}

/// Match each pattern against corresponding tuple item.
pub fn matchPatTupleItems(pats: &aver_rt::AverList<Pattern>, items: &aver_rt::AverList<Val>) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    matchPatTupleItemsAcc(pats, items, &aver_rt::AverList::empty())
}

/// Match a generic variant constructor pattern using the full name stored in the value.
pub fn matchGenericConstructor(ctorName: AverStr, bindings: &aver_rt::AverList<AverStr>, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValVariant(_, fullName, fields) => {
            matchGenericCtorCheck(ctorName, fullName, bindings, &fields)
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    }
}

/// Check if variant full name matches constructor name.
#[inline(always)]
pub fn matchGenericCtorCheck(ctorName: AverStr, fullName: AverStr, bindings: &aver_rt::AverList<AverStr>, fields: &aver_rt::AverList<Val>) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    if (fullName == ctorName) { Ok(zipBindings(bindings, fields)) } else { Err(AverStr::from("no match")) }
}

/// Match constructor pattern by integer tag ID. Builtins dispatch directly; user variants confirm both tag and full name.
#[inline(always)]
pub fn matchPatConstructorById(tag: i64, ctorName: AverStr, bindings: &aver_rt::AverList<AverStr>, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    { let __dispatch_subject = tag; if __dispatch_subject == 1i64 { matchWrapperPatDirect(v, 1i64, bindings) } else { if __dispatch_subject == 2i64 { matchWrapperPatDirect(v, 2i64, bindings) } else { if __dispatch_subject == 3i64 { matchWrapperPatDirect(v, 3i64, bindings) } else { if __dispatch_subject == 4i64 { matchNonePat(v) } else { matchGenericCtorById(tag, ctorName, bindings, v) } } } } }
}

/// Match Ok/Err/Some wrapper by integer tag. 1=Ok, 2=Err, 3=Some.
#[inline(always)]
pub fn matchWrapperPatDirect(v: &Val, tag: i64, bindings: &aver_rt::AverList<AverStr>) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    { let __dispatch_subject = tag; if __dispatch_subject == 1i64 { match v.clone() {
        Val::ValOk(inner) => {
            let inner = (*inner).clone();
            Ok(zipBindings(bindings, &aver_rt::AverList::from_vec(vec![inner])))
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    } } else { if __dispatch_subject == 2i64 { match v.clone() {
        Val::ValErr(inner) => {
            let inner = (*inner).clone();
            Ok(zipBindings(bindings, &aver_rt::AverList::from_vec(vec![inner])))
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    } } else { if __dispatch_subject == 3i64 { match v.clone() {
        Val::ValSome(inner) => {
            let inner = (*inner).clone();
            Ok(zipBindings(bindings, &aver_rt::AverList::from_vec(vec![inner])))
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    } } else { Err(AverStr::from("no match")) } } } }
}

/// Match a user-defined variant by integer tag first, then full constructor name for collision safety.
pub fn matchGenericCtorById(tag: i64, ctorName: AverStr, bindings: &aver_rt::AverList<AverStr>, v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValVariant(vTag, fullName, fields) => {
            if (vTag == tag) { if (fullName == ctorName) { Ok(zipBindings(bindings, &fields)) } else { Err(AverStr::from("no match")) } } else { Err(AverStr::from("no match")) }
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    }
}

/// Match Option.None pattern.
pub fn matchNonePat(v: &Val) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    match v {
        Val::ValNone => {
            Ok(aver_rt::AverList::empty())
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    }
}

/// Match Ok/Err/Some wrapper and bind inner value.
#[inline(always)]
pub fn matchWrapperPat(v: &Val, tag: AverStr, bindings: &aver_rt::AverList<AverStr>) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    { let __dispatch_subject = tag; if &*__dispatch_subject == "Ok" { match v.clone() {
        Val::ValOk(inner) => {
            let inner = (*inner).clone();
            Ok(zipBindings(bindings, &aver_rt::AverList::from_vec(vec![inner])))
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    } } else { if &*__dispatch_subject == "Err" { match v.clone() {
        Val::ValErr(inner) => {
            let inner = (*inner).clone();
            Ok(zipBindings(bindings, &aver_rt::AverList::from_vec(vec![inner])))
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    } } else { if &*__dispatch_subject == "Some" { match v.clone() {
        Val::ValSome(inner) => {
            let inner = (*inner).clone();
            Ok(zipBindings(bindings, &aver_rt::AverList::from_vec(vec![inner])))
        },
        _ => {
            Err(AverStr::from("no match"))
        }
    } } else { Err(AverStr::from("no match")) } } } }
}

/// Pair binding names with values.
#[inline(always)]
pub fn zipBindings(names: &aver_rt::AverList<AverStr>, vals: &aver_rt::AverList<Val>) -> aver_rt::AverList<(AverStr, Val)> {
    crate::cancel_checkpoint();
    zipBindingsAcc(names.clone(), vals.clone(), aver_rt::AverList::empty())
}

/// Accumulate binding pairs in reverse, then reverse at end.
#[inline(always)]
pub fn zipBindingsAcc(mut names: aver_rt::AverList<AverStr>, mut vals: aver_rt::AverList<Val>, mut acc: aver_rt::AverList<(AverStr, Val)>) -> aver_rt::AverList<(AverStr, Val)> {
    loop {
        crate::cancel_checkpoint();
        let reversed = acc.reverse();
        return aver_list_match!(names, [] => reversed, [n, ns] => { aver_list_match!(vals, [] => reversed, [v, vs] => { {
            let __tmp2 = aver_rt::AverList::prepend((n, v), &acc);
            names = ns;
            vals = vs;
            acc = __tmp2;
            continue;
        } }) });
    }
}
