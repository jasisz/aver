#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::helpers::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Dispatch Int.* builtins.
#[inline(always)]
pub fn callInt(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "String.fromInt" {
            crate::aver_generated::domain::builtins::primitives::builtinIntToString(args)
        } else {
            if &*__dispatch_subject == "Float.fromInt" {
                crate::aver_generated::domain::builtins::primitives::builtinIntToFloat(args)
            } else {
                if &*__dispatch_subject == "Int.fromString" {
                    crate::aver_generated::domain::builtins::primitives::builtinIntFromString(args)
                } else {
                    if &*__dispatch_subject == "Int.abs" {
                        crate::aver_generated::domain::builtins::primitives::builtinIntAbs(args)
                    } else {
                        if &*__dispatch_subject == "Int.mod" {
                            crate::aver_generated::domain::builtins::primitives::builtinIntMod(args)
                        } else {
                            if &*__dispatch_subject == "Int.div" {
                                crate::aver_generated::domain::builtins::primitives::builtinIntDiv(
                                    args,
                                )
                            } else {
                                if &*__dispatch_subject == "Int.max" {
                                    crate::aver_generated::domain::builtins::primitives::builtinIntMax(args)
                                } else {
                                    if &*__dispatch_subject == "Int.min" {
                                        crate::aver_generated::domain::builtins::primitives::builtinIntMin(args)
                                    } else {
                                        Err(aver_rt::AverStr::from({
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(37))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&AverStr::from(
                                                    "unknown int builtin: ",
                                                ));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(name)),
                                            ));
                                            __b
                                        }))
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

/// Int.max(a, b) -> larger of a and b.
pub fn builtinIntMax(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinIntMaxInner(&aV, &bV)
    }
}

/// Inner Int.max.
pub fn builtinIntMaxInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let a = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    if (a > b) {
        Ok(crate::aver_generated::domain::value::Val::ValInt(a))
    } else {
        Ok(crate::aver_generated::domain::value::Val::ValInt(b))
    }
}

/// Int.min(a, b) -> smaller of a and b.
pub fn builtinIntMin(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinIntMinInner(&aV, &bV)
    }
}

/// Inner Int.min.
pub fn builtinIntMinInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let a = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    if (a < b) {
        Ok(crate::aver_generated::domain::value::Val::ValInt(a))
    } else {
        Ok(crate::aver_generated::domain::value::Val::ValInt(b))
    }
}

/// Dispatch String.* builtins.
#[inline(always)]
pub fn callString(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "String.len" {
            crate::aver_generated::domain::builtins::primitives::builtinStringLen(args)
        } else {
            if &*__dispatch_subject == "String.charAt" {
                crate::aver_generated::domain::builtins::primitives::builtinStringCharAt(args)
            } else {
                if &*__dispatch_subject == "String.chars" {
                    crate::aver_generated::domain::builtins::primitives::builtinStringChars(args)
                } else {
                    if &*__dispatch_subject == "String.join" {
                        crate::aver_generated::domain::builtins::primitives::builtinStringJoin(args)
                    } else {
                        if &*__dispatch_subject == "String.slice" {
                            crate::aver_generated::domain::builtins::primitives::builtinStringSlice(
                                args,
                            )
                        } else {
                            if &*__dispatch_subject == "String.fromBool" {
                                crate::aver_generated::domain::builtins::primitives::builtinStringFromBool(args)
                            } else {
                                if &*__dispatch_subject == "String.fromInt" {
                                    crate::aver_generated::domain::builtins::primitives::builtinStringFromInt(args)
                                } else {
                                    if &*__dispatch_subject == "String.fromFloat" {
                                        crate::aver_generated::domain::builtins::primitives::builtinStringFromFloat(args)
                                    } else {
                                        if &*__dispatch_subject == "String.contains" {
                                            crate::aver_generated::domain::builtins::primitives::builtinStringContains(args)
                                        } else {
                                            if &*__dispatch_subject == "String.startsWith" {
                                                crate::aver_generated::domain::builtins::primitives::builtinStringStartsWith(args)
                                            } else {
                                                if &*__dispatch_subject == "String.toLower" {
                                                    crate::aver_generated::domain::builtins::primitives::builtinStringToLower(args)
                                                } else {
                                                    if &*__dispatch_subject == "String.toUpper" {
                                                        crate::aver_generated::domain::builtins::primitives::builtinStringToUpper(args)
                                                    } else {
                                                        if &*__dispatch_subject == "String.trim" {
                                                            crate::aver_generated::domain::builtins::primitives::builtinStringTrim(args)
                                                        } else {
                                                            if &*__dispatch_subject
                                                                == "String.endsWith"
                                                            {
                                                                crate::aver_generated::domain::builtins::primitives::builtinStringEndsWith(args)
                                                            } else {
                                                                if &*__dispatch_subject
                                                                    == "String.split"
                                                                {
                                                                    crate::aver_generated::domain::builtins::primitives::builtinStringSplit(args)
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "String.repeat"
                                                                    {
                                                                        crate::aver_generated::domain::builtins::primitives::builtinStringRepeat(args)
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "String.replace"
                                                                        {
                                                                            crate::aver_generated::domain::builtins::primitives::builtinStringReplaceAll(args)
                                                                        } else {
                                                                            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown string builtin: ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name)))); __b }))
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
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Dispatch Char.* builtins.
#[inline(always)]
pub fn callChar(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Char.fromCode" {
            crate::aver_generated::domain::builtins::primitives::builtinCharFromCode(args)
        } else {
            if &*__dispatch_subject == "Char.toCode" {
                crate::aver_generated::domain::builtins::primitives::builtinCharToCode(args)
            } else {
                Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(38)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("unknown char builtin: "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                    __b
                }))
            }
        }
    }
}

/// Float.fromInt(n) -> Float value.
pub fn builtinIntToFloat(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValFloat(
        n.to_f64(),
    ))
}

/// String.fromInt(n) -> string representation.
pub fn builtinIntToString(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        (n.to_string()).into_aver(),
    ))
}

/// Int.abs(n) -> absolute value.
pub fn builtinIntAbs(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValInt(n.abs()))
}

/// Int.mod(a, b) -> a mod b as Result.
pub fn builtinIntMod(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinIntModInner(&aV, &bV)
    }
}

/// Inner impl of Int.mod.
pub fn builtinIntModInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let a = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    if (b == aver_rt::AverInt::from_i64(0)) {
        Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(
                AverStr::from("modulo by zero"),
            )),
        ))
    } else {
        Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValInt({
                let __b = b;
                if __b.is_zero() {
                    aver_rt::AverInt::from_i64(0)
                } else {
                    (a).rem_euclid(&__b).unwrap()
                }
            })),
        ))
    }
}

/// Int.div(a, b) -> a div b as a Result value.
pub fn builtinIntDiv(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinIntDivInner(&aV, &bV)
    }
}

/// Inner impl of Int.div — Euclidean, Err on zero divisor or overflow.
pub fn builtinIntDivInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let a = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    match (match (a).div_euclid(&(b)) {
        Some(__q) => Ok(__q),
        None => Err("division by zero".to_string()),
    })
    .into_aver()
    {
        Ok(q) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValInt(q)),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// String.len(s) -> length as Int.
pub fn builtinStringLen(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValInt(
        aver_rt::AverInt::from_i64(s.chars().count() as i64),
    ))
}

/// String.charAt(s, index) -> single character string or error.
pub fn builtinStringCharAt(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, idxV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringCharAtInner(&sV, &idxV)
    }
}

/// Inner impl of String.charAt.
pub fn builtinStringCharAtInner(sV: &Val, idxV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let idx = crate::aver_generated::domain::builtins::helpers::expectInt(idxV)?;
    match ((idx)
        .to_usize()
        .and_then(|__i| s.chars().nth(__i).map(|c| c.to_string())))
    .into_aver()
    {
        Some(c) => Ok(crate::aver_generated::domain::value::Val::ValSome(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(c)),
        )),
        None => Ok(crate::aver_generated::domain::value::Val::ValNone),
    }
}

/// String.join(list, separator) -> joined string.
pub fn builtinStringJoin(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (lstV, sepV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringJoinInner(&lstV, &sepV)
    }
}

/// Inner impl of String.join.
pub fn builtinStringJoinInner(lstV: &Val, sepV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    let sep = crate::aver_generated::domain::builtins::helpers::expectStr(sepV)?;
    let strs = crate::aver_generated::domain::builtins::primitives::extractStrings(
        items,
        aver_rt::AverList::empty(),
    )?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        (aver_rt::string_join(&strs, &sep)).into_aver(),
    ))
}

/// Convert list of ValStr to list of String.
#[inline(always)]
pub fn extractStrings(
    mut items: aver_rt::AverList<Val>,
    mut acc: aver_rt::AverList<AverStr>,
) -> Result<aver_rt::AverList<AverStr>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(items, [] => { return Ok(acc.reverse()); }, [v, rest] => { match v {
        crate::aver_generated::domain::value::Val::ValStr(s) => {
            {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(s, &acc);
            items = __tco0;
            acc = __tco1;
            continue;
        }
        },
        _ => {
            return Err(AverStr::from("String.join requires list of strings"));
        }
    } })
    }
}

/// String.slice(s, start, end) -> substring.
pub fn builtinStringSlice(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((sV, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((startV, rest2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = rest2;
                        if let Some((endV, rest3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = rest3;
                                if __list_subject.is_empty() {
                                    crate::aver_generated::domain::builtins::primitives::builtinStringSliceInner(&sV, &startV, &endV)
                                } else {
                                    Err(AverStr::from("String.slice takes 3 arguments"))
                                }
                            }
                        } else {
                            Err(AverStr::from("String.slice takes 3 arguments"))
                        }
                    }
                } else {
                    Err(AverStr::from("String.slice takes 3 arguments"))
                }
            }
        } else {
            Err(AverStr::from("String.slice takes 3 arguments"))
        }
    }
}

/// Inner impl of String.slice.
pub fn builtinStringSliceInner(sV: &Val, startV: &Val, endV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let start = crate::aver_generated::domain::builtins::helpers::expectInt(startV)?;
    let end = crate::aver_generated::domain::builtins::helpers::expectInt(endV)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        (aver_rt::string_slice(
            &s,
            crate::aver_int_clamp_i64(&start),
            crate::aver_int_clamp_i64(&end),
        ))
        .into_aver(),
    ))
}

/// String.fromBool(b) -> string.
pub fn builtinStringFromBool(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValBool(b) => Ok(
            crate::aver_generated::domain::value::Val::ValStr((b.to_string()).into_aver()),
        ),
        _ => Err(AverStr::from("String.fromBool requires Bool")),
    }
}

/// String.fromInt(n) -> string (alias for String.fromInt).
pub fn builtinStringFromInt(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        (n.to_string()).into_aver(),
    ))
}

/// String.fromFloat(f) -> string.
pub fn builtinStringFromFloat(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValFloat(f) => Ok(
            crate::aver_generated::domain::value::Val::ValStr((f.to_string()).into_aver()),
        ),
        _ => Err(AverStr::from("String.fromFloat requires Float")),
    }
}

/// String.contains(haystack, needle) -> Bool.
pub fn builtinStringContains(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (hV, nV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringContainsInner(&hV, &nV)
    }
}

/// Inner impl of String.contains.
pub fn builtinStringContainsInner(hV: &Val, nV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let h = crate::aver_generated::domain::builtins::helpers::expectStr(hV)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectStr(nV)?;
    Ok(crate::aver_generated::domain::value::Val::ValBool(
        h.contains(&*n),
    ))
}

/// String.startsWith(s, prefix) -> Bool.
pub fn builtinStringStartsWith(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, pV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringStartsWithInner(&sV, &pV)
    }
}

/// Inner impl of String.startsWith.
pub fn builtinStringStartsWithInner(sV: &Val, pV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let p = crate::aver_generated::domain::builtins::helpers::expectStr(pV)?;
    Ok(crate::aver_generated::domain::value::Val::ValBool(
        s.starts_with(&*p),
    ))
}

/// String.toLower(s) -> lowercase string.
pub fn builtinStringToLower(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        (s.to_lowercase()).into_aver(),
    ))
}

/// Char.fromCode(n) -> single character string.
pub fn builtinCharFromCode(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    match ((n).to_u32().and_then(char::from_u32).map(|c| c.to_string())).into_aver() {
        Some(c) => Ok(crate::aver_generated::domain::value::Val::ValSome(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(c)),
        )),
        None => Ok(crate::aver_generated::domain::value::Val::ValNone),
    }
}

/// Char.toCode(c) -> Int code point.
pub fn builtinCharToCode(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValInt(
        aver_rt::AverInt::from_i64(s.chars().next().map(|c| c as i64).unwrap_or(0)),
    ))
}

/// Int.fromString(s) -> Result<Int, String>.
pub fn builtinIntFromString(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match ({
        let __s = &(s);
        __s.parse::<aver_rt::AverInt>()
            .map_err(|_| format!("Cannot parse '{}' as Int", __s))
    })
    .into_aver()
    {
        Ok(n) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValInt(n)),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// String.chars(s) -> list of single-character strings.
pub fn builtinStringChars(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::builtins::primitives::stringToCharList(
            s.clone(),
            aver_rt::AverInt::from_i64(0),
            aver_rt::AverInt::from_i64(s.chars().count() as i64),
            aver_rt::AverList::empty(),
        ),
    ))
}

/// Split string into list of ValStr chars.
#[inline(always)]
pub fn stringToCharList(
    mut s: AverStr,
    mut pos: aver_rt::AverInt,
    mut total: aver_rt::AverInt,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        let reversed = acc.reverse();
        if (pos < total) {
            match ((pos)
                .to_usize()
                .and_then(|__i| s.chars().nth(__i).map(|c| c.to_string())))
            .into_aver()
            {
                Some(c) => {
                    let __tco1 = pos.add(&aver_rt::AverInt::from_i64(1));
                    let __tco3 = aver_rt::AverList::prepend(
                        crate::aver_generated::domain::value::Val::ValStr(c),
                        &acc,
                    );
                    pos = __tco1;
                    acc = __tco3;
                    continue;
                }
                None => {
                    return reversed;
                }
            }
        } else {
            return reversed;
        }
    }
}

/// Dispatch Float.* builtins.
#[inline(always)]
pub fn callFloat(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "String.fromFloat" {
            crate::aver_generated::domain::builtins::primitives::builtinFloatToString(args)
        } else {
            if &*__dispatch_subject == "Float.fromInt" {
                crate::aver_generated::domain::builtins::primitives::builtinFloatFromInt(args)
            } else {
                if &*__dispatch_subject == "Float.round" {
                    crate::aver_generated::domain::builtins::primitives::builtinFloatRound(args)
                } else {
                    if &*__dispatch_subject == "Float.fromString" {
                        crate::aver_generated::domain::builtins::primitives::builtinFloatFromString(
                            args,
                        )
                    } else {
                        if &*__dispatch_subject == "Float.abs" {
                            crate::aver_generated::domain::builtins::primitives::builtinFloatAbs(
                                args,
                            )
                        } else {
                            if &*__dispatch_subject == "Float.floor" {
                                crate::aver_generated::domain::builtins::primitives::builtinFloatFloor(args)
                            } else {
                                if &*__dispatch_subject == "Float.ceil" {
                                    crate::aver_generated::domain::builtins::primitives::builtinFloatCeil(args)
                                } else {
                                    if &*__dispatch_subject == "Float.min" {
                                        crate::aver_generated::domain::builtins::primitives::builtinFloatMin(args)
                                    } else {
                                        if &*__dispatch_subject == "Float.max" {
                                            crate::aver_generated::domain::builtins::primitives::builtinFloatMax(args)
                                        } else {
                                            if &*__dispatch_subject == "Float.sin" {
                                                crate::aver_generated::domain::builtins::primitives::builtinFloatSin(args)
                                            } else {
                                                if &*__dispatch_subject == "Float.cos" {
                                                    crate::aver_generated::domain::builtins::primitives::builtinFloatCos(args)
                                                } else {
                                                    if &*__dispatch_subject == "Float.sqrt" {
                                                        crate::aver_generated::domain::builtins::primitives::builtinFloatSqrt(args)
                                                    } else {
                                                        if &*__dispatch_subject == "Float.pow" {
                                                            crate::aver_generated::domain::builtins::primitives::builtinFloatPow(args)
                                                        } else {
                                                            if &*__dispatch_subject == "Float.atan2"
                                                            {
                                                                crate::aver_generated::domain::builtins::primitives::builtinFloatAtan2(args)
                                                            } else {
                                                                if &*__dispatch_subject
                                                                    == "Float.pi"
                                                                {
                                                                    crate::aver_generated::domain::builtins::primitives::builtinFloatPi(args)
                                                                } else {
                                                                    Err(aver_rt::AverStr::from({
                                                                        let mut __b = {
                                                                            let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0));
                                                                            __b.push_str(&AverStr::from("unknown float builtin: "));
                                                                            __b
                                                                        };
                                                                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                                                                        __b
                                                                    }))
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
                    }
                }
            }
        }
    }
}

/// Float.fromString(s) -> Result<Float, String>.
pub fn builtinFloatFromString(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match ({
        let __s = &(s);
        __s.parse::<f64>()
            .map_err(|_| format!("Cannot parse '{}' as Float", __s))
    })
    .into_aver()
    {
        Ok(f) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValFloat(f)),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Float.fromInt(n) -> Float.
pub fn builtinFloatFromInt(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValFloat(
        n.to_f64(),
    ))
}

/// Float.round(f) -> Int.
pub fn builtinFloatRound(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValFloat(f) => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                aver_rt::AverInt::from_f64_trunc(f.round()),
            ))
        }
        _ => Err(AverStr::from("Float.round requires Float")),
    }
}

/// String.fromFloat(f) -> string representation.
pub fn builtinFloatToString(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValFloat(f) => Ok(
            crate::aver_generated::domain::value::Val::ValStr((f.to_string()).into_aver()),
        ),
        _ => Err(AverStr::from("String.fromFloat requires Float")),
    }
}

/// Float.abs(f) -> Float.
pub fn builtinFloatAbs(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValFloat(f) => {
            Ok(crate::aver_generated::domain::value::Val::ValFloat(f.abs()))
        }
        _ => Err(AverStr::from("Float.abs requires Float")),
    }
}

/// Float.floor(f) -> Int.
pub fn builtinFloatFloor(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValFloat(f) => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                aver_rt::AverInt::from_f64_trunc(f.floor()),
            ))
        }
        _ => Err(AverStr::from("Float.floor requires Float")),
    }
}

/// Float.ceil(f) -> Int.
pub fn builtinFloatCeil(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValFloat(f) => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                aver_rt::AverInt::from_f64_trunc(f.ceil()),
            ))
        }
        _ => Err(AverStr::from("Float.ceil requires Float")),
    }
}

/// Float.min(a, b) -> Float.
pub fn builtinFloatMin(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (
            crate::aver_generated::domain::value::Val::ValFloat(a),
            crate::aver_generated::domain::value::Val::ValFloat(b),
        ) => Ok(crate::aver_generated::domain::value::Val::ValFloat(
            a.min(b),
        )),
        _ => Err(AverStr::from("Float.min requires two Floats")),
    }
}

/// Float.max(a, b) -> Float.
pub fn builtinFloatMax(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (
            crate::aver_generated::domain::value::Val::ValFloat(a),
            crate::aver_generated::domain::value::Val::ValFloat(b),
        ) => Ok(crate::aver_generated::domain::value::Val::ValFloat(
            a.max(b),
        )),
        _ => Err(AverStr::from("Float.max requires two Floats")),
    }
}

/// Float.sin(f) -> Float.
pub fn builtinFloatSin(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValFloat(f) => {
            Ok(crate::aver_generated::domain::value::Val::ValFloat(f.sin()))
        }
        _ => Err(AverStr::from("Float.sin requires Float")),
    }
}

/// Float.cos(f) -> Float.
pub fn builtinFloatCos(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValFloat(f) => {
            Ok(crate::aver_generated::domain::value::Val::ValFloat(f.cos()))
        }
        _ => Err(AverStr::from("Float.cos requires Float")),
    }
}

/// Float.sqrt(f) -> Float.
pub fn builtinFloatSqrt(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValFloat(f) => Ok(
            crate::aver_generated::domain::value::Val::ValFloat(f.sqrt()),
        ),
        _ => Err(AverStr::from("Float.sqrt requires Float")),
    }
}

/// Float.pow(base, exp) -> Float.
pub fn builtinFloatPow(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (
            crate::aver_generated::domain::value::Val::ValFloat(a),
            crate::aver_generated::domain::value::Val::ValFloat(b),
        ) => Ok(crate::aver_generated::domain::value::Val::ValFloat(
            a.powf(b),
        )),
        _ => Err(AverStr::from("Float.pow requires two Floats")),
    }
}

/// Float.atan2(y, x) -> Float.
pub fn builtinFloatAtan2(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (
            crate::aver_generated::domain::value::Val::ValFloat(a),
            crate::aver_generated::domain::value::Val::ValFloat(b),
        ) => Ok(crate::aver_generated::domain::value::Val::ValFloat(
            a.atan2(b),
        )),
        _ => Err(AverStr::from("Float.atan2 requires two Floats")),
    }
}

/// Float.pi() -> Float.
pub fn builtinFloatPi(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if __list_subject.is_empty() {
            Ok(crate::aver_generated::domain::value::Val::ValFloat(
                std::f64::consts::PI,
            ))
        } else {
            Err(AverStr::from("Float.pi takes 0 arguments"))
        }
    }
}

/// String.toUpper(s) -> uppercase string.
pub fn builtinStringToUpper(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        (s.to_uppercase()).into_aver(),
    ))
}

/// String.trim(s) -> trimmed string.
pub fn builtinStringTrim(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        (s.trim().to_string()).into_aver(),
    ))
}

/// String.endsWith(s, suffix) -> Bool.
pub fn builtinStringEndsWith(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, pV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringEndsWithInner(&sV, &pV)
    }
}

/// Inner String.endsWith.
pub fn builtinStringEndsWithInner(sV: &Val, pV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let p = crate::aver_generated::domain::builtins::helpers::expectStr(pV)?;
    Ok(crate::aver_generated::domain::value::Val::ValBool(
        s.ends_with(&*p),
    ))
}

/// String.split(s, sep) -> List<String>.
pub fn builtinStringSplit(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, sepV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringSplitInner(&sV, &sepV)
    }
}

/// Inner String.split.
pub fn builtinStringSplitInner(sV: &Val, sepV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let sep = crate::aver_generated::domain::builtins::helpers::expectStr(sepV)?;
    let parts =
        (aver_rt::AverList::from_vec(s.split(&*sep).map(|s| s.to_string()).collect::<Vec<_>>()))
            .into_aver();
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::builtins::primitives::strPartsToVals(
            parts,
            aver_rt::AverList::empty(),
        ),
    ))
}

/// Convert string list to ValStr list.
#[inline(always)]
pub fn strPartsToVals(
    mut parts: aver_rt::AverList<AverStr>,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(parts, [] => { return acc.reverse(); }, [s, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::value::Val::ValStr(s), &acc);
            parts = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// String.repeat(s, n) -> repeated string.
pub fn builtinStringRepeat(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, nV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringRepeatInner(&sV, &nV)
    }
}

/// Inner String.repeat.
pub fn builtinStringRepeatInner(sV: &Val, nV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(nV)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::builtins::primitives::repeatStr(s, n, AverStr::from("")),
    ))
}

/// Repeat string n times.
#[inline(always)]
pub fn repeatStr(mut s: AverStr, mut n: aver_rt::AverInt, mut acc: AverStr) -> AverStr {
    loop {
        crate::cancel_checkpoint();
        if (n > aver_rt::AverInt::from_i64(0)) {
            {
                let __tco0 = s.clone();
                let __tco1 = n.sub(&aver_rt::AverInt::from_i64(1));
                let __tco2 = (acc + &s);
                s = __tco0;
                n = __tco1;
                acc = __tco2;
                continue;
            }
        } else {
            return acc;
        }
    }
}

/// String.replace(s, from, to) -> String with all occurrences replaced.
#[inline(always)]
pub fn builtinStringReplaceAll(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    if (aver_rt::AverInt::from_i64(args.len() as i64) == aver_rt::AverInt::from_i64(3)) {
        crate::aver_generated::domain::builtins::primitives::builtinStringReplaceAllExtract(args)
    } else {
        Err(AverStr::from("String.replace takes 3 arguments"))
    }
}

/// Extract args for String.replace.
#[inline(always)]
pub fn builtinStringReplaceAllExtract(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(args.clone(), [] => Err(AverStr::from("String.replace takes 3 arguments")), [sV, r1] => aver_list_match!(r1, [] => Err(AverStr::from("String.replace takes 3 arguments")), [fromV, r2] => aver_list_match!(r2, [] => Err(AverStr::from("String.replace takes 3 arguments")), [toV, r3] => crate::aver_generated::domain::builtins::primitives::builtinStringReplaceAllDo(&sV, &fromV, &toV))))
}

/// Inner impl of String.replace.
pub fn builtinStringReplaceAllDo(sV: &Val, fromV: &Val, toV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let from = crate::aver_generated::domain::builtins::helpers::expectStr(fromV)?;
    let to = crate::aver_generated::domain::builtins::helpers::expectStr(toV)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        (s.replace(&*from, &*to)).into_aver(),
    ))
}
