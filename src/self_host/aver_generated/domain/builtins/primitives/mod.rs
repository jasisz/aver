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
        if &*__dispatch_subject == "Int.toString" {
            builtinIntToString(args)
        } else {
            if &*__dispatch_subject == "Int.toFloat" {
                builtinIntToFloat(args)
            } else {
                if &*__dispatch_subject == "Int.fromString" {
                    builtinIntFromString(args)
                } else {
                    if &*__dispatch_subject == "Int.abs" {
                        builtinIntAbs(args)
                    } else {
                        if &*__dispatch_subject == "Int.mod" {
                            builtinIntMod(args)
                        } else {
                            if &*__dispatch_subject == "Int.max" {
                                builtinIntMax(args)
                            } else {
                                if &*__dispatch_subject == "Int.min" {
                                    builtinIntMin(args)
                                } else {
                                    Err(aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b =
                                                aver_rt::Buffer::with_capacity((37i64) as usize);
                                            __b.push_str(&AverStr::from("unknown int builtin: "));
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

/// Int.max(a, b) -> larger of a and b.
pub fn builtinIntMax(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        builtinIntMaxInner(&aV, &bV)
    }
}

/// Inner Int.max.
pub fn builtinIntMaxInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let a = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    if (a > b) {
        Ok(Val::ValInt(a))
    } else {
        Ok(Val::ValInt(b))
    }
}

/// Int.min(a, b) -> smaller of a and b.
pub fn builtinIntMin(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        builtinIntMinInner(&aV, &bV)
    }
}

/// Inner Int.min.
pub fn builtinIntMinInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let a = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    if (a < b) {
        Ok(Val::ValInt(a))
    } else {
        Ok(Val::ValInt(b))
    }
}

/// Dispatch String.* builtins.
#[inline(always)]
pub fn callString(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "String.len" {
            builtinStringLen(args)
        } else {
            if &*__dispatch_subject == "String.charAt" {
                builtinStringCharAt(args)
            } else {
                if &*__dispatch_subject == "String.chars" {
                    builtinStringChars(args)
                } else {
                    if &*__dispatch_subject == "String.join" {
                        builtinStringJoin(args)
                    } else {
                        if &*__dispatch_subject == "String.slice" {
                            builtinStringSlice(args)
                        } else {
                            if &*__dispatch_subject == "String.fromBool" {
                                builtinStringFromBool(args)
                            } else {
                                if &*__dispatch_subject == "String.fromInt" {
                                    builtinStringFromInt(args)
                                } else {
                                    if &*__dispatch_subject == "String.fromFloat" {
                                        builtinStringFromFloat(args)
                                    } else {
                                        if &*__dispatch_subject == "String.contains" {
                                            builtinStringContains(args)
                                        } else {
                                            if &*__dispatch_subject == "String.startsWith" {
                                                builtinStringStartsWith(args)
                                            } else {
                                                if &*__dispatch_subject == "String.toLower" {
                                                    builtinStringToLower(args)
                                                } else {
                                                    if &*__dispatch_subject == "String.toUpper" {
                                                        builtinStringToUpper(args)
                                                    } else {
                                                        if &*__dispatch_subject == "String.trim" {
                                                            builtinStringTrim(args)
                                                        } else {
                                                            if &*__dispatch_subject
                                                                == "String.endsWith"
                                                            {
                                                                builtinStringEndsWith(args)
                                                            } else {
                                                                if &*__dispatch_subject
                                                                    == "String.split"
                                                                {
                                                                    builtinStringSplit(args)
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "String.repeat"
                                                                    {
                                                                        builtinStringRepeat(args)
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "String.replace"
                                                                        {
                                                                            builtinStringReplaceAll(
                                                                                args,
                                                                            )
                                                                        } else {
                                                                            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((40i64) as usize); __b.push_str(&AverStr::from("unknown string builtin: ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name)))); __b }))
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
            builtinCharFromCode(args)
        } else {
            if &*__dispatch_subject == "Char.toCode" {
                builtinCharToCode(args)
            } else {
                Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity((38i64) as usize);
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

/// Int.toFloat(n) -> Float value.
pub fn builtinIntToFloat(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(Val::ValFloat((n as f64)))
}

/// Int.toString(n) -> string representation.
pub fn builtinIntToString(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(Val::ValStr((n.to_string()).into_aver()))
}

/// Int.abs(n) -> absolute value.
pub fn builtinIntAbs(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(Val::ValInt(n.abs()))
}

/// Int.mod(a, b) -> a mod b as Result.
pub fn builtinIntMod(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        builtinIntModInner(&aV, &bV)
    }
}

/// Inner impl of Int.mod.
pub fn builtinIntModInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let a = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    if (b == 0i64) {
        Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(
            AverStr::from("modulo by zero"),
        ))))
    } else {
        Ok(Val::ValOk(std::sync::Arc::new(Val::ValInt({
            let __b = b;
            if __b == 0i64 {
                0i64
            } else {
                (a).rem_euclid(__b)
            }
        }))))
    }
}

/// String.len(s) -> length as Int.
pub fn builtinStringLen(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(Val::ValInt((s.chars().count() as i64)))
}

/// String.charAt(s, index) -> single character string or error.
pub fn builtinStringCharAt(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, idxV) = pair;
        builtinStringCharAtInner(&sV, &idxV)
    }
}

/// Inner impl of String.charAt.
pub fn builtinStringCharAtInner(sV: &Val, idxV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let idx = crate::aver_generated::domain::builtins::helpers::expectInt(idxV)?;
    match (s.chars().nth(idx as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => Ok(Val::ValSome(std::sync::Arc::new(Val::ValStr(c)))),
        None => Ok(Val::ValNone.clone()),
    }
}

/// String.join(list, separator) -> joined string.
pub fn builtinStringJoin(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (lstV, sepV) = pair;
        builtinStringJoinInner(&lstV, &sepV)
    }
}

/// Inner impl of String.join.
pub fn builtinStringJoinInner(lstV: &Val, sepV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    let sep = crate::aver_generated::domain::builtins::helpers::expectStr(sepV)?;
    let strs = extractStrings(items, aver_rt::AverList::empty())?;
    Ok(Val::ValStr((aver_rt::string_join(&strs, &sep)).into_aver()))
}

/// Convert list of ValStr to list of String.
#[inline(always)]
pub fn extractStrings(
    mut items: aver_rt::AverList<Val>,
    mut acc: aver_rt::AverList<AverStr>,
) -> Result<aver_rt::AverList<AverStr>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(items, [] => Ok(acc.reverse()), [v, rest] => { match v {
            Val::ValStr(s) => {
            let __tmp1 = aver_rt::AverList::prepend(s, &acc);
            items = rest;
            acc = __tmp1;
            continue;
        },
            _ => Err(AverStr::from("String.join requires list of strings"))
        } });
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
                                    builtinStringSliceInner(&sV, &startV, &endV)
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
    Ok(Val::ValStr(
        (aver_rt::string_slice(&s, start, end)).into_aver(),
    ))
}

/// String.fromBool(b) -> string.
pub fn builtinStringFromBool(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValBool(b) => Ok(Val::ValStr((b.to_string()).into_aver())),
        _ => Err(AverStr::from("String.fromBool requires Bool")),
    }
}

/// String.fromInt(n) -> string (alias for Int.toString).
pub fn builtinStringFromInt(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(Val::ValStr((n.to_string()).into_aver()))
}

/// String.fromFloat(f) -> string.
pub fn builtinStringFromFloat(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValFloat(f) => Ok(Val::ValStr((f.to_string()).into_aver())),
        _ => Err(AverStr::from("String.fromFloat requires Float")),
    }
}

/// String.contains(haystack, needle) -> Bool.
pub fn builtinStringContains(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (hV, nV) = pair;
        builtinStringContainsInner(&hV, &nV)
    }
}

/// Inner impl of String.contains.
pub fn builtinStringContainsInner(hV: &Val, nV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let h = crate::aver_generated::domain::builtins::helpers::expectStr(hV)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectStr(nV)?;
    Ok(Val::ValBool(h.contains(&*n)))
}

/// String.startsWith(s, prefix) -> Bool.
pub fn builtinStringStartsWith(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, pV) = pair;
        builtinStringStartsWithInner(&sV, &pV)
    }
}

/// Inner impl of String.startsWith.
pub fn builtinStringStartsWithInner(sV: &Val, pV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let p = crate::aver_generated::domain::builtins::helpers::expectStr(pV)?;
    Ok(Val::ValBool(s.starts_with(&*p)))
}

/// String.toLower(s) -> lowercase string.
pub fn builtinStringToLower(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(Val::ValStr((s.to_lowercase()).into_aver()))
}

/// Char.fromCode(n) -> single character string.
pub fn builtinCharFromCode(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    match (char::from_u32(n as u32).map(|c| c.to_string())).into_aver() {
        Some(c) => Ok(Val::ValSome(std::sync::Arc::new(Val::ValStr(c)))),
        None => Ok(Val::ValNone.clone()),
    }
}

/// Char.toCode(c) -> Int code point.
pub fn builtinCharToCode(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(Val::ValInt(
        (s.chars().next().map(|c| c as i64).unwrap_or(0i64)),
    ))
}

/// Int.fromString(s) -> Result<Int, String>.
pub fn builtinIntFromString(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match (s.parse::<i64>().map_err(|e| e.to_string())).into_aver() {
        Ok(n) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValInt(n)))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// String.chars(s) -> list of single-character strings.
pub fn builtinStringChars(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(Val::ValList(stringToCharList(
        s.clone(),
        0i64,
        (s.chars().count() as i64),
        aver_rt::AverList::empty(),
    )))
}

/// Split string into list of ValStr chars.
#[inline(always)]
pub fn stringToCharList(
    mut s: AverStr,
    mut pos: i64,
    mut total: i64,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        let reversed = acc.reverse();
        return if (pos >= total) {
            reversed
        } else {
            match (s.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                Some(c) => {
                    let __tmp1 = (pos + 1i64);
                    let __tmp3 = aver_rt::AverList::prepend(Val::ValStr(c), &acc);
                    pos = __tmp1;
                    acc = __tmp3;
                    continue;
                }
                None => reversed,
            }
        };
    }
}

/// Dispatch Float.* builtins.
#[inline(always)]
pub fn callFloat(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Float.toString" {
            builtinFloatToString(args)
        } else {
            if &*__dispatch_subject == "Float.fromInt" {
                builtinFloatFromInt(args)
            } else {
                if &*__dispatch_subject == "Float.round" {
                    builtinFloatRound(args)
                } else {
                    if &*__dispatch_subject == "Float.fromString" {
                        builtinFloatFromString(args)
                    } else {
                        if &*__dispatch_subject == "Float.abs" {
                            builtinFloatAbs(args)
                        } else {
                            if &*__dispatch_subject == "Float.floor" {
                                builtinFloatFloor(args)
                            } else {
                                if &*__dispatch_subject == "Float.ceil" {
                                    builtinFloatCeil(args)
                                } else {
                                    if &*__dispatch_subject == "Float.min" {
                                        builtinFloatMin(args)
                                    } else {
                                        if &*__dispatch_subject == "Float.max" {
                                            builtinFloatMax(args)
                                        } else {
                                            if &*__dispatch_subject == "Float.sin" {
                                                builtinFloatSin(args)
                                            } else {
                                                if &*__dispatch_subject == "Float.cos" {
                                                    builtinFloatCos(args)
                                                } else {
                                                    if &*__dispatch_subject == "Float.sqrt" {
                                                        builtinFloatSqrt(args)
                                                    } else {
                                                        if &*__dispatch_subject == "Float.pow" {
                                                            builtinFloatPow(args)
                                                        } else {
                                                            if &*__dispatch_subject == "Float.atan2"
                                                            {
                                                                builtinFloatAtan2(args)
                                                            } else {
                                                                if &*__dispatch_subject
                                                                    == "Float.pi"
                                                                {
                                                                    builtinFloatPi(args)
                                                                } else {
                                                                    Err(aver_rt::AverStr::from({
                                                                        let mut __b = {
                                                                            let mut __b = aver_rt::Buffer::with_capacity((39i64) as usize);
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
    match (s.parse::<f64>().map_err(|e| e.to_string())).into_aver() {
        Ok(f) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValFloat(f)))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Float.fromInt(n) -> Float.
pub fn builtinFloatFromInt(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(Val::ValFloat((n as f64)))
}

/// Float.round(f) -> Int.
pub fn builtinFloatRound(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValFloat(f) => Ok(Val::ValInt(f.round() as i64)),
        _ => Err(AverStr::from("Float.round requires Float")),
    }
}

/// Float.toString(f) -> string representation.
pub fn builtinFloatToString(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValFloat(f) => Ok(Val::ValStr((f.to_string()).into_aver())),
        _ => Err(AverStr::from("Float.toString requires Float")),
    }
}

/// Float.abs(f) -> Float.
pub fn builtinFloatAbs(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValFloat(f) => Ok(Val::ValFloat(f.abs())),
        _ => Err(AverStr::from("Float.abs requires Float")),
    }
}

/// Float.floor(f) -> Int.
pub fn builtinFloatFloor(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValFloat(f) => Ok(Val::ValInt(f.floor() as i64)),
        _ => Err(AverStr::from("Float.floor requires Float")),
    }
}

/// Float.ceil(f) -> Int.
pub fn builtinFloatCeil(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValFloat(f) => Ok(Val::ValInt(f.ceil() as i64)),
        _ => Err(AverStr::from("Float.ceil requires Float")),
    }
}

/// Float.min(a, b) -> Float.
pub fn builtinFloatMin(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (Val::ValFloat(a), Val::ValFloat(b)) => Ok(Val::ValFloat(a.min(b))),
        _ => Err(AverStr::from("Float.min requires two Floats")),
    }
}

/// Float.max(a, b) -> Float.
pub fn builtinFloatMax(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (Val::ValFloat(a), Val::ValFloat(b)) => Ok(Val::ValFloat(a.max(b))),
        _ => Err(AverStr::from("Float.max requires two Floats")),
    }
}

/// Float.sin(f) -> Float.
pub fn builtinFloatSin(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValFloat(f) => Ok(Val::ValFloat(f.sin())),
        _ => Err(AverStr::from("Float.sin requires Float")),
    }
}

/// Float.cos(f) -> Float.
pub fn builtinFloatCos(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValFloat(f) => Ok(Val::ValFloat(f.cos())),
        _ => Err(AverStr::from("Float.cos requires Float")),
    }
}

/// Float.sqrt(f) -> Float.
pub fn builtinFloatSqrt(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValFloat(f) => Ok(Val::ValFloat(f.sqrt())),
        _ => Err(AverStr::from("Float.sqrt requires Float")),
    }
}

/// Float.pow(base, exp) -> Float.
pub fn builtinFloatPow(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (Val::ValFloat(a), Val::ValFloat(b)) => Ok(Val::ValFloat(a.powf(b))),
        _ => Err(AverStr::from("Float.pow requires two Floats")),
    }
}

/// Float.atan2(y, x) -> Float.
pub fn builtinFloatAtan2(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (Val::ValFloat(a), Val::ValFloat(b)) => Ok(Val::ValFloat(a.atan2(b))),
        _ => Err(AverStr::from("Float.atan2 requires two Floats")),
    }
}

/// Float.pi() -> Float.
pub fn builtinFloatPi(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if __list_subject.is_empty() {
            Ok(Val::ValFloat(std::f64::consts::PI))
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
    Ok(Val::ValStr((s.to_uppercase()).into_aver()))
}

/// String.trim(s) -> trimmed string.
pub fn builtinStringTrim(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(Val::ValStr((s.trim().to_string()).into_aver()))
}

/// String.endsWith(s, suffix) -> Bool.
pub fn builtinStringEndsWith(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, pV) = pair;
        builtinStringEndsWithInner(&sV, &pV)
    }
}

/// Inner String.endsWith.
pub fn builtinStringEndsWithInner(sV: &Val, pV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let p = crate::aver_generated::domain::builtins::helpers::expectStr(pV)?;
    Ok(Val::ValBool(s.ends_with(&*p)))
}

/// String.split(s, sep) -> List<String>.
pub fn builtinStringSplit(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, sepV) = pair;
        builtinStringSplitInner(&sV, &sepV)
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
    Ok(Val::ValList(strPartsToVals(
        parts,
        aver_rt::AverList::empty(),
    )))
}

/// Convert string list to ValStr list.
#[inline(always)]
pub fn strPartsToVals(
    mut parts: aver_rt::AverList<AverStr>,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(parts, [] => acc.reverse(), [s, rest] => { {
            let __tmp1 = aver_rt::AverList::prepend(Val::ValStr(s), &acc);
            parts = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// String.repeat(s, n) -> repeated string.
pub fn builtinStringRepeat(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, nV) = pair;
        builtinStringRepeatInner(&sV, &nV)
    }
}

/// Inner String.repeat.
pub fn builtinStringRepeatInner(sV: &Val, nV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let n = crate::aver_generated::domain::builtins::helpers::expectInt(nV)?;
    Ok(Val::ValStr(repeatStr(s, n, AverStr::from(""))))
}

/// Repeat string n times.
#[inline(always)]
pub fn repeatStr(mut s: AverStr, mut n: i64, mut acc: AverStr) -> AverStr {
    loop {
        crate::cancel_checkpoint();
        return if (n <= 0i64) {
            acc
        } else {
            {
                let __tmp0 = s.clone();
                let __tmp1 = (n - 1i64);
                let __tmp2 = (acc + &s);
                s = __tmp0;
                n = __tmp1;
                acc = __tmp2;
                continue;
            }
        };
    }
}

/// String.replace(s, from, to) -> String with all occurrences replaced.
#[inline(always)]
pub fn builtinStringReplaceAll(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    if ((args.len() as i64) == 3i64) {
        builtinStringReplaceAllExtract(args)
    } else {
        Err(AverStr::from("String.replace takes 3 arguments"))
    }
}

/// Extract args for String.replace.
#[inline(always)]
pub fn builtinStringReplaceAllExtract(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(args.clone(), [] => Err(AverStr::from("String.replace takes 3 arguments")), [sV, r1] => aver_list_match!(r1, [] => Err(AverStr::from("String.replace takes 3 arguments")), [fromV, r2] => aver_list_match!(r2, [] => Err(AverStr::from("String.replace takes 3 arguments")), [toV, r3] => builtinStringReplaceAllDo(&sV, &fromV, &toV))))
}

/// Inner impl of String.replace.
pub fn builtinStringReplaceAllDo(sV: &Val, fromV: &Val, toV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let from = crate::aver_generated::domain::builtins::helpers::expectStr(fromV)?;
    let to = crate::aver_generated::domain::builtins::helpers::expectStr(toV)?;
    Ok(Val::ValStr((s.replace(&*from, &*to)).into_aver()))
}
