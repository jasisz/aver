#[allow(unused_imports)]
use crate::*;

/// Dispatch Int.* builtins.
#[inline(always)]
pub fn callInt(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
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
                                        if &*__dispatch_subject == "Int.toBigEndian" {
                                            crate::aver_generated::domain::builtins::endian::call(
                                                name, args,
                                            )
                                        } else {
                                            if &*__dispatch_subject == "Int.toLittleEndian" {
                                                crate::aver_generated::domain::builtins::endian::call(name, args)
                                            } else {
                                                if &*__dispatch_subject == "Int.fromBigEndian" {
                                                    crate::aver_generated::domain::builtins::endian::call(name, args)
                                                } else {
                                                    if &*__dispatch_subject
                                                        == "Int.fromLittleEndian"
                                                    {
                                                        crate::aver_generated::domain::builtins::endian::call(name, args)
                                                    } else {
                                                        Err(aver_rt::AverStr::from({
                                                            let mut __b = {
                                                                let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(37)).to_usize().unwrap_or(0));
                                                                __b.push_str(&::aver_rt::str_lit!(
                                                                    "unknown int builtin: "
                                                                ));
                                                                __b
                                                            };
                                                            __b.push_display(&(name));
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

/// Int.max(a, b) -> larger of a and b.
pub fn builtinIntMax(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinIntMaxInner(&aV, &bV)
    }
}

/// Inner Int.max.
pub fn builtinIntMaxInner(
    aV @ _: &crate::aver_generated::domain::value::Val,
    bV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let a @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    if (a > b) {
        Ok(crate::aver_generated::domain::value::Val::ValInt(
            crate::aver_generated::domain::value::Val__ValInt(a),
        ))
    } else {
        Ok(crate::aver_generated::domain::value::Val::ValInt(
            crate::aver_generated::domain::value::Val__ValInt(b),
        ))
    }
}

/// Int.min(a, b) -> smaller of a and b.
pub fn builtinIntMin(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinIntMinInner(&aV, &bV)
    }
}

/// Inner Int.min.
pub fn builtinIntMinInner(
    aV @ _: &crate::aver_generated::domain::value::Val,
    bV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let a @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    if (a < b) {
        Ok(crate::aver_generated::domain::value::Val::ValInt(
            crate::aver_generated::domain::value::Val__ValInt(a),
        ))
    } else {
        Ok(crate::aver_generated::domain::value::Val::ValInt(
            crate::aver_generated::domain::value::Val__ValInt(b),
        ))
    }
}

/// Dispatch String.* builtins.
#[inline(always)]
pub fn callString(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
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
                                                                            if &*__dispatch_subject
                                                                                == "String.firstCodePoint"
                                                                            {
                                                                                crate::aver_generated::domain::builtins::primitives::builtinStringFirstCodePoint(args)
                                                                            } else {
                                                                                if &*__dispatch_subject == "String.fromCodePoint" { crate::aver_generated::domain::builtins::primitives::builtinStringFromCodePoint(args) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown string builtin: ")); __b }; __b.push_display(&(name)); __b })) }
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
}

/// Float.fromInt(n) -> Float value.
pub fn builtinIntToFloat(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValFloat(
        crate::aver_generated::domain::value::Val__ValFloat(n.to_f64()),
    ))
}

/// String.fromInt(n) -> string representation.
pub fn builtinIntToString(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::value::Val__ValStr(aver_rt::display_str(&(n))),
    ))
}

/// Int.abs(n) -> absolute value.
pub fn builtinIntAbs(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValInt(
        crate::aver_generated::domain::value::Val__ValInt(n.abs()),
    ))
}

/// Int.mod(a, b) -> a mod b as Result.
pub fn builtinIntMod(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinIntModInner(&aV, &bV)
    }
}

/// Inner impl of Int.mod.
pub fn builtinIntModInner(
    aV @ _: &crate::aver_generated::domain::value::Val,
    bV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let a @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    if (b == aver_rt::AverInt::from_i64(0)) {
        Ok(crate::aver_generated::domain::value::Val::ValErr(
            crate::aver_generated::domain::value::Val__ValErr(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(::aver_rt::str_lit!(
                        "modulo by zero"
                    )),
                ),
            ),
        ))
    } else {
        Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt({
                        let __b = b;
                        if __b.is_zero() {
                            aver_rt::AverInt::from_i64(0)
                        } else {
                            (a).rem_euclid(&__b).unwrap()
                        }
                    }),
                ),
            ),
        ))
    }
}

/// Int.div(a, b) -> a div b as a Result value.
pub fn builtinIntDiv(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinIntDivInner(&aV, &bV)
    }
}

/// Inner impl of Int.div — Euclidean, Err on zero divisor or overflow.
pub fn builtinIntDivInner(
    aV @ _: &crate::aver_generated::domain::value::Val,
    bV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let a @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(aV)?;
    let b @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(bV)?;
    match (match (a).div_euclid(&(b)) {
        Some(__q) => Ok(__q),
        None => Err("division by zero".to_string()),
    })
    .into_aver()
    {
        Ok(q @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(q),
                ),
            ),
        )),
        Err(e @ _) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            crate::aver_generated::domain::value::Val__ValErr(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(e),
                ),
            ),
        )),
    }
}

/// String.len(s) -> length as Int.
pub fn builtinStringLen(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValInt(
        crate::aver_generated::domain::value::Val__ValInt(aver_rt::AverInt::from_i64(
            s.chars().count() as i64,
        )),
    ))
}

/// String.charAt(s, index) -> single character string or error.
pub fn builtinStringCharAt(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, idxV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringCharAtInner(&sV, &idxV)
    }
}

/// Inner impl of String.charAt.
pub fn builtinStringCharAtInner(
    sV @ _: &crate::aver_generated::domain::value::Val,
    idxV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let idx @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(idxV)?;
    match (idx)
        .to_usize()
        .and_then(|__i| s.chars().nth(__i))
        .map(aver_rt::char_str)
    {
        Some(c @ _) => Ok(crate::aver_generated::domain::value::Val::ValSome(
            crate::aver_generated::domain::value::Val__ValSome(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(c),
                ),
            ),
        )),
        None => Ok(crate::aver_generated::domain::value::Val::ValNone),
    }
}

/// String.join(list, separator) -> joined string.
pub fn builtinStringJoin(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (lstV, sepV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringJoinInner(&lstV, &sepV)
    }
}

/// Inner impl of String.join.
pub fn builtinStringJoinInner(
    lstV @ _: &crate::aver_generated::domain::value::Val,
    sepV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items @ _ = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    let sep @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(sepV)?;
    let strs @ _ = {
        let __arg5 @ _ =
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
        crate::aver_generated::domain::builtins::primitives::extractStrings__collected(
            &items, __arg5,
        )
    }?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::value::Val__ValStr(aver_rt::string_join(&strs, &sep)),
    ))
}

/// Convert list of ValStr to list of String.
#[inline(always)]
pub fn extractStrings(
    mut items @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<AverStr>,
) -> Result<aver_rt::AverList<AverStr>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(items, [] => { return Ok((acc).reverse_owned()); }, [v, rest] => { { let __t = v; match __t.view() {
        crate::aver_generated::domain::value::Val__view::ValStr(__s_fields) => {
            let (s,) = if __t.owns_node() { let __f = crate::aver_generated::domain::value::Val::take::<crate::aver_generated::domain::value::Val__ValStr>(__t); (__f.0,) } else { let __c = (__s_fields.0.clone(),); drop(__t); __c };
            {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend_owned(s, acc);
            items = __tco0;
            acc = __tco1;
            continue;
        }
        },
        _ => {
            return Err(::aver_rt::str_lit!("String.join requires list of strings"));
        }
    } } })
    }
}

/// String.slice(s, start, end) -> substring.
pub fn builtinStringSlice(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(sV), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(startV), Some(rest2)) = (__list_subject.first(), __list_subject.tail())
                {
                    {
                        let __list_subject = rest2;
                        if let (Some(endV), Some(rest3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = rest3;
                                if __list_subject.is_empty() {
                                    crate::aver_generated::domain::builtins::primitives::builtinStringSliceInner(sV, startV, endV)
                                } else {
                                    Err(::aver_rt::str_lit!("String.slice takes 3 arguments"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("String.slice takes 3 arguments"))
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("String.slice takes 3 arguments"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("String.slice takes 3 arguments"))
        }
    }
}

/// Inner impl of String.slice.
pub fn builtinStringSliceInner(
    sV @ _: &crate::aver_generated::domain::value::Val,
    startV @ _: &crate::aver_generated::domain::value::Val,
    endV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let start @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(startV)?;
    let end @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(endV)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::value::Val__ValStr(aver_rt::string_slice_part(
            &s,
            crate::aver_int_clamp_i64(&start),
            crate::aver_int_clamp_i64(&end),
        )),
    ))
}

/// String.fromBool(b) -> string.
pub fn builtinStringFromBool(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValBool(__b_fields) => {
                let (b,) = if __t.owns_node() {
                    let __f = crate::aver_generated::domain::value::Val::take::<
                        crate::aver_generated::domain::value::Val__ValBool,
                    >(__t);
                    (__f.0,)
                } else {
                    let __c = (__b_fields.0.clone(),);
                    drop(__t);
                    __c
                };
                Ok(crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(aver_rt::bool_str(b)),
                ))
            }
            _ => Err(::aver_rt::str_lit!("String.fromBool requires Bool")),
        }
    }
}

/// String.fromInt(n) -> string (alias for String.fromInt).
pub fn builtinStringFromInt(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::value::Val__ValStr(aver_rt::display_str(&(n))),
    ))
}

/// String.fromFloat(f) -> string.
pub fn builtinStringFromFloat(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let (f,) = if __t.owns_node() {
                    let __f = crate::aver_generated::domain::value::Val::take::<
                        crate::aver_generated::domain::value::Val__ValFloat,
                    >(__t);
                    (__f.0,)
                } else {
                    let __c = (__f_fields.0.clone(),);
                    drop(__t);
                    __c
                };
                Ok(crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(aver_rt::display_str(&(f))),
                ))
            }
            _ => Err(::aver_rt::str_lit!("String.fromFloat requires Float")),
        }
    }
}

/// String.contains(haystack, needle) -> Bool.
pub fn builtinStringContains(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (hV, nV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringContainsInner(&hV, &nV)
    }
}

/// Inner impl of String.contains.
pub fn builtinStringContainsInner(
    hV @ _: &crate::aver_generated::domain::value::Val,
    nV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let h @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(hV)?;
    let n @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(nV)?;
    Ok(crate::aver_generated::domain::value::Val::ValBool(
        crate::aver_generated::domain::value::Val__ValBool(h.contains(&*n)),
    ))
}

/// String.startsWith(s, prefix) -> Bool.
pub fn builtinStringStartsWith(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, pV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringStartsWithInner(&sV, &pV)
    }
}

/// Inner impl of String.startsWith.
pub fn builtinStringStartsWithInner(
    sV @ _: &crate::aver_generated::domain::value::Val,
    pV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let p @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(pV)?;
    Ok(crate::aver_generated::domain::value::Val::ValBool(
        crate::aver_generated::domain::value::Val__ValBool(s.starts_with(&*p)),
    ))
}

/// String.toLower(s) -> lowercase string.
pub fn builtinStringToLower(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::value::Val__ValStr(aver_rt::string_case(&s, false)),
    ))
}

/// String.fromCodePoint(n) -> single character string.
pub fn builtinStringFromCodePoint(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    match (n).to_u32().and_then(char::from_u32).map(aver_rt::char_str) {
        Some(c @ _) => Ok(crate::aver_generated::domain::value::Val::ValSome(
            crate::aver_generated::domain::value::Val__ValSome(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(c),
                ),
            ),
        )),
        None => Ok(crate::aver_generated::domain::value::Val::ValNone),
    }
}

/// String.firstCodePoint(s) -> optional first Unicode code point.
pub fn builtinStringFirstCodePoint(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match (s)
        .chars()
        .next()
        .map(|c| aver_rt::AverInt::from_i64(c as i64))
    {
        Some(code @ _) => Ok(crate::aver_generated::domain::value::Val::ValSome(
            crate::aver_generated::domain::value::Val__ValSome(
                crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(code),
                ),
            ),
        )),
        None => Ok(crate::aver_generated::domain::value::Val::ValNone),
    }
}

/// Int.fromString(s) -> Result<Int, String>.
pub fn builtinIntFromString(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match ({
        let __s = &(s);
        __s.parse::<aver_rt::AverInt>()
            .map_err(|_| format!("Cannot parse '{}' as Int", __s))
    })
    .into_aver()
    {
        Ok(n @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(n),
                ),
            ),
        )),
        Err(e @ _) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            crate::aver_generated::domain::value::Val__ValErr(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(e),
                ),
            ),
        )),
    }
}

/// String.chars(s) -> list of single-character strings.
pub fn builtinStringChars(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::value::Val__ValList({
            let __arg3 @ _ = aver_rt::AverInt::from_i64(s.chars().count() as i64);
            {
                let __arg4 @ _ =
                    <aver_rt::AverList<crate::aver_generated::domain::value::Val>>::empty();
                crate::aver_generated::domain::builtins::primitives::stringToCharList(
                    s,
                    aver_rt::AverInt::from_i64(0),
                    __arg3,
                    __arg4,
                )
            }
        }),
    ))
}

/// Split string into list of ValStr chars.
#[inline(always)]
pub fn stringToCharList(
    s @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    total @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    crate::cancel_checkpoint();
    let __arg4 @ _ = aver_rt::string_index_build(&s);
    crate::aver_generated::domain::builtins::primitives::stringToCharList__indexed(
        s,
        pos.to_i64().expect("Int out of i64 range"),
        total,
        acc,
        __arg4,
    )
}

/// Dispatch Float.* builtins.
#[inline(always)]
pub fn callFloat(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
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
                                                                            __b.push_str(&::aver_rt::str_lit!("unknown float builtin: "));
                                                                            __b
                                                                        };
                                                                        __b.push_display(&(name));
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
pub fn builtinFloatFromString(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match ({
        let __s = &(s);
        __s.parse::<f64>()
            .map_err(|_| format!("Cannot parse '{}' as Float", __s))
    })
    .into_aver()
    {
        Ok(f @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(f),
                ),
            ),
        )),
        Err(e @ _) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            crate::aver_generated::domain::value::Val__ValErr(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(e),
                ),
            ),
        )),
    }
}

/// Float.fromInt(n) -> Float.
pub fn builtinFloatFromInt(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let n @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValFloat(
        crate::aver_generated::domain::value::Val__ValFloat(n.to_f64()),
    ))
}

/// Float.round(f) -> Int.
pub fn builtinFloatRound(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let (f,) = if __t.owns_node() {
                    let __f = crate::aver_generated::domain::value::Val::take::<
                        crate::aver_generated::domain::value::Val__ValFloat,
                    >(__t);
                    (__f.0,)
                } else {
                    let __c = (__f_fields.0.clone(),);
                    drop(__t);
                    __c
                };
                Ok(crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(
                        aver_rt::AverInt::from_f64_trunc(f.round()),
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.round requires Float")),
        }
    }
}

/// String.fromFloat(f) -> string representation.
pub fn builtinFloatToString(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let (f,) = if __t.owns_node() {
                    let __f = crate::aver_generated::domain::value::Val::take::<
                        crate::aver_generated::domain::value::Val__ValFloat,
                    >(__t);
                    (__f.0,)
                } else {
                    let __c = (__f_fields.0.clone(),);
                    drop(__t);
                    __c
                };
                Ok(crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(aver_rt::display_str(&(f))),
                ))
            }
            _ => Err(::aver_rt::str_lit!("String.fromFloat requires Float")),
        }
    }
}

/// Float.abs(f) -> Float.
pub fn builtinFloatAbs(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let (f,) = if __t.owns_node() {
                    let __f = crate::aver_generated::domain::value::Val::take::<
                        crate::aver_generated::domain::value::Val__ValFloat,
                    >(__t);
                    (__f.0,)
                } else {
                    let __c = (__f_fields.0.clone(),);
                    drop(__t);
                    __c
                };
                Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(f.abs()),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.abs requires Float")),
        }
    }
}

/// Float.floor(f) -> Int.
pub fn builtinFloatFloor(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let (f,) = if __t.owns_node() {
                    let __f = crate::aver_generated::domain::value::Val::take::<
                        crate::aver_generated::domain::value::Val__ValFloat,
                    >(__t);
                    (__f.0,)
                } else {
                    let __c = (__f_fields.0.clone(),);
                    drop(__t);
                    __c
                };
                Ok(crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(
                        aver_rt::AverInt::from_f64_trunc(f.floor()),
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.floor requires Float")),
        }
    }
}

/// Float.ceil(f) -> Int.
pub fn builtinFloatCeil(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let (f,) = if __t.owns_node() {
                    let __f = crate::aver_generated::domain::value::Val::take::<
                        crate::aver_generated::domain::value::Val__ValFloat,
                    >(__t);
                    (__f.0,)
                } else {
                    let __c = (__f_fields.0.clone(),);
                    drop(__t);
                    __c
                };
                Ok(crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(
                        aver_rt::AverInt::from_f64_trunc(f.ceil()),
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.ceil requires Float")),
        }
    }
}

/// Float.min(a, b) -> Float.
pub fn builtinFloatMin(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let __t = pair;
        match (__t.0.view(), __t.1.view()) {
            (
                crate::aver_generated::domain::value::Val__view::ValFloat(__a_fields),
                crate::aver_generated::domain::value::Val__view::ValFloat(__b_fields),
            ) => {
                let a = __a_fields.0.clone();
                let b = __b_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(a.min(b)),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.min requires two Floats")),
        }
    }
}

/// Float.max(a, b) -> Float.
pub fn builtinFloatMax(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let __t = pair;
        match (__t.0.view(), __t.1.view()) {
            (
                crate::aver_generated::domain::value::Val__view::ValFloat(__a_fields),
                crate::aver_generated::domain::value::Val__view::ValFloat(__b_fields),
            ) => {
                let a = __a_fields.0.clone();
                let b = __b_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(a.max(b)),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.max requires two Floats")),
        }
    }
}

/// Float.sin(f) -> Float.
pub fn builtinFloatSin(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let (f,) = if __t.owns_node() {
                    let __f = crate::aver_generated::domain::value::Val::take::<
                        crate::aver_generated::domain::value::Val__ValFloat,
                    >(__t);
                    (__f.0,)
                } else {
                    let __c = (__f_fields.0.clone(),);
                    drop(__t);
                    __c
                };
                Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(f.sin()),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.sin requires Float")),
        }
    }
}

/// Float.cos(f) -> Float.
pub fn builtinFloatCos(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let (f,) = if __t.owns_node() {
                    let __f = crate::aver_generated::domain::value::Val::take::<
                        crate::aver_generated::domain::value::Val__ValFloat,
                    >(__t);
                    (__f.0,)
                } else {
                    let __c = (__f_fields.0.clone(),);
                    drop(__t);
                    __c
                };
                Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(f.cos()),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.cos requires Float")),
        }
    }
}

/// Float.sqrt(f) -> Float.
pub fn builtinFloatSqrt(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let (f,) = if __t.owns_node() {
                    let __f = crate::aver_generated::domain::value::Val::take::<
                        crate::aver_generated::domain::value::Val__ValFloat,
                    >(__t);
                    (__f.0,)
                } else {
                    let __c = (__f_fields.0.clone(),);
                    drop(__t);
                    __c
                };
                Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(f.sqrt()),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.sqrt requires Float")),
        }
    }
}

/// Float.pow(base, exp) -> Float.
pub fn builtinFloatPow(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let __t = pair;
        match (__t.0.view(), __t.1.view()) {
            (
                crate::aver_generated::domain::value::Val__view::ValFloat(__a_fields),
                crate::aver_generated::domain::value::Val__view::ValFloat(__b_fields),
            ) => {
                let a = __a_fields.0.clone();
                let b = __b_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(a.powf(b)),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.pow requires two Floats")),
        }
    }
}

/// Float.atan2(y, x) -> Float.
pub fn builtinFloatAtan2(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let __t = pair;
        match (__t.0.view(), __t.1.view()) {
            (
                crate::aver_generated::domain::value::Val__view::ValFloat(__a_fields),
                crate::aver_generated::domain::value::Val__view::ValFloat(__b_fields),
            ) => {
                let a = __a_fields.0.clone();
                let b = __b_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(a.atan2(b)),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Float.atan2 requires two Floats")),
        }
    }
}

/// Float.pi() -> Float.
pub fn builtinFloatPi(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if __list_subject.is_empty() {
            Ok(crate::aver_generated::domain::value::Val::ValFloat(
                crate::aver_generated::domain::value::Val__ValFloat(std::f64::consts::PI),
            ))
        } else {
            Err(::aver_rt::str_lit!("Float.pi takes 0 arguments"))
        }
    }
}

/// String.toUpper(s) -> uppercase string.
pub fn builtinStringToUpper(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::value::Val__ValStr(aver_rt::string_case(&s, true)),
    ))
}

/// String.trim(s) -> trimmed string.
pub fn builtinStringTrim(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::value::Val__ValStr(aver_rt::string_trim(&s)),
    ))
}

/// String.endsWith(s, suffix) -> Bool.
pub fn builtinStringEndsWith(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, pV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringEndsWithInner(&sV, &pV)
    }
}

/// Inner String.endsWith.
pub fn builtinStringEndsWithInner(
    sV @ _: &crate::aver_generated::domain::value::Val,
    pV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let p @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(pV)?;
    Ok(crate::aver_generated::domain::value::Val::ValBool(
        crate::aver_generated::domain::value::Val__ValBool(s.ends_with(&*p)),
    ))
}

/// String.split(s, sep) -> List<String>.
pub fn builtinStringSplit(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, sepV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringSplitInner(&sV, &sepV)
    }
}

/// Inner String.split.
pub fn builtinStringSplitInner(
    sV @ _: &crate::aver_generated::domain::value::Val,
    sepV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let sep @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(sepV)?;
    let parts @ _ = aver_rt::AverList::from_vec(
        s.split(&*sep)
            .map(aver_rt::AverStr::from)
            .collect::<Vec<_>>(),
    );
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::value::Val__ValList({
            let __arg5 @ _ =
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
            crate::aver_generated::domain::builtins::primitives::strPartsToVals__collected(
                &parts, __arg5,
            )
        }),
    ))
}

/// Convert string list to ValStr list.
#[inline(always)]
pub fn strPartsToVals(
    mut parts @ _: aver_rt::AverList<AverStr>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(parts, [] => { return (acc).reverse_owned(); }, [s, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend_owned(crate::aver_generated::domain::value::Val::ValStr(crate::aver_generated::domain::value::Val__ValStr(s)), acc);
            parts = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// String.repeat(s, n) -> repeated string.
pub fn builtinStringRepeat(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (sV, nV) = pair;
        crate::aver_generated::domain::builtins::primitives::builtinStringRepeatInner(&sV, &nV)
    }
}

/// Inner String.repeat.
pub fn builtinStringRepeatInner(
    sV @ _: &crate::aver_generated::domain::value::Val,
    nV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let n @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(nV)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::value::Val__ValStr(
            crate::aver_generated::domain::builtins::primitives::repeatStr(
                s,
                n,
                ::aver_rt::str_lit!(""),
            ),
        ),
    ))
}

/// Repeat string n times.
#[inline(always)]
pub fn repeatStr(mut s @ _: AverStr, mut n @ _: aver_rt::AverInt, mut acc @ _: AverStr) -> AverStr {
    loop {
        crate::cancel_checkpoint();
        if (n > aver_rt::AverInt::from_i64(0)) {
            {
                let __tco1 = n.sub(&aver_rt::AverInt::from_i64(1));
                let __tco2 = (acc + &s);
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
pub fn builtinStringReplaceAll(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if (aver_rt::AverInt::from_i64(args.len() as i64) == aver_rt::AverInt::from_i64(3)) {
        crate::aver_generated::domain::builtins::primitives::builtinStringReplaceAllExtract(args)
    } else {
        Err(::aver_rt::str_lit!("String.replace takes 3 arguments"))
    }
}

/// Extract args for String.replace.
#[inline(always)]
pub fn builtinStringReplaceAllExtract(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(sV), Some(r1)) = (__list_subject.first(), __list_subject.tail()) {
            aver_list_match!(r1, [] => Err(::aver_rt::str_lit!("String.replace takes 3 arguments")), [fromV, r2] => aver_list_match!(r2, [] => Err(::aver_rt::str_lit!("String.replace takes 3 arguments")), [toV, r3] => crate::aver_generated::domain::builtins::primitives::builtinStringReplaceAllDo(sV, &fromV, &toV)))
        } else {
            if __list_subject.is_empty() {
                Err(::aver_rt::str_lit!("String.replace takes 3 arguments"))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Inner impl of String.replace.
pub fn builtinStringReplaceAllDo(
    sV @ _: &crate::aver_generated::domain::value::Val,
    fromV @ _: &crate::aver_generated::domain::value::Val,
    toV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(sV)?;
    let from @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(fromV)?;
    let to @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(toV)?;
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::value::Val__ValStr(aver_rt::string_replace(&s, &from, &to)),
    ))
}

/// Synthesized indexed worker of `stringToCharList`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn stringToCharList__indexed(
    mut s @ _: AverStr,
    mut pos @ _: i64,
    mut total @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut __str_index @ _: aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        let reversed @ _ = acc.reverse();
        if (aver_rt::AverInt::from_i64(pos) < total) {
            match aver_rt::string_index_char_at_i64(&s, &__str_index, pos) {
                Some(c @ _) => {
                    let __tco1 = (pos + 1i64);
                    let __tco3 = aver_rt::AverList::prepend_owned(
                        crate::aver_generated::domain::value::Val::ValStr(
                            crate::aver_generated::domain::value::Val__ValStr(c),
                        ),
                        acc,
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

/// Synthesized collecting variant of `extractStrings`. Appends to a builder where `extractStrings` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn extractStrings__collected(
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<AverStr>,
) -> Result<aver_rt::AverList<AverStr>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = items;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = items;
            if __list_subject.is_empty() {
                return Ok(aver_rt::list_builder_finalize(acc));
            } else {
                if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __t = v;
                        match __t.view() {
                            crate::aver_generated::domain::value::Val__view::ValStr(__s_fields) => {
                                let s = __s_fields.0.clone();
                                {
                                    let __tco0 = rest;
                                    let __tco1 = aver_rt::list_builder_push(acc, s);
                                    __hold0 = __tco0;
                                    items = &__hold0;
                                    acc = __tco1;
                                    continue;
                                }
                            }
                            _ => {
                                return Err(::aver_rt::str_lit!(
                                    "String.join requires list of strings"
                                ));
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

/// Synthesized collecting variant of `strPartsToVals`. Appends to a builder where `strPartsToVals` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn strPartsToVals__collected(
    parts @ _: &aver_rt::AverList<AverStr>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut parts @ _: &aver_rt::AverList<AverStr> = parts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = parts;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let Some((s, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::list_builder_push(
                            acc,
                            crate::aver_generated::domain::value::Val::ValStr(
                                crate::aver_generated::domain::value::Val__ValStr(s),
                            ),
                        );
                        __hold0 = __tco0;
                        parts = &__hold0;
                        acc = __tco1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}
