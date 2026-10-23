#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    TuplesToMap(
        aver_rt::AverList<crate::aver_generated::domain::value::Val>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    TuplesToMapOne(
        aver_rt::AverList<crate::aver_generated::domain::value::Val>,
        aver_rt::AverList<crate::aver_generated::domain::value::Val>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
) -> aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val> {
    loop {
        __state = match __state {
            __MutualTco1::TuplesToMap(mut items @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                aver_list_match!(items, [] => { return acc }, [item, rest] => { { let __t = item; match __t.view() {
        crate::aver_generated::domain::value::Val__view::ValTuple(__parts_fields) => {
            let parts = &__parts_fields.0;
            __MutualTco1::TuplesToMapOne(parts.clone(), rest, acc)
        },
        _ => {
            __MutualTco1::TuplesToMap(rest, acc)
        }
    } } })
            }
            __MutualTco1::TuplesToMapOne(mut parts @ _, mut rest @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                aver_list_match!(parts, [] => __MutualTco1::TuplesToMap(rest, acc), [kV, tail] => { aver_list_match!(tail, [] => __MutualTco1::TuplesToMap(rest, acc), [vV, ignored] => { { let __arg7 @ _ = acc.insert_owned(crate::aver_generated::domain::value::mapKeyRepr(&kV), vV); __MutualTco1::TuplesToMap(rest, __arg7) } }) })
            }
        };
    }
}

/// Convert list of (key, value) tuples to a Map.
pub fn tuplesToMap(
    items @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    acc @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val> {
    __mutual_tco_trampoline_1(__MutualTco1::TuplesToMap(items, acc))
}

/// Extract key-value from tuple parts.
pub fn tuplesToMapOne(
    parts @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    rest @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    acc @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val> {
    __mutual_tco_trampoline_1(__MutualTco1::TuplesToMapOne(parts, rest, acc))
}

/// Dispatch qualified builtin calls to sub-module implementations.
pub fn callBuiltin(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::builtins::callBuiltin__indexed(name, args, __arg2)
}

/// Fast exact-match dispatch for the hottest builtins.
#[inline(always)]
pub fn callBuiltinFast(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Option<Result<crate::aver_generated::domain::value::Val, AverStr>> {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::builtins::callBuiltinFast__indexed(name, args, &__arg2)
}

/// Dispatch pre-evaluated args by integer builtin ID. IDs assigned in Ast.builtinNameToId.
#[inline(always)]
pub fn callBuiltinByIdValues(
    id @ _: aver_rt::AverInt,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = id.clone();
        if __dispatch_subject == aver_rt::AverInt::from_i64(1) {
            crate::aver_generated::domain::builtins::builtinMapSet(args)
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(2) {
                crate::aver_generated::domain::builtins::builtinMapGet(args)
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(3) {
                    crate::aver_generated::domain::builtins::builtinMapHas(args)
                } else {
                    if __dispatch_subject == aver_rt::AverInt::from_i64(4) {
                        crate::aver_generated::domain::builtins::builtinMapFromList(args)
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(5) {
                            crate::aver_generated::domain::builtins::builtinMapEntries(args)
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(6) {
                                crate::aver_generated::domain::builtins::builtinMapRemove(args)
                            } else {
                                if __dispatch_subject == aver_rt::AverInt::from_i64(7) {
                                    crate::aver_generated::domain::builtins::vector::call(
                                        ::aver_rt::str_lit!("Vector.new"),
                                        args,
                                    )
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(8) {
                                        crate::aver_generated::domain::builtins::vector::call(
                                            ::aver_rt::str_lit!("Vector.get"),
                                            args,
                                        )
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(9) {
                                            crate::aver_generated::domain::builtins::vector::call(
                                                ::aver_rt::str_lit!("Vector.set"),
                                                args,
                                            )
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(10)
                                            {
                                                crate::aver_generated::domain::builtins::vector::call(::aver_rt::str_lit!("Vector.len"), args)
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(11)
                                                {
                                                    crate::aver_generated::domain::builtins::vector::call(::aver_rt::str_lit!("Vector.fromList"), args)
                                                } else {
                                                    if __dispatch_subject
                                                        == aver_rt::AverInt::from_i64(12)
                                                    {
                                                        crate::aver_generated::domain::builtins::vector::call(::aver_rt::str_lit!("List.fromVector"), args)
                                                    } else {
                                                        if __dispatch_subject
                                                            == aver_rt::AverInt::from_i64(13)
                                                        {
                                                            Ok(crate::aver_generated::domain::value::Val::ValNone)
                                                        } else {
                                                            if __dispatch_subject
                                                                == aver_rt::AverInt::from_i64(14)
                                                            {
                                                                crate::aver_generated::domain::builtins::wrappers::call(::aver_rt::str_lit!("Option.Some"), args)
                                                            } else {
                                                                if __dispatch_subject
                                                                    == aver_rt::AverInt::from_i64(
                                                                        15,
                                                                    )
                                                                {
                                                                    crate::aver_generated::domain::builtins::wrappers::call(::aver_rt::str_lit!("Option.withDefault"), args)
                                                                } else {
                                                                    if __dispatch_subject == aver_rt::AverInt::from_i64(16) { crate::aver_generated::domain::builtins::wrappers::call(::aver_rt::str_lit!("Result.Ok"), args) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(17) { crate::aver_generated::domain::builtins::wrappers::call(::aver_rt::str_lit!("Result.Err"), args) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(18) { crate::aver_generated::domain::builtins::wrappers::call(::aver_rt::str_lit!("Result.withDefault"), args) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(19) { crate::aver_generated::domain::builtins::primitives::callInt(::aver_rt::str_lit!("String.fromInt"), args) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(20) { crate::aver_generated::domain::builtins::list::call(::aver_rt::str_lit!("List.take"), args) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(21) { crate::aver_generated::domain::builtins::list::call(::aver_rt::str_lit!("List.drop"), args) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("Unknown builtin ID: ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&((id.to_string()).into_aver())))); __b })) } } } } } }
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

/// Continue dispatch after List.
pub fn callBuiltinAfterList(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::builtins::callBuiltinAfterList__indexed(name, args, __arg2)
}

/// Handle non-prefixed builtins: Result.*, Option.*, Bool.*, Map.*, services, and variant constructors.
pub fn callBuiltinOther(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::builtins::callBuiltinOther__indexed(name, args, __arg2)
}

/// Console.print(v) -> print any value and return Unit.
pub fn builtinConsolePrint(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __provider_arg0: AverStr = crate::aver_generated::domain::value::valRepr(&v);
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.print",
            "reissued",
            crate::aver_replay::effect_args("Console.print", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.print",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
}

/// Console.error(v) -> print to stderr.
pub fn builtinConsoleError(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __provider_arg0: AverStr = crate::aver_generated::domain::value::valRepr(&v);
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.error",
            "reissued",
            crate::aver_replay::effect_args("Console.error", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.error",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
}

/// Console.warn(v) -> print warning.
pub fn builtinConsoleWarn(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __provider_arg0: AverStr = crate::aver_generated::domain::value::valRepr(&v);
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.warn",
            "reissued",
            crate::aver_replay::effect_args("Console.warn", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.warn",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
}

/// Console.readLine() -> Result<String, String>.
pub fn builtinConsoleReadLine(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.readLine",
            "recorded",
            crate::aver_replay::effect_args("Console.readLine", || vec![]),
            || {
                crate::provider_support::invoke::<Result<AverStr, AverStr>>(
                    "Console",
                    "Console.readLine",
                    vec![],
                    None,
                    "Result<String, String>",
                )
            },
        )
    } {
        Ok(line @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(line),
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

/// Disk.readText(path) -> Result string.
pub fn builtinDiskReadText(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __provider_arg0: AverStr = path;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.readText",
            "recorded",
            crate::aver_replay::effect_args("Disk.readText", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<AverStr, AverStr>>(
                    "Disk",
                    "Disk.readText",
                    vec![crate::provider_support::encode(__provider_arg0, "Disk")],
                    None,
                    "Result<String, String>",
                )
            },
        )
    } {
        Ok(content @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(content),
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

/// Args.get() -> list of string args.
pub fn builtinArgsGet(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let rawArgs @ _ = {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Args.get",
            "recorded",
            crate::aver_replay::effect_args("Args.get", || vec![]),
            || {
                crate::provider_support::invoke::<aver_rt::AverList<AverStr>>(
                    "Args",
                    "Args.get",
                    vec![],
                    None,
                    "List<String>",
                )
            },
        )
    };
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::value::Val__ValList({
            let __arg2 @ _ =
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
            crate::aver_generated::domain::builtins::stringsToVals__collected(&rawArgs, __arg2)
        }),
    ))
}

/// Env.get(key) -> Option<String>.
pub fn builtinEnvGet(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let key @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __provider_arg0: AverStr = key;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Env.get",
            "recorded",
            crate::aver_replay::effect_args("Env.get", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Option<AverStr>>(
                    "Env",
                    "Env.get",
                    vec![crate::provider_support::encode(__provider_arg0, "Env")],
                    None,
                    "Option<String>",
                )
            },
        )
    } {
        Some(value @ _) => Ok(crate::aver_generated::domain::value::Val::ValSome(
            crate::aver_generated::domain::value::Val__ValSome(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(value),
                ),
            ),
        )),
        None => Ok(crate::aver_generated::domain::value::Val::ValNone),
    }
}

/// Env.set(key, value) -> Result<Unit, String>.
pub fn builtinEnvSet(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (keyV, valueV) = pair;
        crate::aver_generated::domain::builtins::builtinEnvSetInner(&keyV, &valueV)
    }
}

/// Inner Env.set.
pub fn builtinEnvSetInner(
    keyV @ _: &crate::aver_generated::domain::value::Val,
    valueV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let key @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(keyV)?;
    let value @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(valueV)?;
    match {
        let __provider_arg0: AverStr = key;
        let __provider_arg1: AverStr = value;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Env.set",
            "recorded",
            crate::aver_replay::effect_args("Env.set", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Env",
                    "Env.set",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Env"),
                        crate::provider_support::encode(__provider_arg1, "Env"),
                    ],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Convert list of strings to list of ValStr.
#[inline(always)]
pub fn stringsToVals(
    mut strs @ _: aver_rt::AverList<AverStr>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(strs, [] => { return acc.reverse(); }, [s, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend_owned(crate::aver_generated::domain::value::Val::ValStr(crate::aver_generated::domain::value::Val__ValStr(s)), acc);
            strs = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Map.entries(map) -> List of tuples.
pub fn builtinMapEntries(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                Ok(crate::aver_generated::domain::value::Val::ValList(
                    crate::aver_generated::domain::value::Val__ValList({
                        let __arg3 @ _ = {
                            let mut es: Vec<_> =
                                m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                            es.sort_by(|a, b| a.0.cmp(&b.0));
                            aver_rt::AverList::from_vec(es)
                        };
                        {
                            let __arg4 @ _ = aver_rt::list_builder_new(
                                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                            );
                            crate::aver_generated::domain::builtins::mapEntriesToTuples__collected(
                                __arg3, __arg4,
                            )
                        }
                    }),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Map.entries requires a Map")),
        }
    }
}

/// Convert entries to list of (key, value) tuples.
#[inline(always)]
pub fn mapEntriesToTuples(
    mut entries @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(entries, [] => { return acc.reverse(); }, [pair, rest] => { { let (k, v) = pair; {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend_owned(crate::aver_generated::domain::value::Val::ValTuple(crate::aver_generated::domain::value::Val__ValTuple(aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::value::Val::ValStr(crate::aver_generated::domain::value::Val__ValStr(k)), v]))), acc);
            entries = __tco0;
            acc = __tco1;
            continue;
        } } })
    }
}

/// Map.keys(map) -> List of keys.
pub fn builtinMapKeys(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                Ok(crate::aver_generated::domain::value::Val::ValList(
                    crate::aver_generated::domain::value::Val__ValList({
                        let __arg3 @ _ = {
                            let mut ks: Vec<_> = m.keys().cloned().collect();
                            ks.sort();
                            aver_rt::AverList::from_vec(ks)
                        };
                        {
                            let __arg4 @ _ = aver_rt::list_builder_new(
                                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                            );
                            crate::aver_generated::domain::builtins::stringsToVals__collected(
                                &__arg3, __arg4,
                            )
                        }
                    }),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Map.keys requires a Map")),
        }
    }
}

/// Map.values(map) -> List of values.
pub fn builtinMapValues(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                Ok(crate::aver_generated::domain::value::Val::ValList(
                    crate::aver_generated::domain::value::Val__ValList({
                        let mut es: Vec<_> =
                            m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                        es.sort_by(|a, b| a.0.cmp(&b.0));
                        aver_rt::AverList::from_vec(
                            es.into_iter().map(|(_, v)| v).collect::<Vec<_>>(),
                        )
                    }),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Map.values requires a Map")),
        }
    }
}

/// Map.fromList(entries) -> Map.
pub fn builtinMapFromList(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items @ _ = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValMap(
        crate::aver_generated::domain::value::Val__ValMap({
            let __arg3 @ _ =
                <aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>>::new();
            crate::aver_generated::domain::builtins::tuplesToMap(items, __arg3)
        }),
    ))
}

/// Map.size(map) -> Int.
pub fn builtinMapSize(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                Ok(crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(aver_rt::AverInt::from_i64(
                        m.len() as i64,
                    )),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Map.size requires a Map")),
        }
    }
}

/// Map.remove(map, key) -> Map.
pub fn builtinMapRemove(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (mapV, keyV) = pair;
        crate::aver_generated::domain::builtins::builtinMapRemoveInner(&mapV, &keyV)
    }
}

/// Inner Map.remove.
pub fn builtinMapRemoveInner(
    mapV @ _: &crate::aver_generated::domain::value::Val,
    keyV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = mapV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                Ok(crate::aver_generated::domain::value::Val::ValMap(
                    crate::aver_generated::domain::value::Val__ValMap(
                        m.clone()
                            .remove_owned(&crate::aver_generated::domain::value::mapKeyRepr(keyV)),
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Map.remove requires a Map")),
        }
    }
}

/// Dispatch Terminal.*, Time.*, Random.*, Http.*, Tcp.* service builtins.
pub fn callBuiltinServices(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::builtins::callBuiltinServices__indexed(name, args, __arg2)
}

/// Random.int(min, max) -> random integer.
pub fn builtinRandomInt(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (minV, maxV) = pair;
        crate::aver_generated::domain::builtins::builtinRandomIntInner(&minV, &maxV)
    }
}

/// Inner Random.int.
pub fn builtinRandomIntInner(
    minV @ _: &crate::aver_generated::domain::value::Val,
    maxV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let minN @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(minV)?;
    let maxN @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(maxV)?;
    match {
        let __provider_arg0: aver_rt::AverInt = minN;
        let __provider_arg1: aver_rt::AverInt = maxN;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Random.int",
            "recorded",
            crate::aver_replay::effect_args("Random.int", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<Result<aver_rt::AverInt, AverStr>>(
                    "Random",
                    "Random.int",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Random"),
                        crate::provider_support::encode(__provider_arg1, "Random"),
                    ],
                    None,
                    "Result<Int, String>",
                )
            },
        )
    } {
        Ok(value @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(value),
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

/// Time.sleep(ms) -> Result<Unit, String>.
pub fn builtinTimeSleep(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let ms @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    match {
        let __provider_arg0: aver_rt::AverInt = ms;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Time.sleep",
            "recorded",
            crate::aver_replay::effect_args("Time.sleep", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Time",
                    "Time.sleep",
                    vec![crate::provider_support::encode(__provider_arg0, "Time")],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Time.unixMs() -> Int.
pub fn builtinTimeUnixMs(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    Ok(crate::aver_generated::domain::value::Val::ValInt(
        crate::aver_generated::domain::value::Val__ValInt({
            crate::cancel_checkpoint();
            crate::aver_replay::invoke_capability_effect(
                "Time.unixMs",
                "recorded",
                crate::aver_replay::effect_args("Time.unixMs", || vec![]),
                || {
                    crate::provider_support::invoke::<aver_rt::AverInt>(
                        "Time",
                        "Time.unixMs",
                        vec![],
                        None,
                        "Int",
                    )
                },
            )
        }),
    ))
}

/// Terminal no-arg commands: clear, flush, enableRawMode, etc.
pub fn builtinTerminalNoArg(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::builtins::builtinTerminalNoArg__indexed(name, args, &__arg2)
}

/// Clear the terminal screen.
pub fn termClear() -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.clear",
            "recorded",
            crate::aver_replay::effect_args("Terminal.clear", || vec![]),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Terminal",
                    "Terminal.clear",
                    vec![],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Flush terminal output.
pub fn termFlush() -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.flush",
            "recorded",
            crate::aver_replay::effect_args("Terminal.flush", || vec![]),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Terminal",
                    "Terminal.flush",
                    vec![],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Enable terminal raw mode.
pub fn termEnableRawMode() -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.enableRawMode",
            "recorded",
            crate::aver_replay::effect_args("Terminal.enableRawMode", || vec![]),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Terminal",
                    "Terminal.enableRawMode",
                    vec![],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Disable terminal raw mode.
pub fn termDisableRawMode() -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.disableRawMode",
            "recorded",
            crate::aver_replay::effect_args("Terminal.disableRawMode", || vec![]),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Terminal",
                    "Terminal.disableRawMode",
                    vec![],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Hide terminal cursor.
pub fn termHideCursor() -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.hideCursor",
            "recorded",
            crate::aver_replay::effect_args("Terminal.hideCursor", || vec![]),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Terminal",
                    "Terminal.hideCursor",
                    vec![],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Show terminal cursor.
pub fn termShowCursor() -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.showCursor",
            "recorded",
            crate::aver_replay::effect_args("Terminal.showCursor", || vec![]),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Terminal",
                    "Terminal.showCursor",
                    vec![],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Reset terminal color.
pub fn termResetColor() -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.resetColor",
            "recorded",
            crate::aver_replay::effect_args("Terminal.resetColor", || vec![]),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Terminal",
                    "Terminal.resetColor",
                    vec![],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Terminal.readKey() -> Result<Option<String>, String>.
pub fn builtinTerminalReadKey(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.readKey",
            "recorded",
            crate::aver_replay::effect_args("Terminal.readKey", || vec![]),
            || {
                crate::provider_support::invoke::<Result<Option<AverStr>, AverStr>>(
                    "Terminal",
                    "Terminal.readKey",
                    vec![],
                    None,
                    "Result<Option<String>, String>",
                )
            },
        )
    } {
        Ok(key @ _) => match key {
            Some(k @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
                crate::aver_generated::domain::value::Val__ValOk(
                    crate::aver_generated::domain::value::Val::ValSome(
                        crate::aver_generated::domain::value::Val__ValSome(
                            crate::aver_generated::domain::value::Val::ValStr(
                                crate::aver_generated::domain::value::Val__ValStr(k),
                            ),
                        ),
                    ),
                ),
            )),
            None => Ok(crate::aver_generated::domain::value::Val::ValOk(
                crate::aver_generated::domain::value::Val__ValOk(
                    crate::aver_generated::domain::value::Val::ValNone,
                ),
            )),
        },
        Err(e @ _) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            crate::aver_generated::domain::value::Val__ValErr(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(e),
                ),
            ),
        )),
    }
}

/// Terminal.size() -> Result record with width and height.
pub fn builtinTerminalSize(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.size",
            "recorded",
            crate::aver_replay::effect_args("Terminal.size", || vec![]),
            || {
                crate::provider_support::invoke::<
                    Result<crate::aver_generated::terminal::Size, AverStr>,
                >(
                    "Terminal",
                    "Terminal.size",
                    vec![],
                    None,
                    "Result<Terminal.Size, String>",
                )
            },
        )
    } {
        Ok(sz @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValRecord(
                    crate::aver_generated::domain::value::Val__ValRecord(
                        ::aver_rt::str_lit!("TerminalSize"),
                        aver_rt::AverList::from_vec(vec![
                            (
                                ::aver_rt::str_lit!("width"),
                                crate::aver_generated::domain::value::Val::ValInt(
                                    crate::aver_generated::domain::value::Val__ValInt(sz.width),
                                ),
                            ),
                            (
                                ::aver_rt::str_lit!("height"),
                                crate::aver_generated::domain::value::Val::ValInt(
                                    crate::aver_generated::domain::value::Val__ValInt(sz.height),
                                ),
                            ),
                        ]),
                    ),
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

/// Terminal.print(s) -> Result<Unit, String>.
pub fn builtinTerminalPrint(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = &v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValStr(__s_fields) => {
                let s = __s_fields.0.clone();
                crate::aver_generated::domain::builtins::termPrintStr(s)
            }
            _ => {
                let __arg3 @ _ = crate::aver_generated::domain::value::valRepr(&v);
                crate::aver_generated::domain::builtins::termPrintStr(__arg3)
            }
        }
    }
}

/// Print string to terminal.
pub fn termPrintStr(s @ _: AverStr) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        let __provider_arg0: AverStr = s;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.print",
            "recorded",
            crate::aver_replay::effect_args("Terminal.print", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Terminal",
                    "Terminal.print",
                    vec![crate::provider_support::encode(__provider_arg0, "Terminal")],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Terminal.setColor(color) -> Result<Unit, String>.
pub fn builtinTerminalSetColor(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __provider_arg0: AverStr = s;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.setColor",
            "recorded",
            crate::aver_replay::effect_args("Terminal.setColor", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Terminal",
                    "Terminal.setColor",
                    vec![crate::provider_support::encode(__provider_arg0, "Terminal")],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Terminal.moveTo(x, y) -> Result<Unit, String>.
pub fn builtinTerminalMoveTo(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (xV, yV) = pair;
        crate::aver_generated::domain::builtins::builtinTerminalMoveToInner(&xV, &yV)
    }
}

/// Inner Terminal.moveTo.
pub fn builtinTerminalMoveToInner(
    xV @ _: &crate::aver_generated::domain::value::Val,
    yV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let x @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(xV)?;
    let y @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(yV)?;
    match {
        let __provider_arg0: aver_rt::AverInt = x;
        let __provider_arg1: aver_rt::AverInt = y;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Terminal.moveTo",
            "recorded",
            crate::aver_replay::effect_args("Terminal.moveTo", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Terminal",
                    "Terminal.moveTo",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Terminal"),
                        crate::provider_support::encode(__provider_arg1, "Terminal"),
                    ],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Disk.writeText(path, content) -> Result.
pub fn builtinDiskWriteText(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (pathV, contentV) = pair;
        crate::aver_generated::domain::builtins::builtinDiskWriteTextInner(&pathV, &contentV)
    }
}

/// Inner Disk.writeText.
pub fn builtinDiskWriteTextInner(
    pathV @ _: &crate::aver_generated::domain::value::Val,
    contentV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let path @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(pathV)?;
    let content @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(contentV)?;
    match {
        let __provider_arg0: AverStr = path;
        let __provider_arg1: AverStr = content;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.writeText",
            "recorded",
            crate::aver_replay::effect_args("Disk.writeText", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Disk",
                    "Disk.writeText",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Disk"),
                        crate::provider_support::encode(__provider_arg1, "Disk"),
                    ],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Disk.appendText(path, content) -> Result.
pub fn builtinDiskAppendText(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (pathV, contentV) = pair;
        crate::aver_generated::domain::builtins::builtinDiskAppendTextInner(&pathV, &contentV)
    }
}

/// Inner Disk.appendText.
pub fn builtinDiskAppendTextInner(
    pathV @ _: &crate::aver_generated::domain::value::Val,
    contentV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let path @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(pathV)?;
    let content @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(contentV)?;
    match {
        let __provider_arg0: AverStr = path;
        let __provider_arg1: AverStr = content;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.appendText",
            "recorded",
            crate::aver_replay::effect_args("Disk.appendText", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Disk",
                    "Disk.appendText",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Disk"),
                        crate::provider_support::encode(__provider_arg1, "Disk"),
                    ],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Disk.delete(path) -> Result.
pub fn builtinDiskDelete(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __provider_arg0: AverStr = path;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.delete",
            "recorded",
            crate::aver_replay::effect_args("Disk.delete", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Disk",
                    "Disk.delete",
                    vec![crate::provider_support::encode(__provider_arg0, "Disk")],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Disk.deleteDir(path) -> Result.
pub fn builtinDiskDeleteDir(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __provider_arg0: AverStr = path;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.deleteDir",
            "recorded",
            crate::aver_replay::effect_args("Disk.deleteDir", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Disk",
                    "Disk.deleteDir",
                    vec![crate::provider_support::encode(__provider_arg0, "Disk")],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Disk.makeDir(path) -> Result.
pub fn builtinDiskMakeDir(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __provider_arg0: AverStr = path;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.makeDir",
            "recorded",
            crate::aver_replay::effect_args("Disk.makeDir", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Disk",
                    "Disk.makeDir",
                    vec![crate::provider_support::encode(__provider_arg0, "Disk")],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Disk.exists(path) -> Bool.
pub fn builtinDiskExists(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValBool(
        crate::aver_generated::domain::value::Val__ValBool({
            let __provider_arg0: AverStr = path;
            crate::cancel_checkpoint();
            crate::aver_replay::invoke_capability_effect(
                "Disk.exists",
                "recorded",
                crate::aver_replay::effect_args("Disk.exists", || {
                    vec![crate::aver_replay::ReplayValue::to_replay_json(
                        &__provider_arg0,
                    )]
                }),
                || {
                    crate::provider_support::invoke::<bool>(
                        "Disk",
                        "Disk.exists",
                        vec![crate::provider_support::encode(__provider_arg0, "Disk")],
                        None,
                        "Bool",
                    )
                },
            )
        }),
    ))
}

/// Disk.listDir(path) -> Result<List<String>, String>.
pub fn builtinDiskListDir(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __provider_arg0: AverStr = path;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.listDir",
            "recorded",
            crate::aver_replay::effect_args("Disk.listDir", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<aver_rt::AverList<AverStr>, AverStr>>(
                    "Disk",
                    "Disk.listDir",
                    vec![crate::provider_support::encode(__provider_arg0, "Disk")],
                    None,
                    "Result<List<String>, String>",
                )
            },
        )
    } {
        Ok(entries @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValList(
                    crate::aver_generated::domain::value::Val__ValList({
                        let __arg5 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        crate::aver_generated::domain::builtins::stringsToVals__collected(
                            &entries, __arg5,
                        )
                    }),
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

/// Try to construct a variant value from a dotted name like Type.Ctor. Uses stable tags for both builtin and user constructors.
#[inline(always)]
pub fn tryVariantConstructor(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::builtins::tryVariantConstructor__indexed(name, args, __arg2)
}

/// Split 'Type.Ctor' into (Type, Ctor). Returns None if no dot.
#[inline(always)]
pub fn splitDotted(name @ _: AverStr) -> Option<(AverStr, AverStr)> {
    crate::cancel_checkpoint();
    let __arg1 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::builtins::splitDotted__indexed(name, __arg1)
}

/// Find first dot and split.
#[inline(always)]
pub fn splitDottedLoop(
    name @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    total @ _: aver_rt::AverInt,
) -> Option<(AverStr, AverStr)> {
    crate::cancel_checkpoint();
    let __arg3 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::builtins::splitDottedLoop__indexed(name, pos, total, __arg3)
}

/// Bool.or(a, b) -> a || b.
pub fn builtinBoolOr(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::builtinBoolOrInner(&aV, &bV)
    }
}

/// Inner Bool.or.
pub fn builtinBoolOrInner(
    aV @ _: &crate::aver_generated::domain::value::Val,
    bV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = aV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValBool(__a_fields) => {
                let a = __a_fields.0.clone();
                {
                    let __t = bV;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValBool(__b_fields) => {
                            let b = __b_fields.0.clone();
                            if a {
                                Ok(crate::aver_generated::domain::value::Val::ValBool(
                                    crate::aver_generated::domain::value::Val__ValBool(true),
                                ))
                            } else {
                                Ok(crate::aver_generated::domain::value::Val::ValBool(
                                    crate::aver_generated::domain::value::Val__ValBool(b),
                                ))
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("Bool.or requires Bool arguments")),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("Bool.or requires Bool arguments")),
        }
    }
}

/// Bool.and(a, b) -> a && b.
pub fn builtinBoolAnd(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::builtinBoolAndInner(&aV, &bV)
    }
}

/// Inner Bool.and.
pub fn builtinBoolAndInner(
    aV @ _: &crate::aver_generated::domain::value::Val,
    bV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = aV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValBool(__a_fields) => {
                let a = __a_fields.0.clone();
                {
                    let __t = bV;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValBool(__b_fields) => {
                            let b = __b_fields.0.clone();
                            if a {
                                Ok(crate::aver_generated::domain::value::Val::ValBool(
                                    crate::aver_generated::domain::value::Val__ValBool(b),
                                ))
                            } else {
                                Ok(crate::aver_generated::domain::value::Val::ValBool(
                                    crate::aver_generated::domain::value::Val__ValBool(false),
                                ))
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("Bool.and requires Bool arguments")),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("Bool.and requires Bool arguments")),
        }
    }
}

/// Bool.not(a) -> negation.
pub fn builtinBoolNot(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValBool(__b_fields) => {
                let b = __b_fields.0.clone();
                if b {
                    Ok(crate::aver_generated::domain::value::Val::ValBool(
                        crate::aver_generated::domain::value::Val__ValBool(false),
                    ))
                } else {
                    Ok(crate::aver_generated::domain::value::Val::ValBool(
                        crate::aver_generated::domain::value::Val__ValBool(true),
                    ))
                }
            }
            _ => Err(::aver_rt::str_lit!("Bool.not requires Bool argument")),
        }
    }
}

/// Map.set(map, key, value) -> updated map.
pub fn builtinMapSet(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(mapV), Some(r1)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = r1;
                if let (Some(keyV), Some(r2)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = r2;
                        if let (Some(valV), Some(r3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            crate::aver_generated::domain::builtins::builtinMapSetInner(
                                mapV, keyV, valV,
                            )
                        } else {
                            Err(::aver_rt::str_lit!("Map.set takes 3 arguments"))
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("Map.set takes 3 arguments"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("Map.set takes 3 arguments"))
        }
    }
}

/// Set a key in the map. O(1).
pub fn builtinMapSetInner(
    mapV @ _: &crate::aver_generated::domain::value::Val,
    keyV @ _: &crate::aver_generated::domain::value::Val,
    valV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = mapV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                Ok(crate::aver_generated::domain::value::Val::ValMap(
                    crate::aver_generated::domain::value::Val__ValMap(m.clone().insert_owned(
                        crate::aver_generated::domain::value::mapKeyRepr(keyV),
                        valV.clone(),
                    )),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Map.set requires a Map")),
        }
    }
}

/// Map.get(map, key) -> Option<Val>. O(1).
pub fn builtinMapGet(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (mapV, keyV) = pair;
        crate::aver_generated::domain::builtins::builtinMapGetInner(&mapV, &keyV)
    }
}

/// Look up key in map. O(1).
pub fn builtinMapGetInner(
    mapV @ _: &crate::aver_generated::domain::value::Val,
    keyV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = mapV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                {
                    let __subject = m
                        .get(&crate::aver_generated::domain::value::mapKeyRepr(keyV))
                        .cloned();
                    match __subject {
                        Some(v @ _) => Ok(crate::aver_generated::domain::value::Val::ValSome(
                            crate::aver_generated::domain::value::Val__ValSome(v),
                        )),
                        None => Ok(crate::aver_generated::domain::value::Val::ValNone),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("Map.get requires a Map")),
        }
    }
}

/// Map.has(map, key) -> Bool. O(1).
pub fn builtinMapHas(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (mapV, keyV) = pair;
        crate::aver_generated::domain::builtins::builtinMapHasInner(&mapV, &keyV)
    }
}

/// Check if key exists in map. O(1).
pub fn builtinMapHasInner(
    mapV @ _: &crate::aver_generated::domain::value::Val,
    keyV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = mapV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                Ok(crate::aver_generated::domain::value::Val::ValBool(
                    crate::aver_generated::domain::value::Val__ValBool(
                        m.contains_key(&crate::aver_generated::domain::value::mapKeyRepr(keyV)),
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Map.has requires a Map")),
        }
    }
}

/// Time.now() -> ISO timestamp string.
pub fn builtinTimeNow(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    Ok(crate::aver_generated::domain::value::Val::ValStr(
        crate::aver_generated::domain::value::Val__ValStr({
            crate::cancel_checkpoint();
            crate::aver_replay::invoke_capability_effect(
                "Time.now",
                "recorded",
                crate::aver_replay::effect_args("Time.now", || vec![]),
                || {
                    crate::provider_support::invoke::<AverStr>(
                        "Time",
                        "Time.now",
                        vec![],
                        None,
                        "String",
                    )
                },
            )
        }),
    ))
}

/// Http.get/head/delete(url) -> Result<Http.Response, String> forwarded to host.
pub fn builtinHttpSimple(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    method @ _: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let url @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    {
        let __dispatch_subject = method;
        if &*__dispatch_subject == "get" {
            Ok({
                let __arg4 @ _ = {
                    let __provider_arg0: AverStr = url;
                    crate::cancel_checkpoint();
                    crate::aver_replay::invoke_capability_effect(
                        "Http.get",
                        "recorded",
                        crate::aver_replay::effect_args("Http.get", || {
                            vec![crate::aver_replay::ReplayValue::to_replay_json(
                                &__provider_arg0,
                            )]
                        }),
                        || {
                            crate::provider_support::invoke::<
                                Result<crate::aver_generated::http::Response, AverStr>,
                            >(
                                "Http",
                                "Http.get",
                                vec![crate::provider_support::encode(__provider_arg0, "Http")],
                                None,
                                "Result<Http.Response, String>",
                            )
                        },
                    )
                };
                crate::aver_generated::domain::builtins::httpResponseToVal(&__arg4)
            })
        } else {
            if &*__dispatch_subject == "head" {
                Ok({
                    let __arg5 @ _ = {
                        let __provider_arg0: AverStr = url;
                        crate::cancel_checkpoint();
                        crate::aver_replay::invoke_capability_effect(
                            "Http.head",
                            "recorded",
                            crate::aver_replay::effect_args("Http.head", || {
                                vec![crate::aver_replay::ReplayValue::to_replay_json(
                                    &__provider_arg0,
                                )]
                            }),
                            || {
                                crate::provider_support::invoke::<
                                    Result<crate::aver_generated::http::Response, AverStr>,
                                >(
                                    "Http",
                                    "Http.head",
                                    vec![crate::provider_support::encode(__provider_arg0, "Http")],
                                    None,
                                    "Result<Http.Response, String>",
                                )
                            },
                        )
                    };
                    crate::aver_generated::domain::builtins::httpResponseToVal(&__arg5)
                })
            } else {
                Ok({
                    let __arg6 @ _ = {
                        let __provider_arg0: AverStr = url;
                        crate::cancel_checkpoint();
                        crate::aver_replay::invoke_capability_effect(
                            "Http.delete",
                            "recorded",
                            crate::aver_replay::effect_args("Http.delete", || {
                                vec![crate::aver_replay::ReplayValue::to_replay_json(
                                    &__provider_arg0,
                                )]
                            }),
                            || {
                                crate::provider_support::invoke::<
                                    Result<crate::aver_generated::http::Response, AverStr>,
                                >(
                                    "Http",
                                    "Http.delete",
                                    vec![crate::provider_support::encode(__provider_arg0, "Http")],
                                    None,
                                    "Result<Http.Response, String>",
                                )
                            },
                        )
                    };
                    crate::aver_generated::domain::builtins::httpResponseToVal(&__arg6)
                })
            }
        }
    }
}

/// Http.post/put/patch(url, body, contentType, headers) forwarded to host.
pub fn builtinHttpBody(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    method @ _: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(urlV), Some(r1)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = r1;
                if let (Some(bodyV), Some(r2)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = r2;
                        if let (Some(ctV), Some(r3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = r3;
                                if let (Some(hdrsV), Some(r4)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    crate::aver_generated::domain::builtins::builtinHttpBodyInner(
                                        urlV, bodyV, ctV, hdrsV, method,
                                    )
                                } else {
                                    Err(::aver_rt::str_lit!("Http method takes 4 arguments"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("Http method takes 4 arguments"))
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("Http method takes 4 arguments"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("Http method takes 4 arguments"))
        }
    }
}

/// Inner Http body method.
pub fn builtinHttpBodyInner(
    urlV @ _: &crate::aver_generated::domain::value::Val,
    bodyV @ _: &crate::aver_generated::domain::value::Val,
    ctV @ _: &crate::aver_generated::domain::value::Val,
    hdrsV @ _: &crate::aver_generated::domain::value::Val,
    method @ _: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let url @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(urlV)?;
    let body @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(bodyV)?;
    let ct @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(ctV)?;
    {
        let __dispatch_subject = method;
        if &*__dispatch_subject == "post" {
            Ok({
                let __arg8 @ _ = {
                    let __provider_arg0: AverStr = url;
                    let __provider_arg1: AverStr = body;
                    let __provider_arg2: AverStr = ct;
                    let __provider_arg3: aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>> =
                        HashMap::new();
                    crate::cancel_checkpoint();
                    crate::aver_replay::invoke_capability_effect(
                        "Http.post",
                        "recorded",
                        crate::aver_replay::effect_args("Http.post", || {
                            vec![
                                crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                                crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                                crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg2),
                                crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg3),
                            ]
                        }),
                        || {
                            crate::provider_support::invoke::<
                                Result<crate::aver_generated::http::Response, AverStr>,
                            >(
                                "Http",
                                "Http.post",
                                vec![
                                    crate::provider_support::encode(__provider_arg0, "Http"),
                                    crate::provider_support::encode(__provider_arg1, "Http"),
                                    crate::provider_support::encode(__provider_arg2, "Http"),
                                    crate::provider_support::encode(__provider_arg3, "Http"),
                                ],
                                None,
                                "Result<Http.Response, String>",
                            )
                        },
                    )
                };
                crate::aver_generated::domain::builtins::httpResponseToVal(&__arg8)
            })
        } else {
            if &*__dispatch_subject == "put" {
                Ok({
                    let __arg9 @ _ = {
                        let __provider_arg0: AverStr = url;
                        let __provider_arg1: AverStr = body;
                        let __provider_arg2: AverStr = ct;
                        let __provider_arg3: aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>> =
                            HashMap::new();
                        crate::cancel_checkpoint();
                        crate::aver_replay::invoke_capability_effect(
                            "Http.put",
                            "recorded",
                            crate::aver_replay::effect_args("Http.put", || {
                                vec![
                                    crate::aver_replay::ReplayValue::to_replay_json(
                                        &__provider_arg0,
                                    ),
                                    crate::aver_replay::ReplayValue::to_replay_json(
                                        &__provider_arg1,
                                    ),
                                    crate::aver_replay::ReplayValue::to_replay_json(
                                        &__provider_arg2,
                                    ),
                                    crate::aver_replay::ReplayValue::to_replay_json(
                                        &__provider_arg3,
                                    ),
                                ]
                            }),
                            || {
                                crate::provider_support::invoke::<
                                    Result<crate::aver_generated::http::Response, AverStr>,
                                >(
                                    "Http",
                                    "Http.put",
                                    vec![
                                        crate::provider_support::encode(__provider_arg0, "Http"),
                                        crate::provider_support::encode(__provider_arg1, "Http"),
                                        crate::provider_support::encode(__provider_arg2, "Http"),
                                        crate::provider_support::encode(__provider_arg3, "Http"),
                                    ],
                                    None,
                                    "Result<Http.Response, String>",
                                )
                            },
                        )
                    };
                    crate::aver_generated::domain::builtins::httpResponseToVal(&__arg9)
                })
            } else {
                Ok({
                    let __arg10 @ _ = {
                        let __provider_arg0: AverStr = url;
                        let __provider_arg1: AverStr = body;
                        let __provider_arg2: AverStr = ct;
                        let __provider_arg3: aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>> =
                            HashMap::new();
                        crate::cancel_checkpoint();
                        crate::aver_replay::invoke_capability_effect(
                            "Http.patch",
                            "recorded",
                            crate::aver_replay::effect_args("Http.patch", || {
                                vec![
                                    crate::aver_replay::ReplayValue::to_replay_json(
                                        &__provider_arg0,
                                    ),
                                    crate::aver_replay::ReplayValue::to_replay_json(
                                        &__provider_arg1,
                                    ),
                                    crate::aver_replay::ReplayValue::to_replay_json(
                                        &__provider_arg2,
                                    ),
                                    crate::aver_replay::ReplayValue::to_replay_json(
                                        &__provider_arg3,
                                    ),
                                ]
                            }),
                            || {
                                crate::provider_support::invoke::<
                                    Result<crate::aver_generated::http::Response, AverStr>,
                                >(
                                    "Http",
                                    "Http.patch",
                                    vec![
                                        crate::provider_support::encode(__provider_arg0, "Http"),
                                        crate::provider_support::encode(__provider_arg1, "Http"),
                                        crate::provider_support::encode(__provider_arg2, "Http"),
                                        crate::provider_support::encode(__provider_arg3, "Http"),
                                    ],
                                    None,
                                    "Result<Http.Response, String>",
                                )
                            },
                        )
                    };
                    crate::aver_generated::domain::builtins::httpResponseToVal(&__arg10)
                })
            }
        }
    }
}

/// Convert host Http.Response to Val.
#[inline(always)]
pub fn httpResponseToVal(
    result @ _: &Result<crate::aver_generated::http::Response, AverStr>,
) -> crate::aver_generated::domain::value::Val {
    crate::cancel_checkpoint();
    match result {
        Ok(resp @ _) => crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValRecord(
                    crate::aver_generated::domain::value::Val__ValRecord(
                        ::aver_rt::str_lit!("Http.Response"),
                        aver_rt::AverList::from_vec(vec![
                            (
                                ::aver_rt::str_lit!("status"),
                                crate::aver_generated::domain::value::Val::ValInt(
                                    crate::aver_generated::domain::value::Val__ValInt(
                                        resp.status.clone(),
                                    ),
                                ),
                            ),
                            (
                                ::aver_rt::str_lit!("body"),
                                crate::aver_generated::domain::value::Val::ValStr(
                                    crate::aver_generated::domain::value::Val__ValStr(
                                        resp.body.clone(),
                                    ),
                                ),
                            ),
                            (
                                ::aver_rt::str_lit!("headers"),
                                crate::aver_generated::domain::builtins::headersToVal(
                                    &resp.headers,
                                ),
                            ),
                        ]),
                    ),
                ),
            ),
        ),
        Err(e @ _) => {
            let e = e.clone();
            crate::aver_generated::domain::value::Val::ValErr(
                crate::aver_generated::domain::value::Val__ValErr(
                    crate::aver_generated::domain::value::Val::ValStr(
                        crate::aver_generated::domain::value::Val__ValStr(e),
                    ),
                ),
            )
        }
    }
}

/// Convert host headers Map<String, List<String>> to a Val.ValMap whose values are Val.ValList of Val.ValStr.
pub fn headersToVal(
    headers @ _: &aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>>,
) -> crate::aver_generated::domain::value::Val {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::value::Val::ValMap(
        crate::aver_generated::domain::value::Val__ValMap({
            let __arg1 @ _ = {
                let mut ks: Vec<_> = headers.keys().cloned().collect();
                ks.sort();
                aver_rt::AverList::from_vec(ks)
            };
            {
                let __arg2 @ _ =
                    <aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>>::new();
                crate::aver_generated::domain::builtins::headersToValMap(headers, __arg1, __arg2)
            }
        }),
    )
}

/// Walk header keys, converting each value list to a Val.ValList.
#[inline(always)]
pub fn headersToValMap(
    headers @ _: &aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>>,
    mut names @ _: aver_rt::AverList<AverStr>,
    mut acc @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(names, [] => { return acc; }, [name, rest] => { match headers.get(&name).cloned() { Some(values @ _) => { {
            let __tco1 = rest;
            let __tco2 = acc.insert_owned(name, crate::aver_generated::domain::value::Val::ValList(crate::aver_generated::domain::value::Val__ValList({ let __arg6 @ _ = aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)); crate::aver_generated::domain::builtins::stringsToValStrs__collected(&values, __arg6) })));
            names = __tco1;
            acc = __tco2;
            continue;
        } }, None => { {
            let __tco1 = rest;
            let __tco2 = acc.insert_owned(name, crate::aver_generated::domain::value::Val::ValList(crate::aver_generated::domain::value::Val__ValList(aver_rt::AverList::empty())));
            names = __tco1;
            acc = __tco2;
            continue;
        } } } })
    }
}

/// Convert a list of strings into a list of Val.ValStr (tail-recursive).
#[inline(always)]
pub fn stringsToValStrs(
    mut values @ _: aver_rt::AverList<AverStr>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(values, [] => { return acc.reverse(); }, [v, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend_owned(crate::aver_generated::domain::value::Val::ValStr(crate::aver_generated::domain::value::Val__ValStr(v)), acc);
            values = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Tcp.send(host, port, message) -> Result<String, String>.
pub fn builtinTcpSend(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(hostV), Some(r1)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = r1;
                if let (Some(portV), Some(r2)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = r2;
                        if let (Some(msgV), Some(r3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            crate::aver_generated::domain::builtins::builtinTcpSendInner(
                                hostV, portV, msgV,
                            )
                        } else {
                            Err(::aver_rt::str_lit!("Tcp.send takes 3 arguments"))
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("Tcp.send takes 3 arguments"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("Tcp.send takes 3 arguments"))
        }
    }
}

/// Inner Tcp.send.
pub fn builtinTcpSendInner(
    hostV @ _: &crate::aver_generated::domain::value::Val,
    portV @ _: &crate::aver_generated::domain::value::Val,
    msgV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let host @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(hostV)?;
    let port @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(portV)?;
    let msg @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(msgV)?;
    match {
        let __provider_arg0: AverStr = host;
        let __provider_arg1: aver_rt::AverInt = port;
        let __provider_arg2: AverStr = msg;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.send",
            "recorded",
            crate::aver_replay::effect_args("Tcp.send", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg2),
                ]
            }),
            || {
                crate::provider_support::invoke::<Result<AverStr, AverStr>>(
                    "Tcp",
                    "Tcp.send",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Tcp"),
                        crate::provider_support::encode(__provider_arg1, "Tcp"),
                        crate::provider_support::encode(__provider_arg2, "Tcp"),
                    ],
                    None,
                    "Result<String, String>",
                )
            },
        )
    } {
        Ok(resp @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(resp),
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

/// Tcp.ping(host, port) -> Result<Unit, String>.
pub fn builtinTcpPing(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (hostV, portV) = pair;
        crate::aver_generated::domain::builtins::builtinTcpPingInner(&hostV, &portV)
    }
}

/// Inner Tcp.ping.
pub fn builtinTcpPingInner(
    hostV @ _: &crate::aver_generated::domain::value::Val,
    portV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let host @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(hostV)?;
    let port @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(portV)?;
    match {
        let __provider_arg0: AverStr = host;
        let __provider_arg1: aver_rt::AverInt = port;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.ping",
            "recorded",
            crate::aver_replay::effect_args("Tcp.ping", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Tcp",
                    "Tcp.ping",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Tcp"),
                        crate::provider_support::encode(__provider_arg1, "Tcp"),
                    ],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Tcp.connect(host, port) -> Result<Tcp.Connection, String>.
pub fn builtinTcpConnect(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (hostV, portV) = pair;
        crate::aver_generated::domain::builtins::builtinTcpConnectInner(&hostV, &portV)
    }
}

/// Inner Tcp.connect.
pub fn builtinTcpConnectInner(
    hostV @ _: &crate::aver_generated::domain::value::Val,
    portV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let host @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(hostV)?;
    let port @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(portV)?;
    match {
        let __provider_arg0: AverStr = host;
        let __provider_arg1: aver_rt::AverInt = port;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.connect",
            "recorded",
            crate::aver_replay::effect_args("Tcp.connect", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<
                    Result<crate::aver_generated::tcp::Connection, AverStr>,
                >(
                    "Tcp",
                    "Tcp.connect",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Tcp"),
                        crate::provider_support::encode(__provider_arg1, "Tcp"),
                    ],
                    Some("Tcp.Connection"),
                    "Result<Tcp.Connection, String>",
                )
            },
        )
    } {
        Ok(conn @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::builtins::tcpConnToVal(conn),
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

/// Preserve an opaque host Tcp.Connection resource inside Val.
pub fn tcpConnToVal(
    mut conn @ _: crate::aver_generated::tcp::Connection,
) -> crate::aver_generated::domain::value::Val {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::value::Val::ValTcpConnection(
        crate::aver_generated::domain::value::Val__ValTcpConnection(conn),
    )
}

/// Recover an opaque host Tcp.Connection resource from Val.
pub fn valToTcpConn(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::tcp::Connection, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValTcpConnection(__conn_fields) => {
                let conn = &__conn_fields.0;
                Ok(conn.clone())
            }
            _ => Err(::aver_rt::str_lit!("expected Tcp.Connection resource")),
        }
    }
}

/// Tcp.beginConnect(host, port) -> Result<Tcp.Dial, String>.
pub fn builtinTcpBeginConnect(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (hostV, portV) = pair;
        crate::aver_generated::domain::builtins::builtinTcpBeginConnectInner(&hostV, &portV)
    }
}

/// Inner Tcp.beginConnect.
pub fn builtinTcpBeginConnectInner(
    hostV @ _: &crate::aver_generated::domain::value::Val,
    portV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let host @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(hostV)?;
    let port @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(portV)?;
    match {
        let __provider_arg0: AverStr = host;
        let __provider_arg1: aver_rt::AverInt = port;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.beginConnect",
            "recorded",
            crate::aver_replay::effect_args("Tcp.beginConnect", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<Result<crate::aver_generated::tcp::Dial, AverStr>>(
                    "Tcp",
                    "Tcp.beginConnect",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Tcp"),
                        crate::provider_support::encode(__provider_arg1, "Tcp"),
                    ],
                    Some("Tcp.Dial"),
                    "Result<Tcp.Dial, String>",
                )
            },
        )
    } {
        Ok(dial @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValTcpDial(
                    crate::aver_generated::domain::value::Val__ValTcpDial(dial),
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

/// Recover an opaque host Tcp.Dial resource from Val.
pub fn valToTcpDial(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::tcp::Dial, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValTcpDial(__dial_fields) => {
                let dial = &__dial_fields.0;
                Ok(dial.clone())
            }
            _ => Err(::aver_rt::str_lit!("expected Tcp.Dial resource")),
        }
    }
}

/// Tcp.dialled(dial) -> Result<Option<Tcp.Connection>, String>.
pub fn builtinTcpDialled(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let dial @ _ = crate::aver_generated::domain::builtins::valToTcpDial(&v)?;
    match {
        let __provider_arg0: crate::aver_generated::tcp::Dial = dial;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.dialled",
            "recorded",
            crate::aver_replay::effect_args("Tcp.dialled", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<
                    Result<Option<crate::aver_generated::tcp::Connection>, AverStr>,
                >(
                    "Tcp",
                    "Tcp.dialled",
                    vec![crate::provider_support::encode(__provider_arg0, "Tcp")],
                    Some("Tcp.Connection"),
                    "Result<Option<Tcp.Connection>, String>",
                )
            },
        )
    } {
        Ok(settled @ _) => match settled {
            None => Ok(crate::aver_generated::domain::value::Val::ValOk(
                crate::aver_generated::domain::value::Val__ValOk(
                    crate::aver_generated::domain::value::Val::ValNone,
                ),
            )),
            Some(conn @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
                crate::aver_generated::domain::value::Val__ValOk(
                    crate::aver_generated::domain::value::Val::ValSome(
                        crate::aver_generated::domain::value::Val__ValSome(
                            crate::aver_generated::domain::builtins::tcpConnToVal(conn),
                        ),
                    ),
                ),
            )),
        },
        Err(e @ _) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            crate::aver_generated::domain::value::Val__ValErr(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(e),
                ),
            ),
        )),
    }
}

/// Tcp.listen(port, backlog) -> Result<Tcp.Listener, String>.
pub fn builtinTcpListen(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (portV, backlogV) = pair;
        crate::aver_generated::domain::builtins::builtinTcpListenInner(&portV, &backlogV)
    }
}

/// Inner Tcp.listen.
pub fn builtinTcpListenInner(
    portV @ _: &crate::aver_generated::domain::value::Val,
    backlogV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let port @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(portV)?;
    let backlog @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(backlogV)?;
    match {
        let __provider_arg0: aver_rt::AverInt = port;
        let __provider_arg1: aver_rt::AverInt = backlog;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.listen",
            "recorded",
            crate::aver_replay::effect_args("Tcp.listen", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<
                    Result<crate::aver_generated::tcp::Listener, AverStr>,
                >(
                    "Tcp",
                    "Tcp.listen",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Tcp"),
                        crate::provider_support::encode(__provider_arg1, "Tcp"),
                    ],
                    Some("Tcp.Listener"),
                    "Result<Tcp.Listener, String>",
                )
            },
        )
    } {
        Ok(listener @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValTcpListener(
                    crate::aver_generated::domain::value::Val__ValTcpListener(listener),
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

/// Recover an opaque host Tcp.Listener resource from Val.
pub fn valToTcpListener(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::tcp::Listener, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValTcpListener(__listener_fields) => {
                let listener = &__listener_fields.0;
                Ok(listener.clone())
            }
            _ => Err(::aver_rt::str_lit!("expected Tcp.Listener resource")),
        }
    }
}

/// Tcp.accept(listener) -> Result<Option<Tcp.Connection>, String>.
pub fn builtinTcpAccept(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let listener @ _ = crate::aver_generated::domain::builtins::valToTcpListener(&v)?;
    match {
        let __provider_arg0: crate::aver_generated::tcp::Listener = listener;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.accept",
            "recorded",
            crate::aver_replay::effect_args("Tcp.accept", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<
                    Result<Option<crate::aver_generated::tcp::Connection>, AverStr>,
                >(
                    "Tcp",
                    "Tcp.accept",
                    vec![crate::provider_support::encode(__provider_arg0, "Tcp")],
                    Some("Tcp.Connection"),
                    "Result<Option<Tcp.Connection>, String>",
                )
            },
        )
    } {
        Ok(accepted @ _) => match accepted {
            None => Ok(crate::aver_generated::domain::value::Val::ValOk(
                crate::aver_generated::domain::value::Val__ValOk(
                    crate::aver_generated::domain::value::Val::ValNone,
                ),
            )),
            Some(conn @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
                crate::aver_generated::domain::value::Val__ValOk(
                    crate::aver_generated::domain::value::Val::ValSome(
                        crate::aver_generated::domain::value::Val__ValSome(
                            crate::aver_generated::domain::builtins::tcpConnToVal(conn),
                        ),
                    ),
                ),
            )),
        },
        Err(e @ _) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            crate::aver_generated::domain::value::Val__ValErr(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(e),
                ),
            ),
        )),
    }
}

/// Tcp.peerAddress(connection) -> Result<String, String>.
pub fn builtinTcpPeerAddress(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let connection @ _ = crate::aver_generated::domain::builtins::valToTcpConn(&v)?;
    match {
        let __provider_arg0: crate::aver_generated::tcp::Connection = connection;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.peerAddress",
            "recorded",
            crate::aver_replay::effect_args("Tcp.peerAddress", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<AverStr, AverStr>>(
                    "Tcp",
                    "Tcp.peerAddress",
                    vec![crate::provider_support::encode(__provider_arg0, "Tcp")],
                    None,
                    "Result<String, String>",
                )
            },
        )
    } {
        Ok(address @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(address),
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

/// Decode the self-host's nominal variant carrier into Tcp.Socket.
pub fn valToTcpSocket(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::tcp::Socket, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValVariant(__name_fields) => {
                let name = __name_fields.1.clone();
                let fields = &__name_fields.2;
                crate::aver_generated::domain::builtins::valVariantToTcpSocket(name, fields)
            }
            _ => Err(::aver_rt::str_lit!("expected Tcp.Socket value")),
        }
    }
}

/// Decode one named Tcp.Socket variant.
#[inline(always)]
pub fn valVariantToTcpSocket(
    name @ _: AverStr,
    fields @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::tcp::Socket, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name;
        if &*__dispatch_subject == "Tcp.Socket.Listening" {
            match crate::aver_generated::domain::builtins::helpers::oneArg(fields) {
                Ok(listenerV @ _) => Ok(crate::aver_generated::tcp::Socket::Listening(
                    crate::aver_generated::domain::builtins::valToTcpListener(&listenerV)?,
                )),
                Err(_) => Err(::aver_rt::str_lit!(
                    "Tcp.Socket.Listening takes one resource"
                )),
            }
        } else {
            if &*__dispatch_subject == "Tcp.Socket.Dialing" {
                match crate::aver_generated::domain::builtins::helpers::oneArg(fields) {
                    Ok(dialV @ _) => Ok(crate::aver_generated::tcp::Socket::Dialing(
                        crate::aver_generated::domain::builtins::valToTcpDial(&dialV)?,
                    )),
                    Err(_) => Err(::aver_rt::str_lit!("Tcp.Socket.Dialing takes one resource")),
                }
            } else {
                if &*__dispatch_subject == "Tcp.Socket.Connected" {
                    match crate::aver_generated::domain::builtins::helpers::oneArg(fields) {
                        Ok(connV @ _) => Ok(crate::aver_generated::tcp::Socket::Connected(
                            crate::aver_generated::domain::builtins::valToTcpConn(&connV)?,
                        )),
                        Err(_) => Err(::aver_rt::str_lit!(
                            "Tcp.Socket.Connected takes one resource"
                        )),
                    }
                } else {
                    if &*__dispatch_subject == "Tcp.Socket.Sending" {
                        match crate::aver_generated::domain::builtins::helpers::oneArg(fields) {
                            Ok(connV @ _) => Ok(crate::aver_generated::tcp::Socket::Sending(
                                crate::aver_generated::domain::builtins::valToTcpConn(&connV)?,
                            )),
                            Err(_) => {
                                Err(::aver_rt::str_lit!("Tcp.Socket.Sending takes one resource"))
                            }
                        }
                    } else {
                        Err(::aver_rt::str_lit!("expected Tcp.Socket value"))
                    }
                }
            }
        }
    }
}

/// Recover integer caller keys and typed socket states from a self-host Val map.
#[inline(always)]
pub fn tcpSocketEntriesToMap(
    mut entries @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut acc @ _: aver_rt::AverMap<aver_rt::AverInt, crate::aver_generated::tcp::Socket>,
) -> Result<aver_rt::AverMap<aver_rt::AverInt, crate::aver_generated::tcp::Socket>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(entries, [] => { return Ok(acc); }, [pair, rest] => { { let (keyText, socketV) = pair; {
            let __tco0 = rest;
            let __tco1 = acc.insert_owned(({ let __s = &(keyText); __s.parse::<aver_rt::AverInt>().map_err(|_| format!("Cannot parse '{}' as Int", __s)) }).into_aver()?, crate::aver_generated::domain::builtins::valToTcpSocket(&socketV)?);
            entries = __tco0;
            acc = __tco1;
            continue;
        } } })
    }
}

/// Convert ready caller keys back into self-host values.
#[inline(always)]
pub fn tcpReadyKeysToVals(
    mut keys @ _: aver_rt::AverIntList,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(keys, [] => { return acc.reverse(); }, [key, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend_owned(crate::aver_generated::domain::value::Val::ValInt(crate::aver_generated::domain::value::Val__ValInt(key)), acc);
            keys = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Tcp.poll(sockets, timeoutMs) over the single caller-owned socket map.
pub fn builtinTcpPoll(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (socketsV, timeoutV) = pair;
        {
            let __t = socketsV;
            match __t.view() {
                crate::aver_generated::domain::value::Val__view::ValMap(__sockets_fields) => {
                    let sockets = &__sockets_fields.0;
                    crate::aver_generated::domain::builtins::builtinTcpPollInner(
                        sockets.clone(),
                        &timeoutV,
                    )
                }
                _ => Err(::aver_rt::str_lit!(
                    "Tcp.poll requires a Map as its first argument"
                )),
            }
        }
    }
}

/// Inner Tcp.poll.
pub fn builtinTcpPollInner(
    mut sockets @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    timeoutV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let timeoutMs @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(timeoutV)?;
    let typedSockets @ _ = {
        let __arg6 @ _ = {
            let mut es: Vec<_> = sockets
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            es.sort_by(|a, b| a.0.cmp(&b.0));
            aver_rt::AverList::from_vec(es)
        };
        {
            let __arg7 @ _ =
                <aver_rt::AverMap<aver_rt::AverInt, crate::aver_generated::tcp::Socket>>::new();
            crate::aver_generated::domain::builtins::tcpSocketEntriesToMap(__arg6, __arg7)
        }
    }?;
    match {
        let __provider_arg0: aver_rt::AverMap<
            aver_rt::AverInt,
            crate::aver_generated::tcp::Socket,
        > = typedSockets;
        let __provider_arg1: aver_rt::AverInt = timeoutMs;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.poll",
            "recorded",
            crate::aver_replay::effect_args("Tcp.poll", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<Result<aver_rt::AverIntList, AverStr>>(
                    "Tcp",
                    "Tcp.poll",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Tcp"),
                        crate::provider_support::encode(__provider_arg1, "Tcp"),
                    ],
                    None,
                    "Result<List<Int>, String>",
                )
            },
        )
    } {
        Ok(keys @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValList(
                    crate::aver_generated::domain::value::Val__ValList({
                        let __arg8 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        crate::aver_generated::domain::builtins::tcpReadyKeysToVals__collected(
                            &keys, __arg8,
                        )
                    }),
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

/// Tcp.closeDial(dial) -> Result<Unit, String>.
pub fn builtinTcpCloseDial(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let dial @ _ = crate::aver_generated::domain::builtins::valToTcpDial(&v)?;
    match {
        let __provider_arg0: crate::aver_generated::tcp::Dial = dial;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.closeDial",
            "recorded",
            crate::aver_replay::effect_args("Tcp.closeDial", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Tcp",
                    "Tcp.closeDial",
                    vec![crate::provider_support::encode(__provider_arg0, "Tcp")],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Tcp.closeListener(listener) -> Result<Unit, String>.
pub fn builtinTcpCloseListener(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let listener @ _ = crate::aver_generated::domain::builtins::valToTcpListener(&v)?;
    match {
        let __provider_arg0: crate::aver_generated::tcp::Listener = listener;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.closeListener",
            "recorded",
            crate::aver_replay::effect_args("Tcp.closeListener", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Tcp",
                    "Tcp.closeListener",
                    vec![crate::provider_support::encode(__provider_arg0, "Tcp")],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Tcp.writeLine(conn, line) -> Result<Unit, String>.
pub fn builtinTcpWriteLine(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (connV, lineV) = pair;
        crate::aver_generated::domain::builtins::builtinTcpWriteLineInner(&connV, &lineV)
    }
}

/// Inner Tcp.writeLine.
pub fn builtinTcpWriteLineInner(
    connV @ _: &crate::aver_generated::domain::value::Val,
    lineV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let tc @ _ = crate::aver_generated::domain::builtins::valToTcpConn(connV)?;
    let line @ _ = crate::aver_generated::domain::builtins::helpers::expectStr(lineV)?;
    match {
        let __provider_arg0: crate::aver_generated::tcp::Connection = tc;
        let __provider_arg1: AverStr = line;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.writeLine",
            "recorded",
            crate::aver_replay::effect_args("Tcp.writeLine", || {
                vec![
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg0),
                    crate::aver_replay::ReplayValue::to_replay_json(&__provider_arg1),
                ]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Tcp",
                    "Tcp.writeLine",
                    vec![
                        crate::provider_support::encode(__provider_arg0, "Tcp"),
                        crate::provider_support::encode(__provider_arg1, "Tcp"),
                    ],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Tcp.readLine(conn) -> Result<String, String>.
pub fn builtinTcpReadLine(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let tc @ _ = crate::aver_generated::domain::builtins::valToTcpConn(&v)?;
    match {
        let __provider_arg0: crate::aver_generated::tcp::Connection = tc;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.readLine",
            "recorded",
            crate::aver_replay::effect_args("Tcp.readLine", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<AverStr, AverStr>>(
                    "Tcp",
                    "Tcp.readLine",
                    vec![crate::provider_support::encode(__provider_arg0, "Tcp")],
                    None,
                    "Result<String, String>",
                )
            },
        )
    } {
        Ok(line @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(line),
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

/// Tcp.close(conn) -> Result<Unit, String>.
pub fn builtinTcpClose(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let tc @ _ = crate::aver_generated::domain::builtins::valToTcpConn(&v)?;
    match {
        let __provider_arg0: crate::aver_generated::tcp::Connection = tc;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Tcp.close",
            "recorded",
            crate::aver_replay::effect_args("Tcp.close", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<Result<(), AverStr>>(
                    "Tcp",
                    "Tcp.close",
                    vec![crate::provider_support::encode(__provider_arg0, "Tcp")],
                    None,
                    "Result<Unit, String>",
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            crate::aver_generated::domain::value::Val__ValOk(
                crate::aver_generated::domain::value::Val::ValUnit,
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

/// Synthesized indexed worker of `callBuiltin`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn callBuiltin__indexed(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut __str_index @ _: aver_rt::StringIndex,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::builtins::callBuiltinFast__indexed(
        name.clone(),
        args,
        &__str_index,
    ) {
        Some(result @ _) => result,
        None => {
            if name.starts_with("List.") {
                crate::aver_generated::domain::builtins::list::call(name, args)
            } else {
                crate::aver_generated::domain::builtins::callBuiltinAfterList__indexed(
                    name,
                    args,
                    __str_index,
                )
            }
        }
    }
}

/// Synthesized indexed worker of `callBuiltinFast`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn callBuiltinFast__indexed(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    __str_index @ _: &aver_rt::StringIndex,
) -> Option<Result<crate::aver_generated::domain::value::Val, AverStr>> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Map.set" {
            Some(crate::aver_generated::domain::builtins::builtinMapSet(args))
        } else {
            if &*__dispatch_subject == "Map.get" {
                Some(crate::aver_generated::domain::builtins::builtinMapGet(args))
            } else {
                if &*__dispatch_subject == "Map.has" {
                    Some(crate::aver_generated::domain::builtins::builtinMapHas(args))
                } else {
                    if &*__dispatch_subject == "Map.fromList" {
                        Some(crate::aver_generated::domain::builtins::builtinMapFromList(
                            args,
                        ))
                    } else {
                        if &*__dispatch_subject == "Map.entries" {
                            Some(crate::aver_generated::domain::builtins::builtinMapEntries(
                                args,
                            ))
                        } else {
                            if &*__dispatch_subject == "Map.remove" {
                                Some(crate::aver_generated::domain::builtins::builtinMapRemove(
                                    args,
                                ))
                            } else {
                                if &*__dispatch_subject == "Vector.new" {
                                    Some(crate::aver_generated::domain::builtins::vector::call(
                                        name, args,
                                    ))
                                } else {
                                    if &*__dispatch_subject == "Vector.get" {
                                        Some(crate::aver_generated::domain::builtins::vector::call(
                                            name, args,
                                        ))
                                    } else {
                                        if &*__dispatch_subject == "Vector.set" {
                                            Some(crate::aver_generated::domain::builtins::vector::call(name, args))
                                        } else {
                                            if &*__dispatch_subject == "Vector.len" {
                                                Some(crate::aver_generated::domain::builtins::vector::call(name, args))
                                            } else {
                                                if &*__dispatch_subject == "Vector.fromList" {
                                                    Some(crate::aver_generated::domain::builtins::vector::call(name, args))
                                                } else {
                                                    if &*__dispatch_subject == "List.fromVector" {
                                                        Some(crate::aver_generated::domain::builtins::vector::call(name, args))
                                                    } else {
                                                        if &*__dispatch_subject == "Option.None" {
                                                            Some(Ok(crate::aver_generated::domain::value::Val::ValNone))
                                                        } else {
                                                            if &*__dispatch_subject == "Option.Some"
                                                            {
                                                                Some(crate::aver_generated::domain::builtins::wrappers::call(name, args))
                                                            } else {
                                                                if &*__dispatch_subject
                                                                    == "Option.withDefault"
                                                                {
                                                                    Some(crate::aver_generated::domain::builtins::wrappers::call(name, args))
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "Result.Ok"
                                                                    {
                                                                        Some(crate::aver_generated::domain::builtins::wrappers::call(name, args))
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "Result.Err"
                                                                        {
                                                                            Some(crate::aver_generated::domain::builtins::wrappers::call(name, args))
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "Result.withDefault"
                                                                            {
                                                                                Some(crate::aver_generated::domain::builtins::wrappers::call(name, args))
                                                                            } else {
                                                                                if &*__dispatch_subject == "String.fromInt" { Some(crate::aver_generated::domain::builtins::primitives::callInt(name, args)) } else { None }
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

/// Synthesized indexed worker of `callBuiltinAfterList`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn callBuiltinAfterList__indexed(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut __str_index @ _: aver_rt::StringIndex,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if name.starts_with("Vector.") {
        crate::aver_generated::domain::builtins::vector::call(name, args)
    } else {
        if name.starts_with("Int.") {
            crate::aver_generated::domain::builtins::primitives::callInt(name, args)
        } else {
            if name.starts_with("String.") {
                crate::aver_generated::domain::builtins::primitives::callString(name, args)
            } else {
                if name.starts_with("Float.") {
                    crate::aver_generated::domain::builtins::primitives::callFloat(name, args)
                } else {
                    crate::aver_generated::domain::builtins::callBuiltinOther__indexed(
                        name,
                        args,
                        __str_index,
                    )
                }
            }
        }
    }
}

/// Synthesized indexed worker of `callBuiltinOther`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn callBuiltinOther__indexed(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut __str_index @ _: aver_rt::StringIndex,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Console.print" {
            crate::aver_generated::domain::builtins::builtinConsolePrint(args)
        } else {
            if &*__dispatch_subject == "Console.readLine" {
                crate::aver_generated::domain::builtins::builtinConsoleReadLine(args)
            } else {
                if &*__dispatch_subject == "Console.error" {
                    crate::aver_generated::domain::builtins::builtinConsoleError(args)
                } else {
                    if &*__dispatch_subject == "Console.warn" {
                        crate::aver_generated::domain::builtins::builtinConsoleWarn(args)
                    } else {
                        if &*__dispatch_subject == "Disk.readText" {
                            crate::aver_generated::domain::builtins::builtinDiskReadText(args)
                        } else {
                            if &*__dispatch_subject == "Disk.writeText" {
                                crate::aver_generated::domain::builtins::builtinDiskWriteText(args)
                            } else {
                                if &*__dispatch_subject == "Disk.exists" {
                                    crate::aver_generated::domain::builtins::builtinDiskExists(args)
                                } else {
                                    if &*__dispatch_subject == "Disk.listDir" {
                                        crate::aver_generated::domain::builtins::builtinDiskListDir(
                                            args,
                                        )
                                    } else {
                                        if &*__dispatch_subject == "Disk.appendText" {
                                            crate::aver_generated::domain::builtins::builtinDiskAppendText(args)
                                        } else {
                                            if &*__dispatch_subject == "Disk.delete" {
                                                crate::aver_generated::domain::builtins::builtinDiskDelete(args)
                                            } else {
                                                if &*__dispatch_subject == "Disk.deleteDir" {
                                                    crate::aver_generated::domain::builtins::builtinDiskDeleteDir(args)
                                                } else {
                                                    if &*__dispatch_subject == "Disk.makeDir" {
                                                        crate::aver_generated::domain::builtins::builtinDiskMakeDir(args)
                                                    } else {
                                                        if &*__dispatch_subject == "Env.get" {
                                                            crate::aver_generated::domain::builtins::builtinEnvGet(args)
                                                        } else {
                                                            if &*__dispatch_subject == "Env.set" {
                                                                crate::aver_generated::domain::builtins::builtinEnvSet(args)
                                                            } else {
                                                                if &*__dispatch_subject
                                                                    == "Args.get"
                                                                {
                                                                    crate::aver_generated::domain::builtins::builtinArgsGet(args)
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "Result.Ok"
                                                                    {
                                                                        crate::aver_generated::domain::builtins::wrappers::call(name, args)
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "Result.Err"
                                                                        {
                                                                            crate::aver_generated::domain::builtins::wrappers::call(name, args)
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "Result.withDefault"
                                                                            {
                                                                                crate::aver_generated::domain::builtins::wrappers::call(name, args)
                                                                            } else {
                                                                                if &*__dispatch_subject == "Option.Some" { crate::aver_generated::domain::builtins::wrappers::call(name, args) } else { if &*__dispatch_subject == "Option.None" { Ok(crate::aver_generated::domain::value::Val::ValNone) } else { if &*__dispatch_subject == "Option.withDefault" { crate::aver_generated::domain::builtins::wrappers::call(name, args) } else { if &*__dispatch_subject == "Result.fromOption" { crate::aver_generated::domain::builtins::wrappers::callResultFromOption(args) } else { if &*__dispatch_subject == "Bool.or" { crate::aver_generated::domain::builtins::builtinBoolOr(args) } else { if &*__dispatch_subject == "Bool.and" { crate::aver_generated::domain::builtins::builtinBoolAnd(args) } else { if &*__dispatch_subject == "Bool.not" { crate::aver_generated::domain::builtins::builtinBoolNot(args) } else { if &*__dispatch_subject == "Map.set" { crate::aver_generated::domain::builtins::builtinMapSet(args) } else { if &*__dispatch_subject == "Map.get" { crate::aver_generated::domain::builtins::builtinMapGet(args) } else { if &*__dispatch_subject == "Map.has" { crate::aver_generated::domain::builtins::builtinMapHas(args) } else { if &*__dispatch_subject == "Map.entries" { crate::aver_generated::domain::builtins::builtinMapEntries(args) } else { if &*__dispatch_subject == "Map.keys" { crate::aver_generated::domain::builtins::builtinMapKeys(args) } else { if &*__dispatch_subject == "Map.values" { crate::aver_generated::domain::builtins::builtinMapValues(args) } else { if &*__dispatch_subject == "Map.fromList" { crate::aver_generated::domain::builtins::builtinMapFromList(args) } else { if &*__dispatch_subject == "Map.size" { crate::aver_generated::domain::builtins::builtinMapSize(args) } else { if &*__dispatch_subject == "Map.len" { crate::aver_generated::domain::builtins::builtinMapSize(args) } else { if &*__dispatch_subject == "Map.remove" { crate::aver_generated::domain::builtins::builtinMapRemove(args) } else { crate::aver_generated::domain::builtins::callBuiltinServices__indexed(name, args, __str_index) } } } } } } } } } } } } } } } } }
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

/// Synthesized indexed worker of `callBuiltinServices`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn callBuiltinServices__indexed(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut __str_index @ _: aver_rt::StringIndex,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Random.int" {
            crate::aver_generated::domain::builtins::builtinRandomInt(args)
        } else {
            if &*__dispatch_subject == "Time.now" {
                crate::aver_generated::domain::builtins::builtinTimeNow(args)
            } else {
                if &*__dispatch_subject == "Time.sleep" {
                    crate::aver_generated::domain::builtins::builtinTimeSleep(args)
                } else {
                    if &*__dispatch_subject == "Time.unixMs" {
                        crate::aver_generated::domain::builtins::builtinTimeUnixMs(args)
                    } else {
                        if &*__dispatch_subject == "Http.get" {
                            crate::aver_generated::domain::builtins::builtinHttpSimple(
                                args,
                                ::aver_rt::str_lit!("get"),
                            )
                        } else {
                            if &*__dispatch_subject == "Http.head" {
                                crate::aver_generated::domain::builtins::builtinHttpSimple(
                                    args,
                                    ::aver_rt::str_lit!("head"),
                                )
                            } else {
                                if &*__dispatch_subject == "Http.delete" {
                                    crate::aver_generated::domain::builtins::builtinHttpSimple(
                                        args,
                                        ::aver_rt::str_lit!("delete"),
                                    )
                                } else {
                                    if &*__dispatch_subject == "Http.post" {
                                        crate::aver_generated::domain::builtins::builtinHttpBody(
                                            args,
                                            ::aver_rt::str_lit!("post"),
                                        )
                                    } else {
                                        if &*__dispatch_subject == "Http.put" {
                                            crate::aver_generated::domain::builtins::builtinHttpBody(
                                                args,
                                                ::aver_rt::str_lit!("put"),
                                            )
                                        } else {
                                            if &*__dispatch_subject == "Http.patch" {
                                                crate::aver_generated::domain::builtins::builtinHttpBody(args, ::aver_rt::str_lit!("patch"))
                                            } else {
                                                if &*__dispatch_subject == "Tcp.send" {
                                                    crate::aver_generated::domain::builtins::builtinTcpSend(args)
                                                } else {
                                                    if &*__dispatch_subject == "Tcp.ping" {
                                                        crate::aver_generated::domain::builtins::builtinTcpPing(args)
                                                    } else {
                                                        if &*__dispatch_subject == "Tcp.connect" {
                                                            crate::aver_generated::domain::builtins::builtinTcpConnect(args)
                                                        } else {
                                                            if &*__dispatch_subject
                                                                == "Tcp.beginConnect"
                                                            {
                                                                crate::aver_generated::domain::builtins::builtinTcpBeginConnect(args)
                                                            } else {
                                                                if &*__dispatch_subject
                                                                    == "Tcp.dialled"
                                                                {
                                                                    crate::aver_generated::domain::builtins::builtinTcpDialled(args)
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "Tcp.listen"
                                                                    {
                                                                        crate::aver_generated::domain::builtins::builtinTcpListen(args)
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "Tcp.accept"
                                                                        {
                                                                            crate::aver_generated::domain::builtins::builtinTcpAccept(args)
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "Tcp.peerAddress"
                                                                            {
                                                                                crate::aver_generated::domain::builtins::builtinTcpPeerAddress(args)
                                                                            } else {
                                                                                if &*__dispatch_subject == "Tcp.poll" { crate::aver_generated::domain::builtins::builtinTcpPoll(args) } else { if &*__dispatch_subject == "Tcp.writeLine" { crate::aver_generated::domain::builtins::builtinTcpWriteLine(args) } else { if &*__dispatch_subject == "Tcp.readLine" { crate::aver_generated::domain::builtins::builtinTcpReadLine(args) } else { if &*__dispatch_subject == "Tcp.close" { crate::aver_generated::domain::builtins::builtinTcpClose(args) } else { if &*__dispatch_subject == "Tcp.closeDial" { crate::aver_generated::domain::builtins::builtinTcpCloseDial(args) } else { if &*__dispatch_subject == "Tcp.closeListener" { crate::aver_generated::domain::builtins::builtinTcpCloseListener(args) } else { if &*__dispatch_subject == "Terminal.clear" { crate::aver_generated::domain::builtins::builtinTerminalNoArg__indexed(name, args, &__str_index) } else { if &*__dispatch_subject == "Terminal.flush" { crate::aver_generated::domain::builtins::builtinTerminalNoArg__indexed(name, args, &__str_index) } else { if &*__dispatch_subject == "Terminal.enableRawMode" { crate::aver_generated::domain::builtins::builtinTerminalNoArg__indexed(name, args, &__str_index) } else { if &*__dispatch_subject == "Terminal.disableRawMode" { crate::aver_generated::domain::builtins::builtinTerminalNoArg__indexed(name, args, &__str_index) } else { if &*__dispatch_subject == "Terminal.hideCursor" { crate::aver_generated::domain::builtins::builtinTerminalNoArg__indexed(name, args, &__str_index) } else { if &*__dispatch_subject == "Terminal.showCursor" { crate::aver_generated::domain::builtins::builtinTerminalNoArg__indexed(name, args, &__str_index) } else { if &*__dispatch_subject == "Terminal.resetColor" { crate::aver_generated::domain::builtins::builtinTerminalNoArg__indexed(name, args, &__str_index) } else { if &*__dispatch_subject == "Terminal.readKey" { crate::aver_generated::domain::builtins::builtinTerminalReadKey(args) } else { if &*__dispatch_subject == "Terminal.size" { crate::aver_generated::domain::builtins::builtinTerminalSize(args) } else { if &*__dispatch_subject == "Terminal.print" { crate::aver_generated::domain::builtins::builtinTerminalPrint(args) } else { if &*__dispatch_subject == "Terminal.setColor" { crate::aver_generated::domain::builtins::builtinTerminalSetColor(args) } else { if &*__dispatch_subject == "Terminal.moveTo" { crate::aver_generated::domain::builtins::builtinTerminalMoveTo(args) } else { crate::aver_generated::domain::builtins::tryVariantConstructor__indexed(name, args, __str_index) } } } } } } } } } } } } } } } } } }
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

/// Synthesized indexed worker of `builtinTerminalNoArg`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn builtinTerminalNoArg__indexed(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    __str_index @ _: &aver_rt::StringIndex,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Terminal.clear" {
            crate::aver_generated::domain::builtins::termClear()
        } else {
            if &*__dispatch_subject == "Terminal.flush" {
                crate::aver_generated::domain::builtins::termFlush()
            } else {
                if &*__dispatch_subject == "Terminal.enableRawMode" {
                    crate::aver_generated::domain::builtins::termEnableRawMode()
                } else {
                    if &*__dispatch_subject == "Terminal.disableRawMode" {
                        crate::aver_generated::domain::builtins::termDisableRawMode()
                    } else {
                        if &*__dispatch_subject == "Terminal.hideCursor" {
                            crate::aver_generated::domain::builtins::termHideCursor()
                        } else {
                            if &*__dispatch_subject == "Terminal.showCursor" {
                                crate::aver_generated::domain::builtins::termShowCursor()
                            } else {
                                if &*__dispatch_subject == "Terminal.resetColor" {
                                    crate::aver_generated::domain::builtins::termResetColor()
                                } else {
                                    Err(aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(42))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&::aver_rt::str_lit!(
                                                "unknown terminal command: "
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

/// Synthesized indexed worker of `tryVariantConstructor`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tryVariantConstructor__indexed(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut __str_index @ _: aver_rt::StringIndex,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::builtins::splitDotted__indexed(name.clone(), __str_index) {
        Some(_) => Ok(crate::aver_generated::domain::value::Val::ValVariant(
            crate::aver_generated::domain::value::Val__ValVariant(
                crate::aver_generated::domain::ast::ctorNameToTag(name.clone()),
                name,
                args.clone(),
            ),
        )),
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("undefined function: "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
            __b
        })),
    }
}

/// Synthesized indexed worker of `splitDotted`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn splitDotted__indexed(
    name @ _: AverStr,
    mut __str_index @ _: aver_rt::StringIndex,
) -> Option<(AverStr, AverStr)> {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::AverInt::from_i64(name.chars().count() as i64);
    crate::aver_generated::domain::builtins::splitDottedLoop__indexed(
        name,
        aver_rt::AverInt::from_i64(0),
        __arg2,
        __str_index,
    )
}

/// Synthesized indexed worker of `splitDottedLoop`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn splitDottedLoop__indexed(
    mut name @ _: AverStr,
    mut pos @ _: aver_rt::AverInt,
    mut total @ _: aver_rt::AverInt,
    mut __str_index @ _: aver_rt::StringIndex,
) -> Option<(AverStr, AverStr)> {
    loop {
        crate::cancel_checkpoint();
        if (pos < total) {
            match aver_rt::string_index_char_at_i64(
                &name,
                &__str_index,
                aver_rt::string_position(&pos),
            ) {
                Some(c @ _) => {
                    if (&*c == ".") {
                        return Some((
                            aver_rt::string_index_slice_i64(
                                &name,
                                &__str_index,
                                0i64,
                                aver_rt::string_position(&pos),
                            ),
                            aver_rt::string_index_slice_i64(
                                &name,
                                &__str_index,
                                aver_rt::string_position(&pos.add(&aver_rt::AverInt::from_i64(1))),
                                aver_rt::string_position(&total),
                            ),
                        ));
                    } else {
                        {
                            let __tco1 = pos.add(&aver_rt::AverInt::from_i64(1));
                            pos = __tco1;
                            continue;
                        }
                    }
                }
                None => {
                    return None;
                }
            }
        } else {
            return None;
        }
    }
}

/// Synthesized collecting variant of `stringsToVals`. Appends to a builder where `stringsToVals` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn stringsToVals__collected(
    strs @ _: &aver_rt::AverList<AverStr>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut strs @ _: &aver_rt::AverList<AverStr> = strs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = strs;
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
                        strs = &__hold0;
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

/// Synthesized collecting variant of `mapEntriesToTuples`. Appends to a builder where `mapEntriesToTuples` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn mapEntriesToTuples__collected(
    mut entries @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(entries, [] => { return aver_rt::list_builder_finalize(acc); }, [pair, rest] => { { let (k, v) = pair; {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::value::Val::ValTuple(crate::aver_generated::domain::value::Val__ValTuple(aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::value::Val::ValStr(crate::aver_generated::domain::value::Val__ValStr(k)), v]))));
            entries = __tco0;
            acc = __tco1;
            continue;
        } } })
    }
}

/// Synthesized collecting variant of `stringsToValStrs`. Appends to a builder where `stringsToValStrs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn stringsToValStrs__collected(
    values @ _: &aver_rt::AverList<AverStr>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut values @ _: &aver_rt::AverList<AverStr> = values;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = values;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let Some((v, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::list_builder_push(
                            acc,
                            crate::aver_generated::domain::value::Val::ValStr(
                                crate::aver_generated::domain::value::Val__ValStr(v),
                            ),
                        );
                        __hold0 = __tco0;
                        values = &__hold0;
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

/// Synthesized collecting variant of `tcpReadyKeysToVals`. Appends to a builder where `tcpReadyKeysToVals` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn tcpReadyKeysToVals__collected(
    keys @ _: &aver_rt::AverIntList,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    let mut __hold0: aver_rt::AverIntList;
    let mut keys @ _: &aver_rt::AverIntList = keys;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = keys;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let Some((key, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::list_builder_push(
                            acc,
                            crate::aver_generated::domain::value::Val::ValInt(
                                crate::aver_generated::domain::value::Val__ValInt(key),
                            ),
                        );
                        __hold0 = __tco0;
                        keys = &__hold0;
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

pub mod endian;

pub mod helpers;

pub mod list;

pub mod primitives;

pub mod vector;

pub mod wrappers;
