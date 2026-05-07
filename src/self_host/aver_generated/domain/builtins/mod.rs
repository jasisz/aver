#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::helpers::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::list::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::primitives::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::vector::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::wrappers::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    TuplesToMap(aver_rt::AverList<Val>, aver_rt::AverMap<AverStr, Val>),
    TuplesToMapOne(
        aver_rt::AverList<Val>,
        aver_rt::AverList<Val>,
        aver_rt::AverMap<AverStr, Val>,
    ),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> aver_rt::AverMap<AverStr, Val> {
    loop {
        __state = match __state {
            __MutualTco1::TuplesToMap(mut items, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(items, [] => return acc, [item, rest] => match item {
                Val::ValTuple(parts) => __MutualTco1::TuplesToMapOne(parts, rest, acc),
                _ => __MutualTco1::TuplesToMap(rest, acc)
                })
            }
            __MutualTco1::TuplesToMapOne(mut parts, mut rest, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(parts, [] => __MutualTco1::TuplesToMap(rest, acc), [kV, tail] => aver_list_match!(tail, [] => __MutualTco1::TuplesToMap(rest, acc), [vV, ignored] => __MutualTco1::TuplesToMap(rest, acc.insert_owned(crate::aver_generated::domain::value::mapKeyRepr(&kV), vV))))
            }
        };
    }
}

/// Convert list of (key, value) tuples to a Map.
pub fn tuplesToMap(
    items: &aver_rt::AverList<Val>,
    acc: &aver_rt::AverMap<AverStr, Val>,
) -> aver_rt::AverMap<AverStr, Val> {
    __mutual_tco_trampoline_1(__MutualTco1::TuplesToMap(items.clone(), acc.clone()))
}

/// Extract key-value from tuple parts.
pub fn tuplesToMapOne(
    parts: &aver_rt::AverList<Val>,
    rest: &aver_rt::AverList<Val>,
    acc: &aver_rt::AverMap<AverStr, Val>,
) -> aver_rt::AverMap<AverStr, Val> {
    __mutual_tco_trampoline_1(__MutualTco1::TuplesToMapOne(
        parts.clone(),
        rest.clone(),
        acc.clone(),
    ))
}

/// Dispatch qualified builtin calls to sub-module implementations.
pub fn callBuiltin(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match callBuiltinFast(name.clone(), args) {
        Some(result) => result,
        None => {
            if name.starts_with("List.") {
                crate::aver_generated::domain::builtins::list::call(name, args)
            } else {
                callBuiltinAfterList(name, args)
            }
        }
    }
}

/// Fast exact-match dispatch for the hottest builtins.
#[inline(always)]
pub fn callBuiltinFast(
    name: AverStr,
    args: &aver_rt::AverList<Val>,
) -> Option<Result<Val, AverStr>> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Map.set" {
            Some(builtinMapSet(args))
        } else {
            if &*__dispatch_subject == "Map.get" {
                Some(builtinMapGet(args))
            } else {
                if &*__dispatch_subject == "Map.has" {
                    Some(builtinMapHas(args))
                } else {
                    if &*__dispatch_subject == "Map.fromList" {
                        Some(builtinMapFromList(args))
                    } else {
                        if &*__dispatch_subject == "Map.entries" {
                            Some(builtinMapEntries(args))
                        } else {
                            if &*__dispatch_subject == "Map.remove" {
                                Some(builtinMapRemove(args))
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
                                                            Some(Ok(Val::ValNone.clone()))
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

/// Dispatch pre-evaluated args by integer builtin ID. IDs assigned in Ast.builtinNameToId.
#[inline(always)]
pub fn callBuiltinByIdValues(id: i64, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = id;
        if __dispatch_subject == 1i64 {
            builtinMapSet(args)
        } else {
            if __dispatch_subject == 2i64 {
                builtinMapGet(args)
            } else {
                if __dispatch_subject == 3i64 {
                    builtinMapHas(args)
                } else {
                    if __dispatch_subject == 4i64 {
                        builtinMapFromList(args)
                    } else {
                        if __dispatch_subject == 5i64 {
                            builtinMapEntries(args)
                        } else {
                            if __dispatch_subject == 6i64 {
                                builtinMapRemove(args)
                            } else {
                                if __dispatch_subject == 7i64 {
                                    crate::aver_generated::domain::builtins::vector::call(
                                        AverStr::from("Vector.new"),
                                        args,
                                    )
                                } else {
                                    if __dispatch_subject == 8i64 {
                                        crate::aver_generated::domain::builtins::vector::call(
                                            AverStr::from("Vector.get"),
                                            args,
                                        )
                                    } else {
                                        if __dispatch_subject == 9i64 {
                                            crate::aver_generated::domain::builtins::vector::call(
                                                AverStr::from("Vector.set"),
                                                args,
                                            )
                                        } else {
                                            if __dispatch_subject == 10i64 {
                                                crate::aver_generated::domain::builtins::vector::call(AverStr::from("Vector.len"), args)
                                            } else {
                                                if __dispatch_subject == 11i64 {
                                                    crate::aver_generated::domain::builtins::vector::call(AverStr::from("Vector.fromList"), args)
                                                } else {
                                                    if __dispatch_subject == 12i64 {
                                                        crate::aver_generated::domain::builtins::vector::call(AverStr::from("List.fromVector"), args)
                                                    } else {
                                                        if __dispatch_subject == 13i64 {
                                                            Ok(Val::ValNone.clone())
                                                        } else {
                                                            if __dispatch_subject == 14i64 {
                                                                crate::aver_generated::domain::builtins::wrappers::call(AverStr::from("Option.Some"), args)
                                                            } else {
                                                                if __dispatch_subject == 15i64 {
                                                                    crate::aver_generated::domain::builtins::wrappers::call(AverStr::from("Option.withDefault"), args)
                                                                } else {
                                                                    if __dispatch_subject == 16i64 {
                                                                        crate::aver_generated::domain::builtins::wrappers::call(AverStr::from("Result.Ok"), args)
                                                                    } else {
                                                                        if __dispatch_subject
                                                                            == 17i64
                                                                        {
                                                                            crate::aver_generated::domain::builtins::wrappers::call(AverStr::from("Result.Err"), args)
                                                                        } else {
                                                                            if __dispatch_subject
                                                                                == 18i64
                                                                            {
                                                                                crate::aver_generated::domain::builtins::wrappers::call(AverStr::from("Result.withDefault"), args)
                                                                            } else {
                                                                                if __dispatch_subject == 19i64 { crate::aver_generated::domain::builtins::primitives::callInt(AverStr::from("String.fromInt"), args) } else { if __dispatch_subject == 20i64 { crate::aver_generated::domain::builtins::list::call(AverStr::from("List.take"), args) } else { if __dispatch_subject == 21i64 { crate::aver_generated::domain::builtins::list::call(AverStr::from("List.drop"), args) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((36i64) as usize); __b.push_str(&AverStr::from("Unknown builtin ID: ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&((id.to_string()).into_aver())))); __b })) } } }
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

/// Continue dispatch after List.
pub fn callBuiltinAfterList(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
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
                    callBuiltinOther(name, args)
                }
            }
        }
    }
}

/// Handle non-prefixed builtins: Result.*, Option.*, Bool.*, Map.*, services, Char, variant constructors.
pub fn callBuiltinOther(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Char.fromCode" {
            crate::aver_generated::domain::builtins::primitives::callChar(name, args)
        } else {
            if &*__dispatch_subject == "Char.toCode" {
                crate::aver_generated::domain::builtins::primitives::callChar(name, args)
            } else {
                if &*__dispatch_subject == "Console.print" {
                    builtinConsolePrint(args)
                } else {
                    if &*__dispatch_subject == "Console.readLine" {
                        builtinConsoleReadLine(args)
                    } else {
                        if &*__dispatch_subject == "Console.error" {
                            builtinConsoleError(args)
                        } else {
                            if &*__dispatch_subject == "Console.warn" {
                                builtinConsoleWarn(args)
                            } else {
                                if &*__dispatch_subject == "Disk.readText" {
                                    builtinDiskReadText(args)
                                } else {
                                    if &*__dispatch_subject == "Disk.writeText" {
                                        builtinDiskWriteText(args)
                                    } else {
                                        if &*__dispatch_subject == "Disk.exists" {
                                            builtinDiskExists(args)
                                        } else {
                                            if &*__dispatch_subject == "Disk.listDir" {
                                                builtinDiskListDir(args)
                                            } else {
                                                if &*__dispatch_subject == "Disk.appendText" {
                                                    builtinDiskAppendText(args)
                                                } else {
                                                    if &*__dispatch_subject == "Disk.delete" {
                                                        builtinDiskDelete(args)
                                                    } else {
                                                        if &*__dispatch_subject == "Disk.deleteDir"
                                                        {
                                                            builtinDiskDeleteDir(args)
                                                        } else {
                                                            if &*__dispatch_subject
                                                                == "Disk.makeDir"
                                                            {
                                                                builtinDiskMakeDir(args)
                                                            } else {
                                                                if &*__dispatch_subject == "Env.get"
                                                                {
                                                                    builtinEnvGet(args)
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "Env.set"
                                                                    {
                                                                        builtinEnvSet(args)
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "Args.get"
                                                                        {
                                                                            builtinArgsGet(args)
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "Result.Ok"
                                                                            {
                                                                                crate::aver_generated::domain::builtins::wrappers::call(name, args)
                                                                            } else {
                                                                                if &*__dispatch_subject == "Result.Err" { crate::aver_generated::domain::builtins::wrappers::call(name, args) } else { if &*__dispatch_subject == "Result.withDefault" { crate::aver_generated::domain::builtins::wrappers::call(name, args) } else { if &*__dispatch_subject == "Option.Some" { crate::aver_generated::domain::builtins::wrappers::call(name, args) } else { if &*__dispatch_subject == "Option.None" { Ok(Val::ValNone.clone()) } else { if &*__dispatch_subject == "Option.withDefault" { crate::aver_generated::domain::builtins::wrappers::call(name, args) } else { if &*__dispatch_subject == "Option.toResult" { crate::aver_generated::domain::builtins::wrappers::callOptionToResult(args) } else { if &*__dispatch_subject == "Bool.or" { builtinBoolOr(args) } else { if &*__dispatch_subject == "Bool.and" { builtinBoolAnd(args) } else { if &*__dispatch_subject == "Bool.not" { builtinBoolNot(args) } else { if &*__dispatch_subject == "Map.set" { builtinMapSet(args) } else { if &*__dispatch_subject == "Map.get" { builtinMapGet(args) } else { if &*__dispatch_subject == "Map.has" { builtinMapHas(args) } else { if &*__dispatch_subject == "Map.entries" { builtinMapEntries(args) } else { if &*__dispatch_subject == "Map.keys" { builtinMapKeys(args) } else { if &*__dispatch_subject == "Map.values" { builtinMapValues(args) } else { if &*__dispatch_subject == "Map.fromList" { builtinMapFromList(args) } else { if &*__dispatch_subject == "Map.size" { builtinMapSize(args) } else { if &*__dispatch_subject == "Map.len" { builtinMapSize(args) } else { if &*__dispatch_subject == "Map.remove" { builtinMapRemove(args) } else { callBuiltinServices(name, args) } } } } } } } } } } } } } } } } } } }
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

/// Console.print(v) -> print any value and return Unit.
pub fn builtinConsolePrint(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __effect_arg0 = crate::aver_generated::domain::value::valRepr(&v);
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.print",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_print(&__effect_arg0),
        )
    };
    Ok(Val::ValUnit.clone())
}

/// Console.error(v) -> print to stderr.
pub fn builtinConsoleError(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __effect_arg0 = crate::aver_generated::domain::value::valRepr(&v);
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.error",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_error(&__effect_arg0),
        )
    };
    Ok(Val::ValUnit.clone())
}

/// Console.warn(v) -> print warning.
pub fn builtinConsoleWarn(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __effect_arg0 = crate::aver_generated::domain::value::valRepr(&v);
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.warn",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_warn(&__effect_arg0),
        )
    };
    Ok(Val::ValUnit.clone())
}

/// Console.readLine() -> Result<String, String>.
pub fn builtinConsoleReadLine(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Console.readLine", vec![], || {
            (aver_rt::read_line()).into_aver()
        })
    } {
        Ok(line) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValStr(line)))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Disk.readText(path) -> Result string.
pub fn builtinDiskReadText(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __effect_arg0 = path;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Disk.readText",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || (aver_rt::read_text(&__effect_arg0)).into_aver(),
        )
    } {
        Ok(content) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValStr(content)))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Args.get() -> list of string args.
pub fn builtinArgsGet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let rawArgs = {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Args.get", vec![], || aver_replay::current_cli_args())
    };
    Ok(Val::ValList(stringsToVals(
        rawArgs,
        aver_rt::AverList::empty(),
    )))
}

/// Env.get(key) -> Option<String>.
pub fn builtinEnvGet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let key = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __effect_arg0 = key;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Env.get",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || (aver_rt::env_get(&__effect_arg0)).into_aver(),
        )
    } {
        Some(value) => Ok(Val::ValSome(std::sync::Arc::new(Val::ValStr(value)))),
        None => Ok(Val::ValNone.clone()),
    }
}

/// Env.set(key, value) -> Unit.
pub fn builtinEnvSet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (keyV, valueV) = pair;
        builtinEnvSetInner(&keyV, &valueV)
    }
}

/// Inner Env.set.
pub fn builtinEnvSetInner(keyV: &Val, valueV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let key = crate::aver_generated::domain::builtins::helpers::expectStr(keyV)?;
    let value = crate::aver_generated::domain::builtins::helpers::expectStr(valueV)?;
    {
        let __effect_arg0 = key;
        let __effect_arg1 = value;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Env.set",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
            ],
            || aver_rt::env_set(&__effect_arg0, &__effect_arg1).expect("Env.set failed"),
        )
    };
    Ok(Val::ValUnit.clone())
}

/// Convert list of strings to list of ValStr.
#[inline(always)]
pub fn stringsToVals(
    mut strs: aver_rt::AverList<AverStr>,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(strs, [] => acc.reverse(), [s, rest] => { {
            let __tmp1 = aver_rt::AverList::prepend(Val::ValStr(s), &acc);
            strs = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Map.entries(map) -> List of tuples.
pub fn builtinMapEntries(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValMap(m) => Ok(Val::ValList(mapEntriesToTuples(
            {
                let mut es: Vec<_> = m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                es.sort_by(|a, b| a.0.cmp(&b.0));
                aver_rt::AverList::from_vec(es)
            },
            aver_rt::AverList::empty(),
        ))),
        _ => Err(AverStr::from("Map.entries requires a Map")),
    }
}

/// Convert entries to list of (key, value) tuples.
#[inline(always)]
pub fn mapEntriesToTuples(
    mut entries: aver_rt::AverList<(AverStr, Val)>,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(entries, [] => acc.reverse(), [pair, rest] => { match pair {
            (k, v) => {
            let __tmp1 = aver_rt::AverList::prepend(Val::ValTuple(aver_rt::AverList::from_vec(vec![Val::ValStr(k), v])), &acc);
            entries = rest;
            acc = __tmp1;
            continue;
        }
        } });
    }
}

/// Map.keys(map) -> List of keys.
pub fn builtinMapKeys(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValMap(m) => Ok(Val::ValList(stringsToVals(
            {
                let mut ks: Vec<_> = m.keys().cloned().collect();
                ks.sort();
                aver_rt::AverList::from_vec(ks)
            },
            aver_rt::AverList::empty(),
        ))),
        _ => Err(AverStr::from("Map.keys requires a Map")),
    }
}

/// Map.values(map) -> List of values.
pub fn builtinMapValues(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValMap(m) => Ok(Val::ValList(aver_rt::AverList::from_vec(
            m.values().cloned().collect::<Vec<_>>(),
        ))),
        _ => Err(AverStr::from("Map.values requires a Map")),
    }
}

/// Map.fromList(entries) -> Map.
pub fn builtinMapFromList(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    Ok(Val::ValMap(tuplesToMap(&items, &HashMap::new())))
}

/// Map.size(map) -> Int.
pub fn builtinMapSize(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValMap(m) => Ok(Val::ValInt((m.len() as i64))),
        _ => Err(AverStr::from("Map.size requires a Map")),
    }
}

/// Map.remove(map, key) -> Map.
pub fn builtinMapRemove(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (mapV, keyV) = pair;
        builtinMapRemoveInner(&mapV, &keyV)
    }
}

/// Inner Map.remove.
pub fn builtinMapRemoveInner(mapV: &Val, keyV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        Val::ValMap(m) => Ok(Val::ValMap(
            m.remove_owned(&crate::aver_generated::domain::value::mapKeyRepr(keyV)),
        )),
        _ => Err(AverStr::from("Map.remove requires a Map")),
    }
}

/// Dispatch Terminal.*, Time.*, Random.*, Http.*, Tcp.* service builtins.
pub fn callBuiltinServices(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Random.int" {
            builtinRandomInt(args)
        } else {
            if &*__dispatch_subject == "Time.now" {
                builtinTimeNow(args)
            } else {
                if &*__dispatch_subject == "Time.sleep" {
                    builtinTimeSleep(args)
                } else {
                    if &*__dispatch_subject == "Time.unixMs" {
                        builtinTimeUnixMs(args)
                    } else {
                        if &*__dispatch_subject == "Http.get" {
                            builtinHttpSimple(args, AverStr::from("get"))
                        } else {
                            if &*__dispatch_subject == "Http.head" {
                                builtinHttpSimple(args, AverStr::from("head"))
                            } else {
                                if &*__dispatch_subject == "Http.delete" {
                                    builtinHttpSimple(args, AverStr::from("delete"))
                                } else {
                                    if &*__dispatch_subject == "Http.post" {
                                        builtinHttpBody(args, AverStr::from("post"))
                                    } else {
                                        if &*__dispatch_subject == "Http.put" {
                                            builtinHttpBody(args, AverStr::from("put"))
                                        } else {
                                            if &*__dispatch_subject == "Http.patch" {
                                                builtinHttpBody(args, AverStr::from("patch"))
                                            } else {
                                                if &*__dispatch_subject == "HttpServer.listen" {
                                                    builtinHttpServerListen(args)
                                                } else {
                                                    if &*__dispatch_subject
                                                        == "HttpServer.listenWith"
                                                    {
                                                        builtinHttpServerListenWith(args)
                                                    } else {
                                                        if &*__dispatch_subject == "Tcp.send" {
                                                            builtinTcpSend(args)
                                                        } else {
                                                            if &*__dispatch_subject == "Tcp.ping" {
                                                                builtinTcpPing(args)
                                                            } else {
                                                                if &*__dispatch_subject
                                                                    == "Tcp.connect"
                                                                {
                                                                    builtinTcpConnect(args)
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "Tcp.writeLine"
                                                                    {
                                                                        builtinTcpWriteLine(args)
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "Tcp.readLine"
                                                                        {
                                                                            builtinTcpReadLine(args)
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "Tcp.close"
                                                                            {
                                                                                builtinTcpClose(
                                                                                    args,
                                                                                )
                                                                            } else {
                                                                                if &*__dispatch_subject == "Terminal.clear" { builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.flush" { builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.enableRawMode" { builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.disableRawMode" { builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.hideCursor" { builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.showCursor" { builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.resetColor" { builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.readKey" { builtinTerminalReadKey(args) } else { if &*__dispatch_subject == "Terminal.size" { builtinTerminalSize(args) } else { if &*__dispatch_subject == "Terminal.print" { builtinTerminalPrint(args) } else { if &*__dispatch_subject == "Terminal.setColor" { builtinTerminalSetColor(args) } else { if &*__dispatch_subject == "Terminal.moveTo" { builtinTerminalMoveTo(args) } else { tryVariantConstructor(name, args) } } } } } } } } } } } }
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

/// Fail explicitly for host services that still need callback/runtime bridging in the self-host.
#[inline(always)]
pub fn builtinUnsupportedHostService(name: AverStr) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    Err(aver_rt::AverStr::from({
        let mut __b = {
            let mut __b = aver_rt::Buffer::with_capacity((68i64) as usize);
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
            __b
        };
        __b.push_str(&AverStr::from(
            " is not supported in the self-hosted interpreter yet",
        ));
        __b
    }))
}

/// HttpServer.listen(port, handler) through the generated self-host runtime bridge.
pub fn builtinHttpServerListen(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (portV, handlerV) = pair;
        builtinHttpServerListenInner(&portV, &handlerV)
    }
}

/// Inner HttpServer.listen bridge.
pub fn builtinHttpServerListenInner(portV: &Val, handlerV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let port = crate::aver_generated::domain::builtins::helpers::expectInt(portV)?;
    match {
        let __effect_arg0 = port;
        let __effect_arg1 = handlerV.clone();
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "HttpServer.listen",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                serde_json::Value::String("<handler>".to_string()),
            ],
            || crate::self_host_support::http_server_listen(__effect_arg0, __effect_arg1),
        )
    } {
        Ok(_) => Ok(Val::ValUnit.clone()),
        Err(e) => Err(e),
    }
}

/// HttpServer.listenWith(port, context, handler) through the generated self-host runtime bridge.
pub fn builtinHttpServerListenWith(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((portV, rest1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest1;
                if let Some((contextV, rest2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = rest2;
                        if let Some((handlerV, ignored)) =
                            aver_rt::list_uncons_cloned(&__list_subject)
                        {
                            builtinHttpServerListenWithInner(&portV, &contextV, &handlerV)
                        } else {
                            Err(AverStr::from("HttpServer.listenWith takes 3 arguments"))
                        }
                    }
                } else {
                    Err(AverStr::from("HttpServer.listenWith takes 3 arguments"))
                }
            }
        } else {
            Err(AverStr::from("HttpServer.listenWith takes 3 arguments"))
        }
    }
}

/// Inner HttpServer.listenWith bridge.
pub fn builtinHttpServerListenWithInner(
    portV: &Val,
    contextV: &Val,
    handlerV: &Val,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let port = crate::aver_generated::domain::builtins::helpers::expectInt(portV)?;
    match {
        let __effect_arg0 = port;
        let __effect_arg1 = contextV.clone();
        let __effect_arg2 = handlerV.clone();
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "HttpServer.listenWith",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
                serde_json::Value::String("<handler>".to_string()),
            ],
            || {
                crate::self_host_support::http_server_listen_with(
                    __effect_arg0,
                    __effect_arg1.clone(),
                    __effect_arg2,
                )
            },
        )
    } {
        Ok(_) => Ok(Val::ValUnit.clone()),
        Err(e) => Err(e),
    }
}

/// Random.int(min, max) -> random integer.
pub fn builtinRandomInt(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (minV, maxV) = pair;
        builtinRandomIntInner(&minV, &maxV)
    }
}

/// Inner Random.int.
pub fn builtinRandomIntInner(minV: &Val, maxV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let minN = crate::aver_generated::domain::builtins::helpers::expectInt(minV)?;
    let maxN = crate::aver_generated::domain::builtins::helpers::expectInt(maxV)?;
    Ok(Val::ValInt({
        let __effect_arg0 = minN;
        let __effect_arg1 = maxN;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Random.int",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
            ],
            || aver_rt::random::random_int(__effect_arg0, __effect_arg1).unwrap(),
        )
    }))
}

/// Time.sleep(ms) -> Unit.
pub fn builtinTimeSleep(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let ms = crate::aver_generated::domain::builtins::helpers::expectInt(&v)?;
    {
        let __effect_arg0 = ms;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Time.sleep",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || aver_rt::time_sleep(__effect_arg0),
        )
    };
    Ok(Val::ValUnit.clone())
}

/// Time.unixMs() -> Int.
pub fn builtinTimeUnixMs(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    Ok(Val::ValInt({
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Time.unixMs", vec![], || aver_rt::time_unix_ms())
    }))
}

/// Terminal no-arg commands: clear, flush, enableRawMode, etc.
pub fn builtinTerminalNoArg(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Terminal.clear" {
            termClear()
        } else {
            if &*__dispatch_subject == "Terminal.flush" {
                termFlush()
            } else {
                if &*__dispatch_subject == "Terminal.enableRawMode" {
                    termEnableRawMode()
                } else {
                    if &*__dispatch_subject == "Terminal.disableRawMode" {
                        termDisableRawMode()
                    } else {
                        if &*__dispatch_subject == "Terminal.hideCursor" {
                            termHideCursor()
                        } else {
                            if &*__dispatch_subject == "Terminal.showCursor" {
                                termShowCursor()
                            } else {
                                if &*__dispatch_subject == "Terminal.resetColor" {
                                    termResetColor()
                                } else {
                                    Err(aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b =
                                                aver_rt::Buffer::with_capacity((42i64) as usize);
                                            __b.push_str(&AverStr::from(
                                                "unknown terminal command: ",
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

/// Clear the terminal screen.
pub fn termClear() -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Terminal.clear", vec![], || {
            aver_rt::terminal_clear().unwrap()
        })
    };
    Ok(Val::ValUnit.clone())
}

/// Flush terminal output.
pub fn termFlush() -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Terminal.flush", vec![], || {
            aver_rt::terminal_flush().unwrap()
        })
    };
    Ok(Val::ValUnit.clone())
}

/// Enable terminal raw mode.
pub fn termEnableRawMode() -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Terminal.enableRawMode", vec![], || {
            aver_rt::terminal_enable_raw_mode().unwrap()
        })
    };
    Ok(Val::ValUnit.clone())
}

/// Disable terminal raw mode.
pub fn termDisableRawMode() -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Terminal.disableRawMode", vec![], || {
            aver_rt::terminal_disable_raw_mode().unwrap()
        })
    };
    Ok(Val::ValUnit.clone())
}

/// Hide terminal cursor.
pub fn termHideCursor() -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Terminal.hideCursor", vec![], || {
            aver_rt::terminal_hide_cursor().unwrap()
        })
    };
    Ok(Val::ValUnit.clone())
}

/// Show terminal cursor.
pub fn termShowCursor() -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Terminal.showCursor", vec![], || {
            aver_rt::terminal_show_cursor().unwrap()
        })
    };
    Ok(Val::ValUnit.clone())
}

/// Reset terminal color.
pub fn termResetColor() -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Terminal.resetColor", vec![], || {
            aver_rt::terminal_reset_color().unwrap()
        })
    };
    Ok(Val::ValUnit.clone())
}

/// Terminal.readKey() -> Option<String>.
pub fn builtinTerminalReadKey(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Terminal.readKey", vec![], || {
            (aver_rt::terminal_read_key()).into_aver()
        })
    } {
        Some(k) => Ok(Val::ValSome(std::sync::Arc::new(Val::ValStr(k)))),
        None => Ok(Val::ValNone.clone()),
    }
}

/// Terminal.size() -> Record with width and height.
pub fn builtinTerminalSize(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let sz = {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Terminal.size", vec![], || {
            let (w, h) = aver_rt::terminal_size().unwrap();
            aver_rt::TerminalSize {
                width: w,
                height: h,
            }
        })
    };
    Ok(Val::ValRecord(
        AverStr::from("TerminalSize"),
        aver_rt::AverList::from_vec(vec![
            (AverStr::from("width"), Val::ValInt(sz.width.clone())),
            (AverStr::from("height"), Val::ValInt(sz.height.clone())),
        ]),
    ))
}

/// Terminal.print(s) -> print string to terminal.
pub fn builtinTerminalPrint(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v.clone() {
        Val::ValStr(s) => termPrintStr(s),
        _ => termPrintStr(crate::aver_generated::domain::value::valRepr(&v)),
    }
}

/// Print string to terminal.
pub fn termPrintStr(s: AverStr) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __effect_arg0 = s;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Terminal.print",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || {
                let __s = format!("{}", __effect_arg0);
                aver_rt::terminal_print(&__s).unwrap()
            },
        )
    };
    Ok(Val::ValUnit.clone())
}

/// Terminal.setColor(color) -> Unit.
pub fn builtinTerminalSetColor(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let s = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    {
        let __effect_arg0 = s;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Terminal.setColor",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || aver_rt::terminal_set_color(&__effect_arg0).unwrap(),
        )
    };
    Ok(Val::ValUnit.clone())
}

/// Terminal.moveTo(x, y) -> Unit.
pub fn builtinTerminalMoveTo(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (xV, yV) = pair;
        builtinTerminalMoveToInner(&xV, &yV)
    }
}

/// Inner Terminal.moveTo.
pub fn builtinTerminalMoveToInner(xV: &Val, yV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let x = crate::aver_generated::domain::builtins::helpers::expectInt(xV)?;
    let y = crate::aver_generated::domain::builtins::helpers::expectInt(yV)?;
    {
        let __effect_arg0 = x;
        let __effect_arg1 = y;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Terminal.moveTo",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
            ],
            || aver_rt::terminal_move_to(__effect_arg0, __effect_arg1).unwrap(),
        )
    };
    Ok(Val::ValUnit.clone())
}

/// Disk.writeText(path, content) -> Result.
pub fn builtinDiskWriteText(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (pathV, contentV) = pair;
        builtinDiskWriteTextInner(&pathV, &contentV)
    }
}

/// Inner Disk.writeText.
pub fn builtinDiskWriteTextInner(pathV: &Val, contentV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let path = crate::aver_generated::domain::builtins::helpers::expectStr(pathV)?;
    let content = crate::aver_generated::domain::builtins::helpers::expectStr(contentV)?;
    match {
        let __effect_arg0 = path;
        let __effect_arg1 = content;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Disk.writeText",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
            ],
            || (aver_rt::write_text(&__effect_arg0, &__effect_arg1)).into_aver(),
        )
    } {
        Ok(_) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValUnit.clone()))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Disk.appendText(path, content) -> Result.
pub fn builtinDiskAppendText(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (pathV, contentV) = pair;
        builtinDiskAppendTextInner(&pathV, &contentV)
    }
}

/// Inner Disk.appendText.
pub fn builtinDiskAppendTextInner(pathV: &Val, contentV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let path = crate::aver_generated::domain::builtins::helpers::expectStr(pathV)?;
    let content = crate::aver_generated::domain::builtins::helpers::expectStr(contentV)?;
    match {
        let __effect_arg0 = path;
        let __effect_arg1 = content;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Disk.appendText",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
            ],
            || (aver_rt::append_text(&__effect_arg0, &__effect_arg1)).into_aver(),
        )
    } {
        Ok(_) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValUnit.clone()))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Disk.delete(path) -> Result.
pub fn builtinDiskDelete(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __effect_arg0 = path;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Disk.delete",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || (aver_rt::delete_file(&__effect_arg0)).into_aver(),
        )
    } {
        Ok(_) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValUnit.clone()))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Disk.deleteDir(path) -> Result.
pub fn builtinDiskDeleteDir(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __effect_arg0 = path;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Disk.deleteDir",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || (aver_rt::delete_dir(&__effect_arg0)).into_aver(),
        )
    } {
        Ok(_) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValUnit.clone()))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Disk.makeDir(path) -> Result.
pub fn builtinDiskMakeDir(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __effect_arg0 = path;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Disk.makeDir",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || (aver_rt::make_dir(&__effect_arg0)).into_aver(),
        )
    } {
        Ok(_) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValUnit.clone()))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Disk.exists(path) -> Bool.
pub fn builtinDiskExists(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(Val::ValBool({
        let __effect_arg0 = path;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Disk.exists",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || aver_rt::path_exists(&__effect_arg0),
        )
    }))
}

/// Disk.listDir(path) -> Result<List<String>, String>.
pub fn builtinDiskListDir(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    match {
        let __effect_arg0 = path;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Disk.listDir",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || (aver_rt::list_dir(&__effect_arg0)).into_aver(),
        )
    } {
        Ok(entries) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValList(
            stringsToVals(entries, aver_rt::AverList::empty()),
        )))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Try to construct a variant value from a dotted name like Type.Ctor. Uses stable tags for both builtin and user constructors.
#[inline(always)]
pub fn tryVariantConstructor(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match splitDotted(name.clone()) {
        Some(_) => Ok(Val::ValVariant(
            crate::aver_generated::domain::ast::ctorNameToTag(name.clone()),
            name,
            args.clone(),
        )),
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity((36i64) as usize);
                __b.push_str(&AverStr::from("undefined function: "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
            __b
        })),
    }
}

/// Split 'Type.Ctor' into (Type, Ctor). Returns None if no dot.
#[inline(always)]
pub fn splitDotted(name: AverStr) -> Option<(AverStr, AverStr)> {
    crate::cancel_checkpoint();
    splitDottedLoop(name.clone(), 0i64, (name.chars().count() as i64))
}

/// Find first dot and split.
#[inline(always)]
pub fn splitDottedLoop(
    mut name: AverStr,
    mut pos: i64,
    mut total: i64,
) -> Option<(AverStr, AverStr)> {
    loop {
        crate::cancel_checkpoint();
        return if (pos >= total) {
            None
        } else {
            match (name.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                Some(c) => {
                    if (&*c == ".") {
                        Some((
                            (aver_rt::string_slice(&name, 0i64, pos)).into_aver(),
                            (aver_rt::string_slice(&name, (pos + 1i64), total)).into_aver(),
                        ))
                    } else {
                        {
                            let __tmp1 = (pos + 1i64);
                            pos = __tmp1;
                            continue;
                        }
                    }
                }
                None => None,
            }
        };
    }
}

/// Bool.or(a, b) -> a || b.
pub fn builtinBoolOr(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        builtinBoolOrInner(&aV, &bV)
    }
}

/// Inner Bool.or.
pub fn builtinBoolOrInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match aV.clone() {
        Val::ValBool(a) => match bV.clone() {
            Val::ValBool(b) => {
                if a {
                    Ok(Val::ValBool(true))
                } else {
                    Ok(Val::ValBool(b))
                }
            }
            _ => Err(AverStr::from("Bool.or requires Bool arguments")),
        },
        _ => Err(AverStr::from("Bool.or requires Bool arguments")),
    }
}

/// Bool.and(a, b) -> a && b.
pub fn builtinBoolAnd(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        builtinBoolAndInner(&aV, &bV)
    }
}

/// Inner Bool.and.
pub fn builtinBoolAndInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match aV.clone() {
        Val::ValBool(a) => match bV.clone() {
            Val::ValBool(b) => {
                if a {
                    Ok(Val::ValBool(b))
                } else {
                    Ok(Val::ValBool(false))
                }
            }
            _ => Err(AverStr::from("Bool.and requires Bool arguments")),
        },
        _ => Err(AverStr::from("Bool.and requires Bool arguments")),
    }
}

/// Bool.not(a) -> negation.
pub fn builtinBoolNot(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValBool(b) => {
            if b {
                Ok(Val::ValBool(false))
            } else {
                Ok(Val::ValBool(true))
            }
        }
        _ => Err(AverStr::from("Bool.not requires Bool argument")),
    }
}

/// Map.set(map, key, value) -> updated map.
pub fn builtinMapSet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((mapV, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = r1;
                if let Some((keyV, r2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = r2;
                        if let Some((valV, r3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            builtinMapSetInner(&mapV, &keyV, &valV)
                        } else {
                            Err(AverStr::from("Map.set takes 3 arguments"))
                        }
                    }
                } else {
                    Err(AverStr::from("Map.set takes 3 arguments"))
                }
            }
        } else {
            Err(AverStr::from("Map.set takes 3 arguments"))
        }
    }
}

/// Set a key in the map. O(1).
pub fn builtinMapSetInner(mapV: &Val, keyV: &Val, valV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        Val::ValMap(m) => Ok(Val::ValMap(m.insert_owned(
            crate::aver_generated::domain::value::mapKeyRepr(keyV),
            valV.clone(),
        ))),
        _ => Err(AverStr::from("Map.set requires a Map")),
    }
}

/// Map.get(map, key) -> Option<Val>. O(1).
pub fn builtinMapGet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (mapV, keyV) = pair;
        builtinMapGetInner(&mapV, &keyV)
    }
}

/// Look up key in map. O(1).
pub fn builtinMapGetInner(mapV: &Val, keyV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        Val::ValMap(m) => {
            match m
                .get(&crate::aver_generated::domain::value::mapKeyRepr(keyV))
                .cloned()
            {
                Some(v) => Ok(Val::ValSome(std::sync::Arc::new(v))),
                None => Ok(Val::ValNone.clone()),
            }
        }
        _ => Err(AverStr::from("Map.get requires a Map")),
    }
}

/// Map.has(map, key) -> Bool. O(1).
pub fn builtinMapHas(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (mapV, keyV) = pair;
        builtinMapHasInner(&mapV, &keyV)
    }
}

/// Check if key exists in map. O(1).
pub fn builtinMapHasInner(mapV: &Val, keyV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        Val::ValMap(m) => Ok(Val::ValBool(
            m.contains_key(&crate::aver_generated::domain::value::mapKeyRepr(keyV)),
        )),
        _ => Err(AverStr::from("Map.has requires a Map")),
    }
}

/// Time.now() -> ISO timestamp string.
pub fn builtinTimeNow(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    Ok(Val::ValStr({
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Time.now", vec![], || (aver_rt::time_now()).into_aver())
    }))
}

/// Http.get/head/delete(url) -> Result<HttpResponse, String> forwarded to host.
pub fn builtinHttpSimple(args: &aver_rt::AverList<Val>, method: AverStr) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let url = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    {
        let __dispatch_subject = method;
        if &*__dispatch_subject == "get" {
            Ok(httpResponseToVal(&{
                let __effect_arg0 = url;
                crate::cancel_checkpoint();
                aver_replay::invoke_effect(
                    "Http.get",
                    vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
                    || (aver_rt::http::get(&__effect_arg0)).into_aver(),
                )
            }))
        } else {
            if &*__dispatch_subject == "head" {
                Ok(httpResponseToVal(&{
                    let __effect_arg0 = url;
                    crate::cancel_checkpoint();
                    aver_replay::invoke_effect(
                        "Http.head",
                        vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
                        || (aver_rt::http::head(&__effect_arg0)).into_aver(),
                    )
                }))
            } else {
                Ok(httpResponseToVal(&{
                    let __effect_arg0 = url;
                    crate::cancel_checkpoint();
                    aver_replay::invoke_effect(
                        "Http.delete",
                        vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
                        || (aver_rt::http::delete(&__effect_arg0)).into_aver(),
                    )
                }))
            }
        }
    }
}

/// Http.post/put/patch(url, body, contentType, headers) forwarded to host.
pub fn builtinHttpBody(args: &aver_rt::AverList<Val>, method: AverStr) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((urlV, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = r1;
                if let Some((bodyV, r2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = r2;
                        if let Some((ctV, r3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = r3;
                                if let Some((hdrsV, r4)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    builtinHttpBodyInner(&urlV, &bodyV, &ctV, &hdrsV, method)
                                } else {
                                    Err(AverStr::from("Http method takes 4 arguments"))
                                }
                            }
                        } else {
                            Err(AverStr::from("Http method takes 4 arguments"))
                        }
                    }
                } else {
                    Err(AverStr::from("Http method takes 4 arguments"))
                }
            }
        } else {
            Err(AverStr::from("Http method takes 4 arguments"))
        }
    }
}

/// Inner Http body method.
pub fn builtinHttpBodyInner(
    urlV: &Val,
    bodyV: &Val,
    ctV: &Val,
    hdrsV: &Val,
    method: AverStr,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let url = crate::aver_generated::domain::builtins::helpers::expectStr(urlV)?;
    let body = crate::aver_generated::domain::builtins::helpers::expectStr(bodyV)?;
    let ct = crate::aver_generated::domain::builtins::helpers::expectStr(ctV)?;
    {
        let __dispatch_subject = method;
        if &*__dispatch_subject == "post" {
            Ok(httpResponseToVal(&{
                let __effect_arg0 = url;
                let __effect_arg1 = body;
                let __effect_arg2 = ct;
                let __effect_arg3 = HashMap::new();
                crate::cancel_checkpoint();
                aver_replay::invoke_effect(
                    "Http.post",
                    vec![
                        aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                        aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
                        aver_replay::ReplayValue::to_replay_json(&__effect_arg2),
                        aver_replay::ReplayValue::to_replay_json(&__effect_arg3),
                    ],
                    || {
                        (aver_rt::http::post(
                            &__effect_arg0,
                            &__effect_arg1,
                            &__effect_arg2,
                            &__effect_arg3,
                        ))
                        .into_aver()
                    },
                )
            }))
        } else {
            if &*__dispatch_subject == "put" {
                Ok(httpResponseToVal(&{
                    let __effect_arg0 = url;
                    let __effect_arg1 = body;
                    let __effect_arg2 = ct;
                    let __effect_arg3 = HashMap::new();
                    crate::cancel_checkpoint();
                    aver_replay::invoke_effect(
                        "Http.put",
                        vec![
                            aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                            aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
                            aver_replay::ReplayValue::to_replay_json(&__effect_arg2),
                            aver_replay::ReplayValue::to_replay_json(&__effect_arg3),
                        ],
                        || {
                            (aver_rt::http::put(
                                &__effect_arg0,
                                &__effect_arg1,
                                &__effect_arg2,
                                &__effect_arg3,
                            ))
                            .into_aver()
                        },
                    )
                }))
            } else {
                Ok(httpResponseToVal(&{
                    let __effect_arg0 = url;
                    let __effect_arg1 = body;
                    let __effect_arg2 = ct;
                    let __effect_arg3 = HashMap::new();
                    crate::cancel_checkpoint();
                    aver_replay::invoke_effect(
                        "Http.patch",
                        vec![
                            aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                            aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
                            aver_replay::ReplayValue::to_replay_json(&__effect_arg2),
                            aver_replay::ReplayValue::to_replay_json(&__effect_arg3),
                        ],
                        || {
                            (aver_rt::http::patch(
                                &__effect_arg0,
                                &__effect_arg1,
                                &__effect_arg2,
                                &__effect_arg3,
                            ))
                            .into_aver()
                        },
                    )
                }))
            }
        }
    }
}

/// Convert host HttpResponse to Val.
#[inline(always)]
pub fn httpResponseToVal(result: &Result<HttpResponse, AverStr>) -> Val {
    crate::cancel_checkpoint();
    match result.clone() {
        Ok(resp) => Val::ValOk(std::sync::Arc::new(Val::ValRecord(
            AverStr::from("HttpResponse"),
            aver_rt::AverList::from_vec(vec![
                (AverStr::from("status"), Val::ValInt(resp.status.clone())),
                (AverStr::from("body"), Val::ValStr(resp.body.clone())),
                (AverStr::from("headers"), headersToVal(&resp.headers)),
            ]),
        ))),
        Err(e) => Val::ValErr(std::sync::Arc::new(Val::ValStr(e))),
    }
}

/// Convert host headers Map<String, List<String>> to a Val.ValMap whose values are Val.ValList of Val.ValStr.
#[inline(always)]
pub fn headersToVal(headers: &aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>>) -> Val {
    crate::cancel_checkpoint();
    Val::ValMap(headersToValMap(
        headers.clone(),
        {
            let mut ks: Vec<_> = headers.keys().cloned().collect();
            ks.sort();
            aver_rt::AverList::from_vec(ks)
        },
        HashMap::new(),
    ))
}

/// Walk header keys, converting each value list to a Val.ValList.
#[inline(always)]
pub fn headersToValMap(
    mut headers: aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>>,
    mut names: aver_rt::AverList<AverStr>,
    mut acc: aver_rt::AverMap<AverStr, Val>,
) -> aver_rt::AverMap<AverStr, Val> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(names, [] => acc, [name, rest] => { match headers.get(&name).cloned() { Some(values) => { {
            let __tmp2 = acc.insert_owned(name, Val::ValList(stringsToValStrs(values, aver_rt::AverList::empty())));
            names = rest;
            acc = __tmp2;
            continue;
        } }, None => { {
            let __tmp2 = acc.insert_owned(name, Val::ValList(aver_rt::AverList::empty()));
            names = rest;
            acc = __tmp2;
            continue;
        } } } });
    }
}

/// Convert a list of strings into a list of Val.ValStr (tail-recursive).
#[inline(always)]
pub fn stringsToValStrs(
    mut values: aver_rt::AverList<AverStr>,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(values, [] => acc.reverse(), [v, rest] => { {
            let __tmp1 = aver_rt::AverList::prepend(Val::ValStr(v), &acc);
            values = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Tcp.send(host, port, message) -> Result<String, String>.
pub fn builtinTcpSend(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((hostV, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = r1;
                if let Some((portV, r2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = r2;
                        if let Some((msgV, r3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            builtinTcpSendInner(&hostV, &portV, &msgV)
                        } else {
                            Err(AverStr::from("Tcp.send takes 3 arguments"))
                        }
                    }
                } else {
                    Err(AverStr::from("Tcp.send takes 3 arguments"))
                }
            }
        } else {
            Err(AverStr::from("Tcp.send takes 3 arguments"))
        }
    }
}

/// Inner Tcp.send.
pub fn builtinTcpSendInner(hostV: &Val, portV: &Val, msgV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let host = crate::aver_generated::domain::builtins::helpers::expectStr(hostV)?;
    let port = crate::aver_generated::domain::builtins::helpers::expectInt(portV)?;
    let msg = crate::aver_generated::domain::builtins::helpers::expectStr(msgV)?;
    match {
        let __effect_arg0 = host;
        let __effect_arg1 = port;
        let __effect_arg2 = msg;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Tcp.send",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg2),
            ],
            || (aver_rt::tcp::send(&__effect_arg0, __effect_arg1, &__effect_arg2)).into_aver(),
        )
    } {
        Ok(resp) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValStr(resp)))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Tcp.ping(host, port) -> Result<Unit, String>.
pub fn builtinTcpPing(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (hostV, portV) = pair;
        builtinTcpPingInner(&hostV, &portV)
    }
}

/// Inner Tcp.ping.
pub fn builtinTcpPingInner(hostV: &Val, portV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let host = crate::aver_generated::domain::builtins::helpers::expectStr(hostV)?;
    let port = crate::aver_generated::domain::builtins::helpers::expectInt(portV)?;
    match {
        let __effect_arg0 = host;
        let __effect_arg1 = port;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Tcp.ping",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
            ],
            || (aver_rt::tcp::ping(&__effect_arg0, __effect_arg1)).into_aver(),
        )
    } {
        Ok(_) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValUnit.clone()))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Tcp.connect(host, port) -> Result<Tcp.Connection, String>.
pub fn builtinTcpConnect(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (hostV, portV) = pair;
        builtinTcpConnectInner(&hostV, &portV)
    }
}

/// Inner Tcp.connect.
pub fn builtinTcpConnectInner(hostV: &Val, portV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let host = crate::aver_generated::domain::builtins::helpers::expectStr(hostV)?;
    let port = crate::aver_generated::domain::builtins::helpers::expectInt(portV)?;
    match {
        let __effect_arg0 = host;
        let __effect_arg1 = port;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Tcp.connect",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
            ],
            || (aver_rt::tcp::connect(&__effect_arg0, __effect_arg1)).into_aver(),
        )
    } {
        Ok(conn) => Ok(Val::ValOk(std::sync::Arc::new(tcpConnToVal(&conn)))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Convert host Tcp.Connection record to Val.
#[inline(always)]
pub fn tcpConnToVal(conn: &Tcp_Connection) -> Val {
    crate::cancel_checkpoint();
    Val::ValRecord(
        AverStr::from("Tcp.Connection"),
        aver_rt::AverList::from_vec(vec![
            (AverStr::from("id"), Val::ValStr(conn.id.clone())),
            (AverStr::from("host"), Val::ValStr(conn.host.clone())),
            (AverStr::from("port"), Val::ValInt(conn.port.clone())),
        ]),
    )
}

/// Extract Tcp.Connection fields from ValRecord.
pub fn valToTcpConn(v: &Val) -> Result<Tcp_Connection, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        Val::ValRecord(_, fields) => valToTcpConnFields(&fields),
        _ => Err(AverStr::from("expected Tcp.Connection record")),
    }
}

/// Build Tcp.Connection from field list.
pub fn valToTcpConnFields(
    fields: &aver_rt::AverList<(AverStr, Val)>,
) -> Result<Tcp_Connection, AverStr> {
    crate::cancel_checkpoint();
    let idVal = lookupFieldVal(fields.clone(), AverStr::from("id"));
    let hostVal = lookupFieldVal(fields.clone(), AverStr::from("host"));
    let portVal = lookupFieldVal(fields.clone(), AverStr::from("port"));
    match idVal {
        Val::ValStr(id) => match hostVal {
            Val::ValStr(host) => match portVal {
                Val::ValInt(port) => Ok(Tcp_Connection {
                    id: id,
                    host: host,
                    port: port,
                }),
                _ => Err(AverStr::from("bad Tcp.Connection port")),
            },
            _ => Err(AverStr::from("bad Tcp.Connection host")),
        },
        _ => Err(AverStr::from("bad Tcp.Connection id")),
    }
}

/// Find field value by name, return ValUnit if missing.
#[inline(always)]
pub fn lookupFieldVal(mut fields: aver_rt::AverList<(AverStr, Val)>, mut name: AverStr) -> Val {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fields, [] => Val::ValUnit, [pair, rest] => { match pair {
            (k, v) => if (k == name) { v } else { {
            fields = rest;
            continue;
        } }
        } });
    }
}

/// Tcp.writeLine(conn, line) -> Result<Unit, String>.
pub fn builtinTcpWriteLine(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (connV, lineV) = pair;
        builtinTcpWriteLineInner(&connV, &lineV)
    }
}

/// Inner Tcp.writeLine.
pub fn builtinTcpWriteLineInner(connV: &Val, lineV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let tc = valToTcpConn(connV)?;
    let line = crate::aver_generated::domain::builtins::helpers::expectStr(lineV)?;
    match {
        let __effect_arg0 = tc;
        let __effect_arg1 = line;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Tcp.writeLine",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
            ],
            || (aver_rt::tcp::write_line(&__effect_arg0, &__effect_arg1)).into_aver(),
        )
    } {
        Ok(_) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValUnit.clone()))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Tcp.readLine(conn) -> Result<String, String>.
pub fn builtinTcpReadLine(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let tc = valToTcpConn(&v)?;
    match {
        let __effect_arg0 = tc;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Tcp.readLine",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || (aver_rt::tcp::read_line(&__effect_arg0)).into_aver(),
        )
    } {
        Ok(line) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValStr(line)))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

/// Tcp.close(conn) -> Result<Unit, String>.
pub fn builtinTcpClose(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let tc = valToTcpConn(&v)?;
    match {
        let __effect_arg0 = tc;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Tcp.close",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || (aver_rt::tcp::close(&__effect_arg0)).into_aver(),
        )
    } {
        Ok(_) => Ok(Val::ValOk(std::sync::Arc::new(Val::ValUnit.clone()))),
        Err(e) => Ok(Val::ValErr(std::sync::Arc::new(Val::ValStr(e)))),
    }
}

pub mod helpers;

pub mod list;

pub mod primitives;

pub mod vector;

pub mod wrappers;
