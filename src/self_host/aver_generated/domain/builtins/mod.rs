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
                aver_list_match!(items, [] => { return acc }, [item, rest] => match item {
                    crate::aver_generated::domain::value::Val::ValTuple(parts) => {
                        __MutualTco1::TuplesToMapOne(parts, rest, acc)
                    },
                    _ => {
                        __MutualTco1::TuplesToMap(rest, acc)
                    }
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
    match crate::aver_generated::domain::builtins::callBuiltinFast(name.clone(), args) {
        Some(result) => result,
        None => {
            if name.starts_with("List.") {
                crate::aver_generated::domain::builtins::list::call(name, args)
            } else {
                crate::aver_generated::domain::builtins::callBuiltinAfterList(name, args)
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

/// Dispatch pre-evaluated args by integer builtin ID. IDs assigned in Ast.builtinNameToId.
#[inline(always)]
pub fn callBuiltinByIdValues(
    id: aver_rt::AverInt,
    args: &aver_rt::AverList<Val>,
) -> Result<Val, AverStr> {
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
                                        AverStr::from("Vector.new"),
                                        args,
                                    )
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(8) {
                                        crate::aver_generated::domain::builtins::vector::call(
                                            AverStr::from("Vector.get"),
                                            args,
                                        )
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(9) {
                                            crate::aver_generated::domain::builtins::vector::call(
                                                AverStr::from("Vector.set"),
                                                args,
                                            )
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(10)
                                            {
                                                crate::aver_generated::domain::builtins::vector::call(AverStr::from("Vector.len"), args)
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(11)
                                                {
                                                    crate::aver_generated::domain::builtins::vector::call(AverStr::from("Vector.fromList"), args)
                                                } else {
                                                    if __dispatch_subject
                                                        == aver_rt::AverInt::from_i64(12)
                                                    {
                                                        crate::aver_generated::domain::builtins::vector::call(AverStr::from("List.fromVector"), args)
                                                    } else {
                                                        if __dispatch_subject
                                                            == aver_rt::AverInt::from_i64(13)
                                                        {
                                                            Ok(crate::aver_generated::domain::value::Val::ValNone)
                                                        } else {
                                                            if __dispatch_subject
                                                                == aver_rt::AverInt::from_i64(14)
                                                            {
                                                                crate::aver_generated::domain::builtins::wrappers::call(AverStr::from("Option.Some"), args)
                                                            } else {
                                                                if __dispatch_subject
                                                                    == aver_rt::AverInt::from_i64(
                                                                        15,
                                                                    )
                                                                {
                                                                    crate::aver_generated::domain::builtins::wrappers::call(AverStr::from("Option.withDefault"), args)
                                                                } else {
                                                                    if __dispatch_subject == aver_rt::AverInt::from_i64(16) { crate::aver_generated::domain::builtins::wrappers::call(AverStr::from("Result.Ok"), args) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(17) { crate::aver_generated::domain::builtins::wrappers::call(AverStr::from("Result.Err"), args) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(18) { crate::aver_generated::domain::builtins::wrappers::call(AverStr::from("Result.withDefault"), args) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(19) { crate::aver_generated::domain::builtins::primitives::callInt(AverStr::from("String.fromInt"), args) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(20) { crate::aver_generated::domain::builtins::list::call(AverStr::from("List.take"), args) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(21) { crate::aver_generated::domain::builtins::list::call(AverStr::from("List.drop"), args) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("Unknown builtin ID: ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&((id.to_string()).into_aver())))); __b })) } } } } } }
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
                    crate::aver_generated::domain::builtins::callBuiltinOther(name, args)
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
                                    crate::aver_generated::domain::builtins::builtinDiskReadText(
                                        args,
                                    )
                                } else {
                                    if &*__dispatch_subject == "Disk.writeText" {
                                        crate::aver_generated::domain::builtins::builtinDiskWriteText(args)
                                    } else {
                                        if &*__dispatch_subject == "Disk.exists" {
                                            crate::aver_generated::domain::builtins::builtinDiskExists(args)
                                        } else {
                                            if &*__dispatch_subject == "Disk.listDir" {
                                                crate::aver_generated::domain::builtins::builtinDiskListDir(args)
                                            } else {
                                                if &*__dispatch_subject == "Disk.appendText" {
                                                    crate::aver_generated::domain::builtins::builtinDiskAppendText(args)
                                                } else {
                                                    if &*__dispatch_subject == "Disk.delete" {
                                                        crate::aver_generated::domain::builtins::builtinDiskDelete(args)
                                                    } else {
                                                        if &*__dispatch_subject == "Disk.deleteDir"
                                                        {
                                                            crate::aver_generated::domain::builtins::builtinDiskDeleteDir(args)
                                                        } else {
                                                            if &*__dispatch_subject
                                                                == "Disk.makeDir"
                                                            {
                                                                crate::aver_generated::domain::builtins::builtinDiskMakeDir(args)
                                                            } else {
                                                                if &*__dispatch_subject == "Env.get"
                                                                {
                                                                    crate::aver_generated::domain::builtins::builtinEnvGet(args)
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "Env.set"
                                                                    {
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
                                                                                if &*__dispatch_subject == "Result.Err" { crate::aver_generated::domain::builtins::wrappers::call(name, args) } else { if &*__dispatch_subject == "Result.withDefault" { crate::aver_generated::domain::builtins::wrappers::call(name, args) } else { if &*__dispatch_subject == "Option.Some" { crate::aver_generated::domain::builtins::wrappers::call(name, args) } else { if &*__dispatch_subject == "Option.None" { Ok(crate::aver_generated::domain::value::Val::ValNone) } else { if &*__dispatch_subject == "Option.withDefault" { crate::aver_generated::domain::builtins::wrappers::call(name, args) } else { if &*__dispatch_subject == "Option.toResult" { crate::aver_generated::domain::builtins::wrappers::callOptionToResult(args) } else { if &*__dispatch_subject == "Bool.or" { crate::aver_generated::domain::builtins::builtinBoolOr(args) } else { if &*__dispatch_subject == "Bool.and" { crate::aver_generated::domain::builtins::builtinBoolAnd(args) } else { if &*__dispatch_subject == "Bool.not" { crate::aver_generated::domain::builtins::builtinBoolNot(args) } else { if &*__dispatch_subject == "Map.set" { crate::aver_generated::domain::builtins::builtinMapSet(args) } else { if &*__dispatch_subject == "Map.get" { crate::aver_generated::domain::builtins::builtinMapGet(args) } else { if &*__dispatch_subject == "Map.has" { crate::aver_generated::domain::builtins::builtinMapHas(args) } else { if &*__dispatch_subject == "Map.entries" { crate::aver_generated::domain::builtins::builtinMapEntries(args) } else { if &*__dispatch_subject == "Map.keys" { crate::aver_generated::domain::builtins::builtinMapKeys(args) } else { if &*__dispatch_subject == "Map.values" { crate::aver_generated::domain::builtins::builtinMapValues(args) } else { if &*__dispatch_subject == "Map.fromList" { crate::aver_generated::domain::builtins::builtinMapFromList(args) } else { if &*__dispatch_subject == "Map.size" { crate::aver_generated::domain::builtins::builtinMapSize(args) } else { if &*__dispatch_subject == "Map.len" { crate::aver_generated::domain::builtins::builtinMapSize(args) } else { if &*__dispatch_subject == "Map.remove" { crate::aver_generated::domain::builtins::builtinMapRemove(args) } else { crate::aver_generated::domain::builtins::callBuiltinServices(name, args) } } } } } } } } } } } } } } } } } } }
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
        Ok(line) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(line)),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
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
        Ok(content) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(content)),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Args.get() -> list of string args.
pub fn builtinArgsGet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let rawArgs = {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Args.get", vec![], || aver_replay::current_cli_args())
    };
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::builtins::stringsToVals__collected(
            rawArgs,
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
        ),
    ))
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
        Some(value) => Ok(crate::aver_generated::domain::value::Val::ValSome(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(value)),
        )),
        None => Ok(crate::aver_generated::domain::value::Val::ValNone),
    }
}

/// Env.set(key, value) -> Unit.
pub fn builtinEnvSet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (keyV, valueV) = pair;
        crate::aver_generated::domain::builtins::builtinEnvSetInner(&keyV, &valueV)
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
}

/// Convert list of strings to list of ValStr.
#[inline(always)]
pub fn stringsToVals(
    mut strs: aver_rt::AverList<AverStr>,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(strs, [] => { return acc.reverse(); }, [s, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::value::Val::ValStr(s), &acc);
            strs = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Map.entries(map) -> List of tuples.
pub fn builtinMapEntries(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            Ok(crate::aver_generated::domain::value::Val::ValList(
                crate::aver_generated::domain::builtins::mapEntriesToTuples__collected(
                    {
                        let mut es: Vec<_> =
                            m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                        es.sort_by(|a, b| a.0.cmp(&b.0));
                        aver_rt::AverList::from_vec(es)
                    },
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            ))
        }
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
        aver_list_match!(entries, [] => { return acc.reverse(); }, [pair, rest] => { { let (k, v) = pair; {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::value::Val::ValTuple(aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::value::Val::ValStr(k), v])), &acc);
            entries = __tco0;
            acc = __tco1;
            continue;
        } } })
    }
}

/// Map.keys(map) -> List of keys.
pub fn builtinMapKeys(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            Ok(crate::aver_generated::domain::value::Val::ValList(
                crate::aver_generated::domain::builtins::stringsToVals__collected(
                    {
                        let mut ks: Vec<_> = m.keys().cloned().collect();
                        ks.sort();
                        aver_rt::AverList::from_vec(ks)
                    },
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            ))
        }
        _ => Err(AverStr::from("Map.keys requires a Map")),
    }
}

/// Map.values(map) -> List of values.
pub fn builtinMapValues(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            Ok(crate::aver_generated::domain::value::Val::ValList({
                let mut es: Vec<_> = m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                es.sort_by(|a, b| a.0.cmp(&b.0));
                aver_rt::AverList::from_vec(es.into_iter().map(|(_, v)| v).collect::<Vec<_>>())
            }))
        }
        _ => Err(AverStr::from("Map.values requires a Map")),
    }
}

/// Map.fromList(entries) -> Map.
pub fn builtinMapFromList(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValMap(
        crate::aver_generated::domain::builtins::tuplesToMap(&items, &HashMap::new()),
    ))
}

/// Map.size(map) -> Int.
pub fn builtinMapSize(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                aver_rt::AverInt::from_i64(m.len() as i64),
            ))
        }
        _ => Err(AverStr::from("Map.size requires a Map")),
    }
}

/// Map.remove(map, key) -> Map.
pub fn builtinMapRemove(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (mapV, keyV) = pair;
        crate::aver_generated::domain::builtins::builtinMapRemoveInner(&mapV, &keyV)
    }
}

/// Inner Map.remove.
pub fn builtinMapRemoveInner(mapV: &Val, keyV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            Ok(crate::aver_generated::domain::value::Val::ValMap(
                m.remove_owned(&crate::aver_generated::domain::value::mapKeyRepr(keyV)),
            ))
        }
        _ => Err(AverStr::from("Map.remove requires a Map")),
    }
}

/// Dispatch Terminal.*, Time.*, Random.*, Http.*, Tcp.* service builtins.
pub fn callBuiltinServices(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
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
                                AverStr::from("get"),
                            )
                        } else {
                            if &*__dispatch_subject == "Http.head" {
                                crate::aver_generated::domain::builtins::builtinHttpSimple(
                                    args,
                                    AverStr::from("head"),
                                )
                            } else {
                                if &*__dispatch_subject == "Http.delete" {
                                    crate::aver_generated::domain::builtins::builtinHttpSimple(
                                        args,
                                        AverStr::from("delete"),
                                    )
                                } else {
                                    if &*__dispatch_subject == "Http.post" {
                                        crate::aver_generated::domain::builtins::builtinHttpBody(
                                            args,
                                            AverStr::from("post"),
                                        )
                                    } else {
                                        if &*__dispatch_subject == "Http.put" {
                                            crate::aver_generated::domain::builtins::builtinHttpBody(
                                                args,
                                                AverStr::from("put"),
                                            )
                                        } else {
                                            if &*__dispatch_subject == "Http.patch" {
                                                crate::aver_generated::domain::builtins::builtinHttpBody(args, AverStr::from("patch"))
                                            } else {
                                                if &*__dispatch_subject == "HttpServer.listen" {
                                                    crate::aver_generated::domain::builtins::builtinHttpServerListen(args)
                                                } else {
                                                    if &*__dispatch_subject
                                                        == "HttpServer.listenWith"
                                                    {
                                                        crate::aver_generated::domain::builtins::builtinHttpServerListenWith(args)
                                                    } else {
                                                        if &*__dispatch_subject == "Tcp.send" {
                                                            crate::aver_generated::domain::builtins::builtinTcpSend(args)
                                                        } else {
                                                            if &*__dispatch_subject == "Tcp.ping" {
                                                                crate::aver_generated::domain::builtins::builtinTcpPing(args)
                                                            } else {
                                                                if &*__dispatch_subject
                                                                    == "Tcp.connect"
                                                                {
                                                                    crate::aver_generated::domain::builtins::builtinTcpConnect(args)
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "Tcp.writeLine"
                                                                    {
                                                                        crate::aver_generated::domain::builtins::builtinTcpWriteLine(args)
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "Tcp.readLine"
                                                                        {
                                                                            crate::aver_generated::domain::builtins::builtinTcpReadLine(args)
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "Tcp.close"
                                                                            {
                                                                                crate::aver_generated::domain::builtins::builtinTcpClose(args)
                                                                            } else {
                                                                                if &*__dispatch_subject == "Terminal.clear" { crate::aver_generated::domain::builtins::builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.flush" { crate::aver_generated::domain::builtins::builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.enableRawMode" { crate::aver_generated::domain::builtins::builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.disableRawMode" { crate::aver_generated::domain::builtins::builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.hideCursor" { crate::aver_generated::domain::builtins::builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.showCursor" { crate::aver_generated::domain::builtins::builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.resetColor" { crate::aver_generated::domain::builtins::builtinTerminalNoArg(name, args) } else { if &*__dispatch_subject == "Terminal.readKey" { crate::aver_generated::domain::builtins::builtinTerminalReadKey(args) } else { if &*__dispatch_subject == "Terminal.size" { crate::aver_generated::domain::builtins::builtinTerminalSize(args) } else { if &*__dispatch_subject == "Terminal.print" { crate::aver_generated::domain::builtins::builtinTerminalPrint(args) } else { if &*__dispatch_subject == "Terminal.setColor" { crate::aver_generated::domain::builtins::builtinTerminalSetColor(args) } else { if &*__dispatch_subject == "Terminal.moveTo" { crate::aver_generated::domain::builtins::builtinTerminalMoveTo(args) } else { crate::aver_generated::domain::builtins::tryVariantConstructor(name, args) } } } } } } } } } } } }
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
pub fn builtinUnsupportedHostService(name: AverStr) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    Err(aver_rt::AverStr::from({
        let mut __b = {
            let mut __b = aver_rt::Buffer::with_capacity(
                (aver_rt::AverInt::from_i64(68)).to_usize().unwrap_or(0),
            );
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
        crate::aver_generated::domain::builtins::builtinHttpServerListenInner(&portV, &handlerV)
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
            || {
                crate::self_host_support::http_server_listen(
                    crate::to_host_i64(
                        &__effect_arg0,
                        "HttpServer.listen: port must fit a 64-bit integer",
                    ),
                    __effect_arg1,
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValUnit),
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
                            crate::aver_generated::domain::builtins::builtinHttpServerListenWithInner(&portV, &contextV, &handlerV)
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
                    crate::to_host_i64(
                        &__effect_arg0,
                        "HttpServer.listen: port must fit a 64-bit integer",
                    ),
                    __effect_arg1.clone(),
                    __effect_arg2,
                )
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValUnit),
        Err(e) => Err(e),
    }
}

/// Random.int(min, max) -> random integer.
pub fn builtinRandomInt(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (minV, maxV) = pair;
        crate::aver_generated::domain::builtins::builtinRandomIntInner(&minV, &maxV)
    }
}

/// Inner Random.int.
pub fn builtinRandomIntInner(minV: &Val, maxV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let minN = crate::aver_generated::domain::builtins::helpers::expectInt(minV)?;
    let maxN = crate::aver_generated::domain::builtins::helpers::expectInt(maxV)?;
    Ok(crate::aver_generated::domain::value::Val::ValInt({
        let __effect_arg0 = minN;
        let __effect_arg1 = maxN;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Random.int",
            vec![
                aver_replay::ReplayValue::to_replay_json(&__effect_arg0),
                aver_replay::ReplayValue::to_replay_json(&__effect_arg1),
            ],
            || {
                aver_rt::AverInt::from_i64(
                    aver_rt::random::random_int(
                        crate::to_host_i64(
                            &__effect_arg0,
                            "Random.int: bounds must fit a 64-bit integer",
                        ),
                        crate::to_host_i64(
                            &__effect_arg1,
                            "Random.int: bounds must fit a 64-bit integer",
                        ),
                    )
                    .unwrap(),
                )
            },
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
            || {
                aver_rt::time_sleep(crate::to_host_i64(
                    &__effect_arg0,
                    "Time.sleep: ms must fit a 64-bit integer",
                ))
            },
        )
    };
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
}

/// Time.unixMs() -> Int.
pub fn builtinTimeUnixMs(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    Ok(crate::aver_generated::domain::value::Val::ValInt({
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Time.unixMs", vec![], || {
            aver_rt::AverInt::from_i64(aver_rt::time_unix_ms())
        })
    }))
}

/// Terminal no-arg commands: clear, flush, enableRawMode, etc.
pub fn builtinTerminalNoArg(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
        Some(k) => Ok(crate::aver_generated::domain::value::Val::ValSome(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(k)),
        )),
        None => Ok(crate::aver_generated::domain::value::Val::ValNone),
    }
}

/// Terminal.size() -> Record with width and height.
pub fn builtinTerminalSize(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let sz = {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Terminal.size", vec![], || {
            let (w, h) = aver_rt::terminal_size().unwrap();
            crate::Terminal_Size {
                width: aver_rt::AverInt::from_i64(w),
                height: aver_rt::AverInt::from_i64(h),
            }
        })
    };
    Ok(crate::aver_generated::domain::value::Val::ValRecord(
        AverStr::from("TerminalSize"),
        aver_rt::AverList::from_vec(vec![
            (
                AverStr::from("width"),
                crate::aver_generated::domain::value::Val::ValInt(sz.width.clone()),
            ),
            (
                AverStr::from("height"),
                crate::aver_generated::domain::value::Val::ValInt(sz.height.clone()),
            ),
        ]),
    ))
}

/// Terminal.print(s) -> print string to terminal.
pub fn builtinTerminalPrint(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValStr(s) => {
            crate::aver_generated::domain::builtins::termPrintStr(s)
        }
        _ => crate::aver_generated::domain::builtins::termPrintStr(
            crate::aver_generated::domain::value::valRepr(&v),
        ),
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
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
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
}

/// Terminal.moveTo(x, y) -> Unit.
pub fn builtinTerminalMoveTo(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (xV, yV) = pair;
        crate::aver_generated::domain::builtins::builtinTerminalMoveToInner(&xV, &yV)
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
            || {
                aver_rt::terminal_move_to(
                    crate::to_host_i64(
                        &__effect_arg0,
                        "Terminal.moveTo: coordinates must fit a 64-bit integer",
                    ),
                    crate::to_host_i64(
                        &__effect_arg1,
                        "Terminal.moveTo: coordinates must fit a 64-bit integer",
                    ),
                )
                .unwrap()
            },
        )
    };
    Ok(crate::aver_generated::domain::value::Val::ValUnit)
}

/// Disk.writeText(path, content) -> Result.
pub fn builtinDiskWriteText(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (pathV, contentV) = pair;
        crate::aver_generated::domain::builtins::builtinDiskWriteTextInner(&pathV, &contentV)
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
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValUnit),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Disk.appendText(path, content) -> Result.
pub fn builtinDiskAppendText(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (pathV, contentV) = pair;
        crate::aver_generated::domain::builtins::builtinDiskAppendTextInner(&pathV, &contentV)
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
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValUnit),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
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
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValUnit),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
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
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValUnit),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
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
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValUnit),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Disk.exists(path) -> Bool.
pub fn builtinDiskExists(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let path = crate::aver_generated::domain::builtins::helpers::expectStr(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValBool({
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
        Ok(entries) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValList(
                crate::aver_generated::domain::builtins::stringsToVals__collected(
                    entries,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Try to construct a variant value from a dotted name like Type.Ctor. Uses stable tags for both builtin and user constructors.
#[inline(always)]
pub fn tryVariantConstructor(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::builtins::splitDotted(name.clone()) {
        Some(_) => Ok(crate::aver_generated::domain::value::Val::ValVariant(
            crate::aver_generated::domain::ast::ctorNameToTag(name.clone()),
            name,
            args.clone(),
        )),
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                );
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
    crate::aver_generated::domain::builtins::splitDottedLoop(
        name.clone(),
        aver_rt::AverInt::from_i64(0),
        aver_rt::AverInt::from_i64(name.chars().count() as i64),
    )
}

/// Find first dot and split.
#[inline(always)]
pub fn splitDottedLoop(
    mut name: AverStr,
    mut pos: aver_rt::AverInt,
    mut total: aver_rt::AverInt,
) -> Option<(AverStr, AverStr)> {
    loop {
        crate::cancel_checkpoint();
        if (pos < total) {
            match ((pos)
                .to_usize()
                .and_then(|__i| name.chars().nth(__i).map(|c| c.to_string())))
            .into_aver()
            {
                Some(c) => {
                    if (c == AverStr::from(".")) {
                        return Some((
                            (aver_rt::string_slice(
                                &name,
                                crate::aver_int_clamp_i64(&aver_rt::AverInt::from_i64(0)),
                                crate::aver_int_clamp_i64(&pos),
                            ))
                            .into_aver(),
                            (aver_rt::string_slice(
                                &name,
                                crate::aver_int_clamp_i64(&pos.add(&aver_rt::AverInt::from_i64(1))),
                                crate::aver_int_clamp_i64(&total),
                            ))
                            .into_aver(),
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

/// Bool.or(a, b) -> a || b.
pub fn builtinBoolOr(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::builtinBoolOrInner(&aV, &bV)
    }
}

/// Inner Bool.or.
pub fn builtinBoolOrInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match aV.clone() {
        crate::aver_generated::domain::value::Val::ValBool(a) => match bV.clone() {
            crate::aver_generated::domain::value::Val::ValBool(b) => {
                if a {
                    Ok(crate::aver_generated::domain::value::Val::ValBool(true))
                } else {
                    Ok(crate::aver_generated::domain::value::Val::ValBool(b))
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
        crate::aver_generated::domain::builtins::builtinBoolAndInner(&aV, &bV)
    }
}

/// Inner Bool.and.
pub fn builtinBoolAndInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match aV.clone() {
        crate::aver_generated::domain::value::Val::ValBool(a) => match bV.clone() {
            crate::aver_generated::domain::value::Val::ValBool(b) => {
                if a {
                    Ok(crate::aver_generated::domain::value::Val::ValBool(b))
                } else {
                    Ok(crate::aver_generated::domain::value::Val::ValBool(false))
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
        crate::aver_generated::domain::value::Val::ValBool(b) => {
            if b {
                Ok(crate::aver_generated::domain::value::Val::ValBool(false))
            } else {
                Ok(crate::aver_generated::domain::value::Val::ValBool(true))
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
                            crate::aver_generated::domain::builtins::builtinMapSetInner(
                                &mapV, &keyV, &valV,
                            )
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
        crate::aver_generated::domain::value::Val::ValMap(m) => Ok(
            crate::aver_generated::domain::value::Val::ValMap(m.insert_owned(
                crate::aver_generated::domain::value::mapKeyRepr(keyV),
                valV.clone(),
            )),
        ),
        _ => Err(AverStr::from("Map.set requires a Map")),
    }
}

/// Map.get(map, key) -> Option<Val>. O(1).
pub fn builtinMapGet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (mapV, keyV) = pair;
        crate::aver_generated::domain::builtins::builtinMapGetInner(&mapV, &keyV)
    }
}

/// Look up key in map. O(1).
pub fn builtinMapGetInner(mapV: &Val, keyV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            match m
                .get(&crate::aver_generated::domain::value::mapKeyRepr(keyV))
                .cloned()
            {
                Some(v) => Ok(crate::aver_generated::domain::value::Val::ValSome(
                    std::sync::Arc::new(v),
                )),
                None => Ok(crate::aver_generated::domain::value::Val::ValNone),
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
        crate::aver_generated::domain::builtins::builtinMapHasInner(&mapV, &keyV)
    }
}

/// Check if key exists in map. O(1).
pub fn builtinMapHasInner(mapV: &Val, keyV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            Ok(crate::aver_generated::domain::value::Val::ValBool(
                m.contains_key(&crate::aver_generated::domain::value::mapKeyRepr(keyV)),
            ))
        }
        _ => Err(AverStr::from("Map.has requires a Map")),
    }
}

/// Time.now() -> ISO timestamp string.
pub fn builtinTimeNow(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    Ok(crate::aver_generated::domain::value::Val::ValStr({
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
            Ok(crate::aver_generated::domain::builtins::httpResponseToVal(
                &{
                    let __effect_arg0 = url;
                    crate::cancel_checkpoint();
                    aver_replay::invoke_effect(
                        "Http.get",
                        vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
                        || (aver_rt::http::get(&__effect_arg0)).into_aver(),
                    )
                },
            ))
        } else {
            if &*__dispatch_subject == "head" {
                Ok(crate::aver_generated::domain::builtins::httpResponseToVal(
                    &{
                        let __effect_arg0 = url;
                        crate::cancel_checkpoint();
                        aver_replay::invoke_effect(
                            "Http.head",
                            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
                            || (aver_rt::http::head(&__effect_arg0)).into_aver(),
                        )
                    },
                ))
            } else {
                Ok(crate::aver_generated::domain::builtins::httpResponseToVal(
                    &{
                        let __effect_arg0 = url;
                        crate::cancel_checkpoint();
                        aver_replay::invoke_effect(
                            "Http.delete",
                            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
                            || (aver_rt::http::delete(&__effect_arg0)).into_aver(),
                        )
                    },
                ))
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
                                    crate::aver_generated::domain::builtins::builtinHttpBodyInner(
                                        &urlV, &bodyV, &ctV, &hdrsV, method,
                                    )
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
            Ok(crate::aver_generated::domain::builtins::httpResponseToVal(
                &{
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
                },
            ))
        } else {
            if &*__dispatch_subject == "put" {
                Ok(crate::aver_generated::domain::builtins::httpResponseToVal(
                    &{
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
                    },
                ))
            } else {
                Ok(crate::aver_generated::domain::builtins::httpResponseToVal(
                    &{
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
                    },
                ))
            }
        }
    }
}

/// Convert host HttpResponse to Val.
#[inline(always)]
pub fn httpResponseToVal(result: &Result<HttpResponse, AverStr>) -> Val {
    crate::cancel_checkpoint();
    match result.clone() {
        Ok(resp) => crate::aver_generated::domain::value::Val::ValOk(std::sync::Arc::new(
            crate::aver_generated::domain::value::Val::ValRecord(
                AverStr::from("HttpResponse"),
                aver_rt::AverList::from_vec(vec![
                    (
                        AverStr::from("status"),
                        crate::aver_generated::domain::value::Val::ValInt(resp.status.clone()),
                    ),
                    (
                        AverStr::from("body"),
                        crate::aver_generated::domain::value::Val::ValStr(resp.body.clone()),
                    ),
                    (
                        AverStr::from("headers"),
                        crate::aver_generated::domain::builtins::headersToVal(&resp.headers),
                    ),
                ]),
            ),
        )),
        Err(e) => crate::aver_generated::domain::value::Val::ValErr(std::sync::Arc::new(
            crate::aver_generated::domain::value::Val::ValStr(e),
        )),
    }
}

/// Convert host headers Map<String, List<String>> to a Val.ValMap whose values are Val.ValList of Val.ValStr.
pub fn headersToVal(headers: &aver_rt::AverMap<AverStr, aver_rt::AverList<AverStr>>) -> Val {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::value::Val::ValMap(
        crate::aver_generated::domain::builtins::headersToValMap(
            headers.clone(),
            {
                let mut ks: Vec<_> = headers.keys().cloned().collect();
                ks.sort();
                aver_rt::AverList::from_vec(ks)
            },
            HashMap::new(),
        ),
    )
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
        aver_list_match!(names, [] => { return acc; }, [name, rest] => { match headers.get(&name).cloned() { Some(values) => { {
            let __tco1 = rest;
            let __tco2 = acc.insert_owned(name, crate::aver_generated::domain::value::Val::ValList(crate::aver_generated::domain::builtins::stringsToValStrs__collected(values, aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)))));
            names = __tco1;
            acc = __tco2;
            continue;
        } }, None => { {
            let __tco1 = rest;
            let __tco2 = acc.insert_owned(name, crate::aver_generated::domain::value::Val::ValList(aver_rt::AverList::empty()));
            names = __tco1;
            acc = __tco2;
            continue;
        } } } })
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
        aver_list_match!(values, [] => { return acc.reverse(); }, [v, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::value::Val::ValStr(v), &acc);
            values = __tco0;
            acc = __tco1;
            continue;
        } })
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
                            crate::aver_generated::domain::builtins::builtinTcpSendInner(
                                &hostV, &portV, &msgV,
                            )
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
            || {
                (aver_rt::tcp::send(
                    &__effect_arg0,
                    crate::to_host_i64(&__effect_arg1, "Tcp.send: port must be an Int"),
                    &__effect_arg2,
                ))
                .into_aver()
            },
        )
    } {
        Ok(resp) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(resp)),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Tcp.ping(host, port) -> Result<Unit, String>.
pub fn builtinTcpPing(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (hostV, portV) = pair;
        crate::aver_generated::domain::builtins::builtinTcpPingInner(&hostV, &portV)
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
            || {
                (aver_rt::tcp::ping(
                    &__effect_arg0,
                    crate::to_host_i64(&__effect_arg1, "Tcp.ping: port must be an Int"),
                ))
                .into_aver()
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValUnit),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Tcp.connect(host, port) -> Result<Tcp.Connection, String>.
pub fn builtinTcpConnect(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (hostV, portV) = pair;
        crate::aver_generated::domain::builtins::builtinTcpConnectInner(&hostV, &portV)
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
            || {
                (aver_rt::tcp::connect(
                    &__effect_arg0,
                    crate::to_host_i64(&__effect_arg1, "Tcp.connect: port must be an Int"),
                ))
                .into_aver()
            },
        )
    } {
        Ok(conn) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::builtins::tcpConnToVal(&conn)),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Convert host Tcp.Connection record to Val.
pub fn tcpConnToVal(conn: &Tcp_Connection) -> Val {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::value::Val::ValRecord(
        AverStr::from("Tcp.Connection"),
        aver_rt::AverList::from_vec(vec![
            (
                AverStr::from("id"),
                crate::aver_generated::domain::value::Val::ValStr(conn.id.clone()),
            ),
            (
                AverStr::from("host"),
                crate::aver_generated::domain::value::Val::ValStr(conn.host.clone()),
            ),
            (
                AverStr::from("port"),
                crate::aver_generated::domain::value::Val::ValInt(conn.port.clone()),
            ),
        ]),
    )
}

/// Extract Tcp.Connection fields from ValRecord.
pub fn valToTcpConn(v: &Val) -> Result<Tcp_Connection, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValRecord(_, fields) => {
            crate::aver_generated::domain::builtins::valToTcpConnFields(&fields)
        }
        _ => Err(AverStr::from("expected Tcp.Connection record")),
    }
}

/// Build Tcp.Connection from field list.
pub fn valToTcpConnFields(
    fields: &aver_rt::AverList<(AverStr, Val)>,
) -> Result<Tcp_Connection, AverStr> {
    crate::cancel_checkpoint();
    let idVal = crate::aver_generated::domain::builtins::lookupFieldVal(
        fields.clone(),
        AverStr::from("id"),
    );
    let hostVal = crate::aver_generated::domain::builtins::lookupFieldVal(
        fields.clone(),
        AverStr::from("host"),
    );
    let portVal = crate::aver_generated::domain::builtins::lookupFieldVal(
        fields.clone(),
        AverStr::from("port"),
    );
    match idVal {
        crate::aver_generated::domain::value::Val::ValStr(id) => match hostVal {
            crate::aver_generated::domain::value::Val::ValStr(host) => match portVal {
                crate::aver_generated::domain::value::Val::ValInt(port) => Ok(Tcp_Connection {
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
        aver_list_match!(fields, [] => { return crate::aver_generated::domain::value::Val::ValUnit; }, [pair, rest] => { { let (k, v) = pair; if (k == name) { return v; } else { {
            let __tco0 = rest;
            fields = __tco0;
            continue;
        } } } })
    }
}

/// Tcp.writeLine(conn, line) -> Result<Unit, String>.
pub fn builtinTcpWriteLine(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (connV, lineV) = pair;
        crate::aver_generated::domain::builtins::builtinTcpWriteLineInner(&connV, &lineV)
    }
}

/// Inner Tcp.writeLine.
pub fn builtinTcpWriteLineInner(connV: &Val, lineV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let tc = crate::aver_generated::domain::builtins::valToTcpConn(connV)?;
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
            || {
                (aver_rt::tcp::write_line(
                    &crate::tcp_connection_to_host(&__effect_arg0),
                    &__effect_arg1,
                ))
                .into_aver()
            },
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValUnit),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Tcp.readLine(conn) -> Result<String, String>.
pub fn builtinTcpReadLine(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let tc = crate::aver_generated::domain::builtins::valToTcpConn(&v)?;
    match {
        let __effect_arg0 = tc;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Tcp.readLine",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || {
                (aver_rt::tcp::read_line(&crate::tcp_connection_to_host(&__effect_arg0)))
                    .into_aver()
            },
        )
    } {
        Ok(line) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(line)),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Tcp.close(conn) -> Result<Unit, String>.
pub fn builtinTcpClose(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let tc = crate::aver_generated::domain::builtins::valToTcpConn(&v)?;
    match {
        let __effect_arg0 = tc;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Tcp.close",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || (aver_rt::tcp::close(&crate::tcp_connection_to_host(&__effect_arg0))).into_aver(),
        )
    } {
        Ok(_) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValUnit),
        )),
        Err(e) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(e)),
        )),
    }
}

/// Synthesized collecting variant of `stringsToVals`. Appends to a builder where `stringsToVals` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn stringsToVals__collected(
    mut strs: aver_rt::AverList<AverStr>,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(strs, [] => { return aver_rt::list_builder_finalize(acc); }, [s, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::value::Val::ValStr(s));
            strs = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `mapEntriesToTuples`. Appends to a builder where `mapEntriesToTuples` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn mapEntriesToTuples__collected(
    mut entries: aver_rt::AverList<(AverStr, Val)>,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(entries, [] => { return aver_rt::list_builder_finalize(acc); }, [pair, rest] => { { let (k, v) = pair; {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::value::Val::ValTuple(aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::value::Val::ValStr(k), v])));
            entries = __tco0;
            acc = __tco1;
            continue;
        } } })
    }
}

/// Synthesized collecting variant of `stringsToValStrs`. Appends to a builder where `stringsToValStrs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn stringsToValStrs__collected(
    mut values: aver_rt::AverList<AverStr>,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(values, [] => { return aver_rt::list_builder_finalize(acc); }, [v, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::value::Val::ValStr(v));
            values = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

pub mod helpers;

pub mod list;

pub mod primitives;

pub mod vector;

pub mod wrappers;
