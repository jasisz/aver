#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::helpers::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Dispatch Result.* and Option.* builtins.
#[inline(always)]
pub fn call(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Result.Ok" {
            builtinResultOk(args)
        } else {
            if &*__dispatch_subject == "Result.Err" {
                builtinResultErr(args)
            } else {
                if &*__dispatch_subject == "Result.withDefault" {
                    builtinResultWithDefault(args)
                } else {
                    if &*__dispatch_subject == "Option.Some" {
                        builtinOptionSome(args)
                    } else {
                        if &*__dispatch_subject == "Option.withDefault" {
                            builtinOptionWithDefault(args)
                        } else {
                            Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity((41i64) as usize);
                                    __b.push_str(&AverStr::from("unknown wrapper builtin: "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(name),
                                )));
                                __b
                            }))
                        }
                    }
                }
            }
        }
    }
}

/// Result.Ok(value) -> wrapped Ok value.
pub fn builtinResultOk(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    Ok(Val::ValOk(std::sync::Arc::new(v)))
}

/// Result.Err(value) -> wrapped Err value.
pub fn builtinResultErr(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    Ok(Val::ValErr(std::sync::Arc::new(v)))
}

/// Result.withDefault(result, default) -> unwrap Ok or return default.
pub fn builtinResultWithDefault(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (resV, defV) = pair;
        match resV {
            Val::ValOk(inner) => {
                let inner = (*inner).clone();
                Ok(inner)
            }
            Val::ValErr(_) => Ok(defV),
            _ => Err(AverStr::from("Result.withDefault requires Result value")),
        }
    }
}

/// Option.Some(value) -> wrapped Some value.
pub fn builtinOptionSome(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    Ok(Val::ValSome(std::sync::Arc::new(v)))
}

/// Option.withDefault(option, default) -> unwrap Some or return default.
pub fn builtinOptionWithDefault(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (optV, defV) = pair;
        match optV {
            Val::ValSome(inner) => {
                let inner = (*inner).clone();
                Ok(inner)
            }
            Val::ValNone => Ok(defV),
            _ => Err(AverStr::from("Option.withDefault requires Option value")),
        }
    }
}

/// Option.toResult(option, errMsg) -> Result.
pub fn callOptionToResult(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (optV, errV) = pair;
        match optV {
            Val::ValSome(inner) => {
                let inner = (*inner).clone();
                Ok(Val::ValOk(std::sync::Arc::new(inner)))
            }
            Val::ValNone => Ok(Val::ValErr(std::sync::Arc::new(errV))),
            _ => Err(AverStr::from("Option.toResult requires Option value")),
        }
    }
}
