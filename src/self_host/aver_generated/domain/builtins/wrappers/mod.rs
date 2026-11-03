#[allow(unused_imports)]
use crate::*;

/// Dispatch Result.* and Option.* builtins.
#[inline(always)]
pub fn call(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Result.Ok" {
            crate::aver_generated::domain::builtins::wrappers::builtinResultOk(args)
        } else {
            if &*__dispatch_subject == "Result.Err" {
                crate::aver_generated::domain::builtins::wrappers::builtinResultErr(args)
            } else {
                if &*__dispatch_subject == "Result.withDefault" {
                    crate::aver_generated::domain::builtins::wrappers::builtinResultWithDefault(
                        args,
                    )
                } else {
                    if &*__dispatch_subject == "Option.Some" {
                        crate::aver_generated::domain::builtins::wrappers::builtinOptionSome(args)
                    } else {
                        if &*__dispatch_subject == "Option.withDefault" {
                            crate::aver_generated::domain::builtins::wrappers::builtinOptionWithDefault(args)
                        } else {
                            Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(41usize);
                                    __b.push_str(&::aver_rt::str_lit!("unknown wrapper builtin: "));
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

/// Result.Ok(value) -> wrapped Ok value.
pub fn builtinResultOk(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    Ok(crate::aver_generated::domain::value::Val::ValOk(
        crate::aver_generated::domain::value::Val__ValOk(v),
    ))
}

/// Result.Err(value) -> wrapped Err value.
pub fn builtinResultErr(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    Ok(crate::aver_generated::domain::value::Val::ValErr(
        crate::aver_generated::domain::value::Val__ValErr(v),
    ))
}

/// Result.withDefault(result, default) -> unwrap Ok or return default.
pub fn builtinResultWithDefault(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (resV, defV) = pair;
        {
            let __t = resV;
            match __t.view() {
                crate::aver_generated::domain::value::Val__view::ValOk(_) => {
                    let (inner,) =
                        match __t.try_take::<crate::aver_generated::domain::value::Val__ValOk>() {
                            Ok(__f) => (__f.0,),
                            Err(__t) => match __t.view() {
                                crate::aver_generated::domain::value::Val__view::ValOk(__f) => {
                                    (__f.0.clone(),)
                                }
                                _ => unreachable!(),
                            },
                        };
                    Ok(inner)
                }
                crate::aver_generated::domain::value::Val__view::ValErr(_) => Ok(defV),
                _ => Err(::aver_rt::str_lit!(
                    "Result.withDefault requires Result value"
                )),
            }
        }
    }
}

/// Option.Some(value) -> wrapped Some value.
pub fn builtinOptionSome(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    Ok(crate::aver_generated::domain::value::Val::ValSome(
        crate::aver_generated::domain::value::Val__ValSome(v),
    ))
}

/// Option.withDefault(option, default) -> unwrap Some or return default.
pub fn builtinOptionWithDefault(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (optV, defV) = pair;
        {
            let __t = optV;
            match __t.view() {
                crate::aver_generated::domain::value::Val__view::ValSome(_) => {
                    let (inner,) = match __t
                        .try_take::<crate::aver_generated::domain::value::Val__ValSome>()
                    {
                        Ok(__f) => (__f.0,),
                        Err(__t) => match __t.view() {
                            crate::aver_generated::domain::value::Val__view::ValSome(__f) => {
                                (__f.0.clone(),)
                            }
                            _ => unreachable!(),
                        },
                    };
                    Ok(inner)
                }
                crate::aver_generated::domain::value::Val__view::ValNone => Ok(defV),
                _ => Err(::aver_rt::str_lit!(
                    "Option.withDefault requires Option value"
                )),
            }
        }
    }
}

/// Result.fromOption(option, errMsg) -> Result.
pub fn callResultFromOption(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (optV, errV) = pair;
        {
            let __t = optV;
            match __t.view() {
                crate::aver_generated::domain::value::Val__view::ValSome(_) => {
                    let (inner, __tok0) = match __t
                        .try_take_reusing::<crate::aver_generated::domain::value::Val__ValSome>(
                    ) {
                        Ok((__f, __tok0)) => (__f.0, __tok0),
                        Err(__t) => match __t.view() {
                            crate::aver_generated::domain::value::Val__view::ValSome(__f) => {
                                (__f.0.clone(), aver_rt::thin::Reuse::empty())
                            }
                            _ => unreachable!(),
                        },
                    };
                    Ok(crate::aver_generated::domain::value::Val::new_in(
                        [&__tok0],
                        crate::aver_generated::domain::value::Val__ValOk(inner),
                    ))
                }
                crate::aver_generated::domain::value::Val__view::ValNone => {
                    Ok(crate::aver_generated::domain::value::Val::ValErr(
                        crate::aver_generated::domain::value::Val__ValErr(errV),
                    ))
                }
                _ => Err(::aver_rt::str_lit!(
                    "Result.fromOption requires Option value"
                )),
            }
        }
    }
}
