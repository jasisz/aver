#[allow(unused_imports)]
use crate::*;

/// If variable not in env, try a top-level binding, then a named function reference, then a nullary variant.
#[inline(always)]
pub fn evalVarFallback(
    name @ _: AverStr,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::store::lookupGlobal(fns, name.clone()) {
        Some(v @ _) => Ok(v),
        None => crate::aver_generated::domain::eval::common::evalVarFallbackNamed(name, fns),
    }
}

/// Resolve a name that is neither a local nor a top-level binding: function reference, then nullary variant.
#[inline(always)]
pub fn evalVarFallbackNamed(
    name @ _: AverStr,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::store::lookupFnOption(fns, name.clone()) {
        Some(fd @ _) => Ok(crate::aver_generated::domain::value::Val::ValFnRef(
            crate::aver_generated::domain::value::Val__ValFnRef(fd.name),
        )),
        None => match crate::aver_generated::domain::builtins::splitDotted(name.clone()) {
            Some(_) => Ok(crate::aver_generated::domain::value::Val::ValVariant(
                crate::aver_generated::domain::value::Val__ValVariant(
                    crate::aver_generated::domain::ast::ctorNameToTag(name.clone()),
                    name,
                    aver_rt::AverList::empty(),
                ),
            )),
            None => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(36usize);
                    __b.push_str(&::aver_rt::str_lit!("undefined variable: "));
                    __b
                };
                __b.push_display(&(name));
                __b
            })),
        },
    }
}

/// Internal prefix used to distinguish ? propagation from evaluator runtime errors.
pub fn propagationPrefix() -> AverStr {
    crate::cancel_checkpoint();
    ::aver_rt::str_lit!("__aver_prop__:")
}

/// Mark a language-level Result.Err so function boundaries can convert it back into a value.
pub fn wrapPropagatedError(msg @ _: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    (::aver_rt::str_lit!("__aver_prop__:") + &msg)
}

/// Return the propagated payload when the evaluator error is an internal ? marker.
#[inline(always)]
pub fn unwrapPropagatedError(err @ _: &AverStr) -> Option<AverStr> {
    crate::cancel_checkpoint();
    let prefix @ _ = ::aver_rt::str_lit!("__aver_prop__:");
    if err.starts_with(&*prefix) {
        Some(aver_rt::string_slice_part(
            &err,
            crate::aver_int_clamp_i64(&aver_rt::AverInt::from_i64(prefix.chars().count() as i64)),
            crate::aver_int_clamp_i64(&aver_rt::AverInt::from_i64(err.chars().count() as i64)),
        ))
    } else {
        None
    }
}

/// Catch internal ? propagation at a function boundary and turn it back into Result.Err(value).
#[inline(always)]
pub fn normalizeFnReturn(
    result @ _: &Result<crate::aver_generated::domain::value::Val, AverStr>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match result {
        Ok(v @ _) => Ok(v.clone()),
        Err(err @ _) => {
            let err = err.clone();
            match crate::aver_generated::domain::eval::common::unwrapPropagatedError(&err) {
                Some(msg @ _) => Ok(crate::aver_generated::domain::value::Val::ValErr(
                    crate::aver_generated::domain::value::Val__ValErr(
                        crate::aver_generated::domain::value::Val::ValStr(
                            crate::aver_generated::domain::value::Val__ValStr(msg),
                        ),
                    ),
                )),
                None => Err(err),
            }
        }
    }
}

/// Look up a record field by name.
#[inline(always)]
pub fn lookupField(
    fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut name @ _: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    let mut __hold0: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>;
    let mut fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> =
        fields;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fields;
            match aver_rt::list_uncons(__list_subject) {
                Some((pair, rest)) => {
                    let (k, v) = pair.clone();
                    if (k == name) {
                        return Ok(v);
                    } else {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            fields = &__hold0;
                            continue;
                        }
                    }
                }
                None => {
                    return Err((::aver_rt::str_lit!("unknown field: ") + &name));
                }
            }
        }
    }
}
