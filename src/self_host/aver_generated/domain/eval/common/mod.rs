#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::store::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// If variable not in env, try as a named function reference before falling back to a nullary variant.
#[inline(always)]
pub fn evalVarFallback(name: AverStr, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::store::lookupFnOption(fns, name.clone()) {
        Some(fd) => Ok(crate::aver_generated::domain::value::Val::ValFnRef(
            fd.name.clone(),
        )),
        None => match crate::aver_generated::domain::builtins::splitDotted(name.clone()) {
            Some(_) => Ok(crate::aver_generated::domain::value::Val::ValVariant(
                crate::aver_generated::domain::ast::ctorNameToTag(name.clone()),
                name,
                aver_rt::AverList::empty(),
            )),
            None => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("undefined variable: "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                __b
            })),
        },
    }
}

/// Internal prefix used to distinguish ? propagation from evaluator runtime errors.
pub fn propagationPrefix() -> AverStr {
    crate::cancel_checkpoint();
    AverStr::from("__aver_prop__:")
}

/// Mark a language-level Result.Err so function boundaries can convert it back into a value.
pub fn wrapPropagatedError(msg: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    (AverStr::from("__aver_prop__:") + &msg)
}

/// Return the propagated payload when the evaluator error is an internal ? marker.
#[inline(always)]
pub fn unwrapPropagatedError(err: AverStr) -> Option<AverStr> {
    crate::cancel_checkpoint();
    let prefix = AverStr::from("__aver_prop__:");
    if err.starts_with(&*prefix) {
        Some(
            (aver_rt::string_slice(
                &err,
                crate::aver_int_clamp_i64(&aver_rt::AverInt::from_i64(
                    prefix.chars().count() as i64
                )),
                crate::aver_int_clamp_i64(&aver_rt::AverInt::from_i64(err.chars().count() as i64)),
            ))
            .into_aver(),
        )
    } else {
        None
    }
}

/// Catch internal ? propagation at a function boundary and turn it back into Result.Err(value).
#[inline(always)]
pub fn normalizeFnReturn(result: &Result<Val, AverStr>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match result.clone() {
        Ok(v) => Ok(v),
        Err(err) => {
            match crate::aver_generated::domain::eval::common::unwrapPropagatedError(err.clone()) {
                Some(msg) => Ok(crate::aver_generated::domain::value::Val::ValErr(
                    std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(msg)),
                )),
                None => Err(err),
            }
        }
    }
}

/// Look up a record field by name.
#[inline(always)]
pub fn lookupField(
    mut fields: aver_rt::AverList<(AverStr, Val)>,
    mut name: AverStr,
) -> Result<Val, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return Err((AverStr::from("unknown field: ") + &name)); }, [pair, rest] => { { let (k, v) = pair; if (k == name) { return Ok(v); } else { {
            let __tco0 = rest;
            fields = __tco0;
            continue;
        } } } })
    }
}
