#[allow(unused_imports)]
use crate::*;

/// Extract single argument from args list.
pub fn oneArg(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((a, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if __list_subject.is_empty() {
                    Ok(a)
                } else {
                    Err(AverStr::from("expected 1 argument"))
                }
            }
        } else {
            Err(AverStr::from("expected 1 argument"))
        }
    }
}

/// Extract two arguments from args list.
pub fn twoArgs(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<
    (
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::value::Val,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((a, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((b, rest2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = rest2;
                        if __list_subject.is_empty() {
                            Ok((a, b))
                        } else {
                            Err(AverStr::from("expected 2 arguments"))
                        }
                    }
                } else {
                    Err(AverStr::from("expected 2 arguments"))
                }
            }
        } else {
            Err(AverStr::from("expected 2 arguments"))
        }
    }
}

/// Extract list from Val or error.
pub fn expectList(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValList(items) => Ok(items),
        _ => Err(AverStr::from("expected list argument")),
    }
}

/// Extract int from Val or error.
pub fn expectInt(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverInt, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValInt(n) => Ok(n),
        _ => Err(AverStr::from("expected int argument")),
    }
}

/// Extract string from Val or error.
pub fn expectStr(v @ _: &crate::aver_generated::domain::value::Val) -> Result<AverStr, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValStr(s) => Ok(s),
        _ => Err(AverStr::from("expected string argument")),
    }
}
