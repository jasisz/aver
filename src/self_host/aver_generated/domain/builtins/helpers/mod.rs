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

/// Extract the validated octets from the self-hosted Bytes record.
pub fn expectBytes(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverIntList, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValRecord(name, fields) => {
            if (&*name == "Bytes") {
                crate::aver_generated::domain::builtins::helpers::expectBytesFields(fields)
            } else {
                Err(AverStr::from("expected Bytes argument"))
            }
        }
        _ => Err(AverStr::from("expected Bytes argument")),
    }
}

/// Find the opaque Bytes values field in its interpreter representation.
#[inline(always)]
pub fn expectBytesFields(
    mut fields @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> Result<aver_rt::AverIntList, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return Err(AverStr::from("expected Bytes argument")); }, [field, rest] => { { let (name, value) = field; if (&*name == "values") { match value {
        crate::aver_generated::domain::value::Val::ValList(items) => {
            return crate::aver_generated::domain::builtins::helpers::expectByteItems__collected(items, aver_rt::int_list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)));
        },
        _ => {
            return Err(AverStr::from("expected Bytes argument"));
        }
    } } else { {
            let __tco0 = rest;
            fields = __tco0;
            continue;
        } } } })
    }
}

/// Project byte values while preserving their source order.
#[inline(always)]
pub fn expectByteItems(
    mut items @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverIntList,
) -> Result<aver_rt::AverIntList, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(items, [] => { return Ok(acc.reverse()); }, [item, rest] => { match item {
        crate::aver_generated::domain::value::Val::ValInt(value) => {
            {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverIntList::prepend(value, &acc);
            items = __tco0;
            acc = __tco1;
            continue;
        }
        },
        _ => {
            return Err(AverStr::from("expected Bytes argument"));
        }
    } })
    }
}

/// Synthesized collecting variant of `expectByteItems`. Appends to a builder where `expectByteItems` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn expectByteItems__collected(
    mut items @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverIntList,
) -> Result<aver_rt::AverIntList, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(items, [] => { return Ok(aver_rt::int_list_builder_finalize(acc)); }, [item, rest] => { match item {
        crate::aver_generated::domain::value::Val::ValInt(value) => {
            {
            let __tco0 = rest;
            let __tco1 = aver_rt::int_list_builder_push(acc, value);
            items = __tco0;
            acc = __tco1;
            continue;
        }
        },
        _ => {
            return Err(AverStr::from("expected Bytes argument"));
        }
    } })
    }
}
