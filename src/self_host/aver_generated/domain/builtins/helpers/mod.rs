#[allow(unused_imports)]
use crate::*;

/// Extract single argument from args list.
pub fn oneArg(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let Some((a, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
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
        let __list_subject = args;
        if let Some((a, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
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
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValList(__items_fields) => {
                let items = __items_fields.0.clone();
                Ok(items)
            }
            _ => Err(AverStr::from("expected list argument")),
        }
    }
}

/// Extract int from Val or error.
pub fn expectInt(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverInt, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValInt(__n_fields) => {
                let n = __n_fields.0.clone();
                Ok(n)
            }
            _ => Err(AverStr::from("expected int argument")),
        }
    }
}

/// Extract string from Val or error.
pub fn expectStr(v @ _: &crate::aver_generated::domain::value::Val) -> Result<AverStr, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValStr(__s_fields) => {
                let s = __s_fields.0.clone();
                Ok(s)
            }
            _ => Err(AverStr::from("expected string argument")),
        }
    }
}

/// Extract the validated octets from the self-hosted Bytes record.
pub fn expectBytes(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverIntList, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValRecord(__name_fields) => {
                let name = __name_fields.0.clone();
                let fields = &__name_fields.1;
                if (&*name == "Bytes") {
                    crate::aver_generated::domain::builtins::helpers::expectBytesFields(fields)
                } else {
                    Err(AverStr::from("expected Bytes argument"))
                }
            }
            _ => Err(AverStr::from("expected Bytes argument")),
        }
    }
}

/// Find the opaque Bytes values field in its interpreter representation.
#[inline(always)]
pub fn expectBytesFields(
    fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> Result<aver_rt::AverIntList, AverStr> {
    let mut __hold0: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>;
    let mut fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> =
        fields;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fields;
            if __list_subject.is_empty() {
                return Err(AverStr::from("expected Bytes argument"));
            } else {
                if let (Some(field), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (name, value) = field.clone();
                        if (&*name == "values") {
                            {
                                let __t = value;
                                match __t.view() {
                                    crate::aver_generated::domain::value::Val__view::ValList(
                                        __items_fields,
                                    ) => {
                                        let items = &__items_fields.0;
                                        return crate::aver_generated::domain::builtins::helpers::expectByteItems__collected(items, aver_rt::int_list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)));
                                    }
                                    _ => {
                                        return Err(AverStr::from("expected Bytes argument"));
                                    }
                                }
                            }
                        } else {
                            {
                                let __tco0 = rest;
                                __hold0 = __tco0;
                                fields = &__hold0;
                                continue;
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

/// Project byte values while preserving their source order.
#[inline(always)]
pub fn expectByteItems(
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverIntList,
) -> Result<aver_rt::AverIntList, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = items;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = items;
            if __list_subject.is_empty() {
                return Ok(acc.reverse());
            } else {
                if let (Some(item), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __t = item;
                        match __t.view() {
                            crate::aver_generated::domain::value::Val__view::ValInt(
                                __value_fields,
                            ) => {
                                let value = __value_fields.0.clone();
                                {
                                    let __tco0 = rest;
                                    let __tco1 = aver_rt::AverIntList::prepend(value, &acc);
                                    __hold0 = __tco0;
                                    items = &__hold0;
                                    acc = __tco1;
                                    continue;
                                }
                            }
                            _ => {
                                return Err(AverStr::from("expected Bytes argument"));
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

/// Synthesized collecting variant of `expectByteItems`. Appends to a builder where `expectByteItems` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn expectByteItems__collected(
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverIntList,
) -> Result<aver_rt::AverIntList, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = items;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = items;
            if __list_subject.is_empty() {
                return Ok(aver_rt::int_list_builder_finalize(acc));
            } else {
                if let (Some(item), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __t = item;
                        match __t.view() {
                            crate::aver_generated::domain::value::Val__view::ValInt(
                                __value_fields,
                            ) => {
                                let value = __value_fields.0.clone();
                                {
                                    let __tco0 = rest;
                                    let __tco1 = aver_rt::int_list_builder_push(acc, value);
                                    __hold0 = __tco0;
                                    items = &__hold0;
                                    acc = __tco1;
                                    continue;
                                }
                            }
                            _ => {
                                return Err(AverStr::from("expected Bytes argument"));
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
