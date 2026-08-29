#[allow(unused_imports)]
use crate::*;

/// Dispatch the four Int endian conversion builtins.
#[inline(always)]
pub fn call(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Int.toBigEndian" {
            crate::aver_generated::domain::builtins::endian::encodeBig(args)
        } else {
            if &*__dispatch_subject == "Int.toLittleEndian" {
                crate::aver_generated::domain::builtins::endian::encodeLittle(args)
            } else {
                if &*__dispatch_subject == "Int.fromBigEndian" {
                    crate::aver_generated::domain::builtins::endian::decodeBig(args)
                } else {
                    if &*__dispatch_subject == "Int.fromLittleEndian" {
                        crate::aver_generated::domain::builtins::endian::decodeLittle(args)
                    } else {
                        Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("unknown endian builtin: "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(name))));
                            __b
                        }))
                    }
                }
            }
        }
    }
}

/// Wrap native octets for the self-hosted Bytes record.
#[inline(always)]
pub fn byteVals(
    mut octets @ _: aver_rt::AverIntList,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(octets, [] => { return acc.reverse(); }, [octet, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::value::Val::ValInt(octet), &acc);
            octets = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Mirror the opaque Bytes value in the interpreter's nominal record form.
pub fn bytesVal(
    bytes @ _: &crate::aver_generated::bytes::Bytes,
) -> crate::aver_generated::domain::value::Val {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::value::Val::ValRecord(
        AverStr::from("Bytes"),
        aver_rt::AverList::from_vec(vec![(
            AverStr::from("values"),
            crate::aver_generated::domain::value::Val::ValList(
                crate::aver_generated::domain::builtins::endian::byteVals__collected(
                    crate::aver_generated::bytes::octets(bytes),
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            ),
        )]),
    )
}

/// Int.toBigEndian(value, width) -> semantic Result value.
pub fn encodeBig(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (valueV, widthV) = pair;
        crate::aver_generated::domain::builtins::endian::encodeBigInner(&valueV, &widthV)
    }
}

/// Encode one unsigned integer in exact-width big-endian order.
pub fn encodeBigInner(
    valueV @ _: &crate::aver_generated::domain::value::Val,
    widthV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let value @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(valueV)?;
    let width @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(widthV)?;
    match aver_rt::int_to_big_endian(&(value), &(width))
        .map(|__bytes| crate::aver_generated::bytes::Bytes {
            values: aver_rt::AverPackedU8::from_vec(__bytes),
        })
        .map_err(aver_rt::AverStr::from)
    {
        Ok(bytes @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::builtins::endian::bytesVal(
                &bytes,
            )),
        )),
        Err(error @ _) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(error)),
        )),
    }
}

/// Int.toLittleEndian(value, width) -> semantic Result value.
pub fn encodeLittle(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (valueV, widthV) = pair;
        crate::aver_generated::domain::builtins::endian::encodeLittleInner(&valueV, &widthV)
    }
}

/// Encode one unsigned integer in exact-width little-endian order.
pub fn encodeLittleInner(
    valueV @ _: &crate::aver_generated::domain::value::Val,
    widthV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let value @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(valueV)?;
    let width @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(widthV)?;
    match aver_rt::int_to_little_endian(&(value), &(width))
        .map(|__bytes| crate::aver_generated::bytes::Bytes {
            values: aver_rt::AverPackedU8::from_vec(__bytes),
        })
        .map_err(aver_rt::AverStr::from)
    {
        Ok(bytes @ _) => Ok(crate::aver_generated::domain::value::Val::ValOk(
            std::sync::Arc::new(crate::aver_generated::domain::builtins::endian::bytesVal(
                &bytes,
            )),
        )),
        Err(error @ _) => Ok(crate::aver_generated::domain::value::Val::ValErr(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValStr(error)),
        )),
    }
}

/// Int.fromBigEndian(bytes) -> unsigned integer value.
pub fn decodeBig(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let value @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let octets @ _ = crate::aver_generated::domain::builtins::helpers::expectBytes(&value)?;
    let bytes @ _ = crate::aver_generated::bytes::fromList(&octets)?;
    Ok(crate::aver_generated::domain::value::Val::ValInt(
        aver_rt::int_from_big_endian((bytes).values.as_slice()),
    ))
}

/// Int.fromLittleEndian(bytes) -> unsigned integer value.
pub fn decodeLittle(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let value @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let octets @ _ = crate::aver_generated::domain::builtins::helpers::expectBytes(&value)?;
    let bytes @ _ = crate::aver_generated::bytes::fromList(&octets)?;
    Ok(crate::aver_generated::domain::value::Val::ValInt(
        aver_rt::int_from_little_endian((bytes).values.as_slice()),
    ))
}

/// Synthesized collecting variant of `byteVals`. Appends to a builder where `byteVals` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn byteVals__collected(
    mut octets @ _: aver_rt::AverIntList,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(octets, [] => { return aver_rt::list_builder_finalize(acc); }, [octet, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::value::Val::ValInt(octet));
            octets = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}
