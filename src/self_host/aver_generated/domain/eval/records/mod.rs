#[allow(unused_imports)]
use crate::*;

/// Check if this is a Type.update(record, _named(...)) call.
#[inline(always)]
pub fn isRecordUpdate(
    name @ _: &AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> bool {
    crate::cancel_checkpoint();
    if name.contains(".update") {
        crate::aver_generated::domain::eval::records::hasNamedRecord(args)
    } else {
        false
    }
}

/// Check if last arg is a _named record sentinel.
#[inline(always)]
pub fn hasNamedRecord(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> bool {
    crate::cancel_checkpoint();
    if (aver_rt::AverInt::from_i64(args.len() as i64) == aver_rt::AverInt::from_i64(2)) {
        crate::aver_generated::domain::eval::records::isNamedSentinel(args)
    } else {
        false
    }
}

/// Check second element is ValRecord with _named tag.
#[inline(always)]
pub fn isNamedSentinel(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> bool {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let Some((_, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
            crate::aver_generated::domain::eval::records::isNamedSentinelInner(rest)
        } else {
            if __list_subject.is_empty() {
                false
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Check first element of rest is a _named record.
#[inline(always)]
pub fn isNamedSentinelInner(
    mut rest @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> bool {
    crate::cancel_checkpoint();
    aver_list_match!(rest, [] => false, [v, ignored] => { { let __t = v; match __t.view() {
        crate::aver_generated::domain::value::Val__view::ValRecord(__typeName_fields) => {
            let typeName = __typeName_fields.0.clone();
            (&*typeName == "_named")
        },
        _ => {
            false
        }
    } } })
}

/// Perform record update: merge named fields into base record.
#[inline(always)]
pub fn doRecordUpdate(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(baseVal), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            crate::aver_generated::domain::eval::records::doRecordUpdateInner(baseVal, rest)
        } else {
            if __list_subject.is_empty() {
                Err(::aver_rt::str_lit!(
                    "record update requires (record, named fields)"
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Extract named fields from rest and apply update.
#[inline(always)]
pub fn doRecordUpdateInner(
    baseVal @ _: &crate::aver_generated::domain::value::Val,
    mut rest @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(rest, [] => Err(::aver_rt::str_lit!("record update requires named fields")), [namedVal, ignored] => { { let __t = namedVal; match __t.view() {
        crate::aver_generated::domain::value::Val__view::ValRecord(__namedFields_fields) => {
            let namedFields = &__namedFields_fields.1;
            crate::aver_generated::domain::eval::records::applyRecordUpdate(baseVal, namedFields)
        },
        _ => {
            Err(::aver_rt::str_lit!("record update requires named fields"))
        }
    } } })
}

/// Override fields in a record value.
pub fn applyRecordUpdate(
    baseVal @ _: &crate::aver_generated::domain::value::Val,
    namedFields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = baseVal;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValRecord(__typeName_fields) => {
                let typeName = __typeName_fields.0.clone();
                let existingFields = &__typeName_fields.1;
                Ok(crate::aver_generated::domain::value::Val::ValRecord(
                    crate::aver_generated::domain::value::Val__ValRecord(
                        typeName,
                        crate::aver_generated::domain::eval::records::mergeRecordFields(
                            existingFields,
                            namedFields,
                        ),
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("update requires a record value")),
        }
    }
}

/// For each field in existing: use override if present, otherwise keep.
#[inline(always)]
pub fn mergeRecordFields(
    existing @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    overrides @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    crate::cancel_checkpoint();
    let __arg2 @ _ =
        aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
    crate::aver_generated::domain::eval::records::mergeRecordFieldsAcc__collected(
        existing,
        overrides.clone(),
        __arg2,
    )
}

/// Accumulate merged fields.
#[inline(always)]
pub fn mergeRecordFieldsAcc(
    mut existing @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut overrides @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(existing, [] => { return aver_rt::AverList::concat(&acc.reverse(), &overrides); }, [pair, rest] => { { let (k, v) = pair; match crate::aver_generated::domain::eval::records::findOverride(k.clone(), &overrides) { Some(__stp0 @ _) => { {
            let __tco0 = rest;
            let __tco1 = crate::aver_generated::domain::eval::records::removeOverride(k.clone(), overrides);
            let __tco2 = aver_rt::AverList::prepend_owned((k, __stp0), acc);
            existing = __tco0;
            overrides = __tco1;
            acc = __tco2;
            continue;
        } }, None => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend_owned((k, v), acc);
            existing = __tco0;
            acc = __tco2;
            continue;
        } } } } })
    }
}

/// Check if field k has an override.
#[inline(always)]
pub fn mergeOneField(
    k @ _: AverStr,
    mut v @ _: crate::aver_generated::domain::value::Val,
    mut rest @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut overrides @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    acc @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::records::findOverride(k.clone(), &overrides) {
        Some(newV @ _) => {
            let __arg6 @ _ =
                crate::aver_generated::domain::eval::records::removeOverride(k.clone(), overrides);
            {
                let __arg7 @ _ = aver_rt::AverList::prepend((k, newV), acc);
                crate::aver_generated::domain::eval::records::mergeRecordFieldsAcc(
                    rest, __arg6, __arg7,
                )
            }
        }
        None => {
            let __arg8 @ _ = aver_rt::AverList::prepend((k, v), acc);
            crate::aver_generated::domain::eval::records::mergeRecordFieldsAcc(
                rest, overrides, __arg8,
            )
        }
    }
}

/// Find a field value in the overrides list.
#[inline(always)]
pub fn findOverride(
    mut k @ _: AverStr,
    overrides @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> Option<crate::aver_generated::domain::value::Val> {
    let mut __hold1: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>;
    let mut overrides @ _: &aver_rt::AverList<(
        AverStr,
        crate::aver_generated::domain::value::Val,
    )> = overrides;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = overrides;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (ok, ov) = pair.clone();
                        if (ok == k) {
                            return Some(ov);
                        } else {
                            {
                                let __tco1 = rest;
                                __hold1 = __tco1;
                                overrides = &__hold1;
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

/// Remove a field from overrides list.
#[inline(always)]
pub fn removeOverride(
    k @ _: AverStr,
    mut overrides @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    crate::cancel_checkpoint();
    let __arg2 @ _ =
        <aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>>::empty();
    crate::aver_generated::domain::eval::records::removeOverrideAcc(k, overrides, __arg2)
}

/// Accumulate non-matching fields, then concat remainder when match found.
#[inline(always)]
pub fn removeOverrideAcc(
    mut k @ _: AverStr,
    mut overrides @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    loop {
        crate::cancel_checkpoint();
        let reversed @ _ = acc.reverse();
        aver_rt::aver_list_match_reuse!(overrides, [] => { return reversed; }, [pair, rest, __list_slot] => { { let (ok, ov) = pair; if (ok == k) { return aver_rt::AverList::concat(&reversed, &rest); } else { {
            let __tco1 = rest;
            let __tco2 = __list_slot.prepend((ok, ov), acc);
            overrides = __tco1;
            acc = __tco2;
            continue;
        } } } })
    }
}

/// Synthesized collecting variant of `mergeRecordFieldsAcc`. Appends to a builder where `mergeRecordFieldsAcc` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn mergeRecordFieldsAcc__collected(
    existing @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut overrides @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    let mut __hold0: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>;
    let mut existing @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> =
        existing;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = existing;
            if __list_subject.is_empty() {
                return aver_rt::AverList::concat(&aver_rt::list_builder_finalize(acc), &overrides);
            } else {
                if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (k, v) = pair.clone();
                        match crate::aver_generated::domain::eval::records::findOverride(
                            k.clone(),
                            &overrides,
                        ) {
                            Some(__stp0 @ _) => {
                                let __tco0 = rest;
                                let __tco1 =
                                    crate::aver_generated::domain::eval::records::removeOverride(
                                        k.clone(),
                                        overrides,
                                    );
                                let __tco2 = aver_rt::list_builder_push(acc, (k, __stp0));
                                __hold0 = __tco0;
                                existing = &__hold0;
                                overrides = __tco1;
                                acc = __tco2;
                                continue;
                            }
                            None => {
                                let __tco0 = rest;
                                let __tco2 = aver_rt::list_builder_push(acc, (k, v));
                                __hold0 = __tco0;
                                existing = &__hold0;
                                acc = __tco2;
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
