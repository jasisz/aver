#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    MergeRecordFieldsAcc(
        aver_rt::AverList<(AverStr, Val)>,
        aver_rt::AverList<(AverStr, Val)>,
        aver_rt::AverList<(AverStr, Val)>,
    ),
    MergeOneField(
        AverStr,
        Val,
        aver_rt::AverList<(AverStr, Val)>,
        aver_rt::AverList<(AverStr, Val)>,
        aver_rt::AverList<(AverStr, Val)>,
    ),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> aver_rt::AverList<(AverStr, Val)> {
    loop {
        __state = match __state {
            __MutualTco1::MergeRecordFieldsAcc(mut existing, mut overrides, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(existing, [] => { return aver_rt::AverList::concat(&acc.reverse(), &overrides) }, [pair, rest] => { { let (k, v) = pair; __MutualTco1::MergeOneField(k, v, rest, overrides, acc) } })
            }
            __MutualTco1::MergeOneField(mut k, mut v, mut rest, mut overrides, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::records::findOverride(
                    k.clone(),
                    overrides.clone(),
                ) {
                    Some(newV) => __MutualTco1::MergeRecordFieldsAcc(
                        rest,
                        crate::aver_generated::domain::eval::records::removeOverride(
                            k.clone(),
                            &overrides,
                        ),
                        aver_rt::AverList::prepend((k, newV), &acc),
                    ),
                    None => __MutualTco1::MergeRecordFieldsAcc(
                        rest,
                        overrides,
                        aver_rt::AverList::prepend((k, v), &acc),
                    ),
                }
            }
        };
    }
}

/// Accumulate merged fields.
pub fn mergeRecordFieldsAcc(
    existing: &aver_rt::AverList<(AverStr, Val)>,
    overrides: &aver_rt::AverList<(AverStr, Val)>,
    acc: &aver_rt::AverList<(AverStr, Val)>,
) -> aver_rt::AverList<(AverStr, Val)> {
    __mutual_tco_trampoline_1(__MutualTco1::MergeRecordFieldsAcc(
        existing.clone(),
        overrides.clone(),
        acc.clone(),
    ))
}

/// Check if field k has an override.
pub fn mergeOneField(
    k: AverStr,
    v: &Val,
    rest: &aver_rt::AverList<(AverStr, Val)>,
    overrides: &aver_rt::AverList<(AverStr, Val)>,
    acc: &aver_rt::AverList<(AverStr, Val)>,
) -> aver_rt::AverList<(AverStr, Val)> {
    __mutual_tco_trampoline_1(__MutualTco1::MergeOneField(
        k,
        v.clone(),
        rest.clone(),
        overrides.clone(),
        acc.clone(),
    ))
}

/// Check if this is a Type.update(record, _named(...)) call.
#[inline(always)]
pub fn isRecordUpdate(name: AverStr, args: &aver_rt::AverList<Val>) -> bool {
    crate::cancel_checkpoint();
    if name.contains(".update") {
        crate::aver_generated::domain::eval::records::hasNamedRecord(args)
    } else {
        false
    }
}

/// Check if last arg is a _named record sentinel.
#[inline(always)]
pub fn hasNamedRecord(args: &aver_rt::AverList<Val>) -> bool {
    crate::cancel_checkpoint();
    if ((args.len() as i64) == 2i64) {
        crate::aver_generated::domain::eval::records::isNamedSentinel(args)
    } else {
        false
    }
}

/// Check second element is ValRecord with _named tag.
#[inline(always)]
pub fn isNamedSentinel(args: &aver_rt::AverList<Val>) -> bool {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((_, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            crate::aver_generated::domain::eval::records::isNamedSentinelInner(&rest)
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
pub fn isNamedSentinelInner(rest: &aver_rt::AverList<Val>) -> bool {
    crate::cancel_checkpoint();
    aver_list_match!(rest.clone(), [] => false, [v, ignored] => match v {
        crate::aver_generated::domain::value::Val::ValRecord(typeName, _) => {
            (&*typeName == "_named")
        },
        _ => {
            false
        }
    })
}

/// Perform record update: merge named fields into base record.
#[inline(always)]
pub fn doRecordUpdate(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(args.clone(), [] => Err(AverStr::from("record update requires (record, named fields)")), [baseVal, rest] => crate::aver_generated::domain::eval::records::doRecordUpdateInner(&baseVal, &rest))
}

/// Extract named fields from rest and apply update.
#[inline(always)]
pub fn doRecordUpdateInner(baseVal: &Val, rest: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(rest.clone(), [] => Err(AverStr::from("record update requires named fields")), [namedVal, ignored] => match namedVal {
        crate::aver_generated::domain::value::Val::ValRecord(_, namedFields) => {
            crate::aver_generated::domain::eval::records::applyRecordUpdate(baseVal, &namedFields)
        },
        _ => {
            Err(AverStr::from("record update requires named fields"))
        }
    })
}

/// Override fields in a record value.
pub fn applyRecordUpdate(
    baseVal: &Val,
    namedFields: &aver_rt::AverList<(AverStr, Val)>,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match baseVal.clone() {
        crate::aver_generated::domain::value::Val::ValRecord(typeName, existingFields) => {
            Ok(crate::aver_generated::domain::value::Val::ValRecord(
                typeName,
                crate::aver_generated::domain::eval::records::mergeRecordFields(
                    &existingFields,
                    namedFields,
                ),
            ))
        }
        _ => Err(AverStr::from("update requires a record value")),
    }
}

/// For each field in existing: use override if present, otherwise keep.
#[inline(always)]
pub fn mergeRecordFields(
    existing: &aver_rt::AverList<(AverStr, Val)>,
    overrides: &aver_rt::AverList<(AverStr, Val)>,
) -> aver_rt::AverList<(AverStr, Val)> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::records::mergeRecordFieldsAcc(
        existing,
        overrides,
        &aver_rt::AverList::empty(),
    )
}

/// Find a field value in the overrides list.
#[inline(always)]
pub fn findOverride(
    mut k: AverStr,
    mut overrides: aver_rt::AverList<(AverStr, Val)>,
) -> Option<Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(overrides, [] => { return None; }, [pair, rest] => { { let (ok, ov) = pair; if (ok == k) { return Some(ov); } else { {
            let __tco1 = rest;
            overrides = __tco1;
            continue;
        } } } })
    }
}

/// Remove a field from overrides list.
#[inline(always)]
pub fn removeOverride(
    k: AverStr,
    overrides: &aver_rt::AverList<(AverStr, Val)>,
) -> aver_rt::AverList<(AverStr, Val)> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::records::removeOverrideAcc(
        k,
        overrides.clone(),
        aver_rt::AverList::empty(),
    )
}

/// Accumulate non-matching fields, then concat remainder when match found.
#[inline(always)]
pub fn removeOverrideAcc(
    mut k: AverStr,
    mut overrides: aver_rt::AverList<(AverStr, Val)>,
    mut acc: aver_rt::AverList<(AverStr, Val)>,
) -> aver_rt::AverList<(AverStr, Val)> {
    loop {
        crate::cancel_checkpoint();
        let reversed = acc.reverse();
        aver_list_match!(overrides, [] => { return reversed; }, [pair, rest] => { { let (ok, ov) = pair; if (ok == k) { return aver_rt::AverList::concat(&reversed, &rest); } else { {
            let __tco1 = rest;
            let __tco2 = aver_rt::AverList::prepend((ok, ov), &acc);
            overrides = __tco1;
            acc = __tco2;
            continue;
        } } } })
    }
}
