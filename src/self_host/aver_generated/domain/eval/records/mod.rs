#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Check if this is a Type.update(record, _named(...)) call.
#[inline(always)]
pub fn isRecordUpdate(
    name: AverStr,
    args: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
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
pub fn hasNamedRecord(args: &aver_rt::AverList<crate::aver_generated::domain::value::Val>) -> bool {
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
    args: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> bool {
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
pub fn isNamedSentinelInner(
    rest: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> bool {
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
pub fn doRecordUpdate(
    args: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(args.clone(), [] => Err(AverStr::from("record update requires (record, named fields)")), [baseVal, rest] => crate::aver_generated::domain::eval::records::doRecordUpdateInner(&baseVal, &rest))
}

/// Extract named fields from rest and apply update.
#[inline(always)]
pub fn doRecordUpdateInner(
    baseVal: &crate::aver_generated::domain::value::Val,
    rest: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
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
    baseVal: &crate::aver_generated::domain::value::Val,
    namedFields: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
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
    existing: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    overrides: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::records::mergeRecordFieldsAcc__collected(
        existing.clone(),
        overrides.clone(),
        aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
    )
}

/// Accumulate merged fields.
#[inline(always)]
pub fn mergeRecordFieldsAcc(
    mut existing: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut overrides: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut acc: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(existing, [] => { return aver_rt::AverList::concat(&acc.reverse(), &overrides); }, [pair, rest] => { { let (k, v) = pair; match crate::aver_generated::domain::eval::records::findOverride(k.clone(), overrides.clone()) { Some(__stp0 @ _) => { {
            let __tco0 = rest;
            let __tco1 = crate::aver_generated::domain::eval::records::removeOverride(k.clone(), &overrides);
            let __tco2 = aver_rt::AverList::prepend((k, __stp0), &acc);
            existing = __tco0;
            overrides = __tco1;
            acc = __tco2;
            continue;
        } }, None => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend((k, v), &acc);
            existing = __tco0;
            acc = __tco2;
            continue;
        } } } } })
    }
}

/// Check if field k has an override.
#[inline(always)]
pub fn mergeOneField(
    k: AverStr,
    v: &crate::aver_generated::domain::value::Val,
    rest: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    overrides: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    acc: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::records::findOverride(k.clone(), overrides.clone()) {
        Some(newV @ _) => crate::aver_generated::domain::eval::records::mergeRecordFieldsAcc(
            rest.clone(),
            crate::aver_generated::domain::eval::records::removeOverride(k.clone(), overrides),
            aver_rt::AverList::prepend((k, newV), &acc.clone()),
        ),
        None => crate::aver_generated::domain::eval::records::mergeRecordFieldsAcc(
            rest.clone(),
            overrides.clone(),
            aver_rt::AverList::prepend((k, v.clone()), &acc.clone()),
        ),
    }
}

/// Find a field value in the overrides list.
#[inline(always)]
pub fn findOverride(
    mut k: AverStr,
    mut overrides: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> Option<crate::aver_generated::domain::value::Val> {
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
    overrides: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
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
    mut overrides: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut acc: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
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

/// Synthesized collecting variant of `mergeRecordFieldsAcc`. Appends to a builder where `mergeRecordFieldsAcc` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn mergeRecordFieldsAcc__collected(
    mut existing: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut overrides: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    mut acc: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(existing, [] => { return aver_rt::AverList::concat(&aver_rt::list_builder_finalize(acc), &overrides); }, [pair, rest] => { { let (k, v) = pair; match crate::aver_generated::domain::eval::records::findOverride(k.clone(), overrides.clone()) { Some(__stp0 @ _) => { {
            let __tco0 = rest;
            let __tco1 = crate::aver_generated::domain::eval::records::removeOverride(k.clone(), &overrides);
            let __tco2 = aver_rt::list_builder_push(acc, (k, __stp0));
            existing = __tco0;
            overrides = __tco1;
            acc = __tco2;
            continue;
        } }, None => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, (k, v));
            existing = __tco0;
            acc = __tco2;
            continue;
        } } } } })
    }
}
