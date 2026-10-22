#[allow(unused_imports)]
use crate::*;

/// Execute a leaf-like fast path directly from slots.
pub fn runFastLeafSlot(
    leaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match leaf {
        crate::aver_generated::domain::ast::FastLeaf::LeafConstInt(n) => {
            let n = n.clone();
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                crate::aver_generated::domain::value::Val__ValInt(n),
            ))
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafConstFloat(f) => {
            let f = f.clone();
            Ok(crate::aver_generated::domain::value::Val::ValFloat(
                crate::aver_generated::domain::value::Val__ValFloat(f),
            ))
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafConstStr(s) => {
            let s = s.clone();
            Ok(crate::aver_generated::domain::value::Val::ValStr(
                crate::aver_generated::domain::value::Val__ValStr(s),
            ))
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafConstBool(b) => {
            let b = b.clone();
            Ok(crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool(b),
            ))
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafSlot(slot) => {
            let slot = slot.clone();
            crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafFieldAccess(slot, field) => {
            let slot = slot.clone();
            let field = field.clone();
            crate::aver_generated::domain::eval::fast::fastFieldAccessSlot(calleeEnv, slot, field)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafMapGet(mapSlot, keySlot) => {
            let mapSlot = mapSlot.clone();
            let keySlot = keySlot.clone();
            crate::aver_generated::domain::eval::fast::fastMapGetSlot(calleeEnv, mapSlot, keySlot)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafMapSet(mapSlot, keySlot, valueSlot) => {
            let mapSlot = mapSlot.clone();
            let keySlot = keySlot.clone();
            let valueSlot = valueSlot.clone();
            crate::aver_generated::domain::eval::fast::fastMapSetSlot(
                calleeEnv, mapSlot, keySlot, valueSlot,
            )
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafMapHas(mapSlot, keySlot) => {
            let mapSlot = mapSlot.clone();
            let keySlot = keySlot.clone();
            crate::aver_generated::domain::eval::fast::fastMapHasSlot(calleeEnv, mapSlot, keySlot)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafMapRemove(mapSlot, keySlot) => {
            let mapSlot = mapSlot.clone();
            let keySlot = keySlot.clone();
            crate::aver_generated::domain::eval::fast::fastMapRemoveSlot(
                calleeEnv, mapSlot, keySlot,
            )
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafVectorNew(sizeSlot, fill) => {
            let sizeSlot = sizeSlot.clone();
            let fill = fill.clone();
            crate::aver_generated::domain::eval::fast::fastVectorNewSlot(calleeEnv, sizeSlot, fill)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafVectorLen(vecSlot) => {
            let vecSlot = vecSlot.clone();
            crate::aver_generated::domain::eval::fast::fastVectorLenSlot(calleeEnv, vecSlot)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafVectorGetOrInt(
            vecSlot,
            idxSlot,
            defaultValue,
        ) => {
            let vecSlot = vecSlot.clone();
            let idxSlot = idxSlot.clone();
            let defaultValue = defaultValue.clone();
            crate::aver_generated::domain::eval::fast::fastVectorGetOrIntSlot(
                calleeEnv,
                vecSlot,
                idxSlot,
                defaultValue,
            )
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafBinopSlots(op, slotA, slotB) => {
            let slotA = slotA.clone();
            let slotB = slotB.clone();
            crate::aver_generated::domain::eval::fast::fastBinopSlots(calleeEnv, op, slotA, slotB)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafCmpSlots(op, slotA, slotB) => {
            let slotA = slotA.clone();
            let slotB = slotB.clone();
            crate::aver_generated::domain::eval::fast::fastCmpSlots(calleeEnv, op, slotA, slotB)
        }
    }
}

/// Execute a bool-slot branch with precomputed leaf results.
pub fn fastBoolSlotBranch(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot @ _: aver_rt::AverInt,
    thenLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
    elseLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let slotV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    {
        let __t = slotV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValBool(__cond_fields) => {
                let cond = __cond_fields.0.clone();
                crate::aver_generated::domain::eval::fast::selectFastLeaf(
                    cond, thenLeaf, elseLeaf, calleeEnv,
                )
            }
            _ => Err(::aver_rt::str_lit!("fast bool branch expects Bool slot")),
        }
    }
}

/// Execute an int equality branch with precomputed leaf results.
pub fn fastEqIntBranch(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot @ _: aver_rt::AverInt,
    expected @ _: aver_rt::AverInt,
    thenLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
    elseLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let slotV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    {
        let __t = slotV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValInt(__actual_fields) => {
                let actual = __actual_fields.0.clone();
                crate::aver_generated::domain::eval::fast::selectFastLeaf(
                    (actual == expected),
                    thenLeaf,
                    elseLeaf,
                    calleeEnv,
                )
            }
            _ => Err(::aver_rt::str_lit!("fast int branch expects Int slot")),
        }
    }
}

/// Execute a string equality branch with precomputed leaf results.
pub fn fastEqStringBranch(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot @ _: aver_rt::AverInt,
    expected @ _: AverStr,
    thenLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
    elseLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let slotV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    {
        let __t = slotV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValStr(__actual_fields) => {
                let actual = __actual_fields.0.clone();
                crate::aver_generated::domain::eval::fast::selectFastLeaf(
                    (actual == expected),
                    thenLeaf,
                    elseLeaf,
                    calleeEnv,
                )
            }
            _ => Err(::aver_rt::str_lit!(
                "fast string branch expects String slot"
            )),
        }
    }
}

/// Execute an int less-than branch with precomputed leaf results.
pub fn fastLtIntSlotsBranch(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    lhsSlot @ _: aver_rt::AverInt,
    rhsSlot @ _: aver_rt::AverInt,
    thenLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
    elseLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let lhsV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, lhsSlot)?;
    let rhsV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, rhsSlot)?;
    {
        let __t = lhsV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValInt(__lhs_fields) => {
                let lhs = __lhs_fields.0.clone();
                {
                    let __t = rhsV;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValInt(__rhs_fields) => {
                            let rhs = __rhs_fields.0.clone();
                            crate::aver_generated::domain::eval::fast::selectFastLeaf(
                                (lhs < rhs),
                                thenLeaf,
                                elseLeaf,
                                calleeEnv,
                            )
                        }
                        _ => Err(::aver_rt::str_lit!("fast lt branch expects Int rhs slot")),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("fast lt branch expects Int lhs slot")),
        }
    }
}

/// Choose and execute one of two leaf results.
#[inline(always)]
pub fn selectFastLeaf(
    cond @ _: bool,
    thenLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
    elseLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if cond {
        crate::aver_generated::domain::eval::fast::runFastLeafSlot(thenLeaf, calleeEnv)
    } else {
        crate::aver_generated::domain::eval::fast::runFastLeafSlot(elseLeaf, calleeEnv)
    }
}

/// Execute a two-arm [] / [h, ..t] list branch with precomputed leaf bodies.
pub fn fastListSlotBranch(
    mut calleeEnv @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot @ _: aver_rt::AverInt,
    emptyLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
    headSlot @ _: aver_rt::AverInt,
    tailSlot @ _: aver_rt::AverInt,
    consLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let listV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(&calleeEnv, slot)?;
    {
        let __t = listV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValList(__items_fields) => {
                let items = &__items_fields.0;
                crate::aver_generated::domain::eval::fast::fastListSlotBranchItems(
                    items, calleeEnv, emptyLeaf, headSlot, tailSlot, consLeaf,
                )
            }
            _ => Err(::aver_rt::str_lit!("no matching arm")),
        }
    }
}

/// Finish [] / [h, ..t] list branching once the list payload is extracted.
#[inline(always)]
pub fn fastListSlotBranchItems(
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut calleeEnv @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    emptyLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
    headSlot @ _: aver_rt::AverInt,
    tailSlot @ _: aver_rt::AverInt,
    consLeaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = items;
        if __list_subject.is_empty() {
            crate::aver_generated::domain::eval::fast::runFastLeafSlot(emptyLeaf, &calleeEnv)
        } else {
            if let (Some(head), Some(tail)) = (__list_subject.first(), __list_subject.tail()) {
                crate::aver_generated::domain::eval::fast::runFastLeafSlot(
                    consLeaf,
                    &crate::aver_generated::domain::eval::slots::setSlot(
                        crate::aver_generated::domain::eval::slots::setSlot(
                            calleeEnv, headSlot, head,
                        ),
                        tailSlot,
                        &crate::aver_generated::domain::value::Val::ValList(
                            crate::aver_generated::domain::value::Val__ValList(tail),
                        ),
                    ),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Read a record field directly from a resolved slot.
pub fn fastFieldAccessSlot(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot @ _: aver_rt::AverInt,
    field @ _: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let recordV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    {
        let __t = recordV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValRecord(__fields_fields) => {
                let fields = &__fields_fields.1;
                crate::aver_generated::domain::eval::common::lookupField(fields, field)
            }
            _ => Err(::aver_rt::str_lit!("field access on non-record")),
        }
    }
}

/// Read a map key directly from resolved slots.
pub fn fastMapGetSlot(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mapSlot @ _: aver_rt::AverInt,
    keySlot @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    crate::aver_generated::domain::eval::fast::fastMapGetSlotInner(&mapV, &keyV)
}

/// Look up a key in a ValMap without going through builtin dispatch.
pub fn fastMapGetSlotInner(
    mapV @ _: &crate::aver_generated::domain::value::Val,
    keyV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = mapV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                match m
                    .get(&crate::aver_generated::domain::value::mapKeyRepr(keyV))
                    .cloned()
                {
                    Some(v @ _) => Ok(crate::aver_generated::domain::value::Val::ValSome(
                        crate::aver_generated::domain::value::Val__ValSome(v),
                    )),
                    None => Ok(crate::aver_generated::domain::value::Val::ValNone),
                }
            }
            _ => Err(::aver_rt::str_lit!("Map.get requires a Map")),
        }
    }
}

/// Update a map directly from resolved slots.
pub fn fastMapSetSlot(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mapSlot @ _: aver_rt::AverInt,
    keySlot @ _: aver_rt::AverInt,
    valueSlot @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    let valueV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, valueSlot)?;
    crate::aver_generated::domain::eval::fast::fastMapSetSlotInner(&mapV, &keyV, &valueV)
}

/// Set a key in a ValMap without going through builtin dispatch.
pub fn fastMapSetSlotInner(
    mapV @ _: &crate::aver_generated::domain::value::Val,
    keyV @ _: &crate::aver_generated::domain::value::Val,
    valueV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = mapV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = __m_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValMap(
                    crate::aver_generated::domain::value::Val__ValMap(m.insert_owned(
                        crate::aver_generated::domain::value::mapKeyRepr(keyV),
                        valueV.clone(),
                    )),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Map.set requires a Map")),
        }
    }
}

/// Fast path for Vector.new(slot, int) wrappers.
pub fn fastVectorNewSlot(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    sizeSlot @ _: aver_rt::AverInt,
    fill @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let sizeV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, sizeSlot)?;
    crate::aver_generated::domain::eval::fast::fastVectorNewSlotInner(&sizeV, fill)
}

/// Allocate a vector when the wrapper shape is known in advance.
pub fn fastVectorNewSlotInner(
    sizeV @ _: &crate::aver_generated::domain::value::Val,
    fill @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = sizeV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValInt(__size_fields) => {
                let size = __size_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValVector(
                    crate::aver_generated::domain::value::Val__ValVector(
                        match aver_rt::checked_vector_size(&(size)) {
                            Some(__n) => Ok(aver_rt::AverVector::new(
                                __n,
                                crate::aver_generated::domain::value::Val::ValInt(
                                    crate::aver_generated::domain::value::Val__ValInt(fill),
                                ),
                            )),
                            None => {
                                Err(aver_rt::AverStr::from(aver_rt::vector_size_error_message()))
                            }
                        }?,
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Vector.new size must be Int")),
        }
    }
}

/// Fast path for Option.withDefault(Vector.get(slotVec, slotIdx), int).
pub fn fastVectorGetOrIntSlot(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    vecSlot @ _: aver_rt::AverInt,
    idxSlot @ _: aver_rt::AverInt,
    defaultValue @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, vecSlot)?;
    let idxV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, idxSlot)?;
    crate::aver_generated::domain::eval::fast::fastVectorGetOrIntSlotInner(
        &vecV,
        &idxV,
        defaultValue,
    )
}

/// Read a vector cell with an integer default without going through builtin dispatch.
pub fn fastVectorGetOrIntSlotInner(
    vecV @ _: &crate::aver_generated::domain::value::Val,
    idxV @ _: &crate::aver_generated::domain::value::Val,
    defaultValue @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = vecV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValVector(__vec_fields) => {
                let vec = &__vec_fields.0;
                {
                    let __t = idxV;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValInt(__idx_fields) => {
                            let idx = __idx_fields.0.clone();
                            match (idx).to_usize().and_then(|__i| vec.get(__i).cloned()) {
                                Some(v @ _) => Ok(v),
                                None => Ok(crate::aver_generated::domain::value::Val::ValInt(
                                    crate::aver_generated::domain::value::Val__ValInt(defaultValue),
                                )),
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("Vector.get index must be Int")),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("Vector.get expects a Vector")),
        }
    }
}

/// Check map membership directly from resolved slots.
pub fn fastMapHasSlot(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mapSlot @ _: aver_rt::AverInt,
    keySlot @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    {
        let __t = mapV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = &__m_fields.0;
                Ok(crate::aver_generated::domain::value::Val::ValBool(
                    crate::aver_generated::domain::value::Val__ValBool(
                        m.contains_key(&crate::aver_generated::domain::value::mapKeyRepr(&keyV)),
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Map.has requires a Map")),
        }
    }
}

/// Remove a map key directly from resolved slots.
pub fn fastMapRemoveSlot(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mapSlot @ _: aver_rt::AverInt,
    keySlot @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    {
        let __t = mapV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValMap(__m_fields) => {
                let m = __m_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValMap(
                    crate::aver_generated::domain::value::Val__ValMap(
                        m.remove_owned(&crate::aver_generated::domain::value::mapKeyRepr(&keyV)),
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Map.remove requires a Map")),
        }
    }
}

/// Get vector length directly from a resolved slot.
pub fn fastVectorLenSlot(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    vecSlot @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, vecSlot)?;
    {
        let __t = vecV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValVector(__vec_fields) => {
                let vec = &__vec_fields.0;
                Ok(crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(aver_rt::AverInt::from_i64(
                        vec.len() as i64,
                    )),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Vector.len expects a Vector")),
        }
    }
}

/// Execute arithmetic on two slot values without entering the full eval.
pub fn fastBinopSlots(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    op @ _: &crate::aver_generated::domain::ast::BinOp,
    slotA @ _: aver_rt::AverInt,
    slotB @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let va @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotA)?;
    let vb @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotB)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(va, vb, op)
}

/// Execute comparison on two slot values without entering the full eval.
pub fn fastCmpSlots(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
    slotA @ _: aver_rt::AverInt,
    slotB @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let va @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotA)?;
    let vb @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotB)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(va, vb, op)
}
