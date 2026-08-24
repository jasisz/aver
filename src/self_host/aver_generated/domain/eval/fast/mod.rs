#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::common::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::ops::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::slots::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Execute a leaf-like fast path directly from slots.
pub fn runFastLeafSlot(
    leaf: &crate::aver_generated::domain::ast::FastLeaf,
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match leaf.clone() {
        crate::aver_generated::domain::ast::FastLeaf::LeafConstInt(n) => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(n))
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafConstFloat(f) => {
            Ok(crate::aver_generated::domain::value::Val::ValFloat(f))
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafConstStr(s) => {
            Ok(crate::aver_generated::domain::value::Val::ValStr(s))
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafConstBool(b) => {
            Ok(crate::aver_generated::domain::value::Val::ValBool(b))
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafSlot(slot) => {
            crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafFieldAccess(slot, field) => {
            crate::aver_generated::domain::eval::fast::fastFieldAccessSlot(calleeEnv, slot, field)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafMapGet(mapSlot, keySlot) => {
            crate::aver_generated::domain::eval::fast::fastMapGetSlot(calleeEnv, mapSlot, keySlot)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafMapSet(mapSlot, keySlot, valueSlot) => {
            crate::aver_generated::domain::eval::fast::fastMapSetSlot(
                calleeEnv, mapSlot, keySlot, valueSlot,
            )
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafMapHas(mapSlot, keySlot) => {
            crate::aver_generated::domain::eval::fast::fastMapHasSlot(calleeEnv, mapSlot, keySlot)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafMapRemove(mapSlot, keySlot) => {
            crate::aver_generated::domain::eval::fast::fastMapRemoveSlot(
                calleeEnv, mapSlot, keySlot,
            )
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafVectorNew(sizeSlot, fill) => {
            crate::aver_generated::domain::eval::fast::fastVectorNewSlot(calleeEnv, sizeSlot, fill)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafVectorLen(vecSlot) => {
            crate::aver_generated::domain::eval::fast::fastVectorLenSlot(calleeEnv, vecSlot)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafVectorGetOrInt(
            vecSlot,
            idxSlot,
            defaultValue,
        ) => crate::aver_generated::domain::eval::fast::fastVectorGetOrIntSlot(
            calleeEnv,
            vecSlot,
            idxSlot,
            defaultValue,
        ),
        crate::aver_generated::domain::ast::FastLeaf::LeafBinopSlots(op, slotA, slotB) => {
            crate::aver_generated::domain::eval::fast::fastBinopSlots(calleeEnv, &op, slotA, slotB)
        }
        crate::aver_generated::domain::ast::FastLeaf::LeafCmpSlots(op, slotA, slotB) => {
            crate::aver_generated::domain::eval::fast::fastCmpSlots(calleeEnv, &op, slotA, slotB)
        }
    }
}

/// Execute a bool-slot branch with precomputed leaf results.
pub fn fastBoolSlotBranch(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot: aver_rt::AverInt,
    thenLeaf: &crate::aver_generated::domain::ast::FastLeaf,
    elseLeaf: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let slotV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    match slotV {
        crate::aver_generated::domain::value::Val::ValBool(cond) => {
            crate::aver_generated::domain::eval::fast::selectFastLeaf(
                cond, thenLeaf, elseLeaf, calleeEnv,
            )
        }
        _ => Err(AverStr::from("fast bool branch expects Bool slot")),
    }
}

/// Execute an int equality branch with precomputed leaf results.
pub fn fastEqIntBranch(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot: aver_rt::AverInt,
    expected: aver_rt::AverInt,
    thenLeaf: &crate::aver_generated::domain::ast::FastLeaf,
    elseLeaf: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let slotV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    match slotV {
        crate::aver_generated::domain::value::Val::ValInt(actual) => {
            crate::aver_generated::domain::eval::fast::selectFastLeaf(
                (actual == expected),
                thenLeaf,
                elseLeaf,
                calleeEnv,
            )
        }
        _ => Err(AverStr::from("fast int branch expects Int slot")),
    }
}

/// Execute a string equality branch with precomputed leaf results.
pub fn fastEqStringBranch(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot: aver_rt::AverInt,
    expected: AverStr,
    thenLeaf: &crate::aver_generated::domain::ast::FastLeaf,
    elseLeaf: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let slotV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    match slotV {
        crate::aver_generated::domain::value::Val::ValStr(actual) => {
            crate::aver_generated::domain::eval::fast::selectFastLeaf(
                (actual == expected),
                thenLeaf,
                elseLeaf,
                calleeEnv,
            )
        }
        _ => Err(AverStr::from("fast string branch expects String slot")),
    }
}

/// Execute an int less-than branch with precomputed leaf results.
pub fn fastLtIntSlotsBranch(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    lhsSlot: aver_rt::AverInt,
    rhsSlot: aver_rt::AverInt,
    thenLeaf: &crate::aver_generated::domain::ast::FastLeaf,
    elseLeaf: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let lhsV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, lhsSlot)?;
    let rhsV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, rhsSlot)?;
    match lhsV {
        crate::aver_generated::domain::value::Val::ValInt(lhs) => match rhsV {
            crate::aver_generated::domain::value::Val::ValInt(rhs) => {
                crate::aver_generated::domain::eval::fast::selectFastLeaf(
                    (lhs < rhs),
                    thenLeaf,
                    elseLeaf,
                    calleeEnv,
                )
            }
            _ => Err(AverStr::from("fast lt branch expects Int rhs slot")),
        },
        _ => Err(AverStr::from("fast lt branch expects Int lhs slot")),
    }
}

/// Choose and execute one of two leaf results.
#[inline(always)]
pub fn selectFastLeaf(
    cond: bool,
    thenLeaf: &crate::aver_generated::domain::ast::FastLeaf,
    elseLeaf: &crate::aver_generated::domain::ast::FastLeaf,
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
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
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot: aver_rt::AverInt,
    emptyLeaf: &crate::aver_generated::domain::ast::FastLeaf,
    headSlot: aver_rt::AverInt,
    tailSlot: aver_rt::AverInt,
    consLeaf: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let listV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    match listV {
        crate::aver_generated::domain::value::Val::ValList(items) => {
            crate::aver_generated::domain::eval::fast::fastListSlotBranchItems(
                &items, calleeEnv, emptyLeaf, headSlot, tailSlot, consLeaf,
            )
        }
        _ => Err(AverStr::from("no matching arm")),
    }
}

/// Finish [] / [h, ..t] list branching once the list payload is extracted.
#[inline(always)]
pub fn fastListSlotBranchItems(
    items: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    emptyLeaf: &crate::aver_generated::domain::ast::FastLeaf,
    headSlot: aver_rt::AverInt,
    tailSlot: aver_rt::AverInt,
    consLeaf: &crate::aver_generated::domain::ast::FastLeaf,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(items.clone(), [] => crate::aver_generated::domain::eval::fast::runFastLeafSlot(emptyLeaf, calleeEnv), [head, tail] => crate::aver_generated::domain::eval::fast::runFastLeafSlot(consLeaf, &crate::aver_generated::domain::eval::slots::setSlot(&crate::aver_generated::domain::eval::slots::setSlot(calleeEnv, headSlot, &head), tailSlot, &crate::aver_generated::domain::value::Val::ValList(tail))))
}

/// Read a record field directly from a resolved slot.
pub fn fastFieldAccessSlot(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot: aver_rt::AverInt,
    field: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let recordV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    match recordV {
        crate::aver_generated::domain::value::Val::ValRecord(_, fields) => {
            crate::aver_generated::domain::eval::common::lookupField(fields, field)
        }
        _ => Err(AverStr::from("field access on non-record")),
    }
}

/// Read a map key directly from resolved slots.
pub fn fastMapGetSlot(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mapSlot: aver_rt::AverInt,
    keySlot: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    crate::aver_generated::domain::eval::fast::fastMapGetSlotInner(&mapV, &keyV)
}

/// Look up a key in a ValMap without going through builtin dispatch.
pub fn fastMapGetSlotInner(
    mapV: &crate::aver_generated::domain::value::Val,
    keyV: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            match m
                .get(&crate::aver_generated::domain::value::mapKeyRepr(keyV))
                .cloned()
            {
                Some(v @ _) => Ok(crate::aver_generated::domain::value::Val::ValSome(
                    std::sync::Arc::new(v),
                )),
                None => Ok(crate::aver_generated::domain::value::Val::ValNone),
            }
        }
        _ => Err(AverStr::from("Map.get requires a Map")),
    }
}

/// Update a map directly from resolved slots.
pub fn fastMapSetSlot(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mapSlot: aver_rt::AverInt,
    keySlot: aver_rt::AverInt,
    valueSlot: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    let valueV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, valueSlot)?;
    crate::aver_generated::domain::eval::fast::fastMapSetSlotInner(&mapV, &keyV, &valueV)
}

/// Set a key in a ValMap without going through builtin dispatch.
pub fn fastMapSetSlotInner(
    mapV: &crate::aver_generated::domain::value::Val,
    keyV: &crate::aver_generated::domain::value::Val,
    valueV: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        crate::aver_generated::domain::value::Val::ValMap(m) => Ok(
            crate::aver_generated::domain::value::Val::ValMap(m.insert_owned(
                crate::aver_generated::domain::value::mapKeyRepr(keyV),
                valueV.clone(),
            )),
        ),
        _ => Err(AverStr::from("Map.set requires a Map")),
    }
}

/// Fast path for Vector.new(slot, int) wrappers.
pub fn fastVectorNewSlot(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    sizeSlot: aver_rt::AverInt,
    fill: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let sizeV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, sizeSlot)?;
    crate::aver_generated::domain::eval::fast::fastVectorNewSlotInner(&sizeV, fill)
}

/// Allocate a vector when the wrapper shape is known in advance.
pub fn fastVectorNewSlotInner(
    sizeV: &crate::aver_generated::domain::value::Val,
    fill: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match sizeV.clone() {
        crate::aver_generated::domain::value::Val::ValInt(size) => {
            Ok(crate::aver_generated::domain::value::Val::ValVector(
                match aver_rt::checked_vector_size(&(size)) {
                    Some(__n) => Ok(aver_rt::AverVector::new(
                        __n,
                        crate::aver_generated::domain::value::Val::ValInt(fill),
                    )),
                    None => Err(aver_rt::AverStr::from(aver_rt::vector_size_error_message())),
                }?,
            ))
        }
        _ => Err(AverStr::from("Vector.new size must be Int")),
    }
}

/// Fast path for Option.withDefault(Vector.get(slotVec, slotIdx), int).
pub fn fastVectorGetOrIntSlot(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    vecSlot: aver_rt::AverInt,
    idxSlot: aver_rt::AverInt,
    defaultValue: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, vecSlot)?;
    let idxV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, idxSlot)?;
    crate::aver_generated::domain::eval::fast::fastVectorGetOrIntSlotInner(
        &vecV,
        &idxV,
        defaultValue,
    )
}

/// Read a vector cell with an integer default without going through builtin dispatch.
pub fn fastVectorGetOrIntSlotInner(
    vecV: &crate::aver_generated::domain::value::Val,
    idxV: &crate::aver_generated::domain::value::Val,
    defaultValue: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match vecV.clone() {
        crate::aver_generated::domain::value::Val::ValVector(vec) => match idxV.clone() {
            crate::aver_generated::domain::value::Val::ValInt(idx) => {
                match (idx).to_usize().and_then(|__i| vec.get(__i).cloned()) {
                    Some(v @ _) => Ok(v),
                    None => Ok(crate::aver_generated::domain::value::Val::ValInt(
                        defaultValue,
                    )),
                }
            }
            _ => Err(AverStr::from("Vector.get index must be Int")),
        },
        _ => Err(AverStr::from("Vector.get expects a Vector")),
    }
}

/// Check map membership directly from resolved slots.
pub fn fastMapHasSlot(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mapSlot: aver_rt::AverInt,
    keySlot: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    match mapV {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            Ok(crate::aver_generated::domain::value::Val::ValBool(
                m.contains_key(&crate::aver_generated::domain::value::mapKeyRepr(&keyV)),
            ))
        }
        _ => Err(AverStr::from("Map.has requires a Map")),
    }
}

/// Remove a map key directly from resolved slots.
pub fn fastMapRemoveSlot(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mapSlot: aver_rt::AverInt,
    keySlot: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    match mapV {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            Ok(crate::aver_generated::domain::value::Val::ValMap(
                m.remove_owned(&crate::aver_generated::domain::value::mapKeyRepr(&keyV)),
            ))
        }
        _ => Err(AverStr::from("Map.remove requires a Map")),
    }
}

/// Get vector length directly from a resolved slot.
pub fn fastVectorLenSlot(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    vecSlot: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, vecSlot)?;
    match vecV {
        crate::aver_generated::domain::value::Val::ValVector(vec) => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                aver_rt::AverInt::from_i64(vec.len() as i64),
            ))
        }
        _ => Err(AverStr::from("Vector.len expects a Vector")),
    }
}

/// Execute arithmetic on two slot values without entering the full eval.
pub fn fastBinopSlots(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    op: &crate::aver_generated::domain::ast::BinOp,
    slotA: aver_rt::AverInt,
    slotB: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let va = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotA)?;
    let vb = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotB)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(&va, &vb, op)
}

/// Execute comparison on two slot values without entering the full eval.
pub fn fastCmpSlots(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    op: &crate::aver_generated::domain::ast::CmpOp,
    slotA: aver_rt::AverInt,
    slotB: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let va = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotA)?;
    let vb = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotB)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(&va, &vb, op)
}
