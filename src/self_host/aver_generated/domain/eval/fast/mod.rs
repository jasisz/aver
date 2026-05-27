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
    leaf: &FastLeaf,
    calleeEnv: &aver_rt::AverVector<Val>,
) -> Result<Val, AverStr> {
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
    calleeEnv: &aver_rt::AverVector<Val>,
    slot: i64,
    thenLeaf: &FastLeaf,
    elseLeaf: &FastLeaf,
) -> Result<Val, AverStr> {
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
    calleeEnv: &aver_rt::AverVector<Val>,
    slot: i64,
    expected: i64,
    thenLeaf: &FastLeaf,
    elseLeaf: &FastLeaf,
) -> Result<Val, AverStr> {
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
    calleeEnv: &aver_rt::AverVector<Val>,
    slot: i64,
    expected: AverStr,
    thenLeaf: &FastLeaf,
    elseLeaf: &FastLeaf,
) -> Result<Val, AverStr> {
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
    calleeEnv: &aver_rt::AverVector<Val>,
    lhsSlot: i64,
    rhsSlot: i64,
    thenLeaf: &FastLeaf,
    elseLeaf: &FastLeaf,
) -> Result<Val, AverStr> {
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
    thenLeaf: &FastLeaf,
    elseLeaf: &FastLeaf,
    calleeEnv: &aver_rt::AverVector<Val>,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    if cond {
        crate::aver_generated::domain::eval::fast::runFastLeafSlot(thenLeaf, calleeEnv)
    } else {
        crate::aver_generated::domain::eval::fast::runFastLeafSlot(elseLeaf, calleeEnv)
    }
}

/// Execute a two-arm [] / [h, ..t] list branch with precomputed leaf bodies.
pub fn fastListSlotBranch(
    calleeEnv: &aver_rt::AverVector<Val>,
    slot: i64,
    emptyLeaf: &FastLeaf,
    headSlot: i64,
    tailSlot: i64,
    consLeaf: &FastLeaf,
) -> Result<Val, AverStr> {
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
    items: &aver_rt::AverList<Val>,
    calleeEnv: &aver_rt::AverVector<Val>,
    emptyLeaf: &FastLeaf,
    headSlot: i64,
    tailSlot: i64,
    consLeaf: &FastLeaf,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(items.clone(), [] => crate::aver_generated::domain::eval::fast::runFastLeafSlot(emptyLeaf, calleeEnv), [head, tail] => crate::aver_generated::domain::eval::fast::runFastLeafSlot(consLeaf, &crate::aver_generated::domain::eval::slots::setSlot(&crate::aver_generated::domain::eval::slots::setSlot(calleeEnv, headSlot, &head), tailSlot, &crate::aver_generated::domain::value::Val::ValList(tail))))
}

/// Read a record field directly from a resolved slot.
pub fn fastFieldAccessSlot(
    calleeEnv: &aver_rt::AverVector<Val>,
    slot: i64,
    field: AverStr,
) -> Result<Val, AverStr> {
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
    calleeEnv: &aver_rt::AverVector<Val>,
    mapSlot: i64,
    keySlot: i64,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    crate::aver_generated::domain::eval::fast::fastMapGetSlotInner(&mapV, &keyV)
}

/// Look up a key in a ValMap without going through builtin dispatch.
pub fn fastMapGetSlotInner(mapV: &Val, keyV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        crate::aver_generated::domain::value::Val::ValMap(m) => {
            match m
                .get(&crate::aver_generated::domain::value::mapKeyRepr(keyV))
                .cloned()
            {
                Some(v) => Ok(crate::aver_generated::domain::value::Val::ValSome(
                    std::sync::Arc::new(v),
                )),
                None => Ok(Val::ValNone.clone()),
            }
        }
        _ => Err(AverStr::from("Map.get requires a Map")),
    }
}

/// Update a map directly from resolved slots.
pub fn fastMapSetSlot(
    calleeEnv: &aver_rt::AverVector<Val>,
    mapSlot: i64,
    keySlot: i64,
    valueSlot: i64,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    let valueV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, valueSlot)?;
    crate::aver_generated::domain::eval::fast::fastMapSetSlotInner(&mapV, &keyV, &valueV)
}

/// Set a key in a ValMap without going through builtin dispatch.
pub fn fastMapSetSlotInner(mapV: &Val, keyV: &Val, valueV: &Val) -> Result<Val, AverStr> {
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
    calleeEnv: &aver_rt::AverVector<Val>,
    sizeSlot: i64,
    fill: i64,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let sizeV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, sizeSlot)?;
    crate::aver_generated::domain::eval::fast::fastVectorNewSlotInner(&sizeV, fill)
}

/// Allocate a vector when the wrapper shape is known in advance.
pub fn fastVectorNewSlotInner(sizeV: &Val, fill: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match sizeV.clone() {
        crate::aver_generated::domain::value::Val::ValInt(size) => Ok(
            crate::aver_generated::domain::value::Val::ValVector(aver_rt::AverVector::new(
                size as usize,
                crate::aver_generated::domain::value::Val::ValInt(fill),
            )),
        ),
        _ => Err(AverStr::from("Vector.new size must be Int")),
    }
}

/// Fast path for Option.withDefault(Vector.get(slotVec, slotIdx), int).
pub fn fastVectorGetOrIntSlot(
    calleeEnv: &aver_rt::AverVector<Val>,
    vecSlot: i64,
    idxSlot: i64,
    defaultValue: i64,
) -> Result<Val, AverStr> {
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
    vecV: &Val,
    idxV: &Val,
    defaultValue: i64,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match vecV.clone() {
        crate::aver_generated::domain::value::Val::ValVector(vec) => match idxV.clone() {
            crate::aver_generated::domain::value::Val::ValInt(idx) => {
                match vec.get(idx as usize).cloned() {
                    Some(v) => Ok(v),
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
    calleeEnv: &aver_rt::AverVector<Val>,
    mapSlot: i64,
    keySlot: i64,
) -> Result<Val, AverStr> {
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
    calleeEnv: &aver_rt::AverVector<Val>,
    mapSlot: i64,
    keySlot: i64,
) -> Result<Val, AverStr> {
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
    calleeEnv: &aver_rt::AverVector<Val>,
    vecSlot: i64,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, vecSlot)?;
    match vecV {
        crate::aver_generated::domain::value::Val::ValVector(vec) => Ok(
            crate::aver_generated::domain::value::Val::ValInt((vec.len() as i64)),
        ),
        _ => Err(AverStr::from("Vector.len expects a Vector")),
    }
}

/// Execute arithmetic on two slot values without entering the full eval.
pub fn fastBinopSlots(
    calleeEnv: &aver_rt::AverVector<Val>,
    op: &BinOp,
    slotA: i64,
    slotB: i64,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let va = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotA)?;
    let vb = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotB)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(&va, &vb, op)
}

/// Execute comparison on two slot values without entering the full eval.
pub fn fastCmpSlots(
    calleeEnv: &aver_rt::AverVector<Val>,
    op: &CmpOp,
    slotA: i64,
    slotB: i64,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let va = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotA)?;
    let vb = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotB)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(&va, &vb, op)
}
