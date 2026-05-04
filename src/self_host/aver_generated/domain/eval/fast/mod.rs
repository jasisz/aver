#[allow(unused_imports)]
use crate::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::common::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::ops::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::slots::*;

/// Execute a leaf-like fast path directly from slots.
pub fn runFastLeafSlot(leaf: &FastLeaf, calleeEnv: &aver_rt::AverVector<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match leaf.clone() {
        FastLeaf::LeafConstInt(n) => {
            Ok(Val::ValInt(n))
        },
        FastLeaf::LeafConstFloat(f) => {
            Ok(Val::ValFloat(f))
        },
        FastLeaf::LeafConstStr(s) => {
            Ok(Val::ValStr(s))
        },
        FastLeaf::LeafConstBool(b) => {
            Ok(Val::ValBool(b))
        },
        FastLeaf::LeafSlot(slot) => {
            crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)
        },
        FastLeaf::LeafFieldAccess(slot, field) => {
            fastFieldAccessSlot(calleeEnv, slot, field)
        },
        FastLeaf::LeafMapGet(mapSlot, keySlot) => {
            fastMapGetSlot(calleeEnv, mapSlot, keySlot)
        },
        FastLeaf::LeafMapSet(mapSlot, keySlot, valueSlot) => {
            fastMapSetSlot(calleeEnv, mapSlot, keySlot, valueSlot)
        },
        FastLeaf::LeafMapHas(mapSlot, keySlot) => {
            fastMapHasSlot(calleeEnv, mapSlot, keySlot)
        },
        FastLeaf::LeafMapRemove(mapSlot, keySlot) => {
            fastMapRemoveSlot(calleeEnv, mapSlot, keySlot)
        },
        FastLeaf::LeafVectorNew(sizeSlot, fill) => {
            fastVectorNewSlot(calleeEnv, sizeSlot, fill)
        },
        FastLeaf::LeafVectorLen(vecSlot) => {
            fastVectorLenSlot(calleeEnv, vecSlot)
        },
        FastLeaf::LeafVectorGetOrInt(vecSlot, idxSlot, defaultValue) => {
            fastVectorGetOrIntSlot(calleeEnv, vecSlot, idxSlot, defaultValue)
        },
        FastLeaf::LeafBinopSlots(op, slotA, slotB) => {
            fastBinopSlots(calleeEnv, &op, slotA, slotB)
        },
        FastLeaf::LeafCmpSlots(op, slotA, slotB) => {
            fastCmpSlots(calleeEnv, &op, slotA, slotB)
        }
    }
}

/// Execute a bool-slot branch with precomputed leaf results.
pub fn fastBoolSlotBranch(calleeEnv: &aver_rt::AverVector<Val>, slot: i64, thenLeaf: &FastLeaf, elseLeaf: &FastLeaf) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let slotV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    match slotV {
        Val::ValBool(cond) => {
            selectFastLeaf(cond, thenLeaf, elseLeaf, calleeEnv)
        },
        _ => {
            Err(AverStr::from("fast bool branch expects Bool slot"))
        }
    }
}

/// Execute an int equality branch with precomputed leaf results.
pub fn fastEqIntBranch(calleeEnv: &aver_rt::AverVector<Val>, slot: i64, expected: i64, thenLeaf: &FastLeaf, elseLeaf: &FastLeaf) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let slotV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    match slotV {
        Val::ValInt(actual) => {
            selectFastLeaf((actual == expected), thenLeaf, elseLeaf, calleeEnv)
        },
        _ => {
            Err(AverStr::from("fast int branch expects Int slot"))
        }
    }
}

/// Execute a string equality branch with precomputed leaf results.
pub fn fastEqStringBranch(calleeEnv: &aver_rt::AverVector<Val>, slot: i64, expected: AverStr, thenLeaf: &FastLeaf, elseLeaf: &FastLeaf) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let slotV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    match slotV {
        Val::ValStr(actual) => {
            selectFastLeaf((actual == expected), thenLeaf, elseLeaf, calleeEnv)
        },
        _ => {
            Err(AverStr::from("fast string branch expects String slot"))
        }
    }
}

/// Execute an int less-than branch with precomputed leaf results.
pub fn fastLtIntSlotsBranch(calleeEnv: &aver_rt::AverVector<Val>, lhsSlot: i64, rhsSlot: i64, thenLeaf: &FastLeaf, elseLeaf: &FastLeaf) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let lhsV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, lhsSlot)?;
    let rhsV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, rhsSlot)?;
    match lhsV {
        Val::ValInt(lhs) => {
            match rhsV {
        Val::ValInt(rhs) => {
            selectFastLeaf((lhs < rhs), thenLeaf, elseLeaf, calleeEnv)
        },
        _ => {
            Err(AverStr::from("fast lt branch expects Int rhs slot"))
        }
    }
        },
        _ => {
            Err(AverStr::from("fast lt branch expects Int lhs slot"))
        }
    }
}

/// Choose and execute one of two leaf results.
#[inline(always)]
pub fn selectFastLeaf(cond: bool, thenLeaf: &FastLeaf, elseLeaf: &FastLeaf, calleeEnv: &aver_rt::AverVector<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    if cond { runFastLeafSlot(thenLeaf, calleeEnv) } else { runFastLeafSlot(elseLeaf, calleeEnv) }
}

/// Execute a two-arm [] / [h, ..t] list branch with precomputed leaf bodies.
pub fn fastListSlotBranch(calleeEnv: &aver_rt::AverVector<Val>, slot: i64, emptyLeaf: &FastLeaf, headSlot: i64, tailSlot: i64, consLeaf: &FastLeaf) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let listV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    match listV {
        Val::ValList(items) => {
            fastListSlotBranchItems(&items, calleeEnv, emptyLeaf, headSlot, tailSlot, consLeaf)
        },
        _ => {
            Err(AverStr::from("no matching arm"))
        }
    }
}

/// Finish [] / [h, ..t] list branching once the list payload is extracted.
#[inline(always)]
pub fn fastListSlotBranchItems(items: &aver_rt::AverList<Val>, calleeEnv: &aver_rt::AverVector<Val>, emptyLeaf: &FastLeaf, headSlot: i64, tailSlot: i64, consLeaf: &FastLeaf) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(items.clone(), [] => runFastLeafSlot(emptyLeaf, calleeEnv), [head, tail] => runFastLeafSlot(consLeaf, &crate::aver_generated::domain::eval::slots::setSlot(&crate::aver_generated::domain::eval::slots::setSlot(calleeEnv, headSlot, &head), tailSlot, &Val::ValList(tail))))
}

/// Read a record field directly from a resolved slot.
pub fn fastFieldAccessSlot(calleeEnv: &aver_rt::AverVector<Val>, slot: i64, field: AverStr) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let recordV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot)?;
    match recordV {
        Val::ValRecord(_, fields) => {
            crate::aver_generated::domain::eval::common::lookupField(fields, field)
        },
        _ => {
            Err(AverStr::from("field access on non-record"))
        }
    }
}

/// Read a map key directly from resolved slots.
pub fn fastMapGetSlot(calleeEnv: &aver_rt::AverVector<Val>, mapSlot: i64, keySlot: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    fastMapGetSlotInner(&mapV, &keyV)
}

/// Look up a key in a ValMap without going through builtin dispatch.
pub fn fastMapGetSlotInner(mapV: &Val, keyV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        Val::ValMap(m) => {
            match m.get(&crate::aver_generated::domain::value::mapKeyRepr(keyV)).cloned() { Some(v) => { Ok(Val::ValSome(std::sync::Arc::new(v))) }, None => { Ok(Val::ValNone.clone()) } }
        },
        _ => {
            Err(AverStr::from("Map.get requires a Map"))
        }
    }
}

/// Update a map directly from resolved slots.
pub fn fastMapSetSlot(calleeEnv: &aver_rt::AverVector<Val>, mapSlot: i64, keySlot: i64, valueSlot: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    let valueV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, valueSlot)?;
    fastMapSetSlotInner(&mapV, &keyV, &valueV)
}

/// Set a key in a ValMap without going through builtin dispatch.
pub fn fastMapSetSlotInner(mapV: &Val, keyV: &Val, valueV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match mapV.clone() {
        Val::ValMap(m) => {
            Ok(Val::ValMap(m.insert_owned(crate::aver_generated::domain::value::mapKeyRepr(keyV), valueV.clone())))
        },
        _ => {
            Err(AverStr::from("Map.set requires a Map"))
        }
    }
}

/// Fast path for Vector.new(slot, int) wrappers.
pub fn fastVectorNewSlot(calleeEnv: &aver_rt::AverVector<Val>, sizeSlot: i64, fill: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let sizeV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, sizeSlot)?;
    fastVectorNewSlotInner(&sizeV, fill)
}

/// Allocate a vector when the wrapper shape is known in advance.
pub fn fastVectorNewSlotInner(sizeV: &Val, fill: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match sizeV.clone() {
        Val::ValInt(size) => {
            Ok(Val::ValVector(aver_rt::AverVector::new(size as usize, Val::ValInt(fill))))
        },
        _ => {
            Err(AverStr::from("Vector.new size must be Int"))
        }
    }
}

/// Fast path for Option.withDefault(Vector.get(slotVec, slotIdx), int).
pub fn fastVectorGetOrIntSlot(calleeEnv: &aver_rt::AverVector<Val>, vecSlot: i64, idxSlot: i64, defaultValue: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, vecSlot)?;
    let idxV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, idxSlot)?;
    fastVectorGetOrIntSlotInner(&vecV, &idxV, defaultValue)
}

/// Read a vector cell with an integer default without going through builtin dispatch.
pub fn fastVectorGetOrIntSlotInner(vecV: &Val, idxV: &Val, defaultValue: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match vecV.clone() {
        Val::ValVector(vec) => {
            match idxV.clone() {
        Val::ValInt(idx) => {
            match vec.get(idx as usize).cloned() { Some(v) => { Ok(v) }, None => { Ok(Val::ValInt(defaultValue)) } }
        },
        _ => {
            Err(AverStr::from("Vector.get index must be Int"))
        }
    }
        },
        _ => {
            Err(AverStr::from("Vector.get expects a Vector"))
        }
    }
}

/// Check map membership directly from resolved slots.
pub fn fastMapHasSlot(calleeEnv: &aver_rt::AverVector<Val>, mapSlot: i64, keySlot: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    match mapV {
        Val::ValMap(m) => {
            Ok(Val::ValBool(m.contains_key(&crate::aver_generated::domain::value::mapKeyRepr(&keyV))))
        },
        _ => {
            Err(AverStr::from("Map.has requires a Map"))
        }
    }
}

/// Remove a map key directly from resolved slots.
pub fn fastMapRemoveSlot(calleeEnv: &aver_rt::AverVector<Val>, mapSlot: i64, keySlot: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let mapV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, mapSlot)?;
    let keyV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, keySlot)?;
    match mapV {
        Val::ValMap(m) => {
            Ok(Val::ValMap(m.remove_owned(&crate::aver_generated::domain::value::mapKeyRepr(&keyV))))
        },
        _ => {
            Err(AverStr::from("Map.remove requires a Map"))
        }
    }
}

/// Get vector length directly from a resolved slot.
pub fn fastVectorLenSlot(calleeEnv: &aver_rt::AverVector<Val>, vecSlot: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, vecSlot)?;
    match vecV {
        Val::ValVector(vec) => {
            Ok(Val::ValInt((vec.len() as i64)))
        },
        _ => {
            Err(AverStr::from("Vector.len expects a Vector"))
        }
    }
}

/// Execute arithmetic on two slot values without entering the full eval.
pub fn fastBinopSlots(calleeEnv: &aver_rt::AverVector<Val>, op: &BinOp, slotA: i64, slotB: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let va = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotA)?;
    let vb = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotB)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(&va, &vb, op)
}

/// Execute comparison on two slot values without entering the full eval.
pub fn fastCmpSlots(calleeEnv: &aver_rt::AverVector<Val>, op: &CmpOp, slotA: i64, slotB: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let va = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotA)?;
    let vb = crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slotB)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(&va, &vb, op)
}
