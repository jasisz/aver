#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Apply a binary operation to two integers.
pub fn applyBinop(x: i64, y: i64, op: &BinOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        BinOp::OpAdd => Ok(Val::ValInt((x + y))),
        BinOp::OpSub => Ok(Val::ValInt((x - y))),
        BinOp::OpMul => Ok(Val::ValInt((x * y))),
        BinOp::OpDiv => {
            if (y == 0i64) {
                Err(AverStr::from("division by zero"))
            } else {
                Ok(Val::ValInt((x / y)))
            }
        }
    }
}

/// Apply a binary operation to two floats.
pub fn applyBinopFloat(x: f64, y: f64, op: &BinOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        BinOp::OpAdd => Ok(Val::ValFloat((x + y))),
        BinOp::OpSub => Ok(Val::ValFloat((x - y))),
        BinOp::OpMul => Ok(Val::ValFloat((x * y))),
        BinOp::OpDiv => {
            if (y == 0.0f64) {
                Err(AverStr::from("float division by zero"))
            } else {
                Ok(Val::ValFloat((x / y)))
            }
        }
    }
}

/// Apply a comparison operation to two integers.
pub fn applyCmp(x: i64, y: i64, op: &CmpOp) -> Val {
    crate::cancel_checkpoint();
    match op {
        CmpOp::CmpEq => Val::ValBool((x == y)),
        CmpOp::CmpNeq => Val::ValBool((x != y)),
        CmpOp::CmpLt => Val::ValBool((x < y)),
        CmpOp::CmpGt => Val::ValBool((x > y)),
        CmpOp::CmpLte => Val::ValBool((x <= y)),
        CmpOp::CmpGte => Val::ValBool((x >= y)),
    }
}

/// Apply a comparison to two floats.
pub fn applyCmpFloat(x: f64, y: f64, op: &CmpOp) -> Val {
    crate::cancel_checkpoint();
    match op {
        CmpOp::CmpEq => Val::ValBool((x == y)),
        CmpOp::CmpNeq => Val::ValBool((x != y)),
        CmpOp::CmpLt => Val::ValBool((x < y)),
        CmpOp::CmpGt => Val::ValBool((x > y)),
        CmpOp::CmpLte => Val::ValBool((x <= y)),
        CmpOp::CmpGte => Val::ValBool((x >= y)),
    }
}

/// Apply binary op to two evaluated values.
pub fn evalBinopVals(va: &Val, vb: &Val, op: &BinOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match (va.clone(), vb.clone()) {
        (Val::ValInt(x), Val::ValInt(y)) => applyBinop(x, y, op),
        (Val::ValFloat(x), Val::ValFloat(y)) => applyBinopFloat(x, y, op),
        (Val::ValInt(x), Val::ValFloat(y)) => applyBinopFloat(x as f64, y, op),
        (Val::ValFloat(x), Val::ValInt(y)) => applyBinopFloat(x, y as f64, op),
        (Val::ValStr(x), Val::ValStr(y)) => match op {
            BinOp::OpAdd => Ok(Val::ValStr((x + &y))),
            _ => Err(AverStr::from("strings only support +")),
        },
        _ => Err(AverStr::from("binop type mismatch")),
    }
}

/// Apply comparison to two evaluated values (Int, Float, or String).
pub fn evalCmpVals(va: &Val, vb: &Val, op: &CmpOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match (va.clone(), vb.clone()) {
        (Val::ValInt(x), Val::ValInt(y)) => Ok(applyCmp(x, y, op)),
        (Val::ValFloat(x), Val::ValFloat(y)) => Ok(applyCmpFloat(x, y, op)),
        (Val::ValInt(x), Val::ValFloat(y)) => Ok(applyCmpFloat(x as f64, y, op)),
        (Val::ValFloat(x), Val::ValInt(y)) => Ok(applyCmpFloat(x, y as f64, op)),
        (Val::ValStr(x), Val::ValStr(y)) => Ok(applyStrCmp(x, y, op)),
        (Val::ValBool(x), Val::ValBool(y)) => applyBoolCmp(x, y, op),
        (a, b) => evalCmpRepr(&a, &b, op),
    }
}

/// String comparison: all operators supported.
pub fn applyStrCmp(x: AverStr, y: AverStr, op: &CmpOp) -> Val {
    crate::cancel_checkpoint();
    match op {
        CmpOp::CmpEq => Val::ValBool((x == y)),
        CmpOp::CmpNeq => Val::ValBool((x != y)),
        CmpOp::CmpLt => Val::ValBool((x < y)),
        CmpOp::CmpGt => Val::ValBool((x > y)),
        CmpOp::CmpLte => Val::ValBool((x <= y)),
        CmpOp::CmpGte => Val::ValBool((x >= y)),
    }
}

/// Bool comparison: eq and neq.
pub fn applyBoolCmp(x: bool, y: bool, op: &CmpOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        CmpOp::CmpEq => Ok(Val::ValBool((x == y))),
        CmpOp::CmpNeq => Ok(Val::ValBool((x != y))),
        _ => Err(AverStr::from("bools only support == and !=")),
    }
}

/// Fallback comparison using repr (for variants, lists, etc).
pub fn evalCmpRepr(va: &Val, vb: &Val, op: &CmpOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        CmpOp::CmpEq => Ok(Val::ValBool(
            (crate::aver_generated::domain::value::valRepr(va)
                == crate::aver_generated::domain::value::valRepr(vb)),
        )),
        CmpOp::CmpNeq => Ok(Val::ValBool(
            (crate::aver_generated::domain::value::valRepr(va)
                != crate::aver_generated::domain::value::valRepr(vb)),
        )),
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity((83i64) as usize);
                                __b.push_str(&AverStr::from("comparison "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(cmpOpName(op)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(" not supported for "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::aver_generated::domain::value::valRepr(va)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(" and "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::aver_generated::domain::value::valRepr(vb)),
            )));
            __b
        })),
    }
}

/// Human-readable comparison operator.
pub fn cmpOpName(op: &CmpOp) -> AverStr {
    crate::cancel_checkpoint();
    match op {
        CmpOp::CmpEq => AverStr::from("=="),
        CmpOp::CmpNeq => AverStr::from("!="),
        CmpOp::CmpLt => AverStr::from("<"),
        CmpOp::CmpGt => AverStr::from(">"),
        CmpOp::CmpLte => AverStr::from("<="),
        CmpOp::CmpGte => AverStr::from(">="),
    }
}

/// Read a vector cell and return an integer fallback when the index misses.
#[inline(always)]
pub fn evalVectorGetOrIntVals(vecV: &Val, idxV: &Val, defaultValue: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match evalVectorGetMaybeDefault(vecV, idxV) {
        Ok(maybeV) => match maybeV {
            Some(v) => Ok(v),
            None => Ok(Val::ValInt(defaultValue)),
        },
        Err(err) => Err(err),
    }
}

/// Apply Int.mod and return an integer fallback when the result would be Err.
pub fn evalIntModOrIntVals(aV: &Val, bV: &Val, defaultValue: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match aV.clone() {
        Val::ValInt(a) => match bV.clone() {
            Val::ValInt(b) => {
                if (b == 0i64) {
                    Ok(Val::ValInt(defaultValue))
                } else {
                    Ok(Val::ValInt(
                        (if (b) == 0i64 {
                            Err("Int.mod: divisor must not be zero".to_string())
                        } else {
                            Ok((a).rem_euclid(b))
                        })
                        .into_aver()
                        .unwrap_or(defaultValue),
                    ))
                }
            }
            _ => Err(AverStr::from("expected int argument")),
        },
        _ => Err(AverStr::from("expected int argument")),
    }
}

/// Apply Vector.set and return None when the caller should fall back to its default expression.
pub fn evalVectorSetMaybeDefault(
    vecV: &Val,
    idxV: &Val,
    valueV: &Val,
) -> Result<Option<Val>, AverStr> {
    crate::cancel_checkpoint();
    match vecV.clone() {
        Val::ValVector(vec) => match idxV.clone() {
            Val::ValInt(idx) => {
                if (idx < 0i64) {
                    Ok(None)
                } else {
                    if (idx < (vec.len() as i64)) {
                        Ok(Some(Val::ValVector({
                            let __vec = vec.clone();
                            let __idx = idx as usize;
                            if __idx < __vec.len() {
                                __vec.set_unchecked(__idx, valueV.clone())
                            } else {
                                __vec
                            }
                        })))
                    } else {
                        Ok(None)
                    }
                }
            }
            _ => Err(AverStr::from("Vector.set: second arg must be Int")),
        },
        _ => Err(AverStr::from("Vector.set: first arg must be Vector")),
    }
}

/// Apply Vector.get and return None when the caller should fall back to its default expression.
pub fn evalVectorGetMaybeDefault(vecV: &Val, idxV: &Val) -> Result<Option<Val>, AverStr> {
    crate::cancel_checkpoint();
    match vecV.clone() {
        Val::ValVector(vec) => match idxV.clone() {
            Val::ValInt(idx) => match vec.get(idx as usize).cloned() {
                Some(v) => Ok(Some(v)),
                None => Ok(None),
            },
            _ => Err(AverStr::from("Vector.get: expected (Vector, Int)")),
        },
        _ => Err(AverStr::from("Vector.get: expected (Vector, Int)")),
    }
}
