#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Apply a binary operation to two integers.
pub fn applyBinop(
    x: aver_rt::AverInt,
    y: aver_rt::AverInt,
    op: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::BinOp::OpAdd => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(x.add(&y)))
        }
        crate::aver_generated::domain::ast::BinOp::OpSub => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(x.sub(&y)))
        }
        crate::aver_generated::domain::ast::BinOp::OpMul => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(x.mul(&y)))
        }
        crate::aver_generated::domain::ast::BinOp::OpDiv => {
            if (y == aver_rt::AverInt::from_i64(0)) {
                Err(AverStr::from("division by zero"))
            } else {
                Ok(crate::aver_generated::domain::value::Val::ValInt(
                    (x).div_euclid(&(y))
                        .unwrap_or(aver_rt::AverInt::from_i64(0)),
                ))
            }
        }
    }
}

/// Apply a binary operation to two floats.
pub fn applyBinopFloat(
    x: f64,
    y: f64,
    op: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::BinOp::OpAdd => {
            Ok(crate::aver_generated::domain::value::Val::ValFloat((x + y)))
        }
        crate::aver_generated::domain::ast::BinOp::OpSub => {
            Ok(crate::aver_generated::domain::value::Val::ValFloat((x - y)))
        }
        crate::aver_generated::domain::ast::BinOp::OpMul => {
            Ok(crate::aver_generated::domain::value::Val::ValFloat((x * y)))
        }
        crate::aver_generated::domain::ast::BinOp::OpDiv => {
            if (y == 0.0f64) {
                Err(AverStr::from("float division by zero"))
            } else {
                Ok(crate::aver_generated::domain::value::Val::ValFloat((x / y)))
            }
        }
    }
}

/// Apply a comparison operation to two integers.
pub fn applyCmp(
    x: aver_rt::AverInt,
    y: aver_rt::AverInt,
    op: &crate::aver_generated::domain::ast::CmpOp,
) -> crate::aver_generated::domain::value::Val {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            crate::aver_generated::domain::value::Val::ValBool((x == y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            crate::aver_generated::domain::value::Val::ValBool((x != y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            crate::aver_generated::domain::value::Val::ValBool((x < y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            crate::aver_generated::domain::value::Val::ValBool((x > y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            crate::aver_generated::domain::value::Val::ValBool((x <= y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            crate::aver_generated::domain::value::Val::ValBool((x >= y))
        }
    }
}

/// Apply a comparison to two floats.
pub fn applyCmpFloat(
    x: f64,
    y: f64,
    op: &crate::aver_generated::domain::ast::CmpOp,
) -> crate::aver_generated::domain::value::Val {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            crate::aver_generated::domain::value::Val::ValBool((x == y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            crate::aver_generated::domain::value::Val::ValBool((x != y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            crate::aver_generated::domain::value::Val::ValBool((x < y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            crate::aver_generated::domain::value::Val::ValBool((x > y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            crate::aver_generated::domain::value::Val::ValBool((x <= y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            crate::aver_generated::domain::value::Val::ValBool((x >= y))
        }
    }
}

/// Apply binary op to two evaluated values.
pub fn evalBinopVals(
    va: &crate::aver_generated::domain::value::Val,
    vb: &crate::aver_generated::domain::value::Val,
    op: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match (va.clone(), vb.clone()) {
        (
            crate::aver_generated::domain::value::Val::ValInt(x),
            crate::aver_generated::domain::value::Val::ValInt(y),
        ) => crate::aver_generated::domain::eval::ops::applyBinop(x, y, op),
        (
            crate::aver_generated::domain::value::Val::ValFloat(x),
            crate::aver_generated::domain::value::Val::ValFloat(y),
        ) => crate::aver_generated::domain::eval::ops::applyBinopFloat(x, y, op),
        (
            crate::aver_generated::domain::value::Val::ValInt(x),
            crate::aver_generated::domain::value::Val::ValFloat(y),
        ) => crate::aver_generated::domain::eval::ops::applyBinopFloat(x.to_f64(), y, op),
        (
            crate::aver_generated::domain::value::Val::ValFloat(x),
            crate::aver_generated::domain::value::Val::ValInt(y),
        ) => crate::aver_generated::domain::eval::ops::applyBinopFloat(x, y.to_f64(), op),
        (
            crate::aver_generated::domain::value::Val::ValStr(x),
            crate::aver_generated::domain::value::Val::ValStr(y),
        ) => match op {
            crate::aver_generated::domain::ast::BinOp::OpAdd => {
                Ok(crate::aver_generated::domain::value::Val::ValStr((x + &y)))
            }
            _ => Err(AverStr::from("strings only support +")),
        },
        _ => Err(AverStr::from("binop type mismatch")),
    }
}

/// Apply unary minus to an evaluated value. Numeric only.
pub fn evalNegVals(
    v: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match v.clone() {
        crate::aver_generated::domain::value::Val::ValInt(n) => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                aver_rt::AverInt::from_i64(0).sub(&n),
            ))
        }
        crate::aver_generated::domain::value::Val::ValFloat(f) => Ok(
            crate::aver_generated::domain::value::Val::ValFloat((0.0f64 - f)),
        ),
        _ => Err(AverStr::from("unary '-' requires Int or Float")),
    }
}

/// Apply comparison to two evaluated values (Int, Float, or String).
pub fn evalCmpVals(
    va: &crate::aver_generated::domain::value::Val,
    vb: &crate::aver_generated::domain::value::Val,
    op: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match (va.clone(), vb.clone()) {
        (
            crate::aver_generated::domain::value::Val::ValInt(x),
            crate::aver_generated::domain::value::Val::ValInt(y),
        ) => Ok(crate::aver_generated::domain::eval::ops::applyCmp(x, y, op)),
        (
            crate::aver_generated::domain::value::Val::ValFloat(x),
            crate::aver_generated::domain::value::Val::ValFloat(y),
        ) => Ok(crate::aver_generated::domain::eval::ops::applyCmpFloat(
            x, y, op,
        )),
        (
            crate::aver_generated::domain::value::Val::ValInt(x),
            crate::aver_generated::domain::value::Val::ValFloat(y),
        ) => Ok(crate::aver_generated::domain::eval::ops::applyCmpFloat(
            x.to_f64(),
            y,
            op,
        )),
        (
            crate::aver_generated::domain::value::Val::ValFloat(x),
            crate::aver_generated::domain::value::Val::ValInt(y),
        ) => Ok(crate::aver_generated::domain::eval::ops::applyCmpFloat(
            x,
            y.to_f64(),
            op,
        )),
        (
            crate::aver_generated::domain::value::Val::ValStr(x),
            crate::aver_generated::domain::value::Val::ValStr(y),
        ) => Ok(crate::aver_generated::domain::eval::ops::applyStrCmp(
            x, y, op,
        )),
        (
            crate::aver_generated::domain::value::Val::ValBool(x),
            crate::aver_generated::domain::value::Val::ValBool(y),
        ) => crate::aver_generated::domain::eval::ops::applyBoolCmp(x, y, op),
        (a, b) => crate::aver_generated::domain::eval::ops::evalCmpRepr(&a, &b, op),
    }
}

/// String comparison: all operators supported.
pub fn applyStrCmp(
    x: AverStr,
    y: AverStr,
    op: &crate::aver_generated::domain::ast::CmpOp,
) -> crate::aver_generated::domain::value::Val {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            crate::aver_generated::domain::value::Val::ValBool((x == y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            crate::aver_generated::domain::value::Val::ValBool((x != y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            crate::aver_generated::domain::value::Val::ValBool((x < y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            crate::aver_generated::domain::value::Val::ValBool((x > y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            crate::aver_generated::domain::value::Val::ValBool((x <= y))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            crate::aver_generated::domain::value::Val::ValBool((x >= y))
        }
    }
}

/// Bool comparison: eq and neq.
pub fn applyBoolCmp(
    x: bool,
    y: bool,
    op: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            Ok(crate::aver_generated::domain::value::Val::ValBool((x == y)))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            Ok(crate::aver_generated::domain::value::Val::ValBool((x != y)))
        }
        _ => Err(AverStr::from("bools only support == and !=")),
    }
}

/// Fallback comparison using repr (for variants, lists, etc).
pub fn evalCmpRepr(
    va: &crate::aver_generated::domain::value::Val,
    vb: &crate::aver_generated::domain::value::Val,
    op: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            Ok(crate::aver_generated::domain::value::Val::ValBool(
                (crate::aver_generated::domain::value::valRepr(va)
                    == crate::aver_generated::domain::value::valRepr(vb)),
            ))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            Ok(crate::aver_generated::domain::value::Val::ValBool(
                (crate::aver_generated::domain::value::valRepr(va)
                    != crate::aver_generated::domain::value::valRepr(vb)),
            ))
        }
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(83)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("comparison "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::aver_generated::domain::eval::ops::cmpOpName(op)),
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
pub fn cmpOpName(op: &crate::aver_generated::domain::ast::CmpOp) -> AverStr {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => AverStr::from("=="),
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => AverStr::from("!="),
        crate::aver_generated::domain::ast::CmpOp::CmpLt => AverStr::from("<"),
        crate::aver_generated::domain::ast::CmpOp::CmpGt => AverStr::from(">"),
        crate::aver_generated::domain::ast::CmpOp::CmpLte => AverStr::from("<="),
        crate::aver_generated::domain::ast::CmpOp::CmpGte => AverStr::from(">="),
    }
}

/// Read a vector cell and return an integer fallback when the index misses.
#[inline(always)]
pub fn evalVectorGetOrIntVals(
    vecV: &crate::aver_generated::domain::value::Val,
    idxV: &crate::aver_generated::domain::value::Val,
    defaultValue: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::ops::evalVectorGetMaybeDefault(vecV, idxV) {
        Ok(maybeV @ _) => match maybeV {
            Some(v @ _) => Ok(v),
            None => Ok(crate::aver_generated::domain::value::Val::ValInt(
                defaultValue,
            )),
        },
        Err(err @ _) => Err(err),
    }
}

/// Apply Int.mod and return an integer fallback when the result would be Err.
pub fn evalIntModOrIntVals(
    aV: &crate::aver_generated::domain::value::Val,
    bV: &crate::aver_generated::domain::value::Val,
    defaultValue: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match aV.clone() {
        crate::aver_generated::domain::value::Val::ValInt(a) => match bV.clone() {
            crate::aver_generated::domain::value::Val::ValInt(b) => {
                if (b == aver_rt::AverInt::from_i64(0)) {
                    Ok(crate::aver_generated::domain::value::Val::ValInt(
                        defaultValue,
                    ))
                } else {
                    Ok(crate::aver_generated::domain::value::Val::ValInt(
                        (match (a).rem_euclid(&(b)) {
                            Some(__r) => Ok(__r),
                            None => Err("division by zero".to_string()),
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
    vecV: &crate::aver_generated::domain::value::Val,
    idxV: &crate::aver_generated::domain::value::Val,
    valueV: &crate::aver_generated::domain::value::Val,
) -> Result<Option<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    match vecV.clone() {
        crate::aver_generated::domain::value::Val::ValVector(vec) => match idxV.clone() {
            crate::aver_generated::domain::value::Val::ValInt(idx) => {
                if (idx < aver_rt::AverInt::from_i64(0)) {
                    Ok(None)
                } else {
                    if (idx < aver_rt::AverInt::from_i64(vec.len() as i64)) {
                        Ok(Some(crate::aver_generated::domain::value::Val::ValVector(
                            {
                                let __vec = vec.clone();
                                match (idx).to_usize() {
                                    Some(__idx) if __idx < __vec.len() => {
                                        __vec.set_unchecked(__idx, valueV.clone())
                                    }
                                    _ => __vec,
                                }
                            },
                        )))
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
pub fn evalVectorGetMaybeDefault(
    vecV: &crate::aver_generated::domain::value::Val,
    idxV: &crate::aver_generated::domain::value::Val,
) -> Result<Option<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    match vecV.clone() {
        crate::aver_generated::domain::value::Val::ValVector(vec) => match idxV.clone() {
            crate::aver_generated::domain::value::Val::ValInt(idx) => {
                match (idx).to_usize().and_then(|__i| vec.get(__i).cloned()) {
                    Some(v @ _) => Ok(Some(v)),
                    None => Ok(None),
                }
            }
            _ => Err(AverStr::from("Vector.get: expected (Vector, Int)")),
        },
        _ => Err(AverStr::from("Vector.get: expected (Vector, Int)")),
    }
}
