#[allow(unused_imports)]
use crate::*;

/// Apply a binary operation to two integers.
pub fn applyBinop(
    x @ _: aver_rt::AverInt,
    y @ _: aver_rt::AverInt,
    op @ _: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::BinOp::OpAdd => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                crate::aver_generated::domain::value::Val__ValInt(x.add(&y)),
            ))
        }
        crate::aver_generated::domain::ast::BinOp::OpSub => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                crate::aver_generated::domain::value::Val__ValInt(x.sub(&y)),
            ))
        }
        crate::aver_generated::domain::ast::BinOp::OpMul => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                crate::aver_generated::domain::value::Val__ValInt(x.mul(&y)),
            ))
        }
        crate::aver_generated::domain::ast::BinOp::OpDiv => {
            if (y == aver_rt::AverInt::from_i64(0)) {
                Err(::aver_rt::str_lit!("division by zero"))
            } else {
                Ok(crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(
                        (x).div_euclid(&(y))
                            .unwrap_or(aver_rt::AverInt::from_i64(0)),
                    ),
                ))
            }
        }
    }
}

/// Apply a binary operation to two floats.
pub fn applyBinopFloat(
    x @ _: f64,
    y @ _: f64,
    op @ _: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::BinOp::OpAdd => {
            Ok(crate::aver_generated::domain::value::Val::ValFloat(
                crate::aver_generated::domain::value::Val__ValFloat((x + y)),
            ))
        }
        crate::aver_generated::domain::ast::BinOp::OpSub => {
            Ok(crate::aver_generated::domain::value::Val::ValFloat(
                crate::aver_generated::domain::value::Val__ValFloat((x - y)),
            ))
        }
        crate::aver_generated::domain::ast::BinOp::OpMul => {
            Ok(crate::aver_generated::domain::value::Val::ValFloat(
                crate::aver_generated::domain::value::Val__ValFloat((x * y)),
            ))
        }
        crate::aver_generated::domain::ast::BinOp::OpDiv => {
            if (y == 0.0f64) {
                Err(::aver_rt::str_lit!("float division by zero"))
            } else {
                Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat((x / y)),
                ))
            }
        }
    }
}

/// Apply a comparison operation to two integers.
pub fn applyCmp(
    x @ _: aver_rt::AverInt,
    y @ _: aver_rt::AverInt,
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
) -> crate::aver_generated::domain::value::Val {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x == y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x != y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x < y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x > y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x <= y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x >= y)),
            )
        }
    }
}

/// Apply a comparison to two floats.
pub fn applyCmpFloat(
    x @ _: f64,
    y @ _: f64,
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
) -> crate::aver_generated::domain::value::Val {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x == y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x != y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x < y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x > y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x <= y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x >= y)),
            )
        }
    }
}

/// Apply binary op to two evaluated values.
pub fn evalBinopVals(
    mut va @ _: crate::aver_generated::domain::value::Val,
    mut vb @ _: crate::aver_generated::domain::value::Val,
    op @ _: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = (va, vb);
        match (__t.0.view(), __t.1.view()) {
            (
                crate::aver_generated::domain::value::Val__view::ValInt(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValInt(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                crate::aver_generated::domain::eval::ops::applyBinop(x, y, op)
            }
            (
                crate::aver_generated::domain::value::Val__view::ValFloat(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValFloat(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                crate::aver_generated::domain::eval::ops::applyBinopFloat(x, y, op)
            }
            (
                crate::aver_generated::domain::value::Val__view::ValInt(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValFloat(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                {
                    let __arg13 @ _ = x.to_f64();
                    crate::aver_generated::domain::eval::ops::applyBinopFloat(__arg13, y, op)
                }
            }
            (
                crate::aver_generated::domain::value::Val__view::ValFloat(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValInt(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                {
                    let __arg14 @ _ = y.to_f64();
                    crate::aver_generated::domain::eval::ops::applyBinopFloat(x, __arg14, op)
                }
            }
            (
                crate::aver_generated::domain::value::Val__view::ValStr(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValStr(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                match op {
                    crate::aver_generated::domain::ast::BinOp::OpAdd => {
                        Ok(crate::aver_generated::domain::value::Val::ValStr(
                            crate::aver_generated::domain::value::Val__ValStr((x + &y)),
                        ))
                    }
                    _ => Err(::aver_rt::str_lit!("strings only support +")),
                }
            }
            _ => Err(::aver_rt::str_lit!("binop type mismatch")),
        }
    }
}

/// Apply unary minus to an evaluated value. Numeric only.
pub fn evalNegVals(
    v @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValInt(__n_fields) => {
                let n = __n_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(
                        aver_rt::AverInt::from_i64(0).sub(&n),
                    ),
                ))
            }
            crate::aver_generated::domain::value::Val__view::ValFloat(__f_fields) => {
                let f = __f_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat((0.0f64 - f)),
                ))
            }
            _ => Err(::aver_rt::str_lit!("unary '-' requires Int or Float")),
        }
    }
}

/// Apply comparison to two evaluated values (Int, Float, or String).
pub fn evalCmpVals(
    mut va @ _: crate::aver_generated::domain::value::Val,
    mut vb @ _: crate::aver_generated::domain::value::Val,
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = (va, vb);
        match (__t.0.view(), __t.1.view()) {
            (
                crate::aver_generated::domain::value::Val__view::ValInt(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValInt(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                Ok(crate::aver_generated::domain::eval::ops::applyCmp(x, y, op))
            }
            (
                crate::aver_generated::domain::value::Val__view::ValFloat(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValFloat(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                Ok(crate::aver_generated::domain::eval::ops::applyCmpFloat(
                    x, y, op,
                ))
            }
            (
                crate::aver_generated::domain::value::Val__view::ValInt(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValFloat(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                Ok({
                    let __arg17 @ _ = x.to_f64();
                    crate::aver_generated::domain::eval::ops::applyCmpFloat(__arg17, y, op)
                })
            }
            (
                crate::aver_generated::domain::value::Val__view::ValFloat(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValInt(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                Ok({
                    let __arg18 @ _ = y.to_f64();
                    crate::aver_generated::domain::eval::ops::applyCmpFloat(x, __arg18, op)
                })
            }
            (
                crate::aver_generated::domain::value::Val__view::ValStr(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValStr(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                Ok(crate::aver_generated::domain::eval::ops::applyStrCmp(
                    x, y, op,
                ))
            }
            (
                crate::aver_generated::domain::value::Val__view::ValBool(__x_fields),
                crate::aver_generated::domain::value::Val__view::ValBool(__y_fields),
            ) => {
                let x = __x_fields.0.clone();
                let y = __y_fields.0.clone();
                crate::aver_generated::domain::eval::ops::applyBoolCmp(x, y, op)
            }
            (_, _) => {
                let a = __t.0.clone();
                let b = __t.1.clone();
                crate::aver_generated::domain::eval::ops::evalCmpRepr(&a, &b, op)
            }
        }
    }
}

/// String comparison: all operators supported.
pub fn applyStrCmp(
    x @ _: AverStr,
    y @ _: AverStr,
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
) -> crate::aver_generated::domain::value::Val {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x == y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x != y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x < y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x > y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x <= y)),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x >= y)),
            )
        }
    }
}

/// Bool comparison: eq and neq.
pub fn applyBoolCmp(
    x @ _: bool,
    y @ _: bool,
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            Ok(crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x == y)),
            ))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            Ok(crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool((x != y)),
            ))
        }
        _ => Err(::aver_rt::str_lit!("bools only support == and !=")),
    }
}

/// Fallback comparison using repr (for variants, lists, etc).
pub fn evalCmpRepr(
    va @ _: &crate::aver_generated::domain::value::Val,
    vb @ _: &crate::aver_generated::domain::value::Val,
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            Ok(crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool(
                    (crate::aver_generated::domain::value::valRepr(va)
                        == crate::aver_generated::domain::value::valRepr(vb)),
                ),
            ))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            Ok(crate::aver_generated::domain::value::Val::ValBool(
                crate::aver_generated::domain::value::Val__ValBool(
                    (crate::aver_generated::domain::value::valRepr(va)
                        != crate::aver_generated::domain::value::valRepr(vb)),
                ),
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
                                __b.push_str(&::aver_rt::str_lit!("comparison "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::aver_generated::domain::eval::ops::cmpOpName(op)),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" not supported for "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::aver_generated::domain::value::valRepr(va)),
                    )));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" and "));
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
pub fn cmpOpName(op @ _: &crate::aver_generated::domain::ast::CmpOp) -> AverStr {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            ::aver_rt::str_lit!("==")
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            ::aver_rt::str_lit!("!=")
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            ::aver_rt::str_lit!("<")
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            ::aver_rt::str_lit!(">")
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            ::aver_rt::str_lit!("<=")
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            ::aver_rt::str_lit!(">=")
        }
    }
}

/// Read a vector cell and return an integer fallback when the index misses.
#[inline(always)]
pub fn evalVectorGetOrIntVals(
    vecV @ _: &crate::aver_generated::domain::value::Val,
    idxV @ _: &crate::aver_generated::domain::value::Val,
    defaultValue @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::ops::evalVectorGetMaybeDefault(vecV, idxV) {
        Ok(maybeV @ _) => match maybeV {
            Some(v @ _) => Ok(v),
            None => Ok(crate::aver_generated::domain::value::Val::ValInt(
                crate::aver_generated::domain::value::Val__ValInt(defaultValue),
            )),
        },
        Err(err @ _) => Err(err),
    }
}

/// Apply Int.mod and return an integer fallback when the result would be Err.
pub fn evalIntModOrIntVals(
    aV @ _: &crate::aver_generated::domain::value::Val,
    bV @ _: &crate::aver_generated::domain::value::Val,
    defaultValue @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = aV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValInt(__a_fields) => {
                let a = __a_fields.0.clone();
                {
                    let __t = bV;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValInt(__b_fields) => {
                            let b = __b_fields.0.clone();
                            if (b == aver_rt::AverInt::from_i64(0)) {
                                Ok(crate::aver_generated::domain::value::Val::ValInt(
                                    crate::aver_generated::domain::value::Val__ValInt(defaultValue),
                                ))
                            } else {
                                Ok(crate::aver_generated::domain::value::Val::ValInt(
                                    crate::aver_generated::domain::value::Val__ValInt(
                                        (match (a).rem_euclid(&(b)) {
                                            Some(__r) => Ok(__r),
                                            None => Err("division by zero".to_string()),
                                        })
                                        .into_aver()
                                        .unwrap_or(defaultValue),
                                    ),
                                ))
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("expected int argument")),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("expected int argument")),
        }
    }
}

/// Apply Vector.set and return None when the caller should fall back to its default expression.
pub fn evalVectorSetMaybeDefault(
    vecV @ _: &crate::aver_generated::domain::value::Val,
    idxV @ _: &crate::aver_generated::domain::value::Val,
    valueV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<Option<crate::aver_generated::domain::value::Val>, AverStr> {
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
                            if (idx < aver_rt::AverInt::from_i64(0)) {
                                Ok(None)
                            } else {
                                if (idx < aver_rt::AverInt::from_i64(vec.len() as i64)) {
                                    Ok(Some(crate::aver_generated::domain::value::Val::ValVector(
                                        crate::aver_generated::domain::value::Val__ValVector({
                                            let __vec = vec.clone();
                                            match (idx).to_usize() {
                                                Some(__idx) if __idx < __vec.len() => {
                                                    __vec.set_unchecked(__idx, valueV.clone())
                                                }
                                                _ => __vec,
                                            }
                                        }),
                                    )))
                                } else {
                                    Ok(None)
                                }
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("Vector.set: second arg must be Int")),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("Vector.set: first arg must be Vector")),
        }
    }
}

/// Apply Vector.get and return None when the caller should fall back to its default expression.
pub fn evalVectorGetMaybeDefault(
    vecV @ _: &crate::aver_generated::domain::value::Val,
    idxV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<Option<crate::aver_generated::domain::value::Val>, AverStr> {
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
                                Some(v @ _) => Ok(Some(v)),
                                None => Ok(None),
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("Vector.get: expected (Vector, Int)")),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("Vector.get: expected (Vector, Int)")),
        }
    }
}
