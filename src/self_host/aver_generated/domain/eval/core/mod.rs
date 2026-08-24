#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::common::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::fast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::ops::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::records::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::slots::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::store::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::match_mod::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

#[derive(Clone, Debug, PartialEq)]
pub enum SlotTailStep {
    SlotTailDone(crate::aver_generated::domain::value::Val),
    SlotTailRecurEnv(aver_rt::AverVector<crate::aver_generated::domain::value::Val>),
}

impl aver_rt::AverDisplay for SlotTailStep {
    fn aver_display(&self) -> String {
        match self {
            SlotTailStep::SlotTailDone(f0) => format!("SlotTailDone({})", f0.aver_display_inner()),
            SlotTailStep::SlotTailRecurEnv(f0) => {
                format!("SlotTailRecurEnv({})", f0.aver_display_inner())
            }
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for SlotTailStep {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("SlotTailStep".to_string()),
        );
        match self {
            SlotTailStep::SlotTailDone(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("SlotTailDone".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            SlotTailStep::SlotTailRecurEnv(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("SlotTailRecurEnv".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "SlotTailStep" {
            return Err(format!(
                "$variant type mismatch: expected SlotTailStep, got {}",
                type_name
            ));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name")
                .ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields")
                .ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "SlotTailDone" => Ok(SlotTailStep::SlotTailDone(
                <Val as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant SlotTailDone missing field #{}", 0))?,
                )?,
            )),
            "SlotTailRecurEnv" => Ok(SlotTailStep::SlotTailRecurEnv(
                <aver_rt::AverVector<Val> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant SlotTailRecurEnv missing field #{}", 0))?,
                )?,
            )),
            _ => Err(format!(
                "unknown variant '{}' for SlotTailStep",
                variant_name
            )),
        }
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    EvalExpr(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalExprBasic(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalExprInternal(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalExprAggregate(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalExprCalls(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalBoolBranch(
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalMatchExpr(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalCallBuiltinById(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalCallBuiltinMaybeSpecial(
        AverStr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalOptionWithDefaultExpr(
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalOptionWithDefaultExprInner(
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorSetWithDefaultExpr(
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorSetWithDefaultExprValues(
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorSetWithDefaultExprResult(
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorGetWithDefaultExpr(
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorGetWithDefaultExprValues(
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorGetWithDefaultExprResult(
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalMatch(
        crate::aver_generated::domain::value::Val,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::EvalExpr(mut expr, mut env) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::core::evalImmediateNamedExpr(&expr) {
                    Some(v @ _) => return Ok(v),
                    None => __MutualTco1::EvalExprBasic(expr, env),
                }
            }
            __MutualTco1::EvalExprBasic(mut expr, mut env) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                        cond,
                        thenExpr,
                        elseExpr,
                    ) => {
                        let cond = (*cond).clone();
                        let thenExpr = (*thenExpr).clone();
                        let elseExpr = (*elseExpr).clone();
                        __MutualTco1::EvalBoolBranch(cond, thenExpr, elseExpr, env)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprVar(name) => {
                        return crate::aver_generated::domain::eval::core::evalVar(
                            name, &env, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprSlot(_) => {
                        return Err(AverStr::from("ExprSlot in map-based eval path"));
                    }
                    crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(_, _, _) => {
                        return Err(AverStr::from("ExprBinopSlotInt in map-based eval path"));
                    }
                    crate::aver_generated::domain::ast::Expr::ExprBinopSlots(_, _, _) => {
                        return Err(AverStr::from("ExprBinopSlots in map-based eval path"));
                    }
                    _ => __MutualTco1::EvalExprInternal(expr, env),
                }
            }
            __MutualTco1::EvalExprInternal(mut expr, mut env) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(_, _, _) => {
                        return Err(AverStr::from("ExprCmpSlotInt in map-based eval path"));
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCmpSlots(_, _, _) => {
                        return Err(AverStr::from("ExprCmpSlots in map-based eval path"));
                    }
                    crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                        vecExpr,
                        idxExpr,
                        defaultValue,
                    ) => {
                        let vecExpr = (*vecExpr).clone();
                        let idxExpr = (*idxExpr).clone();
                        return crate::aver_generated::domain::eval::core::evalVectorGetOrIntExpr(
                            &vecExpr,
                            &idxExpr,
                            defaultValue,
                            &env,
                            &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                        a,
                        b,
                        defaultValue,
                    ) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalIntModOrIntExpr(
                            &a,
                            &b,
                            defaultValue,
                            &env,
                            &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprAdd(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalBinop(
                            &a,
                            &b,
                            &env,
                            &*fns,
                            &crate::aver_generated::domain::ast::BinOp::OpAdd,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprSub(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalBinop(
                            &a,
                            &b,
                            &env,
                            &*fns,
                            &crate::aver_generated::domain::ast::BinOp::OpSub,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprMul(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalBinop(
                            &a,
                            &b,
                            &env,
                            &*fns,
                            &crate::aver_generated::domain::ast::BinOp::OpMul,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprDiv(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalBinop(
                            &a,
                            &b,
                            &env,
                            &*fns,
                            &crate::aver_generated::domain::ast::BinOp::OpDiv,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprNeg(inner) => {
                        let inner = (*inner).clone();
                        return crate::aver_generated::domain::eval::core::evalNeg(
                            &inner, &env, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprEq(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmp(
                            &a,
                            &b,
                            &env,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpEq,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprNeq(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmp(
                            &a,
                            &b,
                            &env,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpNeq,
                        );
                    }
                    _ => __MutualTco1::EvalExprAggregate(expr, env),
                }
            }
            __MutualTco1::EvalExprAggregate(mut expr, mut env) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprLt(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmp(
                            &a,
                            &b,
                            &env,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpLt,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprGt(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmp(
                            &a,
                            &b,
                            &env,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpGt,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprLte(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmp(
                            &a,
                            &b,
                            &env,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpLte,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprGte(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmp(
                            &a,
                            &b,
                            &env,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpGte,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprConcat(parts) => {
                        return crate::aver_generated::domain::eval::core::evalConcatExpr(
                            &parts, &env, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprTuple(exprs) => {
                        return crate::aver_generated::domain::eval::core::evalTupleExpr(
                            &exprs, &env, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprList(exprs) => {
                        return crate::aver_generated::domain::eval::core::evalListExpr(
                            &exprs, &env, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprRecord(name, fieldExprs) => {
                        return crate::aver_generated::domain::eval::core::evalRecordExpr(
                            name,
                            &fieldExprs,
                            &env,
                            &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprFieldAccess(obj, field) => {
                        let obj = (*obj).clone();
                        return crate::aver_generated::domain::eval::core::evalFieldAccess(
                            &obj, field, &env, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCall(name, argExprs) => {
                        return crate::aver_generated::domain::eval::core::evalCall(
                            name, &argExprs, &env, &*fns,
                        );
                    }
                    _ => __MutualTco1::EvalExprCalls(expr, env),
                }
            }
            __MutualTco1::EvalExprCalls(mut expr, mut env) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprCallDirect(fnId, argExprs) => {
                        return crate::aver_generated::domain::eval::core::evalCallDirect(
                            fnId, &argExprs, &env, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, argExprs) => {
                        __MutualTco1::EvalCallBuiltinMaybeSpecial(name, argExprs, env)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(id, argExprs) => {
                        __MutualTco1::EvalCallBuiltinById(id, argExprs, env)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprMatch(scrutinee, arms) => {
                        let scrutinee = (*scrutinee).clone();
                        __MutualTco1::EvalMatchExpr(scrutinee, arms, env)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprPropagate(inner) => {
                        let inner = (*inner).clone();
                        return crate::aver_generated::domain::eval::core::evalPropagate(
                            &inner, &env, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
                        exprs,
                        unwrap,
                    ) => {
                        return crate::aver_generated::domain::eval::core::evalIndependentProduct(
                            &exprs, unwrap, &env, &*fns,
                        );
                    }
                    _ => {
                        return Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(50)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("unsupported named-env expression: "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::aver_generated::domain::eval::core::exprLabel(&expr)),
                            )));
                            __b
                        }));
                    }
                }
            }
            __MutualTco1::EvalBoolBranch(mut cond, mut thenExpr, mut elseExpr, mut env) => {
                crate::cancel_checkpoint();
                let condV =
                    crate::aver_generated::domain::eval::core::evalExpr(&cond, &env, &*fns)?;
                match condV.clone() {
                    crate::aver_generated::domain::value::Val::ValBool(flag) => {
                        if flag {
                            __MutualTco1::EvalExpr(thenExpr, env)
                        } else {
                            __MutualTco1::EvalExpr(elseExpr, env)
                        }
                    }
                    _ => {
                        return Err((AverStr::from(
                            "bool branch condition must evaluate to Bool, got: ",
                        ) + &crate::aver_generated::domain::value::valRepr(&condV)));
                    }
                }
            }
            __MutualTco1::EvalMatchExpr(mut scrutinee, mut arms, mut env) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::core::evalExpr(&scrutinee, &env, &*fns) {
                    Ok(v @ _) => __MutualTco1::EvalMatch(v, arms, env),
                    Err(e @ _) => return Err(e),
                }
            }
            __MutualTco1::EvalCallBuiltinById(mut id, mut argExprs, mut env) => {
                crate::cancel_checkpoint();
                {
                    let __int_match_subject = id.clone();
                    if __int_match_subject == aver_rt::AverInt::from_i64(15) {
                        __MutualTco1::EvalOptionWithDefaultExpr(argExprs, env)
                    } else {
                        match crate::aver_generated::domain::eval::core::evalArgs(&argExprs, &env, &*fns) { Err(e @ _) => { return Err(e) }, Ok(args @ _) => { return crate::aver_generated::domain::builtins::callBuiltinByIdValues(id, &args) } }
                    }
                }
            }
            __MutualTco1::EvalCallBuiltinMaybeSpecial(mut name, mut argExprs, mut env) => {
                crate::cancel_checkpoint();
                match &*name.clone() {
                    "Option.withDefault" => __MutualTco1::EvalOptionWithDefaultExpr(argExprs, env),
                    _ => {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                            name, &argExprs, &env, &*fns,
                        );
                    }
                }
            }
            __MutualTco1::EvalOptionWithDefaultExpr(mut argExprs, mut env) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = argExprs.clone();
                    if let Some((optionExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = rest;
                            if let Some((defaultExpr, ignored)) =
                                aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                __MutualTco1::EvalOptionWithDefaultExprInner(
                                    optionExpr,
                                    defaultExpr,
                                    env,
                                )
                            } else {
                                return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                                    AverStr::from("Option.withDefault"),
                                    &argExprs,
                                    &env,
                                    &*fns,
                                );
                            }
                        }
                    } else {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                            AverStr::from("Option.withDefault"),
                            &argExprs,
                            &env,
                            &*fns,
                        );
                    }
                }
            }
            __MutualTco1::EvalOptionWithDefaultExprInner(
                mut optionExpr,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                match optionExpr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, vecArgs) => {
                        let __dispatch_subject = name;
                        if &*__dispatch_subject == "Vector.set" {
                            __MutualTco1::EvalVectorSetWithDefaultExpr(vecArgs, defaultExpr, env)
                        } else {
                            if &*__dispatch_subject == "Vector.get" {
                                __MutualTco1::EvalVectorGetWithDefaultExpr(
                                    vecArgs,
                                    defaultExpr,
                                    env,
                                )
                            } else {
                                return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                                    AverStr::from("Option.withDefault"),
                                    &aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]),
                                    &env,
                                    &*fns,
                                );
                            }
                        }
                    }
                    _ => {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                            AverStr::from("Option.withDefault"),
                            &aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]),
                            &env,
                            &*fns,
                        );
                    }
                }
            }
            __MutualTco1::EvalVectorSetWithDefaultExpr(mut vecArgs, mut defaultExpr, mut env) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = vecArgs.clone();
                    if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = r1;
                            if let Some((idxExpr, r2)) =
                                aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = r2;
                                    if let Some((valueExpr, ignored)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        __MutualTco1::EvalVectorSetWithDefaultExprValues(
                                            vecExpr,
                                            idxExpr,
                                            valueExpr,
                                            defaultExpr,
                                            env,
                                        )
                                    } else {
                                        return crate::aver_generated::domain::eval::core::evalCallBuiltin(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(AverStr::from("Vector.set"), vecArgs), defaultExpr]), &env, &*fns);
                                    }
                                }
                            } else {
                                return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                                    AverStr::from("Option.withDefault"),
                                    &aver_rt::AverList::from_vec(vec![
                                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                            AverStr::from("Vector.set"),
                                            vecArgs,
                                        ),
                                        defaultExpr,
                                    ]),
                                    &env,
                                    &*fns,
                                );
                            }
                        }
                    } else {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                            AverStr::from("Option.withDefault"),
                            &aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    AverStr::from("Vector.set"),
                                    vecArgs,
                                ),
                                defaultExpr,
                            ]),
                            &env,
                            &*fns,
                        );
                    }
                }
            }
            __MutualTco1::EvalVectorSetWithDefaultExprValues(
                mut vecExpr,
                mut idxExpr,
                mut valueExpr,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                let vecV =
                    crate::aver_generated::domain::eval::core::evalExpr(&vecExpr, &env, &*fns)?;
                let idxV =
                    crate::aver_generated::domain::eval::core::evalExpr(&idxExpr, &env, &*fns)?;
                let valueV =
                    crate::aver_generated::domain::eval::core::evalExpr(&valueExpr, &env, &*fns)?;
                __MutualTco1::EvalVectorSetWithDefaultExprResult(
                    vecV,
                    idxV,
                    valueV,
                    defaultExpr,
                    env,
                )
            }
            __MutualTco1::EvalVectorSetWithDefaultExprResult(
                mut vecV,
                mut idxV,
                mut valueV,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::ops::evalVectorSetMaybeDefault(
                    &vecV, &idxV, &valueV,
                ) {
                    Ok(maybeV @ _) => match maybeV {
                        Some(v @ _) => return Ok(v),
                        None => __MutualTco1::EvalExpr(defaultExpr, env),
                    },
                    Err(err @ _) => return Err(err),
                }
            }
            __MutualTco1::EvalVectorGetWithDefaultExpr(mut vecArgs, mut defaultExpr, mut env) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = vecArgs.clone();
                    if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = r1;
                            if let Some((idxExpr, ignored)) =
                                aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                __MutualTco1::EvalVectorGetWithDefaultExprValues(
                                    vecExpr,
                                    idxExpr,
                                    defaultExpr,
                                    env,
                                )
                            } else {
                                return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                                    AverStr::from("Option.withDefault"),
                                    &aver_rt::AverList::from_vec(vec![
                                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                            AverStr::from("Vector.get"),
                                            vecArgs,
                                        ),
                                        defaultExpr,
                                    ]),
                                    &env,
                                    &*fns,
                                );
                            }
                        }
                    } else {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                            AverStr::from("Option.withDefault"),
                            &aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    AverStr::from("Vector.get"),
                                    vecArgs,
                                ),
                                defaultExpr,
                            ]),
                            &env,
                            &*fns,
                        );
                    }
                }
            }
            __MutualTco1::EvalVectorGetWithDefaultExprValues(
                mut vecExpr,
                mut idxExpr,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                let vecV =
                    crate::aver_generated::domain::eval::core::evalExpr(&vecExpr, &env, &*fns)?;
                let idxV =
                    crate::aver_generated::domain::eval::core::evalExpr(&idxExpr, &env, &*fns)?;
                __MutualTco1::EvalVectorGetWithDefaultExprResult(vecV, idxV, defaultExpr, env)
            }
            __MutualTco1::EvalVectorGetWithDefaultExprResult(
                mut vecV,
                mut idxV,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::ops::evalVectorGetMaybeDefault(
                    &vecV, &idxV,
                ) {
                    Ok(maybeV @ _) => match maybeV {
                        Some(v @ _) => return Ok(v),
                        None => __MutualTco1::EvalExpr(defaultExpr, env),
                    },
                    Err(err @ _) => return Err(err),
                }
            }
            __MutualTco1::EvalMatch(mut v, mut arms, mut env) => {
                crate::cancel_checkpoint();
                aver_list_match!(arms, [] => { return Err(AverStr::from("no matching arm")) }, [arm, rest] => match crate::aver_generated::domain::match_mod::matchPattern(&arm.pattern, &v) { Ok(bindings @ _) => { __MutualTco1::EvalExpr(arm.body.clone(), crate::aver_generated::domain::eval::store::mergeBindings(bindings, env)) }, Err(_) => { __MutualTco1::EvalMatch(v, rest, env) } })
            }
        };
    }
}

/// Evaluate an expression in the given environment.
pub fn evalExpr(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalExpr(expr.clone(), env.clone()), &fns)
}

/// Continue named-env evaluation for branch, vars, and slot-only internal nodes.
pub fn evalExprBasic(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalExprBasic(expr.clone(), env.clone()), &fns)
}

/// Continue named-env expression evaluation for comparisons and arithmetic.
pub fn evalExprInternal(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalExprInternal(expr.clone(), env.clone()),
        &fns,
    )
}

/// Continue named-env evaluation for aggregate expression forms.
pub fn evalExprAggregate(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalExprAggregate(expr.clone(), env.clone()),
        &fns,
    )
}

/// Finish named-env evaluation for calls, matches, propagation, and products.
pub fn evalExprCalls(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalExprCalls(expr.clone(), env.clone()), &fns)
}

/// Evaluate a direct bool branch in map path.
pub fn evalBoolBranch(
    cond: &crate::aver_generated::domain::ast::Expr,
    thenExpr: &crate::aver_generated::domain::ast::Expr,
    elseExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalBoolBranch(
            cond.clone(),
            thenExpr.clone(),
            elseExpr.clone(),
            env.clone(),
        ),
        &fns,
    )
}

/// Evaluate a match expression.
pub fn evalMatchExpr(
    scrutinee: &crate::aver_generated::domain::ast::Expr,
    arms: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalMatchExpr(scrutinee.clone(), arms.clone(), env.clone()),
        &fns,
    )
}

/// Builtin dispatch by integer ID — no string comparison.
pub fn evalCallBuiltinById(
    id: aver_rt::AverInt,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalCallBuiltinById(id, argExprs.clone(), env.clone()),
        &fns,
    )
}

/// Builtin dispatch with fast-path peepholes for hot patterns.
pub fn evalCallBuiltinMaybeSpecial(
    name: AverStr,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalCallBuiltinMaybeSpecial(name, argExprs.clone(), env.clone()),
        &fns,
    )
}

/// Specialize hot Option.withDefault(Vector.get/Vector.set, default) patterns in map path.
pub fn evalOptionWithDefaultExpr(
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalOptionWithDefaultExpr(argExprs.clone(), env.clone()),
        &fns,
    )
}

/// Dispatch specialized Option.withDefault cases in map path.
pub fn evalOptionWithDefaultExprInner(
    optionExpr: &crate::aver_generated::domain::ast::Expr,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalOptionWithDefaultExprInner(
            optionExpr.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &fns,
    )
}

/// Specialized Vector.set + Option.withDefault in map path.
pub fn evalVectorSetWithDefaultExpr(
    vecArgs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorSetWithDefaultExpr(
            vecArgs.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &fns,
    )
}

/// Evaluate Vector.set operands in map path and defer the default expression until needed.
pub fn evalVectorSetWithDefaultExprValues(
    vecExpr: &crate::aver_generated::domain::ast::Expr,
    idxExpr: &crate::aver_generated::domain::ast::Expr,
    valueExpr: &crate::aver_generated::domain::ast::Expr,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorSetWithDefaultExprValues(
            vecExpr.clone(),
            idxExpr.clone(),
            valueExpr.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &fns,
    )
}

/// Finish specialized Vector.set + Option.withDefault in map path with lazy default evaluation.
pub fn evalVectorSetWithDefaultExprResult(
    vecV: &crate::aver_generated::domain::value::Val,
    idxV: &crate::aver_generated::domain::value::Val,
    valueV: &crate::aver_generated::domain::value::Val,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorSetWithDefaultExprResult(
            vecV.clone(),
            idxV.clone(),
            valueV.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &fns,
    )
}

/// Specialized Vector.get + Option.withDefault in map path.
pub fn evalVectorGetWithDefaultExpr(
    vecArgs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorGetWithDefaultExpr(
            vecArgs.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &fns,
    )
}

/// Evaluate Vector.get operands in map path and defer the default expression until needed.
pub fn evalVectorGetWithDefaultExprValues(
    vecExpr: &crate::aver_generated::domain::ast::Expr,
    idxExpr: &crate::aver_generated::domain::ast::Expr,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorGetWithDefaultExprValues(
            vecExpr.clone(),
            idxExpr.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &fns,
    )
}

/// Finish specialized Vector.get + Option.withDefault in map path with lazy default evaluation.
pub fn evalVectorGetWithDefaultExprResult(
    vecV: &crate::aver_generated::domain::value::Val,
    idxV: &crate::aver_generated::domain::value::Val,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorGetWithDefaultExprResult(
            vecV.clone(),
            idxV.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &fns,
    )
}

/// Try each match arm until one matches. Self-recursive for codegen TCO.
pub fn evalMatch(
    v: &crate::aver_generated::domain::value::Val,
    arms: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalMatch(v.clone(), arms.clone(), env.clone()),
        &fns,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    EvalTailExprSlot(
        aver_rt::AverInt,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverInt,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalTailBoolBranchSlot(
        aver_rt::AverInt,
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverInt,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalTailMatchExprSlot(
        aver_rt::AverInt,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
        aver_rt::AverInt,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalTailMatchSlot(
        aver_rt::AverInt,
        crate::aver_generated::domain::value::Val,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
        aver_rt::AverInt,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
}

fn __mutual_tco_trampoline_2(
    mut __state: __MutualTco2,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::EvalTailExprSlot(mut selfId, mut expr, mut slotCount, mut env) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprCallDirect(fnId, argExprs) => {
                        if (fnId == selfId) {
                            match crate::aver_generated::domain::eval::core::evalArgsSlotToSlotEnv(argExprs, env, (*slotMap).clone(), (*fns).clone(), match (slotCount).to_u32() { Some(__n) => Ok(aver_rt::AverVector::new(__n as usize, crate::aver_generated::domain::value::Val::ValUnit)), None => Err(aver_rt::AverStr::from("Vector.new: size must be between 0 and 4294967295")) }?, aver_rt::AverInt::from_i64(0)) { Ok(nextEnv @ _) => { return Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailRecurEnv(nextEnv)) }, Err(e @ _) => { return Err(e) } }
                        } else {
                            match crate::aver_generated::domain::eval::core::evalCallDirectSlot(fnId, &argExprs, &env, &*slotMap, &*fns) { Ok(v @ _) => { return Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v)) }, Err(e @ _) => { return Err(e) } }
                        }
                    }
                    crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                        cond,
                        thenExpr,
                        elseExpr,
                    ) => {
                        let cond = (*cond).clone();
                        let thenExpr = (*thenExpr).clone();
                        let elseExpr = (*elseExpr).clone();
                        __MutualTco2::EvalTailBoolBranchSlot(
                            selfId, cond, thenExpr, elseExpr, slotCount, env,
                        )
                    }
                    crate::aver_generated::domain::ast::Expr::ExprMatch(scrutinee, arms) => {
                        let scrutinee = (*scrutinee).clone();
                        __MutualTco2::EvalTailMatchExprSlot(selfId, scrutinee, arms, slotCount, env)
                    }
                    _ => {
                        match crate::aver_generated::domain::eval::core::evalExprSlot(&expr, &env, &*slotMap, &*fns) { Ok(v @ _) => { return Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v)) }, Err(e @ _) => { return Err(e) } }
                    }
                }
            }
            __MutualTco2::EvalTailBoolBranchSlot(
                mut selfId,
                mut cond,
                mut thenExpr,
                mut elseExpr,
                mut slotCount,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                let condV = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &cond, &env, &*slotMap, &*fns,
                )?;
                match condV.clone() {
                    crate::aver_generated::domain::value::Val::ValBool(flag) => {
                        if flag {
                            __MutualTco2::EvalTailExprSlot(selfId, thenExpr, slotCount, env)
                        } else {
                            __MutualTco2::EvalTailExprSlot(selfId, elseExpr, slotCount, env)
                        }
                    }
                    _ => {
                        return Err((AverStr::from(
                            "bool branch condition must evaluate to Bool, got: ",
                        ) + &crate::aver_generated::domain::value::valRepr(&condV)));
                    }
                }
            }
            __MutualTco2::EvalTailMatchExprSlot(
                mut selfId,
                mut scrutinee,
                mut arms,
                mut slotCount,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                let v = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &scrutinee, &env, &*slotMap, &*fns,
                )?;
                __MutualTco2::EvalTailMatchSlot(selfId, v, arms, slotCount, env)
            }
            __MutualTco2::EvalTailMatchSlot(
                mut selfId,
                mut v,
                mut arms,
                mut slotCount,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                aver_list_match!(arms, [] => { return Err(AverStr::from("no matching arm")) }, [arm, rest] => match crate::aver_generated::domain::match_mod::matchPattern(&arm.pattern, &v) { Ok(bindings @ _) => { __MutualTco2::EvalTailExprSlot(selfId, arm.body.clone(), slotCount, crate::aver_generated::domain::eval::core::mergeBindingsSlot(&bindings, &arm.bindingSlots, &env)) }, Err(_) => { __MutualTco2::EvalTailMatchSlot(selfId, v, rest, slotCount, env) } })
            }
        };
    }
}

/// Evaluate a tail-position expression in slot mode, converting self-recursive direct calls into loop re-entry.
pub fn evalTailExprSlot(
    selfId: aver_rt::AverInt,
    expr: &crate::aver_generated::domain::ast::Expr,
    slotCount: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::EvalTailExprSlot(selfId, expr.clone(), slotCount, env.clone()),
        &slotMap,
        &fns,
    )
}

/// Evaluate a tail-position bool branch in slot mode.
pub fn evalTailBoolBranchSlot(
    selfId: aver_rt::AverInt,
    cond: &crate::aver_generated::domain::ast::Expr,
    thenExpr: &crate::aver_generated::domain::ast::Expr,
    elseExpr: &crate::aver_generated::domain::ast::Expr,
    slotCount: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::EvalTailBoolBranchSlot(
            selfId,
            cond.clone(),
            thenExpr.clone(),
            elseExpr.clone(),
            slotCount,
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Evaluate a tail-position match expression in slot mode.
pub fn evalTailMatchExprSlot(
    selfId: aver_rt::AverInt,
    scrutinee: &crate::aver_generated::domain::ast::Expr,
    arms: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    slotCount: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::EvalTailMatchExprSlot(
            selfId,
            scrutinee.clone(),
            arms.clone(),
            slotCount,
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Evaluate a tail-position match arm in slot mode, preserving binding slots.
pub fn evalTailMatchSlot(
    selfId: aver_rt::AverInt,
    v: &crate::aver_generated::domain::value::Val,
    arms: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    slotCount: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::EvalTailMatchSlot(selfId, v.clone(), arms.clone(), slotCount, env.clone()),
        &slotMap,
        &fns,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    EvalExprSlot(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalExprSlotBasic(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalExprSlotInternal(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalExprSlotAggregate(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalExprSlotCalls(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalBoolBranchSlot(
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalCallBuiltinByIdSlot(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalCallBuiltinSlotMaybeSpecial(
        AverStr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalOptionWithDefaultExprSlot(
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalOptionWithDefaultExprSlotInner(
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorSetWithDefaultExprSlot(
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorSetWithDefaultExprSlotValues(
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorSetWithDefaultExprSlotResult(
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorGetWithDefaultExprSlot(
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorGetWithDefaultExprSlotValues(
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalVectorGetWithDefaultExprSlotResult(
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::value::Val,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalMatchExprSlot(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalMatchSlot(
        crate::aver_generated::domain::value::Val,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
}

fn __mutual_tco_trampoline_3(
    mut __state: __MutualTco3,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::EvalExprSlot(mut expr, mut env) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::core::evalImmediateSlotExpr(&expr, &env)
                {
                    Some(result @ _) => return result,
                    None => __MutualTco3::EvalExprSlotBasic(expr, env),
                }
            }
            __MutualTco3::EvalExprSlotBasic(mut expr, mut env) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                        cond,
                        thenExpr,
                        elseExpr,
                    ) => {
                        let cond = (*cond).clone();
                        let thenExpr = (*thenExpr).clone();
                        let elseExpr = (*elseExpr).clone();
                        __MutualTco3::EvalBoolBranchSlot(cond, thenExpr, elseExpr, env)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprVar(name) => {
                        return crate::aver_generated::domain::eval::core::evalVarSlot(name, &*fns);
                    }
                    crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(op, slot, rhs) => {
                        return crate::aver_generated::domain::eval::core::evalBinopSlotInt(
                            &op, slot, rhs, &env,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprBinopSlots(op, lhs, rhs) => {
                        return crate::aver_generated::domain::eval::core::evalBinopSlots(
                            &op, lhs, rhs, &env,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(op, slot, rhs) => {
                        return crate::aver_generated::domain::eval::core::evalCmpSlotInt(
                            &op, slot, rhs, &env,
                        );
                    }
                    _ => __MutualTco3::EvalExprSlotInternal(expr, env),
                }
            }
            __MutualTco3::EvalExprSlotInternal(mut expr, mut env) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprCmpSlots(op, lhs, rhs) => {
                        return crate::aver_generated::domain::eval::core::evalCmpSlots(
                            &op, lhs, rhs, &env,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                        vecExpr,
                        idxExpr,
                        defaultValue,
                    ) => {
                        let vecExpr = (*vecExpr).clone();
                        let idxExpr = (*idxExpr).clone();
                        return crate::aver_generated::domain::eval::core::evalVectorGetOrIntExprSlot(&vecExpr, &idxExpr, defaultValue, &env, &*slotMap, &*fns);
                    }
                    crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                        a,
                        b,
                        defaultValue,
                    ) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalIntModOrIntExprSlot(
                            &a,
                            &b,
                            defaultValue,
                            &env,
                            &*slotMap,
                            &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprAdd(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalBinopSlot(
                            &a,
                            &b,
                            &env,
                            &*slotMap,
                            &*fns,
                            &crate::aver_generated::domain::ast::BinOp::OpAdd,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprSub(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalBinopSlot(
                            &a,
                            &b,
                            &env,
                            &*slotMap,
                            &*fns,
                            &crate::aver_generated::domain::ast::BinOp::OpSub,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprMul(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalBinopSlot(
                            &a,
                            &b,
                            &env,
                            &*slotMap,
                            &*fns,
                            &crate::aver_generated::domain::ast::BinOp::OpMul,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprDiv(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalBinopSlot(
                            &a,
                            &b,
                            &env,
                            &*slotMap,
                            &*fns,
                            &crate::aver_generated::domain::ast::BinOp::OpDiv,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprNeg(inner) => {
                        let inner = (*inner).clone();
                        return crate::aver_generated::domain::eval::core::evalNegSlot(
                            &inner, &env, &*slotMap, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprEq(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmpSlot(
                            &a,
                            &b,
                            &env,
                            &*slotMap,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpEq,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprNeq(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmpSlot(
                            &a,
                            &b,
                            &env,
                            &*slotMap,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpNeq,
                        );
                    }
                    _ => __MutualTco3::EvalExprSlotAggregate(expr, env),
                }
            }
            __MutualTco3::EvalExprSlotAggregate(mut expr, mut env) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprLt(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmpSlot(
                            &a,
                            &b,
                            &env,
                            &*slotMap,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpLt,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprGt(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmpSlot(
                            &a,
                            &b,
                            &env,
                            &*slotMap,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpGt,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprLte(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmpSlot(
                            &a,
                            &b,
                            &env,
                            &*slotMap,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpLte,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprGte(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        return crate::aver_generated::domain::eval::core::evalCmpSlot(
                            &a,
                            &b,
                            &env,
                            &*slotMap,
                            &*fns,
                            &crate::aver_generated::domain::ast::CmpOp::CmpGte,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprConcat(parts) => {
                        return crate::aver_generated::domain::eval::core::evalConcatSlot(
                            &parts, &env, &*slotMap, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprTuple(exprs) => {
                        return crate::aver_generated::domain::eval::core::evalTupleSlot(
                            &exprs, &env, &*slotMap, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprList(exprs) => {
                        return crate::aver_generated::domain::eval::core::evalListSlot(
                            &exprs, &env, &*slotMap, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprRecord(name, fieldExprs) => {
                        return crate::aver_generated::domain::eval::core::evalRecordSlot(
                            name,
                            &fieldExprs,
                            &env,
                            &*slotMap,
                            &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprFieldAccess(obj, field) => {
                        let obj = (*obj).clone();
                        return crate::aver_generated::domain::eval::core::evalFieldAccessSlot(
                            &obj, field, &env, &*slotMap, &*fns,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCall(name, argExprs) => {
                        return crate::aver_generated::domain::eval::core::evalCallSlot(
                            name, &argExprs, &env, &*slotMap, &*fns,
                        );
                    }
                    _ => __MutualTco3::EvalExprSlotCalls(expr, env),
                }
            }
            __MutualTco3::EvalExprSlotCalls(mut expr, mut env) => {
                crate::cancel_checkpoint();
                match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprCallDirect(fnId, argExprs) => {
            return crate::aver_generated::domain::eval::core::evalCallDirectSlot(fnId, &argExprs, &env, &*slotMap, &*fns)
        },
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, argExprs) => {
            __MutualTco3::EvalCallBuiltinSlotMaybeSpecial(name, argExprs, env)
        },
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(id, argExprs) => {
            __MutualTco3::EvalCallBuiltinByIdSlot(id, argExprs, env)
        },
        crate::aver_generated::domain::ast::Expr::ExprMatch(scrutinee, arms) => {
            let scrutinee = (*scrutinee).clone();
            __MutualTco3::EvalMatchExprSlot(scrutinee, arms, env)
        },
        crate::aver_generated::domain::ast::Expr::ExprPropagate(inner) => {
            let inner = (*inner).clone();
            return crate::aver_generated::domain::eval::core::evalPropagateSlot(&inner, &env, &*slotMap, &*fns)
        },
        crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(exprs, unwrap) => {
            return crate::aver_generated::domain::eval::core::evalIndependentProductSlot(&exprs, unwrap, &env, &*slotMap, &*fns)
        },
        _ => {
            return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(45)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported slot expression: ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::aver_generated::domain::eval::core::exprLabel(&expr))))); __b }))
        }
    }
            }
            __MutualTco3::EvalBoolBranchSlot(mut cond, mut thenExpr, mut elseExpr, mut env) => {
                crate::cancel_checkpoint();
                let condV = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &cond, &env, &*slotMap, &*fns,
                )?;
                match condV.clone() {
                    crate::aver_generated::domain::value::Val::ValBool(flag) => {
                        if flag {
                            __MutualTco3::EvalExprSlot(thenExpr, env)
                        } else {
                            __MutualTco3::EvalExprSlot(elseExpr, env)
                        }
                    }
                    _ => {
                        return Err((AverStr::from(
                            "bool branch condition must evaluate to Bool, got: ",
                        ) + &crate::aver_generated::domain::value::valRepr(&condV)));
                    }
                }
            }
            __MutualTco3::EvalCallBuiltinByIdSlot(mut id, mut argExprs, mut env) => {
                crate::cancel_checkpoint();
                {
                    let __int_match_subject = id.clone();
                    if __int_match_subject == aver_rt::AverInt::from_i64(15) {
                        __MutualTco3::EvalOptionWithDefaultExprSlot(argExprs, env)
                    } else {
                        match crate::aver_generated::domain::eval::core::evalArgsSlot(&argExprs, &env, &*slotMap, &*fns) { Err(e @ _) => { return Err(e) }, Ok(args @ _) => { return crate::aver_generated::domain::builtins::callBuiltinByIdValues(id, &args) } }
                    }
                }
            }
            __MutualTco3::EvalCallBuiltinSlotMaybeSpecial(mut name, mut argExprs, mut env) => {
                crate::cancel_checkpoint();
                match &*name.clone() {
                    "Option.withDefault" => {
                        __MutualTco3::EvalOptionWithDefaultExprSlot(argExprs, env)
                    }
                    _ => {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(
                            name, &argExprs, &env, &*slotMap, &*fns,
                        );
                    }
                }
            }
            __MutualTco3::EvalOptionWithDefaultExprSlot(mut argExprs, mut env) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = argExprs.clone();
                    if let Some((optionExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = rest;
                            if let Some((defaultExpr, ignored)) =
                                aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                __MutualTco3::EvalOptionWithDefaultExprSlotInner(
                                    optionExpr,
                                    defaultExpr,
                                    env,
                                )
                            } else {
                                return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &argExprs, &env, &*slotMap, &*fns);
                            }
                        }
                    } else {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(
                            AverStr::from("Option.withDefault"),
                            &argExprs,
                            &env,
                            &*slotMap,
                            &*fns,
                        );
                    }
                }
            }
            __MutualTco3::EvalOptionWithDefaultExprSlotInner(
                mut optionExpr,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                match optionExpr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, vecArgs) => {
                        let __dispatch_subject = name;
                        if &*__dispatch_subject == "Vector.set" {
                            __MutualTco3::EvalVectorSetWithDefaultExprSlot(
                                vecArgs,
                                defaultExpr,
                                env,
                            )
                        } else {
                            if &*__dispatch_subject == "Vector.get" {
                                __MutualTco3::EvalVectorGetWithDefaultExprSlot(
                                    vecArgs,
                                    defaultExpr,
                                    env,
                                )
                            } else {
                                return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]), &env, &*slotMap, &*fns);
                            }
                        }
                    }
                    _ => {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(
                            AverStr::from("Option.withDefault"),
                            &aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]),
                            &env,
                            &*slotMap,
                            &*fns,
                        );
                    }
                }
            }
            __MutualTco3::EvalVectorSetWithDefaultExprSlot(
                mut vecArgs,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = vecArgs.clone();
                    if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = r1;
                            if let Some((idxExpr, r2)) =
                                aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = r2;
                                    if let Some((valueExpr, ignored)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        __MutualTco3::EvalVectorSetWithDefaultExprSlotValues(
                                            vecExpr,
                                            idxExpr,
                                            valueExpr,
                                            defaultExpr,
                                            env,
                                        )
                                    } else {
                                        return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(AverStr::from("Vector.set"), vecArgs), defaultExpr]), &env, &*slotMap, &*fns);
                                    }
                                }
                            } else {
                                return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(AverStr::from("Vector.set"), vecArgs), defaultExpr]), &env, &*slotMap, &*fns);
                            }
                        }
                    } else {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(
                            AverStr::from("Option.withDefault"),
                            &aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    AverStr::from("Vector.set"),
                                    vecArgs,
                                ),
                                defaultExpr,
                            ]),
                            &env,
                            &*slotMap,
                            &*fns,
                        );
                    }
                }
            }
            __MutualTco3::EvalVectorSetWithDefaultExprSlotValues(
                mut vecExpr,
                mut idxExpr,
                mut valueExpr,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                let vecV = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &vecExpr, &env, &*slotMap, &*fns,
                )?;
                let idxV = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &idxExpr, &env, &*slotMap, &*fns,
                )?;
                let valueV = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &valueExpr, &env, &*slotMap, &*fns,
                )?;
                __MutualTco3::EvalVectorSetWithDefaultExprSlotResult(
                    vecV,
                    idxV,
                    valueV,
                    defaultExpr,
                    env,
                )
            }
            __MutualTco3::EvalVectorSetWithDefaultExprSlotResult(
                mut vecV,
                mut idxV,
                mut valueV,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::ops::evalVectorSetMaybeDefault(
                    &vecV, &idxV, &valueV,
                ) {
                    Ok(maybeV @ _) => match maybeV {
                        Some(v @ _) => return Ok(v),
                        None => __MutualTco3::EvalExprSlot(defaultExpr, env),
                    },
                    Err(err @ _) => return Err(err),
                }
            }
            __MutualTco3::EvalVectorGetWithDefaultExprSlot(
                mut vecArgs,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = vecArgs.clone();
                    if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = r1;
                            if let Some((idxExpr, ignored)) =
                                aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                __MutualTco3::EvalVectorGetWithDefaultExprSlotValues(
                                    vecExpr,
                                    idxExpr,
                                    defaultExpr,
                                    env,
                                )
                            } else {
                                return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(AverStr::from("Vector.get"), vecArgs), defaultExpr]), &env, &*slotMap, &*fns);
                            }
                        }
                    } else {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(
                            AverStr::from("Option.withDefault"),
                            &aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    AverStr::from("Vector.get"),
                                    vecArgs,
                                ),
                                defaultExpr,
                            ]),
                            &env,
                            &*slotMap,
                            &*fns,
                        );
                    }
                }
            }
            __MutualTco3::EvalVectorGetWithDefaultExprSlotValues(
                mut vecExpr,
                mut idxExpr,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                let vecV = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &vecExpr, &env, &*slotMap, &*fns,
                )?;
                let idxV = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &idxExpr, &env, &*slotMap, &*fns,
                )?;
                __MutualTco3::EvalVectorGetWithDefaultExprSlotResult(vecV, idxV, defaultExpr, env)
            }
            __MutualTco3::EvalVectorGetWithDefaultExprSlotResult(
                mut vecV,
                mut idxV,
                mut defaultExpr,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::ops::evalVectorGetMaybeDefault(
                    &vecV, &idxV,
                ) {
                    Ok(maybeV @ _) => match maybeV {
                        Some(v @ _) => return Ok(v),
                        None => __MutualTco3::EvalExprSlot(defaultExpr, env),
                    },
                    Err(err @ _) => return Err(err),
                }
            }
            __MutualTco3::EvalMatchExprSlot(mut scrutinee, mut arms, mut env) => {
                crate::cancel_checkpoint();
                let v = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &scrutinee, &env, &*slotMap, &*fns,
                )?;
                __MutualTco3::EvalMatchSlot(v, arms, env)
            }
            __MutualTco3::EvalMatchSlot(mut v, mut arms, mut env) => {
                crate::cancel_checkpoint();
                aver_list_match!(arms, [] => { return Err(AverStr::from("no matching arm")) }, [arm, rest] => match crate::aver_generated::domain::match_mod::matchPattern(&arm.pattern, &v) { Ok(bindings @ _) => { __MutualTco3::EvalExprSlot(arm.body.clone(), crate::aver_generated::domain::eval::core::mergeBindingsSlot(&bindings, &arm.bindingSlots, &env)) }, Err(_) => { __MutualTco3::EvalMatchSlot(v, rest, env) } })
            }
        };
    }
}

/// Evaluate an expression using slot-based environment.
pub fn evalExprSlot(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalExprSlot(expr.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Continue slot-based evaluation for branches, vars, and slot-only nodes.
pub fn evalExprSlotBasic(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalExprSlotBasic(expr.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Continue slot-based expression evaluation for comparisons and arithmetic.
pub fn evalExprSlotInternal(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalExprSlotInternal(expr.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Continue slot-based evaluation for aggregate and call expression forms.
pub fn evalExprSlotAggregate(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalExprSlotAggregate(expr.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Finish slot-based evaluation for calls, matches, propagation, and products.
pub fn evalExprSlotCalls(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalExprSlotCalls(expr.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Evaluate a direct bool branch in slot path.
pub fn evalBoolBranchSlot(
    cond: &crate::aver_generated::domain::ast::Expr,
    thenExpr: &crate::aver_generated::domain::ast::Expr,
    elseExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalBoolBranchSlot(
            cond.clone(),
            thenExpr.clone(),
            elseExpr.clone(),
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Builtin dispatch by integer ID (slot-based path) — no string comparison.
pub fn evalCallBuiltinByIdSlot(
    id: aver_rt::AverInt,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalCallBuiltinByIdSlot(id, argExprs.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Builtin dispatch with fast-path peepholes for hot patterns in slot path.
pub fn evalCallBuiltinSlotMaybeSpecial(
    name: AverStr,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalCallBuiltinSlotMaybeSpecial(name, argExprs.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Specialize hot Option.withDefault(Vector.get/Vector.set, default) patterns in slot path.
pub fn evalOptionWithDefaultExprSlot(
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalOptionWithDefaultExprSlot(argExprs.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Dispatch specialized Option.withDefault cases in slot path.
pub fn evalOptionWithDefaultExprSlotInner(
    optionExpr: &crate::aver_generated::domain::ast::Expr,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalOptionWithDefaultExprSlotInner(
            optionExpr.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Specialized Vector.set + Option.withDefault in slot path.
pub fn evalVectorSetWithDefaultExprSlot(
    vecArgs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorSetWithDefaultExprSlot(
            vecArgs.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Evaluate Vector.set operands in slot path and defer the default expression until needed.
pub fn evalVectorSetWithDefaultExprSlotValues(
    vecExpr: &crate::aver_generated::domain::ast::Expr,
    idxExpr: &crate::aver_generated::domain::ast::Expr,
    valueExpr: &crate::aver_generated::domain::ast::Expr,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorSetWithDefaultExprSlotValues(
            vecExpr.clone(),
            idxExpr.clone(),
            valueExpr.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Finish specialized Vector.set + Option.withDefault in slot path with lazy default evaluation.
pub fn evalVectorSetWithDefaultExprSlotResult(
    vecV: &crate::aver_generated::domain::value::Val,
    idxV: &crate::aver_generated::domain::value::Val,
    valueV: &crate::aver_generated::domain::value::Val,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorSetWithDefaultExprSlotResult(
            vecV.clone(),
            idxV.clone(),
            valueV.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Specialized Vector.get + Option.withDefault in slot path.
pub fn evalVectorGetWithDefaultExprSlot(
    vecArgs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorGetWithDefaultExprSlot(
            vecArgs.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Evaluate Vector.get operands in slot path and defer the default expression until needed.
pub fn evalVectorGetWithDefaultExprSlotValues(
    vecExpr: &crate::aver_generated::domain::ast::Expr,
    idxExpr: &crate::aver_generated::domain::ast::Expr,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorGetWithDefaultExprSlotValues(
            vecExpr.clone(),
            idxExpr.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Finish specialized Vector.get + Option.withDefault in slot path with lazy default evaluation.
pub fn evalVectorGetWithDefaultExprSlotResult(
    vecV: &crate::aver_generated::domain::value::Val,
    idxV: &crate::aver_generated::domain::value::Val,
    defaultExpr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorGetWithDefaultExprSlotResult(
            vecV.clone(),
            idxV.clone(),
            defaultExpr.clone(),
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Match expression in slot path.
pub fn evalMatchExprSlot(
    scrutinee: &crate::aver_generated::domain::ast::Expr,
    arms: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalMatchExprSlot(scrutinee.clone(), arms.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Try each match arm in slot path.
pub fn evalMatchSlot(
    v: &crate::aver_generated::domain::value::Val,
    arms: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalMatchSlot(v.clone(), arms.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco4 {
    EvalArgsMapToNamedEnv(
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        aver_rt::AverList<AverStr>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalArgsMapToNamedEnvBind(
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        aver_rt::AverList<AverStr>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
        crate::aver_generated::domain::value::Val,
    ),
}

fn __mutual_tco_trampoline_4(
    mut __state: __MutualTco4,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    loop {
        __state = match __state {
            __MutualTco4::EvalArgsMapToNamedEnv(mut exprs, mut params, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(exprs, [] => { return Ok(acc) }, [e, restExprs] => { match crate::aver_generated::domain::eval::core::evalExpr(&e, &*env, &*fns) { Err(err @ _) => { return Err(err) }, Ok(v @ _) => { __MutualTco4::EvalArgsMapToNamedEnvBind(restExprs, params, acc, v) } } })
            }
            __MutualTco4::EvalArgsMapToNamedEnvBind(mut restExprs, mut params, mut acc, mut v) => {
                crate::cancel_checkpoint();
                aver_list_match!(params, [] => __MutualTco4::EvalArgsMapToNamedEnv(restExprs, aver_rt::AverList::empty(), acc), [param, restParams] => __MutualTco4::EvalArgsMapToNamedEnv(restExprs, restParams, acc.insert_owned(param, v)))
            }
        };
    }
}

/// Evaluate arg expressions directly into a callee named env from map caller state.
pub fn evalArgsMapToNamedEnv(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    params: &aver_rt::AverList<AverStr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    acc: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    __mutual_tco_trampoline_4(
        __MutualTco4::EvalArgsMapToNamedEnv(exprs.clone(), params.clone(), acc.clone()),
        &env,
        &fns,
    )
}

/// Bind one evaluated arg into the callee named env when a parameter exists.
pub fn evalArgsMapToNamedEnvBind(
    restExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    params: &aver_rt::AverList<AverStr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    acc: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    v: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    __mutual_tco_trampoline_4(
        __MutualTco4::EvalArgsMapToNamedEnvBind(
            restExprs.clone(),
            params.clone(),
            acc.clone(),
            v.clone(),
        ),
        &env,
        &fns,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco5 {
    EvalArgsSlotToNamedEnv(
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        aver_rt::AverList<AverStr>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    EvalArgsSlotToNamedEnvBind(
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        aver_rt::AverList<AverStr>,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
        crate::aver_generated::domain::value::Val,
    ),
}

fn __mutual_tco_trampoline_5(
    mut __state: __MutualTco5,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    loop {
        __state = match __state {
            __MutualTco5::EvalArgsSlotToNamedEnv(mut exprs, mut params, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(exprs, [] => { return Ok(acc) }, [e, restExprs] => { match crate::aver_generated::domain::eval::core::evalExprSlot(&e, &*env, &*slotMap, &*fns) { Err(err @ _) => { return Err(err) }, Ok(v @ _) => { __MutualTco5::EvalArgsSlotToNamedEnvBind(restExprs, params, acc, v) } } })
            }
            __MutualTco5::EvalArgsSlotToNamedEnvBind(mut restExprs, mut params, mut acc, mut v) => {
                crate::cancel_checkpoint();
                aver_list_match!(params, [] => __MutualTco5::EvalArgsSlotToNamedEnv(restExprs, aver_rt::AverList::empty(), acc), [param, restParams] => __MutualTco5::EvalArgsSlotToNamedEnv(restExprs, restParams, acc.insert_owned(param, v)))
            }
        };
    }
}

/// Evaluate arg expressions directly into a callee named env from slot caller state.
pub fn evalArgsSlotToNamedEnv(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    params: &aver_rt::AverList<AverStr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    acc: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    __mutual_tco_trampoline_5(
        __MutualTco5::EvalArgsSlotToNamedEnv(exprs.clone(), params.clone(), acc.clone()),
        &env,
        &slotMap,
        &fns,
    )
}

/// Bind one evaluated arg into the callee named env when a parameter exists.
pub fn evalArgsSlotToNamedEnvBind(
    restExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    params: &aver_rt::AverList<AverStr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    acc: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    v: &crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    __mutual_tco_trampoline_5(
        __MutualTco5::EvalArgsSlotToNamedEnvBind(
            restExprs.clone(),
            params.clone(),
            acc.clone(),
            v.clone(),
        ),
        &env,
        &slotMap,
        &fns,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco6 {
    MergeBindingsSlot(
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    MergeOneBindingSlot(
        AverStr,
        crate::aver_generated::domain::value::Val,
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
}

fn __mutual_tco_trampoline_6(
    mut __state: __MutualTco6,
    bindingSlots: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> aver_rt::AverVector<crate::aver_generated::domain::value::Val> {
    loop {
        __state = match __state {
            __MutualTco6::MergeBindingsSlot(mut bindings, mut env) => {
                crate::cancel_checkpoint();
                aver_list_match!(bindings, [] => { return env }, [pair, rest] => { { let (name, val) = pair; __MutualTco6::MergeOneBindingSlot(name, val, rest, env) } })
            }
            __MutualTco6::MergeOneBindingSlot(mut name, mut val, mut rest, mut env) => {
                crate::cancel_checkpoint();
                match bindingSlots.get(&name).cloned() {
                    Some(slot @ _) => __MutualTco6::MergeBindingsSlot(
                        rest,
                        crate::aver_generated::domain::eval::slots::setSlot(&env, slot, &val),
                    ),
                    None => __MutualTco6::MergeBindingsSlot(rest, env),
                }
            }
        };
    }
}

/// Merge pattern bindings into slot env using the slots assigned for this specific arm.
pub fn mergeBindingsSlot(
    bindings: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    bindingSlots: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverVector<crate::aver_generated::domain::value::Val> {
    __mutual_tco_trampoline_6(
        __MutualTco6::MergeBindingsSlot(bindings.clone(), env.clone()),
        &bindingSlots,
    )
}

/// Merge one pattern binding into slot env.
pub fn mergeOneBindingSlot(
    name: AverStr,
    val: &crate::aver_generated::domain::value::Val,
    rest: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    bindingSlots: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverVector<crate::aver_generated::domain::value::Val> {
    __mutual_tco_trampoline_6(
        __MutualTco6::MergeOneBindingSlot(name, val.clone(), rest.clone(), env.clone()),
        &bindingSlots,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco7 {
    EvalStmtBindSlotNext(
        aver_rt::AverInt,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalStmtExprSlotNext(
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalStmtBindFallbackSlotNext(
        AverStr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalStmtsSlot(
        aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
}

fn __mutual_tco_trampoline_7(
    mut __state: __MutualTco7,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    loop {
        __state = match __state {
            __MutualTco7::EvalStmtBindSlotNext(mut slot, mut e, mut rest, mut env) => {
                crate::cancel_checkpoint();
                let v = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &e, &env, &*slotMap, &*fns,
                )?;
                let nextEnv = crate::aver_generated::domain::eval::slots::setSlot(&env, slot, &v);
                {
                    let __list_subject = rest.clone();
                    if __list_subject.is_empty() {
                        return Ok(v);
                    } else {
                        __MutualTco7::EvalStmtsSlot(rest, nextEnv)
                    }
                }
            }
            __MutualTco7::EvalStmtExprSlotNext(mut e, mut rest, mut env) => {
                crate::cancel_checkpoint();
                let v = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &e, &env, &*slotMap, &*fns,
                )?;
                {
                    let __list_subject = rest.clone();
                    if __list_subject.is_empty() {
                        return Ok(v);
                    } else {
                        __MutualTco7::EvalStmtsSlot(rest, env)
                    }
                }
            }
            __MutualTco7::EvalStmtBindFallbackSlotNext(mut name, mut e, mut rest, mut env) => {
                crate::cancel_checkpoint();
                let v = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &e, &env, &*slotMap, &*fns,
                )?;
                {
                    let __list_subject = rest.clone();
                    if __list_subject.is_empty() {
                        return Ok(v);
                    } else {
                        __MutualTco7::EvalStmtsSlot(rest, env)
                    }
                }
            }
            __MutualTco7::EvalStmtsSlot(mut stmts, mut env) => {
                crate::cancel_checkpoint();
                aver_list_match!(stmts, [] => { return Ok(crate::aver_generated::domain::value::Val::ValUnit) }, [stmt, rest] => match stmt {
                    crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, e) => {
                        __MutualTco7::EvalStmtBindSlotNext(slot, e, rest, env)
                    },
                    crate::aver_generated::domain::ast::Stmt::StmtExpr(e) => {
                        __MutualTco7::EvalStmtExprSlotNext(e, rest, env)
                    },
                    crate::aver_generated::domain::ast::Stmt::StmtBind(name, e) => {
                        __MutualTco7::EvalStmtBindFallbackSlotNext(name, e, rest, env)
                    }
                })
            }
        };
    }
}

/// Evaluate a slot binding and continue statement execution without packing a tuple.
pub fn evalStmtBindSlotNext(
    slot: aver_rt::AverInt,
    e: &crate::aver_generated::domain::ast::Expr,
    rest: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_7(
        __MutualTco7::EvalStmtBindSlotNext(slot, e.clone(), rest.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Evaluate an expression statement and continue statement execution without packing a tuple.
pub fn evalStmtExprSlotNext(
    e: &crate::aver_generated::domain::ast::Expr,
    rest: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_7(
        __MutualTco7::EvalStmtExprSlotNext(e.clone(), rest.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Fallback for unresolved StmtBind in slot path without packing a tuple.
pub fn evalStmtBindFallbackSlotNext(
    name: AverStr,
    e: &crate::aver_generated::domain::ast::Expr,
    rest: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_7(
        __MutualTco7::EvalStmtBindFallbackSlotNext(name, e.clone(), rest.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

/// Evaluate statements in slot path, threading env.
pub fn evalStmtsSlot(
    stmts: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_7(
        __MutualTco7::EvalStmtsSlot(stmts.clone(), env.clone()),
        &slotMap,
        &fns,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco8 {
    EvalStmtsSlotTail(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
        aver_rt::AverInt,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalStmtsSlotTailNext(
        aver_rt::AverInt,
        crate::aver_generated::domain::ast::Stmt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
        aver_rt::AverInt,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalStmtsSlotTailBind(
        aver_rt::AverInt,
        aver_rt::AverInt,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
        aver_rt::AverInt,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
    EvalStmtsSlotTailExpr(
        aver_rt::AverInt,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
        aver_rt::AverInt,
        aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    ),
}

fn __mutual_tco_trampoline_8(
    mut __state: __MutualTco8,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    loop {
        __state = match __state {
            __MutualTco8::EvalStmtsSlotTail(mut selfId, mut stmts, mut slotCount, mut env) => {
                crate::cancel_checkpoint();
                aver_list_match!(stmts, [] => { return Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(crate::aver_generated::domain::value::Val::ValUnit)) }, [stmt, rest] => { { let __list_subject = rest.clone(); if __list_subject.is_empty() { return crate::aver_generated::domain::eval::core::evalTailStmtSlot(selfId, &stmt, slotCount, &env, &*slotMap, &*fns) } else { __MutualTco8::EvalStmtsSlotTailNext(selfId, stmt, rest, slotCount, env) } } })
            }
            __MutualTco8::EvalStmtsSlotTailNext(
                mut selfId,
                mut stmt,
                mut rest,
                mut slotCount,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                match stmt {
                    crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, e) => {
                        __MutualTco8::EvalStmtsSlotTailBind(selfId, slot, e, rest, slotCount, env)
                    }
                    crate::aver_generated::domain::ast::Stmt::StmtExpr(e) => {
                        __MutualTco8::EvalStmtsSlotTailExpr(selfId, e, rest, slotCount, env)
                    }
                    crate::aver_generated::domain::ast::Stmt::StmtBind(_, e) => {
                        __MutualTco8::EvalStmtsSlotTailExpr(selfId, e, rest, slotCount, env)
                    }
                }
            }
            __MutualTco8::EvalStmtsSlotTailBind(
                mut selfId,
                mut slot,
                mut e,
                mut rest,
                mut slotCount,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                let v = crate::aver_generated::domain::eval::core::evalExprSlot(
                    &e, &env, &*slotMap, &*fns,
                )?;
                __MutualTco8::EvalStmtsSlotTail(
                    selfId,
                    rest,
                    slotCount,
                    crate::aver_generated::domain::eval::slots::setSlot(&env, slot, &v),
                )
            }
            __MutualTco8::EvalStmtsSlotTailExpr(
                mut selfId,
                mut e,
                mut rest,
                mut slotCount,
                mut env,
            ) => {
                crate::cancel_checkpoint();
                crate::aver_generated::domain::eval::core::evalExprSlot(
                    &e, &env, &*slotMap, &*fns,
                )?;
                __MutualTco8::EvalStmtsSlotTail(selfId, rest, slotCount, env)
            }
        };
    }
}

/// Evaluate slot statements when the final expression may self-tail-recur.
pub fn evalStmtsSlotTail(
    selfId: aver_rt::AverInt,
    stmts: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slotCount: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::EvalStmtsSlotTail(selfId, stmts.clone(), slotCount, env.clone()),
        &slotMap,
        &fns,
    )
}

/// Evaluate one non-final slot statement before continuing the tail-aware slot loop.
pub fn evalStmtsSlotTailNext(
    selfId: aver_rt::AverInt,
    stmt: &crate::aver_generated::domain::ast::Stmt,
    rest: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slotCount: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::EvalStmtsSlotTailNext(
            selfId,
            stmt.clone(),
            rest.clone(),
            slotCount,
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Evaluate one slot binding before continuing the tail-aware slot loop.
pub fn evalStmtsSlotTailBind(
    selfId: aver_rt::AverInt,
    slot: aver_rt::AverInt,
    e: &crate::aver_generated::domain::ast::Expr,
    rest: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slotCount: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::EvalStmtsSlotTailBind(
            selfId,
            slot,
            e.clone(),
            rest.clone(),
            slotCount,
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Evaluate one non-final expression statement before continuing the tail-aware slot loop.
pub fn evalStmtsSlotTailExpr(
    selfId: aver_rt::AverInt,
    e: &crate::aver_generated::domain::ast::Expr,
    rest: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slotCount: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::EvalStmtsSlotTailExpr(
            selfId,
            e.clone(),
            rest.clone(),
            slotCount,
            env.clone(),
        ),
        &slotMap,
        &fns,
    )
}

/// Recognize immediate literal expressions in named-env mode.
pub fn evalImmediateNamedExpr(
    expr: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::value::Val> {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprInt(n) => {
            Some(crate::aver_generated::domain::value::Val::ValInt(n))
        }
        crate::aver_generated::domain::ast::Expr::ExprFloat(f) => {
            Some(crate::aver_generated::domain::value::Val::ValFloat(f))
        }
        crate::aver_generated::domain::ast::Expr::ExprStr(s) => {
            Some(crate::aver_generated::domain::value::Val::ValStr(s))
        }
        crate::aver_generated::domain::ast::Expr::ExprBool(b) => {
            Some(crate::aver_generated::domain::value::Val::ValBool(b))
        }
        _ => None,
    }
}

/// Name the AST node kind for diagnostics; interpolation renders primitives only, so an Expr needs a named conversion.
pub fn exprLabel(expr: &crate::aver_generated::domain::ast::Expr) -> AverStr {
    crate::cancel_checkpoint();
    match expr {
        crate::aver_generated::domain::ast::Expr::ExprInt(_) => AverStr::from("ExprInt"),
        crate::aver_generated::domain::ast::Expr::ExprFloat(_) => AverStr::from("ExprFloat"),
        crate::aver_generated::domain::ast::Expr::ExprStr(_) => AverStr::from("ExprStr"),
        crate::aver_generated::domain::ast::Expr::ExprBool(_) => AverStr::from("ExprBool"),
        crate::aver_generated::domain::ast::Expr::ExprBoolBranch(_, _, _) => {
            AverStr::from("ExprBoolBranch")
        }
        crate::aver_generated::domain::ast::Expr::ExprVar(_) => AverStr::from("ExprVar"),
        crate::aver_generated::domain::ast::Expr::ExprSlot(_) => AverStr::from("ExprSlot"),
        crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(_, _, _) => {
            AverStr::from("ExprBinopSlotInt")
        }
        crate::aver_generated::domain::ast::Expr::ExprBinopSlots(_, _, _) => {
            AverStr::from("ExprBinopSlots")
        }
        crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(_, _, _) => {
            AverStr::from("ExprCmpSlotInt")
        }
        crate::aver_generated::domain::ast::Expr::ExprCmpSlots(_, _, _) => {
            AverStr::from("ExprCmpSlots")
        }
        crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(_, _, _) => {
            AverStr::from("ExprVectorGetOrInt")
        }
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(_, _, _) => {
            AverStr::from("ExprIntModOrInt")
        }
        crate::aver_generated::domain::ast::Expr::ExprAdd(_, _) => AverStr::from("ExprAdd"),
        crate::aver_generated::domain::ast::Expr::ExprSub(_, _) => AverStr::from("ExprSub"),
        crate::aver_generated::domain::ast::Expr::ExprMul(_, _) => AverStr::from("ExprMul"),
        crate::aver_generated::domain::ast::Expr::ExprDiv(_, _) => AverStr::from("ExprDiv"),
        crate::aver_generated::domain::ast::Expr::ExprNeg(_) => AverStr::from("ExprNeg"),
        crate::aver_generated::domain::ast::Expr::ExprEq(_, _) => AverStr::from("ExprEq"),
        crate::aver_generated::domain::ast::Expr::ExprNeq(_, _) => AverStr::from("ExprNeq"),
        _ => crate::aver_generated::domain::eval::core::exprLabelAggregate(expr),
    }
}

/// Continue naming AST node kinds for the comparison, aggregate, and call forms.
pub fn exprLabelAggregate(expr: &crate::aver_generated::domain::ast::Expr) -> AverStr {
    crate::cancel_checkpoint();
    match expr {
        crate::aver_generated::domain::ast::Expr::ExprLt(_, _) => AverStr::from("ExprLt"),
        crate::aver_generated::domain::ast::Expr::ExprGt(_, _) => AverStr::from("ExprGt"),
        crate::aver_generated::domain::ast::Expr::ExprLte(_, _) => AverStr::from("ExprLte"),
        crate::aver_generated::domain::ast::Expr::ExprGte(_, _) => AverStr::from("ExprGte"),
        crate::aver_generated::domain::ast::Expr::ExprConcat(_) => AverStr::from("ExprConcat"),
        crate::aver_generated::domain::ast::Expr::ExprTuple(_) => AverStr::from("ExprTuple"),
        crate::aver_generated::domain::ast::Expr::ExprList(_) => AverStr::from("ExprList"),
        crate::aver_generated::domain::ast::Expr::ExprRecord(_, _) => AverStr::from("ExprRecord"),
        crate::aver_generated::domain::ast::Expr::ExprFieldAccess(_, _) => {
            AverStr::from("ExprFieldAccess")
        }
        crate::aver_generated::domain::ast::Expr::ExprCall(_, _) => AverStr::from("ExprCall"),
        crate::aver_generated::domain::ast::Expr::ExprCallDirect(_, _) => {
            AverStr::from("ExprCallDirect")
        }
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(_, _) => {
            AverStr::from("ExprCallBuiltin")
        }
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(_, _) => {
            AverStr::from("ExprCallBuiltinId")
        }
        crate::aver_generated::domain::ast::Expr::ExprMatch(_, _) => AverStr::from("ExprMatch"),
        crate::aver_generated::domain::ast::Expr::ExprPropagate(_) => {
            AverStr::from("ExprPropagate")
        }
        crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(_, _) => {
            AverStr::from("ExprIndependentProduct")
        }
        _ => AverStr::from("Expr"),
    }
}

/// Look up variable, Option.None, a named function reference, or a nullary variant constructor.
#[inline(always)]
pub fn evalVar(
    name: AverStr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Unit" {
            Ok(crate::aver_generated::domain::value::Val::ValUnit)
        } else {
            if &*__dispatch_subject == "Option.None" {
                Ok(crate::aver_generated::domain::value::Val::ValNone)
            } else {
                match crate::aver_generated::domain::eval::store::lookupVar(env, name.clone()) {
                    Ok(v @ _) => Ok(v),
                    Err(_) => {
                        crate::aver_generated::domain::eval::common::evalVarFallback(name, fns)
                    }
                }
            }
        }
    }
}

/// Evaluate a fused Vector.get + Option.withDefault in map path.
pub fn evalVectorGetOrIntExpr(
    vecExpr: &crate::aver_generated::domain::ast::Expr,
    idxExpr: &crate::aver_generated::domain::ast::Expr,
    defaultValue: aver_rt::AverInt,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV = crate::aver_generated::domain::eval::core::evalExpr(vecExpr, env, fns)?;
    let idxV = crate::aver_generated::domain::eval::core::evalExpr(idxExpr, env, fns)?;
    crate::aver_generated::domain::eval::ops::evalVectorGetOrIntVals(&vecV, &idxV, defaultValue)
}

/// Evaluate a fused Int.mod + Result.withDefault in map path.
pub fn evalIntModOrIntExpr(
    a: &crate::aver_generated::domain::ast::Expr,
    b: &crate::aver_generated::domain::ast::Expr,
    defaultValue: aver_rt::AverInt,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let aV = crate::aver_generated::domain::eval::core::evalExpr(a, env, fns)?;
    let bV = crate::aver_generated::domain::eval::core::evalExpr(b, env, fns)?;
    crate::aver_generated::domain::eval::ops::evalIntModOrIntVals(&aV, &bV, defaultValue)
}

/// Evaluate ? operator: unwrap Ok or propagate Err.
pub fn evalPropagate(
    inner: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::eval::core::evalExpr(inner, env, fns)?;
    match v {
        crate::aver_generated::domain::value::Val::ValOk(x) => {
            let x = (*x).clone();
            Ok(x)
        }
        crate::aver_generated::domain::value::Val::ValErr(e) => {
            let e = (*e).clone();
            match e {
                crate::aver_generated::domain::value::Val::ValStr(msg) => {
                    Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(msg))
                }
                _ => Err(
                    crate::aver_generated::domain::eval::common::wrapPropagatedError(
                        AverStr::from("propagated error"),
                    ),
                ),
            }
        }
        _ => Err(AverStr::from("? operator requires Result value")),
    }
}

/// Evaluate independent product using the self-host's own divide-and-conquer ?!, then unwrap guest Results if needed.
pub fn evalIndependentProduct(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    unwrap: bool,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items = crate::aver_generated::domain::eval::core::evalIndependentItems(exprs, env, fns)?;
    if unwrap {
        crate::aver_generated::domain::eval::core::unwrapProductResults__collected(
            items,
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
        )
    } else {
        Ok(crate::aver_generated::domain::value::Val::ValTuple(items))
    }
}

/// Evaluate guest independent-product branches through a balanced tree of the self-host's own ?!.
pub fn evalIndependentItems(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(exprs.clone(), [] => Ok(aver_rt::AverList::empty()), [a, rest] => { aver_list_match!(rest, [] => match crate::aver_generated::domain::eval::core::evalExpr(&a, env, fns) { Ok(va @ _) => { Ok(aver_rt::AverList::from_vec(vec![va])) }, Err(e @ _) => { Err(e) } }, [b, rest2] => { { let __list_subject = rest2; if __list_subject.is_empty() { { let (va, vb) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let _r0 = crate::aver_generated::domain::eval::core::evalExpr(&a, env, fns); crate::aver_replay::set_effect_branch(1); let _r1 = crate::aver_generated::domain::eval::core::evalExpr(&b, env, fns); crate::aver_replay::exit_effect_group(); match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|_s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let _h0 = { let env = env; let fns = fns; let a = a.clone(); _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalExpr(&a, env, fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let _h1 = { let env = env; let fns = fns; let b = b.clone(); _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalExpr(&b, env, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let _b0 = _h0.join().unwrap(); let _b1 = _h1.join().unwrap(); match (_b0, _b1) { (crate::ParallelBranch::Completed(_r0), crate::ParallelBranch::Completed(_r1)) => match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (_b0, _b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = _b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = _b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok(aver_rt::AverList::from_vec(vec![va, vb])) } } else { { let (leftExprs, rightExprs) = crate::aver_generated::domain::eval::core::splitIndependentExprs(exprs.clone(), aver_rt::AverList::empty(), aver_rt::AverList::empty(), true); { let (leftVals, rightVals) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let _r0 = crate::aver_generated::domain::eval::core::evalIndependentItems(&leftExprs, env, fns); crate::aver_replay::set_effect_branch(1); let _r1 = crate::aver_generated::domain::eval::core::evalIndependentItems(&rightExprs, env, fns); crate::aver_replay::exit_effect_group(); match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|_s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let _h0 = { let env = env; let fns = fns; let leftExprs = leftExprs.clone(); _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalIndependentItems(&leftExprs, env, fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let _h1 = { let env = env; let fns = fns; let rightExprs = rightExprs.clone(); _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalIndependentItems(&rightExprs, env, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let _b0 = _h0.join().unwrap(); let _b1 = _h1.join().unwrap(); match (_b0, _b1) { (crate::ParallelBranch::Completed(_r0), crate::ParallelBranch::Completed(_r1)) => match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (_b0, _b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = _b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = _b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok(crate::aver_generated::domain::eval::core::interleaveIndependentVals(leftVals, rightVals, true, aver_rt::AverList::empty())) } } } } }) })
}

/// Split a product into alternating branches so recursive ?! can cover all items.
#[inline(always)]
pub fn splitIndependentExprs(
    mut exprs: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut leftAcc: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut rightAcc: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut sendLeft: bool,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return (leftAcc.reverse(), rightAcc.reverse()); }, [expr, rest] => { if sendLeft { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(expr, &leftAcc);
            let __tco3 = false;
            exprs = __tco0;
            leftAcc = __tco1;
            sendLeft = __tco3;
            continue;
        } } else { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(expr, &rightAcc);
            let __tco3 = true;
            exprs = __tco0;
            rightAcc = __tco2;
            sendLeft = __tco3;
            continue;
        } } })
    }
}

/// Restore original product order after alternating recursive split.
#[inline(always)]
pub fn interleaveIndependentVals(
    mut left: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut right: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut takeLeft: bool,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        if takeLeft {
            aver_list_match!(left, [] => { return crate::aver_generated::domain::eval::core::finishIndependentVals(right, acc); }, [v, rest] => { {
            let __tco0 = rest;
            let __tco2 = false;
            let __tco3 = aver_rt::AverList::prepend(v, &acc);
            left = __tco0;
            takeLeft = __tco2;
            acc = __tco3;
            continue;
        } })
        } else {
            aver_list_match!(right, [] => { return crate::aver_generated::domain::eval::core::finishIndependentVals(left, acc); }, [v, rest] => { {
            let __tco1 = rest;
            let __tco2 = true;
            let __tco3 = aver_rt::AverList::prepend(v, &acc);
            right = __tco1;
            takeLeft = __tco2;
            acc = __tco3;
            continue;
        } })
        }
    }
}

/// Append remaining values after interleaving and return in forward order.
#[inline(always)]
pub fn finishIndependentVals(
    mut items: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(items, [] => { return acc.reverse(); }, [v, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(v, &acc);
            items = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Unwrap all Result values in an independent product (?!).
#[inline(always)]
pub fn unwrapProductResults(
    mut items: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(items, [] => { return Ok(crate::aver_generated::domain::value::Val::ValTuple(acc.reverse())); }, [v, rest] => { match v {
        crate::aver_generated::domain::value::Val::ValOk(x) => {
            let x = (*x).clone();
            {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(x, &acc);
            items = __tco0;
            acc = __tco1;
            continue;
        }
        },
        crate::aver_generated::domain::value::Val::ValErr(e) => {
            let e = (*e).clone();
            match e {
        crate::aver_generated::domain::value::Val::ValStr(msg) => {
            return Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(msg));
        },
        _ => {
            return Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(AverStr::from("propagated error")));
        }
    }
        },
        _ => {
            return Err(AverStr::from("?! operator requires all elements to be Result values"));
        }
    } })
    }
}

/// Evaluate right side of binop and apply.
pub fn evalBinopRight(
    va: &crate::aver_generated::domain::value::Val,
    b: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    op: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(b, env, fns) {
        Err(e @ _) => Err(e),
        Ok(vb @ _) => crate::aver_generated::domain::eval::ops::evalBinopVals(va, &vb, op),
    }
}

/// Evaluate a binary operation on two expressions.
pub fn evalBinop(
    a: &crate::aver_generated::domain::ast::Expr,
    b: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    op: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(a, env, fns) {
        Err(e @ _) => Err(e),
        Ok(va @ _) => {
            crate::aver_generated::domain::eval::core::evalBinopRight(&va, b, env, fns, op)
        }
    }
}

/// Evaluate unary minus on the named-env path.
pub fn evalNeg(
    inner: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(inner, env, fns) {
        Err(e @ _) => Err(e),
        Ok(v @ _) => crate::aver_generated::domain::eval::ops::evalNegVals(&v),
    }
}

/// Evaluate right side of comparison and apply.
pub fn evalCmpRight(
    va: &crate::aver_generated::domain::value::Val,
    b: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    op: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(b, env, fns) {
        Err(e @ _) => Err(e),
        Ok(vb @ _) => crate::aver_generated::domain::eval::ops::evalCmpVals(va, &vb, op),
    }
}

/// Evaluate a comparison expression.
pub fn evalCmp(
    a: &crate::aver_generated::domain::ast::Expr,
    b: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    op: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(a, env, fns) {
        Err(e @ _) => Err(e),
        Ok(va @ _) => crate::aver_generated::domain::eval::core::evalCmpRight(&va, b, env, fns, op),
    }
}

/// Evaluate string interpolation: concat all parts as strings.
pub fn evalConcatExpr(
    parts: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalConcatParts(
        parts.clone(),
        env.clone(),
        fns.clone(),
        AverStr::from(""),
    )
}

/// Concatenate interpolation parts. Self-recursive for codegen TCO.
pub fn evalConcatParts(
    mut parts: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: crate::aver_generated::domain::eval::store::FnStore,
    mut acc: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    let fns = std::sync::Arc::new(fns);
    let env = std::sync::Arc::new(env);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(parts, [] => { return Ok(crate::aver_generated::domain::value::Val::ValStr(acc)); }, [p, rest] => { match crate::aver_generated::domain::eval::core::evalExpr(&p, &*env, &*fns) { Err(e @ _) => { return Err(e); }, Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco3 = (acc + &crate::aver_generated::domain::value::valRepr(&v));
            parts = __tco0;
            acc = __tco3;
            continue;
        } } } })
    }
}

/// Evaluate tuple literal.
pub fn evalTupleExpr(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items = crate::aver_generated::domain::eval::core::evalListItems(exprs, env, fns)?;
    Ok(crate::aver_generated::domain::value::Val::ValTuple(items))
}

/// Evaluate record constructor.
pub fn evalRecordExpr(
    name: AverStr,
    fieldExprs: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fields = crate::aver_generated::domain::eval::core::evalRecordFields(fieldExprs, env, fns)?;
    Ok(crate::aver_generated::domain::value::Val::ValRecord(
        name, fields,
    ))
}

/// Evaluate record field expressions.
pub fn evalRecordFields(
    fieldExprs: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(fieldExprs.clone(), [] => Ok(aver_rt::AverList::empty()), [pair, rest] => { { let (fname, expr) = pair; crate::aver_generated::domain::eval::core::evalRecordFieldOne(fname, &expr, &rest, env, fns) } })
}

/// Evaluate one record field and continue.
pub fn evalRecordFieldOne(
    fname: AverStr,
    expr: &crate::aver_generated::domain::ast::Expr,
    rest: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::eval::core::evalExpr(expr, env, fns)?;
    let restFields = crate::aver_generated::domain::eval::core::evalRecordFields(rest, env, fns)?;
    Ok(aver_rt::AverList::prepend((fname, v), &restFields))
}

/// Evaluate field access: obj.field.
pub fn evalFieldAccess(
    obj: &crate::aver_generated::domain::ast::Expr,
    field: AverStr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::eval::core::evalExpr(obj, env, fns)?;
    match v {
        crate::aver_generated::domain::value::Val::ValRecord(_, fields) => {
            crate::aver_generated::domain::eval::common::lookupField(fields, field)
        }
        _ => Err(AverStr::from("field access on non-record")),
    }
}

/// Evaluate a list literal.
pub fn evalListExpr(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalListItems(exprs, env, fns) {
        Ok(items @ _) => Ok(crate::aver_generated::domain::value::Val::ValList(items)),
        Err(e @ _) => Err(e),
    }
}

/// Evaluate list of expressions into list of values.
pub fn evalListItems(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalListItemsRev(
        exprs.clone(),
        env.clone(),
        fns.clone(),
        aver_rt::AverList::empty(),
    )
}

/// Tail-recursive worker for evalListItems. Accumulates in reverse.
pub fn evalListItemsRev(
    mut exprs: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: crate::aver_generated::domain::eval::store::FnStore,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let fns = std::sync::Arc::new(fns);
    let env = std::sync::Arc::new(env);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return Ok(acc.reverse()); }, [e, rest] => { match crate::aver_generated::domain::eval::core::evalExpr(&e, &*env, &*fns) { Err(err @ _) => { return Err(err); }, Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco3 = aver_rt::AverList::prepend(v, &acc);
            exprs = __tco0;
            acc = __tco3;
            continue;
        } } } })
    }
}

/// Evaluate a list of argument expressions.
pub fn evalArgs(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalArgsRev(
        exprs.clone(),
        env.clone(),
        fns.clone(),
        aver_rt::AverList::empty(),
    )
}

/// Tail-recursive worker for evalArgs. Accumulates in reverse.
pub fn evalArgsRev(
    mut exprs: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: crate::aver_generated::domain::eval::store::FnStore,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let env = std::sync::Arc::new(env);
    let fns = std::sync::Arc::new(fns);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return Ok(acc.reverse()); }, [e, rest] => { match crate::aver_generated::domain::eval::core::evalExpr(&e, &*env, &*fns) { Err(err @ _) => { return Err(err); }, Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco3 = aver_rt::AverList::prepend(v, &acc);
            exprs = __tco0;
            acc = __tco3;
            continue;
        } } } })
    }
}

/// Dispatch call: record update if Type.update, else normal call.
pub fn callWithArgs(
    args: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    name: AverStr,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::eval::records::isRecordUpdate(name.clone(), args) {
        crate::aver_generated::domain::eval::records::doRecordUpdate(args)
    } else {
        crate::aver_generated::domain::eval::core::callWithArgsNormal(args, name, fns)
    }
}

/// Normal function call dispatch. Uses slot-based eval for resolved fns.
pub fn callWithArgsNormal(
    args: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    name: AverStr,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::store::lookupFnOption(fns, name.clone()) {
        Some(fd @ _) => crate::aver_generated::domain::eval::core::callResolved(&fd, args, fns),
        None => crate::aver_generated::domain::builtins::callBuiltin(name, args),
    }
}

/// Call a function: slot-based if resolved, map-based otherwise.
pub fn callResolved(
    fd: &crate::aver_generated::domain::ast::FnDef,
    args: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fnId = crate::aver_generated::domain::eval::store::lookupFnId(fns, fd.name.clone())?;
    crate::aver_generated::domain::eval::core::callResolvedById(fnId, fd, args, fns)
}

/// Call a function with a known store id: slot-based if resolved, map-based otherwise.
pub fn callResolvedById(
    fnId: aver_rt::AverInt,
    fd: &crate::aver_generated::domain::ast::FnDef,
    args: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if (fd.slotCount > aver_rt::AverInt::from_i64(0)) {
        crate::aver_generated::domain::eval::core::evalResolvedSlotFn(
            fnId,
            fd,
            &crate::aver_generated::domain::eval::slots::buildSlotEnv(args, fd.slotCount.clone())?,
            fns,
        )
    } else {
        crate::aver_generated::domain::eval::core::evalResolvedNamedFn(
            fd,
            &crate::aver_generated::domain::eval::store::zipArgs(
                fd.params.clone(),
                args.clone(),
                HashMap::new(),
            ),
            fns,
        )
    }
}

/// Evaluate a resolved function body in slot mode, looping self-tail-calls instead of recursing on the host stack.
pub fn evalResolvedSlotFn(
    fnId: aver_rt::AverInt,
    fd: &crate::aver_generated::domain::ast::FnDef,
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if fd.tailLoop {
        crate::aver_generated::domain::eval::core::evalResolvedSlotLoop(
            fnId,
            fd.clone(),
            calleeEnv.clone(),
            fns.clone(),
        )
    } else {
        crate::aver_generated::domain::eval::core::evalResolvedSlotDirect(fd, calleeEnv, fns)
    }
}

/// Run the tail-aware slot evaluator only for functions that actually end in self-tail-calls.
pub fn evalResolvedSlotLoop(
    mut fnId: aver_rt::AverInt,
    fd: crate::aver_generated::domain::ast::FnDef,
    mut calleeEnv: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fns: crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    let fns = std::sync::Arc::new(fns);
    let fd = std::sync::Arc::new(fd);
    loop {
        crate::cancel_checkpoint();
        let step = crate::aver_generated::domain::eval::core::evalResolvedSlotStep(
            fnId.clone(),
            &*fd,
            &calleeEnv,
            &*fns,
        )?;
        match step {
            crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v) => {
                return crate::aver_generated::domain::eval::common::normalizeFnReturn(&Ok(v));
            }
            crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailRecurEnv(nextEnv) => {
                let __tco2 = nextEnv;
                calleeEnv = __tco2;
                continue;
            }
        }
    }
}

/// Evaluate a resolved function body in slot mode without the tail-loop machinery.
pub fn evalResolvedSlotDirect(
    fd: &crate::aver_generated::domain::ast::FnDef,
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let result = match fd.fastPath.clone() {
        crate::aver_generated::domain::ast::FnFastPath::FastLeaf(leaf) => {
            crate::aver_generated::domain::eval::fast::runFastLeafSlot(&leaf, calleeEnv)
        }
        crate::aver_generated::domain::ast::FnFastPath::FastForwardCall(targetId, slotArgs) => {
            crate::aver_generated::domain::eval::core::fastForwardCall(
                calleeEnv, targetId, &slotArgs, fns,
            )
        }
        crate::aver_generated::domain::ast::FnFastPath::FastBoolSlotBranch(
            slot,
            thenLeaf,
            elseLeaf,
        ) => crate::aver_generated::domain::eval::fast::fastBoolSlotBranch(
            calleeEnv, slot, &thenLeaf, &elseLeaf,
        ),
        crate::aver_generated::domain::ast::FnFastPath::FastEqIntBranch(
            slot,
            expected,
            thenLeaf,
            elseLeaf,
        ) => crate::aver_generated::domain::eval::fast::fastEqIntBranch(
            calleeEnv, slot, expected, &thenLeaf, &elseLeaf,
        ),
        crate::aver_generated::domain::ast::FnFastPath::FastEqStringBranch(
            slot,
            expected,
            thenLeaf,
            elseLeaf,
        ) => crate::aver_generated::domain::eval::fast::fastEqStringBranch(
            calleeEnv, slot, expected, &thenLeaf, &elseLeaf,
        ),
        crate::aver_generated::domain::ast::FnFastPath::FastLtIntSlotsBranch(
            lhsSlot,
            rhsSlot,
            thenLeaf,
            elseLeaf,
        ) => crate::aver_generated::domain::eval::fast::fastLtIntSlotsBranch(
            calleeEnv, lhsSlot, rhsSlot, &thenLeaf, &elseLeaf,
        ),
        crate::aver_generated::domain::ast::FnFastPath::FastListSlotBranch(
            slot,
            emptyLeaf,
            headSlot,
            tailSlot,
            consLeaf,
        ) => crate::aver_generated::domain::eval::fast::fastListSlotBranch(
            calleeEnv, slot, &emptyLeaf, headSlot, tailSlot, &consLeaf,
        ),
        crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr => {
            crate::aver_generated::domain::eval::core::evalResolvedSingleExprSlot(
                &fd.body,
                calleeEnv,
                &fd.slotMap,
                fns,
            )
        }
        crate::aver_generated::domain::ast::FnFastPath::FastNone => {
            crate::aver_generated::domain::eval::core::evalStmtsSlot(
                &fd.body,
                calleeEnv,
                &fd.slotMap,
                fns,
            )
        }
    };
    crate::aver_generated::domain::eval::common::normalizeFnReturn(&result)
}

/// Run one slot-frame step and either finish with a value or request a self-tail-call re-entry.
pub fn evalResolvedSlotStep(
    fnId: aver_rt::AverInt,
    fd: &crate::aver_generated::domain::ast::FnDef,
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    crate::cancel_checkpoint();
    match fd.fastPath.clone() {
        crate::aver_generated::domain::ast::FnFastPath::FastLeaf(leaf) => {
            match crate::aver_generated::domain::eval::fast::runFastLeafSlot(&leaf, calleeEnv) {
                Ok(v @ _) => {
                    Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v))
                }
                Err(e @ _) => Err(e),
            }
        }
        crate::aver_generated::domain::ast::FnFastPath::FastForwardCall(targetId, slotArgs) => {
            match crate::aver_generated::domain::eval::core::fastForwardCall(
                calleeEnv, targetId, &slotArgs, fns,
            ) {
                Ok(v @ _) => {
                    Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v))
                }
                Err(e @ _) => Err(e),
            }
        }
        crate::aver_generated::domain::ast::FnFastPath::FastBoolSlotBranch(
            slot,
            thenLeaf,
            elseLeaf,
        ) => {
            match crate::aver_generated::domain::eval::fast::fastBoolSlotBranch(
                calleeEnv, slot, &thenLeaf, &elseLeaf,
            ) {
                Ok(v @ _) => {
                    Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v))
                }
                Err(e @ _) => Err(e),
            }
        }
        crate::aver_generated::domain::ast::FnFastPath::FastEqIntBranch(
            slot,
            expected,
            thenLeaf,
            elseLeaf,
        ) => {
            match crate::aver_generated::domain::eval::fast::fastEqIntBranch(
                calleeEnv, slot, expected, &thenLeaf, &elseLeaf,
            ) {
                Ok(v @ _) => {
                    Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v))
                }
                Err(e @ _) => Err(e),
            }
        }
        crate::aver_generated::domain::ast::FnFastPath::FastEqStringBranch(
            slot,
            expected,
            thenLeaf,
            elseLeaf,
        ) => {
            match crate::aver_generated::domain::eval::fast::fastEqStringBranch(
                calleeEnv, slot, expected, &thenLeaf, &elseLeaf,
            ) {
                Ok(v @ _) => {
                    Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v))
                }
                Err(e @ _) => Err(e),
            }
        }
        crate::aver_generated::domain::ast::FnFastPath::FastLtIntSlotsBranch(
            lhsSlot,
            rhsSlot,
            thenLeaf,
            elseLeaf,
        ) => {
            match crate::aver_generated::domain::eval::fast::fastLtIntSlotsBranch(
                calleeEnv, lhsSlot, rhsSlot, &thenLeaf, &elseLeaf,
            ) {
                Ok(v @ _) => {
                    Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v))
                }
                Err(e @ _) => Err(e),
            }
        }
        crate::aver_generated::domain::ast::FnFastPath::FastListSlotBranch(
            slot,
            emptyLeaf,
            headSlot,
            tailSlot,
            consLeaf,
        ) => {
            match crate::aver_generated::domain::eval::fast::fastListSlotBranch(
                calleeEnv, slot, &emptyLeaf, headSlot, tailSlot, &consLeaf,
            ) {
                Ok(v @ _) => {
                    Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v))
                }
                Err(e @ _) => Err(e),
            }
        }
        crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr => {
            crate::aver_generated::domain::eval::core::evalResolvedSingleExprSlotTail(
                fnId,
                &fd.body,
                fd.slotCount.clone(),
                &fd.slotMap,
                calleeEnv,
                fns,
            )
        }
        crate::aver_generated::domain::ast::FnFastPath::FastNone => {
            crate::aver_generated::domain::eval::core::evalStmtsSlotTail(
                fnId,
                &fd.body,
                fd.slotCount.clone(),
                calleeEnv,
                &fd.slotMap,
                fns,
            )
        }
    }
}

/// Evaluate a resolved function body in named-env mode, using a fast expr tag when available.
pub fn evalResolvedNamedFn(
    fd: &crate::aver_generated::domain::ast::FnDef,
    calleeEnv: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let result = match fd.fastPath.clone() {
        crate::aver_generated::domain::ast::FnFastPath::FastLeaf(leaf) => {
            crate::aver_generated::domain::eval::core::runFastLeafNamed(
                &leaf, &fd.body, calleeEnv, fns,
            )
        }
        crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr => {
            crate::aver_generated::domain::eval::core::evalResolvedSingleExprNamed(
                &fd.body, calleeEnv, fns,
            )
        }
        crate::aver_generated::domain::ast::FnFastPath::FastNone => {
            crate::aver_generated::domain::eval::core::evalStmts(
                fd.body.clone(),
                calleeEnv.clone(),
                fns.clone(),
            )
        }
        _ => crate::aver_generated::domain::eval::core::evalResolvedSingleExprNamed(
            &fd.body, calleeEnv, fns,
        ),
    };
    crate::aver_generated::domain::eval::common::normalizeFnReturn(&result)
}

/// Fast path for a single expression body in slot mode.
pub fn evalResolvedSingleExprSlot(
    body: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = body.clone();
        if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            if (rest == aver_rt::AverList::empty()) {
                crate::aver_generated::domain::eval::core::evalResolvedSingleStmtSlot(
                    &stmt, calleeEnv, slotMap, fns,
                )
            } else {
                crate::aver_generated::domain::eval::core::evalStmtsSlot(
                    body, calleeEnv, slotMap, fns,
                )
            }
        } else {
            crate::aver_generated::domain::eval::core::evalStmtsSlot(body, calleeEnv, slotMap, fns)
        }
    }
}

/// Tail-aware version of the single-expression fast path for slot functions.
pub fn evalResolvedSingleExprSlotTail(
    selfId: aver_rt::AverInt,
    body: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slotCount: aver_rt::AverInt,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = body.clone();
        if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            if (rest == aver_rt::AverList::empty()) {
                crate::aver_generated::domain::eval::core::evalTailStmtSlot(
                    selfId, &stmt, slotCount, calleeEnv, slotMap, fns,
                )
            } else {
                match crate::aver_generated::domain::eval::core::evalStmtsSlot(
                    body, calleeEnv, slotMap, fns,
                ) {
                    Ok(v @ _) => Ok(
                        crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v),
                    ),
                    Err(e @ _) => Err(e),
                }
            }
        } else {
            match crate::aver_generated::domain::eval::core::evalStmtsSlot(
                body, calleeEnv, slotMap, fns,
            ) {
                Ok(v @ _) => {
                    Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v))
                }
                Err(e @ _) => Err(e),
            }
        }
    }
}

/// Evaluate a final statement in slot mode, capturing self-tail-calls as loop re-entry requests.
pub fn evalTailStmtSlot(
    selfId: aver_rt::AverInt,
    stmt: &crate::aver_generated::domain::ast::Stmt,
    slotCount: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    crate::cancel_checkpoint();
    match stmt.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::eval::core::evalTailExprSlot(
                selfId, &expr, slotCount, env, slotMap, fns,
            )
        }
        _ => {
            match crate::aver_generated::domain::eval::core::evalStmtsSlot(
                &aver_rt::AverList::from_vec(vec![stmt.clone()]),
                env,
                slotMap,
                fns,
            ) {
                Ok(v @ _) => {
                    Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v))
                }
                Err(e @ _) => Err(e),
            }
        }
    }
}

/// Run the single expression stmt fast path in slot mode.
pub fn evalResolvedSingleStmtSlot(
    stmt: &crate::aver_generated::domain::ast::Stmt,
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match stmt.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::eval::core::evalExprSlot(&expr, calleeEnv, slotMap, fns)
        }
        _ => crate::aver_generated::domain::eval::core::evalStmtsSlot(
            &aver_rt::AverList::from_vec(vec![stmt.clone()]),
            calleeEnv,
            slotMap,
            fns,
        ),
    }
}

/// Fast path for a single expression body in named-env mode.
pub fn evalResolvedSingleExprNamed(
    body: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    calleeEnv: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = body.clone();
        if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            if (rest == aver_rt::AverList::empty()) {
                crate::aver_generated::domain::eval::core::evalResolvedSingleStmtNamed(
                    &stmt, calleeEnv, fns,
                )
            } else {
                crate::aver_generated::domain::eval::core::evalStmts(
                    body.clone(),
                    calleeEnv.clone(),
                    fns.clone(),
                )
            }
        } else {
            crate::aver_generated::domain::eval::core::evalStmts(
                body.clone(),
                calleeEnv.clone(),
                fns.clone(),
            )
        }
    }
}

/// Run the single expression stmt fast path in named-env mode.
pub fn evalResolvedSingleStmtNamed(
    stmt: &crate::aver_generated::domain::ast::Stmt,
    calleeEnv: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match stmt.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::eval::core::evalExpr(&expr, calleeEnv, fns)
        }
        _ => crate::aver_generated::domain::eval::core::evalStmts(
            aver_rt::AverList::from_vec(vec![stmt.clone()]),
            calleeEnv.clone(),
            fns.clone(),
        ),
    }
}

/// Execute named-env leaves directly when they are pure constants; otherwise fall back.
pub fn runFastLeafNamed(
    leaf: &crate::aver_generated::domain::ast::FastLeaf,
    body: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    calleeEnv: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
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
        _ => crate::aver_generated::domain::eval::core::evalResolvedSingleExprNamed(
            body, calleeEnv, fns,
        ),
    }
}

/// Forward a direct call by reading already-resolved slot arguments without evaluating an AST arg list.
pub fn fastForwardCall(
    calleeEnv: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fnId: aver_rt::AverInt,
    slotArgs: &aver_rt::AverIntList,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let args = crate::aver_generated::domain::eval::core::collectFastForwardArgs__collected(
        slotArgs.clone(),
        calleeEnv.clone(),
        aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
    )?;
    let fd = crate::aver_generated::domain::eval::store::lookupFnById(fns, fnId.clone())?;
    crate::aver_generated::domain::eval::core::callResolvedById(fnId, &fd, &args, fns)
}

/// Collect forwarded slot arguments in call order.
#[inline(always)]
pub fn collectFastForwardArgs(
    mut slotArgs: aver_rt::AverIntList,
    calleeEnv: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let calleeEnv = std::sync::Arc::new(calleeEnv);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(slotArgs, [] => { return Ok(acc.reverse()); }, [slot, rest] => { match crate::aver_generated::domain::eval::slots::lookupSlot(&*calleeEnv, slot) { Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(v, &acc);
            slotArgs = __tco0;
            acc = __tco2;
            continue;
        } }, Err(e @ _) => { return Err(e); } } })
    }
}

/// Evaluate a function call.
pub fn evalCall(
    name: AverStr,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalArgs(argExprs, env, fns) {
        Err(e @ _) => Err(e),
        Ok(args @ _) => crate::aver_generated::domain::eval::core::callWithArgs(&args, name, fns),
    }
}

/// Call a pre-resolved function directly using the function store.
pub fn evalCallDirect(
    fnId: aver_rt::AverInt,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fd = crate::aver_generated::domain::eval::store::lookupFnById(fns, fnId.clone())?;
    if (fd.slotCount > aver_rt::AverInt::from_i64(0)) {
        crate::aver_generated::domain::eval::core::evalCallDirectMapToSlot(
            fnId, &fd, argExprs, env, fns,
        )
    } else {
        crate::aver_generated::domain::eval::core::evalCallDirectMapToNamed(&fd, argExprs, env, fns)
    }
}

/// Call resolved function by evaluating args directly into the callee slot env.
pub fn evalCallDirectMapToSlot(
    fnId: aver_rt::AverInt,
    fd: &crate::aver_generated::domain::ast::FnDef,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv = crate::aver_generated::domain::eval::core::evalArgsMapToSlotEnv(
        argExprs.clone(),
        env.clone(),
        fns.clone(),
        match (fd.slotCount).to_u32() {
            Some(__n) => Ok(aver_rt::AverVector::new(
                __n as usize,
                crate::aver_generated::domain::value::Val::ValUnit,
            )),
            None => Err(aver_rt::AverStr::from(
                "Vector.new: size must be between 0 and 4294967295",
            )),
        }?,
        aver_rt::AverInt::from_i64(0),
    )?;
    crate::aver_generated::domain::eval::core::evalResolvedSlotFn(fnId, fd, &calleeEnv, fns)
}

/// Call resolved function by evaluating args directly into the callee named env.
pub fn evalCallDirectMapToNamed(
    fd: &crate::aver_generated::domain::ast::FnDef,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv = crate::aver_generated::domain::eval::core::evalArgsMapToNamedEnv(
        argExprs,
        &fd.params,
        env,
        fns,
        &HashMap::new(),
    )?;
    crate::aver_generated::domain::eval::core::evalResolvedNamedFn(fd, &calleeEnv, fns)
}

/// Call a builtin directly, skipping fns lookup.
pub fn evalCallBuiltin(
    name: AverStr,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalArgs(argExprs, env, fns) {
        Err(e @ _) => Err(e),
        Ok(args @ _) => crate::aver_generated::domain::builtins::callBuiltin(name, &args),
    }
}

/// Evaluate a statement expression.
pub fn evalStmtExpr(
    e: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<
    (
        crate::aver_generated::domain::value::Val,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(e, env, fns) {
        Ok(v @ _) => Ok((v, env.clone())),
        Err(msg @ _) => Err(msg),
    }
}

/// Evaluate a binding statement.
pub fn evalStmtBind(
    name: AverStr,
    e: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<
    (
        crate::aver_generated::domain::value::Val,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(e, env, fns) {
        Ok(v @ _) => Ok((v.clone(), env.clone().insert_owned(name, v))),
        Err(msg @ _) => Err(msg),
    }
}

/// Evaluate a statement, returning the result and updated environment.
pub fn evalStmt(
    stmt: &crate::aver_generated::domain::ast::Stmt,
    env: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<
    (
        crate::aver_generated::domain::value::Val,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    match stmt.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(e) => {
            crate::aver_generated::domain::eval::core::evalStmtExpr(&e, env, fns)
        }
        crate::aver_generated::domain::ast::Stmt::StmtBind(name, e) => {
            crate::aver_generated::domain::eval::core::evalStmtBind(name, &e, env, fns)
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(_, e) => {
            crate::aver_generated::domain::eval::core::evalStmtExpr(&e, env, fns)
        }
    }
}

/// Evaluate statements, threading env. Self-recursive for codegen TCO.
pub fn evalStmts(
    mut stmts: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut env: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    let fns = std::sync::Arc::new(fns);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return Ok(crate::aver_generated::domain::value::Val::ValUnit); }, [s, rest] => { match crate::aver_generated::domain::eval::core::evalStmt(&s, &env, &*fns) { Err(e @ _) => { return Err(e); }, Ok(pair @ _) => { { let (v, newEnv) = pair; { let __list_subject = rest.clone(); if __list_subject.is_empty() { return Ok(v); } else { {
            let __tco0 = rest;
            let __tco1 = newEnv;
            stmts = __tco0;
            env = __tco1;
            continue;
        } } } } } } })
    }
}

/// Evaluate top-level statements in source order and keep the resulting env, so top-level bindings stay visible to functions. Each statement sees the bindings before it, both directly and through any function it calls.
pub fn evalTopLevelStmts(
    mut stmts: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut env: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    mut last: crate::aver_generated::domain::value::Val,
    fns: crate::aver_generated::domain::eval::store::FnStore,
) -> Result<
    (
        crate::aver_generated::domain::value::Val,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    AverStr,
> {
    let fns = std::sync::Arc::new(fns);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return Ok((last, env)); }, [s, rest] => { match crate::aver_generated::domain::eval::core::evalStmt(&s, &env, &crate::aver_generated::domain::eval::store::withGlobals(&*fns, &env)) { Err(e @ _) => { return Err(e); }, Ok(pair @ _) => { { let (v, newEnv) = pair; {
            let __tco0 = rest;
            let __tco1 = newEnv;
            let __tco2 = v;
            stmts = __tco0;
            env = __tco1;
            last = __tco2;
            continue;
        } } } } })
    }
}

/// Evaluate a complete program: run top-level stmts, then call main() if it exists.
pub fn evalProgram(
    prog: &crate::aver_generated::domain::ast::Program,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fnsStore = crate::aver_generated::domain::eval::store::fnsToStore(&prog.fns);
    let top = crate::aver_generated::domain::eval::core::evalTopLevelStmts(
        prog.stmts.clone(),
        HashMap::new(),
        crate::aver_generated::domain::value::Val::ValUnit,
        fnsStore.clone(),
    )?;
    {
        let (v, globals) = top;
        crate::aver_generated::domain::eval::core::maybeCallMain(
            &crate::aver_generated::domain::eval::store::withGlobals(&fnsStore, &globals),
            &v,
        )
    }
}

/// Evaluate a program with additional functions from loaded modules.
pub fn evalProgramWithFns(
    prog: &crate::aver_generated::domain::ast::Program,
    extraFns: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let allFns = crate::aver_generated::domain::eval::store::fnsToStore(
        &aver_rt::AverList::concat(&extraFns.clone(), &prog.fns.clone()),
    );
    let top = crate::aver_generated::domain::eval::core::evalTopLevelStmts(
        prog.stmts.clone(),
        HashMap::new(),
        crate::aver_generated::domain::value::Val::ValUnit,
        allFns.clone(),
    )?;
    {
        let (v, globals) = top;
        crate::aver_generated::domain::eval::core::maybeCallMain(
            &crate::aver_generated::domain::eval::store::withGlobals(&allFns, &globals),
            &v,
        )
    }
}

/// If a main() function exists, call it. Otherwise return fallback value.
pub fn maybeCallMain(
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    fallback: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::store::lookupFnOption(fns, AverStr::from("main")) {
        Some(fd @ _) => crate::aver_generated::domain::eval::core::callResolved(
            &fd,
            &aver_rt::AverList::empty(),
            fns,
        ),
        None => Ok(fallback.clone()),
    }
}

/// Recognize immediate slot and literal expressions in slot mode.
pub fn evalImmediateSlotExpr(
    expr: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Option<Result<crate::aver_generated::domain::value::Val, AverStr>> {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => Some(
            crate::aver_generated::domain::eval::slots::lookupSlot(env, slot),
        ),
        crate::aver_generated::domain::ast::Expr::ExprInt(n) => {
            Some(Ok(crate::aver_generated::domain::value::Val::ValInt(n)))
        }
        crate::aver_generated::domain::ast::Expr::ExprFloat(f) => {
            Some(Ok(crate::aver_generated::domain::value::Val::ValFloat(f)))
        }
        crate::aver_generated::domain::ast::Expr::ExprStr(s) => {
            Some(Ok(crate::aver_generated::domain::value::Val::ValStr(s)))
        }
        crate::aver_generated::domain::ast::Expr::ExprBool(b) => {
            Some(Ok(crate::aver_generated::domain::value::Val::ValBool(b)))
        }
        _ => None,
    }
}

/// Resolve unresolved var in slot path: builtins, named function refs, and constructors.
#[inline(always)]
pub fn evalVarSlot(
    name: AverStr,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Unit" {
            Ok(crate::aver_generated::domain::value::Val::ValUnit)
        } else {
            if &*__dispatch_subject == "Option.None" {
                Ok(crate::aver_generated::domain::value::Val::ValNone)
            } else {
                crate::aver_generated::domain::eval::common::evalVarFallback(name, fns)
            }
        }
    }
}

/// Evaluate a specialized slot-vs-int arithmetic expression.
pub fn evalBinopSlotInt(
    op: &crate::aver_generated::domain::ast::BinOp,
    slot: aver_rt::AverInt,
    rhs: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let left = crate::aver_generated::domain::eval::slots::lookupSlot(env, slot)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(
        &left,
        &crate::aver_generated::domain::value::Val::ValInt(rhs),
        op,
    )
}

/// Evaluate a specialized slot-vs-slot arithmetic expression.
pub fn evalBinopSlots(
    op: &crate::aver_generated::domain::ast::BinOp,
    lhs: aver_rt::AverInt,
    rhs: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let left = crate::aver_generated::domain::eval::slots::lookupSlot(env, lhs)?;
    let right = crate::aver_generated::domain::eval::slots::lookupSlot(env, rhs)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(&left, &right, op)
}

/// Evaluate a specialized slot-vs-int comparison.
pub fn evalCmpSlotInt(
    op: &crate::aver_generated::domain::ast::CmpOp,
    slot: aver_rt::AverInt,
    rhs: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let left = crate::aver_generated::domain::eval::slots::lookupSlot(env, slot)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(
        &left,
        &crate::aver_generated::domain::value::Val::ValInt(rhs),
        op,
    )
}

/// Evaluate a specialized slot-vs-slot comparison.
pub fn evalCmpSlots(
    op: &crate::aver_generated::domain::ast::CmpOp,
    lhs: aver_rt::AverInt,
    rhs: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let left = crate::aver_generated::domain::eval::slots::lookupSlot(env, lhs)?;
    let right = crate::aver_generated::domain::eval::slots::lookupSlot(env, rhs)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(&left, &right, op)
}

/// Evaluate a fused Vector.get + Option.withDefault in slot path.
pub fn evalVectorGetOrIntExprSlot(
    vecExpr: &crate::aver_generated::domain::ast::Expr,
    idxExpr: &crate::aver_generated::domain::ast::Expr,
    defaultValue: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV = crate::aver_generated::domain::eval::core::evalExprSlot(vecExpr, env, slotMap, fns)?;
    let idxV = crate::aver_generated::domain::eval::core::evalExprSlot(idxExpr, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalVectorGetOrIntVals(&vecV, &idxV, defaultValue)
}

/// Evaluate a fused Int.mod + Result.withDefault in slot path.
pub fn evalIntModOrIntExprSlot(
    a: &crate::aver_generated::domain::ast::Expr,
    b: &crate::aver_generated::domain::ast::Expr,
    defaultValue: aver_rt::AverInt,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let aV = crate::aver_generated::domain::eval::core::evalExprSlot(a, env, slotMap, fns)?;
    let bV = crate::aver_generated::domain::eval::core::evalExprSlot(b, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalIntModOrIntVals(&aV, &bV, defaultValue)
}

/// Binary op in slot path.
pub fn evalBinopSlot(
    a: &crate::aver_generated::domain::ast::Expr,
    b: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    op: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let va = crate::aver_generated::domain::eval::core::evalExprSlot(a, env, slotMap, fns)?;
    let vb = crate::aver_generated::domain::eval::core::evalExprSlot(b, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(&va, &vb, op)
}

/// Unary minus in slot path.
pub fn evalNegSlot(
    inner: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::eval::core::evalExprSlot(inner, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalNegVals(&v)
}

/// Comparison in slot path.
pub fn evalCmpSlot(
    a: &crate::aver_generated::domain::ast::Expr,
    b: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
    op: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let va = crate::aver_generated::domain::eval::core::evalExprSlot(a, env, slotMap, fns)?;
    let vb = crate::aver_generated::domain::eval::core::evalExprSlot(b, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(&va, &vb, op)
}

/// String interpolation in slot path.
pub fn evalConcatSlot(
    parts: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalConcatPartsSlot(
        parts.clone(),
        env.clone(),
        slotMap.clone(),
        fns.clone(),
        AverStr::from(""),
    )
}

/// Concatenate interpolation parts in slot path.
pub fn evalConcatPartsSlot(
    mut parts: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: crate::aver_generated::domain::eval::store::FnStore,
    mut acc: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    let env = std::sync::Arc::new(env);
    let fns = std::sync::Arc::new(fns);
    let slotMap = std::sync::Arc::new(slotMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(parts, [] => { return Ok(crate::aver_generated::domain::value::Val::ValStr(acc)); }, [p, rest] => { match crate::aver_generated::domain::eval::core::evalExprSlot(&p, &*env, &*slotMap, &*fns) { Err(e @ _) => { return Err(e); }, Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco4 = (acc + &crate::aver_generated::domain::value::valRepr(&v));
            parts = __tco0;
            acc = __tco4;
            continue;
        } } } })
    }
}

/// Tuple literal in slot path.
pub fn evalTupleSlot(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items =
        crate::aver_generated::domain::eval::core::evalListItemsSlot(exprs, env, slotMap, fns)?;
    Ok(crate::aver_generated::domain::value::Val::ValTuple(items))
}

/// List literal in slot path.
pub fn evalListSlot(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items =
        crate::aver_generated::domain::eval::core::evalListItemsSlot(exprs, env, slotMap, fns)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(items))
}

/// Evaluate list of expressions in slot path.
pub fn evalListItemsSlot(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalListItemsSlotRev(
        exprs.clone(),
        env.clone(),
        slotMap.clone(),
        fns.clone(),
        aver_rt::AverList::empty(),
    )
}

/// Tail-recursive worker for evalListItemsSlot. Accumulates in reverse.
pub fn evalListItemsSlotRev(
    mut exprs: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: crate::aver_generated::domain::eval::store::FnStore,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let fns = std::sync::Arc::new(fns);
    let env = std::sync::Arc::new(env);
    let slotMap = std::sync::Arc::new(slotMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return Ok(acc.reverse()); }, [e, rest] => { match crate::aver_generated::domain::eval::core::evalExprSlot(&e, &*env, &*slotMap, &*fns) { Err(err @ _) => { return Err(err); }, Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco4 = aver_rt::AverList::prepend(v, &acc);
            exprs = __tco0;
            acc = __tco4;
            continue;
        } } } })
    }
}

/// Record constructor in slot path.
pub fn evalRecordSlot(
    name: AverStr,
    fieldExprs: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fields = crate::aver_generated::domain::eval::core::evalRecordFieldsSlot(
        fieldExprs, env, slotMap, fns,
    )?;
    Ok(crate::aver_generated::domain::value::Val::ValRecord(
        name, fields,
    ))
}

/// Evaluate record fields in slot path.
pub fn evalRecordFieldsSlot(
    fieldExprs: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(fieldExprs.clone(), [] => Ok(aver_rt::AverList::empty()), [pair, rest] => { { let (fname, expr) = pair; crate::aver_generated::domain::eval::core::evalRecordFieldOneSlot(fname, &expr, &rest, env, slotMap, fns) } })
}

/// Evaluate one record field in slot path.
pub fn evalRecordFieldOneSlot(
    fname: AverStr,
    expr: &crate::aver_generated::domain::ast::Expr,
    rest: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::eval::core::evalExprSlot(expr, env, slotMap, fns)?;
    let restFields =
        crate::aver_generated::domain::eval::core::evalRecordFieldsSlot(rest, env, slotMap, fns)?;
    Ok(aver_rt::AverList::prepend((fname, v), &restFields))
}

/// Field access in slot path.
pub fn evalFieldAccessSlot(
    obj: &crate::aver_generated::domain::ast::Expr,
    field: AverStr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::eval::core::evalExprSlot(obj, env, slotMap, fns)?;
    match v {
        crate::aver_generated::domain::value::Val::ValRecord(_, fields) => {
            crate::aver_generated::domain::eval::common::lookupField(fields, field)
        }
        _ => Err(AverStr::from("field access on non-record")),
    }
}

/// Function call in slot path.
pub fn evalCallSlot(
    name: AverStr,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let args =
        crate::aver_generated::domain::eval::core::evalArgsSlot(argExprs, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::core::callWithArgs(&args, name, fns)
}

/// Call a pre-resolved function directly from slot mode using the function store.
pub fn evalCallDirectSlot(
    fnId: aver_rt::AverInt,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fd = crate::aver_generated::domain::eval::store::lookupFnById(fns, fnId.clone())?;
    if (fd.slotCount > aver_rt::AverInt::from_i64(0)) {
        crate::aver_generated::domain::eval::core::evalCallDirectSlotToSlot(
            fnId, &fd, argExprs, env, slotMap, fns,
        )
    } else {
        crate::aver_generated::domain::eval::core::evalCallDirectSlotToNamed(
            &fd, argExprs, env, slotMap, fns,
        )
    }
}

/// Call resolved function by evaluating args directly into a callee slot env from slot caller state.
pub fn evalCallDirectSlotToSlot(
    fnId: aver_rt::AverInt,
    fd: &crate::aver_generated::domain::ast::FnDef,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv = crate::aver_generated::domain::eval::core::evalArgsSlotToSlotEnv(
        argExprs.clone(),
        env.clone(),
        slotMap.clone(),
        fns.clone(),
        match (fd.slotCount).to_u32() {
            Some(__n) => Ok(aver_rt::AverVector::new(
                __n as usize,
                crate::aver_generated::domain::value::Val::ValUnit,
            )),
            None => Err(aver_rt::AverStr::from(
                "Vector.new: size must be between 0 and 4294967295",
            )),
        }?,
        aver_rt::AverInt::from_i64(0),
    )?;
    crate::aver_generated::domain::eval::core::evalResolvedSlotFn(fnId, fd, &calleeEnv, fns)
}

/// Call resolved function by evaluating args directly into a named env from slot caller state.
pub fn evalCallDirectSlotToNamed(
    fd: &crate::aver_generated::domain::ast::FnDef,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv = crate::aver_generated::domain::eval::core::evalArgsSlotToNamedEnv(
        argExprs,
        &fd.params,
        env,
        slotMap,
        fns,
        &HashMap::new(),
    )?;
    crate::aver_generated::domain::eval::core::evalResolvedNamedFn(fd, &calleeEnv, fns)
}

/// Call a builtin directly, skipping fns lookup (slot-based path).
pub fn evalCallBuiltinSlot(
    name: AverStr,
    argExprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let args =
        crate::aver_generated::domain::eval::core::evalArgsSlot(argExprs, env, slotMap, fns)?;
    crate::aver_generated::domain::builtins::callBuiltin(name, &args)
}

/// Evaluate argument expressions in slot path.
pub fn evalArgsSlot(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(exprs.clone(), [] => Ok(aver_rt::AverList::empty()), [e0, rest] => { aver_list_match!(rest, [] => crate::aver_generated::domain::eval::core::evalArgsSlot1(&e0, env, slotMap, fns), [e1, rest2] => { aver_list_match!(rest2, [] => crate::aver_generated::domain::eval::core::evalArgsSlot2(&e0, &e1, env, slotMap, fns), [e2, rest3] => { { let __list_subject = rest3; if __list_subject.is_empty() { crate::aver_generated::domain::eval::core::evalArgsSlot3(&e0, &e1, &e2, env, slotMap, fns) } else { crate::aver_generated::domain::eval::core::evalArgsSlotRev(exprs.clone(), env.clone(), slotMap.clone(), fns.clone(), aver_rt::AverList::empty()) } } }) }) })
}

/// Fast path for one slot-path argument.
pub fn evalArgsSlot1(
    e0: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    let v0 = crate::aver_generated::domain::eval::core::evalExprSlot(e0, env, slotMap, fns)?;
    Ok(aver_rt::AverList::from_vec(vec![v0]))
}

/// Fast path for two slot-path arguments.
pub fn evalArgsSlot2(
    e0: &crate::aver_generated::domain::ast::Expr,
    e1: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    let v0 = crate::aver_generated::domain::eval::core::evalExprSlot(e0, env, slotMap, fns)?;
    let v1 = crate::aver_generated::domain::eval::core::evalExprSlot(e1, env, slotMap, fns)?;
    Ok(aver_rt::AverList::from_vec(vec![v0, v1]))
}

/// Fast path for three slot-path arguments.
pub fn evalArgsSlot3(
    e0: &crate::aver_generated::domain::ast::Expr,
    e1: &crate::aver_generated::domain::ast::Expr,
    e2: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    let v0 = crate::aver_generated::domain::eval::core::evalExprSlot(e0, env, slotMap, fns)?;
    let v1 = crate::aver_generated::domain::eval::core::evalExprSlot(e1, env, slotMap, fns)?;
    let v2 = crate::aver_generated::domain::eval::core::evalExprSlot(e2, env, slotMap, fns)?;
    Ok(aver_rt::AverList::from_vec(vec![v0, v1, v2]))
}

/// Tail-recursive worker for evalArgsSlot. Accumulates in reverse.
pub fn evalArgsSlotRev(
    mut exprs: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: crate::aver_generated::domain::eval::store::FnStore,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let env = std::sync::Arc::new(env);
    let fns = std::sync::Arc::new(fns);
    let slotMap = std::sync::Arc::new(slotMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return Ok(acc.reverse()); }, [e, rest] => { match crate::aver_generated::domain::eval::core::evalExprSlot(&e, &*env, &*slotMap, &*fns) { Err(err @ _) => { return Err(err); }, Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco4 = aver_rt::AverList::prepend(v, &acc);
            exprs = __tco0;
            acc = __tco4;
            continue;
        } } } })
    }
}

/// Evaluate arg expressions directly into a callee slot env from map caller state.
pub fn evalArgsMapToSlotEnv(
    mut exprs: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns: crate::aver_generated::domain::eval::store::FnStore,
    mut acc: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mut idx: aver_rt::AverInt,
) -> Result<aver_rt::AverVector<crate::aver_generated::domain::value::Val>, AverStr> {
    let env = std::sync::Arc::new(env);
    let fns = std::sync::Arc::new(fns);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return Ok(acc); }, [e, rest] => { match crate::aver_generated::domain::eval::core::evalExpr(&e, &*env, &*fns) { Err(err @ _) => { return Err(err); }, Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco3 = crate::aver_generated::domain::eval::slots::setSlot(&acc, idx.clone(), &v);
            let __tco4 = idx.add(&aver_rt::AverInt::from_i64(1));
            exprs = __tco0;
            acc = __tco3;
            idx = __tco4;
            continue;
        } } } })
    }
}

/// Evaluate arg expressions directly into a callee slot env from slot caller state.
pub fn evalArgsSlotToSlotEnv(
    mut exprs: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: crate::aver_generated::domain::eval::store::FnStore,
    mut acc: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mut idx: aver_rt::AverInt,
) -> Result<aver_rt::AverVector<crate::aver_generated::domain::value::Val>, AverStr> {
    let fns = std::sync::Arc::new(fns);
    let env = std::sync::Arc::new(env);
    let slotMap = std::sync::Arc::new(slotMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return Ok(acc); }, [e, rest] => { match crate::aver_generated::domain::eval::core::evalExprSlot(&e, &*env, &*slotMap, &*fns) { Err(err @ _) => { return Err(err); }, Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco4 = crate::aver_generated::domain::eval::slots::setSlot(&acc, idx.clone(), &v);
            let __tco5 = idx.add(&aver_rt::AverInt::from_i64(1));
            exprs = __tco0;
            acc = __tco4;
            idx = __tco5;
            continue;
        } } } })
    }
}

/// Evaluate ? operator in slot path.
pub fn evalPropagateSlot(
    inner: &crate::aver_generated::domain::ast::Expr,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::eval::core::evalExprSlot(inner, env, slotMap, fns)?;
    match v {
        crate::aver_generated::domain::value::Val::ValOk(x) => {
            let x = (*x).clone();
            Ok(x)
        }
        crate::aver_generated::domain::value::Val::ValErr(e) => {
            let e = (*e).clone();
            match e {
                crate::aver_generated::domain::value::Val::ValStr(msg) => {
                    Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(msg))
                }
                _ => Err(
                    crate::aver_generated::domain::eval::common::wrapPropagatedError(
                        AverStr::from("propagated error"),
                    ),
                ),
            }
        }
        _ => Err(AverStr::from("? operator requires Result value")),
    }
}

/// Evaluate independent product in slot path using the self-host's own divide-and-conquer ?!, then unwrap guest Results if needed.
pub fn evalIndependentProductSlot(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    unwrap: bool,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items = crate::aver_generated::domain::eval::core::evalIndependentItemsSlot(
        exprs, env, slotMap, fns,
    )?;
    if unwrap {
        crate::aver_generated::domain::eval::core::unwrapProductResults__collected(
            items,
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
        )
    } else {
        Ok(crate::aver_generated::domain::value::Val::ValTuple(items))
    }
}

/// Evaluate guest independent-product branches in slot path through a balanced tree of the self-host's own ?!.
pub fn evalIndependentItemsSlot(
    exprs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(exprs.clone(), [] => Ok(aver_rt::AverList::empty()), [a, rest] => { aver_list_match!(rest, [] => match crate::aver_generated::domain::eval::core::evalExprSlot(&a, env, slotMap, fns) { Ok(va @ _) => { Ok(aver_rt::AverList::from_vec(vec![va])) }, Err(e @ _) => { Err(e) } }, [b, rest2] => { { let __list_subject = rest2; if __list_subject.is_empty() { { let (va, vb) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let _r0 = crate::aver_generated::domain::eval::core::evalExprSlot(&a, env, slotMap, fns); crate::aver_replay::set_effect_branch(1); let _r1 = crate::aver_generated::domain::eval::core::evalExprSlot(&b, env, slotMap, fns); crate::aver_replay::exit_effect_group(); match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|_s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let _h0 = { let env = env; let slotMap = slotMap; let fns = fns; let a = a.clone(); _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalExprSlot(&a, env, slotMap, fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let _h1 = { let env = env; let slotMap = slotMap; let fns = fns; let b = b.clone(); _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalExprSlot(&b, env, slotMap, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let _b0 = _h0.join().unwrap(); let _b1 = _h1.join().unwrap(); match (_b0, _b1) { (crate::ParallelBranch::Completed(_r0), crate::ParallelBranch::Completed(_r1)) => match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (_b0, _b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = _b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = _b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok(aver_rt::AverList::from_vec(vec![va, vb])) } } else { { let (leftExprs, rightExprs) = crate::aver_generated::domain::eval::core::splitIndependentExprs(exprs.clone(), aver_rt::AverList::empty(), aver_rt::AverList::empty(), true); { let (leftVals, rightVals) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let _r0 = crate::aver_generated::domain::eval::core::evalIndependentItemsSlot(&leftExprs, env, slotMap, fns); crate::aver_replay::set_effect_branch(1); let _r1 = crate::aver_generated::domain::eval::core::evalIndependentItemsSlot(&rightExprs, env, slotMap, fns); crate::aver_replay::exit_effect_group(); match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|_s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let _h0 = { let env = env; let slotMap = slotMap; let fns = fns; let leftExprs = leftExprs.clone(); _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalIndependentItemsSlot(&leftExprs, env, slotMap, fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let _h1 = { let env = env; let slotMap = slotMap; let fns = fns; let rightExprs = rightExprs.clone(); _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalIndependentItemsSlot(&rightExprs, env, slotMap, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let _b0 = _h0.join().unwrap(); let _b1 = _h1.join().unwrap(); match (_b0, _b1) { (crate::ParallelBranch::Completed(_r0), crate::ParallelBranch::Completed(_r1)) => match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (_b0, _b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = _b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = _b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok(crate::aver_generated::domain::eval::core::interleaveIndependentVals(leftVals, rightVals, true, aver_rt::AverList::empty())) } } } } }) })
}

/// Synthesized collecting variant of `unwrapProductResults`. Appends to a builder where `unwrapProductResults` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn unwrapProductResults__collected(
    mut items: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(items, [] => { return Ok(crate::aver_generated::domain::value::Val::ValTuple(aver_rt::list_builder_finalize(acc))); }, [v, rest] => { match v {
        crate::aver_generated::domain::value::Val::ValOk(x) => {
            let x = (*x).clone();
            {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, x);
            items = __tco0;
            acc = __tco1;
            continue;
        }
        },
        crate::aver_generated::domain::value::Val::ValErr(e) => {
            let e = (*e).clone();
            match e {
        crate::aver_generated::domain::value::Val::ValStr(msg) => {
            return Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(msg));
        },
        _ => {
            return Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(AverStr::from("propagated error")));
        }
    }
        },
        _ => {
            return Err(AverStr::from("?! operator requires all elements to be Result values"));
        }
    } })
    }
}

/// Synthesized collecting variant of `collectFastForwardArgs`. Appends to a builder where `collectFastForwardArgs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn collectFastForwardArgs__collected(
    mut slotArgs: aver_rt::AverIntList,
    calleeEnv: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let calleeEnv = std::sync::Arc::new(calleeEnv);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(slotArgs, [] => { return Ok(aver_rt::list_builder_finalize(acc)); }, [slot, rest] => { match crate::aver_generated::domain::eval::slots::lookupSlot(&*calleeEnv, slot) { Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, v);
            slotArgs = __tco0;
            acc = __tco2;
            continue;
        } }, Err(e @ _) => { return Err(e); } } })
    }
}
