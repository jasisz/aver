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
            serde_json::Value::String("Domain.Eval.Core.SlotTailStep".to_string()),
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
        if type_name != "Domain.Eval.Core.SlotTailStep" && type_name != "SlotTailStep" {
            return Err(format!(
                "$variant type mismatch: expected Domain.Eval.Core.SlotTailStep, got {}",
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
                <crate::aver_generated::domain::value::Val as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant SlotTailDone missing field #{}", 0))?,
                )?,
            )),
            "SlotTailRecurEnv" => Ok(SlotTailStep::SlotTailRecurEnv(<aver_rt::AverVector<
                crate::aver_generated::domain::value::Val,
            > as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant SlotTailRecurEnv missing field #{}", 0))?,
            )?)),
            _ => Err(format!(
                "unknown variant '{}' for Domain.Eval.Core.SlotTailStep",
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
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::EvalExpr(mut expr @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::core::evalImmediateNamedExpr(&expr) {
                    Some(v @ _) => return Ok(v),
                    None => __MutualTco1::EvalExprBasic(expr, env),
                }
            }
            __MutualTco1::EvalExprBasic(mut expr @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprBoolBranch(
                            __cond_fields,
                        ) => {
                            let cond = &__cond_fields.0;
                            let thenExpr = &__cond_fields.1;
                            let elseExpr = &__cond_fields.2;
                            __MutualTco1::EvalBoolBranch(
                                cond.clone(),
                                thenExpr.clone(),
                                elseExpr.clone(),
                                env,
                            )
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprVar(__name_fields) => {
                            let name = __name_fields.0.clone();
                            return crate::aver_generated::domain::eval::core::evalVar(
                                name, &env, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprSlot(_) => {
                            return Err(::aver_rt::str_lit!("ExprSlot in map-based eval path"));
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprBinopSlotInt(_) => {
                            return Err(::aver_rt::str_lit!(
                                "ExprBinopSlotInt in map-based eval path"
                            ));
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprBinopSlots(_) => {
                            return Err(::aver_rt::str_lit!(
                                "ExprBinopSlots in map-based eval path"
                            ));
                        }
                        _ => __MutualTco1::EvalExprInternal(expr, env),
                    }
                }
            }
            __MutualTco1::EvalExprInternal(mut expr @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprCmpSlotInt(_) => {
                            return Err(::aver_rt::str_lit!(
                                "ExprCmpSlotInt in map-based eval path"
                            ));
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCmpSlots(_) => {
                            return Err(::aver_rt::str_lit!("ExprCmpSlots in map-based eval path"));
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprVectorGetOrInt(
                            __vecExpr_fields,
                        ) => {
                            let vecExpr = &__vecExpr_fields.0;
                            let idxExpr = &__vecExpr_fields.1;
                            let defaultValue = __vecExpr_fields.2.clone();
                            return crate::aver_generated::domain::eval::core::evalVectorGetOrIntExpr(vecExpr.clone(), idxExpr.clone(), defaultValue, env, &*fns);
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprIntModOrInt(
                            __a_fields,
                        ) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            let defaultValue = __a_fields.2.clone();
                            return crate::aver_generated::domain::eval::core::evalIntModOrIntExpr(
                                a.clone(),
                                b.clone(),
                                defaultValue,
                                env,
                                &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprAdd(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg22 @ _ = crate::aver_generated::domain::ast::BinOp::OpAdd;
                                return crate::aver_generated::domain::eval::core::evalBinop(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*fns,
                                    &__arg22,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprSub(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg23 @ _ = crate::aver_generated::domain::ast::BinOp::OpSub;
                                return crate::aver_generated::domain::eval::core::evalBinop(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*fns,
                                    &__arg23,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprMul(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg24 @ _ = crate::aver_generated::domain::ast::BinOp::OpMul;
                                return crate::aver_generated::domain::eval::core::evalBinop(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*fns,
                                    &__arg24,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprDiv(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg25 @ _ = crate::aver_generated::domain::ast::BinOp::OpDiv;
                                return crate::aver_generated::domain::eval::core::evalBinop(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*fns,
                                    &__arg25,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprNeg(__inner_fields) => {
                            let inner = &__inner_fields.0;
                            return crate::aver_generated::domain::eval::core::evalNeg(
                                inner.clone(),
                                env,
                                &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprEq(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg26 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpEq;
                                return crate::aver_generated::domain::eval::core::evalCmp(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*fns,
                                    &__arg26,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprNeq(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg27 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpNeq;
                                return crate::aver_generated::domain::eval::core::evalCmp(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*fns,
                                    &__arg27,
                                );
                            }
                        }
                        _ => __MutualTco1::EvalExprAggregate(expr, env),
                    }
                }
            }
            __MutualTco1::EvalExprAggregate(mut expr @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg20 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpLt;
                                return crate::aver_generated::domain::eval::core::evalCmp(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*fns,
                                    &__arg20,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg21 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpGt;
                                return crate::aver_generated::domain::eval::core::evalCmp(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*fns,
                                    &__arg21,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprLte(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg22 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpLte;
                                return crate::aver_generated::domain::eval::core::evalCmp(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*fns,
                                    &__arg22,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprGte(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg23 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpGte;
                                return crate::aver_generated::domain::eval::core::evalCmp(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*fns,
                                    &__arg23,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprConcat(
                            __parts_fields,
                        ) => {
                            let parts = &__parts_fields.0;
                            return crate::aver_generated::domain::eval::core::evalConcatExpr(
                                parts, &env, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprTuple(
                            __exprs_fields,
                        ) => {
                            let exprs = &__exprs_fields.0;
                            return crate::aver_generated::domain::eval::core::evalTupleExpr(
                                exprs, &env, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprList(
                            __exprs_fields,
                        ) => {
                            let exprs = &__exprs_fields.0;
                            return crate::aver_generated::domain::eval::core::evalListExpr(
                                exprs, &env, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprRecord(
                            __name_fields,
                        ) => {
                            let name = __name_fields.0.clone();
                            let fieldExprs = &__name_fields.1;
                            return crate::aver_generated::domain::eval::core::evalRecordExpr(
                                name, fieldExprs, &env, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(
                            __obj_fields,
                        ) => {
                            let obj = &__obj_fields.0;
                            let field = __obj_fields.1.clone();
                            return crate::aver_generated::domain::eval::core::evalFieldAccess(
                                obj.clone(),
                                field,
                                env,
                                &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCall(__name_fields) => {
                            let name = __name_fields.0.clone();
                            let argExprs = &__name_fields.1;
                            return crate::aver_generated::domain::eval::core::evalCall(
                                name, argExprs, &env, &*fns,
                            );
                        }
                        _ => __MutualTco1::EvalExprCalls(expr, env),
                    }
                }
            }
            __MutualTco1::EvalExprCalls(mut expr @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(
                            __fnId_fields,
                        ) => {
                            let fnId = __fnId_fields.0.clone();
                            let argExprs = &__fnId_fields.1;
                            return crate::aver_generated::domain::eval::core::evalCallDirect(
                                fnId, argExprs, &env, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(
                            __name_fields,
                        ) => {
                            let name = __name_fields.0.clone();
                            let argExprs = &__name_fields.1;
                            __MutualTco1::EvalCallBuiltinMaybeSpecial(name, argExprs.clone(), env)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltinId(
                            __id_fields,
                        ) => {
                            let id = __id_fields.0.clone();
                            let argExprs = &__id_fields.1;
                            __MutualTco1::EvalCallBuiltinById(id, argExprs.clone(), env)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprMatch(
                            __scrutinee_fields,
                        ) => {
                            let scrutinee = &__scrutinee_fields.0;
                            let arms = &__scrutinee_fields.1;
                            __MutualTco1::EvalMatchExpr(scrutinee.clone(), arms.clone(), env)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprPropagate(
                            __inner_fields,
                        ) => {
                            let inner = &__inner_fields.0;
                            return crate::aver_generated::domain::eval::core::evalPropagate(
                                inner.clone(),
                                env,
                                &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprIndependentProduct(
                            __exprs_fields,
                        ) => {
                            let exprs = &__exprs_fields.0;
                            let unwrap = __exprs_fields.1.clone();
                            return crate::aver_generated::domain::eval::core::evalIndependentProduct(exprs, unwrap, env, &*fns);
                        }
                        _ => {
                            return Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(50)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!(
                                        "unsupported named-env expression: "
                                    ));
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
            }
            __MutualTco1::EvalBoolBranch(
                mut cond @ _,
                mut thenExpr @ _,
                mut elseExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let condV @ _ =
                    crate::aver_generated::domain::eval::core::evalExpr(cond, env.clone(), &*fns)?;
                {
                    let __t = &condV;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValBool(__flag_fields) => {
                            let flag = __flag_fields.0.clone();
                            if flag {
                                __MutualTco1::EvalExpr(thenExpr, env)
                            } else {
                                __MutualTco1::EvalExpr(elseExpr, env)
                            }
                        }
                        _ => {
                            return Err((::aver_rt::str_lit!(
                                "bool branch condition must evaluate to Bool, got: "
                            ) + &crate::aver_generated::domain::value::valRepr(
                                &condV,
                            )));
                        }
                    }
                }
            }
            __MutualTco1::EvalMatchExpr(mut scrutinee @ _, mut arms @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::core::evalExpr(
                    scrutinee,
                    env.clone(),
                    &*fns,
                ) {
                    Ok(v @ _) => __MutualTco1::EvalMatch(v, arms, env),
                    Err(e @ _) => return Err(e),
                }
            }
            __MutualTco1::EvalCallBuiltinById(mut id @ _, mut argExprs @ _, mut env @ _) => {
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
            __MutualTco1::EvalCallBuiltinMaybeSpecial(
                mut name @ _,
                mut argExprs @ _,
                mut env @ _,
            ) => {
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
            __MutualTco1::EvalOptionWithDefaultExpr(mut argExprs @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = argExprs.clone();
                    if let Some((optionExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = rest;
                            if let (Some(defaultExpr), Some(ignored)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                __MutualTco1::EvalOptionWithDefaultExprInner(
                                    optionExpr,
                                    defaultExpr.clone(),
                                    env,
                                )
                            } else {
                                return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                                    ::aver_rt::str_lit!("Option.withDefault"),
                                    &argExprs,
                                    &env,
                                    &*fns,
                                );
                            }
                        }
                    } else {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                            ::aver_rt::str_lit!("Option.withDefault"),
                            &argExprs,
                            &env,
                            &*fns,
                        );
                    }
                }
            }
            __MutualTco1::EvalOptionWithDefaultExprInner(
                mut optionExpr @ _,
                mut defaultExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                {
                    let __t = &optionExpr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(
                            __name_fields,
                        ) => {
                            let name = __name_fields.0.clone();
                            let vecArgs = __name_fields.1.clone();
                            {
                                let __dispatch_subject = name;
                                if &*__dispatch_subject == "Vector.set" {
                                    __MutualTco1::EvalVectorSetWithDefaultExpr(
                                        vecArgs,
                                        defaultExpr,
                                        env,
                                    )
                                } else {
                                    if &*__dispatch_subject == "Vector.get" {
                                        __MutualTco1::EvalVectorGetWithDefaultExpr(
                                            vecArgs,
                                            defaultExpr,
                                            env,
                                        )
                                    } else {
                                        {
                                            let __arg6 @ _ = aver_rt::AverList::from_vec(vec![
                                                optionExpr,
                                                defaultExpr,
                                            ]);
                                            return crate::aver_generated::domain::eval::core::evalCallBuiltin(::aver_rt::str_lit!("Option.withDefault"), &__arg6, &env, &*fns);
                                        }
                                    }
                                }
                            }
                        }
                        _ => {
                            let __arg7 @ _ =
                                aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]);
                            return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                                ::aver_rt::str_lit!("Option.withDefault"),
                                &__arg7,
                                &env,
                                &*fns,
                            );
                        }
                    }
                }
            }
            __MutualTco1::EvalVectorSetWithDefaultExpr(
                mut vecArgs @ _,
                mut defaultExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = vecArgs.clone();
                    if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = r1;
                            if let (Some(idxExpr), Some(r2)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                {
                                    let __list_subject = r2;
                                    if let (Some(valueExpr), Some(ignored)) =
                                        (__list_subject.first(), __list_subject.tail())
                                    {
                                        __MutualTco1::EvalVectorSetWithDefaultExprValues(
                                            vecExpr,
                                            idxExpr.clone(),
                                            valueExpr.clone(),
                                            defaultExpr,
                                            env,
                                        )
                                    } else {
                                        {
                                            let __arg10 @ _ = aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(::aver_rt::str_lit!("Vector.set"), vecArgs)), defaultExpr]);
                                            return crate::aver_generated::domain::eval::core::evalCallBuiltin(::aver_rt::str_lit!("Option.withDefault"), &__arg10, &env, &*fns);
                                        }
                                    }
                                }
                            } else {
                                {
                                    let __arg11 @ _ = aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(::aver_rt::str_lit!("Vector.set"), vecArgs)), defaultExpr]);
                                    return crate::aver_generated::domain::eval::core::evalCallBuiltin(::aver_rt::str_lit!("Option.withDefault"), &__arg11, &env, &*fns);
                                }
                            }
                        }
                    } else {
                        {
                            let __arg12 @ _ = aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                        ::aver_rt::str_lit!("Vector.set"),
                                        vecArgs,
                                    ),
                                ),
                                defaultExpr,
                            ]);
                            return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                                ::aver_rt::str_lit!("Option.withDefault"),
                                &__arg12,
                                &env,
                                &*fns,
                            );
                        }
                    }
                }
            }
            __MutualTco1::EvalVectorSetWithDefaultExprValues(
                mut vecExpr @ _,
                mut idxExpr @ _,
                mut valueExpr @ _,
                mut defaultExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let vecV @ _ = crate::aver_generated::domain::eval::core::evalExpr(
                    vecExpr,
                    env.clone(),
                    &*fns,
                )?;
                let idxV @ _ = crate::aver_generated::domain::eval::core::evalExpr(
                    idxExpr,
                    env.clone(),
                    &*fns,
                )?;
                let valueV @ _ = crate::aver_generated::domain::eval::core::evalExpr(
                    valueExpr,
                    env.clone(),
                    &*fns,
                )?;
                __MutualTco1::EvalVectorSetWithDefaultExprResult(
                    vecV,
                    idxV,
                    valueV,
                    defaultExpr,
                    env,
                )
            }
            __MutualTco1::EvalVectorSetWithDefaultExprResult(
                mut vecV @ _,
                mut idxV @ _,
                mut valueV @ _,
                mut defaultExpr @ _,
                mut env @ _,
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
            __MutualTco1::EvalVectorGetWithDefaultExpr(
                mut vecArgs @ _,
                mut defaultExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = vecArgs.clone();
                    if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = r1;
                            if let (Some(idxExpr), Some(ignored)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                __MutualTco1::EvalVectorGetWithDefaultExprValues(
                                    vecExpr,
                                    idxExpr.clone(),
                                    defaultExpr,
                                    env,
                                )
                            } else {
                                {
                                    let __arg8 @ _ = aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(::aver_rt::str_lit!("Vector.get"), vecArgs)), defaultExpr]);
                                    return crate::aver_generated::domain::eval::core::evalCallBuiltin(::aver_rt::str_lit!("Option.withDefault"), &__arg8, &env, &*fns);
                                }
                            }
                        }
                    } else {
                        {
                            let __arg9 @ _ = aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                        ::aver_rt::str_lit!("Vector.get"),
                                        vecArgs,
                                    ),
                                ),
                                defaultExpr,
                            ]);
                            return crate::aver_generated::domain::eval::core::evalCallBuiltin(
                                ::aver_rt::str_lit!("Option.withDefault"),
                                &__arg9,
                                &env,
                                &*fns,
                            );
                        }
                    }
                }
            }
            __MutualTco1::EvalVectorGetWithDefaultExprValues(
                mut vecExpr @ _,
                mut idxExpr @ _,
                mut defaultExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let vecV @ _ = crate::aver_generated::domain::eval::core::evalExpr(
                    vecExpr,
                    env.clone(),
                    &*fns,
                )?;
                let idxV @ _ = crate::aver_generated::domain::eval::core::evalExpr(
                    idxExpr,
                    env.clone(),
                    &*fns,
                )?;
                __MutualTco1::EvalVectorGetWithDefaultExprResult(vecV, idxV, defaultExpr, env)
            }
            __MutualTco1::EvalVectorGetWithDefaultExprResult(
                mut vecV @ _,
                mut idxV @ _,
                mut defaultExpr @ _,
                mut env @ _,
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
            __MutualTco1::EvalMatch(mut v @ _, mut arms @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                aver_list_match!(arms, [] => { return Err(::aver_rt::str_lit!("no matching arm")) }, [arm, rest] => { match crate::aver_generated::domain::match_mod::matchPattern(&arm.pattern, v.clone()) { Ok(bindings @ _) => { { let __arg7 @ _ = crate::aver_generated::domain::eval::store::mergeBindings(&bindings, env); __MutualTco1::EvalExpr(arm.body, __arg7) } }, Err(_) => { __MutualTco1::EvalMatch(v, rest, env) } } })
            }
        };
    }
}

/// Evaluate an expression in the given environment.
pub fn evalExpr(
    expr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalExpr(expr, env), &fns)
}

/// Continue named-env evaluation for branch, vars, and slot-only internal nodes.
pub fn evalExprBasic(
    expr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalExprBasic(expr, env), &fns)
}

/// Continue named-env expression evaluation for comparisons and arithmetic.
pub fn evalExprInternal(
    expr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalExprInternal(expr, env), &fns)
}

/// Continue named-env evaluation for aggregate expression forms.
pub fn evalExprAggregate(
    expr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalExprAggregate(expr, env), &fns)
}

/// Finish named-env evaluation for calls, matches, propagation, and products.
pub fn evalExprCalls(
    expr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalExprCalls(expr, env), &fns)
}

/// Evaluate a direct bool branch in map path.
pub fn evalBoolBranch(
    cond @ _: crate::aver_generated::domain::ast::Expr,
    thenExpr @ _: crate::aver_generated::domain::ast::Expr,
    elseExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalBoolBranch(cond, thenExpr, elseExpr, env),
        &fns,
    )
}

/// Evaluate a match expression.
pub fn evalMatchExpr(
    scrutinee @ _: crate::aver_generated::domain::ast::Expr,
    arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalMatchExpr(scrutinee, arms, env), &fns)
}

/// Builtin dispatch by integer ID — no string comparison.
pub fn evalCallBuiltinById(
    id @ _: aver_rt::AverInt,
    argExprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalCallBuiltinById(id, argExprs, env), &fns)
}

/// Builtin dispatch with fast-path peepholes for hot patterns.
pub fn evalCallBuiltinMaybeSpecial(
    name @ _: AverStr,
    argExprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalCallBuiltinMaybeSpecial(name, argExprs, env),
        &fns,
    )
}

/// Specialize hot Option.withDefault(Vector.get/Vector.set, default) patterns in map path.
pub fn evalOptionWithDefaultExpr(
    argExprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalOptionWithDefaultExpr(argExprs, env), &fns)
}

/// Dispatch specialized Option.withDefault cases in map path.
pub fn evalOptionWithDefaultExprInner(
    optionExpr @ _: crate::aver_generated::domain::ast::Expr,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalOptionWithDefaultExprInner(optionExpr, defaultExpr, env),
        &fns,
    )
}

/// Specialized Vector.set + Option.withDefault in map path.
pub fn evalVectorSetWithDefaultExpr(
    vecArgs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorSetWithDefaultExpr(vecArgs, defaultExpr, env),
        &fns,
    )
}

/// Evaluate Vector.set operands in map path and defer the default expression until needed.
pub fn evalVectorSetWithDefaultExprValues(
    vecExpr @ _: crate::aver_generated::domain::ast::Expr,
    idxExpr @ _: crate::aver_generated::domain::ast::Expr,
    valueExpr @ _: crate::aver_generated::domain::ast::Expr,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorSetWithDefaultExprValues(
            vecExpr,
            idxExpr,
            valueExpr,
            defaultExpr,
            env,
        ),
        &fns,
    )
}

/// Finish specialized Vector.set + Option.withDefault in map path with lazy default evaluation.
pub fn evalVectorSetWithDefaultExprResult(
    vecV @ _: crate::aver_generated::domain::value::Val,
    idxV @ _: crate::aver_generated::domain::value::Val,
    valueV @ _: crate::aver_generated::domain::value::Val,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorSetWithDefaultExprResult(vecV, idxV, valueV, defaultExpr, env),
        &fns,
    )
}

/// Specialized Vector.get + Option.withDefault in map path.
pub fn evalVectorGetWithDefaultExpr(
    vecArgs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorGetWithDefaultExpr(vecArgs, defaultExpr, env),
        &fns,
    )
}

/// Evaluate Vector.get operands in map path and defer the default expression until needed.
pub fn evalVectorGetWithDefaultExprValues(
    vecExpr @ _: crate::aver_generated::domain::ast::Expr,
    idxExpr @ _: crate::aver_generated::domain::ast::Expr,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorGetWithDefaultExprValues(vecExpr, idxExpr, defaultExpr, env),
        &fns,
    )
}

/// Finish specialized Vector.get + Option.withDefault in map path with lazy default evaluation.
pub fn evalVectorGetWithDefaultExprResult(
    vecV @ _: crate::aver_generated::domain::value::Val,
    idxV @ _: crate::aver_generated::domain::value::Val,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::EvalVectorGetWithDefaultExprResult(vecV, idxV, defaultExpr, env),
        &fns,
    )
}

/// Try each match arm until one matches. Self-recursive for codegen TCO.
pub fn evalMatch(
    v @ _: crate::aver_generated::domain::value::Val,
    arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalMatch(v, arms, env), &fns)
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
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::EvalTailExprSlot(
                mut selfId @ _,
                mut expr @ _,
                mut slotCount @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(
                            __fnId_fields,
                        ) => {
                            let fnId = __fnId_fields.0.clone();
                            let argExprs = &__fnId_fields.1;
                            if (fnId == selfId) {
                                {
                                    let __subject = {
                                        let __arg19 @ _ = match aver_rt::checked_vector_size(
                                            &(slotCount),
                                        ) {
                                            Some(__n) => Ok(aver_rt::AverVector::new(
                                                __n,
                                                crate::aver_generated::domain::value::Val::ValUnit,
                                            )),
                                            None => Err(aver_rt::AverStr::from(
                                                aver_rt::vector_size_error_message(),
                                            )),
                                        }?;
                                        crate::aver_generated::domain::eval::core::evalArgsSlotToSlotEnv(argExprs, &env, &*slotMap, &*fns, __arg19, aver_rt::AverInt::from_i64(0))
                                    };
                                    match __subject { Ok(nextEnv @ _) => { return Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailRecurEnv(nextEnv)) }, Err(e @ _) => { return Err(e) } }
                                }
                            } else {
                                match crate::aver_generated::domain::eval::core::evalCallDirectSlot(fnId, argExprs, &env, &*slotMap, &*fns) { Ok(v @ _) => { return Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v)) }, Err(e @ _) => { return Err(e) } }
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprBoolBranch(
                            __cond_fields,
                        ) => {
                            let cond = &__cond_fields.0;
                            let thenExpr = &__cond_fields.1;
                            let elseExpr = &__cond_fields.2;
                            __MutualTco2::EvalTailBoolBranchSlot(
                                selfId,
                                cond.clone(),
                                thenExpr.clone(),
                                elseExpr.clone(),
                                slotCount,
                                env,
                            )
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprMatch(
                            __scrutinee_fields,
                        ) => {
                            let scrutinee = &__scrutinee_fields.0;
                            let arms = &__scrutinee_fields.1;
                            __MutualTco2::EvalTailMatchExprSlot(
                                selfId,
                                scrutinee.clone(),
                                arms.clone(),
                                slotCount,
                                env,
                            )
                        }
                        _ => {
                            match crate::aver_generated::domain::eval::core::evalExprSlot(expr, env, &*slotMap, &*fns) { Ok(v @ _) => { return Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v)) }, Err(e @ _) => { return Err(e) } }
                        }
                    }
                }
            }
            __MutualTco2::EvalTailBoolBranchSlot(
                mut selfId @ _,
                mut cond @ _,
                mut thenExpr @ _,
                mut elseExpr @ _,
                mut slotCount @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let condV @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    cond,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                {
                    let __t = &condV;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValBool(__flag_fields) => {
                            let flag = __flag_fields.0.clone();
                            if flag {
                                __MutualTco2::EvalTailExprSlot(selfId, thenExpr, slotCount, env)
                            } else {
                                __MutualTco2::EvalTailExprSlot(selfId, elseExpr, slotCount, env)
                            }
                        }
                        _ => {
                            return Err((::aver_rt::str_lit!(
                                "bool branch condition must evaluate to Bool, got: "
                            ) + &crate::aver_generated::domain::value::valRepr(
                                &condV,
                            )));
                        }
                    }
                }
            }
            __MutualTco2::EvalTailMatchExprSlot(
                mut selfId @ _,
                mut scrutinee @ _,
                mut arms @ _,
                mut slotCount @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let v @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    scrutinee,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                __MutualTco2::EvalTailMatchSlot(selfId, v, arms, slotCount, env)
            }
            __MutualTco2::EvalTailMatchSlot(
                mut selfId @ _,
                mut v @ _,
                mut arms @ _,
                mut slotCount @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                aver_list_match!(arms, [] => { return Err(::aver_rt::str_lit!("no matching arm")) }, [arm, rest] => { match crate::aver_generated::domain::match_mod::matchPattern(&arm.pattern, v.clone()) { Ok(bindings @ _) => { { let __arg10 @ _ = crate::aver_generated::domain::eval::core::mergeBindingsSlot(bindings, &arm.bindingSlots, env); __MutualTco2::EvalTailExprSlot(selfId, arm.body, slotCount, __arg10) } }, Err(_) => { __MutualTco2::EvalTailMatchSlot(selfId, v, rest, slotCount, env) } } })
            }
        };
    }
}

/// Evaluate a tail-position expression in slot mode, converting self-recursive direct calls into loop re-entry.
pub fn evalTailExprSlot(
    selfId @ _: aver_rt::AverInt,
    expr @ _: crate::aver_generated::domain::ast::Expr,
    slotCount @ _: aver_rt::AverInt,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::EvalTailExprSlot(selfId, expr, slotCount, env),
        &slotMap,
        &fns,
    )
}

/// Evaluate a tail-position bool branch in slot mode.
pub fn evalTailBoolBranchSlot(
    selfId @ _: aver_rt::AverInt,
    cond @ _: crate::aver_generated::domain::ast::Expr,
    thenExpr @ _: crate::aver_generated::domain::ast::Expr,
    elseExpr @ _: crate::aver_generated::domain::ast::Expr,
    slotCount @ _: aver_rt::AverInt,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::EvalTailBoolBranchSlot(selfId, cond, thenExpr, elseExpr, slotCount, env),
        &slotMap,
        &fns,
    )
}

/// Evaluate a tail-position match expression in slot mode.
pub fn evalTailMatchExprSlot(
    selfId @ _: aver_rt::AverInt,
    scrutinee @ _: crate::aver_generated::domain::ast::Expr,
    arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    slotCount @ _: aver_rt::AverInt,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::EvalTailMatchExprSlot(selfId, scrutinee, arms, slotCount, env),
        &slotMap,
        &fns,
    )
}

/// Evaluate a tail-position match arm in slot mode, preserving binding slots.
pub fn evalTailMatchSlot(
    selfId @ _: aver_rt::AverInt,
    v @ _: crate::aver_generated::domain::value::Val,
    arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    slotCount @ _: aver_rt::AverInt,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::EvalTailMatchSlot(selfId, v, arms, slotCount, env),
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
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::EvalExprSlot(mut expr @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::core::evalImmediateSlotExpr(&expr, &env)
                {
                    Some(result @ _) => return result,
                    None => __MutualTco3::EvalExprSlotBasic(expr, env),
                }
            }
            __MutualTco3::EvalExprSlotBasic(mut expr @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprBoolBranch(
                            __cond_fields,
                        ) => {
                            let cond = &__cond_fields.0;
                            let thenExpr = &__cond_fields.1;
                            let elseExpr = &__cond_fields.2;
                            __MutualTco3::EvalBoolBranchSlot(
                                cond.clone(),
                                thenExpr.clone(),
                                elseExpr.clone(),
                                env,
                            )
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprVar(__name_fields) => {
                            let name = __name_fields.0.clone();
                            return crate::aver_generated::domain::eval::core::evalVarSlot(
                                name, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprBinopSlotInt(
                            __op_fields,
                        ) => {
                            let op = &__op_fields.0;
                            let slot = __op_fields.1.clone();
                            let rhs = __op_fields.2.clone();
                            return crate::aver_generated::domain::eval::core::evalBinopSlotInt(
                                op, slot, rhs, &env,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprBinopSlots(
                            __op_fields,
                        ) => {
                            let op = &__op_fields.0;
                            let lhs = __op_fields.1.clone();
                            let rhs = __op_fields.2.clone();
                            return crate::aver_generated::domain::eval::core::evalBinopSlots(
                                op, lhs, rhs, &env,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCmpSlotInt(
                            __op_fields,
                        ) => {
                            let op = &__op_fields.0;
                            let slot = __op_fields.1.clone();
                            let rhs = __op_fields.2.clone();
                            return crate::aver_generated::domain::eval::core::evalCmpSlotInt(
                                op, slot, rhs, &env,
                            );
                        }
                        _ => __MutualTco3::EvalExprSlotInternal(expr, env),
                    }
                }
            }
            __MutualTco3::EvalExprSlotInternal(mut expr @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprCmpSlots(
                            __op_fields,
                        ) => {
                            let op = &__op_fields.0;
                            let lhs = __op_fields.1.clone();
                            let rhs = __op_fields.2.clone();
                            return crate::aver_generated::domain::eval::core::evalCmpSlots(
                                op, lhs, rhs, &env,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprVectorGetOrInt(
                            __vecExpr_fields,
                        ) => {
                            let vecExpr = &__vecExpr_fields.0;
                            let idxExpr = &__vecExpr_fields.1;
                            let defaultValue = __vecExpr_fields.2.clone();
                            return crate::aver_generated::domain::eval::core::evalVectorGetOrIntExprSlot(vecExpr.clone(), idxExpr.clone(), defaultValue, env, &*slotMap, &*fns);
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprIntModOrInt(
                            __a_fields,
                        ) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            let defaultValue = __a_fields.2.clone();
                            return crate::aver_generated::domain::eval::core::evalIntModOrIntExprSlot(a.clone(), b.clone(), defaultValue, env, &*slotMap, &*fns);
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprAdd(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg26 @ _ = crate::aver_generated::domain::ast::BinOp::OpAdd;
                                return crate::aver_generated::domain::eval::core::evalBinopSlot(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*slotMap,
                                    &*fns,
                                    &__arg26,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprSub(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg27 @ _ = crate::aver_generated::domain::ast::BinOp::OpSub;
                                return crate::aver_generated::domain::eval::core::evalBinopSlot(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*slotMap,
                                    &*fns,
                                    &__arg27,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprMul(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg28 @ _ = crate::aver_generated::domain::ast::BinOp::OpMul;
                                return crate::aver_generated::domain::eval::core::evalBinopSlot(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*slotMap,
                                    &*fns,
                                    &__arg28,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprDiv(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg29 @ _ = crate::aver_generated::domain::ast::BinOp::OpDiv;
                                return crate::aver_generated::domain::eval::core::evalBinopSlot(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*slotMap,
                                    &*fns,
                                    &__arg29,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprNeg(__inner_fields) => {
                            let inner = &__inner_fields.0;
                            return crate::aver_generated::domain::eval::core::evalNegSlot(
                                inner.clone(),
                                env,
                                &*slotMap,
                                &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprEq(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg30 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpEq;
                                return crate::aver_generated::domain::eval::core::evalCmpSlot(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*slotMap,
                                    &*fns,
                                    &__arg30,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprNeq(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg31 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpNeq;
                                return crate::aver_generated::domain::eval::core::evalCmpSlot(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*slotMap,
                                    &*fns,
                                    &__arg31,
                                );
                            }
                        }
                        _ => __MutualTco3::EvalExprSlotAggregate(expr, env),
                    }
                }
            }
            __MutualTco3::EvalExprSlotAggregate(mut expr @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg21 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpLt;
                                return crate::aver_generated::domain::eval::core::evalCmpSlot(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*slotMap,
                                    &*fns,
                                    &__arg21,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg22 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpGt;
                                return crate::aver_generated::domain::eval::core::evalCmpSlot(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*slotMap,
                                    &*fns,
                                    &__arg22,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprLte(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg23 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpLte;
                                return crate::aver_generated::domain::eval::core::evalCmpSlot(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*slotMap,
                                    &*fns,
                                    &__arg23,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprGte(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg24 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpGte;
                                return crate::aver_generated::domain::eval::core::evalCmpSlot(
                                    a.clone(),
                                    b.clone(),
                                    env,
                                    &*slotMap,
                                    &*fns,
                                    &__arg24,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprConcat(
                            __parts_fields,
                        ) => {
                            let parts = &__parts_fields.0;
                            return crate::aver_generated::domain::eval::core::evalConcatSlot(
                                parts, &env, &*slotMap, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprTuple(
                            __exprs_fields,
                        ) => {
                            let exprs = &__exprs_fields.0;
                            return crate::aver_generated::domain::eval::core::evalTupleSlot(
                                exprs, &env, &*slotMap, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprList(
                            __exprs_fields,
                        ) => {
                            let exprs = &__exprs_fields.0;
                            return crate::aver_generated::domain::eval::core::evalListSlot(
                                exprs, &env, &*slotMap, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprRecord(
                            __name_fields,
                        ) => {
                            let name = __name_fields.0.clone();
                            let fieldExprs = &__name_fields.1;
                            return crate::aver_generated::domain::eval::core::evalRecordSlot(
                                name, fieldExprs, &env, &*slotMap, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(
                            __obj_fields,
                        ) => {
                            let obj = &__obj_fields.0;
                            let field = __obj_fields.1.clone();
                            return crate::aver_generated::domain::eval::core::evalFieldAccessSlot(
                                obj.clone(),
                                field,
                                env,
                                &*slotMap,
                                &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCall(__name_fields) => {
                            let name = __name_fields.0.clone();
                            let argExprs = &__name_fields.1;
                            return crate::aver_generated::domain::eval::core::evalCallSlot(
                                name, argExprs, env, &*slotMap, &*fns,
                            );
                        }
                        _ => __MutualTco3::EvalExprSlotCalls(expr, env),
                    }
                }
            }
            __MutualTco3::EvalExprSlotCalls(mut expr @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(
                            __fnId_fields,
                        ) => {
                            let fnId = __fnId_fields.0.clone();
                            let argExprs = &__fnId_fields.1;
                            return crate::aver_generated::domain::eval::core::evalCallDirectSlot(
                                fnId, argExprs, &env, &*slotMap, &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(
                            __name_fields,
                        ) => {
                            let name = __name_fields.0.clone();
                            let argExprs = &__name_fields.1;
                            __MutualTco3::EvalCallBuiltinSlotMaybeSpecial(
                                name,
                                argExprs.clone(),
                                env,
                            )
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltinId(
                            __id_fields,
                        ) => {
                            let id = __id_fields.0.clone();
                            let argExprs = &__id_fields.1;
                            __MutualTco3::EvalCallBuiltinByIdSlot(id, argExprs.clone(), env)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprMatch(
                            __scrutinee_fields,
                        ) => {
                            let scrutinee = &__scrutinee_fields.0;
                            let arms = &__scrutinee_fields.1;
                            __MutualTco3::EvalMatchExprSlot(scrutinee.clone(), arms.clone(), env)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprPropagate(
                            __inner_fields,
                        ) => {
                            let inner = &__inner_fields.0;
                            return crate::aver_generated::domain::eval::core::evalPropagateSlot(
                                inner.clone(),
                                env,
                                &*slotMap,
                                &*fns,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprIndependentProduct(
                            __exprs_fields,
                        ) => {
                            let exprs = &__exprs_fields.0;
                            let unwrap = __exprs_fields.1.clone();
                            return crate::aver_generated::domain::eval::core::evalIndependentProductSlot(exprs, unwrap, env, &*slotMap, &*fns);
                        }
                        _ => {
                            return Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(45)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!(
                                        "unsupported slot expression: "
                                    ));
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
            }
            __MutualTco3::EvalBoolBranchSlot(
                mut cond @ _,
                mut thenExpr @ _,
                mut elseExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let condV @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    cond,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                {
                    let __t = &condV;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValBool(__flag_fields) => {
                            let flag = __flag_fields.0.clone();
                            if flag {
                                __MutualTco3::EvalExprSlot(thenExpr, env)
                            } else {
                                __MutualTco3::EvalExprSlot(elseExpr, env)
                            }
                        }
                        _ => {
                            return Err((::aver_rt::str_lit!(
                                "bool branch condition must evaluate to Bool, got: "
                            ) + &crate::aver_generated::domain::value::valRepr(
                                &condV,
                            )));
                        }
                    }
                }
            }
            __MutualTco3::EvalCallBuiltinByIdSlot(mut id @ _, mut argExprs @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __int_match_subject = id.clone();
                    if __int_match_subject == aver_rt::AverInt::from_i64(15) {
                        __MutualTco3::EvalOptionWithDefaultExprSlot(argExprs, env)
                    } else {
                        match crate::aver_generated::domain::eval::core::evalArgsSlot(&argExprs, env, &*slotMap, &*fns) { Err(e @ _) => { return Err(e) }, Ok(args @ _) => { return crate::aver_generated::domain::builtins::callBuiltinByIdValues(id, &args) } }
                    }
                }
            }
            __MutualTco3::EvalCallBuiltinSlotMaybeSpecial(
                mut name @ _,
                mut argExprs @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                match &*name.clone() {
                    "Option.withDefault" => {
                        __MutualTco3::EvalOptionWithDefaultExprSlot(argExprs, env)
                    }
                    _ => {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(
                            name, &argExprs, env, &*slotMap, &*fns,
                        );
                    }
                }
            }
            __MutualTco3::EvalOptionWithDefaultExprSlot(mut argExprs @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = argExprs.clone();
                    if let Some((optionExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = rest;
                            if let (Some(defaultExpr), Some(ignored)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                __MutualTco3::EvalOptionWithDefaultExprSlotInner(
                                    optionExpr,
                                    defaultExpr.clone(),
                                    env,
                                )
                            } else {
                                return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(::aver_rt::str_lit!("Option.withDefault"), &argExprs, env, &*slotMap, &*fns);
                            }
                        }
                    } else {
                        return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(
                            ::aver_rt::str_lit!("Option.withDefault"),
                            &argExprs,
                            env,
                            &*slotMap,
                            &*fns,
                        );
                    }
                }
            }
            __MutualTco3::EvalOptionWithDefaultExprSlotInner(
                mut optionExpr @ _,
                mut defaultExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                {
                    let __t = &optionExpr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(
                            __name_fields,
                        ) => {
                            let name = __name_fields.0.clone();
                            let vecArgs = __name_fields.1.clone();
                            {
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
                                        {
                                            let __arg7 @ _ = aver_rt::AverList::from_vec(vec![
                                                optionExpr,
                                                defaultExpr,
                                            ]);
                                            return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(::aver_rt::str_lit!("Option.withDefault"), &__arg7, env, &*slotMap, &*fns);
                                        }
                                    }
                                }
                            }
                        }
                        _ => {
                            let __arg8 @ _ =
                                aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]);
                            return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(
                                ::aver_rt::str_lit!("Option.withDefault"),
                                &__arg8,
                                env,
                                &*slotMap,
                                &*fns,
                            );
                        }
                    }
                }
            }
            __MutualTco3::EvalVectorSetWithDefaultExprSlot(
                mut vecArgs @ _,
                mut defaultExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = vecArgs.clone();
                    if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = r1;
                            if let (Some(idxExpr), Some(r2)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                {
                                    let __list_subject = r2;
                                    if let (Some(valueExpr), Some(ignored)) =
                                        (__list_subject.first(), __list_subject.tail())
                                    {
                                        __MutualTco3::EvalVectorSetWithDefaultExprSlotValues(
                                            vecExpr,
                                            idxExpr.clone(),
                                            valueExpr.clone(),
                                            defaultExpr,
                                            env,
                                        )
                                    } else {
                                        {
                                            let __arg11 @ _ = aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(::aver_rt::str_lit!("Vector.set"), vecArgs)), defaultExpr]);
                                            return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(::aver_rt::str_lit!("Option.withDefault"), &__arg11, env, &*slotMap, &*fns);
                                        }
                                    }
                                }
                            } else {
                                {
                                    let __arg12 @ _ = aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(::aver_rt::str_lit!("Vector.set"), vecArgs)), defaultExpr]);
                                    return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(::aver_rt::str_lit!("Option.withDefault"), &__arg12, env, &*slotMap, &*fns);
                                }
                            }
                        }
                    } else {
                        {
                            let __arg13 @ _ = aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                        ::aver_rt::str_lit!("Vector.set"),
                                        vecArgs,
                                    ),
                                ),
                                defaultExpr,
                            ]);
                            return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(
                                ::aver_rt::str_lit!("Option.withDefault"),
                                &__arg13,
                                env,
                                &*slotMap,
                                &*fns,
                            );
                        }
                    }
                }
            }
            __MutualTco3::EvalVectorSetWithDefaultExprSlotValues(
                mut vecExpr @ _,
                mut idxExpr @ _,
                mut valueExpr @ _,
                mut defaultExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let vecV @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    vecExpr,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                let idxV @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    idxExpr,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                let valueV @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    valueExpr,
                    env.clone(),
                    &*slotMap,
                    &*fns,
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
                mut vecV @ _,
                mut idxV @ _,
                mut valueV @ _,
                mut defaultExpr @ _,
                mut env @ _,
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
                mut vecArgs @ _,
                mut defaultExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                {
                    let __list_subject = vecArgs.clone();
                    if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = r1;
                            if let (Some(idxExpr), Some(ignored)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                __MutualTco3::EvalVectorGetWithDefaultExprSlotValues(
                                    vecExpr,
                                    idxExpr.clone(),
                                    defaultExpr,
                                    env,
                                )
                            } else {
                                {
                                    let __arg9 @ _ = aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(::aver_rt::str_lit!("Vector.get"), vecArgs)), defaultExpr]);
                                    return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(::aver_rt::str_lit!("Option.withDefault"), &__arg9, env, &*slotMap, &*fns);
                                }
                            }
                        }
                    } else {
                        {
                            let __arg10 @ _ = aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                        ::aver_rt::str_lit!("Vector.get"),
                                        vecArgs,
                                    ),
                                ),
                                defaultExpr,
                            ]);
                            return crate::aver_generated::domain::eval::core::evalCallBuiltinSlot(
                                ::aver_rt::str_lit!("Option.withDefault"),
                                &__arg10,
                                env,
                                &*slotMap,
                                &*fns,
                            );
                        }
                    }
                }
            }
            __MutualTco3::EvalVectorGetWithDefaultExprSlotValues(
                mut vecExpr @ _,
                mut idxExpr @ _,
                mut defaultExpr @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let vecV @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    vecExpr,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                let idxV @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    idxExpr,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                __MutualTco3::EvalVectorGetWithDefaultExprSlotResult(vecV, idxV, defaultExpr, env)
            }
            __MutualTco3::EvalVectorGetWithDefaultExprSlotResult(
                mut vecV @ _,
                mut idxV @ _,
                mut defaultExpr @ _,
                mut env @ _,
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
            __MutualTco3::EvalMatchExprSlot(mut scrutinee @ _, mut arms @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                let v @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    scrutinee,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                __MutualTco3::EvalMatchSlot(v, arms, env)
            }
            __MutualTco3::EvalMatchSlot(mut v @ _, mut arms @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                aver_list_match!(arms, [] => { return Err(::aver_rt::str_lit!("no matching arm")) }, [arm, rest] => { match crate::aver_generated::domain::match_mod::matchPattern(&arm.pattern, v.clone()) { Ok(bindings @ _) => { { let __arg8 @ _ = crate::aver_generated::domain::eval::core::mergeBindingsSlot(bindings, &arm.bindingSlots, env); __MutualTco3::EvalExprSlot(arm.body, __arg8) } }, Err(_) => { __MutualTco3::EvalMatchSlot(v, rest, env) } } })
            }
        };
    }
}

/// Evaluate an expression using slot-based environment.
pub fn evalExprSlot(
    expr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalExprSlot(expr, env), &slotMap, &fns)
}

/// Continue slot-based evaluation for branches, vars, and slot-only nodes.
pub fn evalExprSlotBasic(
    expr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalExprSlotBasic(expr, env), &slotMap, &fns)
}

/// Continue slot-based expression evaluation for comparisons and arithmetic.
pub fn evalExprSlotInternal(
    expr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalExprSlotInternal(expr, env),
        &slotMap,
        &fns,
    )
}

/// Continue slot-based evaluation for aggregate and call expression forms.
pub fn evalExprSlotAggregate(
    expr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalExprSlotAggregate(expr, env),
        &slotMap,
        &fns,
    )
}

/// Finish slot-based evaluation for calls, matches, propagation, and products.
pub fn evalExprSlotCalls(
    expr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalExprSlotCalls(expr, env), &slotMap, &fns)
}

/// Evaluate a direct bool branch in slot path.
pub fn evalBoolBranchSlot(
    cond @ _: crate::aver_generated::domain::ast::Expr,
    thenExpr @ _: crate::aver_generated::domain::ast::Expr,
    elseExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalBoolBranchSlot(cond, thenExpr, elseExpr, env),
        &slotMap,
        &fns,
    )
}

/// Builtin dispatch by integer ID (slot-based path) — no string comparison.
pub fn evalCallBuiltinByIdSlot(
    id @ _: aver_rt::AverInt,
    argExprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalCallBuiltinByIdSlot(id, argExprs, env),
        &slotMap,
        &fns,
    )
}

/// Builtin dispatch with fast-path peepholes for hot patterns in slot path.
pub fn evalCallBuiltinSlotMaybeSpecial(
    name @ _: AverStr,
    argExprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalCallBuiltinSlotMaybeSpecial(name, argExprs, env),
        &slotMap,
        &fns,
    )
}

/// Specialize hot Option.withDefault(Vector.get/Vector.set, default) patterns in slot path.
pub fn evalOptionWithDefaultExprSlot(
    argExprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalOptionWithDefaultExprSlot(argExprs, env),
        &slotMap,
        &fns,
    )
}

/// Dispatch specialized Option.withDefault cases in slot path.
pub fn evalOptionWithDefaultExprSlotInner(
    optionExpr @ _: crate::aver_generated::domain::ast::Expr,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalOptionWithDefaultExprSlotInner(optionExpr, defaultExpr, env),
        &slotMap,
        &fns,
    )
}

/// Specialized Vector.set + Option.withDefault in slot path.
pub fn evalVectorSetWithDefaultExprSlot(
    vecArgs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorSetWithDefaultExprSlot(vecArgs, defaultExpr, env),
        &slotMap,
        &fns,
    )
}

/// Evaluate Vector.set operands in slot path and defer the default expression until needed.
pub fn evalVectorSetWithDefaultExprSlotValues(
    vecExpr @ _: crate::aver_generated::domain::ast::Expr,
    idxExpr @ _: crate::aver_generated::domain::ast::Expr,
    valueExpr @ _: crate::aver_generated::domain::ast::Expr,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorSetWithDefaultExprSlotValues(
            vecExpr,
            idxExpr,
            valueExpr,
            defaultExpr,
            env,
        ),
        &slotMap,
        &fns,
    )
}

/// Finish specialized Vector.set + Option.withDefault in slot path with lazy default evaluation.
pub fn evalVectorSetWithDefaultExprSlotResult(
    vecV @ _: crate::aver_generated::domain::value::Val,
    idxV @ _: crate::aver_generated::domain::value::Val,
    valueV @ _: crate::aver_generated::domain::value::Val,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorSetWithDefaultExprSlotResult(vecV, idxV, valueV, defaultExpr, env),
        &slotMap,
        &fns,
    )
}

/// Specialized Vector.get + Option.withDefault in slot path.
pub fn evalVectorGetWithDefaultExprSlot(
    vecArgs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorGetWithDefaultExprSlot(vecArgs, defaultExpr, env),
        &slotMap,
        &fns,
    )
}

/// Evaluate Vector.get operands in slot path and defer the default expression until needed.
pub fn evalVectorGetWithDefaultExprSlotValues(
    vecExpr @ _: crate::aver_generated::domain::ast::Expr,
    idxExpr @ _: crate::aver_generated::domain::ast::Expr,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorGetWithDefaultExprSlotValues(vecExpr, idxExpr, defaultExpr, env),
        &slotMap,
        &fns,
    )
}

/// Finish specialized Vector.get + Option.withDefault in slot path with lazy default evaluation.
pub fn evalVectorGetWithDefaultExprSlotResult(
    vecV @ _: crate::aver_generated::domain::value::Val,
    idxV @ _: crate::aver_generated::domain::value::Val,
    defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalVectorGetWithDefaultExprSlotResult(vecV, idxV, defaultExpr, env),
        &slotMap,
        &fns,
    )
}

/// Match expression in slot path.
pub fn evalMatchExprSlot(
    scrutinee @ _: crate::aver_generated::domain::ast::Expr,
    arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::EvalMatchExprSlot(scrutinee, arms, env),
        &slotMap,
        &fns,
    )
}

/// Try each match arm in slot path.
pub fn evalMatchSlot(
    v @ _: crate::aver_generated::domain::value::Val,
    arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalMatchSlot(v, arms, env), &slotMap, &fns)
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
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    loop {
        __state = match __state {
            __MutualTco4::EvalArgsMapToNamedEnv(mut exprs @ _, mut params @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                aver_list_match!(exprs, [] => { return Ok(acc) }, [e, restExprs] => { match crate::aver_generated::domain::eval::core::evalExpr(e, (*env).clone(), &*fns) { Err(err @ _) => { return Err(err) }, Ok(v @ _) => { __MutualTco4::EvalArgsMapToNamedEnvBind(restExprs, params, acc, v) } } })
            }
            __MutualTco4::EvalArgsMapToNamedEnvBind(
                mut restExprs @ _,
                mut params @ _,
                mut acc @ _,
                mut v @ _,
            ) => {
                crate::cancel_checkpoint();
                aver_list_match!(params, [] => { { let __arg9 @ _ = <aver_rt::AverList<AverStr>>::empty(); __MutualTco4::EvalArgsMapToNamedEnv(restExprs, __arg9, acc) } }, [param, restParams] => { { let __arg8 @ _ = acc.insert_owned(param, v); __MutualTco4::EvalArgsMapToNamedEnv(restExprs, restParams, __arg8) } })
            }
        };
    }
}

/// Evaluate arg expressions directly into a callee named env from map caller state.
pub fn evalArgsMapToNamedEnv(
    exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    params @ _: aver_rt::AverList<AverStr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    acc @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    __mutual_tco_trampoline_4(
        __MutualTco4::EvalArgsMapToNamedEnv(exprs, params, acc),
        &env,
        &fns,
    )
}

/// Bind one evaluated arg into the callee named env when a parameter exists.
pub fn evalArgsMapToNamedEnvBind(
    restExprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    params @ _: aver_rt::AverList<AverStr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    acc @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    v @ _: crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    __mutual_tco_trampoline_4(
        __MutualTco4::EvalArgsMapToNamedEnvBind(restExprs, params, acc, v),
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
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    loop {
        __state = match __state {
            __MutualTco5::EvalArgsSlotToNamedEnv(mut exprs @ _, mut params @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                aver_list_match!(exprs, [] => { return Ok(acc) }, [e, restExprs] => { match crate::aver_generated::domain::eval::core::evalExprSlot(e, (*env).clone(), &*slotMap, &*fns) { Err(err @ _) => { return Err(err) }, Ok(v @ _) => { __MutualTco5::EvalArgsSlotToNamedEnvBind(restExprs, params, acc, v) } } })
            }
            __MutualTco5::EvalArgsSlotToNamedEnvBind(
                mut restExprs @ _,
                mut params @ _,
                mut acc @ _,
                mut v @ _,
            ) => {
                crate::cancel_checkpoint();
                aver_list_match!(params, [] => { { let __arg10 @ _ = <aver_rt::AverList<AverStr>>::empty(); __MutualTco5::EvalArgsSlotToNamedEnv(restExprs, __arg10, acc) } }, [param, restParams] => { { let __arg9 @ _ = acc.insert_owned(param, v); __MutualTco5::EvalArgsSlotToNamedEnv(restExprs, restParams, __arg9) } })
            }
        };
    }
}

/// Evaluate arg expressions directly into a callee named env from slot caller state.
pub fn evalArgsSlotToNamedEnv(
    exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    params @ _: aver_rt::AverList<AverStr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    acc @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    __mutual_tco_trampoline_5(
        __MutualTco5::EvalArgsSlotToNamedEnv(exprs, params, acc),
        &env,
        &slotMap,
        &fns,
    )
}

/// Bind one evaluated arg into the callee named env when a parameter exists.
pub fn evalArgsSlotToNamedEnvBind(
    restExprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    params @ _: aver_rt::AverList<AverStr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    acc @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    v @ _: crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>, AverStr> {
    __mutual_tco_trampoline_5(
        __MutualTco5::EvalArgsSlotToNamedEnvBind(restExprs, params, acc, v),
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
    bindingSlots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> aver_rt::AverVector<crate::aver_generated::domain::value::Val> {
    loop {
        __state = match __state {
            __MutualTco6::MergeBindingsSlot(mut bindings @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                aver_list_match!(bindings, [] => { return env }, [pair, rest] => { { let (name, val) = pair; __MutualTco6::MergeOneBindingSlot(name, val, rest, env) } })
            }
            __MutualTco6::MergeOneBindingSlot(
                mut name @ _,
                mut val @ _,
                mut rest @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                match bindingSlots.get(&name).cloned() {
                    Some(slot @ _) => {
                        let __arg6 @ _ =
                            crate::aver_generated::domain::eval::slots::setSlot(env, slot, &val);
                        __MutualTco6::MergeBindingsSlot(rest, __arg6)
                    }
                    None => __MutualTco6::MergeBindingsSlot(rest, env),
                }
            }
        };
    }
}

/// Merge pattern bindings into slot env using the slots assigned for this specific arm.
pub fn mergeBindingsSlot(
    bindings @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    bindingSlots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverVector<crate::aver_generated::domain::value::Val> {
    __mutual_tco_trampoline_6(
        __MutualTco6::MergeBindingsSlot(bindings, env),
        &bindingSlots,
    )
}

/// Merge one pattern binding into slot env.
pub fn mergeOneBindingSlot(
    name @ _: AverStr,
    val @ _: crate::aver_generated::domain::value::Val,
    rest @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>,
    bindingSlots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverVector<crate::aver_generated::domain::value::Val> {
    __mutual_tco_trampoline_6(
        __MutualTco6::MergeOneBindingSlot(name, val, rest, env),
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
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    loop {
        __state = match __state {
            __MutualTco7::EvalStmtBindSlotNext(
                mut slot @ _,
                mut e @ _,
                mut rest @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let v @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    e,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                let nextEnv @ _ =
                    crate::aver_generated::domain::eval::slots::setSlot(env, slot, &v);
                {
                    let __list_subject = rest.clone();
                    if __list_subject.is_empty() {
                        return Ok(v);
                    } else {
                        __MutualTco7::EvalStmtsSlot(rest, nextEnv)
                    }
                }
            }
            __MutualTco7::EvalStmtExprSlotNext(mut e @ _, mut rest @ _, mut env @ _) => {
                crate::cancel_checkpoint();
                let v @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    e,
                    env.clone(),
                    &*slotMap,
                    &*fns,
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
            __MutualTco7::EvalStmtBindFallbackSlotNext(
                mut name @ _,
                mut e @ _,
                mut rest @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let v @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    e,
                    env.clone(),
                    &*slotMap,
                    &*fns,
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
            __MutualTco7::EvalStmtsSlot(mut stmts @ _, mut env @ _) => {
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
    slot @ _: aver_rt::AverInt,
    e @ _: crate::aver_generated::domain::ast::Expr,
    rest @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_7(
        __MutualTco7::EvalStmtBindSlotNext(slot, e, rest, env),
        &slotMap,
        &fns,
    )
}

/// Evaluate an expression statement and continue statement execution without packing a tuple.
pub fn evalStmtExprSlotNext(
    e @ _: crate::aver_generated::domain::ast::Expr,
    rest @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_7(
        __MutualTco7::EvalStmtExprSlotNext(e, rest, env),
        &slotMap,
        &fns,
    )
}

/// Fallback for unresolved StmtBind in slot path without packing a tuple.
pub fn evalStmtBindFallbackSlotNext(
    name @ _: AverStr,
    e @ _: crate::aver_generated::domain::ast::Expr,
    rest @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_7(
        __MutualTco7::EvalStmtBindFallbackSlotNext(name, e, rest, env),
        &slotMap,
        &fns,
    )
}

/// Evaluate statements in slot path, threading env.
pub fn evalStmtsSlot(
    stmts @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    __mutual_tco_trampoline_7(__MutualTco7::EvalStmtsSlot(stmts, env), &slotMap, &fns)
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
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    loop {
        __state = match __state {
            __MutualTco8::EvalStmtsSlotTail(
                mut selfId @ _,
                mut stmts @ _,
                mut slotCount @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                aver_list_match!(stmts, [] => { return Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(crate::aver_generated::domain::value::Val::ValUnit)) }, [stmt, rest] => { { let __list_subject = rest.clone(); if __list_subject.is_empty() { return crate::aver_generated::domain::eval::core::evalTailStmtSlot(selfId, stmt, slotCount, env, &*slotMap, &*fns) } else { __MutualTco8::EvalStmtsSlotTailNext(selfId, stmt, rest, slotCount, env) } } })
            }
            __MutualTco8::EvalStmtsSlotTailNext(
                mut selfId @ _,
                mut stmt @ _,
                mut rest @ _,
                mut slotCount @ _,
                mut env @ _,
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
                mut selfId @ _,
                mut slot @ _,
                mut e @ _,
                mut rest @ _,
                mut slotCount @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                let v @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
                    e,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                let __arg9 @ _ = crate::aver_generated::domain::eval::slots::setSlot(env, slot, &v);
                __MutualTco8::EvalStmtsSlotTail(selfId, rest, slotCount, __arg9)
            }
            __MutualTco8::EvalStmtsSlotTailExpr(
                mut selfId @ _,
                mut e @ _,
                mut rest @ _,
                mut slotCount @ _,
                mut env @ _,
            ) => {
                crate::cancel_checkpoint();
                crate::aver_generated::domain::eval::core::evalExprSlot(
                    e,
                    env.clone(),
                    &*slotMap,
                    &*fns,
                )?;
                __MutualTco8::EvalStmtsSlotTail(selfId, rest, slotCount, env)
            }
        };
    }
}

/// Evaluate slot statements when the final expression may self-tail-recur.
pub fn evalStmtsSlotTail(
    selfId @ _: aver_rt::AverInt,
    stmts @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slotCount @ _: aver_rt::AverInt,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::EvalStmtsSlotTail(selfId, stmts, slotCount, env),
        &slotMap,
        &fns,
    )
}

/// Evaluate one non-final slot statement before continuing the tail-aware slot loop.
pub fn evalStmtsSlotTailNext(
    selfId @ _: aver_rt::AverInt,
    stmt @ _: crate::aver_generated::domain::ast::Stmt,
    rest @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slotCount @ _: aver_rt::AverInt,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::EvalStmtsSlotTailNext(selfId, stmt, rest, slotCount, env),
        &slotMap,
        &fns,
    )
}

/// Evaluate one slot binding before continuing the tail-aware slot loop.
pub fn evalStmtsSlotTailBind(
    selfId @ _: aver_rt::AverInt,
    slot @ _: aver_rt::AverInt,
    e @ _: crate::aver_generated::domain::ast::Expr,
    rest @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slotCount @ _: aver_rt::AverInt,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::EvalStmtsSlotTailBind(selfId, slot, e, rest, slotCount, env),
        &slotMap,
        &fns,
    )
}

/// Evaluate one non-final expression statement before continuing the tail-aware slot loop.
pub fn evalStmtsSlotTailExpr(
    selfId @ _: aver_rt::AverInt,
    e @ _: crate::aver_generated::domain::ast::Expr,
    rest @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slotCount @ _: aver_rt::AverInt,
    env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::EvalStmtsSlotTailExpr(selfId, e, rest, slotCount, env),
        &slotMap,
        &fns,
    )
}

/// Recognize immediate literal expressions in named-env mode.
pub fn evalImmediateNamedExpr(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::value::Val> {
    crate::cancel_checkpoint();
    {
        let __t = expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__n_fields) => {
                let n = __n_fields.0.clone();
                Some(crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(n),
                ))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFloat(__f_fields) => {
                let f = __f_fields.0.clone();
                Some(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(f),
                ))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprStr(__s_fields) => {
                let s = __s_fields.0.clone();
                Some(crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(s),
                ))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprBool(__b_fields) => {
                let b = __b_fields.0.clone();
                Some(crate::aver_generated::domain::value::Val::ValBool(
                    crate::aver_generated::domain::value::Val__ValBool(b),
                ))
            }
            _ => None,
        }
    }
}

/// Name the AST node kind for diagnostics; interpolation renders primitives only, so an Expr needs a named conversion.
pub fn exprLabel(expr @ _: &crate::aver_generated::domain::ast::Expr) -> AverStr {
    crate::cancel_checkpoint();
    {
        let __t = expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(_) => {
                ::aver_rt::str_lit!("ExprInt")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFloat(_) => {
                ::aver_rt::str_lit!("ExprFloat")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprStr(_) => {
                ::aver_rt::str_lit!("ExprStr")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprBool(_) => {
                ::aver_rt::str_lit!("ExprBool")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprBoolBranch(_) => {
                ::aver_rt::str_lit!("ExprBoolBranch")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprVar(_) => {
                ::aver_rt::str_lit!("ExprVar")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(_) => {
                ::aver_rt::str_lit!("ExprSlot")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprBinopSlotInt(_) => {
                ::aver_rt::str_lit!("ExprBinopSlotInt")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprBinopSlots(_) => {
                ::aver_rt::str_lit!("ExprBinopSlots")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCmpSlotInt(_) => {
                ::aver_rt::str_lit!("ExprCmpSlotInt")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCmpSlots(_) => {
                ::aver_rt::str_lit!("ExprCmpSlots")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprVectorGetOrInt(_) => {
                ::aver_rt::str_lit!("ExprVectorGetOrInt")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIntModOrInt(_) => {
                ::aver_rt::str_lit!("ExprIntModOrInt")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprAdd(_) => {
                ::aver_rt::str_lit!("ExprAdd")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSub(_) => {
                ::aver_rt::str_lit!("ExprSub")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMul(_) => {
                ::aver_rt::str_lit!("ExprMul")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprDiv(_) => {
                ::aver_rt::str_lit!("ExprDiv")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeg(_) => {
                ::aver_rt::str_lit!("ExprNeg")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprEq(_) => {
                ::aver_rt::str_lit!("ExprEq")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeq(_) => {
                ::aver_rt::str_lit!("ExprNeq")
            }
            _ => crate::aver_generated::domain::eval::core::exprLabelAggregate(expr),
        }
    }
}

/// Continue naming AST node kinds for the comparison, aggregate, and call forms.
pub fn exprLabelAggregate(expr @ _: &crate::aver_generated::domain::ast::Expr) -> AverStr {
    crate::cancel_checkpoint();
    {
        let __t = expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprLt(_) => {
                ::aver_rt::str_lit!("ExprLt")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGt(_) => {
                ::aver_rt::str_lit!("ExprGt")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLte(_) => {
                ::aver_rt::str_lit!("ExprLte")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGte(_) => {
                ::aver_rt::str_lit!("ExprGte")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprConcat(_) => {
                ::aver_rt::str_lit!("ExprConcat")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprTuple(_) => {
                ::aver_rt::str_lit!("ExprTuple")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprList(_) => {
                ::aver_rt::str_lit!("ExprList")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprRecord(_) => {
                ::aver_rt::str_lit!("ExprRecord")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(_) => {
                ::aver_rt::str_lit!("ExprFieldAccess")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCall(_) => {
                ::aver_rt::str_lit!("ExprCall")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(_) => {
                ::aver_rt::str_lit!("ExprCallDirect")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(_) => {
                ::aver_rt::str_lit!("ExprCallBuiltin")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltinId(_) => {
                ::aver_rt::str_lit!("ExprCallBuiltinId")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMatch(_) => {
                ::aver_rt::str_lit!("ExprMatch")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprPropagate(_) => {
                ::aver_rt::str_lit!("ExprPropagate")
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIndependentProduct(_) => {
                ::aver_rt::str_lit!("ExprIndependentProduct")
            }
            _ => {
                ::aver_rt::str_lit!("Expr")
            }
        }
    }
}

/// Look up variable, Option.None, a named function reference, or a nullary variant constructor.
#[inline(always)]
pub fn evalVar(
    name @ _: AverStr,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
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
    mut vecExpr @ _: crate::aver_generated::domain::ast::Expr,
    mut idxExpr @ _: crate::aver_generated::domain::ast::Expr,
    defaultValue @ _: aver_rt::AverInt,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV @ _ = crate::aver_generated::domain::eval::core::evalExpr(vecExpr, env.clone(), fns)?;
    let idxV @ _ = crate::aver_generated::domain::eval::core::evalExpr(idxExpr, env, fns)?;
    crate::aver_generated::domain::eval::ops::evalVectorGetOrIntVals(&vecV, &idxV, defaultValue)
}

/// Evaluate a fused Int.mod + Result.withDefault in map path.
pub fn evalIntModOrIntExpr(
    mut a @ _: crate::aver_generated::domain::ast::Expr,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    defaultValue @ _: aver_rt::AverInt,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let aV @ _ = crate::aver_generated::domain::eval::core::evalExpr(a, env.clone(), fns)?;
    let bV @ _ = crate::aver_generated::domain::eval::core::evalExpr(b, env, fns)?;
    crate::aver_generated::domain::eval::ops::evalIntModOrIntVals(&aV, &bV, defaultValue)
}

/// Evaluate ? operator: unwrap Ok or propagate Err.
pub fn evalPropagate(
    mut inner @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::eval::core::evalExpr(inner, env, fns)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValOk(__x_fields) => {
                let x = &__x_fields.0;
                Ok(x.clone())
            }
            crate::aver_generated::domain::value::Val__view::ValErr(__e_fields) => {
                let e = &__e_fields.0;
                {
                    let __t = e;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValStr(__msg_fields) => {
                            let msg = __msg_fields.0.clone();
                            Err(
                                crate::aver_generated::domain::eval::common::wrapPropagatedError(
                                    msg,
                                ),
                            )
                        }
                        _ => Err(
                            crate::aver_generated::domain::eval::common::wrapPropagatedError(
                                ::aver_rt::str_lit!("propagated error"),
                            ),
                        ),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("? operator requires Result value")),
        }
    }
}

/// Evaluate independent product using the self-host's own divide-and-conquer ?!, then unwrap guest Results if needed.
pub fn evalIndependentProduct(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    unwrap @ _: bool,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items @ _ =
        crate::aver_generated::domain::eval::core::evalIndependentItems(exprs, env, fns)?;
    if unwrap {
        {
            let __arg5 @ _ =
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
            crate::aver_generated::domain::eval::core::unwrapProductResults__collected(
                &items, __arg5,
            )
        }
    } else {
        Ok(crate::aver_generated::domain::value::Val::ValTuple(
            crate::aver_generated::domain::value::Val__ValTuple(items),
        ))
    }
}

/// Evaluate guest independent-product branches through a balanced tree of the self-host's own ?!.
pub fn evalIndependentItems(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = exprs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_list_match!(rest, [] => match crate::aver_generated::domain::eval::core::evalExpr(a.clone(), env, fns) { Ok(va @ _) => { Ok(aver_rt::AverList::from_vec(vec![va])) }, Err(e @ _) => { Err(e) } }, [b, rest2] => { { let __list_subject = rest2; if __list_subject.is_empty() { { let (va, vb) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let __r0 = crate::aver_generated::domain::eval::core::evalExpr(a.clone(), env.clone(), fns); crate::aver_replay::set_effect_branch(1); let __r1 = crate::aver_generated::domain::eval::core::evalExpr(b, env, fns); crate::aver_replay::exit_effect_group(); match (__r0, __r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (__r0, __r1) => { if let Err(__err) = __r0 { Err(__err) } else if let Err(__err) = __r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|__s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let __h0 = { let env = env.clone(); let fns = fns; let a = a; aver_rt::spawn_scoped(__s, move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalExpr(a.clone(), env.clone(), fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let __h1 = { let env = env; let fns = fns; let b = b; aver_rt::spawn_scoped(__s, move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalExpr(b, env, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __b0 = __h0.join().unwrap(); let __b1 = __h1.join().unwrap(); match (__b0, __b1) { (crate::ParallelBranch::Completed(__r0), crate::ParallelBranch::Completed(__r1)) => match (__r0, __r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (__r0, __r1) => { if let Err(__err) = __r0 { Err(__err) } else if let Err(__err) = __r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (__b0, __b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = __b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = __b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok(aver_rt::AverList::from_vec(vec![va, vb])) } } else { { let (leftExprs, rightExprs) = { let __arg15 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::Expr>>::empty(); { let __arg16 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::Expr>>::empty(); crate::aver_generated::domain::eval::core::splitIndependentExprs(exprs, __arg15, __arg16, true) } }; { let (leftVals, rightVals) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let __r0 = crate::aver_generated::domain::eval::core::evalIndependentItems(&leftExprs, env.clone(), fns); crate::aver_replay::set_effect_branch(1); let __r1 = crate::aver_generated::domain::eval::core::evalIndependentItems(&rightExprs, env, fns); crate::aver_replay::exit_effect_group(); match (__r0, __r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (__r0, __r1) => { if let Err(__err) = __r0 { Err(__err) } else if let Err(__err) = __r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|__s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let __h0 = { let env = env.clone(); let fns = fns; let leftExprs = leftExprs; aver_rt::spawn_scoped(__s, move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalIndependentItems(&leftExprs, env.clone(), fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let __h1 = { let env = env; let fns = fns; let rightExprs = rightExprs; aver_rt::spawn_scoped(__s, move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalIndependentItems(&rightExprs, env, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __b0 = __h0.join().unwrap(); let __b1 = __h1.join().unwrap(); match (__b0, __b1) { (crate::ParallelBranch::Completed(__r0), crate::ParallelBranch::Completed(__r1)) => match (__r0, __r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (__r0, __r1) => { if let Err(__err) = __r0 { Err(__err) } else if let Err(__err) = __r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (__b0, __b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = __b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = __b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok({ let __arg17 @ _ = <aver_rt::AverList<crate::aver_generated::domain::value::Val>>::empty(); crate::aver_generated::domain::eval::core::interleaveIndependentVals(&leftVals, &rightVals, true, __arg17) }) } } } } })
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Split a product into alternating branches so recursive ?! can cover all items.
#[inline(always)]
pub fn splitIndependentExprs(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut leftAcc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut rightAcc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut sendLeft @ _: bool,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return ((leftAcc).reverse_owned(), (rightAcc).reverse_owned());
            } else {
                if let (Some(expr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if sendLeft {
                        {
                            let __tco0 = rest;
                            let __tco1 = aver_rt::AverList::prepend_owned(expr.clone(), leftAcc);
                            let __tco3 = false;
                            __hold0 = __tco0;
                            exprs = &__hold0;
                            leftAcc = __tco1;
                            sendLeft = __tco3;
                            continue;
                        }
                    } else {
                        {
                            let __tco0 = rest;
                            let __tco2 = aver_rt::AverList::prepend_owned(expr.clone(), rightAcc);
                            let __tco3 = true;
                            __hold0 = __tco0;
                            exprs = &__hold0;
                            rightAcc = __tco2;
                            sendLeft = __tco3;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Restore original product order after alternating recursive split.
#[inline(always)]
pub fn interleaveIndependentVals(
    left @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    right @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut takeLeft @ _: bool,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut left @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = left;
    let mut __hold1: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut right @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = right;
    loop {
        crate::cancel_checkpoint();
        if takeLeft {
            {
                let __list_subject = left;
                if __list_subject.is_empty() {
                    return crate::aver_generated::domain::eval::core::finishIndependentVals(
                        right, acc,
                    );
                } else {
                    if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                        {
                            let __tco0 = rest;
                            let __tco2 = false;
                            let __tco3 = aver_rt::AverList::prepend_owned(v.clone(), acc);
                            __hold0 = __tco0;
                            left = &__hold0;
                            takeLeft = __tco2;
                            acc = __tco3;
                            continue;
                        }
                    } else {
                        panic!("Aver Rust codegen: non-exhaustive list match")
                    }
                }
            }
        } else {
            {
                let __list_subject = right;
                if __list_subject.is_empty() {
                    return crate::aver_generated::domain::eval::core::finishIndependentVals(
                        left, acc,
                    );
                } else {
                    if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                        {
                            let __tco1 = rest;
                            let __tco2 = true;
                            let __tco3 = aver_rt::AverList::prepend_owned(v.clone(), acc);
                            __hold1 = __tco1;
                            right = &__hold1;
                            takeLeft = __tco2;
                            acc = __tco3;
                            continue;
                        }
                    } else {
                        panic!("Aver Rust codegen: non-exhaustive list match")
                    }
                }
            }
        }
    }
}

/// Append remaining values after interleaving and return in forward order.
#[inline(always)]
pub fn finishIndependentVals(
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = items;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = items;
            if __list_subject.is_empty() {
                return (acc).reverse_owned();
            } else {
                if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::AverList::prepend_owned(v.clone(), acc);
                        __hold0 = __tco0;
                        items = &__hold0;
                        acc = __tco1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Unwrap all Result values in an independent product (?!).
#[inline(always)]
pub fn unwrapProductResults(
    mut items @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    loop {
        crate::cancel_checkpoint();
        ::aver_rt::aver_list_match_reuse!(items, [] => { return Ok(crate::aver_generated::domain::value::Val::ValTuple(crate::aver_generated::domain::value::Val__ValTuple((acc).reverse_owned()))); }, [v, rest, __list_slot] => { { let __t = v; match __t.view() {
        crate::aver_generated::domain::value::Val__view::ValOk(__x_fields) => {
            let x = &__x_fields.0;
            {
            let __tco0 = rest;
            let __tco1 = __list_slot.prepend(x.clone(), acc);
            items = __tco0;
            acc = __tco1;
            continue;
        }
        },
        crate::aver_generated::domain::value::Val__view::ValErr(__e_fields) => {
            let e = &__e_fields.0;
            { let __t = e; match __t.view() {
        crate::aver_generated::domain::value::Val__view::ValStr(__msg_fields) => {
            let msg = __msg_fields.0.clone();
            return Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(msg));
        },
        _ => {
            return Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(::aver_rt::str_lit!("propagated error")));
        }
    } }
        },
        _ => {
            return Err(::aver_rt::str_lit!("?! operator requires all elements to be Result values"));
        }
    } } })
    }
}

/// Evaluate right side of binop and apply.
pub fn evalBinopRight(
    mut va @ _: crate::aver_generated::domain::value::Val,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    op @ _: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(b, env, fns) {
        Err(e @ _) => Err(e),
        Ok(vb @ _) => crate::aver_generated::domain::eval::ops::evalBinopVals(va, vb, op),
    }
}

/// Evaluate a binary operation on two expressions.
pub fn evalBinop(
    mut a @ _: crate::aver_generated::domain::ast::Expr,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    op @ _: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(a, env.clone(), fns) {
        Err(e @ _) => Err(e),
        Ok(va @ _) => {
            crate::aver_generated::domain::eval::core::evalBinopRight(va, b, env, fns, op)
        }
    }
}

/// Evaluate unary minus on the named-env path.
pub fn evalNeg(
    mut inner @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(inner, env, fns) {
        Err(e @ _) => Err(e),
        Ok(v @ _) => crate::aver_generated::domain::eval::ops::evalNegVals(&v),
    }
}

/// Evaluate right side of comparison and apply.
pub fn evalCmpRight(
    mut va @ _: crate::aver_generated::domain::value::Val,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(b, env, fns) {
        Err(e @ _) => Err(e),
        Ok(vb @ _) => crate::aver_generated::domain::eval::ops::evalCmpVals(va, vb, op),
    }
}

/// Evaluate a comparison expression.
pub fn evalCmp(
    mut a @ _: crate::aver_generated::domain::ast::Expr,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(a, env.clone(), fns) {
        Err(e @ _) => Err(e),
        Ok(va @ _) => crate::aver_generated::domain::eval::core::evalCmpRight(va, b, env, fns, op),
    }
}

/// Evaluate string interpolation: concat all parts as strings.
pub fn evalConcatExpr(
    parts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalConcatParts(
        parts,
        env,
        fns,
        ::aver_rt::str_lit!(""),
    )
}

/// Concatenate interpolation parts. Self-recursive for codegen TCO.
pub fn evalConcatParts(
    parts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    mut acc @ _: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut parts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = parts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = parts;
            if __list_subject.is_empty() {
                return Ok(crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(acc),
                ));
            } else {
                if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::aver_generated::domain::eval::core::evalExpr(
                        p.clone(),
                        env.clone(),
                        fns,
                    ) {
                        Err(e @ _) => {
                            return Err(e);
                        }
                        Ok(v @ _) => {
                            let __tco0 = rest;
                            let __tco3 = (acc + &crate::aver_generated::domain::value::valRepr(&v));
                            __hold0 = __tco0;
                            parts = &__hold0;
                            acc = __tco3;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Evaluate tuple literal.
pub fn evalTupleExpr(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items @ _ = crate::aver_generated::domain::eval::core::evalListItems(exprs, env, fns)?;
    Ok(crate::aver_generated::domain::value::Val::ValTuple(
        crate::aver_generated::domain::value::Val__ValTuple(items),
    ))
}

/// Evaluate record constructor.
pub fn evalRecordExpr(
    name @ _: AverStr,
    fieldExprs @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fields @ _ =
        crate::aver_generated::domain::eval::core::evalRecordFields(fieldExprs, env, fns)?;
    Ok(crate::aver_generated::domain::value::Val::ValRecord(
        crate::aver_generated::domain::value::Val__ValRecord(name, fields),
    ))
}

/// Evaluate record field expressions.
pub fn evalRecordFields(
    fieldExprs @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = fieldExprs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let (fname, expr) = pair.clone();
                    crate::aver_generated::domain::eval::core::evalRecordFieldOne(
                        fname, expr, &rest, env, fns,
                    )
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Evaluate one record field and continue.
pub fn evalRecordFieldOne(
    fname @ _: AverStr,
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    rest @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::eval::core::evalExpr(expr, env.clone(), fns)?;
    let restFields @ _ =
        crate::aver_generated::domain::eval::core::evalRecordFields(rest, env, fns)?;
    Ok(aver_rt::AverList::prepend_owned((fname, v), restFields))
}

/// Evaluate field access: obj.field.
pub fn evalFieldAccess(
    mut obj @ _: crate::aver_generated::domain::ast::Expr,
    field @ _: AverStr,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::eval::core::evalExpr(obj, env, fns)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValRecord(__fields_fields) => {
                let fields = &__fields_fields.1;
                crate::aver_generated::domain::eval::common::lookupField(fields, field)
            }
            _ => Err(::aver_rt::str_lit!("field access on non-record")),
        }
    }
}

/// Evaluate a list literal.
pub fn evalListExpr(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalListItems(exprs, env, fns) {
        Ok(items @ _) => Ok(crate::aver_generated::domain::value::Val::ValList(
            crate::aver_generated::domain::value::Val__ValList(items),
        )),
        Err(e @ _) => Err(e),
    }
}

/// Evaluate list of expressions into list of values.
pub fn evalListItems(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    let __arg3 @ _ = <aver_rt::AverList<crate::aver_generated::domain::value::Val>>::empty();
    crate::aver_generated::domain::eval::core::evalListItemsRev(exprs, env, fns, __arg3)
}

/// Tail-recursive worker for evalListItems. Accumulates in reverse.
pub fn evalListItemsRev(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return Ok((acc).reverse_owned());
            } else {
                if let (Some(e), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::aver_generated::domain::eval::core::evalExpr(
                        e.clone(),
                        env.clone(),
                        fns,
                    ) {
                        Err(err @ _) => {
                            return Err(err);
                        }
                        Ok(v @ _) => {
                            let __tco0 = rest;
                            let __tco3 = aver_rt::AverList::prepend_owned(v, acc);
                            __hold0 = __tco0;
                            exprs = &__hold0;
                            acc = __tco3;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Evaluate a list of argument expressions.
pub fn evalArgs(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    let __arg3 @ _ = <aver_rt::AverList<crate::aver_generated::domain::value::Val>>::empty();
    crate::aver_generated::domain::eval::core::evalArgsRev(exprs, env, fns, __arg3)
}

/// Tail-recursive worker for evalArgs. Accumulates in reverse.
pub fn evalArgsRev(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return Ok((acc).reverse_owned());
            } else {
                if let (Some(e), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::aver_generated::domain::eval::core::evalExpr(
                        e.clone(),
                        env.clone(),
                        fns,
                    ) {
                        Err(err @ _) => {
                            return Err(err);
                        }
                        Ok(v @ _) => {
                            let __tco0 = rest;
                            let __tco3 = aver_rt::AverList::prepend_owned(v, acc);
                            __hold0 = __tco0;
                            exprs = &__hold0;
                            acc = __tco3;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Dispatch call: record update if Type.update, else normal call.
pub fn callWithArgs(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    name @ _: AverStr,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::eval::records::isRecordUpdate(&name, args) {
        crate::aver_generated::domain::eval::records::doRecordUpdate(args)
    } else {
        crate::aver_generated::domain::eval::core::callWithArgsNormal(args, name, fns)
    }
}

/// Normal function call dispatch. Uses slot-based eval for resolved fns.
pub fn callWithArgsNormal(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    name @ _: AverStr,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::store::lookupFnOption(fns, name.clone()) {
        Some(fd @ _) => crate::aver_generated::domain::eval::core::callResolved(&fd, args, fns),
        None => crate::aver_generated::domain::builtins::callBuiltin(name, args),
    }
}

/// Call a function: slot-based if resolved, map-based otherwise.
pub fn callResolved(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fnId @ _ = crate::aver_generated::domain::eval::store::lookupFnId(fns, fd.name.clone())?;
    crate::aver_generated::domain::eval::core::callResolvedById(fnId, fd, args, fns)
}

/// Call a function with a known store id: slot-based if resolved, map-based otherwise.
pub fn callResolvedById(
    fnId @ _: aver_rt::AverInt,
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if (fd.slotCount > aver_rt::AverInt::from_i64(0)) {
        {
            let __arg4 @ _ = crate::aver_generated::domain::eval::slots::buildSlotEnv(
                args,
                fd.slotCount.clone(),
            )?;
            crate::aver_generated::domain::eval::core::evalResolvedSlotFn(fnId, fd, __arg4, fns)
        }
    } else {
        {
            let __arg6 @ _ = {
                let __arg5 @ _ =
                    <aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>>::new();
                crate::aver_generated::domain::eval::store::zipArgs(&fd.params, args, __arg5)
            };
            crate::aver_generated::domain::eval::core::evalResolvedNamedFn(fd, __arg6, fns)
        }
    }
}

/// Evaluate a resolved function body in slot mode, looping self-tail-calls instead of recursing on the host stack.
pub fn evalResolvedSlotFn(
    fnId @ _: aver_rt::AverInt,
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    mut calleeEnv @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if fd.tailLoop {
        crate::aver_generated::domain::eval::core::evalResolvedSlotLoop(fnId, fd, calleeEnv, fns)
    } else {
        crate::aver_generated::domain::eval::core::evalResolvedSlotDirect(fd, calleeEnv, fns)
    }
}

/// Run the tail-aware slot evaluator only for functions that actually end in self-tail-calls.
pub fn evalResolvedSlotLoop(
    mut fnId @ _: aver_rt::AverInt,
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    mut calleeEnv @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    loop {
        crate::cancel_checkpoint();
        let step @ _ = crate::aver_generated::domain::eval::core::evalResolvedSlotStep(
            fnId.clone(),
            fd,
            calleeEnv,
            fns,
        )?;
        match step {
            crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v) => {
                return {
                    let __arg7 @ _ = Ok(v);
                    crate::aver_generated::domain::eval::common::normalizeFnReturn(&__arg7)
                };
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
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    mut calleeEnv @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let result @ _ = match &fd.fastPath {
        crate::aver_generated::domain::ast::FnFastPath::FastLeaf(leaf) => {
            crate::aver_generated::domain::eval::fast::runFastLeafSlot(leaf, &calleeEnv)
        }
        crate::aver_generated::domain::ast::FnFastPath::FastForwardCall(targetId, slotArgs) => {
            let targetId = targetId.clone();
            crate::aver_generated::domain::eval::core::fastForwardCall(
                &calleeEnv, targetId, slotArgs, fns,
            )
        }
        crate::aver_generated::domain::ast::FnFastPath::FastBoolSlotBranch(
            slot,
            thenLeaf,
            elseLeaf,
        ) => {
            let slot = slot.clone();
            crate::aver_generated::domain::eval::fast::fastBoolSlotBranch(
                &calleeEnv, slot, thenLeaf, elseLeaf,
            )
        }
        crate::aver_generated::domain::ast::FnFastPath::FastEqIntBranch(
            slot,
            expected,
            thenLeaf,
            elseLeaf,
        ) => {
            let slot = slot.clone();
            let expected = expected.clone();
            crate::aver_generated::domain::eval::fast::fastEqIntBranch(
                &calleeEnv, slot, expected, thenLeaf, elseLeaf,
            )
        }
        crate::aver_generated::domain::ast::FnFastPath::FastEqStringBranch(
            slot,
            expected,
            thenLeaf,
            elseLeaf,
        ) => {
            let slot = slot.clone();
            let expected = expected.clone();
            crate::aver_generated::domain::eval::fast::fastEqStringBranch(
                &calleeEnv, slot, &expected, thenLeaf, elseLeaf,
            )
        }
        crate::aver_generated::domain::ast::FnFastPath::FastLtIntSlotsBranch(
            lhsSlot,
            rhsSlot,
            thenLeaf,
            elseLeaf,
        ) => {
            let lhsSlot = lhsSlot.clone();
            let rhsSlot = rhsSlot.clone();
            crate::aver_generated::domain::eval::fast::fastLtIntSlotsBranch(
                &calleeEnv, lhsSlot, rhsSlot, thenLeaf, elseLeaf,
            )
        }
        crate::aver_generated::domain::ast::FnFastPath::FastListSlotBranch(
            slot,
            emptyLeaf,
            headSlot,
            tailSlot,
            consLeaf,
        ) => {
            let slot = slot.clone();
            let headSlot = headSlot.clone();
            let tailSlot = tailSlot.clone();
            crate::aver_generated::domain::eval::fast::fastListSlotBranch(
                calleeEnv, slot, emptyLeaf, headSlot, tailSlot, consLeaf,
            )
        }
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
                fd.body.clone(),
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
    fnId @ _: aver_rt::AverInt,
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    mut calleeEnv @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    crate::cancel_checkpoint();
    match &fd.fastPath {
        crate::aver_generated::domain::ast::FnFastPath::FastLeaf(leaf) => {
            match crate::aver_generated::domain::eval::fast::runFastLeafSlot(leaf, &calleeEnv) {
                Ok(v @ _) => {
                    Ok(crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v))
                }
                Err(e @ _) => Err(e),
            }
        }
        crate::aver_generated::domain::ast::FnFastPath::FastForwardCall(targetId, slotArgs) => {
            let targetId = targetId.clone();
            match crate::aver_generated::domain::eval::core::fastForwardCall(
                &calleeEnv, targetId, slotArgs, fns,
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
            let slot = slot.clone();
            match crate::aver_generated::domain::eval::fast::fastBoolSlotBranch(
                &calleeEnv, slot, thenLeaf, elseLeaf,
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
            let slot = slot.clone();
            let expected = expected.clone();
            match crate::aver_generated::domain::eval::fast::fastEqIntBranch(
                &calleeEnv, slot, expected, thenLeaf, elseLeaf,
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
            let slot = slot.clone();
            let expected = expected.clone();
            match crate::aver_generated::domain::eval::fast::fastEqStringBranch(
                &calleeEnv, slot, &expected, thenLeaf, elseLeaf,
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
            let lhsSlot = lhsSlot.clone();
            let rhsSlot = rhsSlot.clone();
            match crate::aver_generated::domain::eval::fast::fastLtIntSlotsBranch(
                &calleeEnv, lhsSlot, rhsSlot, thenLeaf, elseLeaf,
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
            let slot = slot.clone();
            let headSlot = headSlot.clone();
            let tailSlot = tailSlot.clone();
            match crate::aver_generated::domain::eval::fast::fastListSlotBranch(
                calleeEnv, slot, emptyLeaf, headSlot, tailSlot, consLeaf,
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
                fd.body.clone(),
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
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    mut calleeEnv @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let result @ _ = match &fd.fastPath {
        crate::aver_generated::domain::ast::FnFastPath::FastLeaf(leaf) => {
            let leaf = leaf.clone();
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
            crate::aver_generated::domain::eval::core::evalStmts(&fd.body, calleeEnv, fns)
        }
        _ => crate::aver_generated::domain::eval::core::evalResolvedSingleExprNamed(
            &fd.body, calleeEnv, fns,
        ),
    };
    crate::aver_generated::domain::eval::common::normalizeFnReturn(&result)
}

/// Fast path for a single expression body in slot mode.
pub fn evalResolvedSingleExprSlot(
    body @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut calleeEnv @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = body;
        if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
            if (rest == aver_rt::AverList::empty()) {
                crate::aver_generated::domain::eval::core::evalResolvedSingleStmtSlot(
                    stmt, calleeEnv, slotMap, fns,
                )
            } else {
                crate::aver_generated::domain::eval::core::evalStmtsSlot(
                    body.clone(),
                    calleeEnv,
                    slotMap,
                    fns,
                )
            }
        } else {
            crate::aver_generated::domain::eval::core::evalStmtsSlot(
                body.clone(),
                calleeEnv,
                slotMap,
                fns,
            )
        }
    }
}

/// Tail-aware version of the single-expression fast path for slot functions.
pub fn evalResolvedSingleExprSlotTail(
    selfId @ _: aver_rt::AverInt,
    body @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slotCount @ _: aver_rt::AverInt,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut calleeEnv @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = body;
        if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
            if (rest == aver_rt::AverList::empty()) {
                crate::aver_generated::domain::eval::core::evalTailStmtSlot(
                    selfId, stmt, slotCount, calleeEnv, slotMap, fns,
                )
            } else {
                match crate::aver_generated::domain::eval::core::evalStmtsSlot(
                    body.clone(),
                    calleeEnv,
                    slotMap,
                    fns,
                ) {
                    Ok(v @ _) => Ok(
                        crate::aver_generated::domain::eval::core::SlotTailStep::SlotTailDone(v),
                    ),
                    Err(e @ _) => Err(e),
                }
            }
        } else {
            match crate::aver_generated::domain::eval::core::evalStmtsSlot(
                body.clone(),
                calleeEnv,
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

/// Evaluate a final statement in slot mode, capturing self-tail-calls as loop re-entry requests.
pub fn evalTailStmtSlot(
    selfId @ _: aver_rt::AverInt,
    mut stmt @ _: crate::aver_generated::domain::ast::Stmt,
    slotCount @ _: aver_rt::AverInt,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<SlotTailStep, AverStr> {
    crate::cancel_checkpoint();
    match &stmt {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::eval::core::evalTailExprSlot(
                selfId,
                expr.clone(),
                slotCount,
                env,
                slotMap,
                fns,
            )
        }
        _ => {
            let __subject = {
                let __arg9 @ _ = aver_rt::AverList::from_vec(vec![stmt]);
                crate::aver_generated::domain::eval::core::evalStmtsSlot(__arg9, env, slotMap, fns)
            };
            match __subject {
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
    mut stmt @ _: crate::aver_generated::domain::ast::Stmt,
    mut calleeEnv @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match &stmt {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::eval::core::evalExprSlot(
                expr.clone(),
                calleeEnv,
                slotMap,
                fns,
            )
        }
        _ => {
            let __arg5 @ _ = aver_rt::AverList::from_vec(vec![stmt]);
            crate::aver_generated::domain::eval::core::evalStmtsSlot(
                __arg5, calleeEnv, slotMap, fns,
            )
        }
    }
}

/// Fast path for a single expression body in named-env mode.
pub fn evalResolvedSingleExprNamed(
    body @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut calleeEnv @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = body;
        if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
            if (rest == aver_rt::AverList::empty()) {
                crate::aver_generated::domain::eval::core::evalResolvedSingleStmtNamed(
                    stmt, calleeEnv, fns,
                )
            } else {
                crate::aver_generated::domain::eval::core::evalStmts(body, calleeEnv, fns)
            }
        } else {
            crate::aver_generated::domain::eval::core::evalStmts(body, calleeEnv, fns)
        }
    }
}

/// Run the single expression stmt fast path in named-env mode.
pub fn evalResolvedSingleStmtNamed(
    mut stmt @ _: crate::aver_generated::domain::ast::Stmt,
    mut calleeEnv @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match &stmt {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::eval::core::evalExpr(expr.clone(), calleeEnv, fns)
        }
        _ => {
            let __arg4 @ _ = aver_rt::AverList::from_vec(vec![stmt]);
            crate::aver_generated::domain::eval::core::evalStmts(&__arg4, calleeEnv, fns)
        }
    }
}

/// Execute named-env leaves directly when they are pure constants; otherwise fall back.
pub fn runFastLeafNamed(
    leaf @ _: &crate::aver_generated::domain::ast::FastLeaf,
    body @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut calleeEnv @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
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
        _ => crate::aver_generated::domain::eval::core::evalResolvedSingleExprNamed(
            body, calleeEnv, fns,
        ),
    }
}

/// Forward a direct call by reading already-resolved slot arguments without evaluating an AST arg list.
pub fn fastForwardCall(
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    fnId @ _: aver_rt::AverInt,
    slotArgs @ _: &aver_rt::AverIntList,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let args @ _ = {
        let __arg6 @ _ =
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
        crate::aver_generated::domain::eval::core::collectFastForwardArgs__collected(
            slotArgs, calleeEnv, __arg6,
        )
    }?;
    let fd @ _ = crate::aver_generated::domain::eval::store::lookupFnById(fns, fnId.clone())?;
    crate::aver_generated::domain::eval::core::callResolvedById(fnId, &fd, &args, fns)
}

/// Collect forwarded slot arguments in call order.
#[inline(always)]
pub fn collectFastForwardArgs(
    mut slotArgs @ _: aver_rt::AverIntList,
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(slotArgs, [] => { return Ok((acc).reverse_owned()); }, [slot, rest] => { match crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot) { Ok(v @ _) => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend_owned(v, acc);
            slotArgs = __tco0;
            acc = __tco2;
            continue;
        } }, Err(e @ _) => { return Err(e); } } })
    }
}

/// Evaluate a function call.
pub fn evalCall(
    name @ _: AverStr,
    argExprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalArgs(argExprs, env, fns) {
        Err(e @ _) => Err(e),
        Ok(args @ _) => crate::aver_generated::domain::eval::core::callWithArgs(&args, name, fns),
    }
}

/// Call a pre-resolved function directly using the function store.
pub fn evalCallDirect(
    fnId @ _: aver_rt::AverInt,
    argExprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fd @ _ = crate::aver_generated::domain::eval::store::lookupFnById(fns, fnId.clone())?;
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
    fnId @ _: aver_rt::AverInt,
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    argExprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv @ _ = {
        let __arg6 @ _ = match aver_rt::checked_vector_size(&(fd.slotCount)) {
            Some(__n) => Ok(aver_rt::AverVector::new(
                __n,
                crate::aver_generated::domain::value::Val::ValUnit,
            )),
            None => Err(aver_rt::AverStr::from(aver_rt::vector_size_error_message())),
        }?;
        crate::aver_generated::domain::eval::core::evalArgsMapToSlotEnv(
            argExprs,
            env,
            fns,
            __arg6,
            aver_rt::AverInt::from_i64(0),
        )
    }?;
    crate::aver_generated::domain::eval::core::evalResolvedSlotFn(fnId, fd, calleeEnv, fns)
}

/// Call resolved function by evaluating args directly into the callee named env.
pub fn evalCallDirectMapToNamed(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    argExprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv @ _ = {
        let __arg5 @ _ =
            <aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>>::new();
        crate::aver_generated::domain::eval::core::evalArgsMapToNamedEnv(
            argExprs.clone(),
            fd.params.clone(),
            env,
            fns,
            __arg5,
        )
    }?;
    crate::aver_generated::domain::eval::core::evalResolvedNamedFn(fd, calleeEnv, fns)
}

/// Call a builtin directly, skipping fns lookup.
pub fn evalCallBuiltin(
    name @ _: AverStr,
    argExprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalArgs(argExprs, env, fns) {
        Err(e @ _) => Err(e),
        Ok(args @ _) => crate::aver_generated::domain::builtins::callBuiltin(name, &args),
    }
}

/// Evaluate a statement expression.
pub fn evalStmtExpr(
    mut e @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<
    (
        crate::aver_generated::domain::value::Val,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(e, env.clone(), fns) {
        Ok(v @ _) => Ok((v, env)),
        Err(msg @ _) => Err(msg),
    }
}

/// Evaluate a binding statement.
pub fn evalStmtBind(
    name @ _: AverStr,
    mut e @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<
    (
        crate::aver_generated::domain::value::Val,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::core::evalExpr(e, env.clone(), fns) {
        Ok(v @ _) => Ok((v.clone(), env.insert_owned(name, v))),
        Err(msg @ _) => Err(msg),
    }
}

/// Evaluate a statement, returning the result and updated environment.
pub fn evalStmt(
    stmt @ _: &crate::aver_generated::domain::ast::Stmt,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<
    (
        crate::aver_generated::domain::value::Val,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    match stmt {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(e) => {
            crate::aver_generated::domain::eval::core::evalStmtExpr(e.clone(), env, fns)
        }
        crate::aver_generated::domain::ast::Stmt::StmtBind(name, e) => {
            let name = name.clone();
            crate::aver_generated::domain::eval::core::evalStmtBind(name, e.clone(), env, fns)
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(_, e) => {
            crate::aver_generated::domain::eval::core::evalStmtExpr(e.clone(), env, fns)
        }
    }
}

/// Evaluate statements, threading env. Self-recursive for codegen TCO.
pub fn evalStmts(
    stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = stmts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = stmts;
            if __list_subject.is_empty() {
                return Ok(crate::aver_generated::domain::value::Val::ValUnit);
            } else {
                if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::aver_generated::domain::eval::core::evalStmt(s, env, fns) {
                        Err(e @ _) => {
                            return Err(e);
                        }
                        Ok(pair @ _) => {
                            let (v, newEnv) = pair;
                            {
                                let __list_subject = rest.clone();
                                if __list_subject.is_empty() {
                                    return Ok(v);
                                } else {
                                    {
                                        let __tco0 = rest;
                                        let __tco1 = newEnv;
                                        __hold0 = __tco0;
                                        stmts = &__hold0;
                                        env = __tco1;
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Evaluate top-level statements in source order and keep the resulting env, so top-level bindings stay visible to functions. Each statement sees the bindings before it, both directly and through any function it calls.
pub fn evalTopLevelStmts(
    stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut env @ _: aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    mut last @ _: crate::aver_generated::domain::value::Val,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<
    (
        crate::aver_generated::domain::value::Val,
        aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    ),
    AverStr,
> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = stmts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = stmts;
            if __list_subject.is_empty() {
                return Ok((last, env));
            } else {
                if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __subject = {
                            let __arg10 @ _ =
                                crate::aver_generated::domain::eval::store::withGlobals(
                                    fns.clone(),
                                    env.clone(),
                                );
                            crate::aver_generated::domain::eval::core::evalStmt(s, env, &__arg10)
                        };
                        match __subject {
                            Err(e @ _) => {
                                return Err(e);
                            }
                            Ok(pair @ _) => {
                                let (v, newEnv) = pair;
                                {
                                    let __tco0 = rest;
                                    let __tco1 = newEnv;
                                    let __tco2 = v;
                                    __hold0 = __tco0;
                                    stmts = &__hold0;
                                    env = __tco1;
                                    last = __tco2;
                                    continue;
                                }
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Evaluate a complete program: run top-level stmts, then call main() if it exists.
pub fn evalProgram(
    prog @ _: &crate::aver_generated::domain::ast::Program,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fnsStore @ _ = crate::aver_generated::domain::eval::store::fnsToStore(&prog.fns);
    let top @ _ = {
        let __arg5 @ _ =
            <aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>>::new();
        {
            let __arg6 @ _ = crate::aver_generated::domain::value::Val::ValUnit;
            crate::aver_generated::domain::eval::core::evalTopLevelStmts(
                &prog.stmts,
                __arg5,
                __arg6,
                &fnsStore,
            )
        }
    }?;
    {
        let (v, globals) = top;
        {
            let __arg7 @ _ =
                crate::aver_generated::domain::eval::store::withGlobals(fnsStore, globals);
            crate::aver_generated::domain::eval::core::maybeCallMain(&__arg7, v)
        }
    }
}

/// Evaluate a program with additional functions from loaded modules.
pub fn evalProgramWithFns(
    prog @ _: &crate::aver_generated::domain::ast::Program,
    extraFns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let allFns @ _ = {
        let __arg6 @ _ = aver_rt::AverList::concat(&extraFns.clone(), &prog.fns.clone());
        crate::aver_generated::domain::eval::store::fnsToStore(&__arg6)
    };
    let top @ _ = {
        let __arg7 @ _ =
            <aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>>::new();
        {
            let __arg8 @ _ = crate::aver_generated::domain::value::Val::ValUnit;
            crate::aver_generated::domain::eval::core::evalTopLevelStmts(
                &prog.stmts,
                __arg7,
                __arg8,
                &allFns,
            )
        }
    }?;
    {
        let (v, globals) = top;
        {
            let __arg9 @ _ =
                crate::aver_generated::domain::eval::store::withGlobals(allFns, globals);
            crate::aver_generated::domain::eval::core::maybeCallMain(&__arg9, v)
        }
    }
}

/// If a main() function exists, call it. Otherwise return fallback value.
pub fn maybeCallMain(
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    mut fallback @ _: crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::store::lookupFnOption(
        fns,
        ::aver_rt::str_lit!("main"),
    ) {
        Some(fd @ _) => {
            let __arg3 @ _ =
                <aver_rt::AverList<crate::aver_generated::domain::value::Val>>::empty();
            crate::aver_generated::domain::eval::core::callResolved(&fd, &__arg3, fns)
        }
        None => Ok(fallback),
    }
}

/// Recognize immediate slot and literal expressions in slot mode.
pub fn evalImmediateSlotExpr(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Option<Result<crate::aver_generated::domain::value::Val, AverStr>> {
    crate::cancel_checkpoint();
    {
        let __t = expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                Some(crate::aver_generated::domain::eval::slots::lookupSlot(
                    env, slot,
                ))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__n_fields) => {
                let n = __n_fields.0.clone();
                Some(Ok(crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(n),
                )))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFloat(__f_fields) => {
                let f = __f_fields.0.clone();
                Some(Ok(crate::aver_generated::domain::value::Val::ValFloat(
                    crate::aver_generated::domain::value::Val__ValFloat(f),
                )))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprStr(__s_fields) => {
                let s = __s_fields.0.clone();
                Some(Ok(crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(s),
                )))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprBool(__b_fields) => {
                let b = __b_fields.0.clone();
                Some(Ok(crate::aver_generated::domain::value::Val::ValBool(
                    crate::aver_generated::domain::value::Val__ValBool(b),
                )))
            }
            _ => None,
        }
    }
}

/// Resolve unresolved var in slot path: builtins, named function refs, and constructors.
#[inline(always)]
pub fn evalVarSlot(
    name @ _: AverStr,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
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
    op @ _: &crate::aver_generated::domain::ast::BinOp,
    slot @ _: aver_rt::AverInt,
    rhs @ _: aver_rt::AverInt,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let left @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(env, slot)?;
    let __arg5 @ _ = crate::aver_generated::domain::value::Val::ValInt(
        crate::aver_generated::domain::value::Val__ValInt(rhs),
    );
    crate::aver_generated::domain::eval::ops::evalBinopVals(left, __arg5, op)
}

/// Evaluate a specialized slot-vs-slot arithmetic expression.
pub fn evalBinopSlots(
    op @ _: &crate::aver_generated::domain::ast::BinOp,
    lhs @ _: aver_rt::AverInt,
    rhs @ _: aver_rt::AverInt,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let left @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(env, lhs)?;
    let right @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(env, rhs)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(left, right, op)
}

/// Evaluate a specialized slot-vs-int comparison.
pub fn evalCmpSlotInt(
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
    slot @ _: aver_rt::AverInt,
    rhs @ _: aver_rt::AverInt,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let left @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(env, slot)?;
    let __arg5 @ _ = crate::aver_generated::domain::value::Val::ValInt(
        crate::aver_generated::domain::value::Val__ValInt(rhs),
    );
    crate::aver_generated::domain::eval::ops::evalCmpVals(left, __arg5, op)
}

/// Evaluate a specialized slot-vs-slot comparison.
pub fn evalCmpSlots(
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
    lhs @ _: aver_rt::AverInt,
    rhs @ _: aver_rt::AverInt,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let left @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(env, lhs)?;
    let right @ _ = crate::aver_generated::domain::eval::slots::lookupSlot(env, rhs)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(left, right, op)
}

/// Evaluate a fused Vector.get + Option.withDefault in slot path.
pub fn evalVectorGetOrIntExprSlot(
    mut vecExpr @ _: crate::aver_generated::domain::ast::Expr,
    mut idxExpr @ _: crate::aver_generated::domain::ast::Expr,
    defaultValue @ _: aver_rt::AverInt,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(
        vecExpr,
        env.clone(),
        slotMap,
        fns,
    )?;
    let idxV @ _ =
        crate::aver_generated::domain::eval::core::evalExprSlot(idxExpr, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalVectorGetOrIntVals(&vecV, &idxV, defaultValue)
}

/// Evaluate a fused Int.mod + Result.withDefault in slot path.
pub fn evalIntModOrIntExprSlot(
    mut a @ _: crate::aver_generated::domain::ast::Expr,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    defaultValue @ _: aver_rt::AverInt,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let aV @ _ =
        crate::aver_generated::domain::eval::core::evalExprSlot(a, env.clone(), slotMap, fns)?;
    let bV @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(b, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalIntModOrIntVals(&aV, &bV, defaultValue)
}

/// Binary op in slot path.
pub fn evalBinopSlot(
    mut a @ _: crate::aver_generated::domain::ast::Expr,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    op @ _: &crate::aver_generated::domain::ast::BinOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let va @ _ =
        crate::aver_generated::domain::eval::core::evalExprSlot(a, env.clone(), slotMap, fns)?;
    let vb @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(b, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(va, vb, op)
}

/// Unary minus in slot path.
pub fn evalNegSlot(
    mut inner @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(inner, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalNegVals(&v)
}

/// Comparison in slot path.
pub fn evalCmpSlot(
    mut a @ _: crate::aver_generated::domain::ast::Expr,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let va @ _ =
        crate::aver_generated::domain::eval::core::evalExprSlot(a, env.clone(), slotMap, fns)?;
    let vb @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(b, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(va, vb, op)
}

/// String interpolation in slot path.
pub fn evalConcatSlot(
    parts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::core::evalConcatPartsSlot(
        parts,
        env,
        slotMap,
        fns,
        ::aver_rt::str_lit!(""),
    )
}

/// Concatenate interpolation parts in slot path.
pub fn evalConcatPartsSlot(
    parts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    mut acc @ _: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut parts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = parts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = parts;
            if __list_subject.is_empty() {
                return Ok(crate::aver_generated::domain::value::Val::ValStr(
                    crate::aver_generated::domain::value::Val__ValStr(acc),
                ));
            } else {
                if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::aver_generated::domain::eval::core::evalExprSlot(
                        p.clone(),
                        env.clone(),
                        slotMap,
                        fns,
                    ) {
                        Err(e @ _) => {
                            return Err(e);
                        }
                        Ok(v @ _) => {
                            let __tco0 = rest;
                            let __tco4 = (acc + &crate::aver_generated::domain::value::valRepr(&v));
                            __hold0 = __tco0;
                            parts = &__hold0;
                            acc = __tco4;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Tuple literal in slot path.
pub fn evalTupleSlot(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items @ _ =
        crate::aver_generated::domain::eval::core::evalListItemsSlot(exprs, env, slotMap, fns)?;
    Ok(crate::aver_generated::domain::value::Val::ValTuple(
        crate::aver_generated::domain::value::Val__ValTuple(items),
    ))
}

/// List literal in slot path.
pub fn evalListSlot(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items @ _ =
        crate::aver_generated::domain::eval::core::evalListItemsSlot(exprs, env, slotMap, fns)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::value::Val__ValList(items),
    ))
}

/// Evaluate list of expressions in slot path.
pub fn evalListItemsSlot(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    let __arg4 @ _ = <aver_rt::AverList<crate::aver_generated::domain::value::Val>>::empty();
    crate::aver_generated::domain::eval::core::evalListItemsSlotRev(
        exprs, env, slotMap, fns, __arg4,
    )
}

/// Tail-recursive worker for evalListItemsSlot. Accumulates in reverse.
pub fn evalListItemsSlotRev(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return Ok((acc).reverse_owned());
            } else {
                if let (Some(e), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::aver_generated::domain::eval::core::evalExprSlot(
                        e.clone(),
                        env.clone(),
                        slotMap,
                        fns,
                    ) {
                        Err(err @ _) => {
                            return Err(err);
                        }
                        Ok(v @ _) => {
                            let __tco0 = rest;
                            let __tco4 = aver_rt::AverList::prepend_owned(v, acc);
                            __hold0 = __tco0;
                            exprs = &__hold0;
                            acc = __tco4;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Record constructor in slot path.
pub fn evalRecordSlot(
    name @ _: AverStr,
    fieldExprs @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fields @ _ = crate::aver_generated::domain::eval::core::evalRecordFieldsSlot(
        fieldExprs, env, slotMap, fns,
    )?;
    Ok(crate::aver_generated::domain::value::Val::ValRecord(
        crate::aver_generated::domain::value::Val__ValRecord(name, fields),
    ))
}

/// Evaluate record fields in slot path.
pub fn evalRecordFieldsSlot(
    fieldExprs @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = fieldExprs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let (fname, expr) = pair.clone();
                    crate::aver_generated::domain::eval::core::evalRecordFieldOneSlot(
                        fname, expr, &rest, env, slotMap, fns,
                    )
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Evaluate one record field in slot path.
pub fn evalRecordFieldOneSlot(
    fname @ _: AverStr,
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    rest @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<(AverStr, crate::aver_generated::domain::value::Val)>, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ =
        crate::aver_generated::domain::eval::core::evalExprSlot(expr, env.clone(), slotMap, fns)?;
    let restFields @ _ =
        crate::aver_generated::domain::eval::core::evalRecordFieldsSlot(rest, env, slotMap, fns)?;
    Ok(aver_rt::AverList::prepend_owned((fname, v), restFields))
}

/// Field access in slot path.
pub fn evalFieldAccessSlot(
    mut obj @ _: crate::aver_generated::domain::ast::Expr,
    field @ _: AverStr,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(obj, env, slotMap, fns)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValRecord(__fields_fields) => {
                let fields = &__fields_fields.1;
                crate::aver_generated::domain::eval::common::lookupField(fields, field)
            }
            _ => Err(::aver_rt::str_lit!("field access on non-record")),
        }
    }
}

/// Function call in slot path.
pub fn evalCallSlot(
    name @ _: AverStr,
    argExprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let args @ _ =
        crate::aver_generated::domain::eval::core::evalArgsSlot(argExprs, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::core::callWithArgs(&args, name, fns)
}

/// Call a pre-resolved function directly from slot mode using the function store.
pub fn evalCallDirectSlot(
    fnId @ _: aver_rt::AverInt,
    argExprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let fd @ _ = crate::aver_generated::domain::eval::store::lookupFnById(fns, fnId.clone())?;
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
    fnId @ _: aver_rt::AverInt,
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    argExprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv @ _ = {
        let __arg7 @ _ = match aver_rt::checked_vector_size(&(fd.slotCount)) {
            Some(__n) => Ok(aver_rt::AverVector::new(
                __n,
                crate::aver_generated::domain::value::Val::ValUnit,
            )),
            None => Err(aver_rt::AverStr::from(aver_rt::vector_size_error_message())),
        }?;
        crate::aver_generated::domain::eval::core::evalArgsSlotToSlotEnv(
            argExprs,
            env,
            slotMap,
            fns,
            __arg7,
            aver_rt::AverInt::from_i64(0),
        )
    }?;
    crate::aver_generated::domain::eval::core::evalResolvedSlotFn(fnId, fd, calleeEnv, fns)
}

/// Call resolved function by evaluating args directly into a named env from slot caller state.
pub fn evalCallDirectSlotToNamed(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    argExprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv @ _ = {
        let __arg6 @ _ =
            <aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>>::new();
        crate::aver_generated::domain::eval::core::evalArgsSlotToNamedEnv(
            argExprs.clone(),
            fd.params.clone(),
            env,
            slotMap,
            fns,
            __arg6,
        )
    }?;
    crate::aver_generated::domain::eval::core::evalResolvedNamedFn(fd, calleeEnv, fns)
}

/// Call a builtin directly, skipping fns lookup (slot-based path).
pub fn evalCallBuiltinSlot(
    name @ _: AverStr,
    argExprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let args @ _ =
        crate::aver_generated::domain::eval::core::evalArgsSlot(argExprs, env, slotMap, fns)?;
    crate::aver_generated::domain::builtins::callBuiltin(name, &args)
}

/// Evaluate argument expressions in slot path.
pub fn evalArgsSlot(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = exprs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let Some((e0, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_list_match!(rest, [] => crate::aver_generated::domain::eval::core::evalArgsSlot1(e0, env, slotMap, fns), [e1, rest2] => { aver_list_match!(rest2, [] => crate::aver_generated::domain::eval::core::evalArgsSlot2(e0, e1, env, slotMap, fns), [e2, rest3] => { { let __list_subject = rest3; if __list_subject.is_empty() { crate::aver_generated::domain::eval::core::evalArgsSlot3(e0, e1, e2, env, slotMap, fns) } else { { let __arg10 @ _ = <aver_rt::AverList<crate::aver_generated::domain::value::Val>>::empty(); crate::aver_generated::domain::eval::core::evalArgsSlotRev(exprs, &env, slotMap, fns, __arg10) } } } }) })
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Fast path for one slot-path argument.
pub fn evalArgsSlot1(
    mut e0 @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    let v0 @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(e0, env, slotMap, fns)?;
    Ok(aver_rt::AverList::from_vec(vec![v0]))
}

/// Fast path for two slot-path arguments.
pub fn evalArgsSlot2(
    mut e0 @ _: crate::aver_generated::domain::ast::Expr,
    mut e1 @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    let v0 @ _ =
        crate::aver_generated::domain::eval::core::evalExprSlot(e0, env.clone(), slotMap, fns)?;
    let v1 @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(e1, env, slotMap, fns)?;
    Ok(aver_rt::AverList::from_vec(vec![v0, v1]))
}

/// Fast path for three slot-path arguments.
pub fn evalArgsSlot3(
    mut e0 @ _: crate::aver_generated::domain::ast::Expr,
    mut e1 @ _: crate::aver_generated::domain::ast::Expr,
    mut e2 @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    let v0 @ _ =
        crate::aver_generated::domain::eval::core::evalExprSlot(e0, env.clone(), slotMap, fns)?;
    let v1 @ _ =
        crate::aver_generated::domain::eval::core::evalExprSlot(e1, env.clone(), slotMap, fns)?;
    let v2 @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(e2, env, slotMap, fns)?;
    Ok(aver_rt::AverList::from_vec(vec![v0, v1, v2]))
}

/// Tail-recursive worker for evalArgsSlot. Accumulates in reverse.
pub fn evalArgsSlotRev(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return Ok((acc).reverse_owned());
            } else {
                if let (Some(e), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::aver_generated::domain::eval::core::evalExprSlot(
                        e.clone(),
                        env.clone(),
                        slotMap,
                        fns,
                    ) {
                        Err(err @ _) => {
                            return Err(err);
                        }
                        Ok(v @ _) => {
                            let __tco0 = rest;
                            let __tco4 = aver_rt::AverList::prepend_owned(v, acc);
                            __hold0 = __tco0;
                            exprs = &__hold0;
                            acc = __tco4;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Evaluate arg expressions directly into a callee slot env from map caller state.
pub fn evalArgsMapToSlotEnv(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverMap<AverStr, crate::aver_generated::domain::value::Val>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    mut acc @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mut idx @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverVector<crate::aver_generated::domain::value::Val>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return Ok(acc);
            } else {
                if let (Some(e), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::aver_generated::domain::eval::core::evalExpr(
                        e.clone(),
                        env.clone(),
                        fns,
                    ) {
                        Err(err @ _) => {
                            return Err(err);
                        }
                        Ok(v @ _) => {
                            let __tco0 = rest;
                            let __tco3 = crate::aver_generated::domain::eval::slots::setSlot(
                                acc,
                                idx.clone(),
                                &v,
                            );
                            let __tco4 = idx.add(&aver_rt::AverInt::from_i64(1));
                            __hold0 = __tco0;
                            exprs = &__hold0;
                            acc = __tco3;
                            idx = __tco4;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Evaluate arg expressions directly into a callee slot env from slot caller state.
pub fn evalArgsSlotToSlotEnv(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
    mut acc @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mut idx @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverVector<crate::aver_generated::domain::value::Val>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return Ok(acc);
            } else {
                if let (Some(e), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::aver_generated::domain::eval::core::evalExprSlot(
                        e.clone(),
                        env.clone(),
                        slotMap,
                        fns,
                    ) {
                        Err(err @ _) => {
                            return Err(err);
                        }
                        Ok(v @ _) => {
                            let __tco0 = rest;
                            let __tco4 = crate::aver_generated::domain::eval::slots::setSlot(
                                acc,
                                idx.clone(),
                                &v,
                            );
                            let __tco5 = idx.add(&aver_rt::AverInt::from_i64(1));
                            __hold0 = __tco0;
                            exprs = &__hold0;
                            acc = __tco4;
                            idx = __tco5;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Evaluate ? operator in slot path.
pub fn evalPropagateSlot(
    mut inner @ _: crate::aver_generated::domain::ast::Expr,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::eval::core::evalExprSlot(inner, env, slotMap, fns)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValOk(__x_fields) => {
                let x = &__x_fields.0;
                Ok(x.clone())
            }
            crate::aver_generated::domain::value::Val__view::ValErr(__e_fields) => {
                let e = &__e_fields.0;
                {
                    let __t = e;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValStr(__msg_fields) => {
                            let msg = __msg_fields.0.clone();
                            Err(
                                crate::aver_generated::domain::eval::common::wrapPropagatedError(
                                    msg,
                                ),
                            )
                        }
                        _ => Err(
                            crate::aver_generated::domain::eval::common::wrapPropagatedError(
                                ::aver_rt::str_lit!("propagated error"),
                            ),
                        ),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("? operator requires Result value")),
        }
    }
}

/// Evaluate independent product in slot path using the self-host's own divide-and-conquer ?!, then unwrap guest Results if needed.
pub fn evalIndependentProductSlot(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    unwrap @ _: bool,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items @ _ = crate::aver_generated::domain::eval::core::evalIndependentItemsSlot(
        exprs, env, slotMap, fns,
    )?;
    if unwrap {
        {
            let __arg6 @ _ =
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
            crate::aver_generated::domain::eval::core::unwrapProductResults__collected(
                &items, __arg6,
            )
        }
    } else {
        Ok(crate::aver_generated::domain::value::Val::ValTuple(
            crate::aver_generated::domain::value::Val__ValTuple(items),
        ))
    }
}

/// Evaluate guest independent-product branches in slot path through a balanced tree of the self-host's own ?!.
pub fn evalIndependentItemsSlot(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    fns @ _: &crate::aver_generated::domain::eval::store::FnStore,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = exprs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_list_match!(rest, [] => match crate::aver_generated::domain::eval::core::evalExprSlot(a.clone(), env, slotMap, fns) { Ok(va @ _) => { Ok(aver_rt::AverList::from_vec(vec![va])) }, Err(e @ _) => { Err(e) } }, [b, rest2] => { { let __list_subject = rest2; if __list_subject.is_empty() { { let (va, vb) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let __r0 = crate::aver_generated::domain::eval::core::evalExprSlot(a.clone(), env.clone(), slotMap, fns); crate::aver_replay::set_effect_branch(1); let __r1 = crate::aver_generated::domain::eval::core::evalExprSlot(b, env, slotMap, fns); crate::aver_replay::exit_effect_group(); match (__r0, __r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (__r0, __r1) => { if let Err(__err) = __r0 { Err(__err) } else if let Err(__err) = __r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|__s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let __h0 = { let env = env.clone(); let slotMap = slotMap; let fns = fns; let a = a; aver_rt::spawn_scoped(__s, move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalExprSlot(a.clone(), env.clone(), slotMap, fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let __h1 = { let env = env; let slotMap = slotMap; let fns = fns; let b = b; aver_rt::spawn_scoped(__s, move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalExprSlot(b, env, slotMap, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __b0 = __h0.join().unwrap(); let __b1 = __h1.join().unwrap(); match (__b0, __b1) { (crate::ParallelBranch::Completed(__r0), crate::ParallelBranch::Completed(__r1)) => match (__r0, __r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (__r0, __r1) => { if let Err(__err) = __r0 { Err(__err) } else if let Err(__err) = __r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (__b0, __b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = __b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = __b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok(aver_rt::AverList::from_vec(vec![va, vb])) } } else { { let (leftExprs, rightExprs) = { let __arg16 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::Expr>>::empty(); { let __arg17 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::Expr>>::empty(); crate::aver_generated::domain::eval::core::splitIndependentExprs(exprs, __arg16, __arg17, true) } }; { let (leftVals, rightVals) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let __r0 = crate::aver_generated::domain::eval::core::evalIndependentItemsSlot(&leftExprs, env.clone(), slotMap, fns); crate::aver_replay::set_effect_branch(1); let __r1 = crate::aver_generated::domain::eval::core::evalIndependentItemsSlot(&rightExprs, env, slotMap, fns); crate::aver_replay::exit_effect_group(); match (__r0, __r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (__r0, __r1) => { if let Err(__err) = __r0 { Err(__err) } else if let Err(__err) = __r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|__s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let __h0 = { let env = env.clone(); let slotMap = slotMap; let fns = fns; let leftExprs = leftExprs; aver_rt::spawn_scoped(__s, move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalIndependentItemsSlot(&leftExprs, env.clone(), slotMap, fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let __h1 = { let env = env; let slotMap = slotMap; let fns = fns; let rightExprs = rightExprs; aver_rt::spawn_scoped(__s, move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = crate::aver_generated::domain::eval::core::evalIndependentItemsSlot(&rightExprs, env, slotMap, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })) }; let __b0 = __h0.join().unwrap(); let __b1 = __h1.join().unwrap(); match (__b0, __b1) { (crate::ParallelBranch::Completed(__r0), crate::ParallelBranch::Completed(__r1)) => match (__r0, __r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (__r0, __r1) => { if let Err(__err) = __r0 { Err(__err) } else if let Err(__err) = __r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (__b0, __b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = __b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = __b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok({ let __arg18 @ _ = <aver_rt::AverList<crate::aver_generated::domain::value::Val>>::empty(); crate::aver_generated::domain::eval::core::interleaveIndependentVals(&leftVals, &rightVals, true, __arg18) }) } } } } })
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Synthesized collecting variant of `unwrapProductResults`. Appends to a builder where `unwrapProductResults` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn unwrapProductResults__collected(
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = items;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = items;
            if __list_subject.is_empty() {
                return Ok(crate::aver_generated::domain::value::Val::ValTuple(
                    crate::aver_generated::domain::value::Val__ValTuple(
                        aver_rt::list_builder_finalize(acc),
                    ),
                ));
            } else {
                if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __t = v;
                        match __t.view() {
                            crate::aver_generated::domain::value::Val__view::ValOk(__x_fields) => {
                                let x = &__x_fields.0;
                                {
                                    let __tco0 = rest;
                                    let __tco1 = aver_rt::list_builder_push(acc, x.clone());
                                    __hold0 = __tco0;
                                    items = &__hold0;
                                    acc = __tco1;
                                    continue;
                                }
                            }
                            crate::aver_generated::domain::value::Val__view::ValErr(__e_fields) => {
                                let e = &__e_fields.0;
                                {
                                    let __t = e;
                                    match __t.view() {
                                        crate::aver_generated::domain::value::Val__view::ValStr(
                                            __msg_fields,
                                        ) => {
                                            let msg = __msg_fields.0.clone();
                                            return Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(msg));
                                        }
                                        _ => {
                                            return Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(::aver_rt::str_lit!("propagated error")));
                                        }
                                    }
                                }
                            }
                            _ => {
                                return Err(::aver_rt::str_lit!(
                                    "?! operator requires all elements to be Result values"
                                ));
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Synthesized collecting variant of `collectFastForwardArgs`. Appends to a builder where `collectFastForwardArgs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn collectFastForwardArgs__collected(
    slotArgs @ _: &aver_rt::AverIntList,
    calleeEnv @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<aver_rt::AverList<crate::aver_generated::domain::value::Val>, AverStr> {
    let mut __hold0: aver_rt::AverIntList;
    let mut slotArgs @ _: &aver_rt::AverIntList = slotArgs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = slotArgs;
            if __list_subject.is_empty() {
                return Ok(aver_rt::list_builder_finalize(acc));
            } else {
                if let Some((slot, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    match crate::aver_generated::domain::eval::slots::lookupSlot(calleeEnv, slot) {
                        Ok(v @ _) => {
                            let __tco0 = rest;
                            let __tco2 = aver_rt::list_builder_push(acc, v);
                            __hold0 = __tco0;
                            slotArgs = &__hold0;
                            acc = __tco2;
                            continue;
                        }
                        Err(e @ _) => {
                            return Err(e);
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}
