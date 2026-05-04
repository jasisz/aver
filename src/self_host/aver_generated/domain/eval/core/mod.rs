#[allow(unused_imports)]
use crate::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::match_mod::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::store::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::common::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::records::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::slots::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::fast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::ops::*;

#[derive(Clone, Debug, PartialEq)]
pub enum SlotTailStep {
    SlotTailDone(Val),
    SlotTailRecurEnv(aver_rt::AverVector<Val>),
}

impl aver_rt::AverDisplay for SlotTailStep {
    fn aver_display(&self) -> String {
        match self {
            SlotTailStep::SlotTailDone(f0) => format!("SlotTailDone({})", f0.aver_display_inner()),
            SlotTailStep::SlotTailRecurEnv(f0) => format!("SlotTailRecurEnv({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String { self.aver_display() }
}

impl aver_replay::ReplayValue for SlotTailStep {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert("type".to_string(), serde_json::Value::String("SlotTailStep".to_string()));
        match self {
            SlotTailStep::SlotTailDone(f0) => {
                payload.insert("name".to_string(), serde_json::Value::String("SlotTailDone".to_string()));
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            SlotTailStep::SlotTailRecurEnv(f0) => {
                payload.insert("name".to_string(), serde_json::Value::String("SlotTailRecurEnv".to_string()));
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type").ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "SlotTailStep" {
            return Err(format!("$variant type mismatch: expected SlotTailStep, got {}", type_name));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name").ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields").ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "SlotTailDone" => Ok(SlotTailStep::SlotTailDone(<Val as ReplayValue>::from_replay_json(fields.get(0).ok_or_else(|| format!("$variant SlotTailDone missing field #{}", 0))?)?)),
            "SlotTailRecurEnv" => Ok(SlotTailStep::SlotTailRecurEnv(<aver_rt::AverVector<Val> as ReplayValue>::from_replay_json(fields.get(0).ok_or_else(|| format!("$variant SlotTailRecurEnv missing field #{}", 0))?)?)),
            _ => Err(format!("unknown variant '{}' for SlotTailStep", variant_name)),
        }
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    EvalArgsMapToNamedEnv(aver_rt::AverList<Expr>, aver_rt::AverList<AverStr>, aver_rt::AverMap<AverStr, Val>, FnStore, aver_rt::AverMap<AverStr, Val>),
    EvalArgsMapToNamedEnvBind(aver_rt::AverList<Expr>, aver_rt::AverList<AverStr>, aver_rt::AverMap<AverStr, Val>, FnStore, aver_rt::AverMap<AverStr, Val>, Val),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> Result<aver_rt::AverMap<AverStr, Val>, AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::EvalArgsMapToNamedEnv(mut exprs, mut params, mut env, mut fns, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(exprs, [] => { return Ok(acc) }, [e, restExprs] => { match evalExpr(&e, &env, &fns) { Err(err) => { return Err(err) }, Ok(v) => { __MutualTco1::EvalArgsMapToNamedEnvBind(restExprs, params, env, fns, acc, v) } } })
            }
            __MutualTco1::EvalArgsMapToNamedEnvBind(mut restExprs, mut params, mut env, mut fns, mut acc, mut v) => {
                crate::cancel_checkpoint();
                aver_list_match!(params, [] => __MutualTco1::EvalArgsMapToNamedEnv(restExprs, aver_rt::AverList::empty(), env, fns, acc), [param, restParams] => __MutualTco1::EvalArgsMapToNamedEnv(restExprs, restParams, env, fns, acc.insert_owned(param, v)))
            }
        };
    }
}

/// Evaluate arg expressions directly into a callee named env from map caller state.
pub fn evalArgsMapToNamedEnv(exprs: &aver_rt::AverList<Expr>, params: &aver_rt::AverList<AverStr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore, acc: &aver_rt::AverMap<AverStr, Val>) -> Result<aver_rt::AverMap<AverStr, Val>, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalArgsMapToNamedEnv(exprs.clone(), params.clone(), env.clone(), fns.clone(), acc.clone()))
}

/// Bind one evaluated arg into the callee named env when a parameter exists.
pub fn evalArgsMapToNamedEnvBind(restExprs: &aver_rt::AverList<Expr>, params: &aver_rt::AverList<AverStr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore, acc: &aver_rt::AverMap<AverStr, Val>, v: &Val) -> Result<aver_rt::AverMap<AverStr, Val>, AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::EvalArgsMapToNamedEnvBind(restExprs.clone(), params.clone(), env.clone(), fns.clone(), acc.clone(), v.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    EvalArgsSlotToNamedEnv(aver_rt::AverList<Expr>, aver_rt::AverList<AverStr>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore, aver_rt::AverMap<AverStr, Val>),
    EvalArgsSlotToNamedEnvBind(aver_rt::AverList<Expr>, aver_rt::AverList<AverStr>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore, aver_rt::AverMap<AverStr, Val>, Val),
}

fn __mutual_tco_trampoline_2(mut __state: __MutualTco2) -> Result<aver_rt::AverMap<AverStr, Val>, AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::EvalArgsSlotToNamedEnv(mut exprs, mut params, mut env, mut slotMap, mut fns, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(exprs, [] => { return Ok(acc) }, [e, restExprs] => { match evalExprSlot(&e, &env, &slotMap, &fns) { Err(err) => { return Err(err) }, Ok(v) => { __MutualTco2::EvalArgsSlotToNamedEnvBind(restExprs, params, env, slotMap, fns, acc, v) } } })
            }
            __MutualTco2::EvalArgsSlotToNamedEnvBind(mut restExprs, mut params, mut env, mut slotMap, mut fns, mut acc, mut v) => {
                crate::cancel_checkpoint();
                aver_list_match!(params, [] => __MutualTco2::EvalArgsSlotToNamedEnv(restExprs, aver_rt::AverList::empty(), env, slotMap, fns, acc), [param, restParams] => __MutualTco2::EvalArgsSlotToNamedEnv(restExprs, restParams, env, slotMap, fns, acc.insert_owned(param, v)))
            }
        };
    }
}

/// Evaluate arg expressions directly into a callee named env from slot caller state.
pub fn evalArgsSlotToNamedEnv(exprs: &aver_rt::AverList<Expr>, params: &aver_rt::AverList<AverStr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore, acc: &aver_rt::AverMap<AverStr, Val>) -> Result<aver_rt::AverMap<AverStr, Val>, AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::EvalArgsSlotToNamedEnv(exprs.clone(), params.clone(), env.clone(), slotMap.clone(), fns.clone(), acc.clone()))
}

/// Bind one evaluated arg into the callee named env when a parameter exists.
pub fn evalArgsSlotToNamedEnvBind(restExprs: &aver_rt::AverList<Expr>, params: &aver_rt::AverList<AverStr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore, acc: &aver_rt::AverMap<AverStr, Val>, v: &Val) -> Result<aver_rt::AverMap<AverStr, Val>, AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::EvalArgsSlotToNamedEnvBind(restExprs.clone(), params.clone(), env.clone(), slotMap.clone(), fns.clone(), acc.clone(), v.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    EvalExpr(Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalExprBasic(Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalExprInternal(Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalExprAggregate(Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalExprCalls(Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalBoolBranch(Expr, Expr, Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalMatchExpr(Expr, aver_rt::AverList<MatchArm>, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalCallBuiltinById(i64, aver_rt::AverList<Expr>, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalCallBuiltinMaybeSpecial(AverStr, aver_rt::AverList<Expr>, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalOptionWithDefaultExpr(aver_rt::AverList<Expr>, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalOptionWithDefaultExprInner(Expr, Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalVectorSetWithDefaultExpr(aver_rt::AverList<Expr>, Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalVectorSetWithDefaultExprValues(Expr, Expr, Expr, Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalVectorSetWithDefaultExprResult(Val, Val, Val, Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalVectorGetWithDefaultExpr(aver_rt::AverList<Expr>, Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalVectorGetWithDefaultExprValues(Expr, Expr, Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalVectorGetWithDefaultExprResult(Val, Val, Expr, aver_rt::AverMap<AverStr, Val>, FnStore),
    EvalMatch(Val, aver_rt::AverList<MatchArm>, aver_rt::AverMap<AverStr, Val>, FnStore),
}

fn __mutual_tco_trampoline_3(mut __state: __MutualTco3) -> Result<Val, AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::EvalExpr(mut expr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match evalImmediateNamedExpr(&expr) { Some(v) => { return Ok(v) }, None => { __MutualTco3::EvalExprBasic(expr, env, fns) } }
            }
            __MutualTco3::EvalExprBasic(mut expr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                Expr::ExprBoolBranch(cond, thenExpr, elseExpr) => { let cond = (*cond).clone(); let thenExpr = (*thenExpr).clone(); let elseExpr = (*elseExpr).clone(); __MutualTco3::EvalBoolBranch(cond, thenExpr, elseExpr, env, fns) },
                Expr::ExprVar(name) => return evalVar(name, &env, &fns),
                Expr::ExprSlot(_) => return Err(AverStr::from("ExprSlot in map-based eval path")),
                Expr::ExprBinopSlotInt(_, _, _) => return Err(AverStr::from("ExprBinopSlotInt in map-based eval path")),
                Expr::ExprBinopSlots(_, _, _) => return Err(AverStr::from("ExprBinopSlots in map-based eval path")),
                _ => __MutualTco3::EvalExprInternal(expr, env, fns)
                }
            }
            __MutualTco3::EvalExprInternal(mut expr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                Expr::ExprCmpSlotInt(_, _, _) => return Err(AverStr::from("ExprCmpSlotInt in map-based eval path")),
                Expr::ExprCmpSlots(_, _, _) => return Err(AverStr::from("ExprCmpSlots in map-based eval path")),
                Expr::ExprVectorGetOrInt(vecExpr, idxExpr, defaultValue) => { let vecExpr = (*vecExpr).clone(); let idxExpr = (*idxExpr).clone(); return evalVectorGetOrIntExpr(&vecExpr, &idxExpr, defaultValue, &env, &fns) },
                Expr::ExprIntModOrInt(a, b, defaultValue) => { let a = (*a).clone(); let b = (*b).clone(); return evalIntModOrIntExpr(&a, &b, defaultValue, &env, &fns) },
                Expr::ExprAdd(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalBinop(&a, &b, &env, &fns, &BinOp::OpAdd) },
                Expr::ExprSub(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalBinop(&a, &b, &env, &fns, &BinOp::OpSub) },
                Expr::ExprMul(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalBinop(&a, &b, &env, &fns, &BinOp::OpMul) },
                Expr::ExprDiv(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalBinop(&a, &b, &env, &fns, &BinOp::OpDiv) },
                Expr::ExprEq(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmp(&a, &b, &env, &fns, &CmpOp::CmpEq) },
                Expr::ExprNeq(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmp(&a, &b, &env, &fns, &CmpOp::CmpNeq) },
                _ => __MutualTco3::EvalExprAggregate(expr, env, fns)
                }
            }
            __MutualTco3::EvalExprAggregate(mut expr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                Expr::ExprLt(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmp(&a, &b, &env, &fns, &CmpOp::CmpLt) },
                Expr::ExprGt(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmp(&a, &b, &env, &fns, &CmpOp::CmpGt) },
                Expr::ExprLte(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmp(&a, &b, &env, &fns, &CmpOp::CmpLte) },
                Expr::ExprGte(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmp(&a, &b, &env, &fns, &CmpOp::CmpGte) },
                Expr::ExprConcat(parts) => return evalConcatExpr(&parts, &env, &fns),
                Expr::ExprTuple(exprs) => return evalTupleExpr(&exprs, &env, &fns),
                Expr::ExprList(exprs) => return evalListExpr(&exprs, &env, &fns),
                Expr::ExprRecord(name, fieldExprs) => return evalRecordExpr(name, &fieldExprs, &env, &fns),
                Expr::ExprFieldAccess(obj, field) => { let obj = (*obj).clone(); return evalFieldAccess(&obj, field, &env, &fns) },
                Expr::ExprCall(name, argExprs) => return evalCall(name, &argExprs, &env, &fns),
                _ => __MutualTco3::EvalExprCalls(expr, env, fns)
                }
            }
            __MutualTco3::EvalExprCalls(mut expr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                Expr::ExprCallDirect(fnId, argExprs) => return evalCallDirect(fnId, &argExprs, &env, &fns),
                Expr::ExprCallBuiltin(name, argExprs) => __MutualTco3::EvalCallBuiltinMaybeSpecial(name, argExprs, env, fns),
                Expr::ExprCallBuiltinId(id, argExprs) => __MutualTco3::EvalCallBuiltinById(id, argExprs, env, fns),
                Expr::ExprMatch(scrutinee, arms) => { let scrutinee = (*scrutinee).clone(); __MutualTco3::EvalMatchExpr(scrutinee, arms, env, fns) },
                Expr::ExprPropagate(inner) => { let inner = (*inner).clone(); return evalPropagate(&inner, &env, &fns) },
                Expr::ExprIndependentProduct(exprs, unwrap) => return evalIndependentProduct(&exprs, unwrap, &env, &fns),
                _ => return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((50i64) as usize); __b.push_str(&AverStr::from("unsupported named-env expression: ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(expr)))); __b }))
                }
            }
            __MutualTco3::EvalBoolBranch(mut cond, mut thenExpr, mut elseExpr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                let condV = evalExpr(&cond, &env, &fns)?;
                match condV.clone() {
                Val::ValBool(flag) => if flag { __MutualTco3::EvalExpr(thenExpr, env, fns) } else { __MutualTco3::EvalExpr(elseExpr, env, fns) },
                _ => return Err((AverStr::from("bool branch condition must evaluate to Bool, got: ") + &crate::aver_generated::domain::value::valRepr(&condV)))
                }
            }
            __MutualTco3::EvalMatchExpr(mut scrutinee, mut arms, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match evalExpr(&scrutinee, &env, &fns) { Ok(v) => { __MutualTco3::EvalMatch(v, arms, env, fns) }, Err(e) => { return Err(e) } }
            }
            __MutualTco3::EvalCallBuiltinById(mut id, mut argExprs, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match id {
                15i64 => __MutualTco3::EvalOptionWithDefaultExpr(argExprs, env, fns),
                _ => match evalArgs(&argExprs, &env, &fns) { Err(e) => { return Err(e) }, Ok(args) => { return crate::aver_generated::domain::builtins::callBuiltinByIdValues(id, &args) } }
                }
            }
            __MutualTco3::EvalCallBuiltinMaybeSpecial(mut name, mut argExprs, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match &*name.clone() {
                "Option.withDefault" => __MutualTco3::EvalOptionWithDefaultExpr(argExprs, env, fns),
                _ => return evalCallBuiltin(name, &argExprs, &env, &fns)
                }
            }
            __MutualTco3::EvalOptionWithDefaultExpr(mut argExprs, mut env, mut fns) => {
                crate::cancel_checkpoint();
                { let __list_subject = argExprs.clone(); if let Some((optionExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = rest; if let Some((defaultExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) { __MutualTco3::EvalOptionWithDefaultExprInner(optionExpr, defaultExpr, env, fns) } else { return evalCallBuiltin(AverStr::from("Option.withDefault"), &argExprs, &env, &fns) } } } else { return evalCallBuiltin(AverStr::from("Option.withDefault"), &argExprs, &env, &fns) } }
            }
            __MutualTco3::EvalOptionWithDefaultExprInner(mut optionExpr, mut defaultExpr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match optionExpr.clone() {
                Expr::ExprCallBuiltin(name, vecArgs) => { let __dispatch_subject = name; if &*__dispatch_subject == "Vector.set" { __MutualTco3::EvalVectorSetWithDefaultExpr(vecArgs, defaultExpr, env, fns) } else { if &*__dispatch_subject == "Vector.get" { __MutualTco3::EvalVectorGetWithDefaultExpr(vecArgs, defaultExpr, env, fns) } else { return evalCallBuiltin(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]), &env, &fns) } } },
                _ => return evalCallBuiltin(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]), &env, &fns)
                }
            }
            __MutualTco3::EvalVectorSetWithDefaultExpr(mut vecArgs, mut defaultExpr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                { let __list_subject = vecArgs.clone(); if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = r1; if let Some((idxExpr, r2)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = r2; if let Some((valueExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) { __MutualTco3::EvalVectorSetWithDefaultExprValues(vecExpr, idxExpr, valueExpr, defaultExpr, env, fns) } else { return evalCallBuiltin(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.set"), vecArgs), defaultExpr]), &env, &fns) } } } else { return evalCallBuiltin(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.set"), vecArgs), defaultExpr]), &env, &fns) } } } else { return evalCallBuiltin(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.set"), vecArgs), defaultExpr]), &env, &fns) } }
            }
            __MutualTco3::EvalVectorSetWithDefaultExprValues(mut vecExpr, mut idxExpr, mut valueExpr, mut defaultExpr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                let vecV = evalExpr(&vecExpr, &env, &fns)?;
                let idxV = evalExpr(&idxExpr, &env, &fns)?;
                let valueV = evalExpr(&valueExpr, &env, &fns)?;
                __MutualTco3::EvalVectorSetWithDefaultExprResult(vecV, idxV, valueV, defaultExpr, env, fns)
            }
            __MutualTco3::EvalVectorSetWithDefaultExprResult(mut vecV, mut idxV, mut valueV, mut defaultExpr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::ops::evalVectorSetMaybeDefault(&vecV, &idxV, &valueV) { Ok(maybeV) => { match maybeV { Some(v) => { return Ok(v) }, None => { __MutualTco3::EvalExpr(defaultExpr, env, fns) } } }, Err(err) => { return Err(err) } }
            }
            __MutualTco3::EvalVectorGetWithDefaultExpr(mut vecArgs, mut defaultExpr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                { let __list_subject = vecArgs.clone(); if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = r1; if let Some((idxExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) { __MutualTco3::EvalVectorGetWithDefaultExprValues(vecExpr, idxExpr, defaultExpr, env, fns) } else { return evalCallBuiltin(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.get"), vecArgs), defaultExpr]), &env, &fns) } } } else { return evalCallBuiltin(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.get"), vecArgs), defaultExpr]), &env, &fns) } }
            }
            __MutualTco3::EvalVectorGetWithDefaultExprValues(mut vecExpr, mut idxExpr, mut defaultExpr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                let vecV = evalExpr(&vecExpr, &env, &fns)?;
                let idxV = evalExpr(&idxExpr, &env, &fns)?;
                __MutualTco3::EvalVectorGetWithDefaultExprResult(vecV, idxV, defaultExpr, env, fns)
            }
            __MutualTco3::EvalVectorGetWithDefaultExprResult(mut vecV, mut idxV, mut defaultExpr, mut env, mut fns) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::ops::evalVectorGetMaybeDefault(&vecV, &idxV) { Ok(maybeV) => { match maybeV { Some(v) => { return Ok(v) }, None => { __MutualTco3::EvalExpr(defaultExpr, env, fns) } } }, Err(err) => { return Err(err) } }
            }
            __MutualTco3::EvalMatch(mut v, mut arms, mut env, mut fns) => {
                crate::cancel_checkpoint();
                aver_list_match!(arms, [] => { return Err(AverStr::from("no matching arm")) }, [arm, rest] => match crate::aver_generated::domain::match_mod::matchPattern(&arm.pattern, &v) { Ok(bindings) => { __MutualTco3::EvalExpr(arm.body.clone(), crate::aver_generated::domain::eval::store::mergeBindings(bindings, env), fns) }, Err(_) => { __MutualTco3::EvalMatch(v, rest, env, fns) } })
            }
        };
    }
}

/// Evaluate an expression in the given environment.
pub fn evalExpr(expr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalExpr(expr.clone(), env.clone(), fns.clone()))
}

/// Continue named-env evaluation for branch, vars, and slot-only internal nodes.
pub fn evalExprBasic(expr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalExprBasic(expr.clone(), env.clone(), fns.clone()))
}

/// Continue named-env expression evaluation for comparisons and arithmetic.
pub fn evalExprInternal(expr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalExprInternal(expr.clone(), env.clone(), fns.clone()))
}

/// Continue named-env evaluation for aggregate expression forms.
pub fn evalExprAggregate(expr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalExprAggregate(expr.clone(), env.clone(), fns.clone()))
}

/// Finish named-env evaluation for calls, matches, propagation, and products.
pub fn evalExprCalls(expr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalExprCalls(expr.clone(), env.clone(), fns.clone()))
}

/// Evaluate a direct bool branch in map path.
pub fn evalBoolBranch(cond: &Expr, thenExpr: &Expr, elseExpr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalBoolBranch(cond.clone(), thenExpr.clone(), elseExpr.clone(), env.clone(), fns.clone()))
}

/// Evaluate a match expression.
pub fn evalMatchExpr(scrutinee: &Expr, arms: &aver_rt::AverList<MatchArm>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalMatchExpr(scrutinee.clone(), arms.clone(), env.clone(), fns.clone()))
}

/// Builtin dispatch by integer ID — no string comparison.
pub fn evalCallBuiltinById(id: i64, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalCallBuiltinById(id, argExprs.clone(), env.clone(), fns.clone()))
}

/// Builtin dispatch with fast-path peepholes for hot patterns.
pub fn evalCallBuiltinMaybeSpecial(name: AverStr, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalCallBuiltinMaybeSpecial(name, argExprs.clone(), env.clone(), fns.clone()))
}

/// Specialize hot Option.withDefault(Vector.get/Vector.set, default) patterns in map path.
pub fn evalOptionWithDefaultExpr(argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalOptionWithDefaultExpr(argExprs.clone(), env.clone(), fns.clone()))
}

/// Dispatch specialized Option.withDefault cases in map path.
pub fn evalOptionWithDefaultExprInner(optionExpr: &Expr, defaultExpr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalOptionWithDefaultExprInner(optionExpr.clone(), defaultExpr.clone(), env.clone(), fns.clone()))
}

/// Specialized Vector.set + Option.withDefault in map path.
pub fn evalVectorSetWithDefaultExpr(vecArgs: &aver_rt::AverList<Expr>, defaultExpr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalVectorSetWithDefaultExpr(vecArgs.clone(), defaultExpr.clone(), env.clone(), fns.clone()))
}

/// Evaluate Vector.set operands in map path and defer the default expression until needed.
pub fn evalVectorSetWithDefaultExprValues(vecExpr: &Expr, idxExpr: &Expr, valueExpr: &Expr, defaultExpr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalVectorSetWithDefaultExprValues(vecExpr.clone(), idxExpr.clone(), valueExpr.clone(), defaultExpr.clone(), env.clone(), fns.clone()))
}

/// Finish specialized Vector.set + Option.withDefault in map path with lazy default evaluation.
pub fn evalVectorSetWithDefaultExprResult(vecV: &Val, idxV: &Val, valueV: &Val, defaultExpr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalVectorSetWithDefaultExprResult(vecV.clone(), idxV.clone(), valueV.clone(), defaultExpr.clone(), env.clone(), fns.clone()))
}

/// Specialized Vector.get + Option.withDefault in map path.
pub fn evalVectorGetWithDefaultExpr(vecArgs: &aver_rt::AverList<Expr>, defaultExpr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalVectorGetWithDefaultExpr(vecArgs.clone(), defaultExpr.clone(), env.clone(), fns.clone()))
}

/// Evaluate Vector.get operands in map path and defer the default expression until needed.
pub fn evalVectorGetWithDefaultExprValues(vecExpr: &Expr, idxExpr: &Expr, defaultExpr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalVectorGetWithDefaultExprValues(vecExpr.clone(), idxExpr.clone(), defaultExpr.clone(), env.clone(), fns.clone()))
}

/// Finish specialized Vector.get + Option.withDefault in map path with lazy default evaluation.
pub fn evalVectorGetWithDefaultExprResult(vecV: &Val, idxV: &Val, defaultExpr: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalVectorGetWithDefaultExprResult(vecV.clone(), idxV.clone(), defaultExpr.clone(), env.clone(), fns.clone()))
}

/// Try each match arm until one matches. Self-recursive for codegen TCO.
pub fn evalMatch(v: &Val, arms: &aver_rt::AverList<MatchArm>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::EvalMatch(v.clone(), arms.clone(), env.clone(), fns.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco4 {
    EvalExprSlot(Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalExprSlotBasic(Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalExprSlotInternal(Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalExprSlotAggregate(Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalExprSlotCalls(Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalBoolBranchSlot(Expr, Expr, Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalCallBuiltinByIdSlot(i64, aver_rt::AverList<Expr>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalCallBuiltinSlotMaybeSpecial(AverStr, aver_rt::AverList<Expr>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalOptionWithDefaultExprSlot(aver_rt::AverList<Expr>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalOptionWithDefaultExprSlotInner(Expr, Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalVectorSetWithDefaultExprSlot(aver_rt::AverList<Expr>, Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalVectorSetWithDefaultExprSlotValues(Expr, Expr, Expr, Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalVectorSetWithDefaultExprSlotResult(Val, Val, Val, Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalVectorGetWithDefaultExprSlot(aver_rt::AverList<Expr>, Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalVectorGetWithDefaultExprSlotValues(Expr, Expr, Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalVectorGetWithDefaultExprSlotResult(Val, Val, Expr, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalMatchExprSlot(Expr, aver_rt::AverList<MatchArm>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalMatchSlot(Val, aver_rt::AverList<MatchArm>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
}

fn __mutual_tco_trampoline_4(mut __state: __MutualTco4) -> Result<Val, AverStr> {
    loop {
        __state = match __state {
            __MutualTco4::EvalExprSlot(mut expr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match evalImmediateSlotExpr(&expr, &env) { Some(result) => { return result }, None => { __MutualTco4::EvalExprSlotBasic(expr, env, slotMap, fns) } }
            }
            __MutualTco4::EvalExprSlotBasic(mut expr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                Expr::ExprBoolBranch(cond, thenExpr, elseExpr) => { let cond = (*cond).clone(); let thenExpr = (*thenExpr).clone(); let elseExpr = (*elseExpr).clone(); __MutualTco4::EvalBoolBranchSlot(cond, thenExpr, elseExpr, env, slotMap, fns) },
                Expr::ExprVar(name) => return evalVarSlot(name, &fns),
                Expr::ExprBinopSlotInt(op, slot, rhs) => return evalBinopSlotInt(&op, slot, rhs, &env),
                Expr::ExprBinopSlots(op, lhs, rhs) => return evalBinopSlots(&op, lhs, rhs, &env),
                Expr::ExprCmpSlotInt(op, slot, rhs) => return evalCmpSlotInt(&op, slot, rhs, &env),
                _ => __MutualTco4::EvalExprSlotInternal(expr, env, slotMap, fns)
                }
            }
            __MutualTco4::EvalExprSlotInternal(mut expr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                Expr::ExprCmpSlots(op, lhs, rhs) => return evalCmpSlots(&op, lhs, rhs, &env),
                Expr::ExprVectorGetOrInt(vecExpr, idxExpr, defaultValue) => { let vecExpr = (*vecExpr).clone(); let idxExpr = (*idxExpr).clone(); return evalVectorGetOrIntExprSlot(&vecExpr, &idxExpr, defaultValue, &env, &slotMap, &fns) },
                Expr::ExprIntModOrInt(a, b, defaultValue) => { let a = (*a).clone(); let b = (*b).clone(); return evalIntModOrIntExprSlot(&a, &b, defaultValue, &env, &slotMap, &fns) },
                Expr::ExprAdd(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalBinopSlot(&a, &b, &env, &slotMap, &fns, &BinOp::OpAdd) },
                Expr::ExprSub(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalBinopSlot(&a, &b, &env, &slotMap, &fns, &BinOp::OpSub) },
                Expr::ExprMul(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalBinopSlot(&a, &b, &env, &slotMap, &fns, &BinOp::OpMul) },
                Expr::ExprDiv(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalBinopSlot(&a, &b, &env, &slotMap, &fns, &BinOp::OpDiv) },
                Expr::ExprEq(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmpSlot(&a, &b, &env, &slotMap, &fns, &CmpOp::CmpEq) },
                Expr::ExprNeq(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmpSlot(&a, &b, &env, &slotMap, &fns, &CmpOp::CmpNeq) },
                _ => __MutualTco4::EvalExprSlotAggregate(expr, env, slotMap, fns)
                }
            }
            __MutualTco4::EvalExprSlotAggregate(mut expr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                Expr::ExprLt(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmpSlot(&a, &b, &env, &slotMap, &fns, &CmpOp::CmpLt) },
                Expr::ExprGt(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmpSlot(&a, &b, &env, &slotMap, &fns, &CmpOp::CmpGt) },
                Expr::ExprLte(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmpSlot(&a, &b, &env, &slotMap, &fns, &CmpOp::CmpLte) },
                Expr::ExprGte(a, b) => { let a = (*a).clone(); let b = (*b).clone(); return evalCmpSlot(&a, &b, &env, &slotMap, &fns, &CmpOp::CmpGte) },
                Expr::ExprConcat(parts) => return evalConcatSlot(&parts, &env, &slotMap, &fns),
                Expr::ExprTuple(exprs) => return evalTupleSlot(&exprs, &env, &slotMap, &fns),
                Expr::ExprList(exprs) => return evalListSlot(&exprs, &env, &slotMap, &fns),
                Expr::ExprRecord(name, fieldExprs) => return evalRecordSlot(name, &fieldExprs, &env, &slotMap, &fns),
                Expr::ExprFieldAccess(obj, field) => { let obj = (*obj).clone(); return evalFieldAccessSlot(&obj, field, &env, &slotMap, &fns) },
                Expr::ExprCall(name, argExprs) => return evalCallSlot(name, &argExprs, &env, &slotMap, &fns),
                _ => __MutualTco4::EvalExprSlotCalls(expr, env, slotMap, fns)
                }
            }
            __MutualTco4::EvalExprSlotCalls(mut expr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                Expr::ExprCallDirect(fnId, argExprs) => return evalCallDirectSlot(fnId, &argExprs, &env, &slotMap, &fns),
                Expr::ExprCallBuiltin(name, argExprs) => __MutualTco4::EvalCallBuiltinSlotMaybeSpecial(name, argExprs, env, slotMap, fns),
                Expr::ExprCallBuiltinId(id, argExprs) => __MutualTco4::EvalCallBuiltinByIdSlot(id, argExprs, env, slotMap, fns),
                Expr::ExprMatch(scrutinee, arms) => { let scrutinee = (*scrutinee).clone(); __MutualTco4::EvalMatchExprSlot(scrutinee, arms, env, slotMap, fns) },
                Expr::ExprPropagate(inner) => { let inner = (*inner).clone(); return evalPropagateSlot(&inner, &env, &slotMap, &fns) },
                Expr::ExprIndependentProduct(exprs, unwrap) => return evalIndependentProductSlot(&exprs, unwrap, &env, &slotMap, &fns),
                _ => return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((45i64) as usize); __b.push_str(&AverStr::from("unsupported slot expression: ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(expr)))); __b }))
                }
            }
            __MutualTco4::EvalBoolBranchSlot(mut cond, mut thenExpr, mut elseExpr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let condV = evalExprSlot(&cond, &env, &slotMap, &fns)?;
                match condV.clone() {
                Val::ValBool(flag) => if flag { __MutualTco4::EvalExprSlot(thenExpr, env, slotMap, fns) } else { __MutualTco4::EvalExprSlot(elseExpr, env, slotMap, fns) },
                _ => return Err((AverStr::from("bool branch condition must evaluate to Bool, got: ") + &crate::aver_generated::domain::value::valRepr(&condV)))
                }
            }
            __MutualTco4::EvalCallBuiltinByIdSlot(mut id, mut argExprs, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match id {
                15i64 => __MutualTco4::EvalOptionWithDefaultExprSlot(argExprs, env, slotMap, fns),
                _ => match evalArgsSlot(&argExprs, &env, &slotMap, &fns) { Err(e) => { return Err(e) }, Ok(args) => { return crate::aver_generated::domain::builtins::callBuiltinByIdValues(id, &args) } }
                }
            }
            __MutualTco4::EvalCallBuiltinSlotMaybeSpecial(mut name, mut argExprs, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match &*name.clone() {
                "Option.withDefault" => __MutualTco4::EvalOptionWithDefaultExprSlot(argExprs, env, slotMap, fns),
                _ => return evalCallBuiltinSlot(name, &argExprs, &env, &slotMap, &fns)
                }
            }
            __MutualTco4::EvalOptionWithDefaultExprSlot(mut argExprs, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                { let __list_subject = argExprs.clone(); if let Some((optionExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = rest; if let Some((defaultExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) { __MutualTco4::EvalOptionWithDefaultExprSlotInner(optionExpr, defaultExpr, env, slotMap, fns) } else { return evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &argExprs, &env, &slotMap, &fns) } } } else { return evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &argExprs, &env, &slotMap, &fns) } }
            }
            __MutualTco4::EvalOptionWithDefaultExprSlotInner(mut optionExpr, mut defaultExpr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match optionExpr.clone() {
                Expr::ExprCallBuiltin(name, vecArgs) => { let __dispatch_subject = name; if &*__dispatch_subject == "Vector.set" { __MutualTco4::EvalVectorSetWithDefaultExprSlot(vecArgs, defaultExpr, env, slotMap, fns) } else { if &*__dispatch_subject == "Vector.get" { __MutualTco4::EvalVectorGetWithDefaultExprSlot(vecArgs, defaultExpr, env, slotMap, fns) } else { return evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]), &env, &slotMap, &fns) } } },
                _ => return evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]), &env, &slotMap, &fns)
                }
            }
            __MutualTco4::EvalVectorSetWithDefaultExprSlot(mut vecArgs, mut defaultExpr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                { let __list_subject = vecArgs.clone(); if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = r1; if let Some((idxExpr, r2)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = r2; if let Some((valueExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) { __MutualTco4::EvalVectorSetWithDefaultExprSlotValues(vecExpr, idxExpr, valueExpr, defaultExpr, env, slotMap, fns) } else { return evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.set"), vecArgs), defaultExpr]), &env, &slotMap, &fns) } } } else { return evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.set"), vecArgs), defaultExpr]), &env, &slotMap, &fns) } } } else { return evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.set"), vecArgs), defaultExpr]), &env, &slotMap, &fns) } }
            }
            __MutualTco4::EvalVectorSetWithDefaultExprSlotValues(mut vecExpr, mut idxExpr, mut valueExpr, mut defaultExpr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let vecV = evalExprSlot(&vecExpr, &env, &slotMap, &fns)?;
                let idxV = evalExprSlot(&idxExpr, &env, &slotMap, &fns)?;
                let valueV = evalExprSlot(&valueExpr, &env, &slotMap, &fns)?;
                __MutualTco4::EvalVectorSetWithDefaultExprSlotResult(vecV, idxV, valueV, defaultExpr, env, slotMap, fns)
            }
            __MutualTco4::EvalVectorSetWithDefaultExprSlotResult(mut vecV, mut idxV, mut valueV, mut defaultExpr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::ops::evalVectorSetMaybeDefault(&vecV, &idxV, &valueV) { Ok(maybeV) => { match maybeV { Some(v) => { return Ok(v) }, None => { __MutualTco4::EvalExprSlot(defaultExpr, env, slotMap, fns) } } }, Err(err) => { return Err(err) } }
            }
            __MutualTco4::EvalVectorGetWithDefaultExprSlot(mut vecArgs, mut defaultExpr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                { let __list_subject = vecArgs.clone(); if let Some((vecExpr, r1)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = r1; if let Some((idxExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) { __MutualTco4::EvalVectorGetWithDefaultExprSlotValues(vecExpr, idxExpr, defaultExpr, env, slotMap, fns) } else { return evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.get"), vecArgs), defaultExpr]), &env, &slotMap, &fns) } } } else { return evalCallBuiltinSlot(AverStr::from("Option.withDefault"), &aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.get"), vecArgs), defaultExpr]), &env, &slotMap, &fns) } }
            }
            __MutualTco4::EvalVectorGetWithDefaultExprSlotValues(mut vecExpr, mut idxExpr, mut defaultExpr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let vecV = evalExprSlot(&vecExpr, &env, &slotMap, &fns)?;
                let idxV = evalExprSlot(&idxExpr, &env, &slotMap, &fns)?;
                __MutualTco4::EvalVectorGetWithDefaultExprSlotResult(vecV, idxV, defaultExpr, env, slotMap, fns)
            }
            __MutualTco4::EvalVectorGetWithDefaultExprSlotResult(mut vecV, mut idxV, mut defaultExpr, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::eval::ops::evalVectorGetMaybeDefault(&vecV, &idxV) { Ok(maybeV) => { match maybeV { Some(v) => { return Ok(v) }, None => { __MutualTco4::EvalExprSlot(defaultExpr, env, slotMap, fns) } } }, Err(err) => { return Err(err) } }
            }
            __MutualTco4::EvalMatchExprSlot(mut scrutinee, mut arms, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let v = evalExprSlot(&scrutinee, &env, &slotMap, &fns)?;
                __MutualTco4::EvalMatchSlot(v, arms, env, slotMap, fns)
            }
            __MutualTco4::EvalMatchSlot(mut v, mut arms, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                aver_list_match!(arms, [] => { return Err(AverStr::from("no matching arm")) }, [arm, rest] => match crate::aver_generated::domain::match_mod::matchPattern(&arm.pattern, &v) { Ok(bindings) => { __MutualTco4::EvalExprSlot(arm.body.clone(), mergeBindingsSlot(&bindings, &arm.bindingSlots, &env), slotMap, fns) }, Err(_) => { __MutualTco4::EvalMatchSlot(v, rest, env, slotMap, fns) } })
            }
        };
    }
}

/// Evaluate an expression using slot-based environment.
pub fn evalExprSlot(expr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalExprSlot(expr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Continue slot-based evaluation for branches, vars, and slot-only nodes.
pub fn evalExprSlotBasic(expr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalExprSlotBasic(expr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Continue slot-based expression evaluation for comparisons and arithmetic.
pub fn evalExprSlotInternal(expr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalExprSlotInternal(expr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Continue slot-based evaluation for aggregate and call expression forms.
pub fn evalExprSlotAggregate(expr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalExprSlotAggregate(expr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Finish slot-based evaluation for calls, matches, propagation, and products.
pub fn evalExprSlotCalls(expr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalExprSlotCalls(expr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate a direct bool branch in slot path.
pub fn evalBoolBranchSlot(cond: &Expr, thenExpr: &Expr, elseExpr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalBoolBranchSlot(cond.clone(), thenExpr.clone(), elseExpr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Builtin dispatch by integer ID (slot-based path) — no string comparison.
pub fn evalCallBuiltinByIdSlot(id: i64, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalCallBuiltinByIdSlot(id, argExprs.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Builtin dispatch with fast-path peepholes for hot patterns in slot path.
pub fn evalCallBuiltinSlotMaybeSpecial(name: AverStr, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalCallBuiltinSlotMaybeSpecial(name, argExprs.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Specialize hot Option.withDefault(Vector.get/Vector.set, default) patterns in slot path.
pub fn evalOptionWithDefaultExprSlot(argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalOptionWithDefaultExprSlot(argExprs.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Dispatch specialized Option.withDefault cases in slot path.
pub fn evalOptionWithDefaultExprSlotInner(optionExpr: &Expr, defaultExpr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalOptionWithDefaultExprSlotInner(optionExpr.clone(), defaultExpr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Specialized Vector.set + Option.withDefault in slot path.
pub fn evalVectorSetWithDefaultExprSlot(vecArgs: &aver_rt::AverList<Expr>, defaultExpr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalVectorSetWithDefaultExprSlot(vecArgs.clone(), defaultExpr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate Vector.set operands in slot path and defer the default expression until needed.
pub fn evalVectorSetWithDefaultExprSlotValues(vecExpr: &Expr, idxExpr: &Expr, valueExpr: &Expr, defaultExpr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalVectorSetWithDefaultExprSlotValues(vecExpr.clone(), idxExpr.clone(), valueExpr.clone(), defaultExpr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Finish specialized Vector.set + Option.withDefault in slot path with lazy default evaluation.
pub fn evalVectorSetWithDefaultExprSlotResult(vecV: &Val, idxV: &Val, valueV: &Val, defaultExpr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalVectorSetWithDefaultExprSlotResult(vecV.clone(), idxV.clone(), valueV.clone(), defaultExpr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Specialized Vector.get + Option.withDefault in slot path.
pub fn evalVectorGetWithDefaultExprSlot(vecArgs: &aver_rt::AverList<Expr>, defaultExpr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalVectorGetWithDefaultExprSlot(vecArgs.clone(), defaultExpr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate Vector.get operands in slot path and defer the default expression until needed.
pub fn evalVectorGetWithDefaultExprSlotValues(vecExpr: &Expr, idxExpr: &Expr, defaultExpr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalVectorGetWithDefaultExprSlotValues(vecExpr.clone(), idxExpr.clone(), defaultExpr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Finish specialized Vector.get + Option.withDefault in slot path with lazy default evaluation.
pub fn evalVectorGetWithDefaultExprSlotResult(vecV: &Val, idxV: &Val, defaultExpr: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalVectorGetWithDefaultExprSlotResult(vecV.clone(), idxV.clone(), defaultExpr.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Match expression in slot path.
pub fn evalMatchExprSlot(scrutinee: &Expr, arms: &aver_rt::AverList<MatchArm>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalMatchExprSlot(scrutinee.clone(), arms.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Try each match arm in slot path.
pub fn evalMatchSlot(v: &Val, arms: &aver_rt::AverList<MatchArm>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::EvalMatchSlot(v.clone(), arms.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco5 {
    EvalStmtBindSlotNext(i64, Expr, aver_rt::AverList<Stmt>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalStmtExprSlotNext(Expr, aver_rt::AverList<Stmt>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalStmtBindFallbackSlotNext(AverStr, Expr, aver_rt::AverList<Stmt>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalStmtsSlot(aver_rt::AverList<Stmt>, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
}

fn __mutual_tco_trampoline_5(mut __state: __MutualTco5) -> Result<Val, AverStr> {
    loop {
        __state = match __state {
            __MutualTco5::EvalStmtBindSlotNext(mut slot, mut e, mut rest, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let v = evalExprSlot(&e, &env, &slotMap, &fns)?;
                let nextEnv = crate::aver_generated::domain::eval::slots::setSlot(&env, slot, &v);
                { let __list_subject = rest.clone(); if __list_subject.is_empty() { return Ok(v) } else { __MutualTco5::EvalStmtsSlot(rest, nextEnv, slotMap, fns) } }
            }
            __MutualTco5::EvalStmtExprSlotNext(mut e, mut rest, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let v = evalExprSlot(&e, &env, &slotMap, &fns)?;
                { let __list_subject = rest.clone(); if __list_subject.is_empty() { return Ok(v) } else { __MutualTco5::EvalStmtsSlot(rest, env, slotMap, fns) } }
            }
            __MutualTco5::EvalStmtBindFallbackSlotNext(mut name, mut e, mut rest, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let v = evalExprSlot(&e, &env, &slotMap, &fns)?;
                { let __list_subject = rest.clone(); if __list_subject.is_empty() { return Ok(v) } else { __MutualTco5::EvalStmtsSlot(rest, env, slotMap, fns) } }
            }
            __MutualTco5::EvalStmtsSlot(mut stmts, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                aver_list_match!(stmts, [] => { return Ok(Val::ValUnit.clone()) }, [stmt, rest] => match stmt {
                Stmt::StmtBindSlot(slot, e) => __MutualTco5::EvalStmtBindSlotNext(slot, e, rest, env, slotMap, fns),
                Stmt::StmtExpr(e) => __MutualTco5::EvalStmtExprSlotNext(e, rest, env, slotMap, fns),
                Stmt::StmtBind(name, e) => __MutualTco5::EvalStmtBindFallbackSlotNext(name, e, rest, env, slotMap, fns)
                })
            }
        };
    }
}

/// Evaluate a slot binding and continue statement execution without packing a tuple.
pub fn evalStmtBindSlotNext(slot: i64, e: &Expr, rest: &aver_rt::AverList<Stmt>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::EvalStmtBindSlotNext(slot, e.clone(), rest.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate an expression statement and continue statement execution without packing a tuple.
pub fn evalStmtExprSlotNext(e: &Expr, rest: &aver_rt::AverList<Stmt>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::EvalStmtExprSlotNext(e.clone(), rest.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Fallback for unresolved StmtBind in slot path without packing a tuple.
pub fn evalStmtBindFallbackSlotNext(name: AverStr, e: &Expr, rest: &aver_rt::AverList<Stmt>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::EvalStmtBindFallbackSlotNext(name, e.clone(), rest.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate statements in slot path, threading env.
pub fn evalStmtsSlot(stmts: &aver_rt::AverList<Stmt>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::EvalStmtsSlot(stmts.clone(), env.clone(), slotMap.clone(), fns.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco6 {
    EvalStmtsSlotTail(i64, aver_rt::AverList<Stmt>, i64, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalStmtsSlotTailNext(i64, Stmt, aver_rt::AverList<Stmt>, i64, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalStmtsSlotTailBind(i64, i64, Expr, aver_rt::AverList<Stmt>, i64, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalStmtsSlotTailExpr(i64, Expr, aver_rt::AverList<Stmt>, i64, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
}

fn __mutual_tco_trampoline_6(mut __state: __MutualTco6) -> Result<SlotTailStep, AverStr> {
    loop {
        __state = match __state {
            __MutualTco6::EvalStmtsSlotTail(mut selfId, mut stmts, mut slotCount, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                aver_list_match!(stmts, [] => { return Ok(SlotTailStep::SlotTailDone(Val::ValUnit.clone())) }, [stmt, rest] => { { let __list_subject = rest.clone(); if __list_subject.is_empty() { return evalTailStmtSlot(selfId, &stmt, slotCount, &env, &slotMap, &fns) } else { __MutualTco6::EvalStmtsSlotTailNext(selfId, stmt, rest, slotCount, env, slotMap, fns) } } })
            }
            __MutualTco6::EvalStmtsSlotTailNext(mut selfId, mut stmt, mut rest, mut slotCount, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match stmt {
                Stmt::StmtBindSlot(slot, e) => __MutualTco6::EvalStmtsSlotTailBind(selfId, slot, e, rest, slotCount, env, slotMap, fns),
                Stmt::StmtExpr(e) => __MutualTco6::EvalStmtsSlotTailExpr(selfId, e, rest, slotCount, env, slotMap, fns),
                Stmt::StmtBind(_, e) => __MutualTco6::EvalStmtsSlotTailExpr(selfId, e, rest, slotCount, env, slotMap, fns)
                }
            }
            __MutualTco6::EvalStmtsSlotTailBind(mut selfId, mut slot, mut e, mut rest, mut slotCount, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let v = evalExprSlot(&e, &env, &slotMap, &fns)?;
                __MutualTco6::EvalStmtsSlotTail(selfId, rest, slotCount, crate::aver_generated::domain::eval::slots::setSlot(&env, slot, &v), slotMap, fns)
            }
            __MutualTco6::EvalStmtsSlotTailExpr(mut selfId, mut e, mut rest, mut slotCount, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let _ = evalExprSlot(&e, &env, &slotMap, &fns)?;
                __MutualTco6::EvalStmtsSlotTail(selfId, rest, slotCount, env, slotMap, fns)
            }
        };
    }
}

/// Evaluate slot statements when the final expression may self-tail-recur.
pub fn evalStmtsSlotTail(selfId: i64, stmts: &aver_rt::AverList<Stmt>, slotCount: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_6(__MutualTco6::EvalStmtsSlotTail(selfId, stmts.clone(), slotCount, env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate one non-final slot statement before continuing the tail-aware slot loop.
pub fn evalStmtsSlotTailNext(selfId: i64, stmt: &Stmt, rest: &aver_rt::AverList<Stmt>, slotCount: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_6(__MutualTco6::EvalStmtsSlotTailNext(selfId, stmt.clone(), rest.clone(), slotCount, env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate one slot binding before continuing the tail-aware slot loop.
pub fn evalStmtsSlotTailBind(selfId: i64, slot: i64, e: &Expr, rest: &aver_rt::AverList<Stmt>, slotCount: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_6(__MutualTco6::EvalStmtsSlotTailBind(selfId, slot, e.clone(), rest.clone(), slotCount, env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate one non-final expression statement before continuing the tail-aware slot loop.
pub fn evalStmtsSlotTailExpr(selfId: i64, e: &Expr, rest: &aver_rt::AverList<Stmt>, slotCount: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_6(__MutualTco6::EvalStmtsSlotTailExpr(selfId, e.clone(), rest.clone(), slotCount, env.clone(), slotMap.clone(), fns.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco7 {
    EvalTailExprSlot(i64, Expr, i64, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalTailBoolBranchSlot(i64, Expr, Expr, Expr, i64, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalTailMatchExprSlot(i64, Expr, aver_rt::AverList<MatchArm>, i64, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
    EvalTailMatchSlot(i64, Val, aver_rt::AverList<MatchArm>, i64, aver_rt::AverVector<Val>, aver_rt::AverMap<AverStr, i64>, FnStore),
}

fn __mutual_tco_trampoline_7(mut __state: __MutualTco7) -> Result<SlotTailStep, AverStr> {
    loop {
        __state = match __state {
            __MutualTco7::EvalTailExprSlot(mut selfId, mut expr, mut slotCount, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                Expr::ExprCallDirect(fnId, argExprs) => if (fnId == selfId) { match evalArgsSlotToSlotEnv(argExprs, env, slotMap, fns, aver_rt::AverVector::new(slotCount as usize, Val::ValUnit.clone()), 0i64) { Ok(nextEnv) => { return Ok(SlotTailStep::SlotTailRecurEnv(nextEnv)) }, Err(e) => { return Err(e) } } } else { match evalCallDirectSlot(fnId, &argExprs, &env, &slotMap, &fns) { Ok(v) => { return Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { return Err(e) } } },
                Expr::ExprBoolBranch(cond, thenExpr, elseExpr) => { let cond = (*cond).clone(); let thenExpr = (*thenExpr).clone(); let elseExpr = (*elseExpr).clone(); __MutualTco7::EvalTailBoolBranchSlot(selfId, cond, thenExpr, elseExpr, slotCount, env, slotMap, fns) },
                Expr::ExprMatch(scrutinee, arms) => { let scrutinee = (*scrutinee).clone(); __MutualTco7::EvalTailMatchExprSlot(selfId, scrutinee, arms, slotCount, env, slotMap, fns) },
                _ => match evalExprSlot(&expr, &env, &slotMap, &fns) { Ok(v) => { return Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { return Err(e) } }
                }
            }
            __MutualTco7::EvalTailBoolBranchSlot(mut selfId, mut cond, mut thenExpr, mut elseExpr, mut slotCount, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let condV = evalExprSlot(&cond, &env, &slotMap, &fns)?;
                match condV.clone() {
                Val::ValBool(flag) => if flag { __MutualTco7::EvalTailExprSlot(selfId, thenExpr, slotCount, env, slotMap, fns) } else { __MutualTco7::EvalTailExprSlot(selfId, elseExpr, slotCount, env, slotMap, fns) },
                _ => return Err((AverStr::from("bool branch condition must evaluate to Bool, got: ") + &crate::aver_generated::domain::value::valRepr(&condV)))
                }
            }
            __MutualTco7::EvalTailMatchExprSlot(mut selfId, mut scrutinee, mut arms, mut slotCount, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                let v = evalExprSlot(&scrutinee, &env, &slotMap, &fns)?;
                __MutualTco7::EvalTailMatchSlot(selfId, v, arms, slotCount, env, slotMap, fns)
            }
            __MutualTco7::EvalTailMatchSlot(mut selfId, mut v, mut arms, mut slotCount, mut env, mut slotMap, mut fns) => {
                crate::cancel_checkpoint();
                aver_list_match!(arms, [] => { return Err(AverStr::from("no matching arm")) }, [arm, rest] => match crate::aver_generated::domain::match_mod::matchPattern(&arm.pattern, &v) { Ok(bindings) => { __MutualTco7::EvalTailExprSlot(selfId, arm.body.clone(), slotCount, mergeBindingsSlot(&bindings, &arm.bindingSlots, &env), slotMap, fns) }, Err(_) => { __MutualTco7::EvalTailMatchSlot(selfId, v, rest, slotCount, env, slotMap, fns) } })
            }
        };
    }
}

/// Evaluate a tail-position expression in slot mode, converting self-recursive direct calls into loop re-entry.
pub fn evalTailExprSlot(selfId: i64, expr: &Expr, slotCount: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_7(__MutualTco7::EvalTailExprSlot(selfId, expr.clone(), slotCount, env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate a tail-position bool branch in slot mode.
pub fn evalTailBoolBranchSlot(selfId: i64, cond: &Expr, thenExpr: &Expr, elseExpr: &Expr, slotCount: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_7(__MutualTco7::EvalTailBoolBranchSlot(selfId, cond.clone(), thenExpr.clone(), elseExpr.clone(), slotCount, env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate a tail-position match expression in slot mode.
pub fn evalTailMatchExprSlot(selfId: i64, scrutinee: &Expr, arms: &aver_rt::AverList<MatchArm>, slotCount: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_7(__MutualTco7::EvalTailMatchExprSlot(selfId, scrutinee.clone(), arms.clone(), slotCount, env.clone(), slotMap.clone(), fns.clone()))
}

/// Evaluate a tail-position match arm in slot mode, preserving binding slots.
pub fn evalTailMatchSlot(selfId: i64, v: &Val, arms: &aver_rt::AverList<MatchArm>, slotCount: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    __mutual_tco_trampoline_7(__MutualTco7::EvalTailMatchSlot(selfId, v.clone(), arms.clone(), slotCount, env.clone(), slotMap.clone(), fns.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco8 {
    MergeBindingsSlot(aver_rt::AverList<(AverStr, Val)>, aver_rt::AverMap<AverStr, i64>, aver_rt::AverVector<Val>),
    MergeOneBindingSlot(AverStr, Val, aver_rt::AverList<(AverStr, Val)>, aver_rt::AverMap<AverStr, i64>, aver_rt::AverVector<Val>),
}

fn __mutual_tco_trampoline_8(mut __state: __MutualTco8) -> aver_rt::AverVector<Val> {
    loop {
        __state = match __state {
            __MutualTco8::MergeBindingsSlot(mut bindings, mut bindingSlots, mut env) => {
                crate::cancel_checkpoint();
                aver_list_match!(bindings, [] => { return env }, [pair, rest] => match pair {
                (name, val) => __MutualTco8::MergeOneBindingSlot(name, val, rest, bindingSlots, env)
                })
            }
            __MutualTco8::MergeOneBindingSlot(mut name, mut val, mut rest, mut bindingSlots, mut env) => {
                crate::cancel_checkpoint();
                match bindingSlots.get(&name).cloned() { Some(slot) => { __MutualTco8::MergeBindingsSlot(rest, bindingSlots, crate::aver_generated::domain::eval::slots::setSlot(&env, slot, &val)) }, None => { __MutualTco8::MergeBindingsSlot(rest, bindingSlots, env) } }
            }
        };
    }
}

/// Merge pattern bindings into slot env using the slots assigned for this specific arm.
pub fn mergeBindingsSlot(bindings: &aver_rt::AverList<(AverStr, Val)>, bindingSlots: &aver_rt::AverMap<AverStr, i64>, env: &aver_rt::AverVector<Val>) -> aver_rt::AverVector<Val> {
    __mutual_tco_trampoline_8(__MutualTco8::MergeBindingsSlot(bindings.clone(), bindingSlots.clone(), env.clone()))
}

/// Merge one pattern binding into slot env.
pub fn mergeOneBindingSlot(name: AverStr, val: &Val, rest: &aver_rt::AverList<(AverStr, Val)>, bindingSlots: &aver_rt::AverMap<AverStr, i64>, env: &aver_rt::AverVector<Val>) -> aver_rt::AverVector<Val> {
    __mutual_tco_trampoline_8(__MutualTco8::MergeOneBindingSlot(name, val.clone(), rest.clone(), bindingSlots.clone(), env.clone()))
}

/// Recognize immediate literal expressions in named-env mode.
pub fn evalImmediateNamedExpr(expr: &Expr) -> Option<Val> {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprInt(n) => {
            Some(Val::ValInt(n))
        },
        Expr::ExprFloat(f) => {
            Some(Val::ValFloat(f))
        },
        Expr::ExprStr(s) => {
            Some(Val::ValStr(s))
        },
        Expr::ExprBool(b) => {
            Some(Val::ValBool(b))
        },
        _ => {
            None
        }
    }
}

/// Look up variable, Option.None, a named function reference, or a nullary variant constructor.
#[inline(always)]
pub fn evalVar(name: AverStr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    { let __dispatch_subject = name.clone(); if &*__dispatch_subject == "Unit" { Ok(Val::ValUnit.clone()) } else { if &*__dispatch_subject == "Option.None" { Ok(Val::ValNone.clone()) } else { match crate::aver_generated::domain::eval::store::lookupVar(env, name.clone()) { Ok(v) => { Ok(v) }, Err(_) => { crate::aver_generated::domain::eval::common::evalVarFallback(name, fns) } } } } }
}

/// Evaluate a fused Vector.get + Option.withDefault in map path.
pub fn evalVectorGetOrIntExpr(vecExpr: &Expr, idxExpr: &Expr, defaultValue: i64, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV = evalExpr(vecExpr, env, fns)?;
    let idxV = evalExpr(idxExpr, env, fns)?;
    crate::aver_generated::domain::eval::ops::evalVectorGetOrIntVals(&vecV, &idxV, defaultValue)
}

/// Evaluate a fused Int.mod + Result.withDefault in map path.
pub fn evalIntModOrIntExpr(a: &Expr, b: &Expr, defaultValue: i64, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let aV = evalExpr(a, env, fns)?;
    let bV = evalExpr(b, env, fns)?;
    crate::aver_generated::domain::eval::ops::evalIntModOrIntVals(&aV, &bV, defaultValue)
}

/// Evaluate ? operator: unwrap Ok or propagate Err.
pub fn evalPropagate(inner: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = evalExpr(inner, env, fns)?;
    match v {
        Val::ValOk(x) => {
            let x = (*x).clone();
            Ok(x)
        },
        Val::ValErr(e) => {
            let e = (*e).clone();
            match e {
        Val::ValStr(msg) => {
            Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(msg))
        },
        _ => {
            Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(AverStr::from("propagated error")))
        }
    }
        },
        _ => {
            Err(AverStr::from("? operator requires Result value"))
        }
    }
}

/// Evaluate independent product using the self-host's own divide-and-conquer ?!, then unwrap guest Results if needed.
pub fn evalIndependentProduct(exprs: &aver_rt::AverList<Expr>, unwrap: bool, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = evalIndependentItems(exprs, env, fns)?;
    if unwrap { unwrapProductResults(items, aver_rt::AverList::empty()) } else { Ok(Val::ValTuple(items)) }
}

/// Evaluate guest independent-product branches through a balanced tree of the self-host's own ?!.
pub fn evalIndependentItems(exprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<aver_rt::AverList<Val>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(exprs.clone(), [] => Ok(aver_rt::AverList::empty()), [a, rest] => { aver_list_match!(rest, [] => match evalExpr(&a, env, fns) { Ok(va) => { Ok(aver_rt::AverList::from_vec(vec![va])) }, Err(e) => { Err(e) } }, [b, rest2] => { { let __list_subject = rest2; if __list_subject.is_empty() { { let (va, vb) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let _r0 = evalExpr(&a, env, fns); crate::aver_replay::set_effect_branch(1); let _r1 = evalExpr(&b, env, fns); crate::aver_replay::exit_effect_group(); match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|_s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let _h0 = _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = evalExpr(&a, env, fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })); let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let _h1 = _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = evalExpr(&b, env, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })); let _b0 = _h0.join().unwrap(); let _b1 = _h1.join().unwrap(); match (_b0, _b1) { (crate::ParallelBranch::Completed(_r0), crate::ParallelBranch::Completed(_r1)) => match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (_b0, _b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = _b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = _b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok(aver_rt::AverList::from_vec(vec![va, vb])) } } else { { let (leftExprs, rightExprs) = splitIndependentExprs(exprs.clone(), aver_rt::AverList::empty(), aver_rt::AverList::empty(), true); { let (leftVals, rightVals) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let _r0 = evalIndependentItems(&leftExprs, env, fns); crate::aver_replay::set_effect_branch(1); let _r1 = evalIndependentItems(&rightExprs, env, fns); crate::aver_replay::exit_effect_group(); match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|_s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let _h0 = _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = evalIndependentItems(&leftExprs, env, fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })); let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let _h1 = _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = evalIndependentItems(&rightExprs, env, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })); let _b0 = _h0.join().unwrap(); let _b1 = _h1.join().unwrap(); match (_b0, _b1) { (crate::ParallelBranch::Completed(_r0), crate::ParallelBranch::Completed(_r1)) => match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (_b0, _b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = _b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = _b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok(interleaveIndependentVals(leftVals, rightVals, true, aver_rt::AverList::empty())) } } } } }) })
}

/// Split a product into alternating branches so recursive ?! can cover all items.
#[inline(always)]
pub fn splitIndependentExprs(mut exprs: aver_rt::AverList<Expr>, mut leftAcc: aver_rt::AverList<Expr>, mut rightAcc: aver_rt::AverList<Expr>, mut sendLeft: bool) -> (aver_rt::AverList<Expr>, aver_rt::AverList<Expr>) {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => (leftAcc.reverse(), rightAcc.reverse()), [expr, rest] => { if sendLeft { {
            let __tmp1 = aver_rt::AverList::prepend(expr, &leftAcc);
            exprs = rest;
            leftAcc = __tmp1;
            sendLeft = false;
            continue;
        } } else { {
            let __tmp2 = aver_rt::AverList::prepend(expr, &rightAcc);
            exprs = rest;
            rightAcc = __tmp2;
            sendLeft = true;
            continue;
        } } });
    }
}

/// Restore original product order after alternating recursive split.
#[inline(always)]
pub fn interleaveIndependentVals(mut left: aver_rt::AverList<Val>, mut right: aver_rt::AverList<Val>, mut takeLeft: bool, mut acc: aver_rt::AverList<Val>) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        return if takeLeft { aver_list_match!(left, [] => finishIndependentVals(right, acc), [v, rest] => { {
            let __tmp3 = aver_rt::AverList::prepend(v, &acc);
            left = rest;
            takeLeft = false;
            acc = __tmp3;
            continue;
        } }) } else { aver_list_match!(right, [] => finishIndependentVals(left, acc), [v, rest] => { {
            let __tmp3 = aver_rt::AverList::prepend(v, &acc);
            right = rest;
            takeLeft = true;
            acc = __tmp3;
            continue;
        } }) };
    }
}

/// Append remaining values after interleaving and return in forward order.
#[inline(always)]
pub fn finishIndependentVals(mut items: aver_rt::AverList<Val>, mut acc: aver_rt::AverList<Val>) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(items, [] => acc.reverse(), [v, rest] => { {
            let __tmp1 = aver_rt::AverList::prepend(v, &acc);
            items = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Unwrap all Result values in an independent product (?!).
#[inline(always)]
pub fn unwrapProductResults(mut items: aver_rt::AverList<Val>, mut acc: aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(items, [] => Ok(Val::ValTuple(acc.reverse())), [v, rest] => { match v {
            Val::ValOk(x) => { let x = (*x).clone(); {
            let __tmp1 = aver_rt::AverList::prepend(x, &acc);
            items = rest;
            acc = __tmp1;
            continue;
        } },
            Val::ValErr(e) => { let e = (*e).clone(); match e {
            Val::ValStr(msg) => Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(msg)),
            _ => Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(AverStr::from("propagated error")))
        } },
            _ => Err(AverStr::from("?! operator requires all elements to be Result values"))
        } });
    }
}

/// Evaluate right side of binop and apply.
pub fn evalBinopRight(va: &Val, b: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore, op: &BinOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match evalExpr(b, env, fns) { Err(e) => { Err(e) }, Ok(vb) => { crate::aver_generated::domain::eval::ops::evalBinopVals(va, &vb, op) } }
}

/// Evaluate a binary operation on two expressions.
pub fn evalBinop(a: &Expr, b: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore, op: &BinOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match evalExpr(a, env, fns) { Err(e) => { Err(e) }, Ok(va) => { evalBinopRight(&va, b, env, fns, op) } }
}

/// Evaluate right side of comparison and apply.
pub fn evalCmpRight(va: &Val, b: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore, op: &CmpOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match evalExpr(b, env, fns) { Err(e) => { Err(e) }, Ok(vb) => { crate::aver_generated::domain::eval::ops::evalCmpVals(va, &vb, op) } }
}

/// Evaluate a comparison expression.
pub fn evalCmp(a: &Expr, b: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore, op: &CmpOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match evalExpr(a, env, fns) { Err(e) => { Err(e) }, Ok(va) => { evalCmpRight(&va, b, env, fns, op) } }
}

/// Evaluate string interpolation: concat all parts as strings.
pub fn evalConcatExpr(parts: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    evalConcatParts(parts.clone(), env.clone(), fns.clone(), AverStr::from(""))
}

/// Concatenate interpolation parts. Self-recursive for codegen TCO.
pub fn evalConcatParts(mut parts: aver_rt::AverList<Expr>, mut env: aver_rt::AverMap<AverStr, Val>, mut fns: FnStore, mut acc: AverStr) -> Result<Val, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(parts, [] => Ok(Val::ValStr(acc)), [p, rest] => { match evalExpr(&p, &env, &fns) { Err(e) => { Err(e) }, Ok(v) => { {
            let __tmp3 = (acc + &crate::aver_generated::domain::value::valRepr(&v));
            parts = rest;
            acc = __tmp3;
            continue;
        } } } });
    }
}

/// Evaluate tuple literal.
pub fn evalTupleExpr(exprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = evalListItems(exprs, env, fns)?;
    Ok(Val::ValTuple(items))
}

/// Evaluate record constructor.
pub fn evalRecordExpr(name: AverStr, fieldExprs: &aver_rt::AverList<(AverStr, Expr)>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let fields = evalRecordFields(fieldExprs, env, fns)?;
    Ok(Val::ValRecord(name, fields))
}

/// Evaluate record field expressions.
pub fn evalRecordFields(fieldExprs: &aver_rt::AverList<(AverStr, Expr)>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(fieldExprs.clone(), [] => Ok(aver_rt::AverList::empty()), [pair, rest] => { { let (fname, expr) = pair; evalRecordFieldOne(fname, &expr, &rest, env, fns) } })
}

/// Evaluate one record field and continue.
pub fn evalRecordFieldOne(fname: AverStr, expr: &Expr, rest: &aver_rt::AverList<(AverStr, Expr)>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    let v = evalExpr(expr, env, fns)?;
    let restFields = evalRecordFields(rest, env, fns)?;
    Ok(aver_rt::AverList::prepend((fname, v), &restFields))
}

/// Evaluate field access: obj.field.
pub fn evalFieldAccess(obj: &Expr, field: AverStr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = evalExpr(obj, env, fns)?;
    match v {
        Val::ValRecord(_, fields) => {
            crate::aver_generated::domain::eval::common::lookupField(fields, field)
        },
        _ => {
            Err(AverStr::from("field access on non-record"))
        }
    }
}

/// Evaluate a list literal.
pub fn evalListExpr(exprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match evalListItems(exprs, env, fns) { Ok(items) => { Ok(Val::ValList(items)) }, Err(e) => { Err(e) } }
}

/// Evaluate list of expressions into list of values.
pub fn evalListItems(exprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<aver_rt::AverList<Val>, AverStr> {
    crate::cancel_checkpoint();
    evalListItemsRev(exprs.clone(), env.clone(), fns.clone(), aver_rt::AverList::empty())
}

/// Tail-recursive worker for evalListItems. Accumulates in reverse.
pub fn evalListItemsRev(mut exprs: aver_rt::AverList<Expr>, mut env: aver_rt::AverMap<AverStr, Val>, mut fns: FnStore, mut acc: aver_rt::AverList<Val>) -> Result<aver_rt::AverList<Val>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => Ok(acc.reverse()), [e, rest] => { match evalExpr(&e, &env, &fns) { Err(err) => { Err(err) }, Ok(v) => { {
            let __tmp3 = aver_rt::AverList::prepend(v, &acc);
            exprs = rest;
            acc = __tmp3;
            continue;
        } } } });
    }
}

/// Evaluate a list of argument expressions.
pub fn evalArgs(exprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<aver_rt::AverList<Val>, AverStr> {
    crate::cancel_checkpoint();
    evalArgsRev(exprs.clone(), env.clone(), fns.clone(), aver_rt::AverList::empty())
}

/// Tail-recursive worker for evalArgs. Accumulates in reverse.
pub fn evalArgsRev(mut exprs: aver_rt::AverList<Expr>, mut env: aver_rt::AverMap<AverStr, Val>, mut fns: FnStore, mut acc: aver_rt::AverList<Val>) -> Result<aver_rt::AverList<Val>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => Ok(acc.reverse()), [e, rest] => { match evalExpr(&e, &env, &fns) { Err(err) => { Err(err) }, Ok(v) => { {
            let __tmp3 = aver_rt::AverList::prepend(v, &acc);
            exprs = rest;
            acc = __tmp3;
            continue;
        } } } });
    }
}

/// Dispatch call: record update if Type.update, else normal call.
pub fn callWithArgs(args: &aver_rt::AverList<Val>, name: AverStr, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::eval::records::isRecordUpdate(name.clone(), args) { crate::aver_generated::domain::eval::records::doRecordUpdate(args) } else { callWithArgsNormal(args, name, fns) }
}

/// Normal function call dispatch. Uses slot-based eval for resolved fns.
pub fn callWithArgsNormal(args: &aver_rt::AverList<Val>, name: AverStr, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::store::lookupFnOption(fns, name.clone()) { Some(fd) => { callResolved(&fd, args, fns) }, None => { crate::aver_generated::domain::builtins::callBuiltin(name, args) } }
}

/// Call a function: slot-based if resolved, map-based otherwise.
pub fn callResolved(fd: &FnDef, args: &aver_rt::AverList<Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let fnId = crate::aver_generated::domain::eval::store::lookupFnId(fns, fd.name.clone())?;
    callResolvedById(fnId, fd, args, fns)
}

/// Call a function with a known store id: slot-based if resolved, map-based otherwise.
pub fn callResolvedById(fnId: i64, fd: &FnDef, args: &aver_rt::AverList<Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    if (fd.slotCount > 0i64) { evalResolvedSlotFn(fnId, fd, &crate::aver_generated::domain::eval::slots::buildSlotEnv(args, fd.slotCount), fns) } else { evalResolvedNamedFn(fd, &crate::aver_generated::domain::eval::store::zipArgs(fd.params.clone(), args.clone(), HashMap::new()), fns) }
}

/// Evaluate a resolved function body in slot mode, looping self-tail-calls instead of recursing on the host stack.
pub fn evalResolvedSlotFn(fnId: i64, fd: &FnDef, calleeEnv: &aver_rt::AverVector<Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    if fd.tailLoop { evalResolvedSlotLoop(fnId, fd.clone(), calleeEnv.clone(), fns.clone()) } else { evalResolvedSlotDirect(fd, calleeEnv, fns) }
}

/// Run the tail-aware slot evaluator only for functions that actually end in self-tail-calls.
pub fn evalResolvedSlotLoop(mut fnId: i64, mut fd: FnDef, mut calleeEnv: aver_rt::AverVector<Val>, mut fns: FnStore) -> Result<Val, AverStr> {
    loop {
        crate::cancel_checkpoint();
        let step = evalResolvedSlotStep(fnId, &fd, &calleeEnv, &fns)?;
        return match step {
            SlotTailStep::SlotTailDone(v) => crate::aver_generated::domain::eval::common::normalizeFnReturn(&Ok(v)),
            SlotTailStep::SlotTailRecurEnv(nextEnv) => {
            calleeEnv = nextEnv;
            continue;
        }
        };
    }
}

/// Evaluate a resolved function body in slot mode without the tail-loop machinery.
pub fn evalResolvedSlotDirect(fd: &FnDef, calleeEnv: &aver_rt::AverVector<Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let result = match fd.fastPath.clone() {
        FnFastPath::FastLeaf(leaf) => {
            crate::aver_generated::domain::eval::fast::runFastLeafSlot(&leaf, calleeEnv)
        },
        FnFastPath::FastForwardCall(targetId, slotArgs) => {
            fastForwardCall(calleeEnv, targetId, &slotArgs, fns)
        },
        FnFastPath::FastBoolSlotBranch(slot, thenLeaf, elseLeaf) => {
            crate::aver_generated::domain::eval::fast::fastBoolSlotBranch(calleeEnv, slot, &thenLeaf, &elseLeaf)
        },
        FnFastPath::FastEqIntBranch(slot, expected, thenLeaf, elseLeaf) => {
            crate::aver_generated::domain::eval::fast::fastEqIntBranch(calleeEnv, slot, expected, &thenLeaf, &elseLeaf)
        },
        FnFastPath::FastEqStringBranch(slot, expected, thenLeaf, elseLeaf) => {
            crate::aver_generated::domain::eval::fast::fastEqStringBranch(calleeEnv, slot, expected, &thenLeaf, &elseLeaf)
        },
        FnFastPath::FastLtIntSlotsBranch(lhsSlot, rhsSlot, thenLeaf, elseLeaf) => {
            crate::aver_generated::domain::eval::fast::fastLtIntSlotsBranch(calleeEnv, lhsSlot, rhsSlot, &thenLeaf, &elseLeaf)
        },
        FnFastPath::FastListSlotBranch(slot, emptyLeaf, headSlot, tailSlot, consLeaf) => {
            crate::aver_generated::domain::eval::fast::fastListSlotBranch(calleeEnv, slot, &emptyLeaf, headSlot, tailSlot, &consLeaf)
        },
        FnFastPath::FastSingleExpr => {
            evalResolvedSingleExprSlot(&fd.body, calleeEnv, &fd.slotMap, fns)
        },
        FnFastPath::FastNone => {
            evalStmtsSlot(&fd.body, calleeEnv, &fd.slotMap, fns)
        }
    };
    crate::aver_generated::domain::eval::common::normalizeFnReturn(&result)
}

/// Run one slot-frame step and either finish with a value or request a self-tail-call re-entry.
pub fn evalResolvedSlotStep(fnId: i64, fd: &FnDef, calleeEnv: &aver_rt::AverVector<Val>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    crate::cancel_checkpoint();
    match fd.fastPath.clone() {
        FnFastPath::FastLeaf(leaf) => {
            match crate::aver_generated::domain::eval::fast::runFastLeafSlot(&leaf, calleeEnv) { Ok(v) => { Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { Err(e) } }
        },
        FnFastPath::FastForwardCall(targetId, slotArgs) => {
            match fastForwardCall(calleeEnv, targetId, &slotArgs, fns) { Ok(v) => { Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { Err(e) } }
        },
        FnFastPath::FastBoolSlotBranch(slot, thenLeaf, elseLeaf) => {
            match crate::aver_generated::domain::eval::fast::fastBoolSlotBranch(calleeEnv, slot, &thenLeaf, &elseLeaf) { Ok(v) => { Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { Err(e) } }
        },
        FnFastPath::FastEqIntBranch(slot, expected, thenLeaf, elseLeaf) => {
            match crate::aver_generated::domain::eval::fast::fastEqIntBranch(calleeEnv, slot, expected, &thenLeaf, &elseLeaf) { Ok(v) => { Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { Err(e) } }
        },
        FnFastPath::FastEqStringBranch(slot, expected, thenLeaf, elseLeaf) => {
            match crate::aver_generated::domain::eval::fast::fastEqStringBranch(calleeEnv, slot, expected, &thenLeaf, &elseLeaf) { Ok(v) => { Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { Err(e) } }
        },
        FnFastPath::FastLtIntSlotsBranch(lhsSlot, rhsSlot, thenLeaf, elseLeaf) => {
            match crate::aver_generated::domain::eval::fast::fastLtIntSlotsBranch(calleeEnv, lhsSlot, rhsSlot, &thenLeaf, &elseLeaf) { Ok(v) => { Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { Err(e) } }
        },
        FnFastPath::FastListSlotBranch(slot, emptyLeaf, headSlot, tailSlot, consLeaf) => {
            match crate::aver_generated::domain::eval::fast::fastListSlotBranch(calleeEnv, slot, &emptyLeaf, headSlot, tailSlot, &consLeaf) { Ok(v) => { Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { Err(e) } }
        },
        FnFastPath::FastSingleExpr => {
            evalResolvedSingleExprSlotTail(fnId, &fd.body, fd.slotCount, &fd.slotMap, calleeEnv, fns)
        },
        FnFastPath::FastNone => {
            evalStmtsSlotTail(fnId, &fd.body, fd.slotCount, calleeEnv, &fd.slotMap, fns)
        }
    }
}

/// Evaluate a resolved function body in named-env mode, using a fast expr tag when available.
pub fn evalResolvedNamedFn(fd: &FnDef, calleeEnv: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let result = match fd.fastPath.clone() {
        FnFastPath::FastLeaf(leaf) => {
            runFastLeafNamed(&leaf, &fd.body, calleeEnv, fns)
        },
        FnFastPath::FastSingleExpr => {
            evalResolvedSingleExprNamed(&fd.body, calleeEnv, fns)
        },
        FnFastPath::FastNone => {
            evalStmts(fd.body.clone(), calleeEnv.clone(), fns.clone())
        },
        _ => {
            evalResolvedSingleExprNamed(&fd.body, calleeEnv, fns)
        }
    };
    crate::aver_generated::domain::eval::common::normalizeFnReturn(&result)
}

/// Fast path for a single expression body in slot mode.
pub fn evalResolvedSingleExprSlot(body: &aver_rt::AverList<Stmt>, calleeEnv: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    { let __list_subject = body.clone(); if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(&__list_subject) { if (rest == aver_rt::AverList::empty()) { evalResolvedSingleStmtSlot(&stmt, calleeEnv, slotMap, fns) } else { evalStmtsSlot(body, calleeEnv, slotMap, fns) } } else { evalStmtsSlot(body, calleeEnv, slotMap, fns) } }
}

/// Tail-aware version of the single-expression fast path for slot functions.
pub fn evalResolvedSingleExprSlotTail(selfId: i64, body: &aver_rt::AverList<Stmt>, slotCount: i64, slotMap: &aver_rt::AverMap<AverStr, i64>, calleeEnv: &aver_rt::AverVector<Val>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    crate::cancel_checkpoint();
    { let __list_subject = body.clone(); if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(&__list_subject) { if (rest == aver_rt::AverList::empty()) { evalTailStmtSlot(selfId, &stmt, slotCount, calleeEnv, slotMap, fns) } else { match evalStmtsSlot(body, calleeEnv, slotMap, fns) { Ok(v) => { Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { Err(e) } } } } else { match evalStmtsSlot(body, calleeEnv, slotMap, fns) { Ok(v) => { Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { Err(e) } } } }
}

/// Evaluate a final statement in slot mode, capturing self-tail-calls as loop re-entry requests.
pub fn evalTailStmtSlot(selfId: i64, stmt: &Stmt, slotCount: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<SlotTailStep, AverStr> {
    crate::cancel_checkpoint();
    match stmt.clone() {
        Stmt::StmtExpr(expr) => {
            evalTailExprSlot(selfId, &expr, slotCount, env, slotMap, fns)
        },
        _ => {
            match evalStmtsSlot(&aver_rt::AverList::from_vec(vec![stmt.clone()]), env, slotMap, fns) { Ok(v) => { Ok(SlotTailStep::SlotTailDone(v)) }, Err(e) => { Err(e) } }
        }
    }
}

/// Run the single expression stmt fast path in slot mode.
pub fn evalResolvedSingleStmtSlot(stmt: &Stmt, calleeEnv: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match stmt.clone() {
        Stmt::StmtExpr(expr) => {
            evalExprSlot(&expr, calleeEnv, slotMap, fns)
        },
        _ => {
            evalStmtsSlot(&aver_rt::AverList::from_vec(vec![stmt.clone()]), calleeEnv, slotMap, fns)
        }
    }
}

/// Fast path for a single expression body in named-env mode.
pub fn evalResolvedSingleExprNamed(body: &aver_rt::AverList<Stmt>, calleeEnv: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    { let __list_subject = body.clone(); if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(&__list_subject) { if (rest == aver_rt::AverList::empty()) { evalResolvedSingleStmtNamed(&stmt, calleeEnv, fns) } else { evalStmts(body.clone(), calleeEnv.clone(), fns.clone()) } } else { evalStmts(body.clone(), calleeEnv.clone(), fns.clone()) } }
}

/// Run the single expression stmt fast path in named-env mode.
pub fn evalResolvedSingleStmtNamed(stmt: &Stmt, calleeEnv: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match stmt.clone() {
        Stmt::StmtExpr(expr) => {
            evalExpr(&expr, calleeEnv, fns)
        },
        _ => {
            evalStmts(aver_rt::AverList::from_vec(vec![stmt.clone()]), calleeEnv.clone(), fns.clone())
        }
    }
}

/// Execute named-env leaves directly when they are pure constants; otherwise fall back.
pub fn runFastLeafNamed(leaf: &FastLeaf, body: &aver_rt::AverList<Stmt>, calleeEnv: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
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
        _ => {
            evalResolvedSingleExprNamed(body, calleeEnv, fns)
        }
    }
}

/// Forward a direct call by reading already-resolved slot arguments without evaluating an AST arg list.
pub fn fastForwardCall(calleeEnv: &aver_rt::AverVector<Val>, fnId: i64, slotArgs: &aver_rt::AverList<i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let args = collectFastForwardArgs(slotArgs.clone(), calleeEnv.clone(), aver_rt::AverList::empty())?;
    let fd = crate::aver_generated::domain::eval::store::lookupFnById(fns, fnId)?;
    callResolvedById(fnId, &fd, &args, fns)
}

/// Collect forwarded slot arguments in call order.
#[inline(always)]
pub fn collectFastForwardArgs(mut slotArgs: aver_rt::AverList<i64>, mut calleeEnv: aver_rt::AverVector<Val>, mut acc: aver_rt::AverList<Val>) -> Result<aver_rt::AverList<Val>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(slotArgs, [] => Ok(acc.reverse()), [slot, rest] => { match crate::aver_generated::domain::eval::slots::lookupSlot(&calleeEnv, slot) { Ok(v) => { {
            let __tmp2 = aver_rt::AverList::prepend(v, &acc);
            slotArgs = rest;
            acc = __tmp2;
            continue;
        } }, Err(e) => { Err(e) } } });
    }
}

/// Evaluate a function call.
pub fn evalCall(name: AverStr, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match evalArgs(argExprs, env, fns) { Err(e) => { Err(e) }, Ok(args) => { callWithArgs(&args, name, fns) } }
}

/// Call a pre-resolved function directly using the function store.
pub fn evalCallDirect(fnId: i64, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let fd = crate::aver_generated::domain::eval::store::lookupFnById(fns, fnId)?;
    if (fd.slotCount > 0i64) { evalCallDirectMapToSlot(fnId, &fd, argExprs, env, fns) } else { evalCallDirectMapToNamed(&fd, argExprs, env, fns) }
}

/// Call resolved function by evaluating args directly into the callee slot env.
pub fn evalCallDirectMapToSlot(fnId: i64, fd: &FnDef, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv = evalArgsMapToSlotEnv(argExprs.clone(), env.clone(), fns.clone(), aver_rt::AverVector::new(fd.slotCount as usize, Val::ValUnit.clone()), 0i64)?;
    evalResolvedSlotFn(fnId, fd, &calleeEnv, fns)
}

/// Call resolved function by evaluating args directly into the callee named env.
pub fn evalCallDirectMapToNamed(fd: &FnDef, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv = evalArgsMapToNamedEnv(argExprs, &fd.params, env, fns, &HashMap::new())?;
    evalResolvedNamedFn(fd, &calleeEnv, fns)
}

/// Call a builtin directly, skipping fns lookup.
pub fn evalCallBuiltin(name: AverStr, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match evalArgs(argExprs, env, fns) { Err(e) => { Err(e) }, Ok(args) => { crate::aver_generated::domain::builtins::callBuiltin(name, &args) } }
}

/// Evaluate a statement expression.
pub fn evalStmtExpr(e: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<(Val, aver_rt::AverMap<AverStr, Val>), AverStr> {
    crate::cancel_checkpoint();
    match evalExpr(e, env, fns) { Ok(v) => { Ok((v, env.clone())) }, Err(e) => { Err(e.clone()) } }
}

/// Evaluate a binding statement.
pub fn evalStmtBind(name: AverStr, e: &Expr, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<(Val, aver_rt::AverMap<AverStr, Val>), AverStr> {
    crate::cancel_checkpoint();
    match evalExpr(e, env, fns) { Ok(v) => { Ok((v.clone(), env.clone().insert_owned(name, v))) }, Err(e) => { Err(e.clone()) } }
}

/// Evaluate a statement, returning the result and updated environment.
pub fn evalStmt(stmt: &Stmt, env: &aver_rt::AverMap<AverStr, Val>, fns: &FnStore) -> Result<(Val, aver_rt::AverMap<AverStr, Val>), AverStr> {
    crate::cancel_checkpoint();
    match stmt.clone() {
        Stmt::StmtExpr(e) => {
            evalStmtExpr(&e, env, fns)
        },
        Stmt::StmtBind(name, e) => {
            evalStmtBind(name, &e, env, fns)
        },
        Stmt::StmtBindSlot(_, e) => {
            evalStmtExpr(&e, env, fns)
        }
    }
}

/// Evaluate statements, threading env. Self-recursive for codegen TCO.
pub fn evalStmts(mut stmts: aver_rt::AverList<Stmt>, mut env: aver_rt::AverMap<AverStr, Val>, mut fns: FnStore) -> Result<Val, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(stmts, [] => Ok(Val::ValUnit.clone()), [s, rest] => { match evalStmt(&s, &env, &fns) { Err(e) => { Err(e) }, Ok(pair) => { match pair {
            (v, newEnv) => { let __list_subject = rest.clone(); if __list_subject.is_empty() { Ok(v) } else { {
            stmts = rest;
            env = newEnv;
            continue;
        } } }
        } } } });
    }
}

/// Evaluate a complete program: run top-level stmts, then call main() if it exists.
pub fn evalProgram(prog: &Program) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let fnsStore = crate::aver_generated::domain::eval::store::fnsToStore(&prog.fns);
    let v = evalStmts(prog.stmts.clone(), HashMap::new(), fnsStore.clone())?;
    maybeCallMain(&fnsStore, &v)
}

/// Evaluate a program with additional functions from loaded modules.
pub fn evalProgramWithFns(prog: &Program, extraFns: &aver_rt::AverList<FnDef>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let allFns = crate::aver_generated::domain::eval::store::fnsToStore(&aver_rt::AverList::concat(&extraFns.clone(), &prog.fns.clone()));
    let v = evalStmts(prog.stmts.clone(), HashMap::new(), allFns.clone())?;
    maybeCallMain(&allFns, &v)
}

/// If a main() function exists, call it. Otherwise return fallback value.
pub fn maybeCallMain(fns: &FnStore, fallback: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::eval::store::lookupFnOption(fns, AverStr::from("main")) { Some(fd) => { callResolved(&fd, &aver_rt::AverList::empty(), fns) }, None => { Ok(fallback.clone()) } }
}

/// Recognize immediate slot and literal expressions in slot mode.
pub fn evalImmediateSlotExpr(expr: &Expr, env: &aver_rt::AverVector<Val>) -> Option<Result<Val, AverStr>> {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprSlot(slot) => {
            Some(crate::aver_generated::domain::eval::slots::lookupSlot(env, slot))
        },
        Expr::ExprInt(n) => {
            Some(Ok(Val::ValInt(n)))
        },
        Expr::ExprFloat(f) => {
            Some(Ok(Val::ValFloat(f)))
        },
        Expr::ExprStr(s) => {
            Some(Ok(Val::ValStr(s)))
        },
        Expr::ExprBool(b) => {
            Some(Ok(Val::ValBool(b)))
        },
        _ => {
            None
        }
    }
}

/// Resolve unresolved var in slot path: builtins, named function refs, and constructors.
#[inline(always)]
pub fn evalVarSlot(name: AverStr, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    { let __dispatch_subject = name.clone(); if &*__dispatch_subject == "Unit" { Ok(Val::ValUnit.clone()) } else { if &*__dispatch_subject == "Option.None" { Ok(Val::ValNone.clone()) } else { crate::aver_generated::domain::eval::common::evalVarFallback(name, fns) } } }
}

/// Evaluate a specialized slot-vs-int arithmetic expression.
pub fn evalBinopSlotInt(op: &BinOp, slot: i64, rhs: i64, env: &aver_rt::AverVector<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let left = crate::aver_generated::domain::eval::slots::lookupSlot(env, slot)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(&left, &Val::ValInt(rhs), op)
}

/// Evaluate a specialized slot-vs-slot arithmetic expression.
pub fn evalBinopSlots(op: &BinOp, lhs: i64, rhs: i64, env: &aver_rt::AverVector<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let left = crate::aver_generated::domain::eval::slots::lookupSlot(env, lhs)?;
    let right = crate::aver_generated::domain::eval::slots::lookupSlot(env, rhs)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(&left, &right, op)
}

/// Evaluate a specialized slot-vs-int comparison.
pub fn evalCmpSlotInt(op: &CmpOp, slot: i64, rhs: i64, env: &aver_rt::AverVector<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let left = crate::aver_generated::domain::eval::slots::lookupSlot(env, slot)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(&left, &Val::ValInt(rhs), op)
}

/// Evaluate a specialized slot-vs-slot comparison.
pub fn evalCmpSlots(op: &CmpOp, lhs: i64, rhs: i64, env: &aver_rt::AverVector<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let left = crate::aver_generated::domain::eval::slots::lookupSlot(env, lhs)?;
    let right = crate::aver_generated::domain::eval::slots::lookupSlot(env, rhs)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(&left, &right, op)
}

/// Evaluate a fused Vector.get + Option.withDefault in slot path.
pub fn evalVectorGetOrIntExprSlot(vecExpr: &Expr, idxExpr: &Expr, defaultValue: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let vecV = evalExprSlot(vecExpr, env, slotMap, fns)?;
    let idxV = evalExprSlot(idxExpr, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalVectorGetOrIntVals(&vecV, &idxV, defaultValue)
}

/// Evaluate a fused Int.mod + Result.withDefault in slot path.
pub fn evalIntModOrIntExprSlot(a: &Expr, b: &Expr, defaultValue: i64, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let aV = evalExprSlot(a, env, slotMap, fns)?;
    let bV = evalExprSlot(b, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalIntModOrIntVals(&aV, &bV, defaultValue)
}

/// Binary op in slot path.
pub fn evalBinopSlot(a: &Expr, b: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore, op: &BinOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let va = evalExprSlot(a, env, slotMap, fns)?;
    let vb = evalExprSlot(b, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalBinopVals(&va, &vb, op)
}

/// Comparison in slot path.
pub fn evalCmpSlot(a: &Expr, b: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore, op: &CmpOp) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let va = evalExprSlot(a, env, slotMap, fns)?;
    let vb = evalExprSlot(b, env, slotMap, fns)?;
    crate::aver_generated::domain::eval::ops::evalCmpVals(&va, &vb, op)
}

/// String interpolation in slot path.
pub fn evalConcatSlot(parts: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    evalConcatPartsSlot(parts.clone(), env.clone(), slotMap.clone(), fns.clone(), AverStr::from(""))
}

/// Concatenate interpolation parts in slot path.
pub fn evalConcatPartsSlot(mut parts: aver_rt::AverList<Expr>, mut env: aver_rt::AverVector<Val>, mut slotMap: aver_rt::AverMap<AverStr, i64>, mut fns: FnStore, mut acc: AverStr) -> Result<Val, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(parts, [] => Ok(Val::ValStr(acc)), [p, rest] => { match evalExprSlot(&p, &env, &slotMap, &fns) { Err(e) => { Err(e) }, Ok(v) => { {
            let __tmp4 = (acc + &crate::aver_generated::domain::value::valRepr(&v));
            parts = rest;
            acc = __tmp4;
            continue;
        } } } });
    }
}

/// Tuple literal in slot path.
pub fn evalTupleSlot(exprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = evalListItemsSlot(exprs, env, slotMap, fns)?;
    Ok(Val::ValTuple(items))
}

/// List literal in slot path.
pub fn evalListSlot(exprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = evalListItemsSlot(exprs, env, slotMap, fns)?;
    Ok(Val::ValList(items))
}

/// Evaluate list of expressions in slot path.
pub fn evalListItemsSlot(exprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<aver_rt::AverList<Val>, AverStr> {
    crate::cancel_checkpoint();
    evalListItemsSlotRev(exprs.clone(), env.clone(), slotMap.clone(), fns.clone(), aver_rt::AverList::empty())
}

/// Tail-recursive worker for evalListItemsSlot. Accumulates in reverse.
pub fn evalListItemsSlotRev(mut exprs: aver_rt::AverList<Expr>, mut env: aver_rt::AverVector<Val>, mut slotMap: aver_rt::AverMap<AverStr, i64>, mut fns: FnStore, mut acc: aver_rt::AverList<Val>) -> Result<aver_rt::AverList<Val>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => Ok(acc.reverse()), [e, rest] => { match evalExprSlot(&e, &env, &slotMap, &fns) { Err(err) => { Err(err) }, Ok(v) => { {
            let __tmp4 = aver_rt::AverList::prepend(v, &acc);
            exprs = rest;
            acc = __tmp4;
            continue;
        } } } });
    }
}

/// Record constructor in slot path.
pub fn evalRecordSlot(name: AverStr, fieldExprs: &aver_rt::AverList<(AverStr, Expr)>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let fields = evalRecordFieldsSlot(fieldExprs, env, slotMap, fns)?;
    Ok(Val::ValRecord(name, fields))
}

/// Evaluate record fields in slot path.
pub fn evalRecordFieldsSlot(fieldExprs: &aver_rt::AverList<(AverStr, Expr)>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(fieldExprs.clone(), [] => Ok(aver_rt::AverList::empty()), [pair, rest] => { { let (fname, expr) = pair; evalRecordFieldOneSlot(fname, &expr, &rest, env, slotMap, fns) } })
}

/// Evaluate one record field in slot path.
pub fn evalRecordFieldOneSlot(fname: AverStr, expr: &Expr, rest: &aver_rt::AverList<(AverStr, Expr)>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<aver_rt::AverList<(AverStr, Val)>, AverStr> {
    crate::cancel_checkpoint();
    let v = evalExprSlot(expr, env, slotMap, fns)?;
    let restFields = evalRecordFieldsSlot(rest, env, slotMap, fns)?;
    Ok(aver_rt::AverList::prepend((fname, v), &restFields))
}

/// Field access in slot path.
pub fn evalFieldAccessSlot(obj: &Expr, field: AverStr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = evalExprSlot(obj, env, slotMap, fns)?;
    match v {
        Val::ValRecord(_, fields) => {
            crate::aver_generated::domain::eval::common::lookupField(fields, field)
        },
        _ => {
            Err(AverStr::from("field access on non-record"))
        }
    }
}

/// Function call in slot path.
pub fn evalCallSlot(name: AverStr, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let args = evalArgsSlot(argExprs, env, slotMap, fns)?;
    callWithArgs(&args, name, fns)
}

/// Call a pre-resolved function directly from slot mode using the function store.
pub fn evalCallDirectSlot(fnId: i64, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let fd = crate::aver_generated::domain::eval::store::lookupFnById(fns, fnId)?;
    if (fd.slotCount > 0i64) { evalCallDirectSlotToSlot(fnId, &fd, argExprs, env, slotMap, fns) } else { evalCallDirectSlotToNamed(&fd, argExprs, env, slotMap, fns) }
}

/// Call resolved function by evaluating args directly into a callee slot env from slot caller state.
pub fn evalCallDirectSlotToSlot(fnId: i64, fd: &FnDef, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv = evalArgsSlotToSlotEnv(argExprs.clone(), env.clone(), slotMap.clone(), fns.clone(), aver_rt::AverVector::new(fd.slotCount as usize, Val::ValUnit.clone()), 0i64)?;
    evalResolvedSlotFn(fnId, fd, &calleeEnv, fns)
}

/// Call resolved function by evaluating args directly into a named env from slot caller state.
pub fn evalCallDirectSlotToNamed(fd: &FnDef, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let calleeEnv = evalArgsSlotToNamedEnv(argExprs, &fd.params, env, slotMap, fns, &HashMap::new())?;
    evalResolvedNamedFn(fd, &calleeEnv, fns)
}

/// Call a builtin directly, skipping fns lookup (slot-based path).
pub fn evalCallBuiltinSlot(name: AverStr, argExprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let args = evalArgsSlot(argExprs, env, slotMap, fns)?;
    crate::aver_generated::domain::builtins::callBuiltin(name.clone(), &args)
}

/// Evaluate argument expressions in slot path.
pub fn evalArgsSlot(exprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<aver_rt::AverList<Val>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(exprs.clone(), [] => Ok(aver_rt::AverList::empty()), [e0, rest] => { aver_list_match!(rest, [] => evalArgsSlot1(&e0, env, slotMap, fns), [e1, rest2] => { aver_list_match!(rest2, [] => evalArgsSlot2(&e0, &e1, env, slotMap, fns), [e2, rest3] => { { let __list_subject = rest3; if __list_subject.is_empty() { evalArgsSlot3(&e0, &e1, &e2, env, slotMap, fns) } else { evalArgsSlotRev(exprs.clone(), env.clone(), slotMap.clone(), fns.clone(), aver_rt::AverList::empty()) } } }) }) })
}

/// Fast path for one slot-path argument.
pub fn evalArgsSlot1(e0: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<aver_rt::AverList<Val>, AverStr> {
    crate::cancel_checkpoint();
    let v0 = evalExprSlot(e0, env, slotMap, fns)?;
    Ok(aver_rt::AverList::from_vec(vec![v0]))
}

/// Fast path for two slot-path arguments.
pub fn evalArgsSlot2(e0: &Expr, e1: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<aver_rt::AverList<Val>, AverStr> {
    crate::cancel_checkpoint();
    let v0 = evalExprSlot(e0, env, slotMap, fns)?;
    let v1 = evalExprSlot(e1, env, slotMap, fns)?;
    Ok(aver_rt::AverList::from_vec(vec![v0, v1]))
}

/// Fast path for three slot-path arguments.
pub fn evalArgsSlot3(e0: &Expr, e1: &Expr, e2: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<aver_rt::AverList<Val>, AverStr> {
    crate::cancel_checkpoint();
    let v0 = evalExprSlot(e0, env, slotMap, fns)?;
    let v1 = evalExprSlot(e1, env, slotMap, fns)?;
    let v2 = evalExprSlot(e2, env, slotMap, fns)?;
    Ok(aver_rt::AverList::from_vec(vec![v0, v1, v2]))
}

/// Tail-recursive worker for evalArgsSlot. Accumulates in reverse.
pub fn evalArgsSlotRev(mut exprs: aver_rt::AverList<Expr>, mut env: aver_rt::AverVector<Val>, mut slotMap: aver_rt::AverMap<AverStr, i64>, mut fns: FnStore, mut acc: aver_rt::AverList<Val>) -> Result<aver_rt::AverList<Val>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => Ok(acc.reverse()), [e, rest] => { match evalExprSlot(&e, &env, &slotMap, &fns) { Err(err) => { Err(err) }, Ok(v) => { {
            let __tmp4 = aver_rt::AverList::prepend(v, &acc);
            exprs = rest;
            acc = __tmp4;
            continue;
        } } } });
    }
}

/// Evaluate arg expressions directly into a callee slot env from map caller state.
pub fn evalArgsMapToSlotEnv(mut exprs: aver_rt::AverList<Expr>, mut env: aver_rt::AverMap<AverStr, Val>, mut fns: FnStore, mut acc: aver_rt::AverVector<Val>, mut idx: i64) -> Result<aver_rt::AverVector<Val>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => Ok(acc), [e, rest] => { match evalExpr(&e, &env, &fns) { Err(err) => { Err(err) }, Ok(v) => { {
            let __tmp3 = crate::aver_generated::domain::eval::slots::setSlot(&acc, idx, &v);
            let __tmp4 = (idx + 1i64);
            exprs = rest;
            acc = __tmp3;
            idx = __tmp4;
            continue;
        } } } });
    }
}

/// Evaluate arg expressions directly into a callee slot env from slot caller state.
pub fn evalArgsSlotToSlotEnv(mut exprs: aver_rt::AverList<Expr>, mut env: aver_rt::AverVector<Val>, mut slotMap: aver_rt::AverMap<AverStr, i64>, mut fns: FnStore, mut acc: aver_rt::AverVector<Val>, mut idx: i64) -> Result<aver_rt::AverVector<Val>, AverStr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => Ok(acc), [e, rest] => { match evalExprSlot(&e, &env, &slotMap, &fns) { Err(err) => { Err(err) }, Ok(v) => { {
            let __tmp4 = crate::aver_generated::domain::eval::slots::setSlot(&acc, idx, &v);
            let __tmp5 = (idx + 1i64);
            exprs = rest;
            acc = __tmp4;
            idx = __tmp5;
            continue;
        } } } });
    }
}

/// Evaluate ? operator in slot path.
pub fn evalPropagateSlot(inner: &Expr, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = evalExprSlot(inner, env, slotMap, fns)?;
    match v {
        Val::ValOk(x) => {
            let x = (*x).clone();
            Ok(x)
        },
        Val::ValErr(e) => {
            let e = (*e).clone();
            match e {
        Val::ValStr(msg) => {
            Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(msg))
        },
        _ => {
            Err(crate::aver_generated::domain::eval::common::wrapPropagatedError(AverStr::from("propagated error")))
        }
    }
        },
        _ => {
            Err(AverStr::from("? operator requires Result value"))
        }
    }
}

/// Evaluate independent product in slot path using the self-host's own divide-and-conquer ?!, then unwrap guest Results if needed.
pub fn evalIndependentProductSlot(exprs: &aver_rt::AverList<Expr>, unwrap: bool, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = evalIndependentItemsSlot(exprs, env, slotMap, fns)?;
    if unwrap { unwrapProductResults(items, aver_rt::AverList::empty()) } else { Ok(Val::ValTuple(items)) }
}

/// Evaluate guest independent-product branches in slot path through a balanced tree of the self-host's own ?!.
pub fn evalIndependentItemsSlot(exprs: &aver_rt::AverList<Expr>, env: &aver_rt::AverVector<Val>, slotMap: &aver_rt::AverMap<AverStr, i64>, fns: &FnStore) -> Result<aver_rt::AverList<Val>, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(exprs.clone(), [] => Ok(aver_rt::AverList::empty()), [a, rest] => { aver_list_match!(rest, [] => match evalExprSlot(&a, env, slotMap, fns) { Ok(va) => { Ok(aver_rt::AverList::from_vec(vec![va])) }, Err(e) => { Err(e) } }, [b, rest2] => { { let __list_subject = rest2; if __list_subject.is_empty() { { let (va, vb) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let _r0 = evalExprSlot(&a, env, slotMap, fns); crate::aver_replay::set_effect_branch(1); let _r1 = evalExprSlot(&b, env, slotMap, fns); crate::aver_replay::exit_effect_group(); match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|_s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let _h0 = _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = evalExprSlot(&a, env, slotMap, fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })); let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let _h1 = _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = evalExprSlot(&b, env, slotMap, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })); let _b0 = _h0.join().unwrap(); let _b1 = _h1.join().unwrap(); match (_b0, _b1) { (crate::ParallelBranch::Completed(_r0), crate::ParallelBranch::Completed(_r1)) => match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (_b0, _b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = _b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = _b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok(aver_rt::AverList::from_vec(vec![va, vb])) } } else { { let (leftExprs, rightExprs) = splitIndependentExprs(exprs.clone(), aver_rt::AverList::empty(), aver_rt::AverList::empty(), true); { let (leftVals, rightVals) = if crate::aver_replay::is_effect_tracking_active() { crate::aver_replay::enter_effect_group(); crate::aver_replay::set_effect_branch(0); let _r0 = evalIndependentItemsSlot(&leftExprs, env, slotMap, fns); crate::aver_replay::set_effect_branch(1); let _r1 = evalIndependentItemsSlot(&rightExprs, env, slotMap, fns); crate::aver_replay::exit_effect_group(); match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }? } else { { let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); std::thread::scope(|_s| { let __parallel_scope0 = __parallel_scope.clone(); let __cancel_flag0 = __cancel_flag.clone(); let _h0 = _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope0.clone(), move || { crate::run_cancelable_branch(__cancel_flag0.clone(), move || { let __result = evalIndependentItemsSlot(&leftExprs, env, slotMap, fns); if let Err(_) = &__result { __cancel_flag0.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })); let __parallel_scope1 = __parallel_scope.clone(); let __cancel_flag1 = __cancel_flag.clone(); let _h1 = _s.spawn(move || crate::aver_replay::with_parallel_scope_context(__parallel_scope1.clone(), move || { crate::run_cancelable_branch(__cancel_flag1.clone(), move || { let __result = evalIndependentItemsSlot(&rightExprs, env, slotMap, fns); if let Err(_) = &__result { __cancel_flag1.store(true, std::sync::atomic::Ordering::Relaxed); } __result }) })); let _b0 = _h0.join().unwrap(); let _b1 = _h1.join().unwrap(); match (_b0, _b1) { (crate::ParallelBranch::Completed(_r0), crate::ParallelBranch::Completed(_r1)) => match (_r0, _r1) { (Ok(__v0), Ok(__v1)) => Ok((__v0, __v1)), (_r0, _r1) => { if let Err(__err) = _r0 { Err(__err) } else if let Err(__err) = _r1 { Err(__err) } else { unreachable!("independent product unwrap requires Result branches") } } }, (_b0, _b1) => { if let crate::ParallelBranch::Completed(Err(__err)) = _b0 { Err(__err) } else if let crate::ParallelBranch::Completed(Err(__err)) = _b1 { Err(__err) } else { panic!("independent product branch cancelled by sibling branch") } } } })? }}; Ok(interleaveIndependentVals(leftVals, rightVals, true, aver_rt::AverList::empty())) } } } } }) })
}
