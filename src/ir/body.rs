use crate::ast::{Expr, FnBody, Stmt};

use super::{
    CallLowerCtx, CallPlan, ForwardCallPlan, LeafOp, classify_call_plan,
    classify_forward_call_plan, classify_leaf_op,
};

/// Minimal body-level semantic IR shared across backends.
///
/// This first slice is intentionally narrow: it only recognizes single-expression
/// function bodies. That is enough to start driving backend emission from a
/// body plan instead of re-discovering call/leaf structure inside each emitter.
#[derive(Debug, Clone, PartialEq)]
pub enum BodyExprPlan<'a> {
    Expr(&'a Expr),
    Leaf(LeafOp<'a>),
    Call { target: CallPlan, args: &'a [Expr] },
    ForwardCall(ForwardCallPlan),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyPlan<'a> {
    SingleExpr(BodyExprPlan<'a>),
}

pub fn classify_body_expr_plan<'a>(expr: &'a Expr, ctx: &impl CallLowerCtx) -> BodyExprPlan<'a> {
    if let Some(leaf) = classify_leaf_op(expr, ctx) {
        return BodyExprPlan::Leaf(leaf);
    }

    if let Some(plan) = classify_forward_call_plan(expr, ctx) {
        return BodyExprPlan::ForwardCall(plan);
    }

    if let Expr::FnCall(fn_expr, args) = expr {
        let target = classify_call_plan(fn_expr, ctx);
        if !matches!(target, CallPlan::Dynamic) {
            return BodyExprPlan::Call { target, args };
        }
    }

    BodyExprPlan::Expr(expr)
}

pub fn classify_body_plan<'a>(body: &'a FnBody, ctx: &impl CallLowerCtx) -> Option<BodyPlan<'a>> {
    let [Stmt::Expr(expr)] = body.stmts() else {
        return None;
    };
    Some(BodyPlan::SingleExpr(classify_body_expr_plan(expr, ctx)))
}
