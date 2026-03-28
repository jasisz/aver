use crate::ast::{Expr, FnBody, Stmt};

use super::{
    CallLowerCtx, CallPlan, ForwardCallPlan, LeafOp, classify_call_plan,
    classify_forward_call_plan, classify_leaf_op,
};

/// Minimal body-level semantic IR shared across backends.
///
/// This first slice is intentionally narrow: it only recognizes single-expression
/// function bodies and simple binding blocks ending in a tail expression.
/// That is enough to start driving backend emission from a body plan instead of
/// re-discovering call/leaf structure inside each emitter.
#[derive(Debug, Clone, PartialEq)]
pub enum BodyExprPlan<'a> {
    Expr(&'a Expr),
    Leaf(LeafOp<'a>),
    Call { target: CallPlan, args: &'a [Expr] },
    ForwardCall(ForwardCallPlan),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BodyBindingPlan<'a> {
    pub name: &'a str,
    pub expr: BodyExprPlan<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyPlan<'a> {
    SingleExpr(BodyExprPlan<'a>),
    Block {
        stmts: &'a [Stmt],
        bindings: Vec<BodyBindingPlan<'a>>,
        tail: BodyExprPlan<'a>,
    },
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
    let stmts = body.stmts();
    let Some((tail_stmt, prefix)) = stmts.split_last() else {
        return None;
    };

    let Stmt::Expr(tail_expr) = tail_stmt else {
        return None;
    };

    if prefix.is_empty() {
        return Some(BodyPlan::SingleExpr(classify_body_expr_plan(
            tail_expr, ctx,
        )));
    }

    let mut bindings = Vec::with_capacity(prefix.len());
    for stmt in prefix {
        let Stmt::Binding(name, _type_ann, expr) = stmt else {
            return None;
        };
        bindings.push(BodyBindingPlan {
            name,
            expr: classify_body_expr_plan(expr, ctx),
        });
    }

    Some(BodyPlan::Block {
        stmts,
        bindings,
        tail: classify_body_expr_plan(tail_expr, ctx),
    })
}
