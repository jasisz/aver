use crate::ast::{Expr, FnBody, FnDef, Stmt};

use super::{
    CallLowerCtx, CallPlan, ForwardCallPlan, LeafOp, classify_call_plan,
    classify_forward_call_plan, classify_leaf_op, classify_match_dispatch_plan,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThinKind {
    Leaf,
    Direct,
    Forward,
    Dispatch,
    Tail,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThinBodyPlan<'a> {
    pub params: &'a [(String, String)],
    pub body: BodyPlan<'a>,
    pub kind: ThinKind,
}

pub trait ThinBodyCtx: CallLowerCtx {
    fn find_fn_def<'a>(&'a self, name: &str) -> Option<&'a FnDef>;
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
    let (tail_stmt, prefix) = stmts.split_last()?;

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

pub fn classify_thin_body_plan<'a>(
    name: &str,
    ctx: &'a impl ThinBodyCtx,
) -> Option<ThinBodyPlan<'a>> {
    let fd = ctx.find_fn_def(name)?;
    classify_thin_fn_def(fd, ctx)
}

pub fn classify_thin_fn_def<'a>(
    fd: &'a FnDef,
    ctx: &impl CallLowerCtx,
) -> Option<ThinBodyPlan<'a>> {
    let body = classify_body_plan(&fd.body, ctx)?;
    Some(ThinBodyPlan {
        params: &fd.params,
        kind: classify_thin_kind(&body, ctx)?,
        body,
    })
}

fn classify_thin_kind(plan: &BodyPlan<'_>, ctx: &impl CallLowerCtx) -> Option<ThinKind> {
    match plan {
        BodyPlan::SingleExpr(expr) => classify_thin_expr_kind(expr, ctx),
        BodyPlan::Block { bindings, tail, .. } => {
            if bindings
                .iter()
                .all(|binding| body_expr_is_thin_binding(&binding.expr))
            {
                classify_thin_expr_kind(tail, ctx)
            } else {
                None
            }
        }
    }
}

fn classify_thin_expr_kind(plan: &BodyExprPlan<'_>, ctx: &impl CallLowerCtx) -> Option<ThinKind> {
    match plan {
        BodyExprPlan::Leaf(_) => Some(ThinKind::Leaf),
        BodyExprPlan::Call { .. } => Some(ThinKind::Direct),
        BodyExprPlan::ForwardCall(_) => Some(ThinKind::Forward),
        BodyExprPlan::Expr(expr) => match expr {
            Expr::Match { arms, .. } if classify_match_dispatch_plan(arms, ctx).is_some() => {
                Some(ThinKind::Dispatch)
            }
            Expr::TailCall(_) => Some(ThinKind::Tail),
            _ => None,
        },
    }
}

fn body_expr_is_thin_binding(plan: &BodyExprPlan<'_>) -> bool {
    match plan {
        BodyExprPlan::Leaf(_) | BodyExprPlan::Call { .. } | BodyExprPlan::ForwardCall(_) => true,
        BodyExprPlan::Expr(expr) => matches!(
            expr,
            Expr::Literal(_) | Expr::Ident(_) | Expr::Constructor(_, _)
        ),
    }
}
