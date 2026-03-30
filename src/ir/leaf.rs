use crate::ast::{Expr, Literal, Spanned};

use super::{CallLowerCtx, CallPlan, classify_call_plan, expr_to_dotted_name};

/// Small expression-shaped leaf operations shared across backends.
///
/// These are still semantic plans, not backend instructions: they recognize
/// common Aver expression shapes whose meaning is clearer than the raw AST.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LeafOp<'a> {
    FieldAccess {
        object: &'a Spanned<Expr>,
        field_name: &'a str,
    },
    MapGet {
        map: &'a Spanned<Expr>,
        key: &'a Spanned<Expr>,
    },
    MapSet {
        map: &'a Spanned<Expr>,
        key: &'a Spanned<Expr>,
        value: &'a Spanned<Expr>,
    },
    VectorNew {
        size: &'a Spanned<Expr>,
        fill: &'a Spanned<Expr>,
    },
    VectorSetOrDefaultSameVector {
        vector: &'a Spanned<Expr>,
        index: &'a Spanned<Expr>,
        value: &'a Spanned<Expr>,
    },
    VectorGetOrDefaultLiteral {
        vector: &'a Spanned<Expr>,
        index: &'a Spanned<Expr>,
        default_literal: &'a Literal,
    },
    /// Fused `Result.withDefault(Int.mod(a, b), literal)` → skip Result allocation.
    IntModOrDefaultLiteral {
        a: &'a Spanned<Expr>,
        b: &'a Spanned<Expr>,
        default_literal: &'a Literal,
    },
    /// Fused `Vector.get(Vector.fromList(list), index)` → skip AverVector allocation.
    ListIndexGet {
        list: &'a Spanned<Expr>,
        index: &'a Spanned<Expr>,
    },
}

pub fn classify_leaf_op<'a>(expr: &'a Expr, ctx: &impl CallLowerCtx) -> Option<LeafOp<'a>> {
    match expr {
        Expr::Attr(object, field_name) => classify_field_access(expr, object, field_name),
        Expr::FnCall(fn_expr, args) => classify_leaf_call(&fn_expr.node, args, ctx),
        _ => None,
    }
}

fn classify_field_access<'a>(
    full_expr: &'a Expr,
    object: &'a Spanned<Expr>,
    field_name: &'a str,
) -> Option<LeafOp<'a>> {
    // Uppercase dotted paths are static module/type/builtin references, not
    // runtime record field access.
    if expr_to_dotted_name(full_expr).is_some_and(|dotted| {
        dotted
            .chars()
            .next()
            .is_some_and(|first| first.is_uppercase())
    }) {
        return None;
    }

    Some(LeafOp::FieldAccess { object, field_name })
}

fn classify_leaf_call<'a>(
    fn_expr: &'a Expr,
    args: &'a [Spanned<Expr>],
    ctx: &impl CallLowerCtx,
) -> Option<LeafOp<'a>> {
    match classify_call_plan(fn_expr, ctx) {
        CallPlan::Builtin(name) => match name.as_str() {
            "Map.get" if args.len() == 2 => Some(LeafOp::MapGet {
                map: &args[0],
                key: &args[1],
            }),
            "Map.set" if args.len() == 3 => Some(LeafOp::MapSet {
                map: &args[0],
                key: &args[1],
                value: &args[2],
            }),
            "Vector.new" if args.len() == 2 => Some(LeafOp::VectorNew {
                size: &args[0],
                fill: &args[1],
            }),
            "Vector.get" if args.len() == 2 => classify_list_index_get(&args[0], &args[1], ctx),
            "Option.withDefault" if args.len() == 2 => {
                classify_vector_set_or_default(&args[0], &args[1], ctx)
                    .or_else(|| classify_vector_get_or_default(&args[0], &args[1], ctx))
            }
            "Result.withDefault" if args.len() == 2 => {
                classify_int_mod_or_default(&args[0], &args[1], ctx)
            }
            _ => None,
        },
        _ => None,
    }
}

fn classify_vector_set_or_default<'a>(
    option_expr: &'a Spanned<Expr>,
    default_expr: &'a Spanned<Expr>,
    ctx: &impl CallLowerCtx,
) -> Option<LeafOp<'a>> {
    let Expr::FnCall(inner_callee, inner_args) = &option_expr.node else {
        return None;
    };
    if inner_args.len() != 3 {
        return None;
    }

    match classify_call_plan(&inner_callee.node, ctx) {
        CallPlan::Builtin(name) if name == "Vector.set" && default_expr == &inner_args[0] => {
            Some(LeafOp::VectorSetOrDefaultSameVector {
                vector: &inner_args[0],
                index: &inner_args[1],
                value: &inner_args[2],
            })
        }
        _ => None,
    }
}

fn classify_vector_get_or_default<'a>(
    option_expr: &'a Spanned<Expr>,
    default_expr: &'a Spanned<Expr>,
    ctx: &impl CallLowerCtx,
) -> Option<LeafOp<'a>> {
    let default_literal = match &default_expr.node {
        Expr::Literal(lit) => lit,
        _ => return None,
    };

    let Expr::FnCall(inner_callee, inner_args) = &option_expr.node else {
        return None;
    };
    if inner_args.len() != 2 {
        return None;
    }

    match classify_call_plan(&inner_callee.node, ctx) {
        CallPlan::Builtin(name) if name == "Vector.get" => {
            Some(LeafOp::VectorGetOrDefaultLiteral {
                vector: &inner_args[0],
                index: &inner_args[1],
                default_literal,
            })
        }
        _ => None,
    }
}

fn classify_list_index_get<'a>(
    vector_expr: &'a Spanned<Expr>,
    index: &'a Spanned<Expr>,
    ctx: &impl CallLowerCtx,
) -> Option<LeafOp<'a>> {
    // Match Vector.get(Vector.fromList(list), index)
    let Expr::FnCall(inner_callee, inner_args) = &vector_expr.node else {
        return None;
    };
    if inner_args.len() != 1 {
        return None;
    }
    match classify_call_plan(&inner_callee.node, ctx) {
        CallPlan::Builtin(name) if name == "Vector.fromList" => Some(LeafOp::ListIndexGet {
            list: &inner_args[0],
            index,
        }),
        _ => None,
    }
}

fn classify_int_mod_or_default<'a>(
    result_expr: &'a Spanned<Expr>,
    default_expr: &'a Spanned<Expr>,
    ctx: &impl CallLowerCtx,
) -> Option<LeafOp<'a>> {
    let default_literal = match &default_expr.node {
        Expr::Literal(lit) => lit,
        _ => return None,
    };

    let Expr::FnCall(inner_callee, inner_args) = &result_expr.node else {
        return None;
    };
    if inner_args.len() != 2 {
        return None;
    }

    match classify_call_plan(&inner_callee.node, ctx) {
        CallPlan::Builtin(name) if name == "Int.mod" => Some(LeafOp::IntModOrDefaultLiteral {
            a: &inner_args[0],
            b: &inner_args[1],
            default_literal,
        }),
        _ => None,
    }
}
