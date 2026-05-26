//! VM-local structural classifiers over [`ResolvedExpr`] /
//! [`ResolvedPattern`].
//!
//! The shared IR classifiers in `crate::ir::{calls, leaf, matches}`
//! still operate on the pre-Phase-E `Expr` shape — they're consumed
//! by the other backends (Rust, wasm-gc, Lean, Dafny, self-host)
//! that haven't migrated yet. The VM compiler has moved to
//! `ResolvedExpr` / `ResolvedPattern` (#147 phase E PR 7), so it
//! needs parallel shape recognisers that don't drag a `&Expr`
//! conversion through every fast path.
//!
//! These mirror the IR classifiers' optimisation menu 1-for-1
//! (`Vector.get` → `LeafOp::FieldAccess`, dispatch-table arms →
//! `MATCH_DISPATCH`, etc.) so the bytecode the VM emits is
//! byte-identical to the pre-migration build. The IR classifiers
//! stay the source of truth for the recognised shapes; this file
//! re-encodes that menu against the resolved AST without re-doing
//! the identity work the resolver pass already finished.

use crate::ast::Literal;
use crate::ir::hir::{
    BuiltinCtor, ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedMatchArm, ResolvedPattern,
};
use crate::ir::{
    BoolCompareOp, BoolMatchShape, DispatchArmPlan, DispatchBindingPlan, DispatchDefaultPlan,
    DispatchLiteral, DispatchTableShape, ListMatchShape, MatchDispatchPlan,
    SemanticDispatchPattern, WrapperKind,
};

/// Mirror of [`crate::ir::LeafOp`] keyed against `ResolvedExpr`.
///
/// Each variant's fields are deliberately preserved (even when the
/// VM compiler currently consumes the leaf by walking the original
/// `ResolvedExpr` instead of these borrows) — keeps the shape
/// documentation aligned with `crate::ir::LeafOp` for cross-reading
/// and leaves room for future consumers that prefer the typed
/// borrows over re-walking. Suppress the dead-code lint at the type
/// level rather than per-field so the docs stay flat.
#[allow(dead_code)]
pub enum ResolvedLeafOp<'a> {
    FieldAccess {
        object: &'a crate::ast::Spanned<ResolvedExpr>,
        field_name: &'a str,
    },
    MapGet {
        map: &'a crate::ast::Spanned<ResolvedExpr>,
        key: &'a crate::ast::Spanned<ResolvedExpr>,
    },
    MapSet {
        map: &'a crate::ast::Spanned<ResolvedExpr>,
        key: &'a crate::ast::Spanned<ResolvedExpr>,
        value: &'a crate::ast::Spanned<ResolvedExpr>,
    },
    VectorNew {
        size: &'a crate::ast::Spanned<ResolvedExpr>,
        fill: &'a crate::ast::Spanned<ResolvedExpr>,
    },
    VectorSetOrDefaultSameVector {
        vector: &'a crate::ast::Spanned<ResolvedExpr>,
        index: &'a crate::ast::Spanned<ResolvedExpr>,
        value: &'a crate::ast::Spanned<ResolvedExpr>,
    },
    VectorGetOrDefaultLiteral {
        vector: &'a crate::ast::Spanned<ResolvedExpr>,
        index: &'a crate::ast::Spanned<ResolvedExpr>,
        default_literal: &'a Literal,
    },
    IntModOrDefaultLiteral {
        a: &'a crate::ast::Spanned<ResolvedExpr>,
        b: &'a crate::ast::Spanned<ResolvedExpr>,
        default_literal: &'a Literal,
    },
    ListIndexGet {
        list: &'a crate::ast::Spanned<ResolvedExpr>,
        index: &'a crate::ast::Spanned<ResolvedExpr>,
    },
    NoneValue,
    VariantConstructor {
        qualified_type_name: String,
        variant_name: String,
    },
    StaticRef(String),
}

/// Bool-subject classifier output, mirror of
/// [`crate::ir::BoolSubjectPlan`].
#[allow(dead_code)]
pub enum ResolvedBoolSubjectPlan<'a> {
    Expr(&'a ResolvedExpr),
    Compare {
        lhs: &'a crate::ast::Spanned<ResolvedExpr>,
        rhs: &'a crate::ast::Spanned<ResolvedExpr>,
        op: BoolCompareOp,
        invert: bool,
    },
}

/// Forward-call shape mirror — the resolved callee + per-arg
/// forwarding slot when every arg is a `Resolved` local. The
/// `callee` field is kept on the plan even though the current
/// consumer re-fetches it from the original `Spanned<ResolvedExpr>`
/// — keeps the shape self-describing for future readers.
#[allow(dead_code)]
pub struct ResolvedForwardCallPlan<'a> {
    pub callee: &'a ResolvedCallee,
    pub forward_slots: Vec<ForwardSlot>,
}

pub enum ForwardSlot {
    Local { slot: u16 },
}

/// Recognise an expression as one of the fused leaf shapes.
/// Mirrors [`crate::ir::classify_leaf_op`] structurally; identity
/// classification (`Builtin` / `Fn` / ...) is read directly off
/// [`ResolvedCallee`] instead of re-derived from a string.
pub fn classify_leaf_op_resolved<'a>(
    expr: &'a ResolvedExpr,
    is_user_type: &impl Fn(&str) -> bool,
) -> Option<ResolvedLeafOp<'a>> {
    match expr {
        ResolvedExpr::Attr(object, field_name) => {
            classify_field_access(expr, object, field_name, is_user_type)
        }
        ResolvedExpr::Call(callee, args) => classify_leaf_call(callee, args, is_user_type),
        ResolvedExpr::Ctor(ctor, args) if args.is_empty() => match ctor {
            ResolvedCtor::Builtin(BuiltinCtor::OptionNone) => Some(ResolvedLeafOp::NoneValue),
            ResolvedCtor::User {
                type_id: _,
                name,
                ctor_id: _,
            } => {
                // Nullary user ctor in non-call position. Reconstruct
                // the qualified type name from the surrounding
                // expression context — the ctor carries only the
                // variant name and a TypeId, not the source dotted
                // form. The VM compiler's `leaf_op` consumer uses
                // the dotted name to look up the variant in the
                // arena; the typecheck-time qualified name is
                // recoverable via the resolver's stored type_id but
                // here we don't have access to it. Skip for now;
                // the call-shape compiler path catches nullary
                // ctors anyway via `compile_constructor`.
                let _ = name;
                None
            }
            _ => None,
        },
        _ => None,
    }
}

fn classify_field_access<'a>(
    full_expr: &'a ResolvedExpr,
    object: &'a crate::ast::Spanned<ResolvedExpr>,
    field_name: &'a str,
    _is_user_type: &impl Fn(&str) -> bool,
) -> Option<ResolvedLeafOp<'a>> {
    // Walk to detect an uppercase dotted path (matches the IR
    // classifier's heuristic). If the field access is on a value
    // expression (e.g. a record), emit the generic FieldAccess.
    let dotted = resolved_to_dotted(full_expr);
    let starts_upper = dotted
        .as_deref()
        .and_then(|d| d.chars().next())
        .is_some_and(|c| c.is_uppercase());

    if !starts_upper {
        return Some(ResolvedLeafOp::FieldAccess { object, field_name });
    }
    // Uppercase dotted path: classify as a static module/builtin
    // reference. The VM has full namespace state in its symbol
    // table, so the consumer (`compile_leaf_op`) can resolve the
    // dotted path at emit time. We pass through the dotted form so
    // the existing emitter logic stays unchanged.
    dotted.map(ResolvedLeafOp::StaticRef)
}

fn classify_leaf_call<'a>(
    callee: &'a ResolvedCallee,
    args: &'a [crate::ast::Spanned<ResolvedExpr>],
    is_user_type: &impl Fn(&str) -> bool,
) -> Option<ResolvedLeafOp<'a>> {
    let builtin_name = match callee {
        ResolvedCallee::Builtin(name) => name.as_str(),
        _ => return None,
    };
    match (builtin_name, args.len()) {
        ("Map.get", 2) => Some(ResolvedLeafOp::MapGet {
            map: &args[0],
            key: &args[1],
        }),
        ("Map.set", 3) => Some(ResolvedLeafOp::MapSet {
            map: &args[0],
            key: &args[1],
            value: &args[2],
        }),
        ("Vector.new", 2) => Some(ResolvedLeafOp::VectorNew {
            size: &args[0],
            fill: &args[1],
        }),
        ("Vector.get", 2) => classify_list_index_get(&args[0], &args[1]),
        ("Option.withDefault", 2) => classify_vector_set_or_default(&args[0], &args[1])
            .or_else(|| classify_vector_get_or_default(&args[0], &args[1])),
        ("Result.withDefault", 2) => classify_int_mod_or_default(&args[0], &args[1], is_user_type),
        _ => None,
    }
}

fn classify_vector_set_or_default<'a>(
    option_expr: &'a crate::ast::Spanned<ResolvedExpr>,
    default_expr: &'a crate::ast::Spanned<ResolvedExpr>,
) -> Option<ResolvedLeafOp<'a>> {
    let ResolvedExpr::Call(inner_callee, inner_args) = &option_expr.node else {
        return None;
    };
    if inner_args.len() != 3 {
        return None;
    }
    let is_vector_set =
        matches!(inner_callee, ResolvedCallee::Builtin(name) if name == "Vector.set");
    if !is_vector_set {
        return None;
    }
    if default_expr.node != inner_args[0].node {
        return None;
    }
    Some(ResolvedLeafOp::VectorSetOrDefaultSameVector {
        vector: &inner_args[0],
        index: &inner_args[1],
        value: &inner_args[2],
    })
}

fn classify_vector_get_or_default<'a>(
    option_expr: &'a crate::ast::Spanned<ResolvedExpr>,
    default_expr: &'a crate::ast::Spanned<ResolvedExpr>,
) -> Option<ResolvedLeafOp<'a>> {
    let default_literal = match &default_expr.node {
        ResolvedExpr::Literal(lit) => lit,
        _ => return None,
    };
    let ResolvedExpr::Call(inner_callee, inner_args) = &option_expr.node else {
        return None;
    };
    if inner_args.len() != 2 {
        return None;
    }
    let is_vector_get =
        matches!(inner_callee, ResolvedCallee::Builtin(name) if name == "Vector.get");
    if !is_vector_get {
        return None;
    }
    Some(ResolvedLeafOp::VectorGetOrDefaultLiteral {
        vector: &inner_args[0],
        index: &inner_args[1],
        default_literal,
    })
}

fn classify_list_index_get<'a>(
    vector_expr: &'a crate::ast::Spanned<ResolvedExpr>,
    index: &'a crate::ast::Spanned<ResolvedExpr>,
) -> Option<ResolvedLeafOp<'a>> {
    let ResolvedExpr::Call(inner_callee, inner_args) = &vector_expr.node else {
        return None;
    };
    if inner_args.len() != 1 {
        return None;
    }
    let is_from_list =
        matches!(inner_callee, ResolvedCallee::Builtin(name) if name == "Vector.fromList");
    if !is_from_list {
        return None;
    }
    Some(ResolvedLeafOp::ListIndexGet {
        list: &inner_args[0],
        index,
    })
}

fn classify_int_mod_or_default<'a>(
    result_expr: &'a crate::ast::Spanned<ResolvedExpr>,
    default_expr: &'a crate::ast::Spanned<ResolvedExpr>,
    _is_user_type: &impl Fn(&str) -> bool,
) -> Option<ResolvedLeafOp<'a>> {
    let default_literal = match &default_expr.node {
        ResolvedExpr::Literal(lit) => lit,
        _ => return None,
    };
    let ResolvedExpr::Call(inner_callee, inner_args) = &result_expr.node else {
        return None;
    };
    if inner_args.len() != 2 {
        return None;
    }
    let is_int_mod = matches!(inner_callee, ResolvedCallee::Builtin(name) if name == "Int.mod");
    if !is_int_mod {
        return None;
    }
    Some(ResolvedLeafOp::IntModOrDefaultLiteral {
        a: &inner_args[0],
        b: &inner_args[1],
        default_literal,
    })
}

/// Classify a [`ResolvedExpr`] as a bool subject for the
/// `match X { true -> _; false -> _ }` shortcut.
pub fn classify_bool_subject_plan_resolved(subject: &ResolvedExpr) -> ResolvedBoolSubjectPlan<'_> {
    let ResolvedExpr::BinOp(op, lhs, rhs) = subject else {
        return ResolvedBoolSubjectPlan::Expr(subject);
    };
    use crate::ast::BinOp;
    match op {
        BinOp::Eq => ResolvedBoolSubjectPlan::Compare {
            lhs,
            rhs,
            op: BoolCompareOp::Eq,
            invert: false,
        },
        BinOp::Lt => ResolvedBoolSubjectPlan::Compare {
            lhs,
            rhs,
            op: BoolCompareOp::Lt,
            invert: false,
        },
        BinOp::Gt => ResolvedBoolSubjectPlan::Compare {
            lhs,
            rhs,
            op: BoolCompareOp::Gt,
            invert: false,
        },
        BinOp::Neq => ResolvedBoolSubjectPlan::Compare {
            lhs,
            rhs,
            op: BoolCompareOp::Eq,
            invert: true,
        },
        BinOp::Gte => ResolvedBoolSubjectPlan::Compare {
            lhs,
            rhs,
            op: BoolCompareOp::Lt,
            invert: true,
        },
        BinOp::Lte => ResolvedBoolSubjectPlan::Compare {
            lhs,
            rhs,
            op: BoolCompareOp::Gt,
            invert: true,
        },
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => ResolvedBoolSubjectPlan::Expr(subject),
    }
}

/// Classify a single arm pattern as dispatchable in the VM's
/// MATCH_DISPATCH table. Mirrors [`crate::ir::classify_dispatch_pattern`].
pub fn classify_dispatch_pattern_resolved(
    pattern: &ResolvedPattern,
) -> Option<SemanticDispatchPattern> {
    match pattern {
        ResolvedPattern::Literal(lit) => Some(SemanticDispatchPattern::Literal(
            dispatch_literal_from_ast(lit),
        )),
        ResolvedPattern::EmptyList => Some(SemanticDispatchPattern::EmptyList),
        ResolvedPattern::Ctor(ctor, bindings) => match ctor {
            ResolvedCtor::Builtin(BuiltinCtor::OptionNone) if bindings.is_empty() => {
                Some(SemanticDispatchPattern::NoneValue)
            }
            ResolvedCtor::Builtin(BuiltinCtor::ResultOk) if bindings.len() <= 1 => {
                Some(SemanticDispatchPattern::WrapperTag(WrapperKind::ResultOk))
            }
            ResolvedCtor::Builtin(BuiltinCtor::ResultErr) if bindings.len() <= 1 => {
                Some(SemanticDispatchPattern::WrapperTag(WrapperKind::ResultErr))
            }
            ResolvedCtor::Builtin(BuiltinCtor::OptionSome) if bindings.len() <= 1 => {
                Some(SemanticDispatchPattern::WrapperTag(WrapperKind::OptionSome))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Mirror of [`crate::ir::classify_list_match_shape`] / `_from_patterns`.
pub fn classify_list_match_shape_resolved(arms: &[ResolvedMatchArm]) -> Option<ListMatchShape> {
    if arms.len() != 2 {
        return None;
    }
    match (&arms[0].pattern, &arms[1].pattern) {
        (ResolvedPattern::EmptyList, ResolvedPattern::Cons(_, _)) => Some(ListMatchShape {
            empty_arm_index: 0,
            cons_arm_index: 1,
        }),
        (ResolvedPattern::Cons(_, _), ResolvedPattern::EmptyList) => Some(ListMatchShape {
            empty_arm_index: 1,
            cons_arm_index: 0,
        }),
        _ => None,
    }
}

/// Mirror of [`crate::ir::classify_bool_match_shape`] / `_from_patterns`.
pub fn classify_bool_match_shape_resolved(arms: &[ResolvedMatchArm]) -> Option<BoolMatchShape> {
    if arms.len() != 2 {
        return None;
    }
    use crate::ast::Literal as Lit;
    match (&arms[0].pattern, &arms[1].pattern) {
        (ResolvedPattern::Literal(Lit::Bool(true)), ResolvedPattern::Literal(Lit::Bool(false))) => {
            Some(BoolMatchShape {
                true_arm_index: 0,
                false_arm_index: 1,
            })
        }
        (ResolvedPattern::Literal(Lit::Bool(false)), ResolvedPattern::Literal(Lit::Bool(true))) => {
            Some(BoolMatchShape {
                true_arm_index: 1,
                false_arm_index: 0,
            })
        }
        (
            ResolvedPattern::Literal(Lit::Bool(true)),
            ResolvedPattern::Wildcard | ResolvedPattern::Ident(_),
        ) => Some(BoolMatchShape {
            true_arm_index: 0,
            false_arm_index: 1,
        }),
        _ => None,
    }
}

/// Mirror of [`crate::ir::classify_dispatch_table_shape`] /
/// `_from_patterns`.
pub fn classify_dispatch_table_shape_resolved(
    arms: &[ResolvedMatchArm],
) -> Option<DispatchTableShape> {
    if arms.len() < 2 {
        return None;
    }
    let has_default = matches!(
        arms.last().map(|a| &a.pattern),
        Some(ResolvedPattern::Wildcard | ResolvedPattern::Ident(_))
    );
    let dispatchable_end = if has_default {
        arms.len() - 1
    } else {
        arms.len()
    };

    let mut entries = Vec::new();
    for (arm_index, arm) in arms[..dispatchable_end].iter().enumerate() {
        let semantic = classify_dispatch_pattern_resolved(&arm.pattern)?;
        entries.push(DispatchArmPlan {
            binding: classify_dispatch_binding_resolved(&arm.pattern, &semantic),
            pattern: semantic,
            arm_index,
        });
    }

    if entries.len() < 2 {
        return None;
    }

    let default_arm = has_default.then(|| {
        let arm_idx = arms.len() - 1;
        let binding_name = match &arms[arm_idx].pattern {
            ResolvedPattern::Ident(name) if name != "_" => Some(name.clone()),
            _ => None,
        };
        DispatchDefaultPlan {
            arm_index: arm_idx,
            binding_name,
        }
    });

    Some(DispatchTableShape {
        entries,
        default_arm,
    })
}

/// Mirror of [`crate::ir::classify_match_dispatch_plan`] /
/// `_from_patterns`.
pub fn classify_match_dispatch_plan_resolved(
    arms: &[ResolvedMatchArm],
) -> Option<MatchDispatchPlan> {
    if let Some(shape) = classify_bool_match_shape_resolved(arms) {
        return Some(MatchDispatchPlan::Bool(shape));
    }
    if let Some(shape) = classify_list_match_shape_resolved(arms) {
        return Some(MatchDispatchPlan::List(shape));
    }
    classify_dispatch_table_shape_resolved(arms).map(MatchDispatchPlan::Table)
}

fn classify_dispatch_binding_resolved(
    pattern: &ResolvedPattern,
    semantic: &SemanticDispatchPattern,
) -> DispatchBindingPlan {
    match (pattern, semantic) {
        (ResolvedPattern::Ctor(_, bindings), SemanticDispatchPattern::WrapperTag(_))
            if !bindings.is_empty() && bindings[0] != "_" =>
        {
            DispatchBindingPlan::WrapperPayload(bindings[0].clone())
        }
        _ => DispatchBindingPlan::None,
    }
}

fn dispatch_literal_from_ast(lit: &Literal) -> DispatchLiteral {
    match lit {
        Literal::Int(i) => DispatchLiteral::Int(*i),
        Literal::Float(f) => DispatchLiteral::Float(f.to_string()),
        Literal::Bool(b) => DispatchLiteral::Bool(*b),
        Literal::Str(s) => DispatchLiteral::Str(s.clone()),
        Literal::Unit => DispatchLiteral::Unit,
    }
}

/// Reconstruct an `Module.member.sub` dotted path from a chain of
/// `ResolvedExpr::Ident` / `ResolvedExpr::Attr` nodes. Used by the
/// VM compiler's leaf-op recognition for static module/builtin
/// references (`Fibonacci.fib`, `Domain.Tag.Active`, ...) and by
/// the `Unresolved`-callee fallback path so the existing namespace
/// dispatch logic can keep operating on dotted names. Returns
/// `None` for anything whose root isn't an `Ident` (resolved
/// slots, calls, etc.).
pub fn resolved_to_dotted(expr: &ResolvedExpr) -> Option<String> {
    match expr {
        ResolvedExpr::Ident(name) => Some(name.clone()),
        ResolvedExpr::Resolved { name, .. } => Some(name.clone()),
        ResolvedExpr::Attr(obj, field) => {
            let head = resolved_to_dotted(&obj.node)?;
            Some(format!("{head}.{field}"))
        }
        _ => None,
    }
}

/// Map a [`ResolvedCallee`] (call-position) to a forward-call plan
/// when every supplied arg is a slot-based `Resolved` reference.
/// Returns `None` if the callee is dynamic / a wrapper with the
/// wrong arity / has any non-local argument.
pub fn classify_forward_call_resolved<'a>(
    callee: &'a ResolvedCallee,
    args: &'a [crate::ast::Spanned<ResolvedExpr>],
) -> Option<ResolvedForwardCallPlan<'a>> {
    match callee {
        // Intrinsics short-circuit the call pipeline well before
        // the forward-call optimisation runs; they should never be
        // a forward-call target. Same for LocalSlot (dynamic) and
        // Unresolved (recovery shape).
        ResolvedCallee::Unresolved { .. } => return None,
        ResolvedCallee::LocalSlot { .. } => return None,
        ResolvedCallee::Intrinsic(_) => return None,
        _ => {}
    }

    let forward_slots = args
        .iter()
        .map(|arg| classify_forward_arg_resolved(&arg.node))
        .collect::<Option<Vec<_>>>()?;

    Some(ResolvedForwardCallPlan {
        callee,
        forward_slots,
    })
}

fn classify_forward_arg_resolved(expr: &ResolvedExpr) -> Option<ForwardSlot> {
    match expr {
        ResolvedExpr::Resolved { slot, .. } => Some(ForwardSlot::Local { slot: *slot }),
        _ => None,
    }
}
