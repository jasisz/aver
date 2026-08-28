//! Explicit value modes for Rust MIR lowering.
//!
//! Aver has value semantics; generated Rust has places, moves and shared
//! references.  Every MIR value is classified here as [`RustValueMode::Copy`],
//! [`RustValueMode::Owned`] or [`RustValueMode::Borrowed`], then materialised
//! for an owning or borrowing consumer. Calls, returns, aggregate fields and
//! match subjects must use these helpers instead of rediscovering ownership
//! from their expression shape.

use crate::ast::Type;
use crate::ir::mir::{MirExpr, MirLocal, MirProject};

use super::emit_ctx::is_copy_type;
use super::from_mir::MirEmitCtx;

/// Rust representation carried by an emitted MIR value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum RustValueMode {
    /// Directly copyable Rust value (`bool`, `f64`, `()`, or proven bare
    /// `i64`). Owning and borrowing consumers both use it by value.
    Copy,
    /// A value represented as `T`. It may move when its MIR place is at its
    /// final use; otherwise an owning consumer clones it.
    Owned,
    /// A logical Aver value reached through `&T` or a TCO wrapper. An owning
    /// consumer clones the referent; a borrowing consumer reuses its borrow.
    Borrowed,
}

/// Provider resources are opaque, cloneable handles. Cloning a generated
/// resource wrapper copies the handle token; it does not duplicate, close or
/// otherwise operate on the provider-owned resource. This is the single Rust
/// ownership policy for standard and program-defined capability resources.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ProviderResourcePolicy {
    CloneHandle,
}

pub(super) const PROVIDER_RESOURCE_POLICY: ProviderResourcePolicy =
    ProviderResourcePolicy::CloneHandle;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BorrowShape {
    Direct,
    DerefWrapper,
}

/// Complete lowering facts needed to cross a Rust ownership boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct RustValueFacts {
    pub mode: RustValueMode,
    borrow_shape: BorrowShape,
    can_move: bool,
    pub(super) provider_resource: bool,
}

/// A source-named local read, excluding compiler-synthetic unnamed locals.
pub(super) fn local_of(expr: &MirExpr) -> Option<&MirLocal> {
    match expr {
        MirExpr::Local(local) if !local.node.name.is_empty() => Some(&local.node),
        _ => None,
    }
}

/// Classify one MIR expression before a Rust consumer chooses whether it owns
/// or borrows the value.
pub(super) fn value_facts(expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> RustValueFacts {
    if let Some(local) = local_of(expr) {
        let name = local.name.as_str();
        let copy =
            ctx.bare.is_bare(local.slot) || ctx.local_types.get(name).is_some_and(is_copy_type);
        let wrapped = ctx.rc_wrapped.contains(name);
        let borrowed = ctx.borrowed_params.contains(name) || wrapped;
        let mode = if copy {
            RustValueMode::Copy
        } else if borrowed {
            RustValueMode::Borrowed
        } else {
            RustValueMode::Owned
        };
        return RustValueFacts {
            mode,
            borrow_shape: if wrapped {
                BorrowShape::DerefWrapper
            } else {
                BorrowShape::Direct
            },
            can_move: mode == RustValueMode::Copy
                || (mode == RustValueMode::Owned
                    && local.last_use
                    && !ctx.loop_carried_params.contains(name)),
            provider_resource: ctx
                .local_types
                .get(name)
                .is_some_and(|ty| is_provider_resource_type(ty, ctx)),
        };
    }

    if let MirExpr::Project(project) = expr {
        let copy = projection_result_is_copy(&project.node, ctx);
        return RustValueFacts {
            mode: if copy {
                RustValueMode::Copy
            } else {
                // A field expression has Rust type `T`, including when its
                // place is rooted behind `&Record`. The root controls whether
                // it may move; it does not change the expression to `&T`.
                RustValueMode::Owned
            },
            borrow_shape: BorrowShape::Direct,
            // A field of a fresh temporary can move. A local-rooted
            // projection stays conservative even when the root is at its
            // final use; proving partial moves belongs in a later MIR pass.
            can_move: copy || projection_root_local(&project.node.base.node).is_none(),
            provider_resource: false,
        };
    }

    RustValueFacts {
        mode: RustValueMode::Owned,
        borrow_shape: BorrowShape::Direct,
        can_move: true,
        provider_resource: false,
    }
}

/// Materialise `expr` for a Rust position that consumes an owned `T`.
pub(super) fn materialize_owned(code: String, expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> String {
    let facts = value_facts(expr, ctx);
    match facts.mode {
        RustValueMode::Copy => code,
        RustValueMode::Borrowed => {
            if facts.provider_resource {
                match PROVIDER_RESOURCE_POLICY {
                    ProviderResourcePolicy::CloneHandle => clone_borrowed(code, facts.borrow_shape),
                }
            } else {
                clone_borrowed(code, facts.borrow_shape)
            }
        }
        RustValueMode::Owned if facts.can_move => code,
        RustValueMode::Owned => format!("{}.clone()", code),
    }
}

/// Materialise `expr` for a Rust position that consumes a shared borrow.
pub(super) fn materialize_borrowed(code: String, expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> String {
    let facts = value_facts(expr, ctx);
    match facts.mode {
        RustValueMode::Copy => code,
        RustValueMode::Borrowed if facts.borrow_shape == BorrowShape::Direct => code,
        RustValueMode::Borrowed => format!("&*{}", code),
        RustValueMode::Owned => format!("&{}", code),
    }
}

/// Whether the expression text itself has Rust type `&T`. Used by match
/// lowering when it can safely pattern-match directly on a shared reference.
pub(super) fn emits_direct_borrow(expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> bool {
    let facts = value_facts(expr, ctx);
    facts.mode == RustValueMode::Borrowed
        && (facts.borrow_shape == BorrowShape::Direct || ctx.rc_wrapped_are_borrowed_refs)
}

/// Align equality operands to one Rust representation. If either expression
/// emits `&T`, compare two borrows; otherwise retain the original `T == T`
/// shape. String-literal dereferencing remains a representation-specific
/// pre-step in the caller.
pub(super) fn align_equality_operands(
    left_code: &str,
    left: &MirExpr,
    right_code: &str,
    right: &MirExpr,
    ctx: &MirEmitCtx<'_>,
) -> (String, String) {
    let left_facts = value_facts(left, ctx);
    let right_facts = value_facts(right, ctx);
    if left_facts.mode != RustValueMode::Borrowed && right_facts.mode != RustValueMode::Borrowed {
        return (left_code.to_string(), right_code.to_string());
    }

    let align = |code: &str, expr: &MirExpr, facts: RustValueFacts| match facts.mode {
        RustValueMode::Copy => code.to_string(),
        RustValueMode::Borrowed
            if facts.borrow_shape == BorrowShape::DerefWrapper
                && ctx.rc_wrapped_are_borrowed_refs =>
        {
            // Mutual-TCO invariants are already represented as `&T`; their
            // `DerefWrapper` clone shape exists only for owning consumers.
            code.to_string()
        }
        RustValueMode::Borrowed => materialize_borrowed(code.to_string(), expr, ctx),
        RustValueMode::Owned => format!("&({code})"),
    };
    (
        align(left_code, left, left_facts),
        align(right_code, right, right_facts),
    )
}

fn clone_borrowed(code: String, shape: BorrowShape) -> String {
    match shape {
        BorrowShape::Direct => format!("{}.clone()", code),
        BorrowShape::DerefWrapper => format!("(*{}).clone()", code),
    }
}

fn projection_root_local(expr: &MirExpr) -> Option<&MirLocal> {
    match expr {
        MirExpr::Local(_) => local_of(expr),
        MirExpr::Project(project) => projection_root_local(&project.node.base.node),
        _ => None,
    }
}

fn projection_result_is_copy(project: &MirProject, ctx: &MirEmitCtx<'_>) -> bool {
    let Some(codegen) = ctx.codegen else {
        return false;
    };
    let Some(local) = local_of(&project.base.node) else {
        return false;
    };
    let Some(named_ty) = ctx
        .local_types
        .get(&local.name)
        .filter(|ty| matches!(ty, Type::Named { .. }))
    else {
        return false;
    };
    super::expr::record_field_is_copy(named_ty, &project.field, codegen)
}

fn is_provider_resource_type(ty: &Type, ctx: &MirEmitCtx<'_>) -> bool {
    let Type::Named { id, name } = ty else {
        return false;
    };
    if let Some(id) = id
        && ctx
            .symbol_table
            .type_entry_if_present(*id)
            .is_some_and(|entry| entry.is_capability_resource)
    {
        return true;
    }
    ctx.codegen.is_some_and(|codegen| {
        codegen
            .capabilities
            .resource_types()
            .any(|canonical| canonical == name)
    })
}
