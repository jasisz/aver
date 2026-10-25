//! Rust backend: `Option` params a function only reads take `Option<&T>`.
//!
//! An `Option<T>` param is borrowed as `&Option<T>` by default, so a caller
//! that has the value somewhere else first copies it into an `Option` of its
//! own: `hit(Map.get(memo, key), e)` cloned the entry out of the map on every
//! call although `hit` only matches it. A param the function only reads is
//! taken as `Option<&T>` instead. A caller passes `map.get(&key)` (see
//! `MirFnEmitPolicy::apply_lent_options`) or `x.as_ref()` of an `Option` it
//! holds, and the function copies only what it hands on: a payload it returns
//! or stores is cloned there, out of the borrow.
//!
//! A param is lent when every read of it is one of:
//! - the subject of a match whose arms are `Some(..)`, `None` or `_`: a match
//!   binds the same borrows from `Option<&T>` as from `&Option<T>`;
//! - an argument at a lent param of another function (the analysis is a
//!   greatest fixpoint, so a value passed on through a chain stays lent);
//! - the function's result, cloned there with `.cloned()`.
//!
//! Anything else (an `==`, a built-in, a record field, a thread of `(a, b)!`)
//! keeps `&Option<T>`. So does every param of a tail-call loop, whose params
//! are rebound on every step, and of the guest entry, whose glue passes its
//! own borrows. Only a payload type a borrow serves (`should_borrow_param`)
//! qualifies.

use std::collections::HashMap;

use crate::ast::Type;
use crate::codegen::CodegenContext;
use crate::ir::FnId;
use crate::ir::mir::{LocalId, MirCallee, MirCtor, MirExpr, MirPattern};

use super::emit_ctx::should_borrow_param;

/// Which params of which functions take `Option<&T>`, by param position.
pub(super) type LentOptions = HashMap<FnId, Vec<bool>>;

/// Compute the lent `Option` params of every function in the program.
pub(super) fn compute(ctx: &CodegenContext) -> LentOptions {
    let Some(program) = ctx.mir_program.as_ref() else {
        return LentOptions::new();
    };
    let mut lent: LentOptions = program
        .iter()
        .filter_map(|(id, mir_fn)| {
            let resolved = ctx.resolved_program.fn_by_id(*id)?;
            let candidate = !ctx.mutual_tco_members.contains(id)
                && !super::toplevel::resolved_fn_has_self_tailcall(resolved)
                && ctx.guest_entry.as_deref() != Some(resolved.name.as_str());
            let flags: Vec<bool> = resolved
                .params
                .iter()
                .map(|(_, ty)| {
                    candidate && matches!(ty, Type::Option(inner) if should_borrow_param(inner))
                })
                .collect();
            (flags.iter().any(|f| *f) && flags.len() == mir_fn.params.len()).then_some((*id, flags))
        })
        .collect();
    loop {
        let mut dropped = Vec::new();
        for (id, flags) in &lent {
            let Some(mir_fn) = program.fn_by_id(*id) else {
                continue;
            };
            for (i, param) in mir_fn.params.iter().enumerate() {
                if flags[i] && !reads_in_place(&mir_fn.body.node, param.local, &lent, true) {
                    dropped.push((*id, i));
                }
            }
        }
        if dropped.is_empty() {
            break;
        }
        for (id, i) in dropped {
            if let Some(flags) = lent.get_mut(&id) {
                flags[i] = false;
            }
        }
    }
    lent.retain(|_, flags| flags.iter().any(|f| *f));
    lent
}

/// Whether param `index` of `fn_id` takes `Option<&T>`.
pub(super) fn is_lent(lent: &LentOptions, fn_id: FnId, index: usize) -> bool {
    lent.get(&fn_id)
        .and_then(|flags| flags.get(index))
        .copied()
        .unwrap_or(false)
}

/// Whether every read of the `Option` local in `slot` within `expr` is one
/// the module doc allows; `tail` says `expr` is the function's result.
pub(super) fn reads_in_place(
    expr: &MirExpr,
    slot: LocalId,
    lent: &LentOptions,
    tail: bool,
) -> bool {
    let is_slot = |e: &MirExpr| matches!(e, MirExpr::Local(local) if local.node.slot == slot);
    match expr {
        MirExpr::Local(local) => tail || local.node.slot != slot,
        MirExpr::Match(m) if is_slot(&m.node.subject.node) => m.node.arms.iter().all(|arm| {
            matches!(
                arm.pattern,
                MirPattern::Ctor {
                    ctor: MirCtor::Builtin(_),
                    ..
                } | MirPattern::Wildcard
            ) && reads_in_place(&arm.body.node, slot, lent, tail)
        }),
        MirExpr::Match(m) => {
            reads_in_place(&m.node.subject.node, slot, lent, false)
                && m.node
                    .arms
                    .iter()
                    .all(|arm| reads_in_place(&arm.body.node, slot, lent, tail))
        }
        MirExpr::Let(binding) => {
            reads_in_place(&binding.node.value.node, slot, lent, false)
                && reads_in_place(&binding.node.body.node, slot, lent, tail)
        }
        MirExpr::IfThenElse(branches) => {
            reads_in_place(&branches.node.cond.node, slot, lent, false)
                && reads_in_place(&branches.node.then_branch.node, slot, lent, tail)
                && reads_in_place(&branches.node.else_branch.node, slot, lent, tail)
        }
        // A branch of `(a, b)!` runs on another thread.
        MirExpr::IndependentProduct(_) => !reads(expr, slot),
        MirExpr::Call(call) => {
            let lends = |i: usize| match call.node.callee {
                MirCallee::Fn(id) => !call.lazy.is_deferred() && is_lent(lent, id, i),
                _ => false,
            };
            call.node.args.iter().enumerate().all(|(i, arg)| {
                (is_slot(&arg.node) && lends(i)) || reads_in_place(&arg.node, slot, lent, false)
            })
        }
        _ => {
            let mut only = true;
            crate::ir::mir::expr::walk_children(expr, &mut |child| {
                only = only && reads_in_place(child, slot, lent, false);
            });
            only
        }
    }
}

/// Whether `expr` reads the local in `slot` at all.
fn reads(expr: &MirExpr, slot: LocalId) -> bool {
    let mut found = matches!(expr, MirExpr::Local(local) if local.node.slot == slot);
    crate::ir::mir::expr::walk_children(expr, &mut |child| found = found || reads(child, slot));
    found
}
