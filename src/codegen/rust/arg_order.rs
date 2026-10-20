//! Rust backend: evaluate the pure arguments of a call that read a local
//! before the earlier argument that hands that local over.
//!
//! Aver evaluates call arguments left to right, so in `f(s, g(s))` the last
//! read of `s` is the one inside `g(s)`. When `f` takes `s` by value, the
//! first argument is then not the last use and Rust clones it, and the clone
//! keeps the caller's copy of every Map and Vector in `s` alive: the first
//! in-place update in `f` copies the whole collection.
//!
//! When the later arguments that read `s` are pure, and so is every argument
//! they would overtake, the order of evaluation cannot be observed (Aver
//! values are immutable and only an effect shows when it ran). This pass binds
//! those arguments to fresh locals first, `let __argN = g(s); f(s, __argN)`,
//! so the read in the first argument becomes the last one and moves.
//!
//! It runs on the Rust backend's own copy of the MIR, after the
//! representation rewrite and between rounds of the by-value parameter facts
//! ([`super::from_mir::compute_owned_record_params`]): a call is rewritten
//! only where its callee takes the moved position by value, and the moved
//! local can in turn make the caller's own parameter by value.

use std::collections::{HashMap, HashSet};

use crate::ast::{Spanned, Type};
use crate::codegen::CodegenContext;
use crate::ir::FnId;
use crate::ir::mir::{LocalId, MirCallee, MirExpr, MirFn, MirLet, MirLocal};

use super::syntax::generated_ident;

/// What one rewrite round reads about the program: which positions each
/// callee takes by value in the Rust ABI, which Map and Vector params it
/// updates in place, which functions are free of effects, and which
/// builtins are host operations.
struct Facts<'a> {
    by_value: HashMap<FnId, Vec<bool>>,
    in_place: HashMap<FnId, Vec<bool>>,
    pure_fns: HashSet<FnId>,
    mutual_tco: &'a HashSet<FnId>,
    builtins: Vec<bool>,
}

/// One round of the rewrite over every function. Returns whether a call
/// changed, so the caller recomputes the by-value facts and tries again.
pub(super) fn hoist_reads_before_moves(ctx: &mut CodegenContext) -> bool {
    let Some(program) = ctx.mir_program.as_ref() else {
        return false;
    };
    let by_value = program
        .iter()
        .map(|(id, mir_fn)| {
            let name = ctx.symbol_table.fn_entry(*id).key.canonical();
            let mask = super::expr::callee_borrow_mask(&name, mir_fn.params.len(), ctx);
            (*id, mask.into_iter().map(|borrowed| !borrowed).collect())
        })
        .collect();
    let pure_fns = program
        .iter()
        .filter(|(_, mir_fn)| mir_fn.effects.is_empty())
        .map(|(id, _)| *id)
        .collect();
    let builtins = program
        .builtins
        .iter()
        .map(|name| ctx.capabilities.operation(name).is_none())
        .collect();
    let in_place = crate::ir::mir::field_moves::in_place_collection_params(program);
    let mutual_tco = ctx.mutual_tco_members.clone();
    let facts = Facts {
        by_value,
        in_place,
        pure_fns,
        mutual_tco: &mutual_tco,
        builtins,
    };
    let Some(mut program) = ctx.mir_program.take() else {
        return false;
    };
    let mut changed = false;
    for mir_fn in program.fns.values_mut() {
        let mut body = std::mem::replace(
            &mut mir_fn.body,
            Spanned::bare(MirExpr::Literal(Spanned::bare(crate::ast::Literal::Unit))),
        );
        changed |= rewrite(&mut body, mir_fn, &facts);
        mir_fn.body = body;
    }
    ctx.mir_program = Some(program);
    changed
}

/// Rewrite every qualifying call in `expr`, innermost first.
fn rewrite(expr: &mut Spanned<MirExpr>, mir_fn: &mut MirFn, facts: &Facts<'_>) -> bool {
    let mut changed = false;
    crate::ir::mir::expr::walk_children_mut(&mut expr.node, &mut |child| {
        changed |= rewrite(child, mir_fn, facts);
    });
    let (callee, args) = match &expr.node {
        MirExpr::Call(call) => match call.node.callee {
            MirCallee::Fn(callee) => (callee, &call.node.args),
            _ => return changed,
        },
        // A tail call stays a plain call unless it is the self loop or a
        // bounce of a mutual trampoline, whose shapes are emitted apart.
        MirExpr::TailCall(call)
            if call.node.target != mir_fn.fn_id
                && !facts.mutual_tco.contains(&call.node.target)
                && !facts.mutual_tco.contains(&mir_fn.fn_id) =>
        {
            (call.node.target, &call.node.args)
        }
        _ => return changed,
    };
    let Some(plan) = plan(callee, args, facts) else {
        return changed;
    };
    apply(expr, mir_fn, &plan);
    true
}

/// Which arguments of one call are bound first, and which direct local
/// argument takes over a last read from them.
struct Plan {
    /// Positions bound to fresh locals before the call, in order.
    hoisted: Vec<usize>,
    /// Slots whose last read moves from a hoisted argument to the direct
    /// local argument at the given position.
    handed: Vec<(LocalId, usize)>,
}

fn plan(callee: FnId, args: &[Spanned<MirExpr>], facts: &Facts<'_>) -> Option<Plan> {
    let fact = |facts: &HashMap<FnId, Vec<bool>>, i: usize| {
        facts
            .get(&callee)
            .and_then(|positions| positions.get(i))
            .copied()
            .unwrap_or(false)
    };
    // A moved record is worth it at any by-value position; a Map or Vector
    // only where the callee updates it in place, as for the by-value facts.
    let worth_moving = |i: usize, ty: Option<&Type>| match ty {
        Some(Type::Named { .. }) => fact(&facts.by_value, i),
        Some(Type::Map(..) | Type::Vector(_)) => {
            fact(&facts.by_value, i) && fact(&facts.in_place, i)
        }
        _ => false,
    };
    let mut hoisted: HashSet<usize> = HashSet::new();
    for (i, arg) in args.iter().enumerate() {
        let MirExpr::Local(local) = &arg.node else {
            continue;
        };
        let local = &local.node;
        if local.last_use || local.name.is_empty() || !worth_moving(i, arg.ty()) {
            continue;
        }
        let readers: Vec<usize> = (i + 1..args.len())
            .filter(|&j| reads_slot(&args[j].node, local.slot))
            .collect();
        // Only worth it when the last read is in one of them, and none of
        // them is the local itself (that would only move the clone).
        if !readers
            .iter()
            .any(|&j| has_last_use_of(&args[j].node, local.slot))
            || readers
                .iter()
                .any(|&j| matches!(args[j].node, MirExpr::Local(_)))
        {
            continue;
        }
        hoisted.extend(readers);
    }
    if hoisted.is_empty() {
        return None;
    }
    let last_hoisted = *hoisted.iter().max()?;
    for (j, arg) in args.iter().enumerate() {
        if hoisted.contains(&j) {
            // A hoisted value is bound to a boxed local: an `Int` argument
            // may be a raw `i64` the callee takes bare.
            if !matches!(arg.ty(), Some(ty) if *ty != Type::Int) || !effect_free(&arg.node, facts) {
                return None;
            }
        } else if j < last_hoisted && !effect_free(&arg.node, facts) {
            return None;
        }
    }
    // A slot whose last read is in a hoisted argument and that a kept
    // argument also reads: the kept read now runs last. It must be the
    // argument itself, so it can carry the last-use mark.
    let mut slots = Vec::new();
    for &j in &hoisted {
        collect_last_use_slots(&args[j].node, &mut slots);
    }
    let mut handed = Vec::new();
    for slot in slots {
        let kept = (0..args.len())
            .rev()
            .find(|k| !hoisted.contains(k) && reads_slot(&args[*k].node, slot));
        let Some(k) = kept else {
            continue;
        };
        match &args[k].node {
            MirExpr::Local(local) if local.node.slot == slot => {
                if !handed.contains(&(slot, k)) {
                    handed.push((slot, k));
                }
            }
            _ => return None,
        }
    }
    let mut hoisted: Vec<usize> = hoisted.into_iter().collect();
    hoisted.sort_unstable();
    Some(Plan { hoisted, handed })
}

/// Bind the planned arguments first and move the last-use marks.
fn apply(expr: &mut Spanned<MirExpr>, mir_fn: &mut MirFn, plan: &Plan) {
    let line = expr.line;
    let args = match &mut expr.node {
        MirExpr::Call(call) => &mut call.node.args,
        MirExpr::TailCall(call) => &mut call.node.args,
        _ => return,
    };
    let mut bindings = Vec::with_capacity(plan.hoisted.len());
    for &j in &plan.hoisted {
        let slot = LocalId(mir_fn.local_count);
        mir_fn.local_count += 1;
        let name = generated_ident(&format!("arg{}", slot.0));
        let read = Spanned::new(
            MirExpr::Local(Spanned::new(
                MirLocal {
                    slot,
                    last_use: true,
                    name: name.clone(),
                },
                args[j].line,
            )),
            args[j].line,
        );
        if let Some(ty) = args[j].ty.get() {
            read.set_ty_shared(ty);
        }
        let mut value = std::mem::replace(&mut args[j], read);
        for (moved, _) in &plan.handed {
            clear_last_use(&mut value.node, *moved);
        }
        bindings.push((slot, name, value));
    }
    for (moved, k) in &plan.handed {
        if let MirExpr::Local(local) = &mut args[*k].node
            && local.node.slot == *moved
        {
            local.node.last_use = true;
        }
    }
    let ty = expr.ty.get().cloned();
    let call = std::mem::replace(
        &mut expr.node,
        MirExpr::Literal(Spanned::bare(crate::ast::Literal::Unit)),
    );
    let mut body = Spanned::new(call, line);
    if let Some(ty) = &ty {
        body.set_ty_shared(ty);
    }
    for (slot, name, value) in bindings.into_iter().rev() {
        let node = Spanned::new(
            MirExpr::Let(Spanned::new(
                MirLet {
                    binding: slot,
                    binding_name: name,
                    value: Box::new(value),
                    body: Box::new(body),
                },
                line,
            )),
            line,
        );
        if let Some(ty) = &ty {
            node.set_ty_shared(ty);
        }
        body = node;
    }
    *expr = body;
}

/// Whether `expr` has no effect, so running it earlier or later among other
/// effect-free expressions cannot be observed. A call is effect-free when it
/// calls a function that declares no effects or a builtin that is not a host
/// operation; an early exit (`?`, `return`) and a tail call are not moved.
fn effect_free(expr: &MirExpr, facts: &Facts<'_>) -> bool {
    match expr {
        MirExpr::Call(call) => {
            let callee = match &call.node.callee {
                MirCallee::Fn(id) => facts.pure_fns.contains(id),
                MirCallee::Builtin(id) => facts.builtins.get(id.0 as usize).copied() == Some(true),
                MirCallee::Intrinsic(_) | MirCallee::LocalSlot { .. } => false,
            };
            callee
                && call
                    .node
                    .args
                    .iter()
                    .all(|arg| effect_free(&arg.node, facts))
        }
        MirExpr::TailCall(_)
        | MirExpr::Try(_)
        | MirExpr::Return(_)
        | MirExpr::IndependentProduct(_) => false,
        _ => {
            let mut free = true;
            crate::ir::mir::expr::walk_children(expr, &mut |child| {
                free = free && effect_free(child, facts);
            });
            free
        }
    }
}

/// Whether `expr` reads `slot` anywhere, including as a called fn value.
fn reads_slot(expr: &MirExpr, slot: LocalId) -> bool {
    match expr {
        MirExpr::Local(local) => local.node.slot == slot,
        MirExpr::Call(call) if matches!(&call.node.callee, MirCallee::LocalSlot { slot: s, .. } if u32::from(*s) == slot.0) => {
            true
        }
        _ => {
            let mut found = false;
            crate::ir::mir::expr::walk_children(expr, &mut |child| {
                found = found || reads_slot(child, slot);
            });
            found
        }
    }
}

/// Whether `expr` holds a read of `slot` marked as its last use.
fn has_last_use_of(expr: &MirExpr, slot: LocalId) -> bool {
    match expr {
        MirExpr::Local(local) => local.node.slot == slot && local.node.last_use,
        _ => {
            let mut found = false;
            crate::ir::mir::expr::walk_children(expr, &mut |child| {
                found = found || has_last_use_of(child, slot);
            });
            found
        }
    }
}

/// The slots read for the last time somewhere in `expr` (an effect-free
/// expression, so no fn value is called in it).
fn collect_last_use_slots(expr: &MirExpr, out: &mut Vec<LocalId>) {
    match expr {
        MirExpr::Local(local) => {
            if local.node.last_use && !out.contains(&local.node.slot) {
                out.push(local.node.slot);
            }
        }
        _ => crate::ir::mir::expr::walk_children(expr, &mut |child| {
            collect_last_use_slots(child, out)
        }),
    }
}

/// Mark every read of `slot` in `expr` as not the last.
fn clear_last_use(expr: &mut MirExpr, slot: LocalId) {
    if let MirExpr::Local(local) = expr {
        if local.node.slot == slot {
            local.node.last_use = false;
        }
        return;
    }
    crate::ir::mir::expr::walk_children_mut(expr, &mut |child| {
        clear_last_use(&mut child.node, slot)
    });
}
