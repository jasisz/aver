//! Rust backend: bind the computed arguments of a call to locals first, in
//! Aver's order, so a variable passed as it is reads last.
//!
//! Aver evaluates call arguments left to right, so in `f(s, g(s))` the last
//! read of `s` is the one inside `g(s)`; passed as it is, the first argument
//! is then not the last use and Rust clones it, and the clone keeps the
//! caller's copy of every Map and Vector in `s` alive: the first in-place
//! update in `f` copies the whole collection. Reading a variable has no
//! effect, so this pass binds every computed argument to a fresh local first,
//! keeping their order, `let __argN = g(s); f(s, __argN)`, and hands the last
//! read over to the variable passed as it is, which then moves. The same
//! holds for a field (`f(w.st, g(w.st.lctx))`). A bound argument's own
//! temporaries (`f(h(cacheFor(st)), st)`) are dropped at its `let`, before
//! the call runs.
//!
//! Every call to a function counts, a tail call to another function too,
//! including a bounce of a mutual trampoline, to itself as well; the self
//! loop keeps its params in place and is emitted apart. An argument that cannot be bound (a raw `i64` the callee takes
//! bare) ends the bound run, so what follows it is left in place and the
//! order still holds. It runs on the Rust backend's own copy of the MIR,
//! after the representation rewrite and before the by-value parameter facts
//! ([`super::from_mir::compute_owned_record_params`]).

use std::collections::HashSet;

use crate::ast::{Spanned, Type};
use crate::codegen::CodegenContext;
use crate::ir::FnId;
use crate::ir::mir::{LocalId, MirCallee, MirExpr, MirFn, MirLet, MirLocal};

use super::syntax::generated_ident;

/// Bind the computed arguments of every call in the program.
pub(super) fn bind_computed_arguments(ctx: &mut CodegenContext) {
    let Some(mut program) = ctx.mir_program.take() else {
        return;
    };
    // Functions whose Rust return type is a boxed `AverInt`, not `i64`.
    let boxed_return: HashSet<FnId> = program
        .iter()
        .filter(|(_, mir_fn)| !mir_fn.repr.bare_return)
        .map(|(id, _)| *id)
        .collect();
    for mir_fn in program.fns.values_mut() {
        // A member of a mutual trampoline calls itself by a bounce too.
        let self_bounces = ctx.mutual_tco_members.contains(&mir_fn.fn_id);
        let mut body = std::mem::replace(
            &mut mir_fn.body,
            Spanned::bare(MirExpr::Literal(Spanned::bare(crate::ast::Literal::Unit))),
        );
        rewrite(&mut body, mir_fn, &boxed_return, self_bounces);
        mir_fn.body = body;
    }
    ctx.mir_program = Some(program);
}

/// Rewrite every call in `expr`, innermost first.
fn rewrite(
    expr: &mut Spanned<MirExpr>,
    mir_fn: &mut MirFn,
    boxed_return: &HashSet<FnId>,
    self_bounces: bool,
) {
    crate::ir::mir::expr::walk_children_mut(&mut expr.node, &mut |child| {
        rewrite(child, mir_fn, boxed_return, self_bounces)
    });
    let args = match &expr.node {
        MirExpr::Call(call) if matches!(call.node.callee, MirCallee::Fn(_)) => &call.node.args,
        MirExpr::TailCall(call) if self_bounces || call.node.target != mir_fn.fn_id => {
            &call.node.args
        }
        _ => return,
    };
    // The computed arguments up to the first one that cannot be bound.
    let bound: Vec<usize> = args
        .iter()
        .enumerate()
        .filter(|(_, arg)| !read(&arg.node))
        .map_while(|(j, arg)| bindable(arg, boxed_return).then_some(j))
        .collect();
    if !bound.is_empty() {
        apply(expr, mir_fn, &bound);
    }
}

/// Whether `expr` only reads: a variable, a field of one, a literal or a
/// function named as a value, or one of them as a saturated position.
fn read(expr: &MirExpr) -> bool {
    match expr {
        MirExpr::Literal(_) | MirExpr::FnValue(_) | MirExpr::Local(_) => true,
        MirExpr::Project(project) => read(&project.node.base.node),
        MirExpr::Box(inner) | MirExpr::Unbox(inner) => read(&inner.node),
        MirExpr::Call(call) => {
            call.node.callee
                == MirCallee::Intrinsic(crate::ir::hir::BuiltinIntrinsic::StrIndexPosition)
                && call.node.args.iter().all(|a| read(&a.node))
        }
        _ => false,
    }
}

/// Whether an argument can be bound to a local: it has a type, and an `Int`
/// is boxed as written, since a bound local is a boxed `AverInt`.
fn bindable(arg: &Spanned<MirExpr>, boxed_return: &HashSet<FnId>) -> bool {
    match arg.ty() {
        Some(Type::Int) => boxed_int(&arg.node, boxed_return),
        Some(_) => true,
        None => false,
    }
}

/// Bind the arguments at `bound` first and hand each last read they held
/// over to the variable passed as it is that reads the same local last.
fn apply(expr: &mut Spanned<MirExpr>, mir_fn: &mut MirFn, bound: &[usize]) {
    let line = expr.line;
    let args = match &mut expr.node {
        MirExpr::Call(call) => &mut call.node.args,
        MirExpr::TailCall(call) => &mut call.node.args,
        _ => return,
    };
    let mut bindings = Vec::with_capacity(bound.len());
    for &j in bound {
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
        bindings.push((slot, name, std::mem::replace(&mut args[j], read)));
    }
    let mut slots = Vec::new();
    for (_, _, value) in &bindings {
        collect_last_use_slots(&value.node, &mut slots);
    }
    for slot in slots {
        // The last argument left in place that reads the local now reads it
        // last; a computed one left in place keeps every read a copy.
        let Some(kept) = args
            .iter_mut()
            .rev()
            .find(|arg| reads_slot(&arg.node, slot))
        else {
            continue;
        };
        for (_, _, value) in &mut bindings {
            clear_last_use(&mut value.node, slot);
        }
        if let Some(local) = root_local_mut(&mut kept.node)
            && local.slot == slot
        {
            local.last_use = true;
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

/// Whether an `Int` argument is a boxed `AverInt` as written, by the reading
/// of `bare_i64`: an explicit box, a value out of `?`, a record field, a
/// builtin's result, a call to a function that returns one, or arithmetic
/// with such an operand (raw arithmetic needs raw operands).
fn boxed_int(expr: &MirExpr, boxed_return: &HashSet<FnId>) -> bool {
    match expr {
        MirExpr::Box(_) | MirExpr::Try(_) | MirExpr::Project(_) => true,
        MirExpr::Call(call) => match call.node.callee {
            MirCallee::Fn(id) => boxed_return.contains(&id),
            MirCallee::Builtin(_) => true,
            MirCallee::Intrinsic(_) | MirCallee::LocalSlot { .. } => false,
        },
        MirExpr::BinOp(op) => {
            boxed_int(&op.node.lhs.node, boxed_return) || boxed_int(&op.node.rhs.node, boxed_return)
        }
        MirExpr::Neg(inner) => boxed_int(&inner.node, boxed_return),
        _ => false,
    }
}

fn root_local_mut(expr: &mut MirExpr) -> Option<&mut MirLocal> {
    match expr {
        MirExpr::Local(local) if !local.node.name.is_empty() => Some(&mut local.node),
        MirExpr::Project(project) => root_local_mut(&mut project.node.base.node),
        _ => None,
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

/// The slots read for the last time somewhere in `expr`.
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
