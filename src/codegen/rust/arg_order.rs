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
//! loop keeps its params in place and is emitted apart. So does a write
//! (`Map.set`, `Map.remove`, `Vector.set`) whose later argument reads the
//! collection last (`Map.set(t, fresh(n, t)?, v)`): the write then takes the
//! collection over instead of copying it. A write is rewritten only then, so
//! the shapes the emitter fuses keep theirs. An argument that cannot be bound (a raw `i64` the callee takes
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
    let builtins = &program.builtins;
    for mir_fn in program.fns.values_mut() {
        // A member of a mutual trampoline calls itself by a bounce too.
        let self_bounces = ctx.mutual_tco_members.contains(&mir_fn.fn_id);
        let mut body = std::mem::replace(
            &mut mir_fn.body,
            Spanned::bare(MirExpr::Literal(Spanned::bare(crate::ast::Literal::Unit))),
        );
        let calls = Calls {
            boxed_return: &boxed_return,
            builtins,
            self_bounces,
            carrier_ctors: &ctx.rust_i64_carrier_ctors,
        };
        rewrite(&mut body, mir_fn, &calls);
        mir_fn.body = body;
    }
    ctx.mir_program = Some(program);
}

/// What the rewrite needs to know about the calls of a function.
struct Calls<'a> {
    /// Functions whose Rust return type is a boxed `AverInt`, not `i64`.
    boxed_return: &'a HashSet<FnId>,
    builtins: &'a [String],
    /// The function is a member of a mutual trampoline.
    self_bounces: bool,
    /// Smart constructors of carriers held as a raw `i64`: called on a
    /// boxed machine word, the emitter binds the word itself and checks it
    /// raw (`emit_carrier_ctor_on_word`).
    carrier_ctors: &'a std::collections::HashMap<FnId, super::carrier::CarrierCtor>,
}

/// Rewrite every call in `expr`, innermost first.
fn rewrite(expr: &mut Spanned<MirExpr>, mir_fn: &mut MirFn, calls: &Calls<'_>) {
    crate::ir::mir::expr::walk_children_mut(&mut expr.node, &mut |child| {
        rewrite(child, mir_fn, calls)
    });
    if matches!(expr.node, MirExpr::RecordUpdate(_)) {
        let hoisted = hoist_reads_after_base_call(expr, mir_fn);
        wrap_in_lets(expr, hoisted);
        return;
    }
    let (boxed_return, builtins, self_bounces) =
        (calls.boxed_return, calls.builtins, calls.self_bounces);
    let mut write = false;
    let args = match &expr.node {
        MirExpr::Call(call) => match call.node.callee {
            MirCallee::Fn(id)
                if calls.carrier_ctors.contains_key(&id)
                    && matches!(call.node.args.as_slice(), [arg] if matches!(arg.node, MirExpr::Box(_))) =>
            {
                return;
            }
            MirCallee::Fn(_) => &call.node.args,
            MirCallee::Builtin(id)
                if matches!(
                    builtins.get(id.0 as usize).map(String::as_str),
                    Some("Map.set" | "Map.remove" | "Vector.set")
                ) =>
            {
                write = true;
                &call.node.args
            }
            _ => return,
        },
        MirExpr::TailCall(call) if self_bounces || call.node.target != mir_fn.fn_id => {
            &call.node.args
        }
        // The self loop assigns its params in place, but a record update
        // read again by a later argument is handled as for any call.
        MirExpr::TailCall(call) => {
            let updates: Vec<usize> = call
                .node
                .args
                .iter()
                .enumerate()
                .filter(|(_, a)| matches!(a.node, MirExpr::RecordUpdate(_)))
                .map(|(j, _)| j)
                .collect();
            let hoisted = hoist_reads_after_update(expr, mir_fn, &updates);
            wrap_in_lets(expr, hoisted);
            return;
        }
        _ => return,
    };
    // The computed arguments up to the first one that cannot be bound.
    let mut bound: Vec<usize> = args
        .iter()
        .enumerate()
        .filter(|(_, arg)| !read(&arg.node))
        .map_while(|(j, arg)| bindable(arg, boxed_return).then_some(j))
        .collect();
    // A field read last after the value that holds it is passed whole
    // (`f(t, t.dense)`) is read first, so the value moves.
    if !write {
        for j in field_reads_after_holder(args) {
            if !bound.contains(&j) && bindable(&args[j], boxed_return) {
                bound.push(j);
            }
        }
        bound.sort_unstable();
    }
    // A write is rewritten only when a bound argument reads the collection
    // it takes (or what holds it) last.
    if write {
        let Some((target, path)) = args.first().and_then(|a| read_path(&a.node)) else {
            return;
        };
        if !bound
            .iter()
            .any(|&j| reads_last(&args[j].node, target.slot, &path))
        {
            return;
        }
    }
    let hoisted = if write {
        Vec::new()
    } else {
        hoist_reads_after_update(expr, mir_fn, &bound)
    };
    if !bound.is_empty() {
        apply(expr, mir_fn, &bound);
    }
    wrap_in_lets(expr, hoisted);
}

/// A local bound to a value before the call: its slot, name and value.
type Binding = (LocalId, String, Spanned<MirExpr>);

/// Where a bound argument updates a record (`T.update(t, ...)`) and every
/// later argument reads only fields of it (`f(T.update(t, n = t.n + 1),
/// g(t.id))`), those field reads move before the call and the update's base
/// becomes the last read of `t`, so the update takes the record over instead
/// of copying it. Reading a field has no effect, so this keeps what the call
/// computes. The reads are returned as the bindings to put before the call.
fn hoist_reads_after_update(
    expr: &mut Spanned<MirExpr>,
    mir_fn: &mut MirFn,
    bound: &[usize],
) -> Vec<Binding> {
    let args = match &mut expr.node {
        MirExpr::Call(call) => &mut call.node.args,
        MirExpr::TailCall(call) => &mut call.node.args,
        _ => return Vec::new(),
    };
    let mut hoisted = Vec::new();
    for &i in bound {
        let MirExpr::RecordUpdate(update) = &args[i].node else {
            continue;
        };
        let MirExpr::Local(base) = &update.node.base.node else {
            continue;
        };
        if base.node.last_use || base.node.name.is_empty() {
            continue;
        }
        let slot = base.node.slot;
        let later = &mut args[i + 1..];
        if later.iter().any(|a| reads_whole(&a.node, slot))
            || !later.iter().any(|a| reads_last(&a.node, slot, &[]))
            || !later.iter_mut().all(|a| fields_typed(a, slot))
        {
            continue;
        }
        for arg in &mut args[i + 1..] {
            hoist_fields(arg, slot, mir_fn, &mut hoisted);
        }
        if let MirExpr::RecordUpdate(update) = &mut args[i].node
            && let MirExpr::Local(base) = &mut update.node.base.node
        {
            base.node.last_use = true;
        }
    }
    hoisted
}

/// Where an update's base is a call handed a record whole (`W.update(f(w,
/// x), n = w.n + 1)`) and the update's fields read only fields of it, those
/// reads move before the call and the call takes the record over. The
/// call's other arguments must not read the record, though bindings the
/// argument order put before it may (they run first).
fn hoist_reads_after_base_call(expr: &mut Spanned<MirExpr>, mir_fn: &mut MirFn) -> Vec<Binding> {
    let MirExpr::RecordUpdate(update) = &mut expr.node else {
        return Vec::new();
    };
    let update = &mut update.node;
    // The call under the bindings the argument order put first.
    let mut call = &mut *update.base;
    while matches!(call.node, MirExpr::Let(_)) {
        let MirExpr::Let(binding) = &mut call.node else {
            unreachable!("matched just above")
        };
        call = &mut *binding.node.body;
    }
    let args = match &mut call.node {
        MirExpr::Call(call) if matches!(call.node.callee, MirCallee::Fn(_)) => &mut call.node.args,
        _ => return Vec::new(),
    };
    let whole: Vec<usize> = args
        .iter()
        .enumerate()
        .filter(|(_, a)| matches!(&a.node, MirExpr::Local(l) if !l.node.last_use && !l.node.name.is_empty()))
        .map(|(j, _)| j)
        .collect();
    for j in whole {
        let MirExpr::Local(local) = &args[j].node else {
            continue;
        };
        let slot = local.node.slot;
        let others_read = args
            .iter()
            .enumerate()
            .any(|(k, a)| k != j && reads_slot(&a.node, slot));
        let fields = &mut update.updates;
        if others_read
            || fields.iter().any(|f| reads_whole(&f.value.node, slot))
            || !fields.iter().any(|f| reads_last(&f.value.node, slot, &[]))
            || !fields.iter_mut().all(|f| fields_typed(&mut f.value, slot))
        {
            continue;
        }
        let mut hoisted = Vec::new();
        for field in fields.iter_mut() {
            hoist_fields(&mut field.value, slot, mir_fn, &mut hoisted);
        }
        if let MirExpr::Local(local) = &mut args[j].node {
            local.node.last_use = true;
        }
        return hoisted;
    }
    Vec::new()
}

/// Whether `expr` reads `slot` other than through a field of it.
fn reads_whole(expr: &MirExpr, slot: LocalId) -> bool {
    match expr {
        MirExpr::Project(_) if read_path(expr).is_some_and(|(l, _)| l.slot == slot) => false,
        MirExpr::Local(local) => local.node.slot == slot,
        MirExpr::Call(call) if matches!(&call.node.callee, MirCallee::LocalSlot { slot: s, .. } if u32::from(*s) == slot.0) => {
            true
        }
        _ => {
            let mut found = false;
            crate::ir::mir::expr::walk_children(expr, &mut |child| {
                found = found || reads_whole(child, slot);
            });
            found
        }
    }
}

/// Whether every field read of `slot` in `expr` has a type, so it can be
/// bound to a local.
fn fields_typed(expr: &mut Spanned<MirExpr>, slot: LocalId) -> bool {
    if read_path(&expr.node).is_some_and(|(l, path)| l.slot == slot && !path.is_empty()) {
        return expr.ty().is_some();
    }
    let mut typed = true;
    crate::ir::mir::expr::walk_children_mut(&mut expr.node, &mut |child| {
        typed = typed && fields_typed(child, slot);
    });
    typed
}

/// Replace every field read of `slot` in `expr` with a fresh local bound
/// to it, none of them the last read of `slot`.
fn hoist_fields(
    expr: &mut Spanned<MirExpr>,
    slot: LocalId,
    mir_fn: &mut MirFn,
    out: &mut Vec<Binding>,
) {
    let is_field =
        read_path(&expr.node).is_some_and(|(l, path)| l.slot == slot && !path.is_empty());
    if !is_field {
        crate::ir::mir::expr::walk_children_mut(&mut expr.node, &mut |child| {
            hoist_fields(child, slot, mir_fn, out)
        });
        return;
    }
    let fresh = LocalId(mir_fn.local_count);
    mir_fn.local_count += 1;
    let name = generated_ident(&format!("arg{}", fresh.0));
    let read = Spanned::new(
        MirExpr::Local(Spanned::new(
            MirLocal {
                slot: fresh,
                last_use: true,
                name: name.clone(),
            },
            expr.line,
        )),
        expr.line,
    );
    if let Some(ty) = expr.ty.get() {
        read.set_ty_shared(ty);
    }
    let mut value = std::mem::replace(expr, read);
    clear_last_use(&mut value.node, slot);
    out.push((fresh, name, value));
}

/// Put `bindings` around `expr`, the first outermost.
fn wrap_in_lets(expr: &mut Spanned<MirExpr>, bindings: Vec<Binding>) {
    if bindings.is_empty() {
        return;
    }
    let line = expr.line;
    let ty = expr.ty.get().cloned();
    let mut body = std::mem::replace(
        expr,
        Spanned::bare(MirExpr::Literal(Spanned::bare(crate::ast::Literal::Unit))),
    );
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

/// Whether `expr` only reads: a variable, a field of one, a literal or a
/// function named as a value, or one of them as a saturated position.
fn read(expr: &MirExpr) -> bool {
    match expr {
        MirExpr::Literal(_) | MirExpr::FnValue(_) | MirExpr::Local(_) => true,
        MirExpr::Project(project) => read(&project.node.base.node),
        MirExpr::Box(inner) | MirExpr::Unbox(inner) => read(&inner.node),
        MirExpr::Call(call) => {
            matches!(
                call.node.callee,
                MirCallee::Intrinsic(
                    crate::ir::hir::BuiltinIntrinsic::StrIndexPosition
                        | crate::ir::hir::BuiltinIntrinsic::PositionOffset
                )
            ) && call.node.args.iter().all(|a| read(&a.node))
        }
        _ => false,
    }
}

/// The arguments that read a field of a variable last, where an earlier
/// argument passes a part of that variable which holds the field. Reading
/// has no effect, so the field may be read before the call instead.
fn field_reads_after_holder(args: &[Spanned<MirExpr>]) -> Vec<usize> {
    let mut out = Vec::new();
    for (j, arg) in args.iter().enumerate() {
        let Some((local, path)) = read_path(&arg.node) else {
            continue;
        };
        if path.is_empty() || !local.last_use {
            continue;
        }
        let held = args[..j].iter().any(|earlier| {
            read_path(&earlier.node).is_some_and(|(holder, prefix)| {
                holder.slot == local.slot
                    && prefix.len() < path.len()
                    && prefix.iter().zip(&path).all(|(a, b)| a == b)
            })
        });
        if held {
            out.push(j);
        }
    }
    out
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

/// The local a read of a variable or of a field path of one starts from,
/// and the fields it goes through.
fn read_path(expr: &MirExpr) -> Option<(&MirLocal, Vec<&str>)> {
    match expr {
        MirExpr::Local(local) if !local.node.name.is_empty() => Some((&local.node, Vec::new())),
        MirExpr::Project(project) => {
            let (local, mut path) = read_path(&project.node.base.node)?;
            path.push(&project.node.field);
            Some((local, path))
        }
        _ => None,
    }
}

/// Whether `expr` reads `slot` for the last time through a part that
/// overlaps `path` (one path extends the other).
fn reads_last(expr: &MirExpr, slot: LocalId, path: &[&str]) -> bool {
    if let Some((local, read)) = read_path(expr) {
        return local.slot == slot && local.last_use && read.iter().zip(path).all(|(a, b)| a == b);
    }
    let mut found = false;
    crate::ir::mir::expr::walk_children(expr, &mut |child| {
        found = found || reads_last(child, slot, path);
    });
    found
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
