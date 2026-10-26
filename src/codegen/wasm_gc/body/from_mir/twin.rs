//! Emitting a twin (`wasm_gc::twins`) and the calls that use one.
//!
//! A twin's body is its function's body. A tail that has its value as a
//! machine word or as the struct's fields returns them at once; any other
//! tail leaves the packed value, which the end of the twin takes apart.
//!
//! An `Int` param the twin takes unpacked arrives as an `i64` and, after
//! the twin's other params, a ref: null when the value is the `i64`,
//! otherwise the packed value itself. The twin's entry (`module.rs`, which
//! adds the ref params) reads a Small packed value into its `i64` and keeps
//! the box, so the body boxes the param at most once, where it wants the
//! value packed. An `Int` local bound to a value with its machine word, and
//! read where the word serves, is held the same way.

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};

use wasm_encoder::Instruction as I;
use wasm_encoder::{BlockType, HeapType};

use super::*;
use crate::codegen::wasm_gc::twins::{TwinEntry, TwinKind, has_word, read_as_word, tails};
use crate::codegen::wasm_gc::types::struct_ref;
use crate::ir::mir::LocalId;

/// The twin being emitted.
struct Current {
    entry: TwinEntry,
    /// The tails (by node address) whose value is its result.
    tails: HashSet<usize>,
    /// The slots of the params it takes unpacked, by position.
    params: Vec<(usize, u32)>,
    /// The slots it holds as an `i64`: those, then some `Int` locals'.
    words: Vec<u32>,
    /// For each such slot: the local of its packed value (the one the
    /// caller passed or the value came as, or the box made on its first
    /// packed read) and its Big flag.
    held: HashMap<u32, (u32, u32)>,
}

thread_local! {
    /// Whether the fn being emitted may call twins (`module.rs` decides: not
    /// a fn a `--certify` build keeps).
    static CALLS: Cell<bool> = const { Cell::new(false) };
    static SELF: RefCell<Option<Current>> = const { RefCell::new(None) };
    /// How deep `emit_mir_expr` is, and the depth whose `struct.new` is the
    /// twin's result.
    static DEPTH: Cell<u32> = const { Cell::new(0) };
    static RETURN_AT: Cell<Option<u32>> = const { Cell::new(None) };
}

pub(crate) fn set_calls(on: bool) {
    CALLS.with(|c| c.set(on));
}

/// The slots the twin being emitted holds as an `i64`: the params it takes
/// unpacked, and each `Int` local (`is_int`) bound, by a `let` or as an
/// `Int` match's subject, to a value with its machine word, that `body`
/// reads where the word serves.
pub(crate) fn word_slots(
    body: &Spanned<MirExpr>,
    is_int: &dyn Fn(u32) -> bool,
    fn_map: &FnMap,
) -> Vec<u32> {
    fn bound<'e>(e: &'e MirExpr, out: &mut Vec<(u32, &'e MirExpr)>) {
        match e {
            MirExpr::Let(l) if !l.node.binding_name.is_empty() => {
                out.push((l.node.binding.0, &l.node.value.node));
            }
            MirExpr::Match(m) => out.extend(m.node.arms.iter().filter_map(|a| match a.pattern {
                MirPattern::Bind(slot, _) => Some((slot.0, &m.node.subject.node)),
                _ => None,
            })),
            _ => {}
        }
        crate::ir::mir::expr::walk_children(e, &mut |c| bound(c, out));
    }
    let int_twin = |id| {
        fn_map
            .twins
            .get(&id)
            .is_some_and(|t| t.kind == TwinKind::Int)
    };
    SELF.with(|s| {
        let mut s = s.borrow_mut();
        let Some(c) = s.as_mut() else {
            return Vec::new();
        };
        let mut values = Vec::new();
        bound(&body.node, &mut values);
        let local = |(slot, value): &&(u32, &MirExpr)| {
            is_int(*slot) && has_word(value, &int_twin) && read_as_word(body, LocalId(*slot))
        };
        let locals = values.iter().filter(local).map(|(slot, _)| *slot);
        c.words = c
            .params
            .iter()
            .map(|(_, slot)| *slot)
            .chain(locals)
            .collect();
        c.words.clone()
    })
}

/// Start emitting the twin `entry` of `mir_fn`, or stop.
pub(crate) fn set_self(twin: Option<(TwinEntry, &MirFn)>) {
    SELF.with(|s| {
        *s.borrow_mut() = twin.map(|(entry, mir_fn)| {
            let mut ts = Vec::new();
            tails(&mir_fn.body, &mut ts);
            let params = mir_fn
                .params
                .iter()
                .enumerate()
                .filter(|(k, _)| entry.unpacked.get(*k).copied().unwrap_or(false))
                .map(|(k, p)| (k, p.local.0))
                .collect();
            Current {
                entry,
                tails: ts.iter().map(|t| *t as *const _ as usize).collect(),
                params,
                words: Vec::new(),
                held: HashMap::new(),
            }
        })
    });
    RETURN_AT.with(|r| r.set(None));
}

pub(crate) fn self_kind() -> Option<TwinKind> {
    SELF.with(|s| s.borrow().as_ref().map(|c| c.entry.kind.clone()))
}

/// The kind of the twin being emitted when `e` is one of its tails.
pub(crate) fn tail_kind(e: &Spanned<MirExpr>) -> Option<TwinKind> {
    if int_tree::in_exact() {
        return None;
    }
    let key = e as *const _ as usize;
    SELF.with(|s| {
        s.borrow()
            .as_ref()
            .filter(|c| c.tails.contains(&key))
            .map(|c| c.entry.kind.clone())
    })
}

/// The packed-value and Big-flag locals of a slot the twin being emitted
/// holds (the slot itself holds the `i64`).
pub(crate) fn held(slot: u32) -> Option<(u32, u32)> {
    SELF.with(|s| s.borrow().as_ref().and_then(|c| c.held.get(&slot).copied()))
}

/// One level of `emit_mir_expr`, undone when dropped.
pub(crate) struct Level;

pub(crate) fn enter() -> Level {
    DEPTH.with(|d| d.set(d.get() + 1));
    Level
}

impl Drop for Level {
    fn drop(&mut self) {
        DEPTH.with(|d| d.set(d.get() - 1));
    }
}

/// The struct the node being emitted builds is the twin's result.
pub(crate) fn return_struct_here() {
    RETURN_AT.with(|r| r.set(Some(DEPTH.with(Cell::get))));
}

pub(crate) fn clear_return() {
    RETURN_AT.with(|r| r.set(None));
}

/// `struct.new $idx`, or, for the twin's own result, `return` with the
/// fields on the stack.
pub(crate) fn struct_new(func: &mut Function, idx: u32) {
    let depth = DEPTH.with(Cell::get);
    let here = RETURN_AT.with(|r| r.get() == Some(depth))
        && matches!(self_kind(), Some(TwinKind::Fields { struct_idx, .. }) if struct_idx == idx);
    if here {
        RETURN_AT.with(|r| r.set(None));
        func.instruction(&I::Return);
    } else {
        func.instruction(&I::StructNew(idx));
    }
}

/// `return` with struct `idx`'s fields on the stack, from the middle of a
/// body (`?`, `?!`): the struct, or, as a twin's own result, the fields.
pub(crate) fn return_struct(func: &mut Function, idx: u32) {
    if !matches!(self_kind(), Some(TwinKind::Fields { struct_idx, .. }) if struct_idx == idx) {
        func.instruction(&I::StructNew(idx));
    }
    func.instruction(&I::Return);
}

/// The twin of the fn `e` calls, when the fn being emitted may call it.
pub(crate) fn callee<'e>(
    e: &'e Spanned<MirExpr>,
    ctx: &EmitCtx<'_>,
) -> Option<(&'e crate::ir::mir::MirCall, TwinEntry)> {
    match &e.node {
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Fn(id) => of_fn(id, ctx).map(|t| (&c.node, t)),
            _ => None,
        },
        _ => None,
    }
}

/// The twin of a tail-call target, inside a twin of the same kind.
pub(crate) fn tail_target(target: crate::ir::FnId, ctx: &EmitCtx<'_>) -> Option<TwinEntry> {
    let kind = self_kind()?;
    ctx.fn_map
        .twins
        .get(&target)
        .filter(|t| t.kind == kind)
        .cloned()
}

/// The twin `target` has, when the fn being emitted may call it.
pub(crate) fn of_fn(target: crate::ir::FnId, ctx: &EmitCtx<'_>) -> Option<TwinEntry> {
    if !CALLS.with(Cell::get) {
        return None;
    }
    ctx.fn_map.twins.get(&target).cloned()
}

fn aint(ctx: &EmitCtx<'_>) -> u32 {
    ctx.registry.aint_struct_idx.unwrap_or_default()
}

/// Whether `arg` has its `Int` on the machine word without boxing it: a
/// literal, a raw value, an `Int` tree, an `Int` twin's result or a slot a
/// twin holds.
pub(crate) fn unpackable(arg: &Spanned<MirExpr>, ctx: &EmitCtx<'_>) -> bool {
    match &arg.node {
        MirExpr::Literal(l) => matches!(l.node, Literal::Int(_)),
        MirExpr::Box(_) => true,
        MirExpr::Local(l) => held(l.node.slot.0).is_some(),
        _ => {
            callee(arg, ctx).is_some_and(|(_, t)| t.kind == TwinKind::Int)
                || mir_renders_raw_i64(&arg.node, ctx)
                || int_tree::makes_word(arg, ctx)
        }
    }
}

/// Whether a call with `args` is better made to `twin`: an arg it takes
/// unpacked has its value unpacked.
pub(crate) fn worth_it(args: &[Spanned<MirExpr>], twin: &TwinEntry, ctx: &EmitCtx<'_>) -> bool {
    args.iter()
        .enumerate()
        .any(|(j, a)| twin.unpacked.get(j).copied().unwrap_or(false) && unpackable(a, ctx))
}

/// Push `arg` as the `i64` of an unpacked param; the local holding its
/// packed value, or nothing for null, is the ref pushed after the last arg.
fn emit_word_arg(
    func: &mut Function,
    arg: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<u32>>, WasmGcError> {
    match &arg.node {
        MirExpr::Local(l) if held(l.node.slot.0).is_some() => {
            let (packed, _) = held(l.node.slot.0).expect("guarded");
            func.instruction(&I::LocalGet(slots.local(l.node.slot.0)));
            return Ok(Some(Some(packed)));
        }
        MirExpr::Literal(l) if matches!(l.node, Literal::Int(_)) => {
            let Literal::Int(v) = l.node else {
                unreachable!()
            };
            func.instruction(&I::I64Const(v));
            return Ok(Some(None));
        }
        MirExpr::Box(inner) => {
            return Ok(emit_mir_int_raw(func, inner, slots, ctx)?.map(|_| None));
        }
        _ if mir_renders_raw_i64(&arg.node, ctx) => {
            return Ok(emit_mir_int_raw(func, arg, slots, ctx)?.map(|_| None));
        }
        _ => {}
    }
    let r = slots.fresh_local(struct_ref(aint(ctx)));
    // A packed value goes as itself; its `i64` means nothing.
    let packed = |f: &mut Function| {
        f.instruction(&I::LocalSet(r));
        f.instruction(&I::I64Const(0));
    };
    if let Some((call, twin)) = callee(arg, ctx)
        && twin.kind == TwinKind::Int
    {
        // An `Int` twin's ref is non-null only for a Big value.
        if emit_call(func, call, &twin, slots, ctx)?.is_none() {
            return Ok(None);
        }
        func.instruction(&I::LocalSet(r));
        return Ok(Some(Some(r)));
    }
    if int_tree::makes_word(arg, ctx) {
        func.instruction(&I::RefNull(HeapType::Concrete(aint(ctx))));
        func.instruction(&I::LocalSet(r));
        let exact = &mut |f: &mut Function| {
            if emit_mir_expr(f, arg, slots, ctx)?.is_none() {
                return Ok(None);
            }
            packed(f);
            Ok(Some(()))
        };
        return Ok(int_tree::emit_word(func, arg, slots, ctx, exact)?.map(|()| Some(r)));
    }
    if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
        return Ok(None);
    }
    packed(func);
    Ok(Some(Some(r)))
}

/// `arg`, an `Int`, as the `i64` a builtin or a carrier takes, where `arg`
/// has it on the machine word ([`unpackable`]); `to_word` lowers a packed
/// value and is the identity on a Small one (`__aint_to_i64_sat`,
/// `__aint_to_i64_checked`). `None` for any other `arg`, and in a fn a
/// `--certify` build keeps.
pub(crate) fn emit_word_lowered(
    func: &mut Function,
    arg: &Spanned<MirExpr>,
    to_word: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<()>>, WasmGcError> {
    if !int_tree::allowed() || !unpackable(arg, ctx) {
        return Ok(None);
    }
    let Some(packed) = emit_word_arg(func, arg, slots, ctx)? else {
        return Ok(Some(None));
    };
    if let Some(r) = packed {
        let w = slots.fresh_local(ValType::I64);
        for i in [
            I::LocalSet(w),
            I::LocalGet(r),
            I::RefIsNull,
            I::If(BlockType::Result(ValType::I64)),
            I::LocalGet(w),
            I::Else,
            I::LocalGet(r),
            I::Call(to_word),
            I::End,
        ] {
            func.instruction(&i);
        }
    }
    Ok(Some(Some(())))
}

/// Push the args of a call to `twin` in its convention: an `Int` it takes
/// unpacked as an `i64`, and, after the last arg, the ref of each.
fn emit_args(
    func: &mut Function,
    target: crate::ir::FnId,
    args: &[Spanned<MirExpr>],
    twin: &TwinEntry,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let mut refs = Vec::new();
    for (j, arg) in args.iter().enumerate() {
        if twin.unpacked.get(j).copied().unwrap_or(false) {
            match emit_word_arg(func, arg, slots, ctx)? {
                Some(r) => refs.push(r),
                None => return Ok(None),
            }
        } else if ctx.callee_param_is_bare(target, j) {
            if emit_mir_int_raw(func, arg, slots, ctx)?.is_none() {
                return Ok(None);
            }
        } else if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    for r in refs {
        func.instruction(&match r {
            Some(local) => I::LocalGet(local),
            None => I::RefNull(HeapType::Concrete(aint(ctx))),
        });
    }
    Ok(Some(()))
}

/// Push the args of `call` and call the twin.
pub(crate) fn emit_call(
    func: &mut Function,
    call: &crate::ir::mir::MirCall,
    twin: &TwinEntry,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let MirCallee::Fn(id) = call.callee else {
        return Ok(None);
    };
    if emit_args(func, id, &call.args, twin, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&I::Call(twin.wasm_idx));
    Ok(Some(()))
}

/// Push the args of a tail call and make it to the twin.
pub(crate) fn emit_tail_call(
    func: &mut Function,
    target: crate::ir::FnId,
    args: &[Spanned<MirExpr>],
    twin: &TwinEntry,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if emit_args(func, target, args, twin, slots, ctx)?.is_none() {
        return Ok(None);
    }
    emit_return_call_insn(func, twin.wasm_idx, ctx.self_wasm_idx);
    Ok(Some(()))
}

/// With a twin's result on the stack: the packed value its fn returns.
pub(crate) fn pack_result(
    func: &mut Function,
    kind: &TwinKind,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    match kind {
        TwinKind::Plain => {}
        TwinKind::Int => {
            let (n, r) = hold_int(func, slots, ctx);
            pack_held(func, n, r, ctx)?;
        }
        TwinKind::Fields { struct_idx, .. } => {
            func.instruction(&I::StructNew(*struct_idx));
        }
    }
    Ok(())
}

/// The locals of the slots the twin being emitted holds: its entry
/// (`module.rs`) fills a param's from its ref param, [`bind`] a local's.
pub(crate) fn emit_entry(slots: &SlotTable, ctx: &EmitCtx<'_>) {
    SELF.with(|s| {
        if let Some(c) = s.borrow_mut().as_mut() {
            c.held = c
                .words
                .iter()
                .map(|&slot| {
                    let packed = slots.fresh_local(struct_ref(aint(ctx)));
                    (slot, (packed, slots.fresh_local(ValType::I32)))
                })
                .collect();
        }
    });
}

/// Bind the local in `slot`, which the twin being emitted holds, to `value`,
/// leaving nothing to store (`Some(false)`).
pub(crate) fn bind(
    func: &mut Function,
    slot: u32,
    value: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let Some(packed) = emit_word_arg(func, value, slots, ctx)? else {
        return Ok(None);
    };
    set_held(func, slot, packed, ctx);
    Ok(Some(false))
}

/// With the `i64` of a value on the stack and its packed value, or null, in
/// the local `packed` (`None`: the value is the `i64`): set the held `slot`.
pub(crate) fn set_held(func: &mut Function, slot: u32, packed: Option<u32>, ctx: &EmitCtx<'_>) {
    func.instruction(&I::LocalSet(slot));
    let Some((held_packed, big)) = held(slot) else {
        return;
    };
    // The box and the Big flag belong to the value bound before, if any: a
    // packed read of a slot bound again in the same frame must not return
    // the old box. Today a slot is bound once per frame (no rebinding, a loop
    // is a tail call), but nothing else guards that.
    func.instruction(&I::RefNull(HeapType::Concrete(aint(ctx))));
    func.instruction(&I::LocalSet(held_packed));
    func.instruction(&I::I32Const(0));
    func.instruction(&I::LocalSet(big));
    if let Some(from) = packed {
        read_packed(func, I::LocalGet(from), slot, held_packed, big, aint(ctx));
    }
}

/// With `from` an `Int` packed or null: a Small one's `i64` into `word`, the
/// packed value into `packed`, and `big` set for a Big one.
pub(crate) fn read_packed(
    func: &mut Function,
    from: I<'_>,
    word: u32,
    packed: u32,
    big: u32,
    aint: u32,
) {
    let get = |field_index| I::StructGet {
        struct_type_index: aint,
        field_index,
    };
    for i in [
        I::Block(BlockType::Empty),
        from,
        I::BrOnNull(0),
        I::LocalTee(packed),
        get(1),
        I::RefIsNull,
        I::If(BlockType::Empty),
        I::LocalGet(packed),
        get(0),
        I::LocalSet(word),
        I::Else,
        I::I32Const(1),
        I::LocalSet(big),
        I::End,
        I::End,
    ] {
        func.instruction(&i);
    }
}

/// Per unpacked param, in order: its position, and the locals of its
/// packed value and of its Big flag, as the twin's body was emitted.
pub(crate) fn entry_locals() -> Vec<(usize, u32, u32)> {
    SELF.with(|s| {
        s.borrow().as_ref().map_or_else(Vec::new, |c| {
            c.params
                .iter()
                .filter_map(|(k, slot)| {
                    let (packed, big) = c.held.get(slot)?;
                    Some((*k, *packed, *big))
                })
                .collect()
        })
    })
}

/// A held slot read where its value is wanted packed: its box, made on the
/// first such read (a Big value came packed).
pub(crate) fn emit_packed_param(
    func: &mut Function,
    slot: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let (r, _) = held(slot).expect("a held slot");
    func.instruction(&I::LocalGet(r));
    func.instruction(&I::RefIsNull);
    func.instruction(&I::If(BlockType::Empty));
    func.instruction(&I::LocalGet(slots.local(slot)));
    lift_i64_result_to_aint(func, ctx)?;
    func.instruction(&I::LocalSet(r));
    func.instruction(&I::End);
    func.instruction(&I::LocalGet(r));
    Ok(())
}

/// A held slot as the result of an `Int` twin: the ref only for a Big value.
pub(crate) fn return_param(func: &mut Function, slot: u32, slots: &SlotTable, ctx: &EmitCtx<'_>) {
    let (r, big) = held(slot).expect("a held slot");
    func.instruction(&I::LocalGet(slots.local(slot)));
    func.instruction(&I::LocalGet(r));
    func.instruction(&I::RefNull(HeapType::Concrete(aint(ctx))));
    func.instruction(&I::LocalGet(big));
    func.instruction(&I::TypedSelect(struct_ref(aint(ctx))));
    func.instruction(&I::Return);
}

/// With an `i64` on the stack: return it as the twin's `Int`.
pub(crate) fn return_small(func: &mut Function, ctx: &EmitCtx<'_>) {
    func.instruction(&I::RefNull(HeapType::Concrete(aint(ctx))));
    func.instruction(&I::Return);
}

/// With the packed result on the stack, at the end of a twin: its unpacked
/// form. A Small `Int` comes back as its `i64`.
pub(crate) fn unpack_result(
    func: &mut Function,
    kind: &TwinKind,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) {
    match kind {
        TwinKind::Plain => {}
        TwinKind::Int => {
            let a = aint(ctx);
            let t = slots.fresh_local(struct_ref(a));
            let get = |field_index| I::StructGet {
                struct_type_index: a,
                field_index,
            };
            func.instruction(&I::LocalTee(t));
            func.instruction(&get(0));
            func.instruction(&I::RefNull(HeapType::Concrete(a)));
            func.instruction(&I::LocalGet(t));
            func.instruction(&I::LocalGet(t));
            func.instruction(&get(1));
            func.instruction(&I::RefIsNull);
            func.instruction(&I::TypedSelect(struct_ref(a)));
        }
        TwinKind::Fields { struct_idx, fields } => {
            let t = slots.fresh_local(struct_ref(*struct_idx));
            func.instruction(&I::LocalSet(t));
            for j in 0..fields.len() {
                func.instruction(&I::LocalGet(t));
                func.instruction(&I::StructGet {
                    struct_type_index: *struct_idx,
                    field_index: j as u32,
                });
            }
        }
    }
}

/// An `Int` twin's result on the stack, into two fresh locals: `(n, r)`.
pub(crate) fn hold_int(func: &mut Function, slots: &SlotTable, ctx: &EmitCtx<'_>) -> (u32, u32) {
    let n = slots.fresh_local(ValType::I64);
    let r = slots.fresh_local(struct_ref(aint(ctx)));
    func.instruction(&I::LocalSet(r));
    func.instruction(&I::LocalSet(n));
    (n, r)
}

/// The `Int` held in `(n, r)`, packed.
pub(crate) fn pack_held(
    func: &mut Function,
    n: u32,
    r: u32,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    func.instruction(&I::LocalGet(r));
    func.instruction(&I::RefIsNull);
    func.instruction(&I::If(BlockType::Result(struct_ref(aint(ctx)))));
    func.instruction(&I::LocalGet(n));
    lift_i64_result_to_aint(func, ctx)?;
    func.instruction(&I::Else);
    func.instruction(&I::LocalGet(r));
    func.instruction(&I::End);
    Ok(())
}
