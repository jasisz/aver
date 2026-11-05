//! A `String` parameter a self loop grows by appending, held as a buffer.
//!
//! On wasm-gc a `String` is an exact-length byte array, so `acc + x` copies
//! all of `acc`. A function that builds a string by tail-calling itself with
//! `acc + x` (`go(rest, acc + piece)`, `go(rest, "{acc}{piece}\n")`) copies
//! the whole accumulator on every step and takes time quadratic in the
//! length of what it builds.
//!
//! Such a parameter gets two more locals in the function, a buffer and a
//! length: the bytes `0..len` of the buffer are the parameter's value. The
//! self tail call appends the new pieces to the buffer, growing it by half
//! again when it is full, instead of building the new string, and leaves
//! the parameter itself as it was. The self tail loop
//! (`self_tail_loop.rs`) keeps the two locals across its iterations
//! instead of resetting them. While the buffer is null the parameter holds
//! the value; once it is set, the buffer does, and every other read of the
//! parameter takes the bytes out as a string of their own.
//!
//! The buffer is written only past `len`, and only after it was grown here:
//! the caller's string becomes the first buffer with no room left, so the
//! first append copies it. A read hands the buffer itself out only when it
//! is exactly full, and swaps a full copy in otherwise; either way the
//! buffer is full afterwards and the next append moves to a new one. No
//! string anyone holds is ever written, so the parameter need not be
//! uniquely owned.
//!
//! The parameter qualifies when the function runs its self tail calls as a
//! loop and, in every self tail call, the parameter appears only at its own
//! position: the call appends there when that argument is the parameter
//! followed by pieces (`acc + a + b`, or an interpolation that starts with
//! `{acc}`) none of which reads it; passes it on unchanged when the
//! argument is the parameter; and otherwise gives the parameter the new
//! value and drops the buffer. Reading it in no other argument keeps the
//! order of evaluation: no argument sees the appended value early.
//!
//! A `--certify` build keeps the plain form: the certificate wall lowers a
//! self loop that resets every local, and prints the string concatenation
//! the body states.

use std::cell::{Cell, RefCell};

use wasm_encoder::Instruction as I;
use wasm_encoder::{BlockType, HeapType, RefType};

use super::*;
use crate::ir::FnId;
use crate::ir::mir::expr::walk_children;
use crate::ir::mir::{MirFn, MirStrPart};

/// One parameter held as a buffer.
#[derive(Clone, Copy)]
struct Held {
    /// Its position among the params.
    position: usize,
    slot: u32,
    buf: u32,
    len: u32,
}

/// The function being emitted.
struct Current {
    fn_id: FnId,
    held: Vec<Held>,
    /// Scratch: the piece being appended, a grown or copied array, and the
    /// length the buffer needs.
    piece: u32,
    array: u32,
    need: u32,
}

thread_local! {
    /// Whether the functions being emitted may hold a parameter as a buffer
    /// (`module.rs`: not in a `--certify` build).
    static ALLOWED: Cell<bool> = const { Cell::new(false) };
    static CURRENT: RefCell<Option<Current>> = const { RefCell::new(None) };
    /// The carried locals of the function emitted last.
    static CARRIED: RefCell<Vec<u32>> = const { RefCell::new(Vec::new()) };
}

pub(crate) fn set_allowed(on: bool) {
    ALLOWED.with(|a| a.set(on));
}

/// The locals the self tail loop of the function just emitted keeps across
/// its iterations: each held parameter's buffer and length.
pub(crate) fn carried_locals() -> Vec<u32> {
    CARRIED.with(|c| c.borrow().clone())
}

/// Ends the emission [`begin`] started, also when it fails: no other body
/// sees the function's params as held.
pub(super) struct End;

impl Drop for End {
    fn drop(&mut self) {
        let carried = CURRENT.with(|c| {
            c.borrow_mut()
                .take()
                .map(|c| c.held.iter().flat_map(|h| [h.buf, h.len]).collect())
                .unwrap_or_default()
        });
        CARRIED.with(|c| *c.borrow_mut() = carried);
    }
}

/// Decide, before `mir_fn`'s body is emitted, which of its params it holds
/// as a buffer, and reserve their locals, until the returned [`End`] drops.
#[must_use]
pub(super) fn begin(mir_fn: &MirFn, slots: &SlotTable, ctx: &EmitCtx<'_>) -> End {
    CURRENT.with(|c| *c.borrow_mut() = None);
    if !ALLOWED.with(Cell::get)
        || !mir_fn.repr.self_tail_loop
        || std::env::var_os("AVER_NO_STRING_BUILDERS").is_some()
    {
        return End;
    }
    let Some(string_idx) = ctx.registry.string_array_type_idx else {
        return End;
    };
    let string = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_idx),
    });
    let positions: Vec<(usize, u32)> = mir_fn
        .params
        .iter()
        .enumerate()
        .filter(|(k, _)| matches!(ctx.params.get(*k), Some((_, Type::Str))))
        .map(|(k, p)| (k, p.local.0))
        .filter(|&(_, slot)| slots.by_slot.get(slot as usize) == Some(&string))
        .filter(|&(k, slot)| qualifies(&mir_fn.body.node, mir_fn.fn_id, k, slot))
        .collect();
    if positions.is_empty() {
        return End;
    }
    let held = positions
        .into_iter()
        .map(|(position, slot)| Held {
            position,
            slot,
            buf: slots.fresh_local(string),
            len: slots.fresh_local(ValType::I32),
        })
        .collect();
    let current = Current {
        fn_id: mir_fn.fn_id,
        held,
        piece: slots.fresh_local(string),
        array: slots.fresh_local(string),
        need: slots.fresh_local(ValType::I32),
    };
    CURRENT.with(|c| *c.borrow_mut() = Some(current));
    End
}

/// Does param `slot`, at `position`, qualify (see the module doc)? At least
/// one self tail call must append to it, or holding it gains nothing.
fn qualifies(body: &MirExpr, fn_id: FnId, position: usize, slot: u32) -> bool {
    let mut appends = false;
    let mut ok = true;
    visit_self_tail_calls(body, fn_id, &mut |args| {
        for (j, arg) in args.iter().enumerate() {
            if j == position {
                // Any other argument here replaces the value, reading the
                // param before the buffer is dropped.
                if let Some(pieces) = pieces(&arg.node, slot) {
                    appends |= !pieces.is_empty();
                    ok &= pieces.iter().all(|p| !p.reads(slot));
                }
            } else {
                ok &= !reads(&arg.node, slot);
            }
        }
    });
    ok && appends
}

fn visit_self_tail_calls<'a>(
    e: &'a MirExpr,
    fn_id: FnId,
    f: &mut dyn FnMut(&'a [Spanned<MirExpr>]),
) {
    if let MirExpr::TailCall(tc) = e
        && tc.node.target == fn_id
    {
        f(&tc.node.args);
    }
    walk_children(e, &mut |c| visit_self_tail_calls(c, fn_id, f));
}

fn reads(e: &MirExpr, slot: u32) -> bool {
    if let MirExpr::Local(l) = e {
        return l.node.slot.0 == slot;
    }
    let mut found = false;
    walk_children(e, &mut |c| found = found || reads(c, slot));
    found
}

/// What a self tail call appends to the param.
enum Piece<'a> {
    /// A `String` value.
    Value(&'a Spanned<MirExpr>),
    /// A literal.
    Text(&'a str),
    /// An interpolated value, turned into a `String` as interpolation does.
    Embed(&'a Spanned<MirExpr>),
}

impl Piece<'_> {
    fn reads(&self, slot: u32) -> bool {
        match self {
            Piece::Value(e) | Piece::Embed(e) => reads(&e.node, slot),
            Piece::Text(_) => false,
        }
    }
}

/// The pieces `arg` appends to the param in `slot`, in order: `Some([])` for
/// the param itself, `None` when `arg` is not the param followed by pieces.
fn pieces(arg: &MirExpr, slot: u32) -> Option<Vec<Piece<'_>>> {
    match arg {
        MirExpr::Local(l) if l.node.slot.0 == slot => Some(Vec::new()),
        MirExpr::BinOp(b)
            if matches!(b.node.op, BinOp::Add)
                && aver_type_str_of(&b.node.lhs).trim() == "String" =>
        {
            let mut pieces = pieces(&b.node.lhs.node, slot)?;
            pieces.push(match &b.node.rhs.node {
                MirExpr::Literal(l) => match &l.node {
                    Literal::Str(s) => Piece::Text(s),
                    _ => Piece::Value(&b.node.rhs),
                },
                _ => Piece::Value(&b.node.rhs),
            });
            Some(pieces)
        }
        MirExpr::InterpolatedStr(parts) => {
            let (MirStrPart::Expr(first), rest) = parts.split_first()? else {
                return None;
            };
            if !matches!(&first.node, MirExpr::Local(l) if l.node.slot.0 == slot)
                || aver_type_str_of(first).trim() != "String"
            {
                return None;
            }
            Some(
                rest.iter()
                    .map(|p| match p {
                        MirStrPart::Literal(s) => Piece::Text(s),
                        MirStrPart::Expr(e) => Piece::Embed(e),
                    })
                    .collect(),
            )
        }
        _ => None,
    }
}

fn held(slot: u32) -> Option<(Held, u32, u32, u32)> {
    CURRENT.with(|c| {
        let c = c.borrow();
        let c = c.as_ref()?;
        let h = c.held.iter().find(|h| h.slot == slot)?;
        Some((*h, c.piece, c.array, c.need))
    })
}

fn held_at(fn_id: FnId, position: usize) -> Option<(Held, u32, u32, u32)> {
    CURRENT.with(|c| {
        let c = c.borrow();
        let c = c.as_ref().filter(|c| c.fn_id == fn_id)?;
        let h = c.held.iter().find(|h| h.position == position)?;
        Some((*h, c.piece, c.array, c.need))
    })
}

fn string_idx(ctx: &EmitCtx<'_>) -> Result<u32, WasmGcError> {
    ctx.registry
        .string_array_type_idx
        .ok_or_else(|| WasmGcError::Validation("a held String param needs the String type".into()))
}

/// Read the param in `slot` when it is held: the parameter while the buffer
/// is null, otherwise the buffer's bytes as a string. `None`, with nothing
/// emitted, when it is not held.
pub(super) fn emit_read(
    func: &mut Function,
    slot: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let Some((h, _, array, _)) = held(slot) else {
        return Ok(None);
    };
    let s = string_idx(ctx)?;
    let string = BlockType::Result(ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(s),
    }));
    func.instruction(&I::LocalGet(h.buf));
    func.instruction(&I::RefIsNull);
    func.instruction(&I::If(string));
    func.instruction(&I::LocalGet(slots.local(slot)));
    func.instruction(&I::Else);
    // Not full: swap in a full copy, which the next append replaces.
    func.instruction(&I::LocalGet(h.len));
    func.instruction(&I::LocalGet(h.buf));
    func.instruction(&I::ArrayLen);
    func.instruction(&I::I32Ne);
    func.instruction(&I::If(BlockType::Empty));
    func.instruction(&I::LocalGet(h.len));
    func.instruction(&I::ArrayNewDefault(s));
    func.instruction(&I::LocalTee(array));
    func.instruction(&I::I32Const(0));
    func.instruction(&I::LocalGet(h.buf));
    func.instruction(&I::I32Const(0));
    func.instruction(&I::LocalGet(h.len));
    func.instruction(&I::ArrayCopy {
        array_type_index_dst: s,
        array_type_index_src: s,
    });
    func.instruction(&I::LocalGet(array));
    func.instruction(&I::LocalSet(h.buf));
    func.instruction(&I::End);
    func.instruction(&I::LocalGet(h.buf));
    func.instruction(&I::End);
    Ok(Some(()))
}

/// Emit argument `position` of a tail call to `target`. `None`, with nothing
/// emitted, when that is not a param the function being emitted holds;
/// `Some(None)` when a piece is not covered.
pub(super) fn emit_tail_arg(
    func: &mut Function,
    target: FnId,
    position: usize,
    arg: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<()>>, WasmGcError> {
    let Some((h, piece, array, need)) = held_at(target, position) else {
        return Ok(None);
    };
    let s = string_idx(ctx)?;
    let param = slots.local(h.slot);
    let Some(list) = pieces(&arg.node, h.slot) else {
        // A new value: the parameter holds it and the buffer is dropped.
        if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
            return Ok(Some(None));
        }
        func.instruction(&I::RefNull(HeapType::Concrete(s)));
        func.instruction(&I::LocalSet(h.buf));
        return Ok(Some(Some(())));
    };
    if !list.is_empty() {
        // The param's value becomes the buffer, with no room left.
        func.instruction(&I::LocalGet(h.buf));
        func.instruction(&I::RefIsNull);
        func.instruction(&I::If(BlockType::Empty));
        func.instruction(&I::LocalGet(param));
        func.instruction(&I::LocalTee(h.buf));
        func.instruction(&I::ArrayLen);
        func.instruction(&I::LocalSet(h.len));
        func.instruction(&I::End);
    }
    for p in list {
        match p {
            Piece::Text(text) => {
                let bytes = text.as_bytes();
                if bytes.is_empty() {
                    continue;
                }
                let seg = ctx.registry.string_literal_segment(bytes).ok_or_else(|| {
                    WasmGcError::Validation(format!(
                        "String literal `{text:?}` was not registered in the data segment table"
                    ))
                })?;
                emit_room(func, h, array, need, s, |f| {
                    f.instruction(&I::I32Const(bytes.len() as i32));
                });
                func.instruction(&I::LocalGet(h.buf));
                func.instruction(&I::LocalGet(h.len));
                func.instruction(&I::I32Const(0));
                func.instruction(&I::I32Const(bytes.len() as i32));
                func.instruction(&I::ArrayInitData {
                    array_type_index: s,
                    array_data_index: seg,
                });
            }
            Piece::Value(e) | Piece::Embed(e) => {
                let emitted = match p {
                    Piece::Embed(_) => emit_mir_primitive_to_string(func, e, slots, ctx)?,
                    _ => emit_mir_expr(func, e, slots, ctx)?.map(|_| ()),
                };
                if emitted.is_none() {
                    return Ok(Some(None));
                }
                func.instruction(&I::LocalSet(piece));
                emit_room(func, h, array, need, s, |f| {
                    f.instruction(&I::LocalGet(piece));
                    f.instruction(&I::ArrayLen);
                });
                func.instruction(&I::LocalGet(h.buf));
                func.instruction(&I::LocalGet(h.len));
                func.instruction(&I::LocalGet(piece));
                func.instruction(&I::I32Const(0));
                func.instruction(&I::LocalGet(piece));
                func.instruction(&I::ArrayLen);
                func.instruction(&I::ArrayCopy {
                    array_type_index_dst: s,
                    array_type_index_src: s,
                });
            }
        }
        // The appended bytes are part of the value now.
        func.instruction(&I::LocalGet(need));
        func.instruction(&I::LocalSet(h.len));
    }
    // The parameter keeps its old value; the buffer holds the new one.
    func.instruction(&I::LocalGet(param));
    Ok(Some(Some(())))
}

/// Make room in the buffer for `extra` more bytes, leaving the length it
/// will have in `need`: when it does not fit, move the bytes to an array
/// half as large again as `need`.
fn emit_room(
    func: &mut Function,
    h: Held,
    array: u32,
    need: u32,
    s: u32,
    extra: impl Fn(&mut Function),
) {
    func.instruction(&I::LocalGet(h.len));
    extra(func);
    func.instruction(&I::I32Add);
    func.instruction(&I::LocalTee(need));
    func.instruction(&I::LocalGet(h.buf));
    func.instruction(&I::ArrayLen);
    func.instruction(&I::I32GtU);
    func.instruction(&I::If(BlockType::Empty));
    func.instruction(&I::LocalGet(need));
    func.instruction(&I::LocalGet(need));
    func.instruction(&I::I32Const(1));
    func.instruction(&I::I32ShrU);
    func.instruction(&I::I32Add);
    func.instruction(&I::I32Const(16));
    func.instruction(&I::I32Add);
    func.instruction(&I::ArrayNewDefault(s));
    func.instruction(&I::LocalTee(array));
    func.instruction(&I::I32Const(0));
    func.instruction(&I::LocalGet(h.buf));
    func.instruction(&I::I32Const(0));
    func.instruction(&I::LocalGet(h.len));
    func.instruction(&I::ArrayCopy {
        array_type_index_dst: s,
        array_type_index_src: s,
    });
    func.instruction(&I::LocalGet(array));
    func.instruction(&I::LocalSet(h.buf));
    func.instruction(&I::End);
}
