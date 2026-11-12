//! The byte buffers of octet groups (`ir::mir::optimize::octets`).
//!
//! A list of an octet group is `[]` (null), a buffer, or cells the rewrite
//! left as cells over either. A buffer is a subtype of the `List<Int>` cell
//! whose length field counts all its octets; it keeps them in push order in
//! segments, each an array of the `Bytes` array type with the `Bytes` views'
//! cell saying how much of it the values sharing it use, the place its
//! first octet has in the whole, and the buffer below it. Pushing onto a
//! buffer whose top segment ends where that use does writes the octet after
//! it and moves the use on. Any other push starts a new segment on top: twice
//! as long as the one below when that one was full, so a run of pushes copies
//! nothing, and short when the push branches off a value another push already
//! went on from, so a buffer kept elsewhere never sees octets pushed after it.
//! `__oct_bytes` is a `Bytes` view of the octets in push order (the reversed
//! list): the one segment's array as it is, or the segments copied into one.
//!
//! Only a group's own reads reach a buffer (pushes, `List.len`, and
//! `Bytes.fromList(List.reverse(..))`), so no other list code meets one; a
//! group's cells are turned into a buffer by the first push or read of them.
//! Plain builds only: a `--certify` build keeps its lists as the
//! certificate's wall proves them, and so does a build whose lists do not
//! keep their length or whose `Bytes` are not views.

use wasm_encoder::{
    AbstractHeapType, BlockType, FieldType, Function, HeapType, Instruction, RefType, StorageType,
    ValType,
};

use super::WasmGcError;
use super::types::{PackedSequenceType, TypeRegistry};

/// A segment holds at most this many octets; a longer run starts another.
const MAX_SEGMENT: i32 = 1 << 20;
/// A segment started by a push that branches off.
const FIRST_SEGMENT: i32 = 16;

/// The `List<Int>` cell a buffer extends, and the buffer's fields: the
/// cell's, then the top segment, its cell, the buffer below, and the place
/// of the segment's first octet.
pub(super) struct Layout {
    pub(super) list_idx: u32,
    pub(super) buffer_fields: Vec<FieldType>,
}

fn bytes(registry: &TypeRegistry) -> Option<PackedSequenceType> {
    registry.packed_sequence("Bytes").filter(|packed| {
        packed.is_view()
            && packed.layout.element == crate::codegen::proof_lower::PackedIntElement::U8
    })
}

/// Whether this build keeps octet groups as buffers.
pub(super) fn enabled(registry: &TypeRegistry) -> bool {
    super::lists::counted_lists()
        && super::packed_sequences::packed_views()
        && registry.list_type_idx("List<Int>").is_some()
        && bytes(registry).is_some()
}

fn nullable(idx: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(idx),
    })
}

fn field(ty: ValType, mutable: bool) -> FieldType {
    FieldType {
        element_type: StorageType::Val(ty),
        mutable,
    }
}

/// The layout of `packed`'s buffer, when `packed` is the one `Bytes`
/// buffers are made of and buffers are on.
pub(super) fn layout(registry: &TypeRegistry, packed: &PackedSequenceType) -> Option<Layout> {
    if !enabled(registry) || bytes(registry)?.array_idx != packed.array_idx {
        return None;
    }
    let list_idx = registry.list_type_idx("List<Int>")?;
    let elem = super::types::aver_to_wasm("Int", Some(registry)).ok()??;
    Some(Layout {
        list_idx,
        buffer_fields: vec![
            field(elem, false),
            field(nullable(list_idx), false),
            field(ValType::I32, true),
            field(nullable(packed.array_idx), false),
            field(nullable(packed.cell_idx()), false),
            field(nullable(packed.octet_buffer_idx()), false),
            field(ValType::I32, false),
        ],
    })
}

/// Buffer field indices.
const LEN: u32 = 2;
const ARR: u32 = 3;
const CELL: u32 = 4;
const BELOW: u32 = 5;
const BASE: u32 = 6;

struct Parts {
    list_idx: u32,
    elem: ValType,
    buffer: u32,
    array: u32,
    cell: u32,
    view: u32,
}

fn parts(registry: &TypeRegistry) -> Result<Parts, WasmGcError> {
    let missing = || WasmGcError::Validation("octet buffers need List<Int> and Bytes views".into());
    let packed = bytes(registry).ok_or_else(missing)?;
    let layout = layout(registry, &packed).ok_or_else(missing)?;
    let elem = match layout.buffer_fields[0].element_type {
        StorageType::Val(v) => v,
        _ => return Err(missing()),
    };
    Ok(Parts {
        list_idx: layout.list_idx,
        elem,
        buffer: packed.octet_buffer_idx(),
        array: packed.array_idx,
        cell: packed.cell_idx(),
        view: packed.type_idx,
    })
}

/// `__oct_push`'s list and result, and `__oct_bytes`'s argument.
pub(super) fn list_ref(registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    Ok(nullable(parts(registry)?.list_idx))
}

/// `__oct_bytes`'s result.
pub(super) fn bytes_ref(registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    Ok(nullable(parts(registry)?.view))
}

fn get(f: &mut Function, struct_type_index: u32, field_index: u32) {
    f.instruction(&Instruction::StructGet {
        struct_type_index,
        field_index,
    });
}

fn local(f: &mut Function, i: u32) {
    f.instruction(&Instruction::LocalGet(i));
}

fn set(f: &mut Function, i: u32) {
    f.instruction(&Instruction::LocalSet(i));
}

fn i32c(f: &mut Function, k: i32) {
    f.instruction(&Instruction::I32Const(k));
}

/// The locals both helpers work with, after their params.
struct L {
    list: u32,
    buf: u32,
    len: u32,
    arr: u32,
    cell: u32,
    below: u32,
    base: u32,
    cur: u32,
    k: u32,
    i: u32,
    out: u32,
}

fn locals(p: &Parts, list: u32, first: u32) -> (L, Vec<(u32, ValType)>) {
    let l = L {
        list,
        buf: first,
        len: first + 1,
        arr: first + 2,
        cell: first + 3,
        below: first + 4,
        base: first + 5,
        cur: first + 6,
        k: first + 7,
        i: first + 8,
        out: first + 9,
    };
    let decl = vec![
        (1, nullable(p.buffer)),
        (1, ValType::I32),
        (1, nullable(p.array)),
        (1, nullable(p.cell)),
        (1, nullable(p.buffer)),
        (1, ValType::I32),
        (1, nullable(p.list_idx)),
        (2, ValType::I32),
        (1, nullable(p.array)),
    ];
    (l, decl)
}

/// Push a buffer `(len, arr, cell, below, base)` from locals.
fn new_buffer(f: &mut Function, p: &Parts, len: u32, arr: u32, cell: u32, below: u32, base: u32) {
    match p.elem {
        ValType::Ref(r) => f.instruction(&Instruction::RefNull(r.heap_type)),
        ValType::I64 => f.instruction(&Instruction::I64Const(0)),
        _ => f.instruction(&Instruction::I32Const(0)),
    };
    f.instruction(&Instruction::RefNull(HeapType::Concrete(p.list_idx)));
    local(f, len);
    local(f, arr);
    local(f, cell);
    local(f, below);
    local(f, base);
    f.instruction(&Instruction::StructNew(p.buffer));
}

fn is_buffer(f: &mut Function, p: &Parts, i: u32) {
    local(f, i);
    f.instruction(&Instruction::RefTestNonNull(HeapType::Concrete(p.buffer)));
}

/// The non-null list in `l.list` as a buffer in `l.buf`, its fields in the
/// other locals: the buffer itself, or for cells, their octets over the
/// buffer that ends them (if any) as one new segment on top of it.
fn open(f: &mut Function, p: &Parts, l: &L) {
    is_buffer(f, p, l.list);
    f.instruction(&Instruction::If(BlockType::Empty));
    local(f, l.list);
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(p.buffer)));
    set(f, l.buf);
    f.instruction(&Instruction::Else);
    // Count the cells down to the end or a buffer.
    i32c(f, 0);
    set(f, l.k);
    local(f, l.list);
    set(f, l.cur);
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    local(f, l.cur);
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    is_buffer(f, p, l.cur);
    f.instruction(&Instruction::BrIf(1));
    local(f, l.k);
    i32c(f, 1);
    f.instruction(&Instruction::I32Add);
    set(f, l.k);
    local(f, l.cur);
    get(f, p.list_idx, 1);
    set(f, l.cur);
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // The segment sits on the buffer below the cells, if any.
    local(f, l.cur);
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(p.buffer)));
    set(f, l.below);
    i32c(f, 0);
    set(f, l.base);
    local(f, l.below);
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    local(f, l.below);
    get(f, p.buffer, LEN);
    set(f, l.base);
    f.instruction(&Instruction::End);
    local(f, l.k);
    f.instruction(&Instruction::ArrayNewDefault(p.array));
    set(f, l.arr);
    // The first cell's octet is the last pushed.
    local(f, l.k);
    set(f, l.i);
    local(f, l.list);
    set(f, l.cur);
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    local(f, l.i);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    local(f, l.i);
    i32c(f, 1);
    f.instruction(&Instruction::I32Sub);
    set(f, l.i);
    local(f, l.arr);
    local(f, l.i);
    local(f, l.cur);
    get(f, p.list_idx, 0);
    match p.elem {
        // A small `Int`'s word; the group's octets are all small.
        ValType::Ref(RefType {
            heap_type: HeapType::Concrete(aint),
            ..
        }) => {
            get(f, aint, 0);
            f.instruction(&Instruction::I32WrapI64);
        }
        ValType::I64 => {
            f.instruction(&Instruction::I32WrapI64);
        }
        _ => {}
    }
    f.instruction(&Instruction::ArraySet(p.array));
    local(f, l.cur);
    get(f, p.list_idx, 1);
    set(f, l.cur);
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    local(f, l.base);
    local(f, l.k);
    f.instruction(&Instruction::I32Add);
    set(f, l.len);
    local(f, l.k);
    f.instruction(&Instruction::StructNew(p.cell));
    set(f, l.cell);
    new_buffer(f, p, l.len, l.arr, l.cell, l.below, l.base);
    set(f, l.buf);
    f.instruction(&Instruction::End);
    // Its fields.
    for (field, into) in [
        (LEN, l.len),
        (ARR, l.arr),
        (CELL, l.cell),
        (BELOW, l.below),
        (BASE, l.base),
    ] {
        local(f, l.buf);
        get(f, p.buffer, field);
        set(f, into);
    }
}

/// `__oct_push : (i64 octet, List<Int>) -> List<Int>`.
pub(super) fn emit_push(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let p = parts(registry)?;
    // params: 0=octet, 1=list.
    let (l, decl) = locals(&p, 1, 2);
    let mut f = Function::new(decl);
    // `[]`: a first segment.
    local(&mut f, l.list);
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    i32c(&mut f, FIRST_SEGMENT);
    f.instruction(&Instruction::ArrayNewDefault(p.array));
    set(&mut f, l.out);
    i32c(&mut f, 0);
    set(&mut f, l.base);
    i32c(&mut f, 0);
    set(&mut f, l.len);
    f.instruction(&Instruction::Else);
    open(&mut f, &p, &l);
    // `k` = the octets in the top segment.
    local(&mut f, l.len);
    local(&mut f, l.base);
    f.instruction(&Instruction::I32Sub);
    set(&mut f, l.k);
    // It ends where its array's use does: in place while there is room,
    // else a segment twice as long on top.
    local(&mut f, l.cell);
    get(&mut f, p.cell, 0);
    local(&mut f, l.k);
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    local(&mut f, l.k);
    local(&mut f, l.arr);
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(BlockType::Empty));
    local(&mut f, l.arr);
    local(&mut f, l.k);
    local(&mut f, 0);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::ArraySet(p.array));
    local(&mut f, l.cell);
    local(&mut f, l.k);
    i32c(&mut f, 1);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::StructSet {
        struct_type_index: p.cell,
        field_index: 0,
    });
    local(&mut f, l.len);
    i32c(&mut f, 1);
    f.instruction(&Instruction::I32Add);
    set(&mut f, l.len);
    new_buffer(&mut f, &p, l.len, l.arr, l.cell, l.below, l.base);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    local(&mut f, l.k);
    i32c(&mut f, 1);
    f.instruction(&Instruction::I32Shl);
    set(&mut f, l.i);
    local(&mut f, l.i);
    i32c(&mut f, MAX_SEGMENT);
    local(&mut f, l.i);
    i32c(&mut f, MAX_SEGMENT);
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::Select);
    set(&mut f, l.i);
    f.instruction(&Instruction::Else);
    // Another push went on from this value: a short segment of its own.
    i32c(&mut f, FIRST_SEGMENT);
    set(&mut f, l.i);
    f.instruction(&Instruction::End);
    local(&mut f, l.i);
    i32c(&mut f, FIRST_SEGMENT);
    local(&mut f, l.i);
    i32c(&mut f, FIRST_SEGMENT);
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::Select);
    f.instruction(&Instruction::ArrayNewDefault(p.array));
    set(&mut f, l.out);
    local(&mut f, l.buf);
    set(&mut f, l.below);
    local(&mut f, l.len);
    set(&mut f, l.base);
    f.instruction(&Instruction::End);
    // The new segment: this octet first.
    local(&mut f, l.out);
    i32c(&mut f, 0);
    local(&mut f, 0);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::ArraySet(p.array));
    i32c(&mut f, 1);
    f.instruction(&Instruction::StructNew(p.cell));
    set(&mut f, l.cell);
    local(&mut f, l.len);
    i32c(&mut f, 1);
    f.instruction(&Instruction::I32Add);
    set(&mut f, l.len);
    new_buffer(&mut f, &p, l.len, l.out, l.cell, l.below, l.base);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `__oct_bytes : (List<Int>) -> Bytes`: a view of the list's octets in
/// push order, of none for `[]`.
pub(super) fn emit_bytes(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let p = parts(registry)?;
    // params: 0=list.
    let (l, decl) = locals(&p, 0, 1);
    let mut f = Function::new(decl);
    let no_cell = |f: &mut Function| {
        f.instruction(&Instruction::RefNull(HeapType::Abstract {
            shared: false,
            ty: AbstractHeapType::None,
        }));
    };
    local(&mut f, l.list);
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    i32c(&mut f, 0);
    f.instruction(&Instruction::ArrayNewDefault(p.array));
    i32c(&mut f, 0);
    i32c(&mut f, 0);
    no_cell(&mut f);
    f.instruction(&Instruction::StructNew(p.view));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    open(&mut f, &p, &l);
    // One segment: its array as it is.
    local(&mut f, l.below);
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    local(&mut f, l.arr);
    i32c(&mut f, 0);
    local(&mut f, l.len);
    local(&mut f, l.cell);
    f.instruction(&Instruction::StructNew(p.view));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // Several: each copied to its place, from the top down.
    local(&mut f, l.len);
    f.instruction(&Instruction::ArrayNewDefault(p.array));
    set(&mut f, l.out);
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    local(&mut f, l.out);
    local(&mut f, l.base);
    local(&mut f, l.arr);
    i32c(&mut f, 0);
    local(&mut f, l.len);
    local(&mut f, l.base);
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: p.array,
        array_type_index_src: p.array,
    });
    local(&mut f, l.below);
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    local(&mut f, l.below);
    set(&mut f, l.buf);
    for (field, into) in [(LEN, l.len), (ARR, l.arr), (BELOW, l.below), (BASE, l.base)] {
        local(&mut f, l.buf);
        get(&mut f, p.buffer, field);
        set(&mut f, into);
    }
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    local(&mut f, l.out);
    i32c(&mut f, 0);
    local(&mut f, l.out);
    f.instruction(&Instruction::ArrayLen);
    no_cell(&mut f);
    f.instruction(&Instruction::StructNew(p.view));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `__oct_last : (List<Int>, i64 n) -> Bytes`: a view of the last `n`
/// octets pushed (all of them past the length, none below one), in push
/// order: of the top segment's array as it is when they are all in it,
/// copied out of the segments otherwise.
pub(super) fn emit_last(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let p = parts(registry)?;
    // params: 0=list, 1=n.
    let (l, decl) = locals(&p, 0, 2);
    let mut f = Function::new(decl);
    let no_cell = |f: &mut Function| {
        f.instruction(&Instruction::RefNull(HeapType::Abstract {
            shared: false,
            ty: AbstractHeapType::None,
        }));
    };
    let empty = |f: &mut Function| {
        i32c(f, 0);
        f.instruction(&Instruction::ArrayNewDefault(p.array));
        i32c(f, 0);
        i32c(f, 0);
        no_cell(f);
        f.instruction(&Instruction::StructNew(p.view));
    };
    local(&mut f, l.list);
    f.instruction(&Instruction::RefIsNull);
    local(&mut f, 1);
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LeS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(BlockType::Empty));
    empty(&mut f);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    open(&mut f, &p, &l);
    // `k` = how many: n, at most the length.
    local(&mut f, 1);
    local(&mut f, l.len);
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    local(&mut f, 1);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Else);
    local(&mut f, l.len);
    f.instruction(&Instruction::End);
    set(&mut f, l.k);
    // All in the top segment: a view of its array.
    local(&mut f, l.k);
    local(&mut f, l.len);
    local(&mut f, l.base);
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32LeU);
    f.instruction(&Instruction::If(BlockType::Empty));
    local(&mut f, l.arr);
    local(&mut f, l.len);
    local(&mut f, l.base);
    f.instruction(&Instruction::I32Sub);
    local(&mut f, l.k);
    f.instruction(&Instruction::I32Sub);
    local(&mut f, l.k);
    local(&mut f, l.cell);
    f.instruction(&Instruction::StructNew(p.view));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // Otherwise `out` gets `[len - k, len)`, segment by segment from the
    // top; `i` = where the wanted octets start in the whole.
    local(&mut f, l.k);
    f.instruction(&Instruction::ArrayNewDefault(p.array));
    set(&mut f, l.out);
    local(&mut f, l.len);
    local(&mut f, l.k);
    f.instruction(&Instruction::I32Sub);
    set(&mut f, l.i);
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // This segment's part: from max(base, i) to len.
    local(&mut f, l.base);
    local(&mut f, l.i);
    local(&mut f, l.base);
    local(&mut f, l.i);
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::Select);
    set(&mut f, l.k);
    local(&mut f, l.out);
    local(&mut f, l.k);
    local(&mut f, l.i);
    f.instruction(&Instruction::I32Sub);
    local(&mut f, l.arr);
    local(&mut f, l.k);
    local(&mut f, l.base);
    f.instruction(&Instruction::I32Sub);
    local(&mut f, l.len);
    local(&mut f, l.k);
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: p.array,
        array_type_index_src: p.array,
    });
    // Done once this segment starts at or before `i`.
    local(&mut f, l.base);
    local(&mut f, l.i);
    f.instruction(&Instruction::I32LeU);
    f.instruction(&Instruction::BrIf(1));
    local(&mut f, l.below);
    set(&mut f, l.buf);
    for (field, into) in [(LEN, l.len), (ARR, l.arr), (BELOW, l.below), (BASE, l.base)] {
        local(&mut f, l.buf);
        get(&mut f, p.buffer, field);
        set(&mut f, into);
    }
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    local(&mut f, l.out);
    i32c(&mut f, 0);
    local(&mut f, l.out);
    f.instruction(&Instruction::ArrayLen);
    no_cell(&mut f);
    f.instruction(&Instruction::StructNew(p.view));
    f.instruction(&Instruction::End);
    Ok(f)
}
