/// Allocation, boundary compaction, wrap/unwrap, and object inspection.
///
/// WASM uses a single bump heap plus explicit boundary compaction:
/// - each function / TCO iteration keeps a mark
/// - on return / tailcall we keep only explicit survivors
/// - everything else dies by resetting the heap suffix
use wasm_encoder::{Function, Instruction, ValType};

use super::super::value::*;
use super::RuntimeFuncIndices;

const HEAP_PTR_GLOBAL: u32 = 0;
const COLLECT_MARK_GLOBAL: u32 = 1;
const COLLECT_FROM_GLOBAL: u32 = 2;
const COLLECT_DST_GLOBAL: u32 = 3;

/// $alloc(size: i32) -> i32
pub(super) fn emit_alloc() -> Function {
    let mut f = Function::new(vec![(5, ValType::I32)]); // locals: ptr, aligned, end, mem_bytes, grow_pages
    f.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::MemorySize(0));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Const(65535));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::MemoryGrow(0));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::GlobalSet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    f
}

/// $truncate(mark: i32) -> ()
pub(super) fn emit_truncate_to_mark() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::GlobalSet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::End);
    f
}

/// $collect_begin(mark: i32) -> ()
pub(super) fn emit_collect_begin() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::GlobalSet(COLLECT_MARK_GLOBAL));
    f.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::GlobalSet(COLLECT_FROM_GLOBAL));
    f.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::GlobalSet(COLLECT_DST_GLOBAL));
    f.instruction(&Instruction::End);
    f
}

/// $rebase_i32(root: i32) -> i32
///
/// After boundary evacuation, pointers inside the temporary compacted block
/// still point into the scratch suffix `[collect_from, collect_dst)`.
/// Rebase them back into the compacted frame window starting at `collect_mark`.
pub(super) fn emit_rebase_i32() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LeS);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::GlobalGet(COLLECT_FROM_GLOBAL));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::GlobalGet(COLLECT_DST_GLOBAL));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::GlobalGet(COLLECT_MARK_GLOBAL));
    f.instruction(&Instruction::GlobalGet(COLLECT_FROM_GLOBAL));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f
}

/// $collect_end() -> ()
pub(super) fn emit_collect_end(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(10, ValType::I32), (1, ValType::I64)]);
    // locals:
    // 0 scan, 1 end, 2 size, 3 kind, 4 meta, 5 count,
    // 6 i, 7 child, 8 field_addr, 9 len, 10 header

    f.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::GlobalSet(COLLECT_DST_GLOBAL));
    f.instruction(&Instruction::GlobalGet(COLLECT_FROM_GLOBAL));
    f.instruction(&Instruction::LocalSet(0));
    f.instruction(&Instruction::GlobalGet(COLLECT_DST_GLOBAL));
    f.instruction(&Instruction::LocalSet(1));

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(10));

    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I64Const(HDR_KIND_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));

    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I64Const(HDR_META_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(HDR_META_MASK as i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(4));

    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(5));

    // size
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_STRING as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_I32 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_MAP_ENTRY as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalSet(2));

    // Wrapper / Wrapper_i32 with pointer payload.
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_I32 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.rebase_i32));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // List<i64> cons: head may be ptr, tail always ptr.
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.rebase_i32));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.rebase_i32));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);

    // List<f64> cons: tail only.
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.rebase_i32));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);

    // Fixed-field records / tuples / variants use meta as pointer bitmask.
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_RECORD as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_TUPLE as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_VARIANT as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.rebase_i32));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // Vectors use bit0 = elements_are_ptr.
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_VECTOR as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.rebase_i32));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // Map entry always holds tuple ptr + tail ptr.
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(OBJ_MAP_ENTRY as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    for offset in [8u64, 16u64] {
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
            offset,
            align: 3,
            memory_index: 0,
        }));
        f.instruction(&Instruction::I32WrapI64);
        f.instruction(&Instruction::Call(rt.rebase_i32));
        f.instruction(&Instruction::LocalSet(7));
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::LocalGet(7));
        f.instruction(&Instruction::I64ExtendI32S);
        f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
            offset,
            align: 3,
            memory_index: 0,
        }));
    }
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(0));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::GlobalGet(COLLECT_DST_GLOBAL));
    f.instruction(&Instruction::GlobalGet(COLLECT_FROM_GLOBAL));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::GlobalGet(COLLECT_MARK_GLOBAL));
    f.instruction(&Instruction::GlobalGet(COLLECT_FROM_GLOBAL));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::GlobalGet(COLLECT_MARK_GLOBAL));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::GlobalSet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::End);
    f
}

/// $retain_i32(root: i32) -> i32
///
/// Keeps a heap pointer alive across the active collection window.
/// Non-positive sentinels and pointers below the current mark are returned
/// unchanged. Objects in the suffix are copied into a temporary compacted
/// target above the current heap, mirroring `aver-memory`'s "collect roots
/// into compacted survivors, then truncate + append" shape.
pub(super) fn emit_retain_i32(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(9, ValType::I32), (1, ValType::I64)]);
    // locals:
    // 1 old_ptr, 2 new_ptr, 3 size, 4 kind, 5 meta, 6 count,
    // 7 i, 8 child, 9 field_addr, 10 header

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LeS);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Else);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::GlobalGet(COLLECT_MARK_GLOBAL));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Else);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::GlobalGet(COLLECT_FROM_GLOBAL));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::GlobalGet(HEAP_PTR_GLOBAL));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Else);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(10));

    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I64Const(HDR_KIND_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(4));

    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_FORWARD as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Else);

    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I64Const(HDR_META_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(HDR_META_MASK as i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(5));

    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(6));

    // size
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_STRING as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_I32 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_MAP_ENTRY as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalSet(3));

    // new_ptr = alloc(size) in the temporary compacted target above the
    // current heap, so source and destination never overlap.
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(2));

    // copy object down
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });

    // store forwarding header in old object
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(
        (OBJ_FORWARD << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));

    // Wrapper / Wrapper_i32 with pointer payload.
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_I32 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.retain_i32));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // List<i64> cons: head may be ptr, tail always ptr.
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.retain_i32));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.retain_i32));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);

    // List<f64> cons: tail only.
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.retain_i32));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);

    // Fixed-field records / tuples / variants use meta as pointer bitmask.
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_RECORD as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_TUPLE as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_VARIANT as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.retain_i32));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // Vectors use bit0 = elements_are_ptr.
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_VECTOR as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.retain_i32));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // Map entry always holds tuple ptr + tail ptr.
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(OBJ_MAP_ENTRY as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    for offset in [8u64, 16u64] {
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
            offset,
            align: 3,
            memory_index: 0,
        }));
        f.instruction(&Instruction::I32WrapI64);
        f.instruction(&Instruction::Call(rt.retain_i32));
        f.instruction(&Instruction::LocalSet(8));
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::LocalGet(8));
        f.instruction(&Instruction::I64ExtendI32S);
        f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
            offset,
            align: 3,
            memory_index: 0,
        }));
    }
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f
}

/// $wrap(tag: i32, inner: i64, ptr_flag: i32) -> i32
pub(super) fn emit_wrap(rt: &RuntimeFuncIndices, obj_kind: u64) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]);
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Const((obj_kind << HDR_KIND_SHIFT) as i64));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Const(HDR_META_SHIFT as i64));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    f
}

/// $wrap_f64(tag: i32, inner: f64) -> i32
pub(super) fn emit_wrap_f64(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]);
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(
        (OBJ_WRAPPER_F64 << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::F64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f
}

/// $wrap_i32(tag: i32, inner: i32, ptr_flag: i32) -> i32
pub(super) fn emit_wrap_i32(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]);
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Const(
        (OBJ_WRAPPER_I32 << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Const(HDR_META_SHIFT as i64));
    f.instruction(&Instruction::I64Shl);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    f
}

/// $unwrap(ptr: i32) -> i64
pub(super) fn emit_unwrap_i64() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f
}

/// $unwrap_f64(ptr: i32) -> f64
pub(super) fn emit_unwrap_f64() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f
}

/// $unwrap_i32(ptr: i32) -> i32
pub(super) fn emit_unwrap_i32() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::End);
    f
}

/// $obj_kind(ptr: i32) -> i32
pub(super) fn emit_obj_kind() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(HDR_KIND_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::End);
    f
}

/// $obj_tag(ptr: i32) -> i32
pub(super) fn emit_obj_tag() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::End);
    f
}

/// $obj_meta(ptr: i32) -> i32
pub(super) fn emit_obj_meta() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(HDR_META_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(HDR_META_MASK as i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::End);
    f
}

/// $obj_field(ptr: i32, idx: i32) -> i64
pub(super) fn emit_obj_field_i64() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f
}

/// $obj_field_f64(ptr: i32, idx: i32) -> f64
pub(super) fn emit_obj_field_f64() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f
}

/// $obj_field_i32(ptr: i32, idx: i32) -> i32
pub(super) fn emit_obj_field_i32() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::End);
    f
}
