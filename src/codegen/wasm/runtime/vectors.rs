/// Vector (flat array) runtime functions for the WASM backend.
///
/// Implements vector operations: `vec_from_list`, `vec_get`, `vec_len`,
/// `vec_set`, `vec_new`, `vec_to_list`. Vectors are stored as flat arrays in
/// linear memory with an 8-byte header containing kind and length.
use wasm_encoder::{Function, Instruction, ValType};

use super::super::value::*;
use super::RuntimeFuncIndices;

/// $vec_from_list(list: i32) -> i32
/// Converts a linked list to a flat vector.
pub(super) fn emit_vec_from_list(rt: &RuntimeFuncIndices) -> Function {
    // params: list=0. locals: len=1, ptr=2, vec=3, i=4, cur=5
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: len
        (1, ValType::I32), // 2: ptr (traversal)
        (1, ValType::I32), // 3: vec
        (1, ValType::I32), // 4: i
        (1, ValType::I32), // 5: cur
    ]);
    // First pass: count length
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Alloc: 8 + len * 8
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(3));
    // Header
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Const(
        (OBJ_VECTOR << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    // Second pass: copy elements
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4)); // i = 0
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(5)); // cur = list
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // vec[i] = head
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field)); // head as i64
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    // i++, cur = tail
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    f
}

/// $vec_get(vec: i32, idx: i64) -> i32  (returns Option: wrapper or NONE)
pub(super) fn emit_vec_get(rt: &RuntimeFuncIndices) -> Function {
    // params: vec=0, idx=1(i64). locals: len=2, i=3
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: len
        (1, ValType::I32), // 3: i (i32 index)
    ]);
    // len = header & 0xFFFFFFFF
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(2));
    // i = i32(idx)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3));
    // Bounds check: i < 0 || i >= len → None
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::Else);
    // In bounds: return Some(vec[i])
    f.instruction(&Instruction::I32Const(WRAP_SOME as i32));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.wrap)); // wrap(SOME, value_i64) → i32
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f
}

/// $vec_len(vec: i32) -> i64
pub(super) fn emit_vec_len() -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::End);
    f
}

/// $vec_set(vec: i32, idx: i64, val: i64) -> i32 (returns Option<Vector>)
/// Creates a NEW vector with the element at idx replaced.
pub(super) fn emit_vec_set(rt: &RuntimeFuncIndices) -> Function {
    // params: vec=0, idx=1(i64), val=2(i64). locals: len=3, new_vec=4, i=5, bytes=6
    let mut f = Function::new(vec![
        (1, ValType::I32), // 3: len
        (1, ValType::I32), // 4: new_vec
        (1, ValType::I32), // 5: i (i32 idx)
        (1, ValType::I32), // 6: bytes
    ]);
    // len
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(5));
    // Bounds check
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::Else);
    // Copy entire vector
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(6)); // bytes = 8 + len*8
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(4));
    // memcpy
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    // Update element at idx
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    // Return Some(new_vec)
    f.instruction(&Instruction::I32Const(WRAP_SOME as i32));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::Call(rt.wrap));
    f.instruction(&Instruction::End); // else
    f.instruction(&Instruction::End);
    f
}

/// $vec_new(size: i64, fill: i64) -> i32
pub(super) fn emit_vec_new(rt: &RuntimeFuncIndices) -> Function {
    // params: size=0(i64), fill=1(i64). locals: len=2, vec=3, i=4
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: len
        (1, ValType::I32), // 3: vec
        (1, ValType::I32), // 4: i
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(2));
    // alloc
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(3));
    // header
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Const(
        (OBJ_VECTOR << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    // Fill
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    f
}

/// $vec_to_list(vec: i32) -> i32
/// Converts a flat vector back into a linked list, preserving order.
pub(super) fn emit_vec_to_list(rt: &RuntimeFuncIndices) -> Function {
    // params: vec=0. locals: len=1, idx=2, acc=3
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: len
        (1, ValType::I32), // 2: idx
        (1, ValType::I32), // 3: acc
    ]);

    // len = header & 0xFFFFFFFF
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(1));

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3)); // acc = []
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalSet(2)); // idx = len

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));

    // idx -= 1
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(2));

    // acc = cons(vec[idx], acc)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    f
}
