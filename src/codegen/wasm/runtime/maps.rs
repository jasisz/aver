/// Map (association list) runtime functions for the WASM backend.
///
/// Maps are implemented as association lists of `(key_ptr, value_i64)` tuples.
/// Provides `map_get`, `map_set`, `map_has`, and `map_keys` operations.
use wasm_encoder::{Function, Instruction, ValType};

use super::super::value::*;
use super::RuntimeFuncIndices;

/// $map_get(map: i32, key: i32) -> i32  (returns Option ptr: wrapper or NONE_SENTINEL)
/// Searches association list for key using str_eq. Returns Option.Some(value) or Option.None.
pub(super) fn emit_map_get(rt: &RuntimeFuncIndices) -> Function {
    // params: map=0, key=1. locals: ptr=2, entry=3(i32), entry_key=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: ptr (current cons cell)
        (1, ValType::I32), // 3: entry (tuple ptr)
        (1, ValType::I32), // 4: entry_key
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1)); // empty → not found
    // entry = head of cons cell (stored as i64, wrap to i32 ptr)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field)); // i64
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3)); // entry tuple ptr
    // entry_key = tuple field 0 (i64 → i32)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field)); // i64
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4));
    // Compare keys: str_eq(entry_key, key)
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.str_eq));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // Found! Return Option.Some(value)
    // value = tuple field 1 (i64)
    f.instruction(&Instruction::I32Const(WRAP_SOME as i32));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field)); // value as i64
    f.instruction(&Instruction::Call(rt.wrap)); // wrap_i64(SOME, value) → i32 ptr
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // Next: ptr = tail
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    // Not found → return None sentinel
    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::End);
    f
}

/// $map_set(map: i32, key: i32, value: i64) -> i32
/// Prepends new (key, value) entry and removes older entries with the same key.
pub(super) fn emit_map_set(rt: &RuntimeFuncIndices) -> Function {
    // params: map=0, key=1, value=2(i64)
    // locals: tuple_ptr=3, cur=4, entry_tuple=5, entry_key=6, kept_rev=7, filtered_tail=8, map_cell=9
    let mut f = Function::new(vec![
        (1, ValType::I32), // 3: tuple_ptr
        (1, ValType::I32), // 4: cur
        (1, ValType::I32), // 5: entry_tuple
        (1, ValType::I32), // 6: entry_key
        (1, ValType::I32), // 7: kept_rev
        (1, ValType::I32), // 8: filtered_tail
        (1, ValType::I32), // 9: map_cell
    ]);
    // Alloc tuple: header + 2 fields
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(3));
    // Header
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Const(
        make_header(OBJ_TUPLE, 0, 0, 2) as i64
    ));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    // Field 0: key (i32 → i64)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    // Field 1: value (i64)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    // kept_rev = [] ; cur = map
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(4));
    // Scan old map and keep only entries with different keys.
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.str_eq));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Rebuild filtered tail in original order.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::I64Const(
        make_header(OBJ_MAP_ENTRY, 0, 0, 2) as i64
    ));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Prepend the new entry to the filtered tail.
    f.instruction(&Instruction::I32Const(24));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::I64Const(
        make_header(OBJ_MAP_ENTRY, 0, 0, 2) as i64
    ));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::End);
    f
}

/// $map_has(map: i32, key: i32) -> i32  (bool)
pub(super) fn emit_map_has(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: ptr
        (1, ValType::I32), // 3: entry_key
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // entry key
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::I32WrapI64); // tuple ptr
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::I32WrapI64); // key ptr
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.str_eq));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // next
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    f
}

/// $map_keys(map: i32) -> i32  (list of key ptrs)
pub(super) fn emit_map_keys(rt: &RuntimeFuncIndices) -> Function {
    // params: map=0. locals: ptr=1, acc=2, key=3(i64)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: ptr
        (1, ValType::I32), // 2: acc (reversed list)
        (1, ValType::I64), // 3: key (i64 from tuple)
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(0)); // empty acc
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // key = entry.field0 (tuple.field0)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field)); // tuple as i64
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field)); // key as i64
    f.instruction(&Instruction::LocalSet(3));
    // acc = cons(key, acc)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(2));
    // next
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // reverse
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_reverse));
    f.instruction(&Instruction::End);
    f
}

/// $map_entries(map: i32) -> i32  (plain list of tuple pointers)
pub(super) fn emit_map_entries(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: ptr
        (1, ValType::I32), // 2: acc
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_reverse));
    f.instruction(&Instruction::End);
    f
}
