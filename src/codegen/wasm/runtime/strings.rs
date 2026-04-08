/// String runtime functions for the WASM backend.
///
/// Handles string equality comparison (`str_eq`), concatenation (`str_concat`),
/// and conversion from numeric types to string objects (`i64_to_str_obj`,
/// `f64_to_str_obj`).
use wasm_encoder::{Function, Instruction, ValType};

use super::super::value::*;
use super::{IO_FLOAT_BUF, IO_INT_BUF, RuntimeFuncIndices};

/// $str_eq(a: i32, b: i32) -> i32
/// Compares two string objects by content. Returns 1 if equal, 0 if not.
pub(super) fn emit_str_eq() -> Function {
    // params: a=0(i32), b=1(i32)
    // locals: len_a=2(i32), len_b=3(i32), i=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: len_a
        (1, ValType::I32), // 3: len_b
        (1, ValType::I32), // 4: i (loop counter)
    ]);

    // Same pointer → equal
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Else);

    // len_a = header_a & 0xFFFFFFFF
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

    // len_b = header_b & 0xFFFFFFFF
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3));

    // len_a != len_b → not equal
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Else);

    // Byte-by-byte comparison using result local
    // Reuse local 4 as loop counter, local 2 doubles as result (set to 1 = equal)
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4)); // i = 0

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if i >= len_a → done (equal)
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    // Compare byte at a+8+i vs b+8+i
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // Not equal → return 0 immediately
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // i++
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0)); // continue loop
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    // All bytes matched → equal
    f.instruction(&Instruction::I32Const(1));

    f.instruction(&Instruction::End); // else (len check)
    f.instruction(&Instruction::End); // else (ptr check)
    f.instruction(&Instruction::End); // func
    f
}

/// $str_concat(a: i32, b: i32) -> i32
/// Concatenates two string objects, returns new string object pointer.
pub(super) fn emit_str_concat(rt: &RuntimeFuncIndices) -> Function {
    // params: a=0, b=1. locals: len_a=2, len_b=3, ptr=4, total=5
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: len_a
        (1, ValType::I32), // 3: len_b
        (1, ValType::I32), // 4: ptr
        (1, ValType::I32), // 5: total_len
    ]);
    // len_a = header_a & 0xFFFFFFFF
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
    // len_b
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3));
    // total = len_a + len_b
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(5));
    // alloc(8 + ((total+7)&~7))
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(4));
    // header
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const(
        (OBJ_STRING << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    // memory.copy a bytes: dst=ptr+8, src=a+8, len=len_a
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    // memory.copy b bytes: dst=ptr+8+len_a, src=b+8, len=len_b
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    // return ptr
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    f
}

/// $i64_to_str_obj(val: i64) -> i32
/// Converts an integer to a heap-allocated string object.
pub(super) fn emit_i64_to_str_obj(rt: &RuntimeFuncIndices) -> Function {
    // params: val=0(i64). locals: packed=1(i32), pos=2(i32), len=3(i32), ptr=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: packed
        (1, ValType::I32), // 2: pos
        (1, ValType::I32), // 3: len
        (1, ValType::I32), // 4: ptr
    ]);
    // Use int_to_str to format into scratch buffer at IO_INT_BUF
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(IO_INT_BUF as i32));
    f.instruction(&Instruction::Call(rt.int_to_str));
    f.instruction(&Instruction::LocalSet(1)); // packed = (pos<<16)|len
    // pos = packed >> 16
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::LocalSet(2));
    // len = packed & 0xFFFF
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0xFFFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));
    // alloc string object: 8 + ((len+7)&~7)
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(4));
    // header
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const(
        (OBJ_STRING << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    // copy bytes: dst=ptr+8, src=IO_INT_BUF+pos, len=len
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(IO_INT_BUF as i32));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    f
}

/// $f64_to_str_obj(val: f64) -> i32
pub(super) fn emit_f64_to_str_obj(rt: &RuntimeFuncIndices) -> Function {
    // Same pattern as i64_to_str_obj but using float_to_str
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: packed
        (1, ValType::I32), // 2: pos
        (1, ValType::I32), // 3: len
        (1, ValType::I32), // 4: ptr
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(IO_FLOAT_BUF as i32));
    f.instruction(&Instruction::Call(rt.float_to_str));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0xFFFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const(
        (OBJ_STRING << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(IO_FLOAT_BUF as i32));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    f
}

/// $str_trim(str: i32) -> i32
pub fn emit_str_trim(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);
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
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(4));
    emit_is_whitespace(&mut f, 4);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32LeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(4));
    emit_is_whitespace(&mut f, 4);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Else);
    emit_alloc_string(&mut f, rt, 5, 6);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f
}

/// $str_slice(str: i32, start: i32, end: i32) -> i32
pub fn emit_str_slice(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32), (1, ValType::I32)]);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LeS);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const(
        (OBJ_STRING << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::Else);
    emit_alloc_string(&mut f, rt, 3, 4);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f
}

/// $str_chars(str: i32) -> i32  (list of single-char strings)
pub fn emit_str_chars(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);
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
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const(
        (OBJ_STRING << HDR_KIND_SHIFT | 1) as i64,
    ));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f
}

/// $str_join(list: i32, sep: i32) -> i32
pub fn emit_str_join(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(5));
    // Pass 1: total length
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Alloc
    emit_alloc_string(&mut f, rt, 2, 8);
    // Pass 2: copy
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Call(rt.obj_field));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.obj_field_i32));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::End);
    f
}

/// $int_from_str(str: i32) -> i32  (Result<Int, String> wrapper ptr)
pub fn emit_int_from_str(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![
        (1, ValType::I32),
        (1, ValType::I64),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);
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
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    // Check '-'
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32GtS);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(b'-' as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Digits
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64Mul);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Negate
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::End);
    // Result.Ok(result)
    f.instruction(&Instruction::I32Const(WRAP_OK as i32));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.wrap));
    f.instruction(&Instruction::End);
    f
}

fn emit_is_whitespace(f: &mut Function, ch_local: u32) {
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b' ' as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'\t' as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'\n' as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'\r' as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
}

fn emit_alloc_string(f: &mut Function, rt: &RuntimeFuncIndices, len_local: u32, ptr_local: u32) {
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::LocalGet(len_local));
    f.instruction(&Instruction::I32Const(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(-8i32));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(ptr_local));
    f.instruction(&Instruction::LocalGet(ptr_local));
    f.instruction(&Instruction::I64Const(
        (OBJ_STRING << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::LocalGet(len_local));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::I64Or);
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
}
