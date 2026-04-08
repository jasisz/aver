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

/// $str_len(str: i32) -> i64
/// Counts UTF-8 code points in a string object.
pub(super) fn emit_str_len() -> Function {
    // params: str=0. locals: byte_len=1, byte_pos=2, char_len=3, lead=4, width=5
    let mut f = Function::new(vec![
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);

    emit_load_string_byte_len(&mut f, 0, 1);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));

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
    emit_set_utf8_char_width(&mut f, 4, 5);

    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::End);
    f
}

/// $str_byte_len(str: i32) -> i64
/// Returns the raw UTF-8 byte length stored in the string header.
pub(super) fn emit_str_byte_len() -> Function {
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

/// $str_char_at(str: i32, idx: i64) -> i32  (Option<String>)
/// Returns the UTF-8 code point at `idx` as a single-character string.
pub fn emit_str_char_at(rt: &RuntimeFuncIndices) -> Function {
    // params: str=0, idx=1(i64). locals: byte_len=2, target=3, byte_pos=4,
    //         char_pos=5, lead=6, width=7, ptr=8
    let mut f = Function::new(vec![(7, ValType::I32)]);

    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    emit_load_string_byte_len(&mut f, 0, 2);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(5));

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(6));
    emit_set_utf8_char_width(&mut f, 6, 7);

    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_alloc_string(&mut f, rt, 7, 8);
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::I32Const(WRAP_SOME as i32));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::Call(rt.wrap_i32));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::End);
    f
}

/// $char_from_code(code: i64) -> i32  (Option<String>)
pub fn emit_char_from_code(rt: &RuntimeFuncIndices) -> Function {
    // params: code64=0. locals: code=1, len=2, ptr=3
    let mut f = Function::new(vec![(3, ValType::I32)]);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(0x10FFFF));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(0xD800));
    f.instruction(&Instruction::I64GeS);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(0xDFFF));
    f.instruction(&Instruction::I64LeS);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::Else);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(1));

    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0x80));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0x800));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0x10000));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(3));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalSet(2));

    emit_alloc_string(&mut f, rt, 2, 3);

    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(6));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Const(0xC0));
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(0x80));
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 9,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(3));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Const(0xE0));
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(6));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(0x80));
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 9,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(0x80));
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 10,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(18));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Const(0xF0));
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(0x80));
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 9,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(6));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(0x80));
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 10,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(0x80));
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 11,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::I32Const(WRAP_SOME as i32));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::Call(rt.wrap_i32));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f
}

/// $char_to_code(str: i32) -> i64
pub fn emit_char_to_code() -> Function {
    // params: str=0. locals: byte_len=1, lead=2, b1=3, b2=4, b3=5, code=6
    let mut f = Function::new(vec![(6, ValType::I32)]);

    emit_load_string_byte_len(&mut f, 0, 1);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(2));

    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0x80));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0xE0));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 9,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0x1F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(6));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0xF0));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(3));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 9,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 10,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0x0F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(6));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 9,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 10,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 11,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0x07));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(18));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(12));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Const(6));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(0x3F));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::End);
    f
}

/// $byte_to_hex(n: i64) -> i32  (Result<String, String>)
pub fn emit_byte_to_hex(rt: &RuntimeFuncIndices) -> Function {
    // params: n=0(i64). locals: byte=1, ptr=2, len=3, nibble=4
    let mut f = Function::new(vec![(4, ValType::I32)]);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Const(255));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_return_err_empty_string(&mut f, rt, 2);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::LocalSet(3));
    emit_alloc_string(&mut f, rt, 3, 2);

    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Const(0xF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(2));
    emit_push_hex_char(&mut f, 4);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));

    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0xF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(2));
    emit_push_hex_char(&mut f, 4);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 9,
        align: 0,
        memory_index: 0,
    }));

    f.instruction(&Instruction::I32Const(WRAP_OK as i32));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Call(rt.wrap_i32));
    f.instruction(&Instruction::End);
    f
}

/// $byte_from_hex(str: i32) -> i32  (Result<Int, String>)
pub fn emit_byte_from_hex(rt: &RuntimeFuncIndices) -> Function {
    // params: str=0. locals: len=1, hi=2, lo=3, ch=4, value=5
    let mut f = Function::new(vec![(5, ValType::I32)]);

    emit_load_string_byte_len(&mut f, 0, 1);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_return_err_empty_string(&mut f, rt, 5);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(4));
    emit_set_hex_nibble(&mut f, rt, 4, 2, 5);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 9,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(4));
    emit_set_hex_nibble(&mut f, rt, 4, 3, 5);

    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalSet(5));

    f.instruction(&Instruction::I32Const(WRAP_OK as i32));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::Call(rt.wrap));
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
    // params: str=0, start=1, end=2.
    // locals: start_idx=3, end_idx=4, byte_len=5, byte_pos=6, char_pos=7,
    //         byte_start=8, byte_end=9, lead=10, width=11, ptr=12
    let mut f = Function::new(vec![(10, ValType::I32)]);

    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalSet(3));

    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalSet(4));

    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(11));
    emit_alloc_string(&mut f, rt, 11, 12);
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    emit_load_string_byte_len(&mut f, 0, 5);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalSet(9));

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::Br(2));
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 8,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(10));
    emit_set_utf8_char_width(&mut f, 10, 11);

    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(11));
    emit_alloc_string(&mut f, rt, 11, 12);
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::MemoryCopy {
        src_mem: 0,
        dst_mem: 0,
    });
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::End);
    f
}

/// $str_chars(str: i32) -> i32  (list of single-char strings)
pub fn emit_str_chars(rt: &RuntimeFuncIndices) -> Function {
    // params: str=0. locals: byte_len=1, byte_pos=2, rev_list=3, lead=4, width=5, ptr=6
    let mut f = Function::new(vec![(6, ValType::I32)]);

    emit_load_string_byte_len(&mut f, 0, 1);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));

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
    emit_set_utf8_char_width(&mut f, 4, 5);

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
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::Call(rt.list_cons));
    f.instruction(&Instruction::LocalSet(3));

    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::Call(rt.list_reverse));
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

/// $float_from_str(str: i32) -> i32  (Result<Float, String> wrapper ptr)
///
/// Supports optional leading '-', a fractional part, and optional `e`/`E`
/// exponent with optional sign. On parse failure it returns
/// Result.Err(original_input_string).
pub fn emit_float_from_str(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![
        (11, ValType::I32), // 1..11: len, idx, negative, seen_dot, saw_digit, ch, digit,
        //           exp_state, exp_negative, saw_exp_digit, exp_value
        (2, ValType::F64), // 12..13: value, frac_div
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
    f.instruction(&Instruction::LocalSet(1));
    // zero-init state
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(10));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(11));
    f.instruction(&Instruction::F64Const(0.0));
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::F64Const(1.0));
    f.instruction(&Instruction::LocalSet(13));
    // Leading '-'
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
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Main scan loop
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
    f.instruction(&Instruction::LocalSet(6));

    // Exponent sign right after e/E.
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'+' as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(1));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'-' as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(1));
    f.instruction(&Instruction::End);

    // Inside exponent digits.
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_float_parse_error(&mut f, rt);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'9' as i32));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_float_parse_error(&mut f, rt);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(10));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(11));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(1));
    f.instruction(&Instruction::End);

    // Decimal point.
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'.' as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_float_parse_error(&mut f, rt);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(1));
    f.instruction(&Instruction::End);

    // Exponent marker.
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'e' as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'E' as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_float_parse_error(&mut f, rt);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(1));
    f.instruction(&Instruction::End);

    // Mantissa digit.
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_float_parse_error(&mut f, rt);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'9' as i32));
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_float_parse_error(&mut f, rt);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(13));
    f.instruction(&Instruction::F64Const(10.0));
    f.instruction(&Instruction::F64Mul);
    f.instruction(&Instruction::LocalSet(13));
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::F64ConvertI32U);
    f.instruction(&Instruction::LocalGet(13));
    f.instruction(&Instruction::F64Div);
    f.instruction(&Instruction::F64Add);
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::F64Const(10.0));
    f.instruction(&Instruction::F64Mul);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::F64ConvertI32U);
    f.instruction(&Instruction::F64Add);
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Reject empty / lone '-'
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_float_parse_error(&mut f, rt);
    f.instruction(&Instruction::End);
    // Reject dangling exponent marker/sign with no digits.
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_float_parse_error(&mut f, rt);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_float_parse_error(&mut f, rt);
    f.instruction(&Instruction::End);
    // Apply exponent.
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::F64Const(10.0));
    f.instruction(&Instruction::F64Div);
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::F64Const(10.0));
    f.instruction(&Instruction::F64Mul);
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(11));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Apply sign
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::F64Const(0.0));
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::F64Sub);
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::End);
    // Result.Ok(value)
    f.instruction(&Instruction::I32Const(WRAP_OK as i32));
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::Call(rt.wrap_f64));
    f.instruction(&Instruction::End);
    f
}

fn emit_load_string_byte_len(f: &mut Function, str_local: u32, len_local: u32) {
    f.instruction(&Instruction::LocalGet(str_local));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(len_local));
}

fn emit_set_utf8_char_width(f: &mut Function, lead_local: u32, width_local: u32) {
    f.instruction(&Instruction::LocalGet(lead_local));
    f.instruction(&Instruction::I32Const(0x80));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(width_local));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(lead_local));
    f.instruction(&Instruction::I32Const(0xE0));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(2));
    f.instruction(&Instruction::LocalSet(width_local));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(lead_local));
    f.instruction(&Instruction::I32Const(0xF0));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(3));
    f.instruction(&Instruction::LocalSet(width_local));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::LocalSet(width_local));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
}

fn emit_push_hex_char(f: &mut Function, nibble_local: u32) {
    f.instruction(&Instruction::LocalGet(nibble_local));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::LocalGet(nibble_local));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(nibble_local));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Const(b'a' as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::End);
}

fn emit_set_hex_nibble(
    f: &mut Function,
    rt: &RuntimeFuncIndices,
    ch_local: u32,
    value_local: u32,
    err_ptr_local: u32,
) {
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'9' as i32));
    f.instruction(&Instruction::I32LeU);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(value_local));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'a' as i32));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'f' as i32));
    f.instruction(&Instruction::I32LeU);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'a' as i32));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(value_local));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'A' as i32));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'F' as i32));
    f.instruction(&Instruction::I32LeU);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(ch_local));
    f.instruction(&Instruction::I32Const(b'A' as i32));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(value_local));
    f.instruction(&Instruction::Else);
    emit_return_err_empty_string(f, rt, err_ptr_local);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
}

fn emit_return_err_empty_string(f: &mut Function, rt: &RuntimeFuncIndices, ptr_local: u32) {
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::Call(rt.alloc));
    f.instruction(&Instruction::LocalSet(ptr_local));
    f.instruction(&Instruction::LocalGet(ptr_local));
    f.instruction(&Instruction::I64Const(
        (OBJ_STRING << HDR_KIND_SHIFT) as i64,
    ));
    f.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(WRAP_ERR as i32));
    f.instruction(&Instruction::LocalGet(ptr_local));
    f.instruction(&Instruction::Call(rt.wrap_i32));
    f.instruction(&Instruction::Return);
}

fn emit_float_parse_error(f: &mut Function, rt: &RuntimeFuncIndices) {
    f.instruction(&Instruction::I32Const(WRAP_ERR as i32));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(rt.wrap_i32));
    f.instruction(&Instruction::Return);
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
