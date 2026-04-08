/// IO and print runtime functions for the WASM backend.
///
/// Handles stdout writing (`fd_write_buf`), integer/float formatting
/// (`int_to_str`, `float_to_str`), and typed value printing
/// (`print_i64`, `print_f64`, `print_string`, `print_bool`, `print_heap`,
/// `print_value`).
use wasm_encoder::{Function, Instruction, ValType};

use super::super::value::*;
use super::{
    IO_FLOAT_BUF, IO_INT_BUF, IO_IOVEC, IO_NWRITTEN, IO_SCRATCH_SIZE, NEWLINE_ADDR,
    RuntimeFuncIndices,
};

/// Addresses of runtime format strings in data section.
#[derive(Default)]
pub struct RtStrings {
    pub true_: (u32, u32),
    pub false_: (u32, u32),
    pub none: (u32, u32),
    pub empty_list: (u32, u32),
    pub result_ok: (u32, u32),
    pub result_err: (u32, u32),
    pub option_some: (u32, u32),
    pub close_paren: (u32, u32),
    pub open_bracket: (u32, u32),
    pub comma_space: (u32, u32),
}

/// $write_stdout(ptr: i32, len: i32) -> ()
/// In aver/* mode: direct call to aver/console_print(ptr, len).
/// In WASI mode: iovec setup + call fd_write(stdout, iovec, 1, nwritten).
pub(super) fn emit_fd_write_buf(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![]);
    match rt.adapter {
        super::super::WasmAdapter::Aver => {
            // Direct capability call — no iovec, no fd numbers
            f.instruction(&Instruction::LocalGet(0)); // ptr
            f.instruction(&Instruction::LocalGet(1)); // len
            f.instruction(&Instruction::Call(rt.fd_write_import));
            f.instruction(&Instruction::End);
        }
        super::super::WasmAdapter::Wasi => {
            // WASI: setup iovec buffer and call fd_write
            f.instruction(&Instruction::I32Const(IO_IOVEC as i32));
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            f.instruction(&Instruction::I32Const(IO_IOVEC as i32));
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::I32Store(wasm_encoder::MemArg {
                offset: 4,
                align: 2,
                memory_index: 0,
            }));
            f.instruction(&Instruction::I32Const(1)); // fd=stdout
            f.instruction(&Instruction::I32Const(IO_IOVEC as i32));
            f.instruction(&Instruction::I32Const(1));
            f.instruction(&Instruction::I32Const(IO_NWRITTEN as i32));
            f.instruction(&Instruction::Call(rt.fd_write_import));
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::End);
        }
    }
    f
}

/// $int_to_str(val: i64, buf: i32) -> i32 — returns (pos<<16)|len
pub(super) fn emit_int_to_str() -> Function {
    // params: val=0, buf=1. locals: n=2(i64), is_neg=3(i32), pos=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I64),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);
    // n = val (plain i64, no tag extraction needed)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    // n==0?
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Else);
    // is_neg
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::End);
    // pos=21
    f.instruction(&Instruction::I32Const(21));
    f.instruction(&Instruction::LocalSet(4));
    // digit loop
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    // neg sign
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(b'-' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    // return (pos<<16)|(21-pos)
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32Const(21));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::End); // else
    f.instruction(&Instruction::End); // func
    f
}

/// $float_to_str(val: f64, buf: i32) -> i32 — returns (pos<<16)|len
///
/// Fixed-precision: prints integer part, then '.' + fractional digits (up to 15),
/// stripping trailing zeros. Whole numbers print without decimal point.
pub(super) fn emit_float_to_str() -> Function {
    // Shortest-roundtrip approach:
    // For N = 1..15, check if trunc(abs_val * 10^N) / 10^N == abs_val.
    // Use the smallest N that gives equality. Then format with N frac digits.
    //
    // params: val=0(f64), buf=1(i32)
    // locals: is_neg=2(i32), abs_val=3(f64), int_part=4(i64), pos=5(i32),
    //         start_pos=6(i32), pow=7(f64), n=8(i32), scaled=9(i64),
    //         frac_int=10(i64), frac_pos=11(i32), frac_digits=12(i32)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 2: is_neg
        (1, ValType::F64), // 3: abs_val
        (1, ValType::I64), // 4: int_part
        (1, ValType::I32), // 5: pos
        (1, ValType::I32), // 6: start_pos
        (1, ValType::F64), // 7: pow
        (1, ValType::I32), // 8: n (frac digit count)
        (1, ValType::I64), // 9: scaled
        (1, ValType::I64), // 10: frac_int
        (1, ValType::I32), // 11: frac_pos
        (1, ValType::I32), // 12: frac_digits
    ]);

    // is_neg = val < 0.0
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Const(0.0));
    f.instruction(&Instruction::F64Lt);
    f.instruction(&Instruction::LocalSet(2));

    // abs_val = |val|
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Abs);
    f.instruction(&Instruction::LocalSet(3));

    // int_part = i64.trunc_f64_s(abs_val)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::F64Floor);
    f.instruction(&Instruction::I64TruncF64S);
    f.instruction(&Instruction::LocalSet(4));

    // --- Write integer part right-to-left at buf[0..21] ---
    f.instruction(&Instruction::I32Const(21));
    f.instruction(&Instruction::LocalSet(5));

    // int_part == 0?
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Else);
    // digit loop
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Eqz);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    f.instruction(&Instruction::End); // if/else

    // neg sign
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(b'-' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);

    // start_pos = pos (beginning of integer digits in buf)
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalSet(6));

    // --- Check if value is exactly an integer ---
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::F64Floor);
    f.instruction(&Instruction::F64Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I32,
    )));
    // Integer: return (start_pos<<16)|(21-start_pos)
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::I32Const(21));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Or);

    f.instruction(&Instruction::Else);

    // --- Find shortest N: trunc(abs_val * 10^N) / 10^N == abs_val ---
    // pow = 1.0, n = 0
    f.instruction(&Instruction::F64Const(1.0));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(8));

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // n++
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(8));
    // pow *= 10.0
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::F64Const(10.0));
    f.instruction(&Instruction::F64Mul);
    f.instruction(&Instruction::LocalSet(7));
    // scaled = i64.trunc_f64_s(abs_val * pow)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::F64Mul);
    // Round to nearest (add 0.5 then floor — but this introduces error)
    // Actually, just use trunc (floor for positive values)
    f.instruction(&Instruction::F64Floor);
    f.instruction(&Instruction::I64TruncF64S);
    f.instruction(&Instruction::LocalSet(9));
    // check = f64.convert_i64_s(scaled) / pow
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::F64ConvertI64S);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::F64Div);
    // if check == abs_val → found shortest
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::F64Eq);
    f.instruction(&Instruction::BrIf(1)); // break out of loop
    // if n >= 15 → give up, use 15
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(15));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::Br(0)); // continue
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    // Now: n = number of frac digits, scaled = integer representation
    // frac_int = scaled - int_part_restored * pow_as_int
    // But simpler: frac_int = scaled % pow_i64
    // where pow_i64 = i64(pow)
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64TruncF64S);
    f.instruction(&Instruction::I64RemS);
    // Make sure it's positive
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64TruncF64S);
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I64TruncF64S);
    f.instruction(&Instruction::I64RemS);
    f.instruction(&Instruction::LocalSet(10)); // frac_int (positive)

    // Write '.' at position 21
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(b'.' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 21,
        align: 0,
        memory_index: 0,
    }));

    // Write frac_int as exactly n digits at buf[22..22+n], right-to-left
    // frac_pos = 22 + n - 1 (rightmost digit position)
    f.instruction(&Instruction::I32Const(22));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(11));

    // frac_digits = n (counter)
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalSet(12));

    // Write digits right-to-left
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    // store digit
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64RemU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    // frac_int /= 10
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I64Const(10));
    f.instruction(&Instruction::I64DivU);
    f.instruction(&Instruction::LocalSet(10));
    // frac_pos--, frac_digits--
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(11));
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    // Strip trailing zeros from frac digits
    // frac_end = 22 + n
    f.instruction(&Instruction::I32Const(22));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(11)); // reuse frac_pos as frac_end

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if frac_end <= 22 → break
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Const(22));
    f.instruction(&Instruction::I32LeS);
    f.instruction(&Instruction::BrIf(1));
    // check last byte
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(b'0' as i32));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::BrIf(1)); // not zero, stop
    // frac_end--
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(11));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    // return (start_pos<<16) | (frac_end - start_pos)
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Or);

    f.instruction(&Instruction::End); // else (has frac)
    f.instruction(&Instruction::End); // func
    f
}

/// $print_i64(val: i64) -> ()
pub(super) fn emit_print_i64(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: tmp
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(IO_INT_BUF as i32));
    f.instruction(&Instruction::Call(rt.int_to_str));
    f.instruction(&Instruction::LocalSet(1));
    // ptr = buf + (tmp >> 16), len = tmp & 0xFFFF
    f.instruction(&Instruction::I32Const(IO_INT_BUF as i32));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0xFFFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    f
}

/// $print_f64(val: f64) -> ()
pub(super) fn emit_print_f64(rt: &RuntimeFuncIndices) -> Function {
    let mut f = Function::new(vec![(1, ValType::I32)]); // local: tmp
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(IO_FLOAT_BUF as i32));
    f.instruction(&Instruction::Call(rt.float_to_str));
    f.instruction(&Instruction::LocalSet(1));
    // ptr = buf + (tmp >> 16), len = tmp & 0xFFFF
    f.instruction(&Instruction::I32Const(IO_FLOAT_BUF as i32));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(16));
    f.instruction(&Instruction::I32ShrU);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0xFFFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    f
}

/// $print_string(ptr: i32) -> ()
pub(super) fn emit_print_string(rt: &RuntimeFuncIndices, _strs: &RtStrings) -> Function {
    let mut f = Function::new(vec![]);
    // Read string length from header (lower 32 bits)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Add);
    // String bytes start at ptr+8
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I64Const(0xFFFFFFFF));
    f.instruction(&Instruction::I64And);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    f
}

/// $print_bool(val: i32) -> ()
pub(super) fn emit_print_bool(rt: &RuntimeFuncIndices, strs: &RtStrings) -> Function {
    let mut f = Function::new(vec![]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // true
    f.instruction(&Instruction::I32Const(strs.true_.0 as i32));
    f.instruction(&Instruction::I32Const(strs.true_.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::Else);
    // false
    f.instruction(&Instruction::I32Const(strs.false_.0 as i32));
    f.instruction(&Instruction::I32Const(strs.false_.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f
}

/// $print_heap(ptr: i32) -> ()
/// Polymorphic printer: reads object header to determine kind and format.
pub(super) fn emit_print_heap(rt: &RuntimeFuncIndices, strs: &RtStrings) -> Function {
    // params: ptr=0. locals: header=1(i64), kind=2(i32), tag=3(i32), inner_ptr=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I64),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);

    macro_rules! print_static {
        ($f:expr, $rt:expr, $s:expr) => {
            $f.instruction(&Instruction::I32Const($s.0 as i32));
            $f.instruction(&Instruction::I32Const($s.1 as i32));
            $f.instruction(&Instruction::Call($rt.fd_write_buf));
        };
    }

    // Check for None sentinel (-1)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(NONE_SENTINEL));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    print_static!(f, rt, strs.none);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Check for empty list (0)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    print_static!(f, rt, strs.empty_list);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Read header
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::LocalSet(1));

    // kind = (header >> 56) & 0xFF
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(56));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(2));

    // === OBJ_STRING (0) ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(rt.print_string));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // === OBJ_WRAPPER (3) — i64 inner ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // tag = (header >> 48) & 0xFF
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));
    // Print prefix
    emit_wrapper_prefix(&mut f, rt, strs, 3);
    // Print inner (i64)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_i64));
    print_static!(f, rt, strs.close_paren);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // === OBJ_WRAPPER_F64 (7) ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));
    emit_wrapper_prefix(&mut f, rt, strs, 3);
    // Print inner (f64)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_f64));
    print_static!(f, rt, strs.close_paren);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // === OBJ_WRAPPER_I32 (8) — string/bool inner ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_I32 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(HDR_TAG_SHIFT as i64));
    f.instruction(&Instruction::I64ShrU);
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::I32Const(0xFF));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(3));
    emit_wrapper_prefix(&mut f, rt, strs, 3);
    // Print inner: load as i32 (string pointer), wrap with quotes
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4)); // inner_ptr
    // Print opening quote
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(b'"' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    // Print string content
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::Call(rt.print_string));
    // Print closing quote
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(b'"' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    print_static!(f, rt, strs.close_paren);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // === OBJ_LIST_CONS (4) — generic elements (i64, may be heap ptrs) ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    print_static!(f, rt, strs.open_bracket);
    // Print first element via print_value
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_value));
    // Traverse tail
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4)); // tail ptr
    // Loop through tail
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz); // tail == 0 (empty)
    f.instruction(&Instruction::BrIf(1));
    print_static!(f, rt, strs.comma_space);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_value));
    // next tail
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    // Close bracket: store ']' at scratch, fd_write_buf it
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(b']' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // === OBJ_LIST_CONS_F64 (9) — f64 elements ===
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    print_static!(f, rt, strs.open_bracket);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_f64));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::BrIf(1));
    print_static!(f, rt, strs.comma_space);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::F64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_f64));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 16,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(b']' as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::End); // func
    f
}

/// $print_value(val: i64) -> ()
/// Generic value printer. Checks if val looks like a heap pointer and dispatches.
pub(super) fn emit_print_value(rt: &RuntimeFuncIndices, strs: &RtStrings) -> Function {
    // params: val=0(i64). locals: ptr=1(i32), kind=2(i32), count=3(i32), i=4(i32)
    let mut f = Function::new(vec![
        (1, ValType::I32), // 1: ptr
        (1, ValType::I32), // 2: kind
        (1, ValType::I32), // 3: count
        (1, ValType::I32), // 4: i (loop counter)
    ]);

    // ptr = i32.wrap(val)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalSet(1));

    // Check: is val in heap range? ptr >= IO_SCRATCH_SIZE and ptr > 0
    // If val sign-extended from i32 equals val, it's a valid i32 pointer
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64ExtendI32S);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I64Eq);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(IO_SCRATCH_SIZE as i32));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

    // It's a heap pointer — dispatch on obj_kind
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.obj_kind));
    f.instruction(&Instruction::LocalSet(2));

    // OBJ_STRING (0)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // Print quoted string: "..."
    emit_scratch_byte(&mut f, rt, b'"');
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.print_string));
    emit_scratch_byte(&mut f, rt, b'"');
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // OBJ_TUPLE (5) — print (a, b, ...)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_TUPLE as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_scratch_byte(&mut f, rt, b'(');
    // count = header & 0xFFFFFFFF
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
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    // Print ", " separator (except first)
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(strs.comma_space.0 as i32));
    f.instruction(&Instruction::I32Const(strs.comma_space.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    // Print element: obj_field(ptr, i)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(8));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
        offset: 8,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::Call(rt.print_value)); // recursive!
    // i++
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    emit_scratch_byte(&mut f, rt, b')');
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End); // OBJ_TUPLE

    // OBJ_WRAPPER / OBJ_WRAPPER_F64 / OBJ_WRAPPER_I32 — delegate to print_heap
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_WRAPPER_I32 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.print_heap));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // OBJ_LIST_CONS / OBJ_LIST_CONS_F64 — delegate to print_heap
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(OBJ_LIST_CONS_F64 as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.print_heap));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // OBJ_VARIANT — print "Variant(...)"
    // For now just delegate to print_heap
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(rt.print_heap));
    f.instruction(&Instruction::Return);

    f.instruction(&Instruction::End); // if (heap pointer)

    // Not a heap pointer — print as integer
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(rt.print_i64));
    f.instruction(&Instruction::End);
    f
}

/// Helper: write single byte to scratch area and fd_write_buf
pub(super) fn emit_scratch_byte(f: &mut Function, rt: &RuntimeFuncIndices, byte: u8) {
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(byte as i32));
    f.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    f.instruction(&Instruction::I32Const(NEWLINE_ADDR as i32));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
}

/// Emit wrapper prefix based on tag local: "Result.Ok(" / "Result.Err(" / "Option.Some("
pub(super) fn emit_wrapper_prefix(
    f: &mut Function,
    rt: &RuntimeFuncIndices,
    strs: &RtStrings,
    tag_local: u32,
) {
    // tag == 0 → Ok
    f.instruction(&Instruction::LocalGet(tag_local));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(strs.result_ok.0 as i32));
    f.instruction(&Instruction::I32Const(strs.result_ok.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::Else);
    // tag == 1 → Err
    f.instruction(&Instruction::LocalGet(tag_local));
    f.instruction(&Instruction::I32Const(WRAP_ERR as i32));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(strs.result_err.0 as i32));
    f.instruction(&Instruction::I32Const(strs.result_err.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::Else);
    // tag == 2 → Some
    f.instruction(&Instruction::I32Const(strs.option_some.0 as i32));
    f.instruction(&Instruction::I32Const(strs.option_some.1 as i32));
    f.instruction(&Instruction::Call(rt.fd_write_buf));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
}
