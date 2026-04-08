/// IO and print runtime functions for the WASM backend.
///
/// Handles stdout writing (`fd_write_buf`), integer/float formatting
/// (`int_to_str`, `float_to_str`), and typed value printing
/// (`print_i64`, `print_f64`, `print_string`, `print_bool`, `print_heap`,
/// `print_value`).
use wasm_encoder::{Function, Instruction, ValType};

use super::{IO_IOVEC, IO_NWRITTEN, RuntimeFuncIndices};

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
