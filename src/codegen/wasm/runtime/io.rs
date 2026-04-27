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

