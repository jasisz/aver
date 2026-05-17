//! Slot-id round-trip helpers — `format_id` / `parse_id`.
//!
//! `Tcp.Connection.id` is a fresh `"tcp-N"` Aver String built by
//! `__rt_tcp_format_id` on every successful `Tcp.connect`; the other
//! helpers (`close` / `writeLine` / `readLine`) reverse it with
//! `__rt_tcp_parse_id` to find the matching pool slot.

use wasm_encoder::{Function, Instruction, ValType};

/// Phase 4.2.2d helper — `__rt_tcp_format_id(slot_idx: i32) ->
/// ref string`. Writes the decimal representation of `slot_idx`
/// after a fixed `"tcp-"` prefix into LM[0..len], then calls
/// `from_lm(len)` to materialise the bytes as a fresh Aver
/// `(array i8)`.
///
/// `slot_idx` is constrained to `0..256` (pool capacity), so the
/// decimal portion is always 1, 2, or 3 bytes; total string length
/// is therefore one of 5 / 6 / 7 bytes. A simple if/elif/else
/// branch ladder is cheaper than a digit-reverse loop at this
/// range.
///
/// Signature: `(i32) -> (ref null $string)`. The string slot type
/// idx is threaded in via the indices bundle on the caller side.
pub(in crate::codegen::wasm_gc) fn emit_tcp_format_id(
    string_type_idx: u32,
    from_lm_fn: u32,
) -> Function {
    use wasm_encoder::{BlockType, MemArg};
    let _ = string_type_idx;
    // Locals beyond param 0 = slot_idx.
    let mut f = Function::new::<Vec<(u32, ValType)>>(Vec::new());

    let mem1 = |offset: u32| MemArg {
        offset: u64::from(offset),
        align: 0,
        memory_index: 0,
    };

    // Step 1 — emit "tcp-" at LM[0..4]. Single i32.store would be
    // cleaner but byte-stores keep the layout self-documenting.
    let prefix: [u8; 4] = *b"tcp-";
    for (i, byte) in prefix.iter().enumerate() {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(i32::from(*byte)));
        f.instruction(&Instruction::I32Store8(mem1(i as u32)));
    }

    // Step 2 — branch on slot_idx range. Each branch leaves the
    // string length (5 / 6 / 7) on the stack at the end of the
    // if-else chain.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));

    // 1-digit: slot_idx ∈ [0, 10).
    f.instruction(&Instruction::I32Const(0)); // address LM+4
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(mem1(4)));
    f.instruction(&Instruction::I32Const(5));

    f.instruction(&Instruction::Else);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(100));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));

    // 2-digit: slot_idx ∈ [10, 100).
    // tens digit
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32DivU);
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(mem1(4)));
    // ones digit
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32RemU);
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(mem1(5)));
    f.instruction(&Instruction::I32Const(6));

    f.instruction(&Instruction::Else);

    // 3-digit: slot_idx ∈ [100, 256).
    // hundreds digit
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(100));
    f.instruction(&Instruction::I32DivU);
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(mem1(4)));
    // tens digit ((slot_idx / 10) % 10)
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32DivU);
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32RemU);
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(mem1(5)));
    // ones digit
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32RemU);
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(mem1(6)));
    f.instruction(&Instruction::I32Const(7));

    f.instruction(&Instruction::End); // 2-digit else
    f.instruction(&Instruction::End); // 1-digit else

    // Stack: len. Step 3 — convert LM[0..len] to a fresh Aver
    // String via the shared `__rt_string_from_lm(len) -> string`
    // bridge helper. Result is on the stack for the function
    // return.
    f.instruction(&Instruction::Call(from_lm_fn));
    f.instruction(&Instruction::End);
    f
}

/// Phase 4.3 helper — `__rt_tcp_parse_id(id: ref string) -> i32`.
/// Reverse of `__rt_tcp_format_id`. Strips the leading 4 ASCII
/// bytes (`"tcp-"`) and reads the remaining `(array i8)` content
/// as a base-10 integer.
///
/// Trust contract: `id` came out of `Tcp.connect` on this same
/// build, so the `"tcp-"` prefix is structurally guaranteed. The
/// helper never validates — callers that hand-craft a `Tcp.Connection`
/// today aren't a supported shape (record is `exposes` but not
/// `exposes opaque`, follow-up could tighten this).
pub(in crate::codegen::wasm_gc) fn emit_tcp_parse_id(string_type_idx: u32) -> Function {
    // Locals beyond param 0 = id (ref string):
    //   1 = acc (i32) — running decimal accumulator
    //   2 = i   (i32) — byte cursor (starts at 4, the post-"tcp-" offset)
    //   3 = len (i32) — total byte length of the id string
    let mut f = Function::new(vec![(3u32, ValType::I32)]);
    let l_acc: u32 = 1;
    let l_i: u32 = 2;
    let l_len: u32 = 3;

    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_acc));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::LocalSet(l_i));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(l_len));

    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // Exit condition: i >= len.
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));

    // acc = acc * 10 + (id[i] - '0')
    f.instruction(&Instruction::LocalGet(l_acc));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::ArrayGetU(string_type_idx));
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_acc));

    // i += 1
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_i));

    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    f.instruction(&Instruction::LocalGet(l_acc));
    f.instruction(&Instruction::End);
    f
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_id_emit_compiles() {
        let _f = emit_tcp_parse_id(1);
    }
}
