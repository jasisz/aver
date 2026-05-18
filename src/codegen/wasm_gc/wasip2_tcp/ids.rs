//! Slot-id round-trip helpers — `format_id` / `parse_id`.
//!
//! `Tcp.Connection.id` is a fresh `"tcp-N"` Aver String built by
//! `__rt_tcp_format_id` on every successful `Tcp.connect`; the other
//! helpers (`close` / `writeLine` / `readLine`) reverse it with
//! `__rt_tcp_parse_id` to find the matching pool slot.

use wasm_encoder::{Function, Instruction, ValType};

/// `__rt_tcp_format_id(id_value: i32) -> ref string` — materialise
/// `"tcp-{id_value}"` as a fresh Aver String. `id_value` is the
/// program-lifetime monotonic counter (full u32 range, not modulo
/// 256 — see Phase 4.7 fix #2), so the decimal portion can run 1
/// to 10 digits.
///
/// Implementation strategy:
///   1. Write `"tcp-"` at LM[0..4].
///   2. Emit decimal digits LSD-first into LM[4..4+n_digits] from
///      the end, walking a temp cursor backwards. `id_value == 0`
///      is a special case (single '0' digit).
///   3. Compact the digits to LM[4..4+n_digits] and call
///      `__rt_string_from_lm(4 + n_digits)`.
///
/// To avoid the backwards-write + memcpy pair, we use a fixed 10-
/// byte scratch at LM[16..26] (well past the prefix), pull digits
/// LSD-first into it from the right, then byte-copy the trailing
/// `n_digits` bytes into LM[4..4+n_digits]. 10 byte writes max per
/// call — negligible vs. a real divide loop.
pub(in crate::codegen::wasm_gc) fn emit_tcp_format_id(
    string_type_idx: u32,
    from_lm_fn: u32,
) -> Function {
    use wasm_encoder::{BlockType, MemArg};
    let _ = string_type_idx;
    // Locals beyond param 0 = id_value (i32):
    //   1 = n       (i32) — running value being divided down
    //   2 = cursor  (i32) — current write offset into scratch
    //   3 = i       (i32) — copy loop index
    //   4 = len     (i32) — final string length (4 + n_digits)
    let mut f = Function::new(vec![(4u32, ValType::I32)]);
    let l_n: u32 = 1;
    let l_cursor: u32 = 2;
    let l_i: u32 = 3;
    let l_len: u32 = 4;

    // Scratch base for the digit stack — LM[16..26]. Out of the way
    // of the "tcp-" prefix (LM[0..4]) and any subsequent string
    // payload the format helper might brush against in pathological
    // alignment cases.
    const SCRATCH_BASE: u32 = 16;
    const SCRATCH_END: u32 = SCRATCH_BASE + 10; // exclusive

    let mem1 = |offset: u32| MemArg {
        offset: u64::from(offset),
        align: 0,
        memory_index: 0,
    };
    let mem1_zero = MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    };

    // Step 1 — emit "tcp-" at LM[0..4].
    let prefix: [u8; 4] = *b"tcp-";
    for (i, byte) in prefix.iter().enumerate() {
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::I32Const(i32::from(*byte)));
        f.instruction(&Instruction::I32Store8(mem1(i as u32)));
    }

    // Step 2 — load id_value into the working local, position the
    // cursor at the post-scratch end (we walk it backwards).
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(l_n));
    f.instruction(&Instruction::I32Const(SCRATCH_END as i32));
    f.instruction(&Instruction::LocalSet(l_cursor));

    // Step 3 — at-least-once digit loop. Even for id_value == 0 we
    // want one '0' digit; an explicit `do-while` shape via block+
    // loop+br_if-at-end gives us that without a leading branch.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // cursor -= 1
    f.instruction(&Instruction::LocalGet(l_cursor));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(l_cursor));
    // LM[cursor] = '0' + (n % 10)
    f.instruction(&Instruction::LocalGet(l_cursor));
    f.instruction(&Instruction::LocalGet(l_n));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32RemU);
    f.instruction(&Instruction::I32Const(0x30));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Store8(mem1_zero));
    // n /= 10
    f.instruction(&Instruction::LocalGet(l_n));
    f.instruction(&Instruction::I32Const(10));
    f.instruction(&Instruction::I32DivU);
    f.instruction(&Instruction::LocalSet(l_n));
    // continue while n != 0
    f.instruction(&Instruction::LocalGet(l_n));
    f.instruction(&Instruction::BrIf(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    // Step 4 — compact LM[cursor..SCRATCH_END] to LM[4..4+n_digits].
    // n_digits = SCRATCH_END - cursor. len = 4 + n_digits.
    f.instruction(&Instruction::I32Const(SCRATCH_END as i32));
    f.instruction(&Instruction::LocalGet(l_cursor));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_len));

    // for (i = 0; i < n_digits; i++)
    //     LM[4 + i] = LM[cursor + i]
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(l_i));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::LocalGet(l_len));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));

    // dst = 4 + i
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Add);
    // src byte
    f.instruction(&Instruction::LocalGet(l_cursor));
    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Load8U(mem1_zero));
    f.instruction(&Instruction::I32Store8(mem1_zero));

    f.instruction(&Instruction::LocalGet(l_i));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(l_i));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // copy loop
    f.instruction(&Instruction::End); // copy block

    // Step 5 — materialise LM[0..len] via from_lm and return.
    f.instruction(&Instruction::LocalGet(l_len));
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
