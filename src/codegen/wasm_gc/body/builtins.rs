//! `Args.get()` inline lowering — the one dotted-builtin helper still
//! shared with the MIR body emitter (`from_mir/`). The rest of the
//! `ResolvedExpr`-walking dotted-builtin dispatch (`emit_dotted_builtin`
//! and the per-family inline emitters) was retired with the HIR walker;
//! the wasip2 effect emitters it used to dispatch now live behind the
//! MIR `from_mir::emit_mir_wasip2_effect` seam.

use wasm_encoder::{Function, Instruction};

use super::super::WasmGcError;
use super::{EmitCtx, SlotTable};

/// Inline lowering of `Args.get()` (no args, returns `List<String>`).
/// Host imports are `args_len(): i64` and `args_get(i: i64): String`;
/// no `args_get_all`. Walks `i = len-1 .. 0` cons-building the list so
/// the result lands in source-arg order without a final reverse pass.
/// Uses the four scratch slots reserved by `slots::SlotTable` (i, len,
/// acc, s).
pub(super) fn emit_args_get_inline(
    func: &mut Function,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let [i_slot, len_slot, acc_slot, s_slot] =
        slots.args_get_scratch.ok_or(WasmGcError::Validation(
            "Args.get() inline expansion requires 4 reserved scratch slots — \
             slots::SlotTable should have allocated them via fn_needs_args_get_scratch"
                .into(),
        ))?;
    let args_len_idx =
        ctx.fn_map
            .effects
            .get("Args.len")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Args.get() inline needs the Args.len effect import — \
             discovery walker should register it when Args.get() is reachable"
                    .into(),
            ))?;
    let args_get_idx =
        ctx.fn_map
            .effects
            .get("Args.get")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Args.get() inline needs the Args.get effect import (the i64 → String form)".into(),
            ))?;
    let list_string_idx =
        ctx.registry
            .list_type_idx("List<String>")
            .ok_or(WasmGcError::Validation(
                "Args.get() inline needs the List<String> registry slot".into(),
            ))?;

    // len = args_len()
    super::emit::emit_caller_fn_idx(func, ctx)?;
    func.instruction(&Instruction::Call(args_len_idx));
    func.instruction(&Instruction::LocalSet(len_slot));
    // i = len - 1
    func.instruction(&Instruction::LocalGet(len_slot));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::LocalSet(i_slot));
    // acc = ref.null List<String>
    func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        list_string_idx,
    )));
    func.instruction(&Instruction::LocalSet(acc_slot));

    func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if i < 0: br out (depth 1 — the surrounding block)
    func.instruction(&Instruction::LocalGet(i_slot));
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64LtS);
    func.instruction(&Instruction::BrIf(1));
    // s = args_get(i)
    func.instruction(&Instruction::LocalGet(i_slot));
    super::emit::emit_caller_fn_idx(func, ctx)?;
    func.instruction(&Instruction::Call(args_get_idx));
    func.instruction(&Instruction::LocalSet(s_slot));
    // acc = struct.new List<String> { head: s, tail: acc }
    func.instruction(&Instruction::LocalGet(s_slot));
    func.instruction(&Instruction::LocalGet(acc_slot));
    func.instruction(&Instruction::StructNew(list_string_idx));
    func.instruction(&Instruction::LocalSet(acc_slot));
    // i = i - 1
    func.instruction(&Instruction::LocalGet(i_slot));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::LocalSet(i_slot));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // end loop
    func.instruction(&Instruction::End); // end block

    // result on stack: acc
    func.instruction(&Instruction::LocalGet(acc_slot));
    Ok(())
}
