//! `Run.fail` and `Run.failure`, lowered inline on both wasm targets.
//!
//! The reason a run failed is one `(ref null $string)` global of the module:
//! null until the first `Run.fail`, that call's message after it. A later
//! `Run.fail` leaves it alone, so the first failure of a turn is the one the
//! generated loop reads back, in the order the turn served its slots.
//!
//! On wasm-gc each operation also calls a recorder import, the way
//! `Work.cancel` does: `aver.run_fail` puts the call in the recording, and
//! `aver.run_failure` hands the reason the module holds through the host,
//! which answers it unchanged when live and answers the recorded one in a
//! replay. A JavaScript host answers both with one line each. A component
//! records nothing, so on wasip2 there is no import at all.

use wasm_encoder::{BlockType, Function, HeapType, Instruction, RefType, ValType};

use super::WasmGcError;
use super::body::{EmitCtx, SlotTable};
use super::types::{OPTION_NONE_TAG, OPTION_SOME_TAG};

fn failure_global(ctx: &EmitCtx<'_>) -> Result<u32, WasmGcError> {
    ctx.fn_map.run_failure_global.ok_or(WasmGcError::Validation(
        "Run.fail or Run.failure is called, but the module allocated no failure global".into(),
    ))
}

fn string_idx(ctx: &EmitCtx<'_>) -> Result<u32, WasmGcError> {
    ctx.registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "Run.fail and Run.failure need the String slot to be allocated".into(),
        ))
}

fn string_ref(string_idx: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(string_idx),
    })
}

/// `Run.fail(message)`, with the message already on the stack: keep it
/// unless a reason is already kept. Leaves nothing behind (`Unit`).
pub(super) fn emit_fail(
    func: &mut Function,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let global = failure_global(ctx)?;
    let string_idx = string_idx(ctx)?;
    let message = slots.run_reason_scratch(string_idx);
    func.instruction(&Instruction::LocalSet(message));
    // The turn's record of the call, in the order the VM records it.
    if let Some(&record) = ctx.fn_map.effects.get("Run.fail") {
        func.instruction(&Instruction::LocalGet(message));
        super::body::emit_caller_fn_idx(func, ctx)?;
        func.instruction(&Instruction::Call(record));
    }
    // global = global is null ? message : global
    func.instruction(&Instruction::LocalGet(message));
    func.instruction(&Instruction::GlobalGet(global));
    func.instruction(&Instruction::GlobalGet(global));
    func.instruction(&Instruction::RefIsNull);
    func.instruction(&Instruction::TypedSelect(string_ref(string_idx)));
    func.instruction(&Instruction::GlobalSet(global));
    Ok(())
}

/// `Run.failure()`: the kept reason as an `Option<String>`.
pub(super) fn emit_failure(
    func: &mut Function,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let global = failure_global(ctx)?;
    let string_idx = string_idx(ctx)?;
    let option_idx =
        ctx.registry
            .option_type_idx("Option<String>")
            .ok_or(WasmGcError::Validation(
                "Run.failure answers Option<String>, and that slot was not registered".into(),
            ))?;
    let reason = slots.run_reason_scratch(string_idx);
    func.instruction(&Instruction::GlobalGet(global));
    // Through the recorder on wasm-gc: live it comes back unchanged, and a
    // replay hands back the reading the recording holds.
    if let Some(&record) = ctx.fn_map.effects.get("Run.failure") {
        super::body::emit_caller_fn_idx(func, ctx)?;
        func.instruction(&Instruction::Call(record));
        func.instruction(&Instruction::RefCastNullable(HeapType::Concrete(
            string_idx,
        )));
    }
    func.instruction(&Instruction::LocalSet(reason));
    func.instruction(&Instruction::LocalGet(reason));
    func.instruction(&Instruction::RefIsNull);
    func.instruction(&Instruction::If(BlockType::Result(ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(option_idx),
    }))));
    func.instruction(&Instruction::I32Const(OPTION_NONE_TAG));
    super::body::emit_default_value(func, "String", ctx.registry)?;
    func.instruction(&Instruction::StructNew(option_idx));
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::I32Const(OPTION_SOME_TAG));
    func.instruction(&Instruction::LocalGet(reason));
    func.instruction(&Instruction::StructNew(option_idx));
    func.instruction(&Instruction::End);
    Ok(())
}
