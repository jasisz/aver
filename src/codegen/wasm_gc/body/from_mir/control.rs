//! Control-flow lowering: `MirExpr::Try` (`?` propagation). Mirrors
//! `emit_error_prop`.

use super::*;

/// Mirror of `emit_error_prop` (emit.rs): `value?` over a `Result<T,E>`.
/// Stash the subject, test the tag — on `Ok` push the payload (field 1;
/// nothing for `Result<Unit,E>`), on `Err` rebuild a fresh
/// `Result<EnclosingT, E>::Err` (tag 0, `default<EnclosingT>`, the
/// subject's err field) and `return` it so the type lines up with the
/// enclosing fn. Returns `Some(produces)` where `produces` is `false`
/// for a `Result<Unit,E>?` (no observable Ok value), else `true`.
pub(crate) fn emit_mir_try(
    func: &mut Function,
    inner: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "ErrorProp (`?`) requires a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(inner);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "ErrorProp: subject type `{subject_ty}` is not a registered Result<T,E>"
        )))?;
    let (t_aver, _e_aver) = TypeRegistry::result_te(&canonical).ok_or(WasmGcError::Validation(
        format!("ErrorProp: Result canonical `{canonical}` malformed"),
    ))?;
    let unit_ok = t_aver.trim() == "Unit";
    let block_ty = if unit_ok {
        wasm_encoder::BlockType::Empty
    } else {
        let ok_wasm = aver_to_wasm(t_aver, Some(ctx.registry))?.ok_or(WasmGcError::Validation(
            format!("ErrorProp: Ok type `{t_aver}` has no wasm representation"),
        ))?;
        wasm_encoder::BlockType::Result(ok_wasm)
    };
    let enclosing_canonical: String = ctx
        .return_type
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let enclosing_idx =
        ctx.registry
            .result_type_idx(&enclosing_canonical)
            .ok_or(WasmGcError::Validation(format!(
                "ErrorProp: enclosing fn return `{}` is not a registered Result<T,E>",
                ctx.return_type
            )))?;
    let (enclosing_t_aver, _) =
        TypeRegistry::result_te(&enclosing_canonical).ok_or(WasmGcError::Validation(format!(
            "ErrorProp: enclosing Result canonical `{enclosing_canonical}` malformed"
        )))?;

    if emit_mir_expr(func, inner, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));

    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(res_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 0,
    });
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(block_ty));
    if !unit_ok {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: res_idx,
            field_index: 1,
        });
    }
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::I32Const(0));
    emit_default_value(func, enclosing_t_aver, ctx.registry)?;
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(res_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 2,
    });
    func.instruction(&Instruction::StructNew(enclosing_idx));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);
    Ok(Some(!unit_ok))
}
