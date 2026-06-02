//! Control-flow lowering: `MirExpr::Try` (`?` propagation) and
//! `MirExpr::IndependentProduct` (`(a, b, c)!` / `?!`). Mirrors
//! `emit_error_prop` and the independent-product emit in `emit_expr`.

use super::*;

/// Mirror of `emit_expr`'s `IndependentProduct` arm. wasm-gc has no
/// parallelism, so both `(a, b, c)!` and `(a, b, c)?!` lower as
/// sequential evaluation wrapped in the structural-scope markers
/// (`enter_group`, per-element `set_branch(i)`, `exit_group`) so the
/// recorder annotates contained effects identically to the VM.
/// `enter_group` / `exit_group` / the branch markers are the
/// ResolvedExpr-free `emit_group_call` / `emit_branch_marker` helpers,
/// reused verbatim; only the per-element evaluation recurses
/// `emit_mir_expr`. Always produces a tuple value, so `Some(true)`.
pub(crate) fn emit_mir_independent_product(
    func: &mut Function,
    ip: &MirIndependentProduct,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    emit_group_call(func, ctx, "__record_enter_group");
    let inner = if ip.unwrap_results {
        emit_mir_ip_unwrap(func, &ip.items, slots, ctx)?
    } else {
        emit_mir_ip_tuple(func, &ip.items, slots, ctx)?
    };
    if inner.is_none() {
        return Ok(None);
    }
    emit_group_call(func, ctx, "__record_exit_group");
    Ok(Some(true))
}

/// `(a, b, c)!` — raw tuple of the elements, each preceded by its
/// branch marker. Mirror of `emit_tuple_literal_with_branch_markers`.
fn emit_mir_ip_tuple(
    func: &mut Function,
    items: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if items.len() < 2 {
        return Err(WasmGcError::Validation(format!(
            "Independent-product `!` tuple needs at least 2 elements; got {}",
            items.len()
        )));
    }
    let elem_tys: Vec<String> = items.iter().map(aver_type_str_of).collect();
    let canonical = format!("Tuple<{}>", elem_tys.join(","))
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect::<String>();
    let tuple_idx = ctx
        .registry
        .tuple_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "`!` tuple: `{canonical}` slot not registered"
        )))?;
    for (idx, item) in items.iter().enumerate() {
        emit_branch_marker(func, ctx, idx as u32);
        if emit_mir_expr(func, item, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    func.instruction(&Instruction::StructNew(tuple_idx));
    Ok(Some(()))
}

/// `(a, b, c)?!` — unwrap each element's `Ok` into the tuple,
/// early-returning a freshly-built `Result<EnclosingT, E>::Err` on the
/// first `Err`. Mirror of `emit_independent_product_unwrap`.
fn emit_mir_ip_unwrap(
    func: &mut Function,
    items: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if items.len() < 2 {
        return Err(WasmGcError::Validation(format!(
            "Independent product `?!` needs at least 2 elements; got {}",
            items.len()
        )));
    }
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "`?!` independent product requires a subject scratch slot but none was reserved".into(),
    ))?;
    let enclosing_canonical: String = ctx
        .return_type
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let enclosing_idx =
        ctx.registry
            .result_type_idx(&enclosing_canonical)
            .ok_or(WasmGcError::Validation(format!(
                "`?!` independent product: enclosing fn return type `{enclosing_canonical}` \
             is not a registered Result<X, E>"
            )))?;
    let (enclosing_t_aver, _) =
        TypeRegistry::result_te(&enclosing_canonical).ok_or(WasmGcError::Validation(format!(
            "`?!` independent product: enclosing canonical `{enclosing_canonical}` malformed"
        )))?;

    for (idx, item) in items.iter().enumerate() {
        emit_branch_marker(func, ctx, idx as u32);
        let elem_aver = aver_type_str_of(item);
        let elem_canonical: String = elem_aver.chars().filter(|c| !c.is_whitespace()).collect();
        let elem_res_idx =
            ctx.registry
                .result_type_idx(&elem_canonical)
                .ok_or(WasmGcError::Validation(format!(
                    "`?!` element type `{elem_aver}` is not a registered Result<T_i, E>"
                )))?;
        let (elem_t_aver, _) =
            TypeRegistry::result_te(&elem_canonical).ok_or(WasmGcError::Validation(format!(
                "`?!` element canonical `{elem_canonical}` malformed"
            )))?;
        let ok_wasm = aver_to_wasm(elem_t_aver, Some(ctx.registry))?.unwrap_or(ValType::I32);
        let block_ty = wasm_encoder::BlockType::Result(ok_wasm);

        if emit_mir_expr(func, item, slots, ctx)?.is_none() {
            return Ok(None);
        }
        func.instruction(&Instruction::LocalSet(scratch));
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(elem_res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: elem_res_idx,
            field_index: 0,
        });
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Eq);
        func.instruction(&Instruction::If(block_ty));
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(elem_res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: elem_res_idx,
            field_index: 1,
        });
        func.instruction(&Instruction::Else);
        func.instruction(&Instruction::I32Const(0));
        emit_default_value(func, enclosing_t_aver, ctx.registry)?;
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(elem_res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: elem_res_idx,
            field_index: 2,
        });
        func.instruction(&Instruction::StructNew(enclosing_idx));
        func.instruction(&Instruction::Return);
        func.instruction(&Instruction::End);
    }

    let mut elem_t_avers: Vec<String> = Vec::with_capacity(items.len());
    for item in items {
        let elem_aver = aver_type_str_of(item);
        let elem_canonical: String = elem_aver.chars().filter(|c| !c.is_whitespace()).collect();
        let (t, _) = TypeRegistry::result_te(&elem_canonical).expect("re-parse");
        elem_t_avers.push(t.to_string());
    }
    let tuple_canonical: String = format!("Tuple<{}>", elem_t_avers.join(","))
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let tuple_idx =
        ctx.registry
            .tuple_type_idx(&tuple_canonical)
            .ok_or(WasmGcError::Validation(format!(
                "`?!` result tuple `{tuple_canonical}` not registered"
            )))?;
    func.instruction(&Instruction::StructNew(tuple_idx));
    Ok(Some(()))
}

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
