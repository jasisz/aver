//! Constructor lowering: `MirExpr::Construct` for user variants and
//! the `Option` / `Result` built-in constructors. Mirrors `emit_expr`'s
//! `Ctor` arm and its helpers.

use super::*;

/// Mirror of `emit_constructor_with_args` (emit.rs): a newtype emits
/// its single payload directly (no `struct.new`); otherwise push each
/// arg and `struct.new $variant`.
pub(crate) fn emit_mir_constructor_with_args(
    func: &mut Function,
    info: &VariantInfo,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if args.len() != info.fields.len() {
        return Err(WasmGcError::Validation(format!(
            "variant has {} field(s) but call supplied {}",
            info.fields.len(),
            args.len()
        )));
    }
    if ctx.registry.newtype_underlying(&info.parent).is_some() {
        return Ok(emit_mir_expr(func, &args[0], slots, ctx)?.map(|_| ()));
    }
    for arg in args {
        if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    func.instruction(&Instruction::StructNew(info.type_idx));
    Ok(Some(()))
}

/// Mirror of `emit_option_constructor` (emit.rs): `Some(v)` →
/// `i32.const 1; v; struct.new $option_T`; `None` →
/// `i32.const 0; default<T>; struct.new $option_T`. `T` comes from the
/// payload's stamped type (Some) or the caller's hint (None).
pub(crate) fn emit_mir_option_constructor(
    func: &mut Function,
    payload: Option<&Spanned<MirExpr>>,
    t_aver_hint: Option<&str>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let t_aver: String = match payload {
        Some(p) => aver_type_str_of(p),
        None => t_aver_hint
            .ok_or(WasmGcError::Validation(
                "Option.None without context — cannot infer the T in Option<T>".into(),
            ))?
            .to_string(),
    };
    let canonical = if t_aver.starts_with("Option<") {
        t_aver.clone()
    } else {
        format!("Option<{t_aver}>")
    };
    let opt_idx = ctx
        .registry
        .option_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option constructor: instantiation `{canonical}` was not registered"
        )))?;
    let inner_ty = TypeRegistry::option_element_type(&canonical).ok_or(WasmGcError::Validation(
        format!("Option canonical `{canonical}` has no element type"),
    ))?;
    match payload {
        Some(p) => {
            func.instruction(&Instruction::I32Const(1));
            if emit_mir_expr(func, p, slots, ctx)?.is_none() {
                return Ok(None);
            }
        }
        None => {
            func.instruction(&Instruction::I32Const(0));
            emit_default_value(func, inner_ty, ctx.registry)?;
        }
    }
    func.instruction(&Instruction::StructNew(opt_idx));
    Ok(Some(()))
}

/// Mirror of `emit_result_constructor` (emit.rs): resolve the
/// `Result<T,E>` instantiation (single registered / by return type / by
/// payload-type match), then `i32.const <tag>; <T-slot>; <E-slot>;
/// struct.new $result`. A `Unit` payload position pushes the `i32`
/// placeholder rather than the (no-value) `Unit`.
pub(crate) fn emit_mir_result_constructor(
    func: &mut Function,
    variant: &str,
    payload: Option<&Spanned<MirExpr>>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let payload = payload.ok_or(WasmGcError::Validation(format!(
        "Result.{variant} requires a payload"
    )))?;
    let payload_ty = aver_type_str_of(payload);
    let canonical = if ctx.registry.result_order.len() == 1 {
        ctx.registry.result_order[0].clone()
    } else {
        let return_canonical: String = ctx
            .return_type
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        if ctx.registry.result_type_idx(&return_canonical).is_some() {
            return_canonical
        } else {
            ctx.registry
                .result_order
                .iter()
                .find(|c| {
                    if let Some((t, e)) = TypeRegistry::result_te(c) {
                        let match_pos = if variant == "Ok" { t } else { e };
                        match_pos == payload_ty.trim()
                    } else {
                        false
                    }
                })
                .cloned()
                .ok_or(WasmGcError::Validation(format!(
                    "Result.{variant}({payload_ty}) — no registered Result<T,E> instantiation matches"
                )))?
        }
    };
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .expect("just-resolved canonical");
    let (t_aver, e_aver) = TypeRegistry::result_te(&canonical).ok_or(WasmGcError::Validation(
        format!("Result canonical `{canonical}` malformed"),
    ))?;

    // A `Unit` payload position pushes nothing via `emit_mir_expr`, but
    // the struct slot is i32-sized — push the placeholder ourselves.
    let emit_payload = |func: &mut Function, pos_ty: &str| -> Result<Option<()>, WasmGcError> {
        if pos_ty.trim() == "Unit" {
            func.instruction(&Instruction::I32Const(0));
            Ok(Some(()))
        } else {
            Ok(emit_mir_expr(func, payload, slots, ctx)?.map(|_| ()))
        }
    };
    if variant == "Ok" {
        func.instruction(&Instruction::I32Const(1));
        if emit_payload(func, t_aver)?.is_none() {
            return Ok(None);
        }
        emit_default_value(func, e_aver, ctx.registry)?;
    } else {
        func.instruction(&Instruction::I32Const(0));
        emit_default_value(func, t_aver, ctx.registry)?;
        if emit_payload(func, e_aver)?.is_none() {
            return Ok(None);
        }
    }
    func.instruction(&Instruction::StructNew(res_idx));
    Ok(Some(()))
}

/// `true` for a MIR `Option.None` constructor (mirror of
/// `is_option_none_expr`) — used to give it the declared element-type
/// hint inside list literals (its own `.ty()` may be a generic `Var`).
pub(crate) fn is_mir_option_none(expr: &Spanned<MirExpr>) -> bool {
    matches!(&expr.node,
        MirExpr::Construct(c)
            if matches!(c.node.ctor, MirCtor::Builtin(BuiltinCtor::OptionNone))
                && c.node.args.is_empty())
}
