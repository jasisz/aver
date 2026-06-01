//! Collection literal lowering: `MirExpr::List`. Mirrors
//! `emit_list_literal`. (Tuple / map literals are inline in the
//! dispatcher.)

use super::*;

/// Mirror of `emit_list_literal` (emit.rs): resolve the `List<T>`
/// instantiation (stamped type, else first-element hint, else sole
/// registered, else return type), then for a non-empty literal push
/// each element left-to-right, push `null`, and `call $cons_T` N times
/// (the cons fold needs no scratch — vital for nested literals).
/// `Option.None` / empty-list elements emit against the resolved
/// element type, since their own stamp can be a generic `Var`.
pub(crate) fn emit_mir_list_literal(
    func: &mut Function,
    outer: &Spanned<MirExpr>,
    items: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let stamped = aver_type_canonical(outer, ctx.return_type, ctx.registry);
    let canonical =
        if stamped.starts_with("List<") && ctx.registry.list_type_idx(&stamped).is_some() {
            stamped
        } else if let Some(first) = items.first() {
            let needs_hint = matches!(&first.node, MirExpr::List(xs) if xs.is_empty())
                || is_mir_option_none(first);
            let elem_ty = if needs_hint {
                let ret: String = ctx
                    .return_type
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if let Some(inner) = ret.strip_prefix("List<").and_then(|s| s.strip_suffix('>')) {
                    inner.to_string()
                } else {
                    aver_type_str_of(first)
                }
            } else {
                aver_type_str_of(first)
            };
            format!("List<{elem_ty}>")
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect::<String>()
        } else if ctx.registry.list_order.len() == 1 {
            ctx.registry.list_order[0].clone()
        } else {
            let ret: String = ctx
                .return_type
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            if ret.starts_with("List<") {
                ret
            } else if let Some(first) = ctx.registry.list_order.first() {
                first.clone()
            } else {
                ret
            }
        };
    let list_idx = ctx
        .registry
        .list_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List literal: cannot resolve list instantiation (got `{canonical}`)"
        )))?;
    if items.is_empty() {
        func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
            list_idx,
        )));
        return Ok(Some(()));
    }
    let cons_fn = ctx
        .fn_map
        .list_ops_lookup(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List literal: cons helper for `{canonical}` not registered"
        )))?
        .cons;
    let elem_ty = TypeRegistry::list_element_type(&canonical).map(|s| s.to_string());
    for item in items {
        if let Some(elem) = elem_ty.as_deref()
            && is_mir_option_none(item)
            && let Some(inner) = elem
                .strip_prefix("Option<")
                .and_then(|s| s.strip_suffix('>'))
        {
            if emit_mir_option_constructor(func, None, Some(inner.trim()), slots, ctx)?.is_none() {
                return Ok(None);
            }
            continue;
        }
        if let Some(elem) = elem_ty.as_deref()
            && let MirExpr::List(xs) = &item.node
            && xs.is_empty()
            && let Some(inner_idx) = ctx.registry.list_type_idx(elem)
        {
            func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                inner_idx,
            )));
            continue;
        }
        if emit_mir_expr(func, item, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        list_idx,
    )));
    for _ in 0..items.len() {
        func.instruction(&Instruction::Call(cons_fn));
    }
    Ok(Some(()))
}
