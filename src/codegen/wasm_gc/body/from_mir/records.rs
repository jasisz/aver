//! Record lowering: `MirExpr::RecordCreate` / `RecordUpdate`.
//! Mirrors `emit_record_create` / `emit_record_update`.

use super::*;

/// Emit a record field / update value, mirroring `emit_record_create`'s
/// per-field special-cases: an `Option.None` value emits through the
/// constructor with the field's declared `T` (the bare-literal value's
/// own `.ty()` may be a generic `Var`, so the field declaration is the
/// authoritative shape), and an empty-list value emits `ref.null` of the
/// field's declared `List<T>`. Everything else recurses via
/// `emit_mir_expr`.
pub(crate) fn emit_mir_record_field_value(
    func: &mut Function,
    value: &Spanned<MirExpr>,
    decl_ty: &str,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if let MirExpr::Construct(c) = &value.node
        && matches!(c.node.ctor, MirCtor::Builtin(BuiltinCtor::OptionNone))
        && let Some(inner) = decl_ty
            .trim()
            .strip_prefix("Option<")
            .and_then(|s| s.strip_suffix('>'))
    {
        return emit_mir_option_constructor(func, None, Some(inner.trim()), slots, ctx);
    }
    if let MirExpr::List(items) = &value.node
        && items.is_empty()
    {
        let canonical: String = decl_ty.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(list_idx) = ctx.registry.list_type_idx(&canonical) {
            func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                list_idx,
            )));
            return Ok(Some(()));
        }
    }
    Ok(emit_mir_expr(func, value, slots, ctx)?.map(|_| ()))
}

/// Mirror of `emit_record_create` (emit.rs): a newtype record emits its
/// single field's value directly; otherwise push every declared field
/// (in declaration order) and `struct.new $type_idx`.
pub(crate) fn emit_mir_record_create(
    func: &mut Function,
    type_name: &str,
    fields: &[MirRecordField],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if ctx.registry.packed_sequence(type_name).is_some() {
        let field = fields.first().ok_or(WasmGcError::Validation(format!(
            "packed record `{type_name}` requires one List<Int> field"
        )))?;
        let produced = emit_mir_record_field_value(func, &field.value, "List<Int>", slots, ctx)?;
        if produced.is_some() {
            let ops = ctx
                .fn_map
                .packed_sequence_ops_lookup(type_name)
                .ok_or_else(|| {
                    WasmGcError::Validation(format!(
                        "packed record `{type_name}` has no construct bridge"
                    ))
                })?;
            func.instruction(&Instruction::Call(ops.pack));
        }
        return Ok(produced);
    }
    if ctx.registry.newtype_underlying(type_name).is_some() {
        let field = fields.first().ok_or(WasmGcError::Validation(format!(
            "newtype record `{type_name}` requires one field"
        )))?;
        let produced = emit_mir_expr(func, &field.value, slots, ctx)?;
        // ETAP-2 carrier-`i64`: an eligible carrier construct is still
        // identity, but the field value is a plain `Int` (`$AverInt`) and the
        // carrier holds a native `i64`, so narrow it at the boundary.
        if produced.is_some() && ctx.registry.is_eligible_carrier(type_name) {
            emit_carrier_construct_bridge(func, ctx)?;
        }
        return Ok(produced.map(|_| ()));
    }
    let type_idx = ctx
        .registry
        .record_type_idx(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "unknown record type `{type_name}`"
        )))?;
    let decl_fields = ctx
        .registry
        .record_fields
        .get(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "record `{type_name}` missing field list"
        )))?
        .clone();
    for (decl_name, decl_ty) in &decl_fields {
        let provided =
            fields
                .iter()
                .find(|f| &f.name == decl_name)
                .ok_or(WasmGcError::Validation(format!(
                    "record `{type_name}` missing field `{decl_name}`"
                )))?;
        if emit_mir_record_field_value(func, &provided.value, decl_ty, slots, ctx)?.is_none() {
            return Ok(None);
        }
        // ETAP-2 multi-field carrier-`i64`: an eligible bounded Int field is
        // stored as a native `i64`, but the rewrite boxes every `RecordCreate`
        // field value (`rewrite_boxed`), so the value just emitted is an
        // `$AverInt`. Narrow it to the i64 the field slot holds via the
        // construct bridge (`__aint_to_i64_checked`, which can never trap — the
        // smart-ctor bound proves the fit). The bridge is a no-op when bignum is
        // off (`Int` is already a scalar i64). Mirrors the single-field carrier
        // construct bridge, per eligible field.
        if ctx.registry.is_eligible_carrier_field(type_name, decl_name) {
            emit_carrier_construct_bridge(func, ctx)?;
        }
    }
    func.instruction(&Instruction::StructNew(type_idx));
    Ok(Some(()))
}

/// Mirror of `emit_record_update` (emit.rs): push each declared field in
/// order — the override value when present, else `struct.get` it from
/// the base — then `struct.new $type_idx`.
pub(crate) fn emit_mir_record_update(
    func: &mut Function,
    type_name: &str,
    base: &Spanned<MirExpr>,
    updates: &[MirRecordField],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if ctx.registry.packed_sequence(type_name).is_some() {
        let produced = if let Some(override_field) = updates.first() {
            emit_mir_record_field_value(func, &override_field.value, "List<Int>", slots, ctx)?
        } else {
            emit_mir_expr(func, base, slots, ctx)?.map(|_| ())
        };
        if produced.is_some() && !updates.is_empty() {
            let ops = ctx
                .fn_map
                .packed_sequence_ops_lookup(type_name)
                .ok_or_else(|| {
                    WasmGcError::Validation(format!(
                        "packed record `{type_name}` has no update bridge"
                    ))
                })?;
            func.instruction(&Instruction::Call(ops.pack));
        }
        return Ok(produced);
    }
    // Mirror `emit_mir_record_create`'s newtype short-circuit: a record with
    // a single primitive field is newtype-erased to the bare underlying value
    // (Int->i64, Float->f64, Bool->i32), so an UPDATE must also emit the bare
    // value with NO struct ops. Without this, `RecordUpdate` would
    // `struct.get`/`struct.new` the erased wrapper and push a `(ref $type)`
    // where the surrounding flow expects the unwrapped primitive -> wasm-gc
    // validation error (`type mismatch: expected i64, found (ref $type)`).
    if ctx.registry.newtype_underlying(type_name).is_some() {
        let produced = if let Some(override_field) = updates.first() {
            // The newtype's only field is overridden: emit the override value
            // directly. The rewrite boxes update values, so an eligible carrier
            // field's value is an `$AverInt` that must be narrowed to the i64
            // the carrier holds — exactly like `emit_mir_record_create`'s
            // single-field carrier construct bridge.
            let p = emit_mir_expr(func, &override_field.value, slots, ctx)?;
            if p.is_some() && ctx.registry.is_eligible_carrier(type_name) {
                emit_carrier_construct_bridge(func, ctx)?;
            }
            p
        } else {
            // No override for the single field: re-emit the base, which already
            // evaluates to the bare erased value (no bridge — a carrier base is
            // already the native `i64`, matching the copied-from-base case).
            emit_mir_expr(func, base, slots, ctx)?
        };
        return Ok(produced.map(|_| ()));
    }
    let type_idx = ctx
        .registry
        .record_type_idx(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "unknown record type `{type_name}`"
        )))?;
    let decl_fields = ctx
        .registry
        .record_fields
        .get(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "record `{type_name}` missing field list"
        )))?
        .clone();
    for (decl_name, decl_ty) in &decl_fields {
        if let Some(override_field) = updates.iter().find(|f| &f.name == decl_name) {
            if emit_mir_record_field_value(func, &override_field.value, decl_ty, slots, ctx)?
                .is_none()
            {
                return Ok(None);
            }
            // ETAP-2 multi-field carrier-`i64`: an OVERRIDE of an eligible
            // bounded field is a boxed `$AverInt` (the rewrite boxes every
            // update value) but the field slot is a native `i64` — narrow it via
            // the construct bridge, exactly like `emit_mir_record_create`.
            if ctx.registry.is_eligible_carrier_field(type_name, decl_name) {
                emit_carrier_construct_bridge(func, ctx)?;
            }
        } else {
            let field_idx = ctx
                .registry
                .record_field_index(type_name, decl_name)
                .ok_or(WasmGcError::Validation(format!(
                    "record `{type_name}` has no field `{decl_name}` to copy from base"
                )))?;
            if emit_mir_expr(func, base, slots, ctx)?.is_none() {
                return Ok(None);
            }
            // A COPIED-from-base eligible field is already a native `i64` in the
            // base struct, so the `struct.get` yields the i64 the new field slot
            // expects — no bridge needed (the boxed `struct.get` of an
            // `$AverInt` field stays `$AverInt`, also matching its slot).
            func.instruction(&Instruction::StructGet {
                struct_type_index: type_idx,
                field_index: field_idx,
            });
        }
    }
    func.instruction(&Instruction::StructNew(type_idx));
    Ok(Some(()))
}
