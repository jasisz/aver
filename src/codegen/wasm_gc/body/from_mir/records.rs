//! Record lowering: `MirExpr::RecordCreate` / `RecordUpdate`.
//! Mirrors `emit_record_create` / `emit_record_update`.

use super::*;

/// Recognise the source-level carrier projection of a proof-packed nominal.
/// The returned base already has the packed array representation at runtime;
/// emitting the ordinary projection would call `unpack` and allocate one cons
/// cell per element.
pub(crate) fn mir_packed_carrier_projection<'a>(
    expr: &'a Spanned<MirExpr>,
    ctx: &EmitCtx<'_>,
) -> Option<(&'a Spanned<MirExpr>, String)> {
    let MirExpr::Project(project) = &expr.node else {
        return None;
    };
    let raw_name = aver_type_str_of(&project.node.base);
    let type_name = ctx.registry.canonical_type_name(&raw_name);
    ctx.registry.packed_sequence(type_name)?;
    let carrier_field = ctx
        .registry
        .record_fields
        .get(type_name)?
        .first()
        .map(|(name, _)| name)?;
    (project.node.field == *carrier_field)
        .then(|| (project.node.base.as_ref(), type_name.to_string()))
}

fn is_mir_packed_carrier_expr(expr: &Spanned<MirExpr>, type_name: &str, ctx: &EmitCtx<'_>) -> bool {
    if aver_type_str_of(expr).trim() == crate::ir::INTERNAL_BYTE_PAYLOAD_TYPE
        && ctx.registry.byte_payload_type_name.as_deref() == Some(type_name)
    {
        return true;
    }
    if let Some((_, projected_name)) = mir_packed_carrier_projection(expr, ctx) {
        return projected_name == type_name;
    }
    let MirExpr::Call(call) = &expr.node else {
        return false;
    };
    let MirCallee::Builtin(id) = call.node.callee else {
        return false;
    };
    let Some(dotted) = ctx.mir_builtins.and_then(|names| names.get(id.0 as usize)) else {
        return false;
    };
    match (dotted.as_str(), call.node.args.as_slice()) {
        ("List.concat", [left, right]) => {
            is_mir_packed_carrier_expr(left, type_name, ctx)
                && is_mir_packed_carrier_expr(right, type_name, ctx)
        }
        ("List.take" | "List.drop", [source, _]) => {
            is_mir_packed_carrier_expr(source, type_name, ctx)
        }
        _ => false,
    }
}

fn emit_mir_packed_carrier_expr(
    func: &mut Function,
    expr: &Spanned<MirExpr>,
    type_name: &str,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if aver_type_str_of(expr).trim() == crate::ir::INTERNAL_BYTE_PAYLOAD_TYPE
        && ctx.registry.byte_payload_type_name.as_deref() == Some(type_name)
    {
        emit_mir_expr(func, expr, slots, ctx)?.ok_or_else(|| {
            WasmGcError::Validation("packed byte payload could not be emitted".into())
        })?;
        return Ok(());
    }
    if let Some((base, projected_name)) = mir_packed_carrier_projection(expr, ctx) {
        debug_assert_eq!(projected_name, type_name);
        emit_mir_expr(func, base, slots, ctx)?.ok_or_else(|| {
            WasmGcError::Validation(format!(
                "packed carrier `{type_name}` base could not be emitted"
            ))
        })?;
        return Ok(());
    }
    let MirExpr::Call(call) = &expr.node else {
        return Err(WasmGcError::Validation(format!(
            "packed carrier `{type_name}` expression lost its recognised shape"
        )));
    };
    let MirCallee::Builtin(id) = call.node.callee else {
        unreachable!("packed carrier recogniser only accepts builtin calls");
    };
    let dotted = ctx
        .mir_builtins
        .and_then(|names| names.get(id.0 as usize))
        .expect("packed carrier recogniser pinned the builtin identity");
    let ops = ctx
        .fn_map
        .packed_sequence_ops_lookup(type_name)
        .ok_or_else(|| {
            WasmGcError::Validation(format!(
                "packed record `{type_name}` has no preserving operation bridge"
            ))
        })?;
    match (dotted.as_str(), call.node.args.as_slice()) {
        ("List.concat", [left, right]) => {
            emit_mir_packed_carrier_expr(func, left, type_name, slots, ctx)?;
            emit_mir_packed_carrier_expr(func, right, type_name, slots, ctx)?;
            func.instruction(&Instruction::Call(ops.concat));
        }
        ("List.take", [source, count]) => {
            emit_mir_packed_carrier_expr(func, source, type_name, slots, ctx)?;
            if !emit_aint_arg_as_i64_sat(func, count, slots, ctx)? {
                return Err(WasmGcError::Validation(
                    "packed take count could not be emitted".into(),
                ));
            }
            func.instruction(&Instruction::Call(ops.take));
        }
        ("List.drop", [source, count]) => {
            emit_mir_packed_carrier_expr(func, source, type_name, slots, ctx)?;
            if !emit_aint_arg_as_i64_sat(func, count, slots, ctx)? {
                return Err(WasmGcError::Validation(
                    "packed drop count could not be emitted".into(),
                ));
            }
            func.instruction(&Instruction::Call(ops.drop));
        }
        _ => unreachable!("packed carrier recogniser and emitter diverged"),
    }
    Ok(())
}

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
    let emitted = emit_mir_expr(func, value, slots, ctx)?;
    if emitted.is_some() && decl_ty.trim() == "Unit" {
        func.instruction(&Instruction::I32Const(0));
    }
    Ok(emitted.map(|_| ()))
}

/// Declared index of each written field, in WRITTEN order, plus whether
/// that sequence is already the declared one — the question every
/// reordering decision below asks. `None` for a field name the record
/// does not declare; a repeated name counts only on its first write,
/// because both walks below keep the first `field = value` and drop the
/// rest.
fn written_field_order(
    decl_fields: &[(String, String)],
    written: &[MirRecordField],
) -> (Vec<Option<usize>>, bool) {
    let indices: Vec<Option<usize>> = written
        .iter()
        .map(|f| decl_fields.iter().position(|(name, _)| *name == f.name))
        .collect();
    let mut in_declared_order = true;
    let mut seen = vec![false; decl_fields.len()];
    let mut previous: Option<usize> = None;
    for index in indices.iter().flatten() {
        if seen[*index] {
            continue;
        }
        seen[*index] = true;
        if previous.is_some_and(|p| p >= *index) {
            in_declared_order = false;
            break;
        }
        previous = Some(*index);
    }
    (indices, in_declared_order)
}

/// Emit one written field value, followed by the carrier narrowing its
/// declared slot needs. Shared by the declared-order and the
/// written-order walks so both agree on what a field value IS.
fn emit_declared_field_value(
    func: &mut Function,
    type_name: &str,
    decl_field: &(String, String),
    value: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let (decl_name, decl_ty) = decl_field;
    if emit_mir_record_field_value(func, value, decl_ty, slots, ctx)?.is_none() {
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
    Ok(Some(()))
}

/// Park a run of already-evaluated field values — `written` deep on the
/// stack, topmost last — into their per-field scratch locals. Drained in
/// REVERSE so a nested record of the SAME type, which shares these
/// locals, has finished reading them before the enclosing one writes:
/// every user expression has already run by the time the first
/// `local.set` executes.
fn park_written_values(func: &mut Function, written: &[u32]) {
    for slot in written.iter().rev() {
        func.instruction(&Instruction::LocalSet(*slot));
    }
}

/// Mirror of `emit_record_create` (emit.rs): a newtype record emits its
/// single field's value directly; otherwise push every declared field
/// (in declaration order) and `struct.new $type_idx`.
///
/// A literal that WRITES its fields out of declared order evaluates them
/// in written order — the order the source reads, the order the VM and
/// the Rust backend run, and the order `src/ir/last_use.rs` assumed when
/// it turned the final read of a local into a consuming move — parks the
/// values in scratch locals, and only then pushes them in the declared
/// order `struct.new` wants. A literal written in declared order keeps
/// the straight-line walk, so its bytes are unchanged.
pub(crate) fn emit_mir_record_create(
    func: &mut Function,
    type_name: &str,
    fields: &[MirRecordField],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    // Resolve an identity-preserving qualified alias spelling
    // (`Dep.Octets`) to its canonical post-flatten name ONCE, so every
    // lookup below (packed, newtype/carrier, plain struct + its
    // bare-keyed `record_fields` list) keys consistently. The demotion
    // scans canonicalize construct sites through the SAME map, so a
    // spelling that resolves a packed/carrier layout here was already
    // visible to them under this key.
    let type_name = ctx.registry.canonical_type_name(type_name);
    if ctx.registry.packed_sequence(type_name).is_some() {
        let field = fields.first().ok_or(WasmGcError::Validation(format!(
            "packed record `{type_name}` requires one List<Int> field"
        )))?;
        if is_mir_packed_carrier_expr(&field.value, type_name, ctx) {
            emit_mir_packed_carrier_expr(func, &field.value, type_name, slots, ctx)?;
            return Ok(Some(()));
        }
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
    let (written_indices, in_declared_order) = written_field_order(&decl_fields, fields);
    if in_declared_order {
        for (decl_name, decl_ty) in &decl_fields {
            let provided =
                fields
                    .iter()
                    .find(|f| &f.name == decl_name)
                    .ok_or(WasmGcError::Validation(format!(
                        "record `{type_name}` missing field `{decl_name}`"
                    )))?;
            if emit_declared_field_value(
                func,
                type_name,
                &(decl_name.clone(), decl_ty.clone()),
                &provided.value,
                slots,
                ctx,
            )?
            .is_none()
            {
                return Ok(None);
            }
        }
        func.instruction(&Instruction::StructNew(type_idx));
        return Ok(Some(()));
    }
    // Written out of declared order.
    let scratch = slots.record_field_scratch(type_name, &decl_fields, ctx.registry)?;
    let mut filled = vec![false; decl_fields.len()];
    let mut written_slots: Vec<u32> = Vec::with_capacity(fields.len());
    for (field, index) in fields.iter().zip(&written_indices) {
        let Some(index) = *index else { continue };
        if filled[index] {
            continue;
        }
        filled[index] = true;
        if emit_declared_field_value(
            func,
            type_name,
            &decl_fields[index],
            &field.value,
            slots,
            ctx,
        )?
        .is_none()
        {
            return Ok(None);
        }
        written_slots.push(scratch[index]);
    }
    if let Some(missing) = filled.iter().position(|filled| !filled) {
        return Err(WasmGcError::Validation(format!(
            "record `{type_name}` missing field `{}`",
            decl_fields[missing].0
        )));
    }
    park_written_values(func, &written_slots);
    for slot in &scratch {
        func.instruction(&Instruction::LocalGet(*slot));
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
    // Same one-shot alias canonicalization as `emit_mir_record_create`.
    let type_name = ctx.registry.canonical_type_name(type_name);
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
    let (written_indices, in_declared_order) = written_field_order(&decl_fields, updates);
    if in_declared_order {
        for (decl_name, decl_ty) in &decl_fields {
            if let Some(override_field) = updates.iter().find(|f| &f.name == decl_name) {
                if emit_declared_field_value(
                    func,
                    type_name,
                    &(decl_name.clone(), decl_ty.clone()),
                    &override_field.value,
                    slots,
                    ctx,
                )?
                .is_none()
                {
                    return Ok(None);
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
                // A COPIED-from-base eligible field is already a native `i64` in
                // the base struct, so the `struct.get` yields the i64 the new
                // field slot expects — no bridge needed (the boxed `struct.get`
                // of an `$AverInt` field stays `$AverInt`, also matching its
                // slot).
                func.instruction(&Instruction::StructGet {
                    struct_type_index: type_idx,
                    field_index: field_idx,
                });
            }
        }
        func.instruction(&Instruction::StructNew(type_idx));
        return Ok(Some(()));
    }
    // Overrides written out of declared order. Evaluate the base first and
    // then the overrides in written order — the order the VM runs them and
    // the order `src/ir/last_use.rs` assumed — park every value, and only
    // then assemble the struct in declared order. A field named twice keeps
    // the first `field = value` and drops the rest unevaluated, and a name
    // the record does not declare is skipped, exactly as the declared-order
    // walk did.
    let mut overridden = vec![false; decl_fields.len()];
    let mut written_slots: Vec<u32> = Vec::with_capacity(updates.len());
    let scratch = slots.record_field_scratch(type_name, &decl_fields, ctx.registry)?;
    let copies_from_base = {
        let mut seen = vec![false; decl_fields.len()];
        for index in written_indices.iter().flatten() {
            seen[*index] = true;
        }
        seen.iter().any(|overridden| !overridden)
    };
    let base_slot = if copies_from_base {
        if emit_mir_expr(func, base, slots, ctx)?.is_none() {
            return Ok(None);
        }
        Some(slots.record_base_scratch(type_name, type_idx)?)
    } else {
        None
    };
    for (field, index) in updates.iter().zip(&written_indices) {
        let Some(index) = *index else { continue };
        if overridden[index] {
            continue;
        }
        overridden[index] = true;
        if emit_declared_field_value(
            func,
            type_name,
            &decl_fields[index],
            &field.value,
            slots,
            ctx,
        )?
        .is_none()
        {
            return Ok(None);
        }
        written_slots.push(scratch[index]);
    }
    park_written_values(func, &written_slots);
    if let Some(base_slot) = base_slot {
        func.instruction(&Instruction::LocalSet(base_slot));
    }
    for (index, (decl_name, _)) in decl_fields.iter().enumerate() {
        if overridden[index] {
            func.instruction(&Instruction::LocalGet(scratch[index]));
            continue;
        }
        let field_idx = ctx
            .registry
            .record_field_index(type_name, decl_name)
            .ok_or(WasmGcError::Validation(format!(
                "record `{type_name}` has no field `{decl_name}` to copy from base"
            )))?;
        func.instruction(&Instruction::LocalGet(
            base_slot.expect("a copied field implies the base was parked"),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: type_idx,
            field_index: field_idx,
        });
    }
    func.instruction(&Instruction::StructNew(type_idx));
    Ok(Some(()))
}
