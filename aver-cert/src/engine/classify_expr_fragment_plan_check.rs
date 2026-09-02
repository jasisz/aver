fn check_expr_fragment_plan_object(
    wasm_bytes: &[u8],
    export_name: &str,
    plan: ExprFragmentPlan,
) -> Result<(usize, Cert, bool, Option<String>), String> {
    let (user_fns, _box_idx, _user_idx_set, carrier, _host_roles, host_table, struct_field_counts) =
        disassemble(wasm_bytes)?;
    let (func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("plan names unknown export `{export_name}`"))?;
    // Zero-arity exports are legitimate compute-face targets (constant
    // constructors); the per-face parameter checks and the exact nominal
    // signature pin keep every other family fail-closed.
    if !frag_calls_resolvable(&f.calls, &host_table) {
        return Err(format!(
            "plan for `{export_name}` does not target a non-recursive expr fragment"
        ));
    }
    let carrier = carrier.ok_or_else(|| {
        format!("plan for `{export_name}` needs the Int carrier type from the wasm module")
    })?;
    let params = f
        .params
        .iter()
        .map(|ty| expr_fragment_ty_from_wasm_param(ty, carrier))
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| format!("plan for `{export_name}` has unsupported wasm parameter types"))?;
    let result = expr_fragment_ty_from_wasm_result(
        f.result
            .ok_or_else(|| format!("plan for `{export_name}` targets a function with no result"))?,
        carrier,
    )
    .ok_or_else(|| format!("plan for `{export_name}` has unsupported wasm result type"))?;
    // Fail-closed host-call discipline: every hostCall node must cite exactly
    // the byte-derived index for its role. Carrier-returning plans and
    // host-call-bearing plans alike are admitted only as one of the exact
    // recognised faces below; anything else is declined here, mirroring the
    // producer gate in `expr_fragment_plan_has_face`, which reads the same two
    // predicates so the pair cannot drift apart.
    check_plan_host_calls(&plan.body, &host_table)
        .map_err(|e| format!("plan for `{export_name}`: {e}"))?;
    // Fail-closed struct discipline: every struct.get.user node must cite a
    // real module struct type (never the carrier) and a field inside its
    // byte-derived field count, mirroring the hostCall index-vs-table check.
    check_plan_struct_gets(&plan.body, carrier, &struct_field_counts)
        .map_err(|e| format!("plan for `{export_name}`: {e}"))?;
    let tag_dispatch = expr_fragment_is_tag_dispatch(&plan);
    let vector_get = expr_fragment_vector_get_face(&plan).is_some();
    let record_proj = expr_fragment_record_proj_face(&plan).is_some();
    // The Int selection face calls a runtime helper and returns an Int
    // carrier, so it would trip the gate below without its own admission.
    let int_cmp = expr_fragment_int_select_face(&plan).is_some();
    let record_compute = expr_fragment_record_compute_face(&plan, &host_table).is_some();
    if (expr_fragment_plan_has_host_calls(&plan) || plan.result == FragTy::IntCarrier)
        && !tag_dispatch
        && !vector_get
        && !record_proj
        && !int_cmp
        && !record_compute
    {
        return Err(format!(
            "plan for `{export_name}` has no rendered proof face: Int-carrier results \
             and runtime host calls are supported only through the tag-dispatch, \
             fused vector-read, record field-read, Int selection, or record \
             projection-compute face"
        ));
    }
    // Face-gated AdtRef admission (the FIX-1 pattern): plans touching opaque
    // user-ADT references are accepted ONLY as an exact recognised face;
    // anything else declines fail-closed on producer and verifier alike.
    if expr_fragment_plan_touches_adt_ref(&plan)
        && expr_fragment_project_face(&plan).is_none()
        && !tag_dispatch
        && !vector_get
        && !record_proj
        && !record_compute
    {
        return Err(format!(
            "plan for `{export_name}` has no rendered proof face: user-ADT references \
             are supported only through the field-projection, fused vector-read, or \
             record field-read face"
        ));
    }
    if plan.params != params {
        return Err(format!(
            "plan for `{export_name}` has params {:?}, but wasm signature requires {:?}",
            plan.params, params
        ));
    }
    if plan.result != result {
        return Err(format!(
            "plan for `{export_name}` has result {:?}, but wasm signature requires {:?}",
            plan.result, result
        ));
    }
    // The ordinary WebAssembly profile admits multiple sign and, in arithmetic
    // NaN cases, payload bit patterns for an arithmetic NaN result.
    // Our current Float codomain face is `floatBitsRepr`, i.e. equality with
    // one exact `UInt64`.  It is therefore not a sound face for a Float result
    // that depends on f64.add/f64.mul over the unrestricted raw-bit domain.
    // Keep comparisons such as f64.le: their Bool result is deterministic even
    // when either operand is NaN.  Re-enable Float-producing arithmetic only
    // after the schema has a relational NaN result representation (or a
    // separately declared deterministic/canonicalizing Wasm profile).
    if expr_fragment_needs_relational_nan_result(&plan) {
        return Err(format!(
            "plan for `{export_name}`: general Wasm allows multiple NaN sign/payload results for \
             f64.add/f64.mul; exact-bit Float output needs a relational result model"
        ));
    }
    let canonical_ops = lower_expr_fragment_plan(&plan, carrier)?;
    let actual_ops = strip_trailing_end(&f.ops);
    let canonical_code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, carrier)?;
    let ops_match = canonical_ops.as_slice() == actual_ops;
    let bytes_match = canonical_code_entry_bytes == f.code_entry_bytes;
    let cert = Cert::ExprFragment {
        name: export_name.to_string(),
        self_idx: f.wasm_idx,
        type_idx: f.type_idx,
        nlocals: f.nlocals,
        carrier,
        source_plan: None,
        record_decl: None,
        record_compute: None,
        plan: plan.clone(),
        ops: canonical_ops,
    };
    let mismatch_reason = if ops_match && bytes_match {
        None
    } else {
        Some(format!(
            "decoded_ops_match={ops_match}, {}",
            byte_match_summary(
                "code_entry_bytes",
                &canonical_code_entry_bytes,
                &f.code_entry_bytes
            )
        ))
    };
    Ok((func_order, cert, ops_match && bytes_match, mismatch_reason))
}

fn expr_fragment_needs_relational_nan_result(plan: &ExprFragmentPlan) -> bool {
    plan.result == FragTy::F64 && block_has_nan_nondeterministic_float_op(&plan.body)
}

fn block_has_nan_nondeterministic_float_op(block: &FragBlock) -> bool {
    // Deliberately exhaustive: extending FragNodeKind must force an explicit
    // decision about nested blocks and Float-bit observation at this gate.
    block.nodes.iter().any(|node| match &node.kind {
        // Packs already-computed values; observes no Float bits itself.
        FragNodeKind::StructNew { .. } => false,
        FragNodeKind::Prim { op, .. } => prim_has_nan_nondeterministic_float_result(op),
        FragNodeKind::If {
            then_block,
            else_block,
            ..
        } => {
            block_has_nan_nondeterministic_float_op(then_block)
                || block_has_nan_nondeterministic_float_op(else_block)
        }
        FragNodeKind::Local { .. }
        | FragNodeKind::ConstBool(_)
        | FragNodeKind::ConstI64(_)
        | FragNodeKind::ConstI32(_)
        | FragNodeKind::ConstF64(_)
        | FragNodeKind::StructGet { .. }
        | FragNodeKind::StructGetUser { .. }
        | FragNodeKind::RefIsNull { .. }
        | FragNodeKind::HostCall { .. }
        | FragNodeKind::SelfCall { .. }
        | FragNodeKind::VectorGetOrDefault { .. }
        // Yields the source Boolean; observes no Float bits.
        | FragNodeKind::IntSignCmp { .. } => false,
    })
}

fn prim_has_nan_nondeterministic_float_result(op: &FragPrim) -> bool {
    match op {
        FragPrim::F64Add | FragPrim::F64Mul => true,
        FragPrim::F64Le
        | FragPrim::F64Ge
        | FragPrim::F64Lt
        | FragPrim::F64Gt
        | FragPrim::F64Eq
        | FragPrim::I64Eq
        | FragPrim::I64LeS
        | FragPrim::I64LtS
        | FragPrim::I64GeS
        | FragPrim::I64GtS
        | FragPrim::I32Eq
        | FragPrim::I32LtS
        | FragPrim::I32GtS
        | FragPrim::I32GeS
        | FragPrim::I32And => false,
    }
}

/// Decode the ordered scalar-leaf field list of a stage-1 flat scalar record at
/// `struct_idx` from the module type section, byte-for-byte the inverse of the
/// wall's `lowerTypeDecl`: an immutable `(ref null carrier)` field is the Int
/// carrier leaf, an immutable `i32` is the Bool scalar, an immutable `f64` is
/// the Float scalar. Any other shape (a nullary/exact ref, a mutable field, a
/// supertype `.sub` form, a non-carrier reference, an unexpected storage, or a
/// missing/oversized index) makes the struct NOT a flat scalar record, so the
/// projection declines fail-closed rather than emitting a record face the wall
/// equality pin would refuse.
fn record_leaves_from_bytes(
    wasm_bytes: &[u8],
    carrier: u32,
    struct_idx: u32,
) -> Option<Vec<RecordLeaf>> {
    use wasmparser::{CompositeInnerType, Parser, Payload, StorageType, ValType};
    let mut next_type_idx: u32 = 0;
    for payload in Parser::new(0).parse_all(wasm_bytes) {
        let Payload::TypeSection(reader) = payload.ok()? else {
            continue;
        };
        for rg in reader {
            for sub in rg.ok()?.into_types() {
                let idx = next_type_idx;
                next_type_idx += 1;
                if idx != struct_idx {
                    continue;
                }
                // `lowerTypeDecl` only ever produces a `.plain` struct: a
                // supertyped `.sub`/`.subFinal` form cannot satisfy the
                // equality pin, so decline it here rather than at the kernel.
                if sub.supertype_idx.is_some() {
                    return None;
                }
                let CompositeInnerType::Struct(st) = &sub.composite_type.inner else {
                    return None;
                };
                let mut leaves = Vec::with_capacity(st.fields.len());
                for field in st.fields.iter() {
                    // Records lower to immutable fields; a mutable storage never
                    // matches `lowerTypeDecl`'s immutable leaf.
                    if field.mutable {
                        return None;
                    }
                    let leaf = match field.element_type {
                        StorageType::Val(ValType::I32) => RecordLeaf::BoolScalar,
                        StorageType::Val(ValType::F64) => RecordLeaf::FloatScalar,
                        StorageType::Val(ValType::Ref(rt)) => {
                            // The Int carrier leaf lowers to `(ref null carrier)`
                            // exactly: a nullable concrete reference at the
                            // module's carrier index.
                            if !rt.is_nullable() || heap_type_index(rt.heap_type()) != Some(carrier)
                            {
                                return None;
                            }
                            RecordLeaf::IntCarrier
                        }
                        _ => return None,
                    };
                    leaves.push(leaf);
                }
                return Some(leaves);
            }
        }
    }
    None
}

#[cfg(all(test, feature = "engine"))]
mod record_leaves_tests {
    use super::*;

    /// A module whose type section holds: index 0 the Int carrier
    /// `{i64, anyref, i32}`, index 1 a Person-shaped record
    /// `{(ref null carrier), i32}`, index 2 a `{f64, (ref null carrier)}`
    /// record, index 3 a struct with a mutable field.
    fn fixture() -> Vec<u8> {
        wat::parse_str(
            r#"(module
                 (type $carrier (struct (field i64) (field anyref) (field i32)))
                 (type $person (struct (field (ref null $carrier)) (field i32)))
                 (type $floaty (struct (field f64) (field (ref null $carrier))))
                 (type $mutrec (struct (field (mut i32)))))"#,
        )
        .expect("fixture module assembles")
    }

    #[test]
    fn decodes_flat_scalar_records() {
        let bytes = fixture();
        assert_eq!(
            record_leaves_from_bytes(&bytes, 0, 1),
            Some(vec![RecordLeaf::IntCarrier, RecordLeaf::BoolScalar])
        );
        assert_eq!(
            record_leaves_from_bytes(&bytes, 0, 2),
            Some(vec![RecordLeaf::FloatScalar, RecordLeaf::IntCarrier])
        );
    }

    #[test]
    fn declines_wrong_struct_index() {
        let bytes = fixture();
        // Out of range.
        assert_eq!(record_leaves_from_bytes(&bytes, 0, 99), None);
        // The carrier struct itself is not a flat scalar record (its first
        // field is a raw `i64`, not a leaf storage).
        assert_eq!(record_leaves_from_bytes(&bytes, 0, 0), None);
    }

    #[test]
    fn declines_scalar_leaf_type_mismatch() {
        let bytes = fixture();
        // A reference field that does NOT point at the claimed carrier index is
        // not the Int carrier leaf: the record decodes to nothing (fail-closed),
        // so the equality pin can never ride a doppelganger carrier.
        assert_eq!(record_leaves_from_bytes(&bytes, 5, 1), None);
    }

    #[test]
    fn declines_mutable_field() {
        let bytes = fixture();
        // Records lower to immutable fields; a mutable storage never matches.
        assert_eq!(record_leaves_from_bytes(&bytes, 0, 3), None);
    }
}

fn check_sym_fragment_plan_object(
    wasm_bytes: &[u8],
    export_name: &str,
    sym_plan: SymPlan,
) -> Result<(usize, Cert, bool, Option<String>), String> {
    let (user_fns, _box_idx, _user_idx_set, carrier, _host_roles, host_table, struct_field_counts) =
        disassemble(wasm_bytes)?;
    let (_func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("source plan names unknown export `{export_name}`"))?;
    // Zero-arity exports are legitimate compute-face targets (constant
    // constructors); the per-face parameter checks and the exact nominal
    // signature pin keep every other family fail-closed.
    if !frag_calls_resolvable(&f.calls, &host_table) {
        return Err(format!(
            "source plan for `{export_name}` does not target a non-recursive expr fragment"
        ));
    }
    let carrier = carrier.ok_or_else(|| {
        format!("source plan for `{export_name}` needs the Int carrier type from the wasm module")
    })?;
    let frag_params = f
        .params
        .iter()
        .map(|ty| expr_fragment_ty_from_wasm_param(ty, carrier))
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| {
            format!("source plan for `{export_name}` has unsupported wasm parameter types")
        })?;
    let frag_result = expr_fragment_ty_from_wasm_result(
        f.result.ok_or_else(|| {
            format!("source plan for `{export_name}` targets a function with no result")
        })?,
        carrier,
    )
    .ok_or_else(|| format!("source plan for `{export_name}` has unsupported wasm result type"))?;
    // Encode-compatibility: each declared source type must ENCODE to the
    // byte-derived wasm representation type at its position. Scalars
    // round-trip exactly; `AdtRef` positions adopt the declared String/Named
    // source type (bytes cannot name it) — the byte-exact gate then pins the
    // adoption through the encoded plan.
    let declared_params = sym_plan
        .params
        .iter()
        .map(SymTy::to_frag_ty)
        .collect::<Option<Vec<_>>>();
    if declared_params.as_deref() != Some(frag_params.as_slice()) {
        return Err(format!(
            "source plan for `{export_name}` has params {:?}, but the wasm signature requires source types encoding to {frag_params:?}",
            sym_plan
                .params
                .iter()
                .map(SymTy::plan_tag)
                .collect::<Vec<_>>()
        ));
    }
    if sym_plan.result.to_frag_ty() != Some(frag_result) {
        return Err(format!(
            "source plan for `{export_name}` has result `{}`, but the wasm signature requires a source type encoding to {frag_result:?}",
            sym_plan.result.plan_tag()
        ));
    }
    if sym_plan.body.result_ty() != Some(sym_plan.result.clone()) {
        return Err(format!(
            "source plan for `{export_name}` root type does not match function result type"
        ));
    }
    // Source-level type names carry the model trust story (see
    // docs/certification.md "Read surface"): they are not byte-derivable, but
    // they must be internally consistent — every used name anchored by a
    // projection, every projection owner matching its value's declared type.
    check_sym_plan_named_consistency(&sym_plan)
        .map_err(|e| format!("source plan for `{export_name}`: {e}"))?;
    // Struct bindings are byte-derived per export (the export's own unique
    // non-carrier struct.get), never taken from the plan; encoding under
    // this table plus canonical byte equality pins the pairing.
    let struct_table = byte_derived_frag_struct_table(&sym_plan, f, carrier, &struct_field_counts)
        .map_err(|e| format!("source plan for `{export_name}`: {e}"))?;
    let plan = sym_plan
        .to_expr_fragment_plan(&host_table, &struct_table)
        .ok_or_else(|| {
            format!("source plan for `{export_name}` cannot be encoded to expr-fragment-v1")
        })?;
    let (func_order, mut cert, canonical_matches_actual, mismatch_reason) =
        check_expr_fragment_plan_object(wasm_bytes, export_name, plan)?;
    let Cert::ExprFragment {
        source_plan,
        record_decl,
        record_compute,
        plan,
        carrier,
        ..
    } = &mut cert
    else {
        unreachable!("expr-fragment plan checker must return an expr-fragment cert")
    };
    *source_plan = Some(sym_plan.clone());
    // Stage-1 record scalar field read: derive the record's ordered scalar-leaf
    // declaration from the module type section at the projected struct index, so
    // the wall pins the whole declaration by equality against those same bytes.
    // A recognized record projection whose struct is not a flat scalar record
    // declines fail-closed here rather than rendering a record face the wall
    // equality pin would refuse.
    if let Some(face) = expr_fragment_record_proj_face(plan) {
        match record_leaves_from_bytes(wasm_bytes, *carrier, face.struct_idx) {
            Some(leaves) => *record_decl = Some((face.struct_idx, leaves)),
            None => {
                return Err(format!(
                    "source plan for `{export_name}` reads a record field, but struct index {} \
                     does not decode to a flat scalar record",
                    face.struct_idx
                ));
            }
        }
    }
    // Record projection-compute face (v1): recognized only when the pinned
    // struct decodes to a flat ALL-Int record; otherwise the export simply
    // stays on the source-level-only route (no error — the face is optional).
    if record_compute.is_none()
        && let Some(face) = expr_fragment_record_compute_face(plan, &host_table)
    {
        if expr_fragment_plan_uses_struct(plan) {
            // A record-shaped plan carries its declaration: the wall pins the
            // type-section entry at the cited index by equality against it.
            if let Some(leaves) = record_leaves_from_bytes(wasm_bytes, *carrier, face.struct_idx)
                && leaves.iter().all(|l| matches!(l, RecordLeaf::IntCarrier))
            {
                *record_decl = Some((face.struct_idx, leaves));
                *record_compute = Some(face);
            }
        } else {
            // A scalar-parameter plan names no record at all; the face's
            // reserved index `0` reads no type-section entry, and the wall's
            // declared face demands none.
            *record_compute = Some(face);
        }
    }
    if record_compute.is_none() && plan_contains_struct_new(&plan.body) {
        return Err(format!(
            "source plan for `{export_name}` constructs a struct outside the \
             compute face (its record is not a flat all-Int declaration)"
        ));
    }
    // Same fail-closed shape for the inline sign template: the compute face is
    // the only face that interprets it, and the generic renderers have no arm
    // for it.
    if record_compute.is_none() && plan_contains_int_sign_cmp(&plan.body) {
        return Err(format!(
            "source plan for `{export_name}` compares a computed Int against a \
             literal outside the compute face"
        ));
    }
    Ok((func_order, cert, canonical_matches_actual, mismatch_reason))
}

fn byte_match_summary(label: &str, expected: &[u8], actual: &[u8]) -> String {
    if expected == actual {
        return format!("{label}_match=true, len={}", actual.len());
    }
    let first_diff = expected
        .iter()
        .zip(actual)
        .position(|(expected, actual)| expected != actual);
    match first_diff {
        Some(idx) => format!(
            "{label}_match=false, expected_len={}, actual_len={}, first_diff={} expected=0x{:02x} actual=0x{:02x}",
            expected.len(),
            actual.len(),
            idx,
            expected[idx],
            actual[idx]
        ),
        None => format!(
            "{label}_match=false, expected_len={}, actual_len={}, first_diff=len",
            expected.len(),
            actual.len()
        ),
    }
}

fn plan_contains_int_sign_cmp(block: &FragBlock) -> bool {
    block.nodes.iter().any(|n| match &n.kind {
        FragNodeKind::IntSignCmp { .. } => true,
        FragNodeKind::If {
            then_block,
            else_block,
            ..
        } => {
            plan_contains_int_sign_cmp(then_block)
                || plan_contains_int_sign_cmp(else_block)
        }
        _ => false,
    })
}

fn plan_contains_struct_new(block: &FragBlock) -> bool {
    block.nodes.iter().any(|n| match &n.kind {
        FragNodeKind::StructNew { .. } => true,
        FragNodeKind::If {
            then_block,
            else_block,
            ..
        } => {
            plan_contains_struct_new(then_block)
                || plan_contains_struct_new(else_block)
        }
        _ => false,
    })
}
