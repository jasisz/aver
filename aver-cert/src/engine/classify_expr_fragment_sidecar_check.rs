pub struct FragmentPlanCheck {
    pub sidecar: FragmentPlanSidecar,
    pub obligation: RederivedObligation,
    pub canonical_matches_actual: bool,
    pub mismatch_reason: Option<String>,
    /// Named runtime contracts this checked plan's obligation consumes (the
    /// box/add wiring of the straight-line integer face; empty for host-free
    /// fragments). The verifier merges these with the byte-derived legacy
    /// contract list, since plan-covered exports are excluded from legacy
    /// classification.
    pub runtime_contracts: Vec<String>,
}

pub fn check_expr_fragment_plan_sidecar(
    wasm_bytes: &[u8],
    export_name: &str,
    plan_text: &str,
) -> Result<FragmentPlanCheck, String> {
    let (user_fns, _box_idx, _user_idx_set, carrier, _host_roles, host_table, _struct_field_counts) =
        disassemble(wasm_bytes)?;
    let (_func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("plan names unknown export `{export_name}`"))?;
    if f.arity == 0 || !frag_calls_resolvable(&f.calls, &host_table) {
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
    let mut parser = FragPlanParser::new(plan_text, params.clone(), result);
    let body = parser.parse()?;
    let plan = ExprFragmentPlan {
        params,
        result,
        body,
    };
    let plan_lean = expr_fragment_plan_lean_value(&plan);
    let sym_plan = SymPlan::from_expr_fragment_source_subset(&plan);
    let sym_plan_lean = sym_plan.as_ref().map(sym_plan_lean_value);
    let sym_plan_sidecar = sym_plan
        .as_ref()
        .map(|sym| sym_fragment_sidecar(export_name, sym));
    let (func_order, cert, sidecar, canonical_matches_actual, mismatch_reason) =
        check_expr_fragment_plan_object(wasm_bytes, export_name, plan)?;
    let obligation = RederivedObligation {
        name: export_name.to_string(),
        func_order,
        code: render_code_value(&cert),
        self_idx: cert.self_idx(),
        carrier: cert.carrier(),
        policy: CertificationPolicy::SimulatesModel,
        termination_witness: None,
        face: ObligationFace::of_cert(&cert),
        fragment_type_idx: match cert.inner() {
            Cert::ExprFragment { type_idx, .. } => Some(*type_idx),
            _ => None,
        },
        fragment_nlocals: match cert.inner() {
            Cert::ExprFragment { nlocals, .. } => Some(*nlocals as u32),
            _ => None,
        },
        fragment_plan: Some(sidecar.clone()),
        fragment_plan_lean: Some(plan_lean),
        fragment_sym_plan: sym_plan_sidecar,
        fragment_sym_plan_lean: sym_plan_lean,
        fragment_struct_entries: Vec::new(),
        fragment_lowered_body_lean: match cert.inner() {
            Cert::ExprFragment { ops, .. } => Some(render_ops_value(ops)),
            _ => None,
        },
        fragment_lowered_code_entry_lean: match cert.inner() {
            Cert::ExprFragment { carrier, plan, .. } => {
                lower_expr_fragment_plan_code_entry_bytes(plan, *carrier)
                    .ok()
                    .map(|bytes| render_byte_list(&bytes))
            }
            _ => None,
        },
        string_concat_plan: None,
        string_concat_sym_plan: None,
        string_concat_plan_lean: None,
        string_concat_sym_plan_lean: None,
        string_concat_type_idx: None,
        string_concat_lowered_body_lean: None,
        string_concat_lowered_code_entry_lean: None,
        string_concat_result_ty: None,
        string_concat_container_ty: None,
        string_concat_func_idx: None,
        string_eq_plan: None,
        string_eq_sym_plan: None,
        string_eq_plan_lean: None,
        string_eq_sym_plan_lean: None,
        string_eq_type_idx: None,
        string_eq_lowered_body_lean: None,
        string_eq_lowered_code_entry_lean: None,
        string_eq_string_ty: None,
        string_eq_func_idx: None,
        construct_plan: None,
        construct_sym_plan: None,
        construct_plan_lean: None,
        construct_sym_plan_lean: None,
        construct_type_idx: None,
        construct_struct_idx: None,
        construct_field_count: None,
        construct_elem_ty_lean: None,
        construct_is_list: false,
        construct_lowered_body_lean: None,
        construct_lowered_code_entry_lean: None,
        recursion_plan_lean: None,
        recursion_host_table_lean: None,
        recursion_type_idx: None,
        recursion_lowered_body_lean: None,
        recursion_lowered_code_entry_lean: None,
        mutual_plan_lean: None,
        mutual_host_table_lean: None,
        mutual_member_set_lean: None,
        mutual_type_idx: None,
        mutual_lowered_body_lean: None,
        mutual_lowered_code_entry_lean: None,
        verbatim_plan_lean: None,
        int_dispatch_plan_lean: None,
        int_dispatch_host_table_lean: None,
        field_projection_plan_lean: None,
        field_projection_struct_idx: None,
        field_projection_field_count: None,
        field_projection_result_ty_lean: None,
        composition_members: Vec::new(),
        composition_host_table_lean: None,
        composition_member_names_lean: None,
    };
    Ok(FragmentPlanCheck {
        sidecar,
        obligation,
        canonical_matches_actual,
        mismatch_reason,
        runtime_contracts: runtime_contracts_for_certs(std::iter::once(&cert)),
    })
}

pub fn check_sym_fragment_plan_sidecar(
    wasm_bytes: &[u8],
    export_name: &str,
    plan_text: &str,
) -> Result<FragmentPlanCheck, String> {
    let (user_fns, _box_idx, _user_idx_set, carrier, _host_roles, host_table, _struct_field_counts) =
        disassemble(wasm_bytes)?;
    let (_func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("source plan names unknown export `{export_name}`"))?;
    if f.arity == 0 || !frag_calls_resolvable(&f.calls, &host_table) {
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
    let (params, result) = sym_sidecar_declared_tys(plan_text, &frag_params, frag_result)
        .map_err(|e| format!("source plan for `{export_name}` {e}"))?;
    let mut parser = SymPlanParser::new(plan_text, params.clone(), result.clone());
    let body = parser.parse()?;
    let sym_plan = SymPlan {
        params,
        result,
        body,
    };
    let (func_order, cert, sidecar, canonical_matches_actual, mismatch_reason) =
        check_sym_fragment_plan_object(wasm_bytes, export_name, sym_plan.clone())?;
    let Cert::ExprFragment { plan, .. } = cert.inner() else {
        unreachable!("sym-fragment plan checker must return an expr-fragment cert")
    };
    let plan_lean = expr_fragment_plan_lean_value(plan);
    let sym_plan_lean = sym_plan_lean_value(&sym_plan);
    let expr_sidecar = expr_fragment_sidecar(export_name, plan);
    let fragment_struct_entries =
        expr_fragment_struct_table_entries(&sym_plan, plan).unwrap_or_default();
    let obligation = RederivedObligation {
        name: export_name.to_string(),
        func_order,
        code: render_code_value(&cert),
        self_idx: cert.self_idx(),
        carrier: cert.carrier(),
        policy: CertificationPolicy::SimulatesModel,
        termination_witness: None,
        face: ObligationFace::of_cert(&cert),
        fragment_type_idx: match cert.inner() {
            Cert::ExprFragment { type_idx, .. } => Some(*type_idx),
            _ => None,
        },
        fragment_nlocals: match cert.inner() {
            Cert::ExprFragment { nlocals, .. } => Some(*nlocals as u32),
            _ => None,
        },
        fragment_plan: Some(expr_sidecar),
        fragment_plan_lean: Some(plan_lean),
        fragment_sym_plan: Some(sidecar.clone()),
        fragment_sym_plan_lean: Some(sym_plan_lean),
        fragment_struct_entries,
        fragment_lowered_body_lean: match cert.inner() {
            Cert::ExprFragment { ops, .. } => Some(render_ops_value(ops)),
            _ => None,
        },
        fragment_lowered_code_entry_lean: match cert.inner() {
            Cert::ExprFragment { carrier, plan, .. } => {
                lower_expr_fragment_plan_code_entry_bytes(plan, *carrier)
                    .ok()
                    .map(|bytes| render_byte_list(&bytes))
            }
            _ => None,
        },
        string_concat_plan: None,
        string_concat_sym_plan: None,
        string_concat_plan_lean: None,
        string_concat_sym_plan_lean: None,
        string_concat_type_idx: None,
        string_concat_lowered_body_lean: None,
        string_concat_lowered_code_entry_lean: None,
        string_concat_result_ty: None,
        string_concat_container_ty: None,
        string_concat_func_idx: None,
        string_eq_plan: None,
        string_eq_sym_plan: None,
        string_eq_plan_lean: None,
        string_eq_sym_plan_lean: None,
        string_eq_type_idx: None,
        string_eq_lowered_body_lean: None,
        string_eq_lowered_code_entry_lean: None,
        string_eq_string_ty: None,
        string_eq_func_idx: None,
        construct_plan: None,
        construct_sym_plan: None,
        construct_plan_lean: None,
        construct_sym_plan_lean: None,
        construct_type_idx: None,
        construct_struct_idx: None,
        construct_field_count: None,
        construct_elem_ty_lean: None,
        construct_is_list: false,
        construct_lowered_body_lean: None,
        construct_lowered_code_entry_lean: None,
        recursion_plan_lean: None,
        recursion_host_table_lean: None,
        recursion_type_idx: None,
        recursion_lowered_body_lean: None,
        recursion_lowered_code_entry_lean: None,
        mutual_plan_lean: None,
        mutual_host_table_lean: None,
        mutual_member_set_lean: None,
        mutual_type_idx: None,
        mutual_lowered_body_lean: None,
        mutual_lowered_code_entry_lean: None,
        verbatim_plan_lean: None,
        int_dispatch_plan_lean: None,
        int_dispatch_host_table_lean: None,
        field_projection_plan_lean: None,
        field_projection_struct_idx: None,
        field_projection_field_count: None,
        field_projection_result_ty_lean: None,
        composition_members: Vec::new(),
        composition_host_table_lean: None,
        composition_member_names_lean: None,
    };
    Ok(FragmentPlanCheck {
        sidecar,
        obligation,
        canonical_matches_actual,
        mismatch_reason,
        runtime_contracts: runtime_contracts_for_certs(std::iter::once(&cert)),
    })
}

fn check_expr_fragment_plan_object(
    wasm_bytes: &[u8],
    export_name: &str,
    plan: ExprFragmentPlan,
) -> Result<(usize, Cert, FragmentPlanSidecar, bool, Option<String>), String> {
    let (user_fns, _box_idx, _user_idx_set, carrier, _host_roles, host_table, struct_field_counts) =
        disassemble(wasm_bytes)?;
    let (func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("plan names unknown export `{export_name}`"))?;
    if f.arity == 0 || !frag_calls_resolvable(&f.calls, &host_table) {
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
    // the byte-derived index for its role. The only fragment shape with a
    // rendered proof face over the Int carrier is the straight-line
    // `add(param0, box(k))` integer face — any other carrier-returning plan
    // (a bare Int passthrough, a host-call chain) has no coherent `Cod`/
    // `codRepr` and is declined here, mirroring the producer gate.
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
    if (plan_has_host_calls(&plan.body) || plan.result == FragTy::IntCarrier)
        && expr_fragment_int_add_face(&plan).is_none()
        && !tag_dispatch
        && !vector_get
        && !record_proj
    {
        return Err(format!(
            "plan for `{export_name}` has no rendered proof face: Int-carrier results \
             are supported only through the straight-line integer, tag-dispatch, \
             fused vector-read, or record field-read face"
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
        plan: plan.clone(),
        ops: canonical_ops,
    };
    let sidecar = expr_fragment_sidecar(export_name, &plan);
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
    Ok((
        func_order,
        cert,
        sidecar,
        ops_match && bytes_match,
        mismatch_reason,
    ))
}

fn expr_fragment_needs_relational_nan_result(plan: &ExprFragmentPlan) -> bool {
    plan.result == FragTy::F64 && block_has_nan_nondeterministic_float_op(&plan.body)
}

fn block_has_nan_nondeterministic_float_op(block: &FragBlock) -> bool {
    // Deliberately exhaustive: extending FragNodeKind must force an explicit
    // decision about nested blocks and Float-bit observation at this gate.
    block.nodes.iter().any(|node| match &node.kind {
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
        | FragNodeKind::VectorGetOrDefault { .. } => false,
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
) -> Result<(usize, Cert, FragmentPlanSidecar, bool, Option<String>), String> {
    let (user_fns, _box_idx, _user_idx_set, carrier, _host_roles, host_table, struct_field_counts) =
        disassemble(wasm_bytes)?;
    let (_func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("source plan names unknown export `{export_name}`"))?;
    if f.arity == 0 || !frag_calls_resolvable(&f.calls, &host_table) {
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
            sym_ty_tags(&sym_plan.params)
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
    // non-carrier struct.get), never taken from the sidecar; encoding under
    // this table plus canonical byte equality pins the pairing.
    let struct_table = byte_derived_frag_struct_table(&sym_plan, f, carrier, &struct_field_counts)
        .map_err(|e| format!("source plan for `{export_name}`: {e}"))?;
    let plan = sym_plan
        .to_expr_fragment_plan(&host_table, &struct_table)
        .ok_or_else(|| {
            format!("source plan for `{export_name}` cannot be encoded to expr-fragment-v1")
        })?;
    let (func_order, mut cert, _expr_sidecar, canonical_matches_actual, mismatch_reason) =
        check_expr_fragment_plan_object(wasm_bytes, export_name, plan)?;
    let Cert::ExprFragment {
        source_plan,
        record_decl,
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
    let sidecar = sym_fragment_sidecar(export_name, &sym_plan);
    Ok((
        func_order,
        cert,
        sidecar,
        canonical_matches_actual,
        mismatch_reason,
    ))
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

/// Read the self-declared `params`/`result` lines of a sym sidecar and verify
/// each declared source type ENCODES to the byte-derived wasm representation
/// type at its position. Scalar positions must round-trip exactly; `AdtRef`
/// positions adopt the declared String/Named source type (bytes cannot name
/// it), which the byte-exact gate then pins through the encoded plan.
fn sym_sidecar_declared_tys(
    plan_text: &str,
    frag_params: &[FragTy],
    frag_result: FragTy,
) -> Result<(Vec<SymTy>, SymTy), String> {
    let mut lines = plan_text.lines();
    let (Some(_header), Some(_profile), Some(params_line), Some(result_line)) =
        (lines.next(), lines.next(), lines.next(), lines.next())
    else {
        return Err("is truncated before its params/result lines".to_string());
    };
    let params = parse_sym_plan_params(params_line.trim())?;
    let result = result_line
        .trim()
        .strip_prefix("result ")
        .and_then(SymTy::from_plan_tag)
        .ok_or_else(|| format!("has a malformed result line `{}`", result_line.trim()))?;
    let declared = params
        .iter()
        .map(SymTy::to_frag_ty)
        .collect::<Option<Vec<_>>>();
    if declared.as_deref() != Some(frag_params) {
        return Err(format!(
            "declares params {:?}, but the wasm signature requires source types encoding to {frag_params:?}",
            sym_ty_tags(&params)
        ));
    }
    if result.to_frag_ty() != Some(frag_result) {
        return Err(format!(
            "declares result `{}`, but the wasm signature requires a source type encoding to {frag_result:?}",
            result.plan_tag()
        ));
    }
    Ok((params, result))
}
