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
    if (plan_has_host_calls(&plan.body) || plan.result == FragTy::IntCarrier)
        && expr_fragment_int_add_face(&plan).is_none()
    {
        return Err(format!(
            "plan for `{export_name}` has no rendered proof face: Int-carrier results \
             are supported only through the straight-line integer face"
        ));
    }
    // Face-gated AdtRef admission (the FIX-1 pattern): plans touching opaque
    // user-ADT references are accepted ONLY as the exact field-projection
    // face; anything else declines fail-closed on producer and verifier alike.
    if expr_fragment_plan_touches_adt_ref(&plan) && expr_fragment_project_face(&plan).is_none() {
        return Err(format!(
            "plan for `{export_name}` has no rendered proof face: user-ADT references \
             are supported only through the field-projection face"
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
    let Cert::ExprFragment { source_plan, .. } = &mut cert else {
        unreachable!("expr-fragment plan checker must return an expr-fragment cert")
    };
    *source_plan = Some(sym_plan.clone());
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
