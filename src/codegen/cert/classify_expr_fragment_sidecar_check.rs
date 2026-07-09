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
    let (user_fns, box_idx, _user_idx_set, carrier, host_roles) = disassemble(wasm_bytes)?;
    let (_func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("plan names unknown export `{export_name}`"))?;
    let host_table = frag_host_table_from_disasm(box_idx, &host_roles);
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
        host: render_host_value(&cert),
        self_idx: cert.self_idx(),
        carrier: cert.carrier(),
        face: ObligationFace::of_cert(&cert),
        fragment_code_idx: match cert.inner() {
            Cert::ExprFragment { code_idx, .. } => Some(*code_idx),
            _ => None,
        },
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
        string_concat_code_idx: None,
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
        string_eq_code_idx: None,
        string_eq_type_idx: None,
        string_eq_lowered_body_lean: None,
        string_eq_lowered_code_entry_lean: None,
        string_eq_string_ty: None,
        string_eq_func_idx: None,
        construct_plan: None,
        construct_sym_plan: None,
        construct_plan_lean: None,
        construct_sym_plan_lean: None,
        construct_code_idx: None,
        construct_type_idx: None,
        construct_lowered_body_lean: None,
        construct_lowered_code_entry_lean: None,
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
    let (user_fns, box_idx, _user_idx_set, carrier, host_roles) = disassemble(wasm_bytes)?;
    let (_func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("source plan names unknown export `{export_name}`"))?;
    let host_table = frag_host_table_from_disasm(box_idx, &host_roles);
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
    let params = frag_params
        .iter()
        .copied()
        .map(SymTy::from_frag_ty)
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| {
            format!(
                "source plan for `{export_name}` cannot describe one of the wasm representation parameters"
            )
        })?;
    let result = SymTy::from_frag_ty(frag_result).ok_or_else(|| {
        format!(
            "source plan for `{export_name}` cannot describe the wasm representation result"
        )
    })?;
    let mut parser = SymPlanParser::new(plan_text, params.clone(), result.clone());
    let body = parser.parse()?;
    let sym_plan = SymPlan {
        params,
        result,
        body,
    };
    let plan = sym_plan.to_expr_fragment_plan(&host_table).ok_or_else(|| {
        format!("source plan for `{export_name}` cannot be encoded to expr-fragment-v1")
    })?;
    let plan_lean = expr_fragment_plan_lean_value(&plan);
    let sym_plan_lean = sym_plan_lean_value(&sym_plan);
    let (func_order, cert, expr_sidecar, canonical_matches_actual, mismatch_reason) =
        check_expr_fragment_plan_object(wasm_bytes, export_name, plan)?;
    let sidecar = sym_fragment_sidecar(export_name, &sym_plan);
    let obligation = RederivedObligation {
        name: export_name.to_string(),
        func_order,
        code: render_code_value(&cert),
        host: render_host_value(&cert),
        self_idx: cert.self_idx(),
        carrier: cert.carrier(),
        face: ObligationFace::of_cert(&cert),
        fragment_code_idx: match cert.inner() {
            Cert::ExprFragment { code_idx, .. } => Some(*code_idx),
            _ => None,
        },
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
        string_concat_code_idx: None,
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
        string_eq_code_idx: None,
        string_eq_type_idx: None,
        string_eq_lowered_body_lean: None,
        string_eq_lowered_code_entry_lean: None,
        string_eq_string_ty: None,
        string_eq_func_idx: None,
        construct_plan: None,
        construct_sym_plan: None,
        construct_plan_lean: None,
        construct_sym_plan_lean: None,
        construct_code_idx: None,
        construct_type_idx: None,
        construct_lowered_body_lean: None,
        construct_lowered_code_entry_lean: None,
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
    let (user_fns, box_idx, _user_idx_set, carrier, host_roles) = disassemble(wasm_bytes)?;
    let (func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("plan names unknown export `{export_name}`"))?;
    let host_table = frag_host_table_from_disasm(box_idx, &host_roles);
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
    // the byte-derived index for its role, and the only host-call fragment
    // shape with a rendered proof face today is the straight-line
    // `add(param0, box(k))` integer face.
    check_plan_host_calls(&plan.body, &host_table)
        .map_err(|e| format!("plan for `{export_name}`: {e}"))?;
    if plan_has_host_calls(&plan.body) && expr_fragment_int_add_face(&plan).is_none() {
        return Err(format!(
            "plan for `{export_name}` uses host calls outside the supported \
             straight-line integer face"
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
        code_idx: f.code_idx,
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
    let (user_fns, box_idx, _user_idx_set, carrier, host_roles) = disassemble(wasm_bytes)?;
    let (_func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("source plan names unknown export `{export_name}`"))?;
    let host_table = frag_host_table_from_disasm(box_idx, &host_roles);
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
    let params = frag_params
        .iter()
        .copied()
        .map(SymTy::from_frag_ty)
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| {
            format!(
                "source plan for `{export_name}` cannot describe one of the wasm representation parameters"
            )
        })?;
    let result = SymTy::from_frag_ty(frag_result).ok_or_else(|| {
        format!(
            "source plan for `{export_name}` cannot describe the wasm representation result"
        )
    })?;
    if sym_plan.params != params {
        return Err(format!(
            "source plan for `{export_name}` has params {:?}, but wasm signature requires {:?}",
            sym_ty_tags(&sym_plan.params),
            sym_ty_tags(&params)
        ));
    }
    if sym_plan.result != result {
        return Err(format!(
            "source plan for `{export_name}` has result `{}`, but wasm signature requires `{}`",
            sym_plan.result.plan_tag(),
            result.plan_tag()
        ));
    }
    if sym_plan.body.result_ty() != Some(result.clone()) {
        return Err(format!(
            "source plan for `{export_name}` root type does not match function result type"
        ));
    }
    let plan = sym_plan.to_expr_fragment_plan(&host_table).ok_or_else(|| {
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
