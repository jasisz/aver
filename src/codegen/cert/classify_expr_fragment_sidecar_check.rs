pub struct FragmentPlanCheck {
    pub sidecar: FragmentPlanSidecar,
    pub obligation: RederivedObligation,
    pub canonical_matches_actual: bool,
    pub mismatch_reason: Option<String>,
}

pub fn check_expr_fragment_plan_sidecar(
    wasm_bytes: &[u8],
    export_name: &str,
    plan_text: &str,
) -> Result<FragmentPlanCheck, String> {
    let (user_fns, _box_idx, _user_idx_set, carrier, _host_roles) = disassemble(wasm_bytes)?;
    let (_func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("plan names unknown export `{export_name}`"))?;
    if f.arity == 0 || !f.calls.is_empty() {
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
    let sym_plan_lean =
        SymPlan::from_expr_fragment_source_subset(&plan).map(|sym| sym_plan_lean_value(&sym));
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
    };
    Ok(FragmentPlanCheck {
        sidecar,
        obligation,
        canonical_matches_actual,
        mismatch_reason,
    })
}

pub fn check_sym_fragment_plan_sidecar(
    wasm_bytes: &[u8],
    export_name: &str,
    plan_text: &str,
) -> Result<FragmentPlanCheck, String> {
    let (user_fns, _box_idx, _user_idx_set, carrier, _host_roles) = disassemble(wasm_bytes)?;
    let (_func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("source plan names unknown export `{export_name}`"))?;
    if f.arity == 0 || !f.calls.is_empty() {
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
    let mut parser = SymPlanParser::new(plan_text, params.clone(), result);
    let body = parser.parse()?;
    let sym_plan = SymPlan {
        params,
        result,
        body,
    };
    let plan = sym_plan.to_expr_fragment_plan().ok_or_else(|| {
        format!("source plan for `{export_name}` cannot be encoded to expr-fragment-v1")
    })?;
    let plan_lean = expr_fragment_plan_lean_value(&plan);
    let sym_plan_lean = sym_plan_lean_value(&sym_plan);
    let (func_order, cert, _expr_sidecar, canonical_matches_actual, mismatch_reason) =
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
        fragment_plan: Some(sidecar.clone()),
        fragment_plan_lean: Some(plan_lean),
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
    };
    Ok(FragmentPlanCheck {
        sidecar,
        obligation,
        canonical_matches_actual,
        mismatch_reason,
    })
}

fn check_expr_fragment_plan_object(
    wasm_bytes: &[u8],
    export_name: &str,
    plan: ExprFragmentPlan,
) -> Result<(usize, Cert, FragmentPlanSidecar, bool, Option<String>), String> {
    let (user_fns, _box_idx, _user_idx_set, carrier, _host_roles) = disassemble(wasm_bytes)?;
    let (func_order, f) = user_fns
        .iter()
        .enumerate()
        .find(|(_, f)| f.name == export_name)
        .ok_or_else(|| format!("plan names unknown export `{export_name}`"))?;
    if f.arity == 0 || !f.calls.is_empty() {
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
    )
    .ok_or_else(|| format!("plan for `{export_name}` has unsupported wasm result type"))?;
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
