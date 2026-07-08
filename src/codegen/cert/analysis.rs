/// Classification of every user function in the module.
pub struct Analysis {
    certs: Vec<Cert>,
    declined: Vec<(String, String)>,
    carrier: Option<u32>,
    contracts: Vec<String>,
}

impl Analysis {
    pub fn certified_names(&self) -> Vec<String> {
        self.certs.iter().map(|c| c.name().to_string()).collect()
    }
    pub fn declined(&self) -> &[(String, String)] {
        &self.declined
    }
}

/// Disassemble the emitted module and classify each user function. `model_files`
/// are the reused `aver proof` Lean model; the recursion classifier reads the
/// combinator operator (`+`/`*`) from them since the bytes cannot tell the bignum
/// helpers apart.
pub fn analyze(wasm_bytes: &[u8], model_files: &[(String, String)]) -> Result<Analysis, String> {
    analyze_with_expr_fragment_plans(wasm_bytes, model_files, &[])
}

pub fn analyze_with_expr_fragment_plans(
    wasm_bytes: &[u8],
    model_files: &[(String, String)],
    expr_fragment_plans: &[ExprFragmentPlanArtifact],
) -> Result<Analysis, String> {
    let (user_fns, box_idx, user_idx_set, carrier, host_roles) = disassemble(wasm_bytes)?;
    let model_ops = model_step_ops(model_files);

    // Index the user functions so the composition pass can walk the call graph.
    let fns: std::collections::HashMap<u32, &UserFn> =
        user_fns.iter().map(|f| (f.wasm_idx, f)).collect();
    let user_names: std::collections::HashSet<&str> =
        user_fns.iter().map(|f| f.name.as_str()).collect();
    let mut producer_plans = std::collections::HashMap::<&str, &ExprFragmentPlan>::new();
    for artifact in expr_fragment_plans {
        if !user_names.contains(artifact.export_name.as_str()) {
            return Err(format!(
                "producer supplied expr-fragment plan for unknown export `{}`",
                artifact.export_name
            ));
        }
        if producer_plans
            .insert(artifact.export_name.as_str(), &artifact.plan)
            .is_some()
        {
            return Err(format!(
                "producer supplied duplicate expr-fragment plan for `{}`",
                artifact.export_name
            ));
        }
    }

    let mut certs = Vec::new();
    let mut declined = Vec::new();
    for f in &user_fns {
        if let Some(plan) = producer_plans.get(f.name.as_str()) {
            match check_expr_fragment_plan_object(wasm_bytes, &f.name, (*plan).clone()) {
                Ok((_func_order, cert, _sidecar, true, _reason)) => certs.push(cert),
                Ok((_func_order, _cert, _sidecar, false, reason)) => declined.push((
                    f.name.clone(),
                    format!(
                        "producer expr-fragment plan does not match emitted wasm: {}",
                        reason.unwrap_or_else(|| "unknown mismatch".to_string())
                    ),
                )),
                Err(reason) => declined.push((
                    f.name.clone(),
                    format!("producer expr-fragment plan rejected: {reason}"),
                )),
            }
            continue;
        }
        match classify_without_expr_fragment(
            f,
            box_idx,
            carrier,
            &user_idx_set,
            &fns,
            &host_roles,
            &model_ops,
        ) {
            Ok(c) => certs.push(c),
            Err(reason) => declined.push((f.name.clone(), reason)),
        }
    }

    // Named runtime contracts actually consumed by the certified functions.
    let contracts = runtime_contracts_for_certs(&certs);

    Ok(Analysis {
        certs,
        declined,
        carrier,
        contracts,
    })
}

fn runtime_contracts_for_certs<'a>(certs: impl IntoIterator<Item = &'a Cert>) -> Vec<String> {
    let mut contracts = Vec::new();
    let mut has_box = false;
    let mut has_add = false;
    let mut has_sub = false;
    let mut has_string_eq = false;
    let mut has_string_concat = false;
    for c in certs {
        match c.inner() {
            Cert::StraightLine { .. } => {
                has_box = true;
                has_add = true;
            }
            Cert::Recursive { .. } => {
                has_box = true;
                has_add = true;
                has_sub = true;
            }
            Cert::AccumulatorRecursive { .. } => {
                has_box = true;
                has_add = true;
                has_sub = true;
            }
            Cert::AdtConstructor { .. }
            | Cert::FieldProjection { .. }
            | Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::ExprFragment { .. } => {}
            Cert::StringEqVerbatimMatch { .. } => {
                has_string_eq = true;
            }
            Cert::StringConcatVerbatimMatch { .. } => {
                has_string_concat = true;
            }
            Cert::MutualRecursion { .. } => {
                // The shared host wires box + sub (no combinator).
                has_box = true;
                has_sub = true;
            }
            Cert::WidenedIntMatch { .. } => {
                has_box = true;
            }
            Cert::VariantDispatch {
                add_idx, sub_idx, ..
            } => {
                has_box = true;
                has_add |= add_idx.is_some();
                has_sub |= sub_idx.is_some();
            }
            Cert::Composition {
                has_add: a,
                has_sub: s,
                has_box: b,
                ..
            } => {
                has_add |= *a;
                has_sub |= *s;
                has_box |= *b;
            }
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    if has_box {
        contracts.push(BOX_CONTRACT.to_string());
    }
    if has_add {
        contracts.push(INT_ADD_CONTRACT.to_string());
    }
    if has_sub {
        contracts.push(INT_SUB_CONTRACT.to_string());
    }
    if has_string_eq {
        contracts.push(STRING_EQ_CONTRACT.to_string());
    }
    if has_string_concat {
        contracts.push(STRING_CONCAT_CONTRACT.to_string());
    }
    contracts
}

#[cfg(all(test, feature = "wasm-compile"))]
mod analysis_tests {
    use super::*;

    fn compile_float_add_probe() -> crate::codegen::wasm_gc::WasmGcCompileOutput {
        let mut items = crate::source::parse_source(
            r#"
module PlanFirstProbe
    intent = "plan-first producer overlay probe"
    depends []
    exposes [floatAddGoal]

fn floatAddGoal(a: Float, b: Float) -> Float
    ? "Small scalar island."
    a + b
"#,
        )
        .expect("source parses");
        let pipeline = crate::ir::pipeline::run(
            &mut items,
            crate::ir::PipelineConfig {
                typecheck: Some(crate::ir::TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );
        assert!(
            pipeline
                .typecheck
                .as_ref()
                .is_none_or(|tc| tc.errors.is_empty()),
            "probe source should typecheck"
        );
        crate::codegen::wasm_gc::compile_to_wasm_gc_with_handler_and_cert_plans(
            &items, None, None,
        )
        .expect("probe compiles to wasm-gc")
    }

    #[test]
    fn expr_fragment_certification_requires_matching_producer_plan() {
        let output = compile_float_add_probe();
        let without_plan = analyze(&output.bytes, &[]).expect("analysis without producer plan");
        assert!(
            !without_plan
                .certified_names()
                .contains(&"floatAddGoal".to_string()),
            "expr-fragment should not be certified without a producer plan"
        );

        let checked = analyze_with_expr_fragment_plans(
            &output.bytes,
            &[],
            &output.expr_fragment_plans,
        )
        .expect("analysis with producer plan");
        assert!(
            checked
                .certified_names()
                .contains(&"floatAddGoal".to_string()),
            "matching producer plan should certify the probe"
        );

        let mut tampered = output
            .expr_fragment_plans
            .iter()
            .find(|artifact| artifact.export_name == "floatAddGoal")
            .expect("producer emitted a floatAddGoal plan")
            .clone();
        let mut changed = false;
        for node in &mut tampered.plan.body.nodes {
            if let FragNodeKind::Prim { op, .. } = &mut node.kind
                && *op == FragPrim::F64Add
            {
                *op = FragPrim::F64Mul;
                changed = true;
                break;
            }
        }
        assert!(changed, "probe plan should contain f64.add");

        let checked = analyze_with_expr_fragment_plans(&output.bytes, &[], &[tampered])
            .expect("analysis should report a declined producer plan");
        assert!(
            !checked
                .certified_names()
                .contains(&"floatAddGoal".to_string()),
            "a bad producer plan must not fall back to byte-derived classification"
        );
        let reason = checked
            .declined()
            .iter()
            .find(|(name, _)| name == "floatAddGoal")
            .map(|(_, reason)| reason.as_str())
            .expect("floatAddGoal should be declined");
        assert!(
            reason.contains("producer expr-fragment plan does not match emitted wasm"),
            "decline reason should identify producer-plan mismatch, got: {reason}"
        );
    }
}
