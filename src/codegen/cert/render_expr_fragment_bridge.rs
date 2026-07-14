/// Integer and Bool source fragments are within the audited symbolic fragment
/// grammar. Float fragments are deliberately excluded: their bit-level source
/// models remain bespoke because floating-point semantics are outside v3's
/// integer/Bool model.
fn expr_fragment_uses_audited_generic(c: &Cert) -> bool {
    let Cert::ExprFragment {
        source_plan: Some(source_plan),
        plan,
        ..
    } = c.inner()
    else {
        return false;
    };
    source_plan
        .params
        .iter()
        .all(|ty| matches!(ty, SymTy::Int | SymTy::Bool))
        && matches!(source_plan.result, SymTy::Int | SymTy::Bool)
        && plan
            .params
            .iter()
            .all(|ty| matches!(ty, FragTy::IntCarrier | FragTy::BoolI32))
        && matches!(plan.result, FragTy::IntCarrier | FragTy::BoolI32)
}

fn expr_fragment_source_model(c: &Cert) -> String {
    debug_assert!(expr_fragment_uses_audited_generic(c));
    match c.arity() {
        1 => c.name().to_string(),
        2 => format!("fun p => {} p.1 p.2", c.name()),
        arity => panic!("unsupported generic expr-fragment source arity {arity}"),
    }
}

fn expr_fragment_claim_lean_value(
    c: &Cert,
    host_table: FragHostTable,
    struct_table_lean: &str,
) -> String {
    debug_assert!(expr_fragment_uses_audited_generic(c));
    let name = c.name();
    format!(
        "({{ exportNameBytes := {}, exportName := {}, carrier := {}, \
         hostTable := {}, structTable := {struct_table_lean}, \
         plan := AverCert.Plans.{name}SymPlan, obligation := AverCert.{name}Ob }} : \
         AverCert.AcceptedArtifact.SymFragmentClaim)",
        render_byte_list(name.as_bytes()),
        lean_str(name),
        c.carrier(),
        host_table.lean_value(),
    )
}

/// Reconstruct the same byte/plan witness emitted as DATA in `Artifact.lean`.
fn expr_fragment_claim_acceptance_proof(c: &Cert) -> String {
    let Cert::ExprFragment {
        carrier,
        self_idx,
        type_idx,
        plan,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(plan, *carrier)
        .expect("generic expr-fragment plan lowers to code-entry bytes");
    let code_entry_bytes = render_byte_list(&code_entry_bytes);
    let lowered_body = lower_expr_fragment_plan(plan, *carrier)
        .map(|ops| render_ops_value(&ops))
        .expect("generic expr-fragment plan lowers to WInstr body");
    let binding = format!(
        "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, \
         codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
    );
    format!(
        "⟨rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {binding}, \
         ⟨⟨rfl, rfl, rfl, rfl⟩, rfl, rfl⟩⟩⟩"
    )
}

fn render_expr_fragment_semantic_bridge(
    c: &Cert,
    host_table: FragHostTable,
    struct_table_lean: &str,
) -> String {
    debug_assert!(expr_fragment_uses_audited_generic(c));
    if let Some(face) = c.int_add_face() {
        return render_expr_fragment_int_add_semantic_bridge(
            c,
            face,
            host_table,
            struct_table_lean,
        );
    }
    render_expr_fragment_int_bool_semantic_bridge(c, host_table, struct_table_lean)
}

fn render_expr_fragment_int_add_semantic_bridge(
    c: &Cert,
    face: FragIntAddFace,
    host_table: FragHostTable,
    struct_table_lean: &str,
) -> String {
    let name = c.name();
    let carrier = c.carrier();
    let k = lean_int_lit(face.k);
    let claim = expr_fragment_claim_lean_value(c, host_table, struct_table_lean);
    let acceptance = expr_fragment_claim_acceptance_proof(c);
    let host_table_lean = host_table.lean_value();
    format!(
        r#"/-! ### {name} — option-(b) integer expr-fragment semantic bridge -/

theorem {name}_exprFragmentClaimAccepted :
    AverCert.AcceptedArtifact.symFragmentClaimAccepted
      AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {claim} := by
  unfold AverCert.AcceptedArtifact.symFragmentClaimAccepted
  exact {acceptance}

theorem {name}_exprFragmentSemanticBridge :
    V3Master.exprFragmentSemanticBridge {claim}
      AverCert.Plans.{name}Plan := by
  refine ⟨rfl, ?_⟩
  intro S add sub mul stringEq stringConcat
    hAdd hSub hMul hStringEq hStringConcat fuel ns vs out hDom hRun
  dsimp [AverCert.{name}Ob] at ns vs hDom hRun ⊢
  rcases hDom with ⟨hRepr, hLen⟩
  cases hRepr with
  | nil => simp at hLen
  | cons hv htail =>
      rename_i n v ns' vs'
      cases htail with
      | cons _ _ => simp at hLen
      | nil =>
          cases hc : add [v, carrierSmall {carrier} ({k})] with
          | none =>
              simp [wFuncN, wRunF, CertModule.{name}Code, CertModule.{name}Host,
                boxRef, popArgs, initLocals, hc] at hRun
          | some result =>
              refine ⟨[v], [v, .null], result, rfl, rfl, ?_, ?_, ?_⟩
              · simp [V3ExprFragmentGeneric.blockCallsOK,
                  V3ExprFragmentGeneric.nodesCallsOK,
                  V3ExprFragmentGeneric.kindCallsOK, AverCert.Plans.{name}Plan,
                  CertModule.{name}Code, CertModule.{name}Host]
              · simp only [V3ExprFragmentFull.evalSymRawPlan]
                rw [show AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
                  {host_table_lean} {struct_table_lean}
                  AverCert.Plans.{name}SymPlan =
                    some AverCert.Plans.{name}Plan by rfl]
                simp [AverCert.Plans.{name}Plan,
                  AverCert.PlanLower.maxFuel, V3ExprFragmentFull.runBlock,
                  V3ExprFragmentFull.runBlockFuel,
                  V3ExprFragmentFull.runNodesFuel,
                  V3ExprFragmentFull.finishWith,
                  AverCert.AcceptedArtifact.exprFragmentNLocals,
                  CertModule.{name}Code, CertModule.{name}Host, boxRef,
                  popArgs, initLocals, carrierSmall, hc] <;>
                try simp_all [V3ExprFragmentFull.runBlockFuel,
                  V3ExprFragmentFull.runNodesFuel,
                  V3ExprFragmentFull.finishWith, PlanLower.popExpected,
                  PlanLower.popExpectedAll, PlanLower.primInstr,
                  V3ExprFragmentFull.runPrim, wRunF,
                  AverCert.AcceptedArtifact.exprFragmentNLocals,
                  CertModule.{name}Code, CertModule.{name}Host, boxRef,
                  popArgs, initLocals, hc, carrierSmall, b32] <;>
                try simp_all [V3ExprFragmentFull.runBlockFuel,
                  V3ExprFragmentFull.runNodesFuel,
                  V3ExprFragmentFull.finishWith, PlanLower.popExpected,
                  PlanLower.popExpectedAll, PlanLower.primInstr,
                  V3ExprFragmentFull.runPrim, wRunF,
                  AverCert.AcceptedArtifact.exprFragmentNLocals,
                  CertModule.{name}Code, CertModule.{name}Host, boxRef,
                  popArgs, initLocals, hc, carrierSmall, b32]
              · simpa [AverCert.Schema.intRepr, {name}] using
                  hAdd n ({k}) v (carrierSmall {carrier} ({k})) result hv
                    (S.smallIntro ({k})) hc

#print axioms {name}_exprFragmentSemanticBridge
"#
    )
}

fn expr_fragment_bridge_eval_tactic(
    plan: &ExprFragmentPlan,
    name: &str,
    host_table_lean: &str,
    struct_table_lean: &str,
    evalset: &str,
) -> String {
    let mut steps = plan
        .params
        .iter()
        .enumerate()
        .filter(|(_, ty)| **ty == FragTy::BoolI32)
        .map(|(i, _)| format!("cases a{i}"))
        .collect::<Vec<_>>();
    let mut conds = Vec::new();
    collect_expr_fragment_conditions(&plan.body, &|idx, _ty| format!("a{idx}"), &mut conds);
    for (i, cond) in conds.iter().enumerate() {
        steps.push(format!("by_cases h{i} : {cond}"));
    }
    let hints = if conds.is_empty() {
        String::new()
    } else {
        format!(
            ", {}",
            (0..conds.len())
                .map(|i| format!("h{i}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    let first = if steps.is_empty() {
        format!("simp [{evalset}, carrierSmall, ge_iff_le]")
    } else {
        format!(
            "{} <;> simp [{evalset}, carrierSmall, ge_iff_le{hints}]",
            steps.join(" <;> ")
        )
    };
    let normalize = "V3ExprFragmentFull.runBlockFuel, \
        V3ExprFragmentFull.runNodesFuel, V3ExprFragmentFull.finishWith, \
        PlanLower.popExpected, PlanLower.popExpectedAll, PlanLower.primInstr, \
        V3ExprFragmentFull.runPrim, wRunF, \
        AverCert.AcceptedArtifact.exprFragmentNLocals, carrierSmall, b32";
    format!(
        "    simp only [V3ExprFragmentFull.evalSymRawPlan]\n    \
         rw [show AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan\n      \
         {host_table_lean} {struct_table_lean} AverCert.Plans.{name}SymPlan =\n      \
         some AverCert.Plans.{name}Plan by rfl]\n    \
         {first} <;>\n    try simp_all [{normalize}] <;>\n    \
         try simp_all [{normalize}] <;>\n    try simp_all [{normalize}]"
    )
}

fn render_expr_fragment_int_bool_semantic_bridge(
    c: &Cert,
    host_table: FragHostTable,
    struct_table_lean: &str,
) -> String {
    let Cert::ExprFragment {
        name,
        carrier,
        plan,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    debug_assert_eq!(plan.result, FragTy::BoolI32);
    let claim = expr_fragment_claim_lean_value(c, host_table, struct_table_lean);
    let acceptance = expr_fragment_claim_acceptance_proof(c);
    let result = expr_fragment_wval_expr(plan, &|idx, _ty| format!("a{idx}"));
    let input_values = plan
        .params
        .iter()
        .enumerate()
        .map(|(idx, ty)| match ty {
            FragTy::IntCarrier => format!("carrierSmall {carrier} a{idx}"),
            FragTy::BoolI32 => format!("b32 a{idx}"),
            _ => unreachable!("generic integer/Bool fragment input"),
        })
        .collect::<Vec<_>>();
    let inputs = format!("[{}]", input_values.join(", "));
    let mut locals = input_values;
    locals.push(".null".to_string());
    let locals = format!("[{}]", locals.join(", "));
    let (dom_name, unpack) = match plan.params.len() {
        1 => ("a0", String::new()),
        2 => ("p", "  rcases p with ⟨a0, a1⟩\n".to_string()),
        arity => panic!("unsupported generic expr-fragment arity {arity}"),
    };
    let evalset = format!(
        "AverCert.Plans.{name}Plan, AverCert.PlanLower.maxFuel, \
         V3ExprFragmentFull.runBlock, V3ExprFragmentFull.runBlockFuel, \
         V3ExprFragmentFull.runNodesFuel, V3ExprFragmentFull.finishWith, \
         AverCert.AcceptedArtifact.exprFragmentNLocals, \
         CertModule.{name}Code, CertModule.{name}Host, wFuncN, wRunF, f, b32, \
         popArgs, initLocals, {name}"
    );
    let host_table_lean = host_table.lean_value();
    let eval_tactic = expr_fragment_bridge_eval_tactic(
        plan,
        name,
        &host_table_lean,
        struct_table_lean,
        &evalset,
    );
    format!(
        r#"/-! ### {name} — option-(b) integer/Bool expr-fragment semantic bridge -/

theorem {name}_exprFragmentClaimAccepted :
    AverCert.AcceptedArtifact.symFragmentClaimAccepted
      AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {claim} := by
  unfold AverCert.AcceptedArtifact.symFragmentClaimAccepted
  exact {acceptance}

theorem {name}_exprFragmentSemanticBridge :
    V3Master.exprFragmentSemanticBridge {claim}
      AverCert.Plans.{name}Plan := by
  refine ⟨rfl, ?_⟩
  intro S add sub mul stringEq stringConcat
    hAdd hSub hMul hStringEq hStringConcat fuel {dom_name} vs out hDom hRun
  dsimp [AverCert.{name}Ob] at {dom_name} vs hDom hRun ⊢
{unpack}  subst vs
  refine ⟨{inputs}, {locals}, {result}, rfl, rfl, ?_, ?_, ?_⟩
  · simp [V3ExprFragmentGeneric.blockCallsOK,
      V3ExprFragmentGeneric.nodesCallsOK, V3ExprFragmentGeneric.kindCallsOK,
      AverCert.Plans.{name}Plan, CertModule.{name}Code, CertModule.{name}Host]
  ·
{eval_tactic}
  · simp [{name}, AverCert.Schema.boolRepr, b32]

#print axioms {name}_exprFragmentSemanticBridge
"#
    )
}
