fn render_claim_cases(
    certs: &[&Cert],
    claims_def: &str,
    mut render_arm: impl FnMut(&Cert) -> String,
) -> String {
    if certs.is_empty() {
        return format!(
            "  intro claim hClaim\n  simp [data, {claims_def}] at hClaim\n"
        );
    }
    let pattern = std::iter::repeat_n("rfl", certs.len())
        .collect::<Vec<_>>()
        .join(" | ");
    let mut out = format!(
        "  intro claim hClaim\n  dsimp [data, {claims_def}] at hClaim\n  \
         simp only [List.mem_cons, List.mem_singleton, List.mem_nil_iff,\n    \
           List.not_mem_nil, or_false] at hClaim\n  \
         rcases hClaim with {pattern}\n"
    );
    for cert in certs {
        out.push_str("  · ");
        let arm = render_arm(cert);
        let mut lines = arm.lines();
        if let Some(first) = lines.next() {
            out.push_str(first);
            out.push('\n');
        }
        for line in lines {
            out.push_str("    ");
            out.push_str(line);
            out.push('\n');
        }
    }
    out
}

fn render_tactic_branch(body: &str) -> String {
    let mut out = String::new();
    for (index, line) in body.trim_end().lines().enumerate() {
        let line = line.strip_prefix("  ").unwrap_or(line);
        if index == 0 {
            out.push_str("  · ");
        } else {
            out.push_str("    ");
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

fn render_expr_side_arm(c: &Cert) -> String {
    let name = c.name();
    if expr_fragment_uses_audited_generic(c) {
        return format!(
            "exact Or.inl ⟨rfl, by\n  intro plan hPlan\n  injection hPlan with hPlan\n  \
             subst plan\n  exact CertProofs.{name}_exprFragmentSemanticBridge⟩"
        );
    }
    if let Some(face) = c.project_face() {
        return format!(
            "exact Or.inr (Or.inl ⟨rfl, by\n  exact \
             V3Master.fieldProjection_direct_canonical_discharges \
             \"{name}\" {} {} {} {} CertModule.{name}Code \
             (fun _ _ _ _ _ => CertModule.{name}Host) (by decide) (by rfl)⟩)",
            c.carrier(),
            face.struct_idx,
            c.self_idx(),
            face.field_idx,
        );
    }
    let Cert::ExprFragment { plan, .. } = c.inner() else {
        unreachable!()
    };
    assert!(
        plan.params.contains(&FragTy::F64) || plan.result == FragTy::F64,
        "only float expression claims may use the bespoke residual: {name}"
    );
    format!(
        "exact Or.inr (Or.inr ⟨rfl, CertProofs.{name}_simulates⟩)"
    )
}

fn render_string_side_arm(_c: &Cert) -> String {
    String::from(
        "intro plan hPlan\ninjection hPlan with hPlan\nsubst plan\nrefine ⟨rfl, ?_⟩\n\
         intro S x vs hDom\nrefine ⟨x, hDom, ?_⟩\nrfl"
    )
}

fn render_construct_side_arm(c: &Cert, model_info: &ModelInfo) -> String {
    let name = c.name();
    let semantic = if adt_constructor_uses_model(c, model_info) {
        format!("exact CertProofs.{name}_constructSemanticBridge")
    } else {
        "intro S x args hDom\nsubst args\nconstructor <;> rfl".to_string()
    };
    format!(
        "intro plan hPlan\ninjection hPlan with hPlan\nsubst plan\nrefine ⟨rfl, ?_⟩\n{semantic}"
    )
}

fn render_recursion_side_arm(c: &Cert) -> String {
    let name = c.name();
    format!(
        "intro plan hPlan\n\
         dsimp [AverCert.AcceptedArtifact.recursionPlanForExport] at hPlan\n\
         injection hPlan with hPlan\nsubst plan\n\
         exact CertProofs.{name}_recursionSemanticBridge"
    )
}

fn render_mutual_side_arm(c: &Cert) -> String {
    let Cert::MutualRecursion { name, scc, .. } = c.inner() else {
        unreachable!()
    };
    let primary = &scc[0].name;
    let claim_defs = scc
        .iter()
        .map(|member| format!("CertProofs.{}_mutualClaim", member.name))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "intro plan hPlan\n\
         dsimp [AverCert.AcceptedArtifact.mutualPlanForExport] at hPlan\n\
         injection hPlan with hPlan\nsubst plan\n\
         simpa [V3Master.mutualSemanticBridge, data, mutualRecursionClaims, \
           CertProofs.{primary}_mutualArtifact, \
           CertProofs.{primary}_mutualClaims, \
           {claim_defs}] using \
         CertProofs.{name}_mutualSemanticBridge"
    )
}

fn render_verbatim_side_arm(_c: &Cert) -> String {
    String::from(
        "intro plan hPlan\ninjection hPlan with hPlan\nsubst plan\n\
         refine ⟨by rfl, rfl, ?_⟩\nintro S x vs hDom\n\
         exact ⟨x, hDom, by rfl⟩"
    )
}

fn render_int_dispatch_side_arm(
    c: &Cert,
    strict: FragHostTable,
) -> String {
    let name = c.name();
    let plan = int_dispatch_plan_from_cert(c, strict)
        .expect("certified Int dispatch has a byte-matching plan");
    let IntDispatchCascade::Test {
        ty_idx,
        hit,
        rest,
    } = &plan.body
    else {
        unreachable!("certified Int dispatch has a test root")
    };
    let root = format!(
        "⟨{ty_idx}, ({}), ({}), rfl⟩",
        int_dispatch_leaf_lean_value(hit),
        int_dispatch_cascade_lean_value(rest),
    );
    format!(
        "intro plan hPlan\ninjection hPlan with hPlan\nsubst plan\n\
         exact ⟨{root}, rfl, CertProofs.{name}_intDispatchSemanticBridge⟩"
    )
}

fn render_field_projection_side_arm(_c: &Cert) -> String {
    String::from(
        "intro plan hPlan\ninjection hPlan with hPlan\nsubst plan\n\
         refine ⟨rfl, rfl, ?_⟩\nintro S x vs hDom\n\
         exact ⟨x.1, x.2, hDom, by rfl⟩"
    )
}

fn render_composition_side_arm(c: &Cert) -> String {
    let name = c.name();
    format!(
        "intro rootMember hRoot callees hShape\n\
         dsimp [data, compositionMembers, CertProofs.{name}CompositionClaim, \
           CertProofs.{name}CompositionMembers] at hRoot hShape ⊢\n\
         simp [AverCert.AcceptedArtifact.compositionMemberForName] at hRoot\n\
         subst rootMember\ninjection hShape with hShape\nsubst callees\n\
         exact CertProofs.{name}_compositionSemanticBridge"
    )
}

fn render_discharge_side_conditions(analysis: &Analysis, model_info: &ModelInfo) -> String {
    let sym = analysis
        .certs
        .iter()
        .filter(|c| matches!(c.inner(), Cert::ExprFragment { source_plan, plan, .. }
            if expr_fragment_source_plan(source_plan, plan).is_some()))
        .collect::<Vec<_>>();
    let string_eq = analysis
        .certs
        .iter()
        .filter(|c| matches!(c.inner(), Cert::StringEqVerbatimMatch { .. }))
        .collect::<Vec<_>>();
    let string_concat = analysis
        .certs
        .iter()
        .filter(|c| matches!(c.inner(), Cert::StringConcatVerbatimMatch { .. }))
        .collect::<Vec<_>>();
    let construct = analysis
        .certs
        .iter()
        .filter(|c| {
            matches!(c.inner(), Cert::AdtConstructor { .. })
                && construct_plan_from_cert(c).is_some()
                && adt_constructor_sym_plan_from_cert(c, model_info).is_some()
        })
        .collect::<Vec<_>>();
    let recursion = analysis
        .certs
        .iter()
        .filter(|c| matches!(c.inner(), Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. }))
        .collect::<Vec<_>>();
    let mutual = analysis
        .certs
        .iter()
        .filter(|c| matches!(c.inner(), Cert::MutualRecursion { .. }))
        .collect::<Vec<_>>();
    let verbatim = analysis
        .certs
        .iter()
        .filter(|c| matches!(c.inner(), Cert::VerbatimWidenedMatch { .. } | Cert::VerbatimVariantDispatch { .. }))
        .collect::<Vec<_>>();
    let int_dispatch = analysis
        .certs
        .iter()
        .filter(|c| matches!(c.inner(), Cert::VariantDispatch { .. } | Cert::WidenedIntMatch { .. }))
        .collect::<Vec<_>>();
    let field_projection = analysis
        .certs
        .iter()
        .filter(|c| matches!(c.inner(), Cert::FieldProjection { .. }))
        .collect::<Vec<_>>();
    let composition = analysis
        .certs
        .iter()
        .filter(|c| matches!(c.inner(), Cert::Composition { .. }))
        .collect::<Vec<_>>();

    let mut out = String::new();
    out.push_str("/-! ### Artifact semantic side conditions consumed by V3Master.accept_sound -/\n\n");
    out.push_str("theorem exprFragmentSideConditions :\n    V3Master.exprFragmentSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&sym, "symFragmentClaims", render_expr_side_arm));
    out.push_str("\ntheorem stringSideConditions :\n    V3Master.stringSemanticBridges data := by\n  refine ⟨?_, ?_⟩\n");
    out.push_str(&render_tactic_branch(&render_claim_cases(
        &string_eq,
        "stringEqClaims",
        render_string_side_arm,
    )));
    out.push_str(&render_tactic_branch(&render_claim_cases(
        &string_concat,
        "stringConcatClaims",
        render_string_side_arm,
    )));
    out.push_str("\n\ntheorem constructSideConditions :\n    V3Master.constructSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&construct, "constructClaims", |c| {
        render_construct_side_arm(c, model_info)
    }));
    out.push_str("\ntheorem recursionSideConditions :\n    V3Master.recursionSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&recursion, "recursionClaims", render_recursion_side_arm));
    out.push_str("\ntheorem mutualSideConditions :\n    V3Master.mutualSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&mutual, "mutualRecursionClaims", render_mutual_side_arm));
    out.push_str("\ntheorem verbatimSideConditions :\n    V3Master.verbatimSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&verbatim, "verbatimClaims", render_verbatim_side_arm));
    out.push_str("\ntheorem intDispatchSideConditions :\n    V3Master.intDispatchSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&int_dispatch, "intDispatchClaims", |c| {
        render_int_dispatch_side_arm(c, analysis.frag_host_table)
    }));
    out.push_str("\ntheorem fieldProjectionSideConditions :\n    V3Master.fieldProjectionSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&field_projection, "fieldProjectionClaims", render_field_projection_side_arm));
    out.push_str("\ntheorem compositionSideConditions :\n    V3Master.compositionClaimSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&composition, "compositionClaims", render_composition_side_arm));
    out.push_str(
        "\ntheorem dischargeSideConditions : V3Master.dischargeSideConditions data := by\n  \
         exact ⟨exprFragmentSideConditions, stringSideConditions, constructSideConditions,\n    \
         recursionSideConditions, mutualSideConditions, verbatimSideConditions,\n    \
         intDispatchSideConditions, fieldProjectionSideConditions,\n    \
         compositionSideConditions⟩\n\n\
         #print axioms AverCert.Artifact.dischargeSideConditions\n",
    );
    out
}
