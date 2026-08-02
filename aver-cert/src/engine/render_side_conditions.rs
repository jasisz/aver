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

fn render_expr_side_arm(c: &Cert) -> String {
    let name = c.name();
    if expr_fragment_uses_audited_generic(c) {
        return format!(
            "exact Or.inl ⟨rfl, by\n  intro plan hPlan\n  injection hPlan with hPlan\n  \
             subst plan\n  exact CertProofs.{name}_exprFragmentSemanticBridge⟩"
        );
    }
    if c.tag_dispatch_face().is_some() {
        return format!(
            "exact Or.inr (Or.inl ⟨rfl, fun plan hp => by\n  \
             have hpe : plan = AverCert.Plans.{name}Plan := by\n    \
             have : (some AverCert.Plans.{name}Plan : Option AverCert.Schema.ExprFragmentRawPlan) = some plan := by\n      \
             rw [← hp]; rfl\n    exact (Option.some.inj this).symm\n  \
             subst hpe\n  exact CertProofs.{name}_exprFragmentSemanticBridge⟩)"
        );
    }
    if c.vector_get_face().is_some() {
        return format!(
            "exact Or.inr (Or.inr (Or.inl ⟨rfl, CertProofs.{name}_simulates⟩))"
        );
    }
    if let Some(face) = c.project_face() {
        return format!(
            "exact Or.inr (Or.inr (Or.inr (Or.inl ⟨rfl, by\n  exact \
             AcceptanceSoundness.fieldProjection_direct_canonical_discharges \
             \"{name}\" {} {} {} {} CertModule.{name}Code \
             (fun _ _ _ _ _ _ => CertModule.{name}Host) (by decide) (by rfl)⟩)))",
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
    // The float arm sits at position five of six: the record-parameter arm
    // was appended after it, so the float payload gains one `Or.inl`.
    format!(
        "exact Or.inr (Or.inr (Or.inr (Or.inr (Or.inl ⟨rfl, CertProofs.{name}_simulates⟩))))"
    )
}

fn render_string_side_arm(_c: &Cert) -> String {
    String::from(
        "intro plan hPlan\ninjection hPlan with hPlan\nsubst plan\nrefine ⟨rfl, ?_⟩\n\
         intro S x vs hDom\nrefine ⟨x, hDom, ?_⟩\nrfl"
    )
}

fn render_construct_side_arm(c: &Cert, model_info: &ModelInfo) -> String {
    if adt_constructor_uses_model(c, model_info) {
        // Named-ADT constructor bridges are derived in the wall from the
        // checked declared-envelope face; the residual list-slice hypothesis
        // excludes this claim by its named result.
        let ret = model_info
            .fns
            .get(c.name())
            .map(|sig| sig.ret.clone())
            .expect("model-bearing ADT constructor has a source signature");
        return format!(
            "intro hNamed\nexact absurd rfl (hNamed \"{ret}\")"
        );
    }
    String::from(
        "intro _hNamed plan hPlan\ninjection hPlan with hPlan\nsubst plan\nrefine ⟨rfl, ?_⟩\n\
         intro S x args hDom\nsubst args\nconstructor <;> rfl",
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
         simpa [AcceptanceSoundness.mutualSemanticBridge, data, mutualRecursionClaims, \
           CertProofs.{primary}_mutualArtifact, \
           CertProofs.{primary}_mutualClaims, \
           {claim_defs}] using \
         CertProofs.{name}_mutualSemanticBridge"
    )
}

fn render_verbatim_side_arm(_c: &Cert) -> String {
    String::from(
        "intro plan hPlan\ninjection hPlan with hPlan\nsubst plan\n\
         refine ⟨rfl, ?_⟩\nintro S x vs hDom\n\
         exact ⟨x, hDom, by rfl⟩"
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
    out.push_str("/-! ### Artifact semantic side conditions consumed by AcceptanceSoundness.accept_sound -/\n\n");
    out.push_str("theorem exprFragmentSideConditions :\n    AcceptanceSoundness.exprFragmentSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&sym, "symFragmentClaims", render_expr_side_arm));
    out.push_str("\ntheorem stringEqSideConditions :\n    AcceptanceSoundness.stringEqSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(
        &string_eq,
        "stringEqClaims",
        render_string_side_arm,
    ));
    out.push_str("\ntheorem constructSideConditions :\n    AcceptanceSoundness.constructListSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&construct, "constructClaims", |c| {
        render_construct_side_arm(c, model_info)
    }));
    out.push_str("\ntheorem recursionSideConditions :\n    AcceptanceSoundness.recursionSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&recursion, "recursionClaims", render_recursion_side_arm));
    out.push_str("\ntheorem mutualSideConditions :\n    AcceptanceSoundness.mutualSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&mutual, "mutualRecursionClaims", render_mutual_side_arm));
    out.push_str("\ntheorem verbatimSideConditions :\n    AcceptanceSoundness.verbatimSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&verbatim, "verbatimClaims", render_verbatim_side_arm));
    out.push_str("\ntheorem fieldProjectionSideConditions :\n    AcceptanceSoundness.fieldProjectionSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&field_projection, "fieldProjectionClaims", render_field_projection_side_arm));
    out.push_str("\ntheorem compositionSideConditions :\n    AcceptanceSoundness.compositionClaimSemanticBridges data := by\n");
    out.push_str(&render_claim_cases(&composition, "compositionClaims", render_composition_side_arm));
    out.push_str(
        "\ntheorem dischargeSideConditions : AcceptanceSoundness.dischargeSideConditions data := by\n  \
         exact ⟨exprFragmentSideConditions, stringEqSideConditions, constructSideConditions,\n    \
         recursionSideConditions, mutualSideConditions, verbatimSideConditions,\n    \
         fieldProjectionSideConditions, compositionSideConditions⟩\n\n\
         #print axioms AverCert.Artifact.dischargeSideConditions\n",
    );
    out
}
