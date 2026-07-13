/// The audited `V3Rec` generic covers the four unary descent-by-one shapes
/// whose byte-role combinator is `Int.add`. Multiplication and the two-argument
/// accumulator have different semantics/arity and deliberately retain their
/// residual bespoke proofs until matching audited generics exist.
fn recursion_uses_audited_generic(c: &Cert) -> bool {
    matches!(
        c.inner(),
        Cert::Recursive {
            combinator: Combinator::Add,
            ..
        }
    )
}

fn recursion_shape_lean_value(c: &Cert) -> String {
    let Cert::Recursive {
        base_k,
        rec_first,
        other,
        combinator: Combinator::Add,
        ..
    } = c.inner()
    else {
        unreachable!("only unary additive recursion has a V3Rec shape")
    };
    let step = match (*rec_first, *other) {
        (false, BodyOperand::Input) => ".inputSecond".to_string(),
        (false, BodyOperand::Const(k)) => {
            format!(".constSecond ({})", lean_int_lit(k))
        }
        (true, BodyOperand::Input) => ".inputFirst".to_string(),
        (true, BodyOperand::Const(k)) => {
            format!(".constFirst ({})", lean_int_lit(k))
        }
    };
    format!(
        "({{ base := {}, step := {step} }} : V3Rec.RecShapeU)",
        lean_int_lit(*base_k)
    )
}

fn recursion_claim_lean_value(c: &Cert) -> String {
    let Cert::Recursive {
        name,
        carrier,
        box_idx,
        add_idx,
        sub_idx,
        ..
    } = c.inner()
    else {
        unreachable!("audited recursion claim is unary")
    };
    format!(
        "({{ exportNameBytes := {}, exportName := {}, carrier := {carrier}, \
         hostTable := {}, obligation := AverCert.{name}Ob }} : \
         AverCert.AcceptedArtifact.RecursionClaim)",
        render_byte_list(name.as_bytes()),
        lean_str(name),
        recursion_host_table_lean_value(*box_idx, *add_idx, *sub_idx),
    )
}

/// Concrete byte/plan acceptance needed to instantiate the artifact-shaped
/// audited discharge from `Final.lean`. This is data reconstruction only: the
/// source-model residual lives exclusively in the semantic bridge below.
fn recursion_claim_acceptance_proof(c: &Cert) -> String {
    let Cert::Recursive {
        self_idx,
        code_idx,
        type_idx,
        carrier,
        ..
    } = c.inner()
    else {
        unreachable!("audited recursion acceptance is unary")
    };
    let plan = recursion_plan_from_cert(c).expect("audited recursion has a canonical plan");
    let body = lower_expr_fragment_plan(&plan, *carrier)
        .map(|ops| render_ops_value(&ops))
        .expect("audited recursion plan lowers to WInstr");
    let bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier)
        .expect("audited recursion plan lowers to exact code-entry bytes");
    let bytes = render_byte_list(&bytes);
    let binding = format!(
        "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, \
         codeEntry := {bytes} }} : AverCert.WasmSlice.FuncBinding)"
    );
    format!(
        "⟨rfl, rfl, rfl, rfl, ⟨({body}), ({bytes}), {binding}, \
         ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
    )
}

/// Option-(b) residual for one unary additive recursion obligation. The
/// generated proof identifies the byte-derived parsed shape and relates the
/// generated source model to the independent `V3Rec.evalRecU` evaluator in
/// both domain directions. Fuel induction and Wasm execution stay in the
/// sha-pinned `V3RecSpike` / `V3DischargeRecursion` wall.
fn render_recursion_semantic_bridge(c: &Cert) -> String {
    let Cert::Recursive {
        name,
        box_idx,
        add_idx,
        sub_idx,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    debug_assert!(recursion_uses_audited_generic(c));
    let shape = recursion_shape_lean_value(c);
    let claim = recursion_claim_lean_value(c);
    let acceptance = recursion_claim_acceptance_proof(c);
    format!(
        r#"/-! ### {name} — option-(b) recursion semantic bridge -/

theorem {name}_recursionClaimAccepted :
    AverCert.AcceptedArtifact.recursionClaimAccepted
      AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
      AverCert.manifest {claim} := by
  dsimp [AverCert.AcceptedArtifact.recursionClaimAccepted,
    AverCert.AcceptedArtifact.recursionPlanForExport,
    AverCert.AcceptedArtifact.recursionPlanAccepted]
  exact {acceptance}

theorem {name}_recursionSemanticBridge :
    V3Master.recursionSemanticBridge {claim}
      AverCert.Plans.{name}RecursionPlan := by
  have hModelFuel : ∀ fuel n,
      V3Rec.evalRecUFuel {shape} fuel n = {name}__fuel fuel n := by
    intro fuel
    induction fuel with
    | zero => intro n; rfl
    | succ fuel ih =>
        intro n
        simp only [V3Rec.evalRecUFuel, {name}__fuel]
        split <;> simp_all [V3Rec.stepEval]
  have hModel : ∀ n, V3Rec.evalRecU {shape} n = {name} n := by
    intro n
    simpa [V3Rec.evalRecU, {name}] using hModelFuel (n.natAbs + 1) n
  refine ⟨{box_idx}, {add_idx}, {sub_idx}, {shape},
    rfl, rfl, rfl, rfl, ?_, ?_, ?_⟩
  · intro add sub mul stringEq stringConcat
    simpa [AverCert.{name}Ob, CertModule.{name}Host]
  · intro S ns vs hDom
    rcases hDom with ⟨hRepr, hLen⟩
    cases ns with
    | nil => simp at hLen
    | cons n ns =>
        cases ns with
        | nil =>
            cases hRepr with
            | cons hv htail =>
                cases htail
                refine ⟨n, _, rfl, hv, ?_⟩
                intro w hw
                simpa [AverCert.Schema.intRepr, hModel n] using hw
        | cons _ _ => simp at hLen
  · intro S n v hv
    refine ⟨[n], ⟨ReprAll.cons hv ReprAll.nil, rfl⟩, ?_⟩
    intro w hw
    simpa [AverCert.Schema.intRepr, hModel n] using hw

#print axioms {name}_recursionSemanticBridge
"#
    )
}

/// Per-obligation `Final.cert` arm. A singleton artifact view supplies the
/// already byte-derived claim to `recursion_claim_discharges`; the only
/// semantic premise is the generated option-(b) bridge above.
fn render_recursion_final_arm(c: &Cert) -> String {
    debug_assert!(recursion_uses_audited_generic(c));
    let name = c.name();
    let claim = recursion_claim_lean_value(c);
    format!(
        r#"let claim : AverCert.AcceptedArtifact.RecursionClaim := {claim}
      let artifact : AverCert.AcceptedArtifact.ArtifactData :=
        {{ modBytes := AverCert.ArtifactBytes.modBytes,
          modLen := AverCert.ArtifactBytes.modLen, manifest := AverCert.manifest,
          symFragmentClaims := [], stringEqClaims := [], stringConcatClaims := [],
          constructClaims := [], recursionClaims := [claim],
          mutualRecursionClaims := [], verbatimClaims := [], intDispatchClaims := [],
          fieldProjectionClaims := [], compositionMembers := [], compositionClaims := [],
          closureFuel := 0,
          closureClaim := {{ roots := [], helpers := [], admitted := [] }} }}
      exact V3Master.recursion_claim_discharges artifact
        (by
          dsimp [AverCert.AcceptedArtifact.acceptedRecursionFragments,
            AverCert.AcceptedArtifact.recursionClaimsAccepted,
            AverCert.AcceptedArtifact.allClaims, artifact]
          exact ⟨CertProofs.{name}_recursionClaimAccepted, trivial⟩)
        claim (by simp [artifact])
        (by
          intro plan hPlan
          dsimp [artifact, AverCert.AcceptedArtifact.recursionPlanForExport] at hPlan
          injection hPlan with hPlan
          subst plan
          exact CertProofs.{name}_recursionSemanticBridge)"#
    )
}
