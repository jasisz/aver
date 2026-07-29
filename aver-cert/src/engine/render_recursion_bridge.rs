/// The audited `RecursionSoundness` generic covers the four unary descent-by-one operand
/// shapes with either the `Int.add` or `Int.mul` semantic combinator. The
/// two-argument accumulator has its own arity-pinned audited shape.
fn recursion_uses_audited_generic(c: &Cert) -> bool {
    matches!(
        c.inner(),
        Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. }
    )
}

fn recursion_shape_lean_value(c: &Cert) -> String {
    let Cert::Recursive {
        base_k,
        rec_first,
        other,
        ..
    } = c.inner()
    else {
        unreachable!("only unary recursion has a RecursionSoundness shape")
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
        "({{ base := {}, step := {step} }} : RecursionSoundness.RecShapeU)",
        lean_int_lit(*base_k)
    )
}

fn recursion_combine_lean_value(c: &Cert) -> &'static str {
    let Cert::Recursive { combinator, .. } = c.inner() else {
        unreachable!("only unary recursion has a RecursionSoundness combinator")
    };
    match combinator {
        Combinator::Add => ".add",
        Combinator::Mul => ".mul",
    }
}

fn recursion_claim_lean_value(c: &Cert) -> String {
    let (name, carrier) = match c.inner() {
        Cert::Recursive { name, carrier, .. }
        | Cert::AccumulatorRecursive { name, carrier, .. } => (name, carrier),
        _ => unreachable!("audited recursion claim has a recursion shape"),
    };
    format!(
        "({{ exportNameBytes := {}, exportName := {}, carrier := {carrier}, \
         hostTable := {}, obligation := AverCert.{name}Ob }} : \
         AverCert.AcceptedArtifact.RecursionClaim)",
        render_byte_list(name.as_bytes()),
        lean_str(name),
        recursion_host_table_lean_value(c),
    )
}

/// Concrete byte/plan acceptance needed to instantiate the artifact-shaped
/// audited discharge from `Final.lean`. This is data reconstruction only: the
/// source-model residual lives exclusively in the semantic bridge below.
fn recursion_claim_acceptance_proof(c: &Cert) -> String {
    let (self_idx, type_idx, carrier) = match c.inner() {
        Cert::Recursive {
            self_idx,
            type_idx,
            carrier,
            ..
        }
        | Cert::AccumulatorRecursive {
            self_idx,
            type_idx,
            carrier,
            ..
        } => (self_idx, type_idx, carrier),
        _ => unreachable!("audited recursion acceptance has a recursion shape"),
    };
    let plan = recursion_plan_from_cert(c).expect("audited recursion has a canonical plan");
    let body = lower_expr_fragment_plan(&plan, *carrier)
        .map(|ops| render_ops_value(&ops))
        .expect("audited recursion plan lowers to WInstr");
    let bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier)
        .expect("audited recursion plan lowers to exact code-entry bytes");
    let bytes = render_byte_list(&bytes);
    let binding = format!(
        "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, \
         codeEntry := {bytes} }} : AverCert.WasmSlice.FuncBinding)"
    );
    format!(
        "⟨rfl, rfl, rfl, rfl, ⟨({body}), ({bytes}), {binding}, \
         ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
    )
}

/// Option-(b) residual for one unary additive or multiplicative recursion obligation. The
/// generated proof identifies the byte-derived parsed shape and relates the
/// generated source model to the independent `RecursionSoundness.evalRecU` evaluator in
/// the represented obligation domain. Fuel induction and Wasm execution stay in the
/// sha-pinned `RecursionSoundness` / `DischargeRecursion` wall.
fn render_unary_recursion_semantic_bridge(c: &Cert, model_info: &ModelInfo) -> String {
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
    let model_name = c.model_lean_name(model_info);
    let shape = recursion_shape_lean_value(c);
    let combine = recursion_combine_lean_value(c);
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
    AcceptanceSoundness.recursionSemanticBridge {claim}
      AverCert.Plans.{name}RecursionPlan := by
  have hModelFuel : ∀ fuel n,
      RecursionSoundness.evalRecUFuel {combine} {shape} fuel n = {model_name}__fuel fuel n := by
    intro fuel
    induction fuel with
    | zero => intro n; rfl
    | succ fuel ih =>
        intro n
        simp only [RecursionSoundness.evalRecUFuel, {model_name}__fuel]
        split <;> simp_all [RecursionSoundness.stepEval, RecursionSoundness.combineEval]
  have hModel : ∀ n, RecursionSoundness.evalRecU {combine} {shape} n = {model_name} n := by
    intro n
    simpa [RecursionSoundness.evalRecU, {model_name}] using hModelFuel (n.natAbs + 1) n
  refine Or.inl ?_
  refine ⟨{combine}, {box_idx}, {add_idx}, {sub_idx}, {shape},
    rfl, rfl, ?_, ?_⟩
  · intro add sub mul stringEq stringConcat toIndex
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
#print axioms {name}_recursionSemanticBridge
"#
    )
}

fn render_accumulator_recursion_semantic_bridge(c: &Cert, model_info: &ModelInfo) -> String {
    let Cert::AccumulatorRecursive {
        name,
        box_idx,
        add_idx,
        sub_idx,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let model_name = c.model_lean_name(model_info);
    let claim = recursion_claim_lean_value(c);
    let acceptance = recursion_claim_acceptance_proof(c);
    format!(
        r#"/-! ### {name} — option-(b) accumulator recursion semantic bridge -/

theorem {name}_recursionClaimAccepted :
    AverCert.AcceptedArtifact.recursionClaimAccepted
      AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
      AverCert.manifest {claim} := by
  dsimp [AverCert.AcceptedArtifact.recursionClaimAccepted,
    AverCert.AcceptedArtifact.recursionPlanForExport,
    AverCert.AcceptedArtifact.recursionPlanAccepted]
  exact {acceptance}

theorem {name}_recursionSemanticBridge :
    AcceptanceSoundness.recursionSemanticBridge {claim}
      AverCert.Plans.{name}RecursionPlan := by
  have hModelFuel : ∀ fuel n acc,
      RecursionSoundness.evalRecAFuel fuel n acc = {model_name}__fuel fuel n acc := by
    intro fuel
    induction fuel with
    | zero => intro n acc; rfl
    | succ fuel ih =>
        intro n acc
        simp only [RecursionSoundness.evalRecAFuel, {model_name}__fuel]
        split <;> simp_all
  have hModel : ∀ n acc, RecursionSoundness.evalRecA n acc = {model_name} n acc := by
    intro n acc
    simpa [RecursionSoundness.evalRecA, {model_name}] using hModelFuel (n.natAbs + 1) n acc
  refine Or.inr ?_
  refine ⟨{box_idx}, {add_idx}, {sub_idx}, .accumulator,
    rfl, rfl, ?_, ?_⟩
  · intro add sub mul stringEq stringConcat toIndex
    simpa [AverCert.{name}Ob, CertModule.{name}Host]
  · intro S ns vs hDom
    rcases hDom with ⟨hRepr, hLen⟩
    cases hRepr with
    | nil => simp at hLen
    | cons hvn htail =>
        rename_i n vn ns1 vs1
        cases htail with
        | nil => simp at hLen
        | cons hvacc htail2 =>
            rename_i acc vacc ns2 vs2
            cases htail2 with
            | nil =>
                refine ⟨n, acc, vn, vacc, rfl, hvn, hvacc, ?_⟩
                intro w hw
                simpa [AverCert.Schema.intRepr, hModel n acc] using hw
            | cons _ _ => simp at hLen
#print axioms {name}_recursionSemanticBridge
"#
    )
}

fn render_recursion_semantic_bridge(c: &Cert, model_info: &ModelInfo) -> String {
    match c.inner() {
        Cert::Recursive { .. } => render_unary_recursion_semantic_bridge(c, model_info),
        Cert::AccumulatorRecursive { .. } => {
            render_accumulator_recursion_semantic_bridge(c, model_info)
        }
        _ => unreachable!(),
    }
}
