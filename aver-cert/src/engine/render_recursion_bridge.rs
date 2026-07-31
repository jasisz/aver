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

/// Render the companion `{name}_recursionClaimAccepted` theorem as a SPLIT
/// proof, mirroring `render_recursion_claim_bundles` in `render_project.rs`.
///
/// This bridge module re-proves acceptance for its export in a STANDALONE file,
/// so the same monolithic witness tuple that inflated the artifact root also
/// inflated this module. Emitting the lowered body, code-entry bytes and
/// function binding as named `def`s and each byte-walking conjunct as its own
/// leaf theorem keeps this module's per-claim peak at the largest single leaf.
/// The leaves are stated over `AverCert.Plans.{name}RecursionPlan`, so the
/// aggregate re-runs no byte-decode work.
fn render_recursion_bridge_claim_accepted(c: &Cert) -> String {
    let (name, self_idx, type_idx, carrier) = match c.inner() {
        Cert::Recursive {
            name,
            self_idx,
            type_idx,
            carrier,
            ..
        }
        | Cert::AccumulatorRecursive {
            name,
            self_idx,
            type_idx,
            carrier,
            ..
        } => (name, self_idx, type_idx, carrier),
        _ => unreachable!("audited recursion acceptance has a recursion shape"),
    };
    let plan_cert = recursion_plan_from_cert(c).expect("audited recursion has a canonical plan");
    let lowered_body = lower_expr_fragment_plan(&plan_cert, *carrier)
        .map(|ops| render_ops_value(&ops))
        .expect("audited recursion plan lowers to WInstr");
    let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(&plan_cert, *carrier)
        .expect("audited recursion plan lowers to exact code-entry bytes");
    let code_entry_bytes = render_byte_list(&code_entry_bytes);
    let export_name_bytes = render_byte_list(name.as_bytes());
    let host_table = recursion_host_table_lean_value(c);
    let claim = recursion_claim_lean_value(c);

    let body = format!("{name}RecursionClaimBody");
    let code_entry = format!("{name}RecursionClaimCodeEntry");
    let binding = format!("{name}RecursionClaimBinding");
    let check_plan = format!("{name}RecursionClaimCheckPlan");
    let lower_body = format!("{name}RecursionClaimLowerBody");
    let lower_code = format!("{name}RecursionClaimLowerCode");
    let func_binding = format!("{name}RecursionClaimFuncBinding");
    let check_shape = format!("{name}RecursionClaimCheckShape");
    let func_type = format!("{name}RecursionClaimFuncType");
    let host_types = format!("{name}RecursionClaimHostTypes");
    let plan = format!("AverCert.Plans.{name}RecursionPlan");
    let obligation = format!("AverCert.{name}Ob");
    format!(
        "-- Witness data for `{name}` as named constants so no large literal is\n\
         -- duplicated across the leaf statements or baked into the aggregate term.\n\
         def {body} : List CertPrelude.WInstr := {lowered_body}\n\n\
         def {code_entry} : AverCert.WasmSlice.ByteSeq := {code_entry_bytes}\n\n\
         def {binding} : AverCert.WasmSlice.FuncBinding :=\n  \
           {{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry} }}\n\n\
         -- One leaf theorem per acceptance conjunct; the heavy ones are the\n\
         -- `modBytes` binding decode and type-section walks.\n\
         theorem {check_plan} :\n  \
           AverCert.PlanCheck.checkRecursionRawPlan {plan} = true := by\n  \
           rfl\n\n\
         theorem {lower_body} :\n  \
           AverCert.PlanLower.lowerRecursionBody {carrier} {plan} = some {body} := by\n  \
           rfl\n\n\
         theorem {lower_code} :\n  \
           AverCert.PlanBytes.lowerRecursionCodeEntry {carrier} {plan} = some {code_entry} := by\n  \
           rfl\n\n\
         theorem {func_binding} :\n  \
           AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry} = some {binding} := by\n  \
           rfl\n\n\
         theorem {check_shape} :\n  \
           AverCert.PlanCheck.checkRecursionPlanShape {binding}.funcIdx {host_table} {obligation}.totalityRole {plan} = true := by\n  \
           rfl\n\n\
         theorem {func_type} :\n  \
           AverCert.WasmSlice.funcTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {binding}.typeIdx {plan}.params.length {carrier} = true := by\n  \
           rfl\n\n\
         theorem {host_types} :\n  \
           AverCert.WasmSlice.hostTableFuncTypesMatch AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {carrier} {host_table} = true := by\n  \
           rfl\n\n\
         theorem {name}_recursionClaimAccepted :\n    \
           AverCert.AcceptedArtifact.recursionClaimAccepted\n      \
             AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen\n      \
             AverCert.manifest {claim} := by\n  \
           dsimp [AverCert.AcceptedArtifact.recursionClaimAccepted,\n    \
             AverCert.AcceptedArtifact.recursionPlanForExport,\n    \
             AverCert.AcceptedArtifact.recursionPlanAccepted]\n  \
           exact ⟨rfl, rfl, {check_plan}, rfl, ⟨{body}, {code_entry}, {binding}, ⟨{lower_body}, {lower_code}, {func_binding}, rfl, {check_shape}, {func_type}, {host_types}, rfl⟩⟩⟩\n"
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
    let claim_accepted = render_recursion_bridge_claim_accepted(c);
    format!(
        r#"/-! ### {name} — option-(b) recursion semantic bridge -/

{claim_accepted}
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
    let claim_accepted = render_recursion_bridge_claim_accepted(c);
    format!(
        r#"/-! ### {name} — option-(b) accumulator recursion semantic bridge -/

{claim_accepted}
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
