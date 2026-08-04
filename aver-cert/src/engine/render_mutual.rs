/// The concrete mutual claim fed to the audited generic discharge.  The SCC
/// member set and host-role table are byte-derived data shared with the
/// accepted-artifact witness.
fn mutual_claim_lean_value(c: &Cert) -> String {
    let Cert::MutualRecursion {
        name,
        carrier,
        box_idx,
        sub_idx,
        scc,
        ..
    } = c.inner()
    else {
        unreachable!("audited mutual claim has a mutual-recursion shape")
    };
    format!(
        "({{ exportNameBytes := {}, exportName := {}, carrier := {carrier}, \
         memberSet := {}, hostTable := {}, obligation := AverCert.{name}Ob }} : \
         AverCert.AcceptedArtifact.MutualRecursionClaim)",
        render_byte_list(name.as_bytes()),
        lean_str(name),
        mutual_member_set_lean_value(scc),
        mutual_host_table_lean_value(*box_idx, *sub_idx),
    )
}

/// Render one SCC member's `{name}_mutualClaimAccepted` companion theorem as a
/// SPLIT proof, mirroring `render_mutual_claim_bundles` in `render_project.rs`.
///
/// This shared bridge module re-proves acceptance for every SCC member in a
/// standalone file, so it carried the same monolithic witness tuple the artifact
/// root did. Emitting the lowered body, code-entry bytes and function binding as
/// named `def`s and each byte-walking conjunct as its own leaf theorem keeps the
/// per-member kernel peak at the largest single leaf. The leaves are stated over
/// `AverCert.Plans.{name}MutualPlan`, so the aggregate re-runs no decode work.
fn render_mutual_bridge_claim_accepted(c: &Cert) -> String {
    let Cert::MutualRecursion {
        name,
        carrier,
        box_idx,
        sub_idx,
        position,
        scc,
        ..
    } = c.inner()
    else {
        unreachable!("audited mutual acceptance has a mutual-recursion shape")
    };
    let member = &scc[*position];
    let plan_cert = mutual_plan_from_cert(c).expect("audited mutual member has a canonical plan");
    let lowered_body = lower_expr_fragment_plan(&plan_cert, *carrier)
        .map(|ops| render_ops_value(&ops))
        .expect("audited mutual plan lowers to WInstr");
    let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(&plan_cert, *carrier)
        .expect("audited mutual plan lowers to exact code-entry bytes");
    let code_entry_bytes = render_byte_list(&code_entry_bytes);
    let export_name_bytes = render_byte_list(name.as_bytes());
    let host_table = mutual_host_table_lean_value(*box_idx, *sub_idx);
    let member_set = mutual_member_set_lean_value(scc);

    let body = format!("{name}MutualClaimBody");
    let code_entry = format!("{name}MutualClaimCodeEntry");
    let binding = format!("{name}MutualClaimBinding");
    let check_plan = format!("{name}MutualClaimCheckPlan");
    let lower_body = format!("{name}MutualClaimLowerBody");
    let lower_code = format!("{name}MutualClaimLowerCode");
    let func_binding = format!("{name}MutualClaimFuncBinding");
    let check_shape = format!("{name}MutualClaimCheckShape");
    let func_type = format!("{name}MutualClaimFuncType");
    let host_types = format!("{name}MutualClaimHostTypes");
    let plan = format!("AverCert.Plans.{name}MutualPlan");
    format!(
        "-- Witness data for `{name}` as named constants so no large literal is\n\
         -- duplicated across the leaf statements or baked into the aggregate term.\n\
         def {body} : List CertPrelude.WInstr := {lowered_body}\n\n\
         def {code_entry} : AverCert.WasmSlice.ByteSeq := {code_entry_bytes}\n\n\
         def {binding} : AverCert.WasmSlice.FuncBinding :=\n  \
           {{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry} }}\n\n\
         theorem {check_plan} :\n  \
           AverCert.PlanCheck.checkMutualRawPlan {plan} = true := by\n  \
           rfl\n\n\
         theorem {lower_body} :\n  \
           AverCert.PlanLower.lowerMutualBody {carrier} {plan} = some {body} := by\n  \
           rfl\n\n\
         theorem {lower_code} :\n  \
           AverCert.PlanBytes.lowerMutualCodeEntry {carrier} {plan} = some {code_entry} := by\n  \
           rfl\n\n\
         theorem {func_binding} :\n  \
           AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry} = some {binding} := by\n  \
           rfl\n\n\
         theorem {check_shape} :\n  \
           AverCert.PlanCheck.checkMutualPlanShape {member_set} {host_table} {plan} = true := by\n  \
           rfl\n\n\
         theorem {func_type} :\n  \
           AverCert.WasmSlice.funcTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {binding}.typeIdx {plan}.params.length {carrier} = true := by\n  \
           rfl\n\n\
         theorem {host_types} :\n  \
           AverCert.WasmSlice.hostTableFuncTypesMatch AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {carrier} {host_table} = true := by\n  \
           rfl\n\n\
         theorem {name}_mutualClaimAccepted :\n    \
           AverCert.AcceptedArtifact.mutualRecursionClaimAccepted\n      \
             AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen AverCert.manifest\n      \
             {name}_mutualClaim := by\n  \
           dsimp [{name}_mutualClaim,\n    \
             AverCert.AcceptedArtifact.mutualRecursionClaimAccepted,\n    \
             AverCert.AcceptedArtifact.mutualPlanForExport,\n    \
             AverCert.AcceptedArtifact.mutualPlanAccepted]\n  \
           exact ⟨rfl, rfl, rfl, {check_plan}, rfl, ⟨{body}, {code_entry}, {binding}, ⟨{lower_body}, {lower_code}, {func_binding}, rfl, {check_shape}, {func_type}, {host_types}, rfl⟩⟩⟩\n",
        self_idx = member.self_idx,
        type_idx = member.type_idx,
    )
}

/// Right-nested constructor for a conjunction of `count` reflexive facts.
fn mutual_rfl_conjunction(count: usize) -> String {
    debug_assert!(count >= 2);
    (0..count - 2).fold("⟨rfl, rfl⟩".to_string(), |tail, _| {
        format!("⟨rfl, {tail}⟩")
    })
}

/// Exhaust a concrete `Fin k` by `Fin.cases`, closing each inhabited branch
/// with `rfl`.  The final successor branch is `Fin 0` and closes by elimination.
fn render_mutual_fin_rfl_cases(k: usize, initial_indent: &str) -> String {
    debug_assert!(k >= 2);
    let mut out = String::new();
    let mut indent = initial_indent.to_string();
    for _ in 0..k {
        out.push_str(&format!("{indent}refine Fin.cases ?_ ?_ i\n"));
        out.push_str(&format!("{indent}· rfl\n"));
        out.push_str(&format!("{indent}· intro i\n"));
        indent.push_str("  ");
    }
    out.push_str(&format!("{indent}exact Fin.elim0 i"));
    out
}

/// Definitions shared by every option-(b) bridge in one SCC: concrete members,
/// plans, raw edges, the audited `AdmittedScc`, claims, and byte acceptance.
/// Emitted once by the primary (lowest-self-index) member.
fn render_mutual_shared_bridge_data(c: &Cert) -> String {
    let Cert::MutualRecursion {
        position,
        carrier,
        box_idx,
        sub_idx,
        scc,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    if *position != 0 {
        return String::new();
    }
    let primary = &scc[0].name;
    let k = scc.len();
    let member_set = mutual_member_set_lean_value(scc);
    let members = scc
        .iter()
        .map(|member| {
            let cross = scc
                .iter()
                .position(|candidate| candidate.self_idx == member.cross_idx)
                .expect("mutual cross target is an SCC member");
            format!(
                "({{ self := {}, base := {}, cross := ⟨{cross}, by omega⟩ }} : \
                 MutualRecursionSoundness.MemberU {k})",
                member.self_idx,
                lean_int_lit(member.base_k),
            )
        })
        .collect::<Vec<_>>()
        .join(",\n    ");
    let plans = scc
        .iter()
        .map(|member| format!("AverCert.Plans.{}MutualPlan", member.name))
        .collect::<Vec<_>>()
        .join(", ");
    let edges = scc
        .iter()
        .map(|member| format!("({}, {}, {member_set})", member.self_idx, member.cross_idx))
        .collect::<Vec<_>>()
        .join(", ");
    let claims = scc
        .iter()
        .map(|member| format!("{}_mutualClaim", member.name))
        .collect::<Vec<_>>()
        .join(", ");
    let accepted = scc.iter().rev().fold("trivial".to_string(), |tail, member| {
        format!("⟨{}_mutualClaimAccepted, {tail}⟩", member.name)
    });
    let lowered = render_mutual_fin_rfl_cases(k, "      ");

    let mut out = format!(
        r#"/-! ### {primary} — option-(b) mutual SCC data -/

def {primary}_mutualMembers : Fin {k} → MutualRecursionSoundness.MemberU {k} := fun i =>
  [{members}].get i

def {primary}_mutualPlans : Fin {k} → MutualRawPlan := fun i =>
  [{plans}].get i

def {primary}_mutualEdges : List (Nat × Nat × List Nat) :=
  [{edges}]

def {primary}_mutualScc : MutualRecursionSoundness.AdmittedScc {k} {carrier} {box_idx} {sub_idx} :=
  {{ members := {primary}_mutualMembers
    plans := {primary}_mutualPlans
    rawEdges := {primary}_mutualEdges
    edgesBound := by decide
    closed := by decide
    checked := by decide
    shaped := by decide
    lowered := by
      intro i
{lowered} }}

"#,
    );
    for member in scc {
        let member_cert = Cert::MutualRecursion {
            name: member.name.clone(),
            self_idx: member.self_idx,
            carrier: *carrier,
            box_idx: *box_idx,
            sub_idx: *sub_idx,
            position: scc
                .iter()
                .position(|candidate| candidate.self_idx == member.self_idx)
                .expect("member belongs to SCC"),
            scc: scc.clone(),
        };
        let wrapped = Cert::NonRecursive {
            inner: Box::new(member_cert),
        };
        let claim = mutual_claim_lean_value(&wrapped);
        let claim_accepted = render_mutual_bridge_claim_accepted(&wrapped);
        out.push_str(&format!(
            r#"def {name}_mutualClaim : AverCert.AcceptedArtifact.MutualRecursionClaim :=
  {claim}

{claim_accepted}
"#,
            name = member.name,
        ));
    }
    out.push_str(&format!(
        r#"def {primary}_mutualClaims : List AverCert.AcceptedArtifact.MutualRecursionClaim :=
  [{claims}]

def {primary}_mutualArtifact : AverCert.AcceptedArtifact.ArtifactData :=
  {{ modBytes := AverCert.ArtifactBytes.modBytes,
    modLen := AverCert.ArtifactBytes.modLen, manifest := AverCert.manifest,
    symFragmentClaims := [], stringEqClaims := [], stringConcatClaims := [],
    constructClaims := [], recursionClaims := [],
    mutualRecursionClaims := {primary}_mutualClaims,
    verbatimClaims := [], intDispatchClaims := [], fieldProjectionClaims := [],
    compositionMembers := [], compositionClaims := [], closureFuel := 0,
    closureClaim := {{ roots := [], helpers := [], admitted := [] }} }}

theorem {primary}_mutualFragmentsAccepted :
    AverCert.AcceptedArtifact.acceptedMutualRecursionFragments
      {primary}_mutualArtifact := by
  dsimp [AverCert.AcceptedArtifact.acceptedMutualRecursionFragments,
    AverCert.AcceptedArtifact.mutualRecursionClaimsAccepted,
    AverCert.AcceptedArtifact.allClaims,
    AverCert.AcceptedArtifact.mutualClaimsFormClosedSccs,
    AverCert.AcceptedArtifact.mutualClaimEdges,
    AverCert.AcceptedArtifact.mutualClaimEdge,
    AverCert.AcceptedArtifact.mutualPlanForExport,
    AverCert.AcceptedArtifact.mutualPlanTarget,
    AverCert.AcceptedArtifact.mutualMembersFormClosedSccs,
    AverCert.AcceptedArtifact.followSccCycle,
    AverCert.AcceptedArtifact.natEdgeLookup,
    AverCert.AcceptedArtifact.natListNodup,
    AverCert.AcceptedArtifact.natListSetEq,
    {primary}_mutualArtifact, {primary}_mutualClaims]
  exact ⟨{accepted}, rfl⟩

"#,
    ));
    out
}

/// Option-(b) residual for one mutual export.  A simultaneous fuel induction
/// relates every source member to the plan-derived k-generic evaluator; the
/// selected member then supplies the represented-domain relation required by
/// `mutualSemanticBridge`.  Wasm execution and totality stay in the audited wall.
fn render_mutual_semantic_bridge(c: &Cert, model_info: &ModelInfo) -> String {
    let Cert::MutualRecursion {
        name,
        position,
        box_idx,
        sub_idx,
        scc,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let model_name = c.model_lean_name(model_info);
    // Every member's model function, by its qualified Lean identifier.
    // `model_citation_gate` covers every member of the SCC, and both `analyze`
    // and `write_project` enforce it, so each resolves.
    let member_model = |member: &MutualMember| -> String {
        model_info
            .model_lean_name(&member.name)
            .expect("model-citing certificate passed the qualified-name gate")
    };
    let primary = &scc[0].name;
    let k = scc.len();
    let model_fuel = scc
        .iter()
        .enumerate()
        .map(|(member_pos, member)| {
            format!(
                "MutualRecursionSoundness.evalMutualUFuel {primary}_mutualMembers fuel \
                 ⟨{member_pos}, by omega⟩ n = {}__fuel fuel n",
                member_model(member),
            )
        })
        .collect::<Vec<_>>()
        .join(" ∧\n      ");
    let source_fuels = scc
        .iter()
        .map(|member| format!("{}__fuel", member_model(member)))
        .collect::<Vec<_>>()
        .join(", ");
    let zero = mutual_rfl_conjunction(k);
    let projection = conjunct_proj(*position, k);
    let fin_cases = render_mutual_fin_rfl_cases(k, "    ");
    format!(
        r#"/-! ### {name} — option-(b) mutual semantic bridge -/

theorem {name}_mutualSemanticBridge :
    AcceptanceSoundness.mutualSemanticBridge {primary}_mutualArtifact
      {name}_mutualClaim AverCert.Plans.{name}MutualPlan := by
  have hModelFuel : ∀ fuel n,
      {model_fuel} := by
    intro fuel
    induction fuel with
    | zero => intro n; exact {zero}
    | succ fuel ih =>
        intro n
        simp only [MutualRecursionSoundness.evalMutualUFuel, {source_fuels}]
        split <;> simp_all [{primary}_mutualMembers]
  have hModel : ∀ n,
      MutualRecursionSoundness.evalMutualU {primary}_mutualMembers
        ⟨{position}, by omega⟩ n = {model_name} n := by
    intro n
    simpa [MutualRecursionSoundness.evalMutualU, {model_name}] using
      (hModelFuel (n.natAbs + 1) n){projection}
  refine ⟨{k}, {box_idx}, {sub_idx}, {primary}_mutualScc,
    ⟨{position}, by omega⟩, rfl, rfl, rfl, rfl, ?_, ?_, ?_⟩
  · intro i _hi
{fin_cases}
  · intro add sub mul stringEq stringConcat toIndex cmp eq
    refine ⟨rfl, rfl, ?_⟩
    intro i
{fin_cases}
  · intro S ns vs hDom
    rcases hDom with ⟨hRepr, hLen⟩
    cases hRepr with
    | nil => simp at hLen
    | cons hv htail =>
        rename_i n v ns vs
        cases htail with
        | nil =>
            refine ⟨n, v, rfl, hv, ?_⟩
            intro w hw
            change S.Repr (MutualRecursionSoundness.evalMutualU {primary}_mutualMembers
              ⟨{position}, by omega⟩ n) w at hw
            rw [hModel n] at hw
            simpa [{name}_mutualClaim, AverCert.{name}Ob,
              AverCert.Schema.intRepr] using hw
        | cons _ _ => simp at hLen
#print axioms {name}_mutualSemanticBridge
"#,
    )
}
