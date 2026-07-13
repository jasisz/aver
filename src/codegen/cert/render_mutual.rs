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

/// Concrete byte/plan acceptance for one member of the shared SCC.  This is
/// data reconstruction only; source-model semantics lives in the bridge below.
fn mutual_claim_acceptance_proof(c: &Cert) -> String {
    let Cert::MutualRecursion {
        carrier,
        position,
        scc,
        ..
    } = c.inner()
    else {
        unreachable!("audited mutual acceptance has a mutual-recursion shape")
    };
    let member = &scc[*position];
    let plan = mutual_plan_from_cert(c).expect("audited mutual member has a canonical plan");
    let body = lower_expr_fragment_plan(&plan, *carrier)
        .map(|ops| render_ops_value(&ops))
        .expect("audited mutual plan lowers to WInstr");
    let bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier)
        .expect("audited mutual plan lowers to exact code-entry bytes");
    let bytes = render_byte_list(&bytes);
    let binding = format!(
        "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {bytes} }} : \
         AverCert.WasmSlice.FuncBinding)",
        member.self_idx, member.code_idx, member.type_idx,
    );
    format!(
        "⟨rfl, rfl, rfl, rfl, rfl, ⟨({body}), ({bytes}), {binding}, \
         ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
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
                 V3Mutual.MemberU {k})",
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

def {primary}_mutualMembers : Fin {k} → V3Mutual.MemberU {k} := fun i =>
  [{members}].get i

def {primary}_mutualPlans : Fin {k} → MutualRawPlan := fun i =>
  [{plans}].get i

def {primary}_mutualEdges : List (Nat × Nat × List Nat) :=
  [{edges}]

def {primary}_mutualScc : V3Mutual.AdmittedScc {k} {carrier} {box_idx} {sub_idx} :=
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
        let acceptance = mutual_claim_acceptance_proof(&wrapped);
        out.push_str(&format!(
            r#"def {name}_mutualClaim : AverCert.AcceptedArtifact.MutualRecursionClaim :=
  {claim}

theorem {name}_mutualClaimAccepted :
    AverCert.AcceptedArtifact.mutualRecursionClaimAccepted
      AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen AverCert.manifest
      {name}_mutualClaim := by
  dsimp [{name}_mutualClaim,
    AverCert.AcceptedArtifact.mutualRecursionClaimAccepted,
    AverCert.AcceptedArtifact.mutualPlanForExport,
    AverCert.AcceptedArtifact.mutualPlanAccepted]
  exact {acceptance}

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
/// selected member then supplies both domain directions required by
/// `mutualSemanticBridge`.  Wasm execution and totality stay in the audited wall.
fn render_mutual_semantic_bridge(c: &Cert) -> String {
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
    let primary = &scc[0].name;
    let k = scc.len();
    let model_fuel = scc
        .iter()
        .enumerate()
        .map(|(member_pos, member)| {
            format!(
                "V3Mutual.evalMutualUFuel {primary}_mutualMembers fuel \
                 ⟨{member_pos}, by omega⟩ n = {}__fuel fuel n",
                member.name,
            )
        })
        .collect::<Vec<_>>()
        .join(" ∧\n      ");
    let source_fuels = scc
        .iter()
        .map(|member| format!("{}__fuel", member.name))
        .collect::<Vec<_>>()
        .join(", ");
    let zero = mutual_rfl_conjunction(k);
    let projection = conjunct_proj(*position, k);
    let fin_cases = render_mutual_fin_rfl_cases(k, "    ");
    format!(
        r#"/-! ### {name} — option-(b) mutual semantic bridge -/

theorem {name}_mutualSemanticBridge :
    V3Master.mutualSemanticBridge {primary}_mutualArtifact
      {name}_mutualClaim AverCert.Plans.{name}MutualPlan := by
  have hModelFuel : ∀ fuel n,
      {model_fuel} := by
    intro fuel
    induction fuel with
    | zero => intro n; exact {zero}
    | succ fuel ih =>
        intro n
        simp only [V3Mutual.evalMutualUFuel, {source_fuels}]
        split <;> simp_all [{primary}_mutualMembers]
  have hModel : ∀ n,
      V3Mutual.evalMutualU {primary}_mutualMembers
        ⟨{position}, by omega⟩ n = {name} n := by
    intro n
    simpa [V3Mutual.evalMutualU, {name}] using
      (hModelFuel (n.natAbs + 1) n){projection}
  refine ⟨{k}, {box_idx}, {sub_idx}, {primary}_mutualScc,
    ⟨{position}, by omega⟩, rfl, rfl, rfl, rfl, ?_, ?_, ?_, ?_⟩
  · intro i _hi
{fin_cases}
  · intro add sub mul stringEq stringConcat
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
            change S.Repr (V3Mutual.evalMutualU {primary}_mutualMembers
              ⟨{position}, by omega⟩ n) w at hw
            rw [hModel n] at hw
            simpa [{name}_mutualClaim, AverCert.{name}Ob,
              AverCert.Schema.intRepr] using hw
        | cons _ _ => simp at hLen
  · intro S n v hv
    refine ⟨[n], ⟨ReprAll.cons hv ReprAll.nil, rfl⟩, ?_⟩
    intro w hw
    change S.Repr (V3Mutual.evalMutualU {primary}_mutualMembers
      ⟨{position}, by omega⟩ n) w at hw
    rw [hModel n] at hw
    simpa [{name}_mutualClaim, AverCert.{name}Ob,
      AverCert.Schema.intRepr] using hw

#print axioms {name}_mutualSemanticBridge
"#,
    )
}
