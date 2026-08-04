/// Classification of every user function in the module.
pub struct Analysis {
    certs: Vec<Cert>,
    declined: Vec<(String, String)>,
    module_envelope: ModuleEnvelopeFacts,
    carrier: Option<u32>,
    contracts: Vec<String>,
    /// The strict byte-derived host-role table (S1 criteria: carrier-binop
    /// signature + strict first-i64-arith + uniqueness, fail-closed to `None`
    /// per role). The Int-face dispatch plan gate requires every role index a
    /// plan claim would cite to be confirmed by THIS table; the coarse
    /// `host_roles` marker map alone would assign a both-ops helper by its
    /// first arithmetic instead of rejecting it as ambiguous.
    frag_host_table: FragHostTable,
    /// String.eq/String.concat helpers, ordered by wasm function index. Unlike
    /// add/sub, each matching helper is retained independently (no uniqueness
    /// decline), exactly as the Rust classifier and audited kernel classifier do.
    string_host_roles: StringHostRoles,
    /// Declarable ADT envelopes by constructor type index. User-ADT claims
    /// declare an envelope from this table; a claim whose ADT is not
    /// extractable declines during analysis (fail-closed).
    declared_envelopes: DeclaredEnvelopes,
}

impl Analysis {
    pub fn certified_names(&self) -> Vec<String> {
        self.certs.iter().map(|c| c.name().to_string()).collect()
    }
    pub fn declined(&self) -> &[(String, String)] {
        &self.declined
    }
}

/// Collect the tags the checked cascade tests, in dispatch order.
fn int_dispatch_test_tags(body: &IntDispatchCascade) -> Vec<u32> {
    let mut tags = Vec::new();
    let mut cursor = body;
    while let IntDispatchCascade::Test { ty_idx, rest, .. } = cursor {
        tags.push(*ty_idx);
        cursor = rest;
    }
    tags
}

/// Collect the tags the checked cascade tests, in dispatch order, with whether
/// each arm BINDS a payload (`Proj`/`HostOp`) or is a `Const` (nullary) arm that
/// reads no field. A binding arm's tag must be a declared Int-payload (`hit`)
/// constructor; a const arm's tag need only be a declared constructor of any
/// shape (mirrors the wall's leaf-aware `dCascadeInEnv`).
fn int_dispatch_test_arms(body: &IntDispatchCascade) -> Vec<(u32, bool)> {
    let mut arms = Vec::new();
    let mut cursor = body;
    while let IntDispatchCascade::Test { ty_idx, hit, rest } = cursor {
        let binds = !matches!(hit, IntDispatchLeaf::Const { .. });
        arms.push((*ty_idx, binds));
        cursor = rest;
    }
    arms
}

/// Fail-closed declared-envelope gate for user-ADT claims: an Int-dispatch or
/// named-ADT constructor certificate is kept only when its ADT's envelope is
/// extractable and the claim's byte-pinned indices are declared hit
/// constructors of that envelope. Everything else keeps its existing route.
fn declared_envelope_gate(
    c: &Cert,
    model_info: &ModelInfo,
    frag_host_table: FragHostTable,
    envelopes: &DeclaredEnvelopes,
) -> Result<(), String> {
    match c.inner() {
        Cert::VariantDispatch { .. } | Cert::WidenedIntMatch { .. } => {
            let plan = int_dispatch_plan_from_cert(c, frag_host_table).ok_or_else(|| {
                "declared-envelope gate: no byte-matching int-dispatch plan".to_string()
            })?;
            let arms = int_dispatch_test_arms(&plan.body);
            let (first, _) = *arms.first().ok_or_else(|| {
                "declared-envelope gate: dispatch plan tests no variant".to_string()
            })?;
            let envelope = envelopes.for_ctor(first).ok_or_else(|| {
                "user ADT layout is outside the declarable envelope vocabulary; declined"
                    .to_string()
            })?;
            let hits = envelope.hit_indices();
            for (tag, binds) in &arms {
                if *binds {
                    // Payload-binding arm: the tag must be a declared Int-payload
                    // (`hit`) constructor, since the arm projects its field.
                    if !hits.contains(tag) {
                        return Err(format!(
                            "dispatch tests variant {tag} which is not a declared Int-payload constructor; declined"
                        ));
                    }
                } else {
                    // Const (nullary) arm: reads no field, so the tag need only be
                    // a declared constructor of any shape.
                    if !envelope.ctors.iter().any(|c| c.idx == *tag) {
                        return Err(format!(
                            "dispatch tests variant {tag} which is not a declared constructor; declined"
                        ));
                    }
                }
            }
            Ok(())
        }
        Cert::AdtConstructor { struct_idx, .. }
            if adt_constructor_uses_model(c, model_info) =>
        {
            let envelope = envelopes.for_ctor(*struct_idx).ok_or_else(|| {
                "user ADT layout is outside the declarable envelope vocabulary; declined"
                    .to_string()
            })?;
            if !envelope.hit_indices().contains(struct_idx) {
                return Err(format!(
                    "constructed variant {struct_idx} is not a declared Int-payload constructor; declined"
                ));
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

/// Disassemble the emitted module and classify each user function. `model_files`
/// are the reused `aver proof` Lean model; the recursion classifier reads the
/// combinator operator (`+`/`*`) from them since the bytes cannot tell the bignum
/// helpers apart.
pub fn analyze(wasm_bytes: &[u8], model_files: &[(String, String)]) -> Result<Analysis, String> {
    analyze_with_fragment_plans(wasm_bytes, model_files, &[])
}

pub fn analyze_with_fragment_plans(
    wasm_bytes: &[u8],
    model_files: &[(String, String)],
    fragment_plans: &[FragmentPlanArtifact],
) -> Result<Analysis, String> {
    let (user_fns, box_idx, user_idx_set, carrier, host_roles, frag_host_table, struct_field_counts) =
        disassemble(wasm_bytes)?;
    let string_host_roles = string_host_roles(&host_roles);
    let model_ops = model_step_ops(model_files);
    let model_info = ModelInfo::from_files(model_files);

    // Index the user functions so the composition pass can walk the call graph.
    let fns: std::collections::HashMap<u32, &UserFn> =
        user_fns.iter().map(|f| (f.wasm_idx, f)).collect();
    let user_names: std::collections::HashSet<&str> =
        user_fns.iter().map(|f| f.name.as_str()).collect();
    let mut producer_plans = std::collections::HashMap::<&str, &FragmentPlan>::new();
    for artifact in fragment_plans {
        if !user_names.contains(artifact.export_name.as_str()) {
            return Err(format!(
                "producer supplied fragment plan for unknown export `{}`",
                artifact.export_name
            ));
        }
        if producer_plans
            .insert(artifact.export_name.as_str(), &artifact.plan)
            .is_some()
        {
            return Err(format!(
                "producer supplied duplicate fragment plan for `{}`",
                artifact.export_name
            ));
        }
    }

    let mut certs = Vec::new();
    let mut declined = Vec::new();
    for f in &user_fns {
        if let Some(plan) = producer_plans.get(f.name.as_str()) {
            let checked = match plan {
                FragmentPlan::Sym(plan) => {
                    check_sym_fragment_plan_object(wasm_bytes, &f.name, (*plan).clone())
                }
                FragmentPlan::Expr(plan) => {
                    check_expr_fragment_plan_object(wasm_bytes, &f.name, (*plan).clone())
                }
            };
            match checked {
                Ok((_func_order, cert, _sidecar, true, _reason)) => certs.push(cert),
                Ok((_func_order, _cert, _sidecar, false, reason)) => declined.push((
                    f.name.clone(),
                    format!(
                        "producer fragment plan does not match emitted wasm: {}",
                        reason.unwrap_or_else(|| "unknown mismatch".to_string())
                    ),
                )),
                Err(reason) => declined.push((
                    f.name.clone(),
                    format!("producer fragment plan rejected: {reason}"),
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
            &ClassifierContext {
                host_roles: &host_roles,
                struct_field_counts: &struct_field_counts,
                model_ops: &model_ops,
            },
        ) {
            Ok(c) => {
                if has_complete_artifact_plan(&c, &model_info, frag_host_table) {
                    certs.push(c);
                } else {
                    declined.push((
                        f.name.clone(),
                        "classified certificate has no byte-matching artifact plan; declined before rendering"
                            .to_string(),
                    ));
                }
            }
            Err(reason) => declined.push((f.name.clone(), reason)),
        }
    }

    // Declared-envelope gate: user-ADT claims must be declarable against the
    // module's type section or they decline before rendering.
    let declared_envelopes = collect_declared_envelopes(wasm_bytes, carrier)?;
    let mut gated_certs = Vec::new();
    for c in certs {
        match declared_envelope_gate(&c, &model_info, frag_host_table, &declared_envelopes) {
            Ok(()) => gated_certs.push(c),
            Err(reason) => declined.push((c.name().to_string(), reason)),
        }
    }
    let certs = gated_certs;

    // Qualified-model-name gate: every class whose generated Lean cites the
    // source model by name must resolve that name to the identifier that
    // actually exists in the emitted model module — the export name itself at
    // entry level, the dotted namespace form for a dependency-module function.
    // An export whose citation cannot be derived declines here (fail-closed)
    // instead of rendering a name Lean would reject.
    let mut named_certs = Vec::new();
    for c in certs {
        match model_citation_gate(&c, &model_info) {
            Ok(()) => named_certs.push(c),
            Err(reason) => declined.push((c.name().to_string(), reason)),
        }
    }
    let certs = named_certs;

    // Named runtime contracts actually consumed by the certified functions.
    let contracts = runtime_contracts_for_certs(&certs);
    let certified = certs
        .iter()
        .map(|cert| (cert.name().to_string(), cert.self_idx()))
        .collect::<Vec<_>>();
    let module_envelope = collect_module_envelope_facts(wasm_bytes, &certified)?;

    Ok(Analysis {
        certs,
        declined,
        module_envelope,
        carrier,
        contracts,
        frag_host_table,
        string_host_roles,
        declared_envelopes,
    })
}

/// Every export name this certificate's renderers cite as a MODEL identifier
/// in generated Lean. Classes that cite no model function (verbatim packs,
/// field projections, string leaves, envelope-modeled dispatch/constructors,
/// plan-modeled fragments) require nothing here; their per-class lookups
/// already skip or decline on a miss.
fn model_citation_names(c: &Cert) -> Vec<String> {
    match c.inner() {
        Cert::Recursive { name, .. } | Cert::AccumulatorRecursive { name, .. } => {
            vec![name.clone()]
        }
        // The bridge's simultaneous fuel induction cites EVERY member's model.
        Cert::MutualRecursion { scc, .. } => {
            scc.iter().map(|member| member.name.clone()).collect()
        }
        // The composition model and its simp set cite the root and every
        // closure member.
        Cert::Composition { name, closure, .. } => std::iter::once(name.clone())
            .chain(closure.iter().map(|entry| entry.name.clone()))
            .collect(),
        // Audited integer/Bool fragments (including the straight-line add
        // face) state `model := <source fn>`; plan-modeled (float) fragments
        // cite no model name. The two Int comparison faces are wall-modeled —
        // `intCmpModel`/`intSelectModel` name no source function — so they cite
        // nothing either, even though their types sit inside the audited gate.
        Cert::ExprFragment { name, .. }
            if c.int_cmp_face().is_none()
                && (expr_fragment_uses_audited_generic(c) || c.int_add_face().is_some()) =>
        {
            vec![name.clone()]
        }
        _ => Vec::new(),
    }
}

/// Fail-closed: decline any cert whose model citation cannot be derived.
fn model_citation_gate(c: &Cert, model_info: &ModelInfo) -> Result<(), String> {
    for name in model_citation_names(c) {
        if model_info.model_lean_name(&name).is_none() {
            return Err(format!(
                "model identifier for `{name}` cannot be resolved to a definition \
                 in the emitted Lean model; declined"
            ));
        }
    }
    Ok(())
}

fn has_complete_artifact_plan(
    c: &Cert,
    model_info: &ModelInfo,
    frag_host_table: FragHostTable,
) -> bool {
    match c.inner() {
        Cert::ExprFragment { .. } => true,
        Cert::StringEqVerbatimMatch { .. } => string_eq_plan_from_cert(c).is_some(),
        Cert::StringConcatVerbatimMatch { .. } => string_concat_plan_from_cert(c).is_some(),
        Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. } => {
            recursion_plan_from_cert(c).is_some()
        }
        Cert::MutualRecursion { .. } => mutual_plan_from_cert(c).is_some(),
        Cert::Composition { .. } => composition_plans_from_cert(c, frag_host_table).is_some(),
        Cert::VerbatimWidenedMatch { .. } | Cert::VerbatimVariantDispatch { .. } => {
            verbatim_plan_from_cert(c).is_some()
        }
        Cert::VariantDispatch { .. } | Cert::WidenedIntMatch { .. } => {
            int_dispatch_plan_from_cert(c, frag_host_table).is_some()
        }
        Cert::AdtConstructor { .. } => {
            adt_constructor_sym_plan_from_cert(c, model_info).is_some()
                && construct_plan_from_cert(c).is_some()
        }
        Cert::FieldProjection { .. } => field_projection_plan_from_cert(c).is_some(),
        Cert::NonRecursive { .. } => unreachable!(),
    }
}

fn runtime_contracts_for_certs<'a>(certs: impl IntoIterator<Item = &'a Cert>) -> Vec<String> {
    let mut contracts = Vec::new();
    let mut has_box = false;
    let mut has_add = false;
    let mut has_sub = false;
    let mut has_mul = false;
    let mut has_string_eq = false;
    let mut has_string_concat = false;
    let mut has_to_index = false;
    let mut has_cmp = false;
    let mut has_eq = false;
    let mut has_add_total = false;
    let mut has_sub_total = false;
    let mut has_mul_total = false;
    for c in certs {
        if c.policy() == CertificationPolicy::SimulatesModelTotally {
            has_add_total = true;
            has_sub_total = true;
            has_mul_total |= c.requires_mul_totality();
        }
        if c.int_add_face().is_some() {
            has_box = true;
            has_add = true;
            continue;
        }
        if c.tag_dispatch_face().is_some() {
            // Both arms box their Int constant (twin of the wall's
            // `useFragBlockFuel` role accounting for the encoded plan).
            has_box = true;
            continue;
        }
        if c.vector_get_face().is_some() {
            // The fused read calls the to-index helper and boxes the default.
            has_box = true;
            has_to_index = true;
            continue;
        }
        // Each comparison face reads exactly ONE helper: `__aint_eq` for the
        // equality operator, `__aint_cmp` for the three relational ones. Twin
        // of the wall's `useHostRole` accounting over the encoded plan.
        if let Some(face) = c.int_cmp_face() {
            match face.op {
                FragIntCmpOp::Eq => has_eq = true,
                FragIntCmpOp::Lt | FragIntCmpOp::Gt | FragIntCmpOp::Ge => has_cmp = true,
            }
            continue;
        }
        match c.inner() {
            Cert::Recursive {
                combinator: Combinator::Add,
                ..
            } => {
                has_box = true;
                has_add = true;
                has_sub = true;
            }
            Cert::Recursive {
                combinator: Combinator::Mul,
                ..
            } => {
                has_box = true;
                has_mul = true;
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
    if has_mul {
        contracts.push(INT_MUL_CONTRACT.to_string());
    }
    if has_string_eq {
        contracts.push(STRING_EQ_CONTRACT.to_string());
    }
    if has_string_concat {
        contracts.push(STRING_CONCAT_CONTRACT.to_string());
    }
    if has_to_index {
        contracts.push(TO_INDEX_CONTRACT.to_string());
    }
    if has_cmp {
        contracts.push(CMP_CONTRACT.to_string());
    }
    if has_eq {
        contracts.push(EQ_CONTRACT.to_string());
    }
    if has_add_total {
        contracts.push(INT_ADD_TOTAL_CONTRACT.to_string());
    }
    if has_sub_total {
        contracts.push(INT_SUB_TOTAL_CONTRACT.to_string());
    }
    if has_mul_total {
        contracts.push(INT_MUL_TOTAL_CONTRACT.to_string());
    }
    contracts
}

#[cfg(test)]
mod analysis_without_box_helper_tests {
    /// A valid module WITHOUT the `__rt_aint_from_i64` export must still
    /// analyze end-to-end: `analyze` returns `Ok`, and the user export is
    /// either certified by a carrier-free class or declared uncertified with a
    /// readable reason — never a whole-module `Err` that aborts `--certify`.
    #[test]
    fn analyze_accepts_module_without_box_helper() {
        let bytes = wat::parse_str(
            r#"(module
  (type $t (func (param i64) (result i64)))
  (func $identity (type $t) local.get 0)
  (export "identity" (func $identity))
)"#,
        )
        .expect("no-box-helper module WAT parses");
        let analysis = super::analyze(&bytes, &[])
            .expect("a module without the Int box helper must analyze, not abort");
        assert!(
            analysis.frag_host_table.box_idx.is_none(),
            "the host-role table must leave the box role unbound, never invent an index"
        );
        let certified = analysis.certified_names();
        if !certified.contains(&"identity".to_string()) {
            let reason = analysis
                .declined()
                .iter()
                .find(|(name, _)| name == "identity")
                .map(|(_, reason)| reason.as_str())
                .expect("identity must be declared uncertified when it does not certify");
            assert!(
                reason.contains("__rt_aint_from_i64"),
                "decline reason should name the missing Int carrier helpers, got: {reason}"
            );
        }
    }

    /// Adversarial half-runtime module: the Int carrier STRUCT TYPE exists,
    /// but the `__rt_aint_from_i64` box helper export does not, so every
    /// integer-family recognizer sees `carrier = Some(..)` with
    /// `box_idx = None`. Each shape below drives one classify path past its
    /// `carrier` admission into the `box_idx` decline:
    ///   - `descend` (self-recursive on the carrier) — `classify_recursion.rs`
    ///     (both the fueled-recursion and mutual-SCC recognizers);
    ///   - `dispatch` (sum-root in, scalar out, parseable straight-line body)
    ///     — `classify_variant_dispatch.rs` via `walk_nonrecursive`;
    ///   - `probe` (calls an unexported, role-free helper) —
    ///     `classify_structural.rs`, where the only non-host call cannot be
    ///     the absent box helper.
    /// All three must decline fail-closed with the readable no-Int-helper
    /// reason; nothing may panic, certify, or invent a box index.
    #[test]
    fn analyze_declines_carrier_without_box_helper_across_classify_paths() {
        let bytes = wat::parse_str(
            r#"(module
  (type $carrier (struct (field i64) (field (ref null $carrier)) (field i32)))
  (type $sum (struct (field i32)))
  (func $descend (param (ref null $carrier)) (result (ref null $carrier))
    local.get 0
    call $descend)
  (func $dispatch (param (ref null $sum)) (result i64)
    i64.const 5)
  (func $helper (param (ref null $sum)) (result i64)
    i64.const 1)
  (func $probe (param (ref null $sum)) (result i64)
    local.get 0
    call $helper)
  (func $addlike (param (ref null $carrier) (ref null $carrier)) (result (ref null $carrier))
    (local i64 i64)
    local.get 2
    local.get 3
    i64.add
    drop
    local.get 0)
  (export "descend" (func $descend))
  (export "dispatch" (func $dispatch))
  (export "probe" (func $probe))
)"#,
        )
        .expect("carrier-without-box-helper module WAT parses");
        let analysis = super::analyze(&bytes, &[])
            .expect("a carriered module without the box helper must analyze, not abort");
        assert!(
            analysis.carrier.is_some(),
            "the fixture deliberately carries the Int carrier struct type"
        );
        assert!(
            analysis.frag_host_table.box_idx.is_none(),
            "no `__rt_aint_from_i64` export means no box role, ever"
        );
        assert!(
            analysis.frag_host_table.add_idx.is_some(),
            "the byte-derived add role binds independently of the box helper"
        );
        assert!(
            analysis.certified_names().is_empty(),
            "no integer-family shape may certify without the box helper, got {:?}",
            analysis.certified_names()
        );
        for export in ["descend", "dispatch", "probe"] {
            let reason = analysis
                .declined()
                .iter()
                .find(|(name, _)| name == export)
                .map(|(_, reason)| reason.as_str())
                .unwrap_or_else(|| panic!("`{export}` must be declared uncertified"));
            assert!(
                reason.contains("__rt_aint_from_i64"),
                "`{export}` must decline with the no-Int-helper reason, got: {reason}"
            );
        }
    }
}

#[cfg(test)]
mod poisoned_role_scan_tests {
    /// A module WITH the `__rt_aint_from_i64` export and one carrier-binop-
    /// signature function whose body starts with an instruction encoding the
    /// certificate decoder's role scan does not support (`ref.as_non_null`).
    const POISONED_WAT: &str = r#"(module
  (type $carrier (struct (field i64) (field anyref) (field i32)))
  (func $box (param i64) (result (ref null $carrier))
    local.get 0
    ref.null any
    i32.const 0
    struct.new $carrier)
  (func $add (param (ref null $carrier) (ref null $carrier)) (result (ref null $carrier))
    local.get 0
    struct.get $carrier 0
    local.get 1
    struct.get $carrier 0
    i64.add
    ref.null any
    i32.const 0
    struct.new $carrier)
  (func $unrelated (param (ref null $carrier) (ref null $carrier)) (result (ref null $carrier))
    local.get 0
    ref.as_non_null)
  (export "__rt_aint_from_i64" (func $box))
)"#;

    /// A module carrying the Int box helper whose module-wide role scan the
    /// certificate decoder cannot complete has NO manifest declaration that
    /// satisfies its acceptance pin. The producer must refuse it outright —
    /// emitting the package would only defer the failure to verification.
    #[test]
    fn analyze_refuses_box_helper_module_with_unscannable_carrier_binop_body() {
        let bytes = wat::parse_str(POISONED_WAT).expect("poisoned module WAT parses");
        let error = match super::analyze(&bytes, &[]) {
            Err(error) => error,
            Ok(_) => {
                panic!("a box-helper module with an unscannable carrier-binop body must be refused")
            }
        };
        assert!(
            error.contains("__rt_aint_from_i64"),
            "the refusal must name the box helper, got: {error}"
        );
        assert!(
            error.contains("role scan") && error.contains("function index 2"),
            "the refusal must name the failed role scan and the offending function, got: {error}"
        );
    }

    /// Control: the same module without the unscannable function analyzes and
    /// binds its box and add roles.
    #[test]
    fn analyze_accepts_box_helper_module_when_every_carrier_binop_body_scans() {
        let healthy = POISONED_WAT.replace(
            r#"  (func $unrelated (param (ref null $carrier) (ref null $carrier)) (result (ref null $carrier))
    local.get 0
    ref.as_non_null)
"#,
            "",
        );
        assert_ne!(healthy, POISONED_WAT, "the control must drop the poisoned function");
        let bytes = wat::parse_str(&healthy).expect("healthy module WAT parses");
        let analysis = super::analyze(&bytes, &[])
            .expect("the healthy sibling must analyze");
        assert_eq!(analysis.frag_host_table.box_idx, Some(0));
        assert_eq!(analysis.frag_host_table.add_idx, Some(1));
    }

    /// Without the box helper export the module-wide table is byte-provably
    /// absent, so an unscannable carrier-binop body is NOT the poisoned state:
    /// analysis proceeds and every integer-family recognizer declines
    /// per-export as usual.
    #[test]
    fn analyze_accepts_unscannable_body_when_box_helper_is_absent() {
        let helperless = POISONED_WAT.replace("  (export \"__rt_aint_from_i64\" (func $box))\n", "");
        assert_ne!(helperless, POISONED_WAT, "the control must drop the helper export");
        let bytes = wat::parse_str(&helperless).expect("helperless module WAT parses");
        let analysis = super::analyze(&bytes, &[])
            .expect("a module without the box helper is never in the poisoned state");
        assert!(analysis.frag_host_table.box_idx.is_none());
    }
}

// These historical tests compile Aver source through the producer and cannot
// live in the independent engine crate. Their end-to-end coverage remains in
// aver-lang's certificate integration suites; engine-only tests are colocated
// with the extracted modules.
#[cfg(any())]
mod analysis_tests {
    use super::*;

    fn compile_float_probe(source: &str) -> crate::codegen::wasm_gc::WasmGcCompileOutput {
        let mut items = crate::source::parse_source(
            source,
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
        let output = compile_float_probe(
            r#"
module PlanFirstProbe
    intent = "plan-first producer overlay probe"
    depends []
    exposes [carrierAnchor, floatLeGoal]

fn carrierAnchor(x: Int) -> Int
    ? "Keeps the shared Int carrier available to the certificate analyzer."
    x + 1

fn floatLeGoal(a: Float, b: Float) -> Bool
    ? "Small scalar comparison island."
    a <= b
"#,
        );
        let without_plan = analyze(&output.bytes, &[]).expect("analysis without producer plan");
        assert!(
            !without_plan
                .certified_names()
                .contains(&"floatLeGoal".to_string()),
            "expr-fragment should not be certified without a producer plan"
        );

        let checked = analyze_with_fragment_plans(&output.bytes, &[], &output.fragment_plans)
            .expect("analysis with producer plan");
        assert!(
            checked
                .certified_names()
                .contains(&"floatLeGoal".to_string()),
            "matching producer plan should certify the probe"
        );
        let source_plan = checked
            .certs
            .iter()
            .find_map(|cert| match cert.inner() {
                Cert::ExprFragment {
                    name,
                    source_plan,
                    ..
                } if name == "floatLeGoal" => source_plan.as_ref(),
                _ => None,
            })
            .expect("source-level producer plan should be preserved on the cert");
        assert_eq!(source_plan.result, SymTy::Bool);

        let mut tampered = output
            .fragment_plans
            .iter()
            .find(|artifact| artifact.export_name == "floatLeGoal")
            .expect("producer emitted a floatLeGoal plan")
            .clone();
        let FragmentPlan::Sym(sym_plan) = &mut tampered.plan else {
            panic!("source-level producer should emit floatLeGoal as a SymPlan");
        };
        let mut changed = false;
        for node in &mut sym_plan.body.nodes {
            if let SymNodeKind::Param { index } = &mut node.kind
                && *index == 0
            {
                *index = 1;
                changed = true;
                break;
            }
        }
        assert!(changed, "probe source plan should contain parameter zero");

        let checked = analyze_with_fragment_plans(&output.bytes, &[], &[tampered])
            .expect("analysis should report a declined producer plan");
        assert!(
            !checked
                .certified_names()
                .contains(&"floatLeGoal".to_string()),
            "a bad producer plan must not fall back to byte-derived classification"
        );
        let reason = checked
            .declined()
            .iter()
            .find(|(name, _)| name == "floatLeGoal")
            .map(|(_, reason)| reason.as_str())
            .expect("floatLeGoal should be declined");
        assert!(
            reason.contains("producer fragment plan does not match emitted wasm"),
            "decline reason should identify producer-plan mismatch, got: {reason}"
        );
    }

    #[test]
    fn float_arithmetic_result_is_declined_until_nan_results_are_relational() {
        let output = compile_float_probe(
            r#"
module FloatNanProfileProbe
    intent = "Float NaN portability boundary probe"
    depends []
    exposes [floatAddGoal, floatMulAddGoal, floatLeGoal]

fn floatAddGoal(a: Float, b: Float) -> Float
    ? "Float addition has a set-valued NaN result in general WebAssembly."
    a + b

fn floatMulAddGoal(a: Float, b: Float) -> Float
    ? "Both arithmetic stages can produce a set-valued NaN result."
    a * b + a

fn floatLeGoal(a: Float, b: Float) -> Bool
    ? "NaN makes the comparison false independently of its payload."
    a <= b
"#,
        );
        let checked = analyze_with_fragment_plans(&output.bytes, &[], &output.fragment_plans)
            .expect("analysis should fail closed per Float export");

        assert!(
            checked
                .certified_names()
                .contains(&"floatLeGoal".to_string()),
            "the deterministic Bool comparison should remain certifiable"
        );
        for name in ["floatAddGoal", "floatMulAddGoal"] {
            assert!(
                !checked.certified_names().contains(&name.to_string()),
                "{name} must not retain an exact-bit Float certificate"
            );
            let reason = checked
                .declined()
                .iter()
                .find(|(declined, _)| declined == name)
                .map(|(_, reason)| reason.as_str())
                .unwrap_or_else(|| panic!("{name} should be reported source-level-only"));
            assert!(
                reason.contains("general Wasm allows multiple NaN sign/payload")
                    && reason.contains("exact-bit Float output needs a relational result model"),
                "{name} should report the semantic boundary honestly, got: {reason}"
            );
        }

        // Old certificate directories may still carry either public sidecar
        // profile. The verifier-owned parsers must enforce the same boundary,
        // not merely the producer-plan overlay above.
        let add_plan = output
            .fragment_plans
            .iter()
            .find(|artifact| artifact.export_name == "floatAddGoal")
            .expect("producer emitted the historical Float-add plan");
        let FragmentPlan::Sym(add_sym) = &add_plan.plan else {
            panic!("Float add should originate as a SymPlan");
        };
        let sym_error = check_sym_fragment_plan_sidecar(
            &output.bytes,
            "floatAddGoal",
            &sym_fragment_plan_text(add_sym),
        )
        .err()
        .expect("historical SymPlan sidecar must be rejected");
        assert!(
            sym_error.contains("exact-bit Float output needs a relational result model"),
            "SymPlan verifier should report the NaN boundary: {sym_error}"
        );

        let add_expr = add_sym
            .to_expr_fragment_plan(
                &FragHostTable::placeholder(),
                &FragStructTable::default(),
            )
            .expect("Float add encodes to the historical ExprFragment plan");
        let expr_error = check_expr_fragment_plan_sidecar(
            &output.bytes,
            "floatAddGoal",
            &expr_fragment_plan_text(&add_expr),
        )
        .err()
        .expect("historical ExprFragment sidecar must be rejected");
        assert!(
            expr_error.contains("exact-bit Float output needs a relational result model"),
            "ExprFragment verifier should report the NaN boundary: {expr_error}"
        );
    }

    #[test]
    fn float_nan_gate_descends_into_nested_if_blocks() {
        fn nested_plan(op: FragPrim) -> ExprFragmentPlan {
            let arithmetic_branch = FragBlock {
                nodes: vec![
                    FragNode {
                        id: FragValueId(0),
                        ty: FragTy::F64,
                        kind: FragNodeKind::Local { index: 1 },
                    },
                    FragNode {
                        id: FragValueId(1),
                        ty: FragTy::F64,
                        kind: FragNodeKind::Local { index: 2 },
                    },
                    FragNode {
                        id: FragValueId(2),
                        ty: FragTy::F64,
                        kind: FragNodeKind::Prim {
                            op,
                            args: vec![FragValueId(0), FragValueId(1)],
                        },
                    },
                ],
                result: FragValueId(2),
            };
            let passthrough_branch = FragBlock {
                nodes: vec![FragNode {
                    id: FragValueId(0),
                    ty: FragTy::F64,
                    kind: FragNodeKind::Local { index: 1 },
                }],
                result: FragValueId(0),
            };
            ExprFragmentPlan {
                params: vec![FragTy::BoolI32, FragTy::F64, FragTy::F64],
                result: FragTy::F64,
                body: FragBlock {
                    nodes: vec![
                        FragNode {
                            id: FragValueId(0),
                            ty: FragTy::BoolI32,
                            kind: FragNodeKind::Local { index: 0 },
                        },
                        FragNode {
                            id: FragValueId(1),
                            ty: FragTy::F64,
                            kind: FragNodeKind::If {
                                cond: FragValueId(0),
                                then_block: Box::new(arithmetic_branch),
                                else_block: Box::new(passthrough_branch),
                            },
                        },
                    ],
                    result: FragValueId(1),
                },
            }
        }

        for op in [FragPrim::F64Add, FragPrim::F64Mul] {
            assert!(
                expr_fragment_needs_relational_nan_result(&nested_plan(op)),
                "the exact-Float gate must inspect arithmetic inside nested If blocks"
            );
        }
    }

    #[test]
    fn straight_line_without_producer_plan_is_source_level_only() {
        let bytes = compile_probe_bytes(
            r#"
module StraightLineNoPlanProbe
    intent = "straight-line no-plan fail-close probe"
    depends []
    exposes [addTwo]

fn addTwo(x: Int) -> Int
    ? "Add a fixed integer."
    x + 2
"#,
        );
        let analysis = analyze(&bytes, &[]).expect("analysis without producer plans");

        assert!(
            !analysis.certified_names().contains(&"addTwo".to_string()),
            "a straight-line integer body must not certify without its producer plan"
        );
        let reason = analysis
            .declined()
            .iter()
            .find(|(name, _)| name == "addTwo")
            .map(|(_, reason)| reason.as_str())
            .expect("addTwo should be listed as source-level-only");
        assert!(
            reason.contains("does not match a certified template"),
            "decline reason should state why the no-plan body is source-level-only, got: {reason}"
        );
    }

    /// The plan-first host-role table must bind `add` to EXACTLY the callee a
    /// straight-line body actually cites. In a real bignum module the multiply
    /// helper's umag loops also contain `i64.add`; the first-arithmetic rule
    /// must nevertheless classify it as `Mul`, distinct from the cited `Add`.
    /// This pins that the strict table never rides on index order the way the
    /// removed `min()` derivation did.
    #[test]
    fn frag_host_table_binds_add_to_the_cited_callee() {
        let mut items = crate::source::parse_source(
            r#"
module RoleProbe
    intent = "host role probe"
    depends []
    exposes [addTwo]

fn addTwo(x: Int) -> Int
    ? "Straight-line integer arithmetic."
    x + 2
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
        let output = crate::codegen::wasm_gc::compile_to_wasm_gc_with_handler_and_cert_plans(
            &items, None, None,
        )
        .expect("probe compiles to wasm-gc");
        let (user_fns, box_idx, _set, _carrier, host_roles, host_table, _struct_counts) =
            disassemble(&output.bytes).expect("disassemble");
        let add_two = user_fns
            .iter()
            .find(|f| f.name == "addTwo")
            .expect("addTwo user fn");
        let [cited_box, cited_add] = add_two.calls.as_slice() else {
            panic!("addTwo should cite exactly box + add, got {:?}", add_two.calls);
        };
        let mul_idx = host_table
            .mul_idx
            .expect("the module must expose the canonical multiply helper");
        assert_eq!(host_roles.get(cited_add), Some(&HostRole::Add));
        assert_eq!(host_roles.get(&mul_idx), Some(&HostRole::Mul));
        assert_ne!(*cited_add, mul_idx, "add and mul roles must stay distinct");
        // The strict table binds box/add to exactly the cited callees.
        assert_eq!(host_table.box_idx, Some(box_idx));
        assert_eq!(host_table.box_idx, Some(*cited_box));
        assert_eq!(
            host_table.add_idx,
            Some(*cited_add),
            "the strict host-role table must bind `add` to the callee the \
             emitted body cites"
        );
    }

    /// Synthetic-module template for the role-table derivation tests: an
    /// optional decoy function (placed at a LOWER index than the genuine add
    /// helper) plus the exact carrier-binop add helper and the named box
    /// export the disassembler requires.
    fn role_table_module(decoy: &str) -> Vec<u8> {
        wat::parse_str(format!(
            r#"(module
  (type $mag (array (mut i64)))
  (type $c (struct (field i64) (field (ref null $mag)) (field i32)))
  (type $bin (func (param (ref null $c)) (param (ref null $c)) (result (ref null $c))))
  (type $box (func (param i64) (result (ref null $c))))
  {decoy}
  (func $box (type $box)
    local.get 0 ref.null $mag i32.const 0 struct.new $c)
  (func $add (type $bin)
    i64.const 1 i64.const 2 i64.add drop local.get 0)
  (export "__rt_aint_from_i64" (func $box))
)"#
        ))
        .expect("role-table module WAT parses")
    }

    /// An EARLIER helper whose body is `i64.add`-shaped but whose signature is
    /// not the carrier binop must never capture the `add` role: the table
    /// binds the genuine helper, index order notwithstanding.
    #[test]
    fn frag_host_table_ignores_earlier_non_carrier_i64_add_helper() {
        let bytes = role_table_module(
            r#"(func $decoy (param i64) (param i64) (result i64)
    local.get 0 local.get 1 i64.add)"#,
        );
        let (_fns, box_idx, _set, carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        assert_eq!(carrier, Some(1), "carrier struct should be recognised");
        assert_eq!(host_table.box_idx, Some(box_idx));
        assert_eq!(
            host_table.add_idx,
            Some(2),
            "the genuine carrier-binop add helper (idx 2) must win over the \
             earlier i64-shaped decoy (idx 0)"
        );
    }

    /// If more than one candidate matches the strict signature + body shape,
    /// the role stays UNBOUND and every plan citing it declines fail-closed —
    /// the table never guesses by index order.
    #[test]
    fn frag_host_table_declines_ambiguous_add_candidates() {
        let bytes = role_table_module(
            r#"(func $decoy (type $bin)
    i64.const 3 i64.const 4 i64.add drop local.get 1)"#,
        );
        let (_fns, box_idx, _set, _carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        assert_eq!(host_table.box_idx, Some(box_idx));
        assert_eq!(
            host_table.add_idx, None,
            "two byte-shape-identical add candidates must leave the role \
             unbound (fail-closed), never bound by index order"
        );
    }

    /// The mul helper's fast path multiplies FIRST, so `first arith == add`
    /// keeps it out of the add candidacy even though its umag loops contain
    /// `i64.add` (which is what earns it the coarse Add marker).
    #[test]
    fn frag_host_table_excludes_mul_first_bodies() {
        let bytes = role_table_module(
            r#"(func $decoy (type $bin)
    i64.const 3 i64.const 4 i64.mul drop
    i64.const 3 i64.const 4 i64.add drop
    local.get 1)"#,
        );
        let (_fns, _box_idx, _set, _carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        assert_eq!(
            host_table.add_idx,
            Some(2),
            "a mul-first body must not compete for the add role"
        );
    }

    fn compile_probe_bytes(src: &str) -> Vec<u8> {
        let mut items = crate::source::parse_source(src).expect("probe source parses");
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
        crate::codegen::wasm_gc::compile_to_wasm_gc_with_handler_and_cert_plans(&items, None, None)
            .expect("probe compiles to wasm-gc")
            .bytes
    }

    /// Admission gate: `disassemble` runs full wasm validation BEFORE any
    /// rederivation, so no byte-derived fact is ever trusted from a module that
    /// is not well-typed wasm. An honest module passes; a truncated one and one
    /// carrying an out-of-range section id are rejected up front.
    #[test]
    fn disassemble_validates_module_before_rederiving() {
        let honest = compile_probe_bytes(include_str!(
            "../../../tools/certkit/fixtures/verbatimwiden.av"
        ));
        assert!(
            disassemble(&honest).is_ok(),
            "an honest, well-typed module must validate and disassemble"
        );
        assert!(
            disassemble(&honest[..honest.len() - 1]).is_err(),
            "a truncated module must fail validation, not be rederived"
        );
        let mut bogus = honest[..8].to_vec();
        bogus.extend_from_slice(&[0x7f, 0x01, 0x00]);
        assert!(
            disassemble(&bogus).is_err(),
            "an out-of-range section id must fail validation, not be rederived"
        );
    }

    /// Isolates the `Validator::validate_all` call itself: a module whose every
    /// section is well-formed (the section parser accepts it) and which carries
    /// everything `disassemble` structurally requires (the box-helper export),
    /// but whose exported function is ill-typed — declared `(result i64)` with
    /// a body that leaves an i32. Only full validation can reject it, so
    /// dropping the `validate_all` line is exactly what makes this test fail
    /// (the truncated/bad-section cases in the test above are also caught by
    /// the section parser and do not isolate the validator).
    #[test]
    fn disassemble_rejects_parseable_but_ill_typed_module() {
        // `wat` encodes without type-checking, so the ill-typed body survives
        // into well-formed binary sections.
        let ill_typed = wat::parse_str(
            r#"(module
                (type $t (func (param i64) (result i64)))
                (func $box (type $t) local.get 0)
                (func $bad (type $t) i32.const 0)
                (export "__rt_aint_from_i64" (func $box))
                (export "bad" (func $bad))
            )"#,
        )
        .expect("wat must encode the ill-typed module");
        assert!(
            wasmparser::Parser::new(0)
                .parse_all(&ill_typed)
                .all(|p| p.is_ok()),
            "the fixture must stay structurally parseable, or this test no \
             longer isolates the validator from the section parser"
        );
        assert!(
            disassemble(&ill_typed).is_err(),
            "a parseable but ill-typed module must be rejected by validation \
             before any rederivation"
        );
    }

    /// The verbatim-widened fixture's `_ -> []` default arm lowers to a
    /// `ref.null` of the `List` struct type. Disassembly must thread that
    /// heap-type index through `Op::RefNull` (not drop it, as the old unit
    /// variant did) so the S2 grammar can re-lower the empty-list default
    /// byte-exactly. The index must equal the module's List struct type — the
    /// same concrete type the function's `List<Int>` result references.
    #[test]
    fn ref_null_threads_default_arm_heap_type() {
        let bytes = compile_probe_bytes(include_str!(
            "../../../tools/certkit/fixtures/verbatimwiden.av"
        ));
        let (user_fns, _box_idx, _set, _carrier, _roles, _table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        let wrap_items = user_fns
            .iter()
            .find(|f| f.name == "wrapItems")
            .expect("wrapItems user fn");
        // `wrapItems` returns `List<Int>`, i.e. a concrete `(ref null $list)`.
        let Some(TyKind::Ref { idx: list_idx, .. }) = wrap_items.result else {
            panic!("wrapItems should return a concrete list ref");
        };
        let ref_null_hty = wrap_items
            .ops
            .iter()
            .find_map(|op| match op {
                Op::RefNull(hty) => Some(*hty),
                _ => None,
            })
            .expect("wrapItems `[]` default arm should lower to a ref.null");
        assert_eq!(
            ref_null_hty,
            Some(list_idx),
            "ref.null must carry the List struct heap-type index (the `[]` \
             default's type), not drop it"
        );
    }

    /// Mirror of `frag_host_table_binds_add_to_the_cited_callee` for the strict
    /// `sub` binding: a straight-line integer subtraction body cites box + sub,
    /// and the strict table must bind `sub` to EXACTLY that cited callee (never
    /// by index order). `x - 2` lowers to `sub(x, box(2))` — the compiler does
    /// not rewrite it as add-with-negated-constant, so it genuinely cites sub.
    #[test]
    fn frag_host_table_binds_sub_to_the_cited_callee() {
        let bytes = compile_probe_bytes(
            r#"
module SubRoleProbe
    intent = "host sub role probe"
    depends []
    exposes [subTwo]

fn subTwo(x: Int) -> Int
    ? "Straight-line integer subtraction."
    x - 2
"#,
        );
        let (user_fns, box_idx, _set, _carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        let sub_two = user_fns
            .iter()
            .find(|f| f.name == "subTwo")
            .expect("subTwo user fn");
        let [cited_box, cited_sub] = sub_two.calls.as_slice() else {
            panic!("subTwo should cite exactly box + sub, got {:?}", sub_two.calls);
        };
        assert_eq!(host_table.box_idx, Some(box_idx));
        assert_eq!(host_table.box_idx, Some(*cited_box));
        assert_eq!(
            host_table.sub_idx,
            Some(*cited_sub),
            "the strict host-role table must bind `sub` to the callee the \
             emitted body cites"
        );
    }

    /// Synthetic-module template for the `sub` role-table derivation tests: an
    /// optional decoy function plus the exact carrier-binop `sub` helper (its
    /// first i64 arithmetic op is `i64.sub`) and the named box export the
    /// disassembler requires. Parallel to `role_table_module` (which emits an
    /// `add`-shaped helper); kept separate so the `add` tests' index
    /// assertions are unaffected.
    fn role_table_module_sub(decoy: &str) -> Vec<u8> {
        wat::parse_str(format!(
            r#"(module
  (type $mag (array (mut i64)))
  (type $c (struct (field i64) (field (ref null $mag)) (field i32)))
  (type $bin (func (param (ref null $c)) (param (ref null $c)) (result (ref null $c))))
  (type $box (func (param i64) (result (ref null $c))))
  {decoy}
  (func $box (type $box)
    local.get 0 ref.null $mag i32.const 0 struct.new $c)
  (func $sub (type $bin)
    i64.const 1 i64.const 2 i64.sub drop local.get 0)
  (export "__rt_aint_from_i64" (func $box))
)"#
        ))
        .expect("role-table-sub module WAT parses")
    }

    /// If more than one candidate matches the strict carrier-binop signature +
    /// `i64.sub`-first body shape, the `sub` role stays UNBOUND (fail-closed) —
    /// the table never guesses by index order. Mirrors the `add` ambiguity test.
    #[test]
    fn frag_host_table_declines_ambiguous_sub_candidates() {
        let bytes = role_table_module_sub(
            r#"(func $decoy (type $bin)
    i64.const 3 i64.const 4 i64.sub drop local.get 1)"#,
        );
        let (_fns, box_idx, _set, _carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        assert_eq!(host_table.box_idx, Some(box_idx));
        assert_eq!(
            host_table.sub_idx, None,
            "two byte-shape-identical sub candidates must leave the role \
             unbound (fail-closed), never bound by index order"
        );
    }

    /// Field-projection tamper matrix at the plan checker (fail-closed, no
    /// lake needed): a `struct.get.user` citing a type outside the module's
    /// struct context or the Int carrier is DECLINED at the checker; a wrong
    /// (but real) struct type or a flipped field index survives the checker
    /// but fails canonical code-entry byte equality.
    #[test]
    fn field_projection_plan_tampers_decline_fail_closed() {
        let bytes = compile_probe_bytes(
            r#"
module ProjTamperProbe
    intent = "field projection tamper probe"
    depends []
    exposes [User, userName, addTwo]

record User
  name: String
  age: Int

fn addTwo(x: Int) -> Int
  ? "Pulls in the Int carrier and box helper."
  x + 2

fn userName(u: User) -> String
  ? "Record field projection."
  u.name
"#,
        );
        let (user_fns, _box_idx, _set, carrier, _roles, _table, struct_counts) =
            disassemble(&bytes).expect("disassemble");
        let carrier = carrier.expect("carrier struct");
        let user_name = user_fns
            .iter()
            .find(|f| f.name == "userName")
            .expect("userName user fn");
        let real_ty = user_name
            .ops
            .iter()
            .find_map(|op| match op {
                Op::StructGet(t, _) if *t != carrier => Some(*t),
                _ => None,
            })
            .expect("userName projects a user struct");
        assert_eq!(
            struct_counts.get(&real_ty),
            Some(&2),
            "User struct should have two fields"
        );
        let projection_plan = |ty_idx: u32, field: u32| ExprFragmentPlan {
            params: vec![FragTy::AdtRef],
            result: FragTy::AdtRef,
            body: FragBlock {
                nodes: vec![
                    FragNode {
                        id: FragValueId(0),
                        ty: FragTy::AdtRef,
                        kind: FragNodeKind::Local { index: 0 },
                    },
                    FragNode {
                        id: FragValueId(1),
                        ty: FragTy::AdtRef,
                        kind: FragNodeKind::StructGetUser {
                            ty_idx,
                            field,
                            value: FragValueId(0),
                        },
                    },
                ],
                result: FragValueId(1),
            },
        };

        // Baseline: the honest plan checks and matches the bytes.
        let (_order, _cert, _sidecar, matches, reason) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(real_ty, 0))
                .expect("honest projection plan is admitted");
        assert!(matches, "honest projection plan must match bytes: {reason:?}");

        // (c) ty_idx outside the module's struct types -> DECLINE at checker.
        let Err(err) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(9999, 0))
        else {
            panic!("out-of-module struct type must be declined")
        };
        assert!(
            err.contains("outside the module's struct types"),
            "wrong reason for out-of-module struct type: {err}"
        );

        // The Int carrier is never a projectable user struct -> DECLINE at checker.
        let Err(err) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(carrier, 0))
        else {
            panic!("carrier-typed projection must be declined")
        };
        assert!(
            err.contains("cites the Int carrier"),
            "wrong reason for carrier projection: {err}"
        );

        // Field outside the struct's byte-derived field count -> DECLINE at checker.
        let Err(err) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(real_ty, 5))
        else {
            panic!("projection past the field count must be declined at the checker")
        };
        assert!(
            err.contains("outside struct"),
            "wrong reason for out-of-range field: {err}"
        );

        // (a) wrong (but real) struct type index -> canonical bytes differ.
        let wrong_ty = struct_counts
            .keys()
            .copied()
            .find(|t| *t != real_ty && *t != carrier)
            .expect("module has another struct type");
        let (_order, _cert, _sidecar, matches, reason) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(wrong_ty, 0))
                .expect("wrong-type plan is well-formed but must not match");
        assert!(
            !matches,
            "wrong struct type must fail canonical byte equality"
        );
        assert!(
            reason.unwrap_or_default().contains("code_entry_bytes_match=false"),
            "wrong-type mismatch should name the byte inequality"
        );

        // (b) field index flipped 0 -> 1: well-typed, wrong bytes.
        let (_order, _cert, _sidecar, matches, _reason) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(real_ty, 1))
                .expect("flipped-field plan is well-formed but must not match");
        assert!(
            !matches,
            "flipped field index must fail canonical byte equality"
        );

        // A bad producer plan must decline, never fall back to legacy classes.
        let tampered = FragmentPlanArtifact {
            export_name: "userName".to_string(),
            plan: FragmentPlan::Expr(projection_plan(real_ty, 1)),
        };
        let checked = analyze_with_fragment_plans(&bytes, &[], &[tampered])
            .expect("analysis reports the declined producer plan");
        assert!(
            !checked.certified_names().contains(&"userName".to_string()),
            "tampered projection plan must not certify"
        );
        let reason = checked
            .declined()
            .iter()
            .find(|(name, _)| name == "userName")
            .map(|(_, reason)| reason.as_str())
            .expect("userName should be declined");
        assert!(
            reason.contains("producer fragment plan does not match emitted wasm"),
            "wrong tamper decline reason: {reason}"
        );
    }

    /// Source-level type names in projection plans carry the MODEL trust
    /// story (producer-asserted, not byte-derivable), but they must be
    /// internally CONSISTENT: a projection whose claimed owner differs from
    /// its value's declared type, or a `named:` type that no projection
    /// anchors to a byte-derived struct index, declines fail-closed. A fully
    /// coordinated relabel remains a read-surface change (equivalent to
    /// shipping a different model) — see docs/certification.md "Read surface".
    #[test]
    fn field_projection_source_name_inconsistency_declines() {
        let bytes = compile_probe_bytes(
            r#"
module ProjNameProbe
    intent = "field projection source-name consistency probe"
    depends []
    exposes [User, userName, addTwo]

record User
  name: String
  age: Int

fn addTwo(x: Int) -> Int
  ? "Pulls in the Int carrier and box helper."
  x + 2

fn userName(u: User) -> String
  ? "Record field projection."
  u.name
"#,
        );
        let sym_projection_plan = |param_name: &str, owner: &str, field_ty: SymTy| SymPlan {
            params: vec![SymTy::Named(param_name.to_string())],
            result: field_ty.clone(),
            body: SymBlock {
                nodes: vec![
                    SymNode {
                        id: SymValueId(0),
                        ty: SymTy::Named(param_name.to_string()),
                        kind: SymNodeKind::Param { index: 0 },
                    },
                    SymNode {
                        id: SymValueId(1),
                        ty: field_ty.clone(),
                        kind: SymNodeKind::ProjectField {
                            type_name: owner.to_string(),
                            field: 0,
                            field_ty,
                            value: SymValueId(0),
                        },
                    },
                ],
                result: SymValueId(1),
            },
        };

        // Baseline sanity: the consistent plan is admitted and byte-matched.
        let (_o, _c, _s, matches, reason) = check_sym_fragment_plan_object(
            &bytes,
            "userName",
            sym_projection_plan("User", "User", SymTy::String),
        )
        .expect("consistent projection plan is admitted");
        assert!(matches, "consistent plan must match bytes: {reason:?}");

        // Owner name diverging from the projected value's declared type. The
        // used-name anchor rule fires first here (`User` is used but never
        // projected); the owner-vs-value rule is defense-in-depth behind it.
        let Err(err) = check_sym_fragment_plan_object(
            &bytes,
            "userName",
            sym_projection_plan("User", "Other", SymTy::String),
        ) else {
            panic!("owner/value name mismatch must be declined")
        };
        assert!(
            err.contains("never projected") || err.contains("claims owner type"),
            "wrong reason for owner mismatch: {err}"
        );

        // A named field type that no projection anchors to the bytes.
        let Err(err) = check_sym_fragment_plan_object(
            &bytes,
            "userName",
            sym_projection_plan("User", "User", SymTy::Named("Ghost".to_string())),
        ) else {
            panic!("unanchored named field type must be declined")
        };
        assert!(
            err.contains("`Ghost` is never projected"),
            "wrong reason for unanchored name: {err}"
        );

        // A named parameter that is never projected at all.
        let bare = SymPlan {
            params: vec![SymTy::Named("User".to_string())],
            result: SymTy::Named("User".to_string()),
            body: SymBlock {
                nodes: vec![SymNode {
                    id: SymValueId(0),
                    ty: SymTy::Named("User".to_string()),
                    kind: SymNodeKind::Param { index: 0 },
                }],
                result: SymValueId(0),
            },
        };
        let Err(err) = check_sym_fragment_plan_object(&bytes, "userName", bare) else {
            panic!("bare named passthrough must be declined")
        };
        assert!(
            err.contains("never projected") || err.contains("no rendered proof face"),
            "wrong reason for bare named passthrough: {err}"
        );
    }

    /// An `add`-first body must not compete for the `sub` role even though it
    /// contains an `i64.sub`: `first arith == sub` keeps it out of sub
    /// candidacy, so the genuine `i64.sub`-first helper (idx 2) binds alone.
    /// Mirrors `frag_host_table_excludes_mul_first_bodies` for the add role.
    #[test]
    fn frag_host_table_excludes_add_first_bodies_from_sub() {
        let bytes = role_table_module_sub(
            r#"(func $decoy (type $bin)
    i64.const 3 i64.const 4 i64.add drop
    i64.const 3 i64.const 4 i64.sub drop
    local.get 1)"#,
        );
        let (_fns, _box_idx, _set, _carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        assert_eq!(
            host_table.sub_idx,
            Some(2),
            "an add-first body must not compete for the sub role"
        );
    }
}
