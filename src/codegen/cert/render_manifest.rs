fn render_obligation_def(c: &Cert, model_info: &ModelInfo) -> String {
    let name = c.name();
    // A host-call expr fragment with the straight-line integer face states the
    // SAME full-strength obligation the legacy straight-line class shipped:
    // any representation in, represented `n + k` out, under the add contract.
    if c.int_add_face().is_some() {
        return format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := List Int, Cod := Int,\n    \
             domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = {arity},\n    \
             codRepr := fun S n w => intRepr S n w,\n    \
             model := {model} }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
            model = c.model_expr(),
            arity = c.arity(),
        );
    }
    // An ADT-ref expr fragment with the field-projection face states the SAME
    // verbatim projection obligation the legacy field-projection class ships:
    // a two-field struct in, the projected field out unchanged.
    if let Some(face) = c.project_face() {
        let model = if face.field_idx == 0 { "p.1" } else { "p.2" };
        return format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := WVal × WVal, Cod := WVal,\n    \
             domRepr := fun _ p vs => vs = [.structv {struct_idx} [p.1, p.2]],\n    \
             codRepr := fun S v w => verbatimRepr S v w,\n    \
             model := fun p => {model} }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
            struct_idx = face.struct_idx,
        );
    }
    match c.inner() {
        Cert::AdtConstructor {
            struct_idx,
            field_count,
            ..
        } if adt_constructor_uses_model(c, model_info) => {
            let sig = model_info.fns.get(name);
            let ret = sig.map(|s| s.ret.as_str()).unwrap_or("Unit");
            let ctor = sig
                .and_then(|s| model_info.inductives.get(&s.ret))
                .and_then(|i| i.ctors.first())
                .map(|c| c.name.as_str())
                .unwrap_or("mk");
            let _ = (struct_idx, field_count);
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := Int, Cod := {ret},\n    \
                 domRepr := fun S n vs => ∃ v, vs = [v] ∧ intRepr S n v,\n    \
                 codRepr := fun S x w => {ret}Repr S x w,\n    \
                 model := fun n => {ret}.{ctor} n }}\n\n",
                carrier = c.carrier(),
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::AdtConstructor {
            struct_idx,
            arity,
            fields,
            ..
        } => {
            // Verbatim pack certificate (dual of the field projection): the body
            // wraps its `field_count` arguments into variant `struct_idx`. No
            // claim about a recursive model representation — `Cod := WVal` and
            // `verbatimRepr` pin the output to the constructed struct byte-for-byte.
            let (dom, pat, args) = verbatim_ctor_shape(*arity, fields);
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := {dom}, Cod := WVal,\n    \
                 domRepr := fun _ p vs => vs = {pat},\n    \
                 codRepr := fun S v w => verbatimRepr S v w,\n    \
                 model := fun p => .structv {struct_idx} {args} }}\n\n",
                carrier = c.carrier(),
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::FieldProjection {
            struct_idx,
            field_idx,
            ..
        } => {
            let model = if *field_idx == 0 { "p.1" } else { "p.2" };
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := WVal × WVal, Cod := WVal,\n    \
                 domRepr := fun _ p vs => vs = [.structv {struct_idx} [p.1, p.2]],\n    \
                 codRepr := fun S v w => verbatimRepr S v w,\n    \
                 model := fun p => {model} }}\n\n",
                carrier = c.carrier(),
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::VariantDispatch { .. } => {
            let ty = model_info
                .fns
                .get(name)
                .and_then(|s| s.params.first())
                .map(|s| s.as_str())
                .unwrap_or("Op");
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := {ty}, Cod := Int,\n    \
                 domRepr := fun S o vs => ∃ v, vs = [v] ∧ {ty}Repr S o v,\n    \
                 codRepr := fun S n w => intRepr S n w,\n    \
                 model := {name} }}\n\n",
                carrier = c.carrier(),
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::WidenedIntMatch { .. } => {
            let ty = model_info
                .fns
                .get(name)
                .and_then(|s| s.params.first())
                .map(|s| s.as_str())
                .unwrap_or("Op");
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := {ty}, Cod := Int,\n    \
                 domRepr := fun S o vs => ∃ v, vs = [v] ∧ {name}DomRepr S o v,\n    \
                 codRepr := fun S n w => intRepr S n w,\n    \
                 model := {name} }}\n\n",
                carrier = c.carrier(),
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::ExprFragment { carrier, plan, .. } => {
            let dom = expr_fragment_dom_type(&plan.params);
            let cod = plan.result.lean_dom_type();
            let dom_repr =
                expr_fragment_dom_repr_list(&plan.params, "p", &carrier.to_string());
            let cod_repr = match plan.result {
                FragTy::F64 => "fun S bits w => floatBitsRepr S bits w",
                FragTy::BoolI32 => "fun S b w => boolRepr S b w",
                FragTy::IntCarrier
                | FragTy::I64
                | FragTy::RawI32
                | FragTy::Ref
                | FragTy::AdtRef => "fun S v w => verbatimRepr S v w",
            };
            let model =
                expr_fragment_value_expr(&plan.body, plan.body.result, &|idx, _ty| {
                    expr_fragment_dom_accessor("p", idx as usize, plan.params.len())
                });
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := {dom}, Cod := {cod},\n    \
                 domRepr := fun _S p vs => vs = {dom_repr},\n    \
                 codRepr := {cod_repr},\n    \
                 model := fun p => {model} }}\n\n",
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::VerbatimWidenedMatch { .. }
        | Cert::VerbatimVariantDispatch { .. }
        | Cert::StringEqVerbatimMatch { .. }
        | Cert::StringConcatVerbatimMatch { .. } => format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := WVal, Cod := WVal,\n    \
             domRepr := fun _S v vs => vs = [v],\n    \
             codRepr := fun S x w => verbatimRepr S x w,\n    \
             model := {name}Model }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
        ),
        // Each SCC member is its own integer-simulation obligation, but the `code`
        // and `host` fields point at the ONE shared table/host named after the
        // primary member (`scc[0]`); the model is this member's own function.
        Cert::MutualRecursion { scc, .. } => format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{primary}Code, host := {host}, self := {self_idx},\n    \
             Dom := List Int, Cod := Int,\n    \
             domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = 1,\n    \
             codRepr := fun S n w => intRepr S n w,\n    \
             model := fun ns => {name} (ns.headD 0) }}\n\n",
            primary = scc[0].name,
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
        ),
        _ => format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := List Int, Cod := Int,\n    \
             domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = {arity},\n    \
             codRepr := fun S n w => intRepr S n w,\n    \
             model := {model} }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
            model = c.model_expr(),
            arity = c.arity(),
        ),
    }
}

/// The generated manifest literal, mirroring `cert-manifest.json`: the subject
/// metadata plus one `Obligation` per certified export. This is the LITERAL the
/// consumer pins.
fn render_manifest_lean(
    analysis: &Analysis,
    model_roots: &[String],
    model_info: &ModelInfo,
    sha: &str,
) -> String {
    let mut s = String::new();
    s.push_str(
        "import Schema\nimport Module\nimport PlanCheck\nimport PlanLower\nimport PlanBytes\nimport WasmSlice\nimport ExprFragmentAccepted\nimport ArtifactBytes\nimport Plans\n",
    );
    for r in model_roots {
        s.push_str(&format!("import {r}\n"));
    }
    s.push_str(
        "\nset_option linter.unusedVariables false\n\n\
         namespace AverCert\nopen AverCert.Schema CertPrelude\n\n",
    );
    s.push_str(&render_user_repr_defs(analysis, model_info));
    // One obligation def per certified export.
    for c in &analysis.certs {
        s.push_str(&render_obligation_def(c, model_info));
    }
    // Subject + manifest.
    let exports = analysis
        .certs
        .iter()
        .map(|c| format!("\"{}\"", c.name()))
        .collect::<Vec<_>>()
        .join(", ");
    let contracts = analysis
        .contracts
        .iter()
        .map(|c| lean_str(c))
        .collect::<Vec<_>>()
        .join(", ");
    let obligations = analysis
        .certs
        .iter()
        .map(|c| format!("{}Ob", c.name()))
        .collect::<Vec<_>>()
        .join(", ");
    let sym_expr_fragment_plans = analysis
        .certs
        .iter()
        .filter_map(|c| match c.inner() {
            Cert::ExprFragment {
                name,
                source_plan,
                plan,
                ..
            } if expr_fragment_source_plan(source_plan, plan).is_some() => {
                Some(format!("({}, Plans.{name}Plan)", lean_str(name)))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let fallback_expr_fragment_plans = analysis
        .certs
        .iter()
        .filter_map(|c| match c.inner() {
            Cert::ExprFragment {
                name,
                source_plan,
                plan,
                ..
            } if expr_fragment_source_plan(source_plan, plan).is_none() => {
                Some(format!("({}, Plans.{name}Plan)", lean_str(name)))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let expr_fragment_plans = sym_expr_fragment_plans
        .into_iter()
        .chain(fallback_expr_fragment_plans)
        .collect::<Vec<_>>()
        .join(", ");
    let expr_sym_fragment_plans = analysis
        .certs
        .iter()
        .filter_map(|c| match c.inner() {
            Cert::ExprFragment {
                name,
                source_plan,
                plan,
                ..
            } if expr_fragment_source_plan(source_plan, plan).is_some() => {
                Some(format!("({}, Plans.{name}SymPlan)", lean_str(name)))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let string_sym_fragment_plans = analysis
        .certs
        .iter()
        .filter_map(|c| match c.inner() {
            Cert::StringEqVerbatimMatch { name, .. } => {
                Some(format!("({}, Plans.{name}StringEqSymPlan)", lean_str(name)))
            }
            Cert::StringConcatVerbatimMatch { name, .. } => {
                Some(format!("({}, Plans.{name}StringConcatSymPlan)", lean_str(name)))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let construct_sym_fragment_plans = analysis
        .certs
        .iter()
        .filter_map(|c| match c.inner() {
            Cert::AdtConstructor { name, .. }
                if adt_constructor_sym_plan_from_cert(c, model_info).is_some() =>
            {
                Some(format!("({}, Plans.{name}ConstructSymPlan)", lean_str(name)))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let sym_fragment_plans = expr_sym_fragment_plans
        .into_iter()
        .chain(string_sym_fragment_plans)
        .chain(construct_sym_fragment_plans)
        .collect::<Vec<_>>()
        .join(", ");
    let string_eq_plans = analysis
        .certs
        .iter()
        .filter_map(|c| match c.inner() {
            Cert::StringEqVerbatimMatch { name, .. } => {
                Some(format!("({}, Plans.{name}StringEqPlan)", lean_str(name)))
            }
            _ => None,
        })
        .collect::<Vec<_>>()
        .join(", ");
    let string_concat_plans = analysis
        .certs
        .iter()
        .filter_map(|c| match c.inner() {
            Cert::StringConcatVerbatimMatch { name, .. } => {
                Some(format!("({}, Plans.{name}StringConcatPlan)", lean_str(name)))
            }
            _ => None,
        })
        .collect::<Vec<_>>()
        .join(", ");
    let construct_plans = analysis
        .certs
        .iter()
        .filter_map(|c| match c.inner() {
            Cert::AdtConstructor { name, .. }
                if construct_plan_from_cert(c).is_some()
                    && adt_constructor_sym_plan_from_cert(c, model_info).is_some() =>
            {
                Some(format!("({}, Plans.{name}ConstructPlan)", lean_str(name)))
            }
            _ => None,
        })
        .collect::<Vec<_>>()
        .join(", ");
    let recursion_plans = analysis
        .certs
        .iter()
        .filter_map(|c| {
            recursion_plan_from_cert(c)
                .map(|_| format!("({}, Plans.{}RecursionPlan)", lean_str(c.name()), c.name()))
        })
        .collect::<Vec<_>>()
        .join(", ");
    s.push_str(&format!(
        "def manifest : Schema.Manifest :=\n  \
         {{ subject :=\n      \
         {{ artifactHash := \"{sha}\",\n        \
         profile := \"{PROFILE_ID}\",\n        \
         abi := \"{RUNTIME_ABI}\",\n        \
         artifactRoot := \"{ARTIFACT_CERTIFICATE_ROOT}\",\n        \
         exports := [{exports}],\n        \
         contracts := [{contracts}] }},\n    \
         symFragmentPlans := [{sym_fragment_plans}],\n    \
         stringEqPlans := [{string_eq_plans}],\n    \
         stringConcatPlans := [{string_concat_plans}],\n    \
         constructPlans := [{construct_plans}],\n    \
         exprFragmentPlans := [{expr_fragment_plans}],\n    \
         recursionPlans := [{recursion_plans}],\n    \
         obligations := [{obligations}] }}\n\n\
         end AverCert\n",
    ));
    s
}

/// The single final theorem: `AverCert.Final.cert : Holds manifest`, proved by
/// composing the per-export `_simulates` obligations. No other final theorem is
/// emitted; the checker pins this exact statement line.
fn render_final(analysis: &Analysis) -> String {
    let mut s = String::new();
    s.push_str(
        "import Certificate\nimport Manifest\nimport Schema\n\n\
         set_option maxRecDepth 1000000\n\
         set_option linter.unusedSimpArgs false\n\n\
         open AverCert AverCert.Schema\n\n",
    );
    s.push_str(
        "/-- THE single artifact certificate: the pinned module hash is this module's\n\
        hash, and every certified export simulates its model under the named runtime\n\
        contracts. Proof composes the per-export obligations; nothing else. -/\n",
    );
    s.push_str(&format!("{FINAL_STATEMENT_LINE} := by\n"));
    if analysis.certs.is_empty() {
        s.push_str(
            "  refine ⟨rfl, ?_⟩\n  \
             intro o ho\n  \
             simp only [manifest, List.mem_nil_iff, List.not_mem_nil] at ho\n",
        );
    } else {
        s.push_str("  refine ⟨rfl, ?_⟩\n  intro o ho\n");
        s.push_str(
            "  simp only [manifest, List.mem_cons, List.mem_singleton, List.mem_nil_iff,\n    \
             List.not_mem_nil, or_false] at ho\n",
        );
        // `rcases` with one `rfl` per obligation, split on the disjunction.
        let pattern = std::iter::repeat_n("rfl", analysis.certs.len())
            .collect::<Vec<_>>()
            .join(" | ");
        s.push_str(&format!("  rcases ho with {pattern}\n"));
        // Every resulting goal is closed by exactly one export's obligation.
        let arms = analysis
            .certs
            .iter()
            .map(|c| format!("exact ⟨rfl, CertProofs.{}_simulates⟩", c.name()))
            .collect::<Vec<_>>()
            .join("\n    | ");
        s.push_str(&format!("  all_goals\n    first\n    | {arms}\n"));
    }
    s.push_str(&format!("\n#print axioms {FINAL_THEOREM}\n"));
    s
}

fn render_lakefile(model_roots: &[String]) -> String {
    let mut roots = vec!["`CertPrelude".to_string(), "`Contracts".to_string()];
    for r in model_roots {
        roots.push(format!("`{r}"));
    }
    roots.push("`Module".to_string());
    roots.push("`Schema".to_string());
    roots.push("`PlanCheck".to_string());
    roots.push("`PlanLower".to_string());
    roots.push("`PlanBytes".to_string());
    roots.push("`WasmSlice".to_string());
    roots.push("`ExprFragmentAccepted".to_string());
    roots.push("`AcceptedArtifact".to_string());
    roots.push("`ArtifactBytes".to_string());
    roots.push("`Plans".to_string());
    roots.push("`Manifest".to_string());
    roots.push("`Certificate".to_string());
    roots.push("`Final".to_string());
    roots.push("`Artifact".to_string());
    format!(
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{}]\n",
        roots.join(", ")
    )
}

struct ManifestHashes<'a> {
    schema: &'a str,
    prelude: &'a str,
    plan_check: &'a str,
    plan_lower: &'a str,
    plan_bytes: &'a str,
    wasm_slice: &'a str,
    expr_fragment_accepted: &'a str,
    accepted_artifact: &'a str,
}

fn render_manifest(
    analysis: &Analysis,
    model_info: &ModelInfo,
    wasm_name: &str,
    sha: &str,
    hashes: &ManifestHashes<'_>,
) -> String {
    let mut s = String::new();
    s.push_str("{\n");
    s.push_str(&format!("  \"schema_version\": {CERT_SCHEMA_VERSION},\n"));
    s.push_str(&format!("  \"wasm\": \"{wasm_name}.wasm\",\n"));
    s.push_str(&format!("  \"wasm_sha256\": \"{sha}\",\n"));
    s.push_str(&format!("  \"level\": \"{CERT_LEVEL}\",\n"));
    s.push_str(&format!("  \"profile\": \"{PROFILE_ID}\",\n"));
    s.push_str(&format!("  \"abi\": \"{RUNTIME_ABI}\",\n"));
    s.push_str(&format!("  \"final_theorem\": \"{FINAL_THEOREM}\",\n"));
    s.push_str(&format!(
        "  \"artifact_certificate_root\": \"{ARTIFACT_CERTIFICATE_ROOT}\",\n"
    ));
    s.push_str(&format!("  \"schema_sha256\": \"{}\",\n", hashes.schema));
    s.push_str(&format!(
        "  \"prelude_sha256\": \"{}\",\n",
        hashes.prelude
    ));
    s.push_str(&format!(
        "  \"plan_check_sha256\": \"{}\",\n",
        hashes.plan_check
    ));
    s.push_str(&format!(
        "  \"plan_lower_sha256\": \"{}\",\n",
        hashes.plan_lower
    ));
    s.push_str(&format!(
        "  \"plan_bytes_sha256\": \"{}\",\n",
        hashes.plan_bytes
    ));
    s.push_str(&format!(
        "  \"wasm_slice_sha256\": \"{}\",\n",
        hashes.wasm_slice
    ));
    s.push_str(&format!(
        "  \"expr_fragment_accepted_sha256\": \"{}\",\n",
        hashes.expr_fragment_accepted
    ));
    s.push_str(&format!(
        "  \"accepted_artifact_sha256\": \"{}\",\n",
        hashes.accepted_artifact
    ));
    if let Some(c) = analysis.carrier {
        s.push_str(&format!("  \"carrier_type_index\": {c},\n"));
    } else {
        s.push_str("  \"carrier_type_index\": null,\n");
    }
    s.push_str("  \"runtime_contracts\": [");
    for (i, c) in analysis.contracts.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!("\n    {}", json_str(c)));
    }
    if !analysis.contracts.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("],\n");
    let accepted_artifact_exports = analysis
        .certs
        .iter()
        .filter(|c| artifact_bridge_profile(c, model_info) == "accepted-artifact-v1")
        .count();
    let legacy_witness_exports = analysis.certs.len().saturating_sub(accepted_artifact_exports);
    s.push_str(&format!(
        "  \"artifact_bridge_counts\": {{\"accepted-artifact-v1\": {accepted_artifact_exports}, \
         \"legacy-witness-v1\": {legacy_witness_exports}}},\n"
    ));
    s.push_str("  \"certified\": [");
    for (i, c) in analysis.certs.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        let kind = match c.inner() {
            Cert::StraightLine { .. } => "straight-line",
            Cert::Recursive { .. } => "self-recursive",
            Cert::AccumulatorRecursive { .. } => "multi-argument self-recursive",
            Cert::AdtConstructor { .. } => "adt-constructor",
            Cert::FieldProjection { .. } => "field-projection",
            Cert::WidenedIntMatch { .. } => "widened-int-match",
            Cert::VerbatimWidenedMatch { .. } => "verbatim-widened-match",
            Cert::VerbatimVariantDispatch { .. } => "verbatim-variant-dispatch",
            Cert::StringEqVerbatimMatch { .. } => "verbatim-string-eq",
            Cert::StringConcatVerbatimMatch { .. } => "verbatim-string-concat",
            Cert::ExprFragment { .. } => "expr-fragment-v1",
            Cert::VariantDispatch { .. } => "variant-dispatch",
            Cert::Composition { .. } => "cross-function-composition",
            Cert::MutualRecursion { .. } => "mutual-recursive",
            Cert::NonRecursive { .. } => unreachable!(),
        };
        let (dom, cod) = c.source_dom_cod(model_info);
        let fragment_json = match c.inner() {
            Cert::ExprFragment {
                source_plan, plan, ..
            } => {
                if let Some(sym) = expr_fragment_source_plan(source_plan, plan) {
                    let sym_sidecar = sym_fragment_sidecar(c.name(), &sym);
                    format!(
                        ", \"source_fragment\": {{\"profile\": \"sym-fragment-v1\", \
                         \"plan\": {}, \"plan_sha256\": {}}}",
                        json_str(&sym_sidecar.path),
                        json_str(&sym_sidecar.sha256)
                    )
                } else {
                    let sidecar = expr_fragment_sidecar(c.name(), plan);
                    format!(
                        ", \"fragment\": {{\"profile\": \"expr-fragment-v1\", \
                         \"plan\": {}, \"plan_sha256\": {}}}",
                        json_str(&sidecar.path),
                        json_str(&sidecar.sha256)
                    )
                }
            }
            Cert::StringConcatVerbatimMatch { .. } => {
                let plan = string_concat_plan_from_cert(c)
                    .expect("certified String.concat should project to a source plan");
                let sym_plan = string_concat_sym_plan_from_plan(&plan);
                let sym_sidecar = sym_fragment_sidecar(c.name(), &sym_plan);
                let sidecar = string_concat_sidecar(c.name(), &plan);
                format!(
                    ", \"source_fragment\": {{\"profile\": \"sym-fragment-v1\", \
                     \"plan\": {}, \"plan_sha256\": {}}}, \
                     \"fragment\": {{\"profile\": \"string-concat-v1\", \
                     \"plan\": {}, \"plan_sha256\": {}}}",
                    json_str(&sym_sidecar.path),
                    json_str(&sym_sidecar.sha256),
                    json_str(&sidecar.path),
                    json_str(&sidecar.sha256)
                )
            }
            Cert::StringEqVerbatimMatch { .. } => {
                let plan = string_eq_plan_from_cert(c)
                    .expect("certified String.eq should project to a source plan");
                let sym_plan = string_eq_sym_plan_from_plan(&plan);
                let sym_sidecar = sym_fragment_sidecar(c.name(), &sym_plan);
                let sidecar = string_eq_sidecar(c.name(), &plan);
                format!(
                    ", \"source_fragment\": {{\"profile\": \"sym-fragment-v1\", \
                     \"plan\": {}, \"plan_sha256\": {}}}, \
                     \"fragment\": {{\"profile\": \"string-eq-v1\", \
                     \"plan\": {}, \"plan_sha256\": {}}}",
                    json_str(&sym_sidecar.path),
                    json_str(&sym_sidecar.sha256),
                    json_str(&sidecar.path),
                    json_str(&sidecar.sha256)
                )
            }
            Cert::AdtConstructor { .. } => {
                if let (Some(sym_plan), Some(plan)) = (
                    adt_constructor_sym_plan_from_cert(c, model_info),
                    construct_plan_from_cert(c),
                ) {
                    let sym_sidecar = sym_fragment_sidecar(c.name(), &sym_plan);
                    let sidecar = construct_sidecar(c.name(), &plan);
                    format!(
                        ", \"source_fragment\": {{\"profile\": \"sym-fragment-v1\", \
                         \"plan\": {}, \"plan_sha256\": {}}}, \
                         \"fragment\": {{\"profile\": \"construct-v1\", \
                         \"plan\": {}, \"plan_sha256\": {}}}",
                        json_str(&sym_sidecar.path),
                        json_str(&sym_sidecar.sha256),
                        json_str(&sidecar.path),
                        json_str(&sidecar.sha256)
                    )
                } else {
                    String::new()
                }
            }
            _ => String::new(),
        };
        let artifact_bridge = artifact_bridge_profile(c, model_info);
        s.push_str(&format!(
            "\n    {{\"name\": {}, \"class\": \"{}\", \"policy\": \"simulatesModel\", \
             \"level\": \"{}\", \"artifact_bridge\": \"{}\", \"dom\": {}, \"cod\": {}, \
             \"theorem\": \"CertProofs.{}_wasm_certified\"{}}}",
            json_str(c.name()),
            kind,
            CERT_LEVEL,
            artifact_bridge,
            json_str(&dom),
            json_str(&cod),
            c.name(),
            fragment_json
        ));
    }
    if !analysis.certs.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("],\n");
    s.push_str("  \"source_level_only\": [");
    for (i, (name, reason)) in analysis.declined.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!(
            "\n    {{\"name\": {}, \"reason\": {}}}",
            json_str(name),
            json_str(reason)
        ));
    }
    if !analysis.declined.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("]\n}\n");
    s
}

fn artifact_bridge_profile(c: &Cert, model_info: &ModelInfo) -> &'static str {
    match c.inner() {
        Cert::ExprFragment { .. }
        | Cert::StringEqVerbatimMatch { .. }
        | Cert::StringConcatVerbatimMatch { .. } => "accepted-artifact-v1",
        Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. }
            if recursion_plan_from_cert(c).is_some() =>
        {
            "accepted-artifact-v1"
        }
        Cert::AdtConstructor { .. }
            if adt_constructor_sym_plan_from_cert(c, model_info).is_some()
                && construct_plan_from_cert(c).is_some() =>
        {
            "accepted-artifact-v1"
        }
        _ => "legacy-witness-v1",
    }
}

/// A Lean string literal (escapes `"` and `\`); contract descriptions never
/// contain control characters.
fn lean_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn json_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}
