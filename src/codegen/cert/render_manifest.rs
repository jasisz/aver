fn render_obligation_def(c: &Cert, model_info: &ModelInfo) -> String {
    let name = c.name();
    // A host-call expr fragment with the straight-line integer face states the
    // SAME full-strength obligation the legacy straight-line class shipped:
    // any representation in, represented `n + k` out, under the add contract.
    if c.int_add_face().is_some() {
        let host = c.host_expr();
        return format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := List Int, Cod := Int,\n    \
             domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = {arity},\n    \
             codRepr := fun S n w => intRepr S n w,\n    \
             model := fun ns => {name} (ns.headD 0) }}\n\n",
            carrier = c.carrier(),
            self_idx = c.self_idx(),
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
        Cert::AdtConstructor { .. } if adt_constructor_uses_model(c, model_info) => {
            let sig = model_info.fns.get(name);
            let ret = sig.map(|s| s.ret.as_str()).unwrap_or("Unit");
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := Int, Cod := {ret},\n    \
                 domRepr := fun S n vs => ∃ v, vs = [v] ∧ intRepr S n v,\n    \
                 codRepr := fun S x w => {ret}Repr S x w,\n    \
                 model := {name} }}\n\n",
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
            let plan_model =
                expr_fragment_value_expr(&plan.body, plan.body.result, &|idx, _ty| {
                    expr_fragment_dom_accessor("p", idx as usize, plan.params.len())
                });
            let model = if expr_fragment_uses_audited_generic(c) {
                expr_fragment_source_model(c)
            } else {
                format!("fun p => {plan_model}")
            };
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := {dom}, Cod := {cod},\n    \
                 domRepr := fun _S p vs => vs = {dom_repr},\n    \
                 codRepr := {cod_repr},\n    \
                 model := {model} }}\n\n",
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
             {{ export_ := \"{name}\", policy := {policy}, termination? := {termination}, carrier := {carrier},\n    \
             code := CertModule.{primary}Code, host := {host}, self := {self_idx},\n    \
             Dom := List Int, Cod := Int,\n    \
             domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = 1,\n    \
             codRepr := fun S n w => intRepr S n w,\n    \
             model := fun ns => {name} (ns.headD 0) }}\n\n",
            primary = scc[0].name,
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
            policy = c.policy().lean_value(),
            termination = c
                .termination_witness()
                .map_or_else(|| "none".to_string(), |w| format!("some {}", w.lean_value())),
        ),
        _ => format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := {policy}, termination? := {termination}, {totality_role}carrier := {carrier},\n    \
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
            policy = c.policy().lean_value(),
            termination = c
                .termination_witness()
                .map(|w| format!("some {}", w.lean_value()))
                .unwrap_or_else(|| "none".to_string()),
            totality_role = if c.requires_mul_totality() {
                "totalityRole := .mul, "
            } else {
                ""
            },
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
        "import Schema\nimport Module\nimport PlanCheck\nimport PlanLower\nimport PlanBytes\nimport WasmSlice\nimport ExprFragmentAccepted\nimport ArtifactBytes\nimport Plans\nimport V3ConstructVerbatim\n",
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
    let declared_uncertified = analysis
        .module_envelope
        .declared_uncertified(analysis.certified_names(), &analysis.declined)
        .iter()
        .map(|(name, reason)| format!("({}, {})", lean_str(name), lean_str(reason)))
        .collect::<Vec<_>>()
        .join(", ");
    let capabilities = analysis
        .module_envelope
        .capabilities
        .iter()
        .map(|(module, field)| format!("({}, {})", lean_str(module), lean_str(field)))
        .collect::<Vec<_>>()
        .join(", ");
    let start = analysis
        .module_envelope
        .start
        .map(|idx| format!("some {idx}"))
        .unwrap_or_else(|| "none".to_string());
    let host_role_table = analysis.frag_host_table.roles_lean_value();
    let string_host_roles = string_host_roles_lean_value(&analysis.string_host_roles);
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
    let mutual_plans = analysis
        .certs
        .iter()
        .filter_map(|c| {
            mutual_plan_from_cert(c)
                .map(|_| format!("({}, Plans.{}MutualPlan)", lean_str(c.name()), c.name()))
        })
        .collect::<Vec<_>>()
        .join(", ");
    let composition_plans = composition_member_plans(analysis)
        .iter()
        .map(|(entry, _)| {
            format!(
                "({}, Plans.{}CompositionPlan)",
                lean_str(&entry.name),
                entry.name
            )
        })
        .collect::<Vec<_>>()
        .join(", ");
    let verbatim_plans = analysis
        .certs
        .iter()
        .filter_map(|c| {
            verbatim_plan_from_cert(c)
                .map(|_| format!("({}, Plans.{}VerbatimPlan)", lean_str(c.name()), c.name()))
        })
        .collect::<Vec<_>>()
        .join(", ");
    let int_dispatch_plans = analysis
        .certs
        .iter()
        .filter_map(|c| {
            int_dispatch_plan_from_cert(c, analysis.frag_host_table)
                .map(|_| format!("({}, Plans.{}IntDispatchPlan)", lean_str(c.name()), c.name()))
        })
        .collect::<Vec<_>>()
        .join(", ");
    let field_projection_plans = analysis
        .certs
        .iter()
        .filter_map(|c| {
            field_projection_plan_from_cert(c).map(|_| {
                format!(
                    "({}, Plans.{}FieldProjectionPlan)",
                    lean_str(c.name()),
                    c.name()
                )
            })
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
         declaredUncertified := [{declared_uncertified}],\n        \
         capabilities := [{capabilities}],\n        \
         start := {start},\n        \
         hostRoleTable := {host_role_table},\n        \
         stringHostRoles := {string_host_roles},\n        \
         contracts := [{contracts}] }},\n    \
         symFragmentPlans := [{sym_fragment_plans}],\n    \
         stringEqPlans := [{string_eq_plans}],\n    \
         stringConcatPlans := [{string_concat_plans}],\n    \
         constructPlans := [{construct_plans}],\n    \
         exprFragmentPlans := [{expr_fragment_plans}],\n    \
         recursionPlans := [{recursion_plans}],\n    \
         mutualPlans := [{mutual_plans}],\n    \
         compositionPlans := [{composition_plans}],\n    \
         verbatimPlans := [{verbatim_plans}],\n    \
         intDispatchPlans := [{int_dispatch_plans}],\n    \
         fieldProjectionPlans := [{field_projection_plans}],\n    \
         obligations := [{obligations}] }}\n\n\
         end AverCert\n",
    ));
    s
}

/// The single final theorem: `AverCert.Final.cert : Holds manifest`, proved by
/// composing audited generic discharges with the residual bespoke families.
/// No other final theorem is emitted; the checker pins this exact statement.
fn render_final(analysis: &Analysis, model_info: &ModelInfo) -> String {
    let mut s = String::new();
    s.push_str(
        "import Certificate\nimport Manifest\nimport Schema\nimport V3DischargeComposition\nimport V3DischargeExprFragment\nimport V3DischargeFieldProj\nimport V3DischargeConstruct\nimport V3DischargeVerbatim\nimport V3DischargeString\nimport V3DischargeIntDispatch\nimport V3DischargeRecursion\n\n\
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
        let struct_table_lean = emit_frag_struct_table_lean(analysis)
            .expect("certified fragment struct table remains consistent");
        let arms = analysis
            .certs
            .iter()
            .map(|c| {
                if expr_fragment_uses_audited_generic(c) {
                    return render_expr_fragment_final_arm(
                        c,
                        analysis.frag_host_table,
                        &struct_table_lean,
                    );
                }
                if matches!(c.inner(), Cert::Composition { .. }) {
                    return render_composition_final_arm(c);
                }
                if let Some(face) = c.project_face() {
                    return format!(
                        "exact V3Master.fieldProjection_direct_canonical_discharges \
                         \"{}\" {} {} {} {} CertModule.{}Code \
                         (fun _ _ _ _ _ => CertModule.{}Host) (by decide) (by rfl)",
                        c.name(),
                        c.carrier(),
                        face.struct_idx,
                        c.self_idx(),
                        face.field_idx,
                        c.name(),
                        c.name(),
                    );
                }
                if let Cert::FieldProjection {
                    name,
                    self_idx,
                    carrier,
                    struct_idx,
                    ..
                } = c.inner()
                {
                    return format!(
                        "exact V3Master.fieldProjection_canonical_discharges \
                         \"{name}\" {carrier} {struct_idx} {self_idx} \
                         AverCert.Plans.{name}FieldProjectionPlan \
                        CertModule.{name}Code \
                         (fun _ _ _ _ _ => CertModule.{name}Host) (by rfl) (by rfl)"
                    );
                }
                if matches!(
                    c.inner(),
                    Cert::VariantDispatch { .. } | Cert::WidenedIntMatch { .. }
                ) {
                    return render_int_dispatch_final_arm(
                        c,
                        model_info,
                        analysis.frag_host_table,
                    );
                }
                if matches!(c.inner(), Cert::AdtConstructor { .. })
                    && adt_constructor_uses_model(c, model_info)
                {
                    return render_adt_constructor_final_arm(c, model_info);
                }
                if recursion_uses_audited_generic(c) {
                    return render_recursion_final_arm(c);
                }
                if let Cert::AdtConstructor {
                    name,
                    self_idx,
                    carrier,
                    struct_idx,
                    arity,
                    ..
                } = c.inner()
                    && !adt_constructor_uses_model(c, model_info)
                {
                    let theorem = if *arity == 1 {
                        "constructUnary_canonical_discharges"
                    } else {
                        "constructBinary_canonical_discharges"
                    };
                    return format!(
                        "exact V3Master.{theorem} \
                         \"{name}\" {carrier} {struct_idx} {self_idx} \
                         AverCert.Plans.{name}ConstructPlan CertModule.{name}Code \
                         (fun _ _ _ _ _ => CertModule.{name}Host) \
                         (by rfl) (by rfl) (by rfl) (by rfl)"
                    );
                }
                if let Cert::VerbatimWidenedMatch {
                    name,
                    self_idx,
                    carrier,
                    ..
                }
                | Cert::VerbatimVariantDispatch {
                    name,
                    self_idx,
                    carrier,
                    ..
                } = c.inner()
                {
                    return format!(
                        "exact V3Master.verbatim_canonical_discharges \
                         \"{name}\" {carrier} {self_idx} \
                         AverCert.Plans.{name}VerbatimPlan CertModule.{name}Code \
                         (fun _ _ _ _ _ => CertModule.{name}Host) \
                         (by rfl) (by rfl)"
                    );
                }
                if let Cert::StringEqVerbatimMatch {
                    name,
                    self_idx,
                    carrier,
                    string_eq_idx,
                    ..
                } = c.inner()
                {
                    let string_ty = string_eq_string_ty_from_cert(c)
                        .expect("certified String.eq must have a byte-derived string type");
                    return format!(
                        "exact V3Master.stringEq_canonical_discharges \
                         \"{name}\" {carrier} {string_ty} {string_eq_idx} {self_idx} \
                         AverCert.Plans.{name}StringEqPlan CertModule.{name}Code \
                         (fun _ _ _ stringEq _ => CertModule.{name}Host stringEq) \
                         (by rfl) (by rfl) (by rfl) (by intros; rfl)"
                    );
                }
                if let Cert::StringConcatVerbatimMatch {
                    name,
                    self_idx,
                    carrier,
                    string_concat_idx,
                    container_ty,
                    result_ty,
                    ..
                } = c.inner()
                {
                    return format!(
                        "exact V3Master.stringConcat_canonical_discharges \
                         \"{name}\" {carrier} {result_ty} {container_ty} \
                         {string_concat_idx} {self_idx} \
                         AverCert.Plans.{name}StringConcatPlan CertModule.{name}Code \
                         (fun _ _ _ _ stringConcat => CertModule.{name}Host stringConcat) \
                         (by rfl) (by rfl) (by rfl) (by intros; rfl)"
                    );
                }
                let theorem = if c.policy() == CertificationPolicy::SimulatesModelTotally {
                    "simulates_total"
                } else {
                    "simulates"
                };
                format!("exact CertProofs.{}_{theorem}", c.name())
            })
            .collect::<Vec<_>>()
            .join("\n    | ");
        s.push_str(&format!("  all_goals\n    first\n    | {arms}\n"));
    }
    s.push_str(&format!("\n#print axioms {FINAL_THEOREM}\n"));
    s
}

fn render_lakefile(model_roots: &[String]) -> String {
    let mut roots = vec!["`CertPrelude".to_string(), "`Contracts".to_string()];
    roots.push("`CertDecode".to_string());
    for r in model_roots {
        roots.push(format!("`{r}"));
    }
    roots.push("`Module".to_string());
    roots.push("`SchemaCore".to_string());
    roots.push("`Schema".to_string());
    roots.push("`PlanCheck".to_string());
    roots.push("`PlanLower".to_string());
    roots.push("`PlanBytes".to_string());
    roots.push("`WasmSlice".to_string());
    roots.push("`ExprFragmentAccepted".to_string());
    roots.push("`AcceptedArtifactCore".to_string());
    roots.push("`AcceptedArtifact".to_string());
    roots.push("`V3ExprFragmentFull".to_string());
    roots.push("`V3StrongFuel".to_string());
    roots.push("`V3IfElse".to_string());
    roots.push("`V3GenericCertified".to_string());
    roots.push("`V3FieldProj".to_string());
    roots.push("`V3ConstructVerbatim".to_string());
    roots.push("`V3DispatchCore".to_string());
    roots.push("`V3String".to_string());
    roots.push("`V3RecSpike".to_string());
    roots.push("`V3MutualGeneric".to_string());
    roots.push("`V3Composition".to_string());
    roots.push("`V3Master".to_string());
    roots.push("`V3DischargeExprFragment".to_string());
    roots.push("`V3DischargeFieldProj".to_string());
    roots.push("`V3DischargeConstruct".to_string());
    roots.push("`V3DischargeVerbatim".to_string());
    roots.push("`V3DischargeString".to_string());
    roots.push("`V3DischargeIntDispatch".to_string());
    roots.push("`V3DischargeRecursion".to_string());
    roots.push("`V3DischargeComposition".to_string());
    roots.push("`V3AcceptSound".to_string());
    roots.push("`ArtifactBytes".to_string());
    roots.push("`Plans".to_string());
    roots.push("`Manifest".to_string());
    roots.push("`Certificate".to_string());
    roots.push("`Final".to_string());
    roots.push("`Artifact".to_string());
    roots.push("`V3AcceptReal".to_string());
    format!(
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{}]\n",
        roots.join(", ")
    )
}

struct ManifestHashes<'a> {
    schema: &'a str,
    schema_core: &'a str,
    prelude: &'a str,
    decode: &'a str,
    plan_check: &'a str,
    plan_lower: &'a str,
    plan_bytes: &'a str,
    wasm_slice: &'a str,
    expr_fragment_accepted: &'a str,
    accepted_artifact: &'a str,
    accepted_artifact_core: &'a str,
    v3_expr_fragment_full: &'a str,
    v3_strong_fuel: &'a str,
    v3_if_else: &'a str,
    v3_generic_certified: &'a str,
    v3_field_proj: &'a str,
    v3_construct_verbatim: &'a str,
    v3_dispatch_core: &'a str,
    v3_string: &'a str,
    v3_rec_spike: &'a str,
    v3_mutual_generic: &'a str,
    v3_composition: &'a str,
    v3_master: &'a str,
    v3_discharge_expr_fragment: &'a str,
    v3_discharge_field_proj: &'a str,
    v3_discharge_construct: &'a str,
    v3_discharge_verbatim: &'a str,
    v3_discharge_string: &'a str,
    v3_discharge_int_dispatch: &'a str,
    v3_discharge_recursion: &'a str,
    v3_discharge_composition: &'a str,
    v3_accept_sound: &'a str,
}

fn render_manifest(
    analysis: &Analysis,
    model_info: &ModelInfo,
    wasm_name: &str,
    sha: &str,
    hashes: &ManifestHashes<'_>,
) -> String {
    let mut s = String::new();
    let has_total = analysis
        .certs
        .iter()
        .any(|c| c.policy() == CertificationPolicy::SimulatesModelTotally);
    let has_partial = analysis
        .certs
        .iter()
        .any(|c| c.policy() == CertificationPolicy::SimulatesModel);
    let artifact_level = match (has_partial, has_total) {
        (true, true) => "mixed L1/L3",
        (false, true) => "L3",
        _ => CERT_LEVEL,
    };
    s.push_str("{\n");
    s.push_str(&format!("  \"schema_version\": {CERT_SCHEMA_VERSION},\n"));
    s.push_str(&format!("  \"wasm\": \"{wasm_name}.wasm\",\n"));
    s.push_str(&format!("  \"wasm_sha256\": \"{sha}\",\n"));
    s.push_str(&format!("  \"level\": \"{artifact_level}\",\n"));
    s.push_str(&format!("  \"profile\": \"{PROFILE_ID}\",\n"));
    s.push_str(&format!("  \"abi\": \"{RUNTIME_ABI}\",\n"));
    s.push_str(&format!("  \"final_theorem\": \"{FINAL_THEOREM}\",\n"));
    s.push_str(&format!(
        "  \"artifact_certificate_root\": \"{ARTIFACT_CERTIFICATE_ROOT}\",\n"
    ));
    s.push_str(&format!("  \"schema_sha256\": \"{}\",\n", hashes.schema));
    s.push_str(&format!(
        "  \"schema_core_sha256\": \"{}\",\n",
        hashes.schema_core
    ));
    s.push_str(&format!(
        "  \"prelude_sha256\": \"{}\",\n",
        hashes.prelude
    ));
    s.push_str(&format!(
        "  \"cert_decode_sha256\": \"{}\",\n",
        hashes.decode
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
    s.push_str(&format!(
        "  \"accepted_artifact_core_sha256\": \"{}\",\n",
        hashes.accepted_artifact_core
    ));
    s.push_str(&format!(
        "  \"v3_expr_fragment_full_sha256\": \"{}\",\n",
        hashes.v3_expr_fragment_full
    ));
    s.push_str(&format!(
        "  \"v3_strong_fuel_sha256\": \"{}\",\n",
        hashes.v3_strong_fuel
    ));
    s.push_str(&format!(
        "  \"v3_if_else_sha256\": \"{}\",\n",
        hashes.v3_if_else
    ));
    s.push_str(&format!(
        "  \"v3_generic_certified_sha256\": \"{}\",\n",
        hashes.v3_generic_certified
    ));
    s.push_str(&format!(
        "  \"v3_field_proj_sha256\": \"{}\",\n",
        hashes.v3_field_proj
    ));
    s.push_str(&format!(
        "  \"v3_construct_verbatim_sha256\": \"{}\",\n",
        hashes.v3_construct_verbatim
    ));
    s.push_str(&format!(
        "  \"v3_dispatch_core_sha256\": \"{}\",\n",
        hashes.v3_dispatch_core
    ));
    s.push_str(&format!(
        "  \"v3_string_sha256\": \"{}\",\n",
        hashes.v3_string
    ));
    s.push_str(&format!(
        "  \"v3_rec_spike_sha256\": \"{}\",\n",
        hashes.v3_rec_spike
    ));
    s.push_str(&format!(
        "  \"v3_mutual_generic_sha256\": \"{}\",\n",
        hashes.v3_mutual_generic
    ));
    s.push_str(&format!(
        "  \"v3_composition_sha256\": \"{}\",\n",
        hashes.v3_composition
    ));
    s.push_str(&format!(
        "  \"v3_master_sha256\": \"{}\",\n",
        hashes.v3_master
    ));
    s.push_str(&format!(
        "  \"v3_discharge_expr_fragment_sha256\": \"{}\",\n",
        hashes.v3_discharge_expr_fragment
    ));
    s.push_str(&format!(
        "  \"v3_discharge_field_proj_sha256\": \"{}\",\n",
        hashes.v3_discharge_field_proj
    ));
    s.push_str(&format!(
        "  \"v3_discharge_construct_sha256\": \"{}\",\n",
        hashes.v3_discharge_construct
    ));
    s.push_str(&format!(
        "  \"v3_discharge_verbatim_sha256\": \"{}\",\n",
        hashes.v3_discharge_verbatim
    ));
    s.push_str(&format!(
        "  \"v3_discharge_string_sha256\": \"{}\",\n",
        hashes.v3_discharge_string
    ));
    s.push_str(&format!(
        "  \"v3_discharge_int_dispatch_sha256\": \"{}\",\n",
        hashes.v3_discharge_int_dispatch
    ));
    s.push_str(&format!(
        "  \"v3_discharge_recursion_sha256\": \"{}\",\n",
        hashes.v3_discharge_recursion
    ));
    s.push_str(&format!(
        "  \"v3_discharge_composition_sha256\": \"{}\",\n",
        hashes.v3_discharge_composition
    ));
    s.push_str(&format!(
        "  \"v3_accept_sound_sha256\": \"{}\",\n",
        hashes.v3_accept_sound
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
    let declared_uncertified = analysis
        .module_envelope
        .declared_uncertified(analysis.certified_names(), &analysis.declined);
    s.push_str("  \"declaredUncertified\": [");
    for (i, (name, reason)) in declared_uncertified.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!(
            "\n    {{\"name\": {}, \"reason\": {}}}",
            json_str(name),
            json_str(reason)
        ));
    }
    if !declared_uncertified.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("],\n");
    s.push_str("  \"capabilities\": [");
    for (i, (module, field)) in analysis.module_envelope.capabilities.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!(
            "\n    {{\"module\": {}, \"name\": {}}}",
            json_str(module),
            json_str(field)
        ));
    }
    if !analysis.module_envelope.capabilities.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("],\n");
    match analysis.module_envelope.start {
        Some(index) => s.push_str(&format!(
            "  \"start\": {{\"present\": true, \"function_index\": {index}}},\n"
        )),
        None => s.push_str(
            "  \"start\": {\"present\": false, \"function_index\": null},\n",
        ),
    }
    let json_role = |index: Option<u32>| {
        index
            .map(|index| index.to_string())
            .unwrap_or_else(|| "null".to_string())
    };
    s.push_str(&format!(
        "  \"hostRoleTable\": {{\"box\": {}, \"add\": {}, \"mul\": {}, \"sub\": {}}},\n",
        json_role(analysis.frag_host_table.box_idx),
        json_role(analysis.frag_host_table.add_idx),
        json_role(analysis.frag_host_table.mul_idx),
        json_role(analysis.frag_host_table.sub_idx),
    ));
    s.push_str("  \"stringHostRoles\": [");
    for (index, (function_index, role)) in analysis.string_host_roles.iter().enumerate() {
        if index > 0 {
            s.push(',');
        }
        s.push_str(&format!(
            "{{\"function_index\": {function_index}, \"role\": {}}}",
            json_str(role.manifest_value())
        ));
    }
    s.push_str("],\n");
    s.push_str("  \"certified\": [");
    for (i, c) in analysis.certs.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        let kind = match c.inner() {
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
        let policy = c.policy();
        let termination_json = match c.termination_witness() {
            Some(TerminationWitness {
                measure: TerminationMeasure::IntNatAbs { param_idx },
                descent,
            }) => format!(
                ", \"termination_witness\": {{\"measure\": {{\"kind\": \"intNatAbs\", \"param_index\": {param_idx}}}, \"descent\": {descent}}}"
            ),
            None => String::new(),
        };
        let theorem = if matches!(c.inner(), Cert::Composition { .. }) {
            "V3Master.composition_claim_discharges_with_bridge".to_string()
        } else if expr_fragment_uses_audited_generic(c) {
            "V3Master.exprFragment_claim_discharges".to_string()
        } else if c.project_face().is_some() {
            "V3Master.fieldProjection_direct_canonical_discharges".to_string()
        } else if matches!(c.inner(), Cert::FieldProjection { .. }) {
            "V3Master.fieldProjection_canonical_discharges".to_string()
        } else if matches!(c.inner(), Cert::AdtConstructor { .. })
            && adt_constructor_uses_model(c, model_info)
        {
            "V3Master.construct_canonical_discharges".to_string()
        } else if let Cert::AdtConstructor { arity, .. } = c.inner()
            && !adt_constructor_uses_model(c, model_info)
        {
            if *arity == 1 {
                "V3Master.constructUnary_canonical_discharges".to_string()
            } else {
                "V3Master.constructBinary_canonical_discharges".to_string()
            }
        } else if matches!(
            c.inner(),
            Cert::VerbatimWidenedMatch { .. } | Cert::VerbatimVariantDispatch { .. }
        ) {
            "V3Master.verbatim_canonical_discharges".to_string()
        } else if matches!(c.inner(), Cert::StringEqVerbatimMatch { .. }) {
            "V3Master.stringEq_canonical_discharges".to_string()
        } else if matches!(c.inner(), Cert::StringConcatVerbatimMatch { .. }) {
            "V3Master.stringConcat_canonical_discharges".to_string()
        } else if matches!(
            c.inner(),
            Cert::VariantDispatch { .. } | Cert::WidenedIntMatch { .. }
        ) {
            "V3Master.intDispatch_canonical_discharges".to_string()
        } else if recursion_uses_audited_generic(c) {
            "V3Master.recursion_claim_discharges".to_string()
        } else {
            let theorem_suffix = if policy == CertificationPolicy::SimulatesModelTotally {
                "wasm_total"
            } else {
                "wasm_certified"
            };
            format!("CertProofs.{}_{theorem_suffix}", c.name())
        };
        s.push_str(&format!(
            "\n    {{\"name\": {}, \"class\": \"{}\", \"policy\": \"{}\", \
             \"level\": \"{}\", \"dom\": {}, \"cod\": {}, \
             \"theorem\": {}{}{}}}",
            json_str(c.name()),
            kind,
            policy.manifest_name(),
            policy.level(),
            json_str(&dom),
            json_str(&cod),
            json_str(&theorem),
            termination_json,
            fragment_json,
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
