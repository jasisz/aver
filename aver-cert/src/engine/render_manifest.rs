fn render_obligation_def(c: &Cert, model_info: &ModelInfo, host_table_lean: &str) -> String {
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
             model := fun ns => {model_name} (ns.headD 0) }}\n\n",
            carrier = c.carrier(),
            self_idx = c.self_idx(),
            arity = c.arity(),
            model_name = c.model_lean_name(model_info),
        );
    }
    if let Some(face) = c.tag_dispatch_face() {
        return format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := AverCert.StandardFace.tagDispatchHost {carrier} {box_idx}, self := {self_idx},\n    \
             Dom := Int × WVal, Cod := Int,\n    \
             domRepr := fun _S p vs => vs = [.structv {opt_idx} [.i32v p.1, p.2]],\n    \
             codRepr := fun S v w => intRepr S v w,\n    \
             model := fun p => if p.1 = {tag} then {then_c} else {else_c} }}\n\n",
            carrier = c.carrier(),
            box_idx = face.box_idx,
            self_idx = c.self_idx(),
            opt_idx = face.opt_idx,
            tag = lean_int_lit(face.tag),
            then_c = lean_int_lit(face.then_c),
            else_c = lean_int_lit(face.else_c),
        );
    }
    // The fused vector-read face: the wall's face terms verbatim (domain is
    // the represented vector with its in-relation length bound, model is the
    // in-bounds read or the literal default).
    if let Some(face) = c.vector_get_face() {
        return format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := List Int × Int, Cod := Int,\n    \
             domRepr := AverCert.StandardFace.vecDomRepr {carrier} {arr_ty},\n    \
             codRepr := fun S n w => intRepr S n w,\n    \
             model := AverCert.StandardFace.vecModel ({d} : Int) }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
            arr_ty = face.arr_ty,
            d = face.default,
        );
    }
    // The two Int comparison faces: every meaning field is a wall term the
    // checked face pins by `HEq` — the model included. Nothing here is read
    // from the source model, which is why these claims cite no model name.
    if let Some(face) = c.int_cmp_bool_face() {
        return format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := Int × Int, Cod := Bool,\n    \
             domRepr := AverCert.StandardFace.intPairSmallBandDomRepr {carrier},\n    \
             codRepr := boolRepr,\n    \
             model := AverCert.StandardFace.intCmpModel {op} }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
            op = face.op.lean_ctor(),
        );
    }
    if let Some(face) = c.int_select_face() {
        return format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := Int × Int, Cod := Int,\n    \
             domRepr := AverCert.StandardFace.intPairSmallBandDomRepr {carrier},\n    \
             codRepr := intRepr,\n    \
             model := AverCert.StandardFace.intSelectModel {op} }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
            op = face.op.lean_ctor(),
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
    // A stage-1 record scalar field read states the wall's record-parameter
    // obligation: the domain is the record denotation, the codomain the
    // projected scalar leaf under the single generic `ReprOf`, and the model the
    // field read — every meaning field is the wall term the checked record face
    // pins by `HEq`, exactly like `intDispatchDeclaredFace`.
    if let Some(face) = c.record_compute_face() {
        let name = c.name();
        return format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := AverCert.StandardFace.recordComputeHost {carrier} {host_table_lean}, self := {self_idx},\n    \
             Dom := List RecordComputeBridge.SVal, Cod := Option RecordComputeBridge.SVal,\n    \
             domRepr := AverCert.StandardFace.recordComputeDomRepr {carrier} {struct_idx} Plans.{name}Plan.params,\n    \
             codRepr := AverCert.StandardFace.recordComputeCodRepr {carrier} {struct_idx},\n    \
             model := AverCert.StandardFace.recordComputeModel Plans.{name}Plan.body }}\n\n",
            carrier = c.carrier(),
            self_idx = c.self_idx(),
            struct_idx = face.struct_idx,
        );
    }
    if let Some(face) = c.record_param_face() {
        return format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := AverCert.StandardFace.emptyHost, self := {self_idx},\n    \
             Dom := AverCert.Schema.RecordFields Plans.{name}RecordFields,\n    \
             Cod := AverCert.Schema.RecordVal (Plans.{name}RecordFields[{field}]'(by decide)),\n    \
             domRepr := AverCert.StandardFace.recordParamDomRepr {carrier} {struct_idx} Plans.{name}RecordFields,\n    \
             codRepr := AverCert.StandardFace.recordParamCodRepr {carrier} Plans.{name}RecordFields {field} (by decide),\n    \
             model := AverCert.StandardFace.recordParamModel Plans.{name}RecordFields {field} (by decide) }}\n\n",
            carrier = c.carrier(),
            self_idx = c.self_idx(),
            struct_idx = face.struct_idx,
            field = face.field_idx,
        );
    }
    match c.inner() {
        Cert::AdtConstructor { struct_idx, .. }
            if adt_constructor_uses_model(c, model_info) =>
        {
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := Int,\n    \
                 Cod := AverCert.DeclaredIndexEnvelope.DAdtVal Plans.{name}DeclaredEnvelope,\n    \
                 domRepr := AverCert.EnvelopeLowering.intArgDomRepr {carrier},\n    \
                 codRepr := AverCert.DeclaredIndexEnvelope.dEnvCodRepr Plans.{name}DeclaredEnvelope,\n    \
                 model := AverCert.DeclaredIndexEnvelope.dEnvCtorModel Plans.{name}DeclaredEnvelope {struct_idx} (by decide) }}\n\n",
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
        Cert::VariantDispatch { .. } | Cert::WidenedIntMatch { .. } => format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := AverCert.DeclaredIndexEnvelope.DAdtVal Plans.{name}DeclaredEnvelope,\n    \
             Cod := Int,\n    \
             domRepr := AverCert.DeclaredIndexEnvelope.dEnvDomRepr Plans.{name}DeclaredEnvelope,\n    \
             codRepr := fun S n w => intRepr S n w,\n    \
             model := AverCert.DeclaredIndexEnvelope.dEnvStructModel Plans.{name}DeclaredEnvelope Plans.{name}IntDispatchPlan.body }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
        ),
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
                expr_fragment_source_model(c, model_info)
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
             model := fun ns => {model_name} (ns.headD 0) }}\n\n",
            primary = scc[0].name,
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
            policy = c.policy().lean_value(),
            termination = c
                .termination_witness()
                .map_or_else(|| "none".to_string(), |w| format!("some {}", w.lean_value())),
            model_name = c.model_lean_name(model_info),
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
            model = c.model_expr(model_info),
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
    target: &str,
    abi: &str,
) -> String {
    let mut s = String::new();
    s.push_str(
        "import Schema\nimport Module\nimport PlanCheck\nimport PlanLower\nimport PlanBytes\nimport WasmSlice\nimport ExprFragmentAccepted\nimport ArtifactBytes\nimport Plans\nimport ConstructVerbatimSoundness\nimport StandardFace\n",
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
    let host_table_lean = analysis.frag_host_table().lean_value();
    for c in &analysis.certs {
        s.push_str(&render_obligation_def(c, model_info, &host_table_lean));
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
    // A module without the Int box helper export has no host-role table: the
    // manifest says `none`, matching the strict byte decoder, which proves the
    // `__rt_aint_from_i64` export absent and resolves the module-wide table to
    // `some none`. A module with the helper always declares `some` table (the
    // decoder resolves `some (some table)`), even when a role inside it is
    // unbound; a module whose role scan the decoder cannot complete is refused
    // at disassembly, so this branch never renders a table for it.
    let host_role_table = if analysis.frag_host_table.box_idx.is_some() {
        format!("some {}", analysis.frag_host_table.roles_lean_value())
    } else {
        "none".to_string()
    };
    let arith_params = analysis
        .frag_host_table
        .arith_params_lean_value(analysis.carrier);
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
         target := \"{target}\",\n        \
         profile := \"{PROFILE_ID}\",\n        \
         abi := \"{abi}\",\n        \
         artifactRoot := \"{ARTIFACT_CERTIFICATE_ROOT}\",\n        \
         exports := [{exports}],\n        \
         declaredUncertified := [{declared_uncertified}],\n        \
         capabilities := [{capabilities}],\n        \
         start := {start},\n        \
         hostRoleTable := {host_role_table},\n        \
         arithParams := {arith_params},\n        \
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
fn render_final() -> String {
    format!(
        "import Manifest\nimport ArtifactSoundness\n\n\
         set_option maxRecDepth 1000000\n\n\
         open AverCert AverCert.Schema\n\n\
         /-- THE single artifact certificate. All migrated families flow through\n\
         the audited accept-sound capstone; Artifact.dischargeSideConditions\n\
         isolates the float-only bespoke residual. -/\n\
         {FINAL_STATEMENT_LINE} :=\n  \
         AverCert.ArtifactSoundness.accept_sound_holds\n    \
         AverCert.Artifact.dischargeSideConditions\n\n\
         #print axioms {FINAL_THEOREM}\n"
    )
}

#[allow(clippy::too_many_arguments)]
fn render_manifest(
    analysis: &Analysis,
    model_info: &ModelInfo,
    artifact_file_name: &str,
    sha: &str,
    target: &str,
    abi: &str,
    wasip2_component_envelope: Option<crate::format::Wasip2ComponentEnvelopeDeclaration>,
    law_claims: &[LawClaim],
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
    s.push_str(&format!(
        "  \"format\": {{\"version\": {}, \"wall_id\": {}}},\n",
        wall::FORMAT_VERSION,
        json_str(wall::current_id()),
    ));
    s.push_str(&format!("  \"wasm\": {},\n", json_str(artifact_file_name)));
    s.push_str(&format!("  \"wasm_sha256\": \"{sha}\",\n"));
    s.push_str(&format!("  \"target\": {},\n", json_str(target)));
    s.push_str(&format!("  \"level\": \"{artifact_level}\",\n"));
    s.push_str(&format!("  \"profile\": \"{PROFILE_ID}\",\n"));
    s.push_str(&format!("  \"abi\": {},\n", json_str(abi)));
    if let Some(envelope) = wasip2_component_envelope {
        s.push_str(&format!(
            "  \"{}\": {{\"{}\": {}, \"{}\": {}, \"{}\": {}, \"{}\": {}}},\n",
            crate::format::WASIP2_COMPONENT_ENVELOPE_FIELD,
            crate::format::WASIP2_COMPONENT_ENVELOPE_KIND_FIELD,
            json_str(envelope.kind()),
            crate::format::WASIP2_COMPONENT_ENVELOPE_PREFIX_LEN_FIELD,
            envelope.prefix_len,
            crate::format::WASIP2_COMPONENT_ENVELOPE_CORE_LEN_FIELD,
            envelope.embedded_core_module_len,
            crate::format::WASIP2_COMPONENT_ENVELOPE_SUFFIX_LEN_FIELD,
            envelope.suffix_len,
        ));
    }
    s.push_str(&format!("  \"final_theorem\": \"{FINAL_THEOREM}\",\n"));
    s.push_str(&format!(
        "  \"artifact_certificate_root\": \"{ARTIFACT_CERTIFICATE_ROOT}\",\n"
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
    // Law-claims surface (schema 7): one entry per universal model law, with
    // the verbatim statement the checker-owned witness re-elaborates the
    // `Laws.lean` corollary at.
    s.push_str("  \"laws\": [");
    for (i, claim) in law_claims.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!(
            "\n    {{\"label\": {}, \"theorem\": {}, \"statement\": {}, \"corollary\": {}}}",
            json_str(&claim.label),
            json_str(&claim.qualified()),
            json_str(&claim.statement),
            json_str(&claim.corollary()),
        ));
    }
    if !law_claims.is_empty() {
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
    // Mirror of the Lean manifest: `null` when the module has no Int box
    // helper export (and therefore no host-role table), the exact role object
    // otherwise.
    if analysis.frag_host_table.box_idx.is_some() {
        s.push_str(&format!(
            "  \"hostRoleTable\": {{\"box\": {}, \"add\": {}, \"mul\": {}, \"sub\": {}, \"toIndex\": {}, \"cmp\": {}, \"eq\": {}}},\n",
            json_role(analysis.frag_host_table.box_idx),
            json_role(analysis.frag_host_table.add_idx),
            json_role(analysis.frag_host_table.mul_idx),
            json_role(analysis.frag_host_table.sub_idx),
            json_role(analysis.frag_host_table.to_index_idx),
            json_role(analysis.frag_host_table.cmp_idx),
            json_role(analysis.frag_host_table.eq_idx),
        ));
    } else {
        s.push_str("  \"hostRoleTable\": null,\n");
    }
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
            Cert::WidenedIntMatch { .. } | Cert::VariantDispatch { .. } => "int-dispatch",
            Cert::VerbatimWidenedMatch { .. } | Cert::VerbatimVariantDispatch { .. } => {
                "verbatim-dispatch"
            }
            Cert::StringEqVerbatimMatch { .. } => "verbatim-string-eq",
            Cert::StringConcatVerbatimMatch { .. } => "verbatim-string-concat",
            Cert::ExprFragment { .. } => "expr-fragment-v1",
            Cert::Composition { .. } => "cross-function-composition",
            Cert::MutualRecursion { .. } => "mutual-recursive",
            Cert::NonRecursive { .. } => unreachable!(),
        };
        let (dom, cod) = c.source_dom_cod(model_info);
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
            "AcceptanceSoundness.composition_claim_discharges_with_bridge".to_string()
        } else if c.int_cmp_bool_face().is_some() {
            "AcceptanceSoundness.intCmpBool_claim_discharges".to_string()
        } else if c.int_select_face().is_some() {
            "AcceptanceSoundness.intSelect_claim_discharges".to_string()
        } else if expr_fragment_uses_audited_generic(c) {
            "AcceptanceSoundness.exprFragment_claim_discharges".to_string()
        } else if c.vector_get_face().is_some() {
            "AverCert.StandardFace.vectorGetOrDefault_simulates_model".to_string()
        } else if c.project_face().is_some() {
            "AcceptanceSoundness.fieldProjection_direct_canonical_discharges".to_string()
        } else if c.record_compute_face().is_some() {
            crate::format::RECORD_COMPUTE_DISCHARGE_THEOREM.to_string()
        } else if c.record_param_face().is_some() {
            "AcceptanceSoundness.recordParam_claim_discharges".to_string()
        } else if matches!(c.inner(), Cert::FieldProjection { .. }) {
            "AcceptanceSoundness.fieldProjection_canonical_discharges".to_string()
        } else if matches!(c.inner(), Cert::AdtConstructor { .. })
            && adt_constructor_uses_model(c, model_info)
        {
            "AcceptanceSoundness.construct_canonical_discharges".to_string()
        } else if let Cert::AdtConstructor { arity, .. } = c.inner()
            && !adt_constructor_uses_model(c, model_info)
        {
            if *arity == 1 {
                "AcceptanceSoundness.constructUnary_canonical_discharges".to_string()
            } else {
                "AcceptanceSoundness.constructBinary_canonical_discharges".to_string()
            }
        } else if matches!(
            c.inner(),
            Cert::VerbatimWidenedMatch { .. } | Cert::VerbatimVariantDispatch { .. }
        ) {
            "AcceptanceSoundness.verbatim_canonical_discharges".to_string()
        } else if matches!(c.inner(), Cert::StringEqVerbatimMatch { .. }) {
            "AcceptanceSoundness.stringEq_canonical_discharges".to_string()
        } else if matches!(c.inner(), Cert::StringConcatVerbatimMatch { .. }) {
            "AcceptanceSoundness.stringConcat_canonical_discharges".to_string()
        } else if matches!(
            c.inner(),
            Cert::VariantDispatch { .. } | Cert::WidenedIntMatch { .. }
        ) {
            "AcceptanceSoundness.intDispatch_canonical_discharges".to_string()
        } else if recursion_uses_audited_generic(c) {
            "AcceptanceSoundness.recursion_claim_discharges".to_string()
        } else if matches!(c.inner(), Cert::MutualRecursion { .. }) {
            "AcceptanceSoundness.mutual_claim_discharges".to_string()
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
             \"theorem\": {}{}}}",
            json_str(c.name()),
            kind,
            policy.manifest_name(),
            policy.level(),
            json_str(&dom),
            json_str(&cod),
            json_str(&theorem),
            termination_json,
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
