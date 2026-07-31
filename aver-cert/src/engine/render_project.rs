// ---- rendering -----------------------------------------------------------

/// Write the artifact-specific `cert/` package. `model_files` are the
/// `(path, content)` pairs from the reused `aver proof` Lean emission. The
/// checker-owned wall and build configuration are resolved from `wall_id`.
/// Any existing `cert/` directory is removed first so a reused output path
/// cannot retain files from an older package format.
///
/// PRECONDITION: `model_files` must be the SAME emission `analysis` was
/// derived from. The renderers cite each certified export's model by the
/// qualified Lean name resolved from these files, and `analyze` already
/// declined every export whose name it could not resolve — so a mismatched
/// pair would leave a renderer with a citation the gate never approved. This
/// is checked here rather than assumed: a mismatch returns an error and no
/// package is written.
pub fn write_project(
    out_dir: &Path,
    wasm_name: &str,
    wasm_bytes: &[u8],
    analysis: &Analysis,
    model_files: &[(String, String)],
) -> Result<(), String> {
    // Enforce the model-file precondition BEFORE touching the output
    // directory, so a mismatched call leaves any existing package intact.
    let model_info = ModelInfo::from_files(model_files);
    for c in &analysis.certs {
        model_citation_gate(c, &model_info).map_err(|reason| {
            format!(
                "model files passed to write_project do not match the analysis: {reason}"
            )
        })?;
    }

    let cert_dir = out_dir.join("cert");
    match std::fs::remove_dir_all(&cert_dir) {
        Ok(()) => {}
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(format!("replace cert dir: {error}")),
    }
    std::fs::create_dir_all(&cert_dir).map_err(|e| format!("create cert dir: {e}"))?;

    // Copy the model files (AverCommon + <Module>.lean) verbatim.
    let mut model_roots: Vec<String> = Vec::new();
    for (path, content) in model_files {
        if path == "lakefile.lean" || path == "lean-toolchain" {
            continue;
        }
        write(&cert_dir, path, &sanitize_model_for_cert(content))?;
        if let Some(stem) = path.strip_suffix(".lean") {
            model_roots.push(model_root_from_stem(stem)?);
        }
    }

    let sha = {
        let mut h = Sha256::new();
        h.update(wasm_bytes);
        hex(&h.finalize())
    };

    write(&cert_dir, "Contracts.lean", &render_contracts(analysis))?;
    write(
        &cert_dir,
        "Module.lean",
        &render_module(analysis, wasm_name, &sha),
    )?;
    // Checker-owned Lean sources are identified by `format.wall_id` and are
    // materialized by the verifier. They are not duplicated in the package.
    // Byte-derived host-role table for source-plan encoding. One module-wide
    // value: every sym-plan encode example and artifact claim consumes it, so
    // plan-supplied indices can never enter the encoder.
    let host_table_lean = byte_derived_frag_host_table_lean(wasm_bytes)?;
    // Struct-binding table for field projections: one module-wide value,
    // unioned from the per-export (source plan, encoded plan) pairs the
    // byte-exact gate already pinned. Like the host-role table, plan-supplied
    // indices can never enter the encoder.
    let struct_table_lean = emit_frag_struct_table_lean(analysis)?;

    write(
        &cert_dir,
        "Plans.lean",
        &render_expr_fragment_plans(analysis, &model_info, &host_table_lean, &struct_table_lean),
    )?;
    write(
        &cert_dir,
        "Manifest.lean",
        &render_manifest_lean(analysis, &model_roots, &model_info, &sha),
    )?;
    write(
        &cert_dir,
        "Certificate.lean",
        &render_certificate(analysis, &model_roots, &model_info),
    )?;
    write(
        &cert_dir,
        "Final.lean",
        &render_final(),
    )?;
    write(
        &cert_dir,
        "Artifact.lean",
        &render_artifact(analysis, &model_info, &host_table_lean, &struct_table_lean),
    )?;
    write(
        &cert_dir,
        "ArtifactCertificate.lean",
        &render_artifact_certificate(),
    )?;
    write(
        &cert_dir,
        "ArtifactSoundness.lean",
        &render_artifact_soundness(),
    )?;
    std::fs::write(
        cert_dir.join("cert-manifest.json"),
        render_manifest(analysis, &model_info, wasm_name, &sha),
    )
    .map_err(|e| format!("write manifest: {e}"))?;
    Ok(())
}

/// The Lean module name a model file is imported by. A module declared as
/// `Data.Fibonacci` transpiles to the file `Data/Fibonacci.lean`; the import
/// lines in `Manifest.lean` and `Certificate.lean` need the dotted form back
/// (`import Data/Fibonacci` is not Lean syntax). Every path segment must be a
/// plain identifier so a model path can never smuggle stray syntax into an
/// interpolated import line.
fn model_root_from_stem(stem: &str) -> Result<String, String> {
    let segments: Vec<&str> = stem.split('/').collect();
    let valid = segments.iter().all(|segment| {
        let mut chars = segment.chars();
        matches!(chars.next(), Some(first) if first.is_ascii_alphabetic())
            && chars.all(|character| character.is_ascii_alphanumeric() || character == '_')
    });
    if valid {
        Ok(segments.join("."))
    } else {
        Err(format!(
            "model file stem `{stem}` is not a Lean module path (each segment must match ^[A-Za-z][A-Za-z0-9_]*$)"
        ))
    }
}

/// The module-wide struct table rendered at emit time: the consistent union of
/// every projection cert's byte-pinned entries.
fn emit_frag_struct_table_lean(analysis: &Analysis) -> Result<String, String> {
    let mut entries: Vec<(String, u32)> = Vec::new();
    for c in &analysis.certs {
        if let Cert::ExprFragment {
            source_plan: Some(source_plan),
            plan,
            ..
        } = c.inner()
        {
            let plan_entries =
                expr_fragment_struct_table_entries(source_plan, plan).ok_or_else(|| {
                    format!("inconsistent struct bindings in `{}` fragment plan", c.name())
                })?;
            entries.extend(plan_entries);
        }
    }
    frag_struct_table_lean_from_entries(entries.iter())
}

fn render_expr_fragment_plans(
    analysis: &Analysis,
    model_info: &ModelInfo,
    host_table_lean: &str,
    struct_table_lean: &str,
) -> String {
    let mut s = String::new();
    s.push_str(
        "-- Compiler-emitted source/fragment plans as Lean data.\n\
         -- This is the package's sole authoritative plan representation. The\n\
         -- checker-owned wall validates and lowers it against the actual artifact.\n\
         import Schema\n\
         import PlanCheck\n\n\
         import PlanLower\n\
         import PlanBytes\n\
         import WasmSlice\n\
         import ExprFragmentAccepted\n\
         import ArtifactBytes\n\
         import Module\n\
         import DeclaredEnvelopeAcceptTransport\n\n\
         set_option maxRecDepth 200000\n\n\
         namespace AverCert.Plans\n\
         open AverCert.Schema\n\n",
    );
    let mut any = false;
    for c in &analysis.certs {
        let Cert::ExprFragment {
            name,
            carrier,
            self_idx,
            type_idx,
            source_plan,
            plan,
            ..
        } = c.inner()
        else {
            continue;
        };
        let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(plan, *carrier)
            .expect("certified expr-fragment plan lowers to code-entry bytes");
        let code_entry_bytes = render_byte_list(&code_entry_bytes);
        let lowered_body = lower_expr_fragment_plan(plan, *carrier)
            .map(|ops| render_ops_value(&ops))
            .expect("certified expr-fragment plan lowers to WInstr body");
        let export_name_bytes = render_byte_list(name.as_bytes());
        let func_binding = format!(
            "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
        );
        let sym_plan = expr_fragment_source_plan(source_plan, plan)
            .map(|sym| {
                format!(
                    "/-- Source-level `SymPlan` projection for `{name}`. Artifact-level\n\
                     acceptance prefers this claim when the fragment has a direct\n\
                     Aver-level meaning; the encoder below still binds it to `{name}Plan`. -/\n\
                     def {name}SymPlan : SymRawPlan := {sym_plan}\n\n\
                     /-- The audited Lean-side source-plan checker accepts `{name}`'s `SymPlan`. -/\n\
                     example : AverCert.PlanCheck.checkSymRawPlan {name}SymPlan = true := rfl\n\n\
                     /-- The audited Lean-side source encoder maps `{name}`'s `SymPlan`,\n\
                         under the byte-derived host-role and struct tables, to the\n\
                         representation plan that is bound to bytes below. -/\n\
                     example : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan {host_table_lean} {struct_table_lean} {name}SymPlan =\n  \
                       some {name}Plan := rfl\n\n",
                    sym_plan = sym_plan_lean_value(&sym)
                )
            })
            .unwrap_or_else(|| {
                format!(
                    "-- `{name}` has no source-level `SymPlan` projection yet;\n\
                     -- its current fragment uses representation-only nodes.\n\n"
                )
            });
        any = true;
        s.push_str(&format!(
            "/-- Raw `expr-fragment-v1` plan for `{name}`. -/\n\
             def {name}Plan : ExprFragmentRawPlan := {plan_value}\n\n\
             {sym_plan}\
             /-- The audited Lean-side structural checker accepts `{name}`'s raw plan. -/\n\
             example : AverCert.PlanCheck.checkExprFragmentRawPlan {name}Plan = true := rfl\n\n\
             /-- The audited Lean-side canonical lowerer maps `{name}`'s raw plan\n\
                 to the same instruction body emitted in `Module.lean`. -/\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerExprFragmentBody {carrier} {name}Plan := rfl\n\n\
             /-- The audited Lean-side byte lowerer maps `{name}`'s raw plan\n\
                 to the exact canonical code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerExprFragmentCodeEntry {carrier} {name}Plan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The audited Lean-side Wasm slicer binds `{name}` to its function\n\
                 index, type index and exact expected code-entry bytes. -/\n\
             example : AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry_bytes} =\n  \
               some {func_binding} := rfl\n\n\
             /-- The audited Lean-side expr-fragment acceptance predicate aggregates\n\
                 plan checking, semantic lowering, byte lowering and byte-origin binding. -/\n\
             example : AverCert.ExprFragmentAccepted.accepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen\n  \
               {export_name_bytes} {carrier} {name}Plan\n  \
               {lowered_body}\n  \
               {code_entry_bytes}\n  \
               {func_binding} := by dsimp [AverCert.ExprFragmentAccepted.accepted]; exact ⟨rfl, rfl, rfl, rfl⟩\n\n",
            plan_value = expr_fragment_plan_lean_value(plan),
            sym_plan = sym_plan,
        ));
    }
    for c in &analysis.certs {
        let Cert::FieldProjection {
            name,
            self_idx,
            type_idx,
            carrier,
            struct_idx,
            field_count,
            ..
        } = c.inner()
        else {
            continue;
        };
        let Some((plan, result_ty)) = field_projection_plan_from_cert(c) else {
            continue;
        };
        let code_entry = lower_field_projection_code_entry(
            &plan,
            *carrier,
            *struct_idx,
            result_ty,
        );
        let code_entry = render_byte_list(&code_entry);
        let result_ty_lean = field_projection_result_ty_lean_value(result_ty);
        let export_name_bytes = render_byte_list(name.as_bytes());
        let binding = format!(
            "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry} }} : AverCert.WasmSlice.FuncBinding)"
        );
        any = true;
        s.push_str(&format!(
            "/-- Byte-origin plan for bare tuple/record projection `{name}`. -/\n\
             def {name}FieldProjectionPlan : FieldProjectionRawPlan := {plan_value}\n\n\
             example : AverCert.PlanCheck.checkFieldProjectionRawPlan {field_count} {name}FieldProjectionPlan = true := rfl\n\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerFieldProjectionBody {struct_idx} {field_count} {name}FieldProjectionPlan := rfl\n\n\
             example : AverCert.PlanBytes.lowerFieldProjectionCodeEntry {carrier} {struct_idx} {field_count} {result_ty_lean} {name}FieldProjectionPlan =\n  \
               some {code_entry} := rfl\n\n\
             example : AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry} =\n  \
               some {binding} := rfl\n\n\
             example : AverCert.WasmSlice.projectionStructTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {struct_idx} {field_count} {field_idx} {result_ty_lean} = true := rfl\n\n\
             example : AverCert.WasmSlice.projectionFuncTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {type_idx} {struct_idx} {result_ty_lean} = true := rfl\n\n",
            plan_value = field_projection_plan_lean_value(&plan),
            field_idx = plan.field_idx,
        ));
    }
    for c in &analysis.certs {
        let Cert::AdtConstructor {
            name,
            self_idx,
            type_idx,
            carrier,
            struct_idx,
            elem_ty,
            ..
        } = c.inner()
        else {
            continue;
        };
        let Some(sym_plan) = adt_constructor_sym_plan_from_cert(c, model_info) else {
            continue;
        };
        let Some(construct_plan) = construct_plan_from_cert(c) else {
            continue;
        };
        let Ok(code_entry_bytes) =
            lower_construct_plan_code_entry_bytes(&construct_plan, *carrier, *struct_idx)
        else {
            continue;
        };
        let lowered_body = lower_construct_plan(&construct_plan, *struct_idx)
            .map(|ops| render_ops_value(&ops))
            .expect("checked construct plan lowers to WInstr body");
        let code_entry_bytes = render_byte_list(&code_entry_bytes);
        let export_name_bytes = render_byte_list(name.as_bytes());
        let elem_ty = construct_val_type_lean_value(*elem_ty)
            .expect("certified constructor has a supported byte-level element type");
        let type_match_pins = if sym_plan_is_list_construct(&sym_plan) {
            format!(
                "/-- The byte-derived list struct binds `{name}`'s element representation. -/\n\
                 theorem {name}ConstructStructTypeMatches :\n  \
                   AverCert.WasmSlice.listConstructStructTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {struct_idx} ({elem_ty}) = true := rfl\n\n\
                 /-- The byte-derived exported signature binds `{name}`'s element representation. -/\n\
                 theorem {name}ConstructFuncTypeMatches :\n  \
                   AverCert.WasmSlice.listConstructFuncTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {type_idx} {name}ConstructPlan.arity {struct_idx} ({elem_ty}) = true := rfl\n\n"
            )
        } else {
            String::new()
        };
        let func_binding = format!(
            "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
        );
        any = true;
        s.push_str(&format!(
            "/-- Source-level constructor `SymPlan` for legacy ADT constructor `{name}`.\n\
                This says what source value is being constructed; the target-bound\n\
                `ConstructRawPlan` below pins the wasm-gc `struct.new` layout. -/\n\
             def {name}ConstructSymPlan : SymRawPlan := {sym_plan_value}\n\n\
             /-- Target-bound constructor witness for `{name}`. -/\n\
             def {name}ConstructPlan : ConstructRawPlan := {construct_plan_value}\n\n\
             /-- The audited Lean-side source-plan checker accepts `{name}`'s constructor plan. -/\n\
             example : AverCert.PlanCheck.checkSymRawPlan {name}ConstructSymPlan = true := rfl\n\n\
             /-- The audited Lean-side structural checker accepts `{name}`'s target constructor plan. -/\n\
             example : AverCert.PlanCheck.checkConstructRawPlan {name}ConstructPlan = true := rfl\n\n\
             /-- The audited Lean-side source/target matcher confirms that `{name}`'s\n\
                 source constructor plan explains the byte-bound constructor witness. -/\n\
             example : AverCert.PlanCheck.constructPlanMatchesSymRawPlan\n  \
               {name}ConstructSymPlan {name}ConstructPlan = true := rfl\n\n\
             /-- `construct` is not yet part of the v1 source-to-fragment encoder. -/\n\
             example : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan {host_table_lean} {struct_table_lean} {name}ConstructSymPlan = none := rfl\n\n\
             /-- The audited Lean-side canonical lowerer maps `{name}`'s constructor plan\n\
                 to the exact instruction body. -/\n\
             example : AverCert.PlanLower.lowerConstructBody {struct_idx} {name}ConstructPlan =\n  \
               some {lowered_body} := rfl\n\n\
             /-- The audited Lean-side canonical lowerer maps `{name}`'s constructor plan\n\
                 to the same instruction body emitted in `Module.lean`. -/\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerConstructBody {struct_idx} {name}ConstructPlan := rfl\n\n\
             /-- The audited Lean-side byte lowerer maps `{name}`'s constructor plan\n\
                 to the exact canonical code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerConstructCodeEntry {carrier} {struct_idx} {name}ConstructPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The audited Lean-side Wasm slicer binds `{name}` to its function\n\
                 index, type index and exact expected code-entry bytes. -/\n\
             example : AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry_bytes} =\n  \
               some {func_binding} := rfl\n\n\
             {type_match_pins}",
            sym_plan_value = sym_plan_lean_value(&sym_plan),
            construct_plan_value = construct_plan_lean_value(&construct_plan),
            lowered_body = lowered_body,
        ));
    }
    for c in &analysis.certs {
        let Cert::StringConcatVerbatimMatch {
            name,
            self_idx,
            type_idx,
            carrier,
            string_concat_idx,
            container_ty,
            result_ty,
            ..
        } = c.inner()
        else {
            continue;
        };
        let plan = string_concat_plan_from_cert(c)
            .expect("certified String.concat should project to a source plan");
        let sym_plan = string_concat_sym_plan_from_plan(&plan);
        let code_entry_bytes = lower_string_concat_plan_code_entry_bytes(
            &plan,
            *carrier,
            *result_ty,
            *container_ty,
            *string_concat_idx,
        )
        .expect("certified String.concat plan lowers to code-entry bytes");
        let code_entry_bytes = render_byte_list(&code_entry_bytes);
        let export_name_bytes = render_byte_list(name.as_bytes());
        let func_binding = format!(
            "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
        );
        any = true;
        s.push_str(&format!(
            "/-- Source-level `SymPlan` view of `{name}`'s string concatenation. -/\n\
             def {name}StringConcatSymPlan : SymRawPlan := {sym_plan_value}\n\n\
             /-- Target-bound `string-concat-v1` encoder witness for `{name}`. -/\n\
             def {name}StringConcatPlan : StringConcatRawPlan := {plan_value}\n\n\
             /-- The audited Lean-side source-plan checker accepts `{name}`'s string `SymPlan`. -/\n\
             example : AverCert.PlanCheck.checkSymRawPlan {name}StringConcatSymPlan = true := rfl\n\n\
             /-- The audited Lean-side source/target matcher confirms that `{name}`'s\n\
                 string `SymPlan` explains the byte-bound String.concat plan below. -/\n\
             example : AverCert.PlanCheck.stringConcatPlanMatchesSymRawPlan\n  \
               {name}StringConcatSymPlan {name}StringConcatPlan = true := rfl\n\n\
             /-- The audited Lean-side structural checker accepts `{name}`'s String.concat plan. -/\n\
             example : AverCert.PlanCheck.checkStringConcatRawPlan {name}StringConcatPlan = true := rfl\n\n\
             /-- The audited Lean-side canonical lowerer maps `{name}`'s String.concat plan\n\
                 to the same instruction body emitted in `Module.lean`. -/\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerStringConcatBody {result_ty} {container_ty} {string_concat_idx} {name}StringConcatPlan := rfl\n\n\
             /-- The audited Lean-side byte lowerer maps `{name}`'s String.concat plan\n\
                 to the exact canonical code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerStringConcatCodeEntry {carrier_state} {result_ty} {container_ty} {string_concat_idx} {name}StringConcatPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The audited Lean-side Wasm slicer binds `{name}` to its function\n\
                 index, type index and exact expected code-entry bytes. -/\n\
             example : AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry_bytes} =\n  \
               some {func_binding} := rfl\n\n\
             \n",
            sym_plan_value = sym_plan_lean_value(&sym_plan),
            plan_value = string_concat_plan_lean_value(&plan),
            carrier_state = render_carrier_state(*carrier),
        ));
    }
    // The carrier state is a property of the MODULE, not of an export, so its
    // audit line is emitted once for the whole package rather than repeated
    // identically per concatenation. Acceptance pins it per claim anyway
    // (`stringConcatPlanAccepted`); this is the readable restatement.
    if let Some(carrier) = analysis
        .certs
        .iter()
        .find_map(|c| match c.inner() {
            Cert::StringConcatVerbatimMatch { carrier, .. } => Some(*carrier),
            _ => None,
        })
    {
        s.push_str(&format!(
            "/-- The module's byte-derived carrier state: the value that selects the\n\
                 locals prelude of every `string-concat-v1` code entry above.\n\
                 Recomputed from the artifact bytes, so no claim can declare a\n\
                 prelude the type section does not license. -/\n\
             example : CertDecode.carrierState AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen =\n  \
               some {carrier_state} := rfl\n\n",
            carrier_state = render_carrier_state(carrier),
        ));
    }
    for c in &analysis.certs {
        let Cert::StringEqVerbatimMatch {
            name,
            self_idx,
            type_idx,
            carrier,
            string_eq_idx,
            ..
        } = c.inner()
        else {
            continue;
        };
        let plan = string_eq_plan_from_cert(c)
            .expect("certified String.eq should project to a source plan");
        let sym_plan = string_eq_sym_plan_from_plan(&plan);
        let string_ty =
            string_eq_string_ty_from_cert(c).expect("certified String.eq should use string arrays");
        let code_entry_bytes =
            lower_string_eq_plan_code_entry_bytes(&plan, *carrier, string_ty, *string_eq_idx)
                .expect("certified String.eq plan lowers to code-entry bytes");
        let code_entry_bytes = render_byte_list(&code_entry_bytes);
        let export_name_bytes = render_byte_list(name.as_bytes());
        let func_binding = format!(
            "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
        );
        any = true;
        s.push_str(&format!(
            "/-- Source-level `SymPlan` view of `{name}`'s String.eq dispatch. -/\n\
             def {name}StringEqSymPlan : SymRawPlan := {sym_plan_value}\n\n\
             /-- Target-bound `string-eq-v1` encoder witness for `{name}`. -/\n\
             def {name}StringEqPlan : StringEqRawPlan := {plan_value}\n\n\
             /-- The audited Lean-side source-plan checker accepts `{name}`'s String.eq `SymPlan`. -/\n\
             example : AverCert.PlanCheck.checkSymRawPlan {name}StringEqSymPlan = true := rfl\n\n\
             /-- The audited Lean-side source/target matcher confirms that `{name}`'s\n\
                 string `SymPlan` explains the byte-bound String.eq plan below. -/\n\
             example : AverCert.PlanCheck.stringEqPlanMatchesSymRawPlan\n  \
               {name}StringEqSymPlan {name}StringEqPlan = true := rfl\n\n\
             /-- The audited Lean-side structural checker accepts `{name}`'s String.eq plan. -/\n\
             example : AverCert.PlanCheck.checkStringEqRawPlan {name}StringEqPlan = true := rfl\n\n\
             /-- The audited Lean-side canonical lowerer maps `{name}`'s String.eq plan\n\
                 to the same instruction body emitted in `Module.lean`. -/\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerStringEqBody {string_ty} {string_eq_idx} {name}StringEqPlan := rfl\n\n\
             /-- The audited Lean-side byte lowerer maps `{name}`'s String.eq plan\n\
                 to the exact canonical code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerStringEqCodeEntry {carrier} {string_ty} {string_eq_idx} {name}StringEqPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The audited Lean-side Wasm slicer binds `{name}` to its function\n\
                 index, type index and exact expected code-entry bytes. -/\n\
             example : AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry_bytes} =\n  \
               some {func_binding} := rfl\n\n\
             \n",
            sym_plan_value = sym_plan_lean_value(&sym_plan),
            plan_value = string_eq_plan_lean_value(&plan),
        ));
    }
    for c in &analysis.certs {
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
            } => (name, *self_idx, *type_idx, *carrier),
            _ => continue,
        };
        let Some(plan) = recursion_plan_from_cert(c) else {
            continue;
        };
        let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, carrier)
            .expect("certified recursion plan lowers to code-entry bytes");
        let code_entry_bytes = render_byte_list(&code_entry_bytes);
        let export_name_bytes = render_byte_list(name.as_bytes());
        let func_binding = format!(
            "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
        );
        let host_table = recursion_host_table_lean_value(c);
        let totality_role = c.totality_role_lean_value();
        let wrong_totality_role = if c.requires_mul_totality() {
            ".addSub"
        } else {
            ".mul"
        };
        let arity = plan.params.len();
        any = true;
        s.push_str(&format!(
            "/-- Byte-derived `recursion-plan-v1` plan for `{name}` (fuel-recursive). -/\n\
             def {name}RecursionPlan : RecursionRawPlan := {plan_value}\n\n\
             /-- The audited recursion-plan checker accepts `{name}`'s byte-derived plan. -/\n\
             example : AverCert.PlanCheck.checkRecursionRawPlan {name}RecursionPlan = true := rfl\n\n\
             /-- The context-sensitive recursion grammar accepts `{name}`'s plan: the\n\
                 self-call targets the export's own byte-derived function index and every\n\
                 host call cites the byte-derived role table. -/\n\
             example : AverCert.PlanCheck.checkRecursionPlanShape {self_idx} {host_table} {totality_role} {name}RecursionPlan = true := rfl\n\n\
             /-- The opposite totality role is rejected by the same byte-pinned grammar. -/\n\
             example : AverCert.PlanCheck.checkRecursionPlanShape {self_idx} {host_table} {wrong_totality_role} {name}RecursionPlan = false := rfl\n\n\
             /-- The declared function type of `{name}` is the canonical certified\n\
                 signature over the Int carrier. -/\n\
             example : AverCert.WasmSlice.funcTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {type_idx} {arity} {carrier} = true := rfl\n\n\
             /-- The audited recursion lowerer maps `{name}`'s plan to the exact `Module.lean` body. -/\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerRecursionBody {carrier} {name}RecursionPlan := rfl\n\n\
             /-- The audited recursion byte lowerer reproduces `{name}`'s exact code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerRecursionCodeEntry {carrier} {name}RecursionPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The Wasm slicer finds `{name}`'s exact code-entry bytes by export name. -/\n\
             example : AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry_bytes} =\n  \
               some {func_binding} := rfl\n\n",
            plan_value = recursion_plan_lean_value(&plan),
        ));
    }
    for c in &analysis.certs {
        let Cert::MutualRecursion {
            name,
            self_idx,
            carrier,
            box_idx,
            sub_idx,
            position,
            scc,
        } = c.inner()
        else {
            continue;
        };
        let Some(plan) = mutual_plan_from_cert(c) else {
            continue;
        };
        let member = &scc[*position];
        let type_idx = member.type_idx;
        let primary = &scc[0].name;
        let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier)
            .expect("certified mutual member plan lowers to code-entry bytes");
        let code_entry_bytes = render_byte_list(&code_entry_bytes);
        let export_name_bytes = render_byte_list(name.as_bytes());
        let func_binding = format!(
            "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
        );
        let host_table = mutual_host_table_lean_value(*box_idx, *sub_idx);
        let member_set = mutual_member_set_lean_value(scc);
        any = true;
        s.push_str(&format!(
            "/-- Byte-derived `mutual-plan-v1` plan for `{name}` (one member of the\n\
                 mutually-recursive SCC whose shared code table `{primary}Code` emits). -/\n\
             def {name}MutualPlan : MutualRawPlan := {plan_value}\n\n\
             /-- The audited mutual-plan checker accepts `{name}`'s byte-derived plan. -/\n\
             example : AverCert.PlanCheck.checkMutualRawPlan {name}MutualPlan = true := rfl\n\n\
             /-- The context-sensitive mutual grammar accepts `{name}`'s plan: the\n\
                 member-call targets an index in the byte-derived SCC member set and every\n\
                 host call cites the byte-derived box/sub role table. -/\n\
             example : AverCert.PlanCheck.checkMutualPlanShape {member_set} {host_table} {name}MutualPlan = true := rfl\n\n\
             /-- The declared function type of `{name}` is the canonical certified\n\
                 signature over the Int carrier. -/\n\
             example : AverCert.WasmSlice.funcTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {type_idx} 1 {carrier} = true := rfl\n\n\
             /-- The audited mutual lowerer maps `{name}`'s plan to its arm of the shared\n\
                 `{primary}Code` table (the exact `Module.lean` body). -/\n\
             example : (CertModule.{primary}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerMutualBody {carrier} {name}MutualPlan := rfl\n\n\
             /-- The audited mutual byte lowerer reproduces `{name}`'s exact code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerMutualCodeEntry {carrier} {name}MutualPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The Wasm slicer finds `{name}`'s exact code-entry bytes by export name. -/\n\
             example : AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry_bytes} =\n  \
               some {func_binding} := rfl\n\n",
            plan_value = mutual_plan_lean_value(&plan),
        ));
    }
    for c in &analysis.certs {
        let (name, self_idx, carrier) = match c.inner() {
            Cert::VerbatimWidenedMatch {
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
            } => (name, *self_idx, *carrier),
            _ => continue,
        };
        let Some(plan) = verbatim_plan_from_cert(c) else {
            continue;
        };
        let code_entry_bytes = render_byte_list(&lower_verbatim_code_entry(&plan, carrier));
        let export_name_bytes = render_byte_list(name.as_bytes());
        let nlocals = if verbatim_dispatch_has_projection(&plan.body) {
            3
        } else {
            2
        };
        any = true;
        s.push_str(&format!(
            "/-- Byte-derived `verbatim-plan-v1` plan for `{name}` (a `Cod := WVal`\n\
                 verbatim `ref.test`-dispatch match; no host, no representation). -/\n\
             def {name}VerbatimPlan : VerbatimRawPlan := {plan_value}\n\n\
             /-- The audited verbatim-plan admission checker accepts `{name}`'s\n\
                 byte-derived plan with its canonical local count. -/\n\
             example : AverCert.PlanCheck.checkVerbatimPlan {nlocals} {name}VerbatimPlan = true := rfl\n\n\
             /-- The audited verbatim lowerer maps `{name}`'s plan to the exact `Module.lean` body. -/\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               some (AverCert.PlanLower.lowerVerbatimBody {name}VerbatimPlan) := rfl\n\n\
             /-- The audited verbatim byte lowerer reproduces `{name}`'s exact code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerVerbatimCodeEntry {carrier} {name}VerbatimPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The Wasm slicer finds `{name}`'s export binding with the exact code-entry bytes. -/\n\
             example : (AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry_bytes}).isSome = true := rfl\n\n",
            plan_value = verbatim_plan_lean_value(&plan),
        ));
    }
    for c in &analysis.certs {
        let (name, self_idx, carrier) = match c.inner() {
            Cert::VariantDispatch {
                name,
                self_idx,
                carrier,
                ..
            }
            | Cert::WidenedIntMatch {
                name,
                self_idx,
                carrier,
                ..
            } => (name, *self_idx, *carrier),
            _ => continue,
        };
        let Some(plan) = int_dispatch_plan_from_cert(c, analysis.frag_host_table) else {
            continue;
        };
        let hosts = int_dispatch_host_table_from_cert(c)
            .expect("Int-face dispatch cert carries its host table");
        let code_entry_bytes = render_byte_list(
            &lower_int_dispatch_code_entry(&plan, carrier, &hosts)
                .expect("certified Int-face dispatch plan lowers to code-entry bytes"),
        );
        let export_name_bytes = render_byte_list(name.as_bytes());
        let host_table = int_dispatch_host_table_lean_value(&hosts);
        any = true;
        s.push_str(&format!(
            "/-- Byte-derived `int-dispatch-v1` plan for `{name}` (a `Cod := Int`\n\
                 ADT-match; host helpers are cited by ROLE — the byte-derived role\n\
                 table below parameterizes the lowerers, so the plan carries no\n\
                 function index). -/\n\
             def {name}IntDispatchPlan : IntDispatchRawPlan := {plan_value}\n\n\
             /-- The audited int-dispatch checker accepts `{name}`'s byte-derived plan. -/\n\
             example : AverCert.PlanCheck.checkIntDispatchRawPlan {name}IntDispatchPlan = true := rfl\n\n\
             /-- The byte-derived role table maps roles to pairwise distinct indices\n\
                 (so the byte-equality gate distinguishes an arm's role). -/\n\
             example : AverCert.PlanCheck.hostTableIndicesDistinct {host_table} = true := rfl\n\n\
             /-- The audited int-dispatch lowerer maps `{name}`'s plan to the exact `Module.lean` body. -/\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerIntDispatchBody {host_table} {name}IntDispatchPlan := rfl\n\n\
             /-- The audited int-dispatch byte lowerer reproduces `{name}`'s exact code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerIntDispatchCodeEntry {carrier} {host_table} {name}IntDispatchPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The Wasm slicer finds `{name}`'s export binding with the exact code-entry bytes. -/\n\
             example : (AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry_bytes}).isSome = true := rfl\n\n",
            plan_value = int_dispatch_plan_lean_value(&plan),
        ));
    }
    for (entry, plan) in composition_member_plans(analysis) {
        let Cert::Composition { carrier, closure, .. } = analysis
            .certs
            .iter()
            .find(|cert| match cert.inner() {
                Cert::Composition { closure, .. } => {
                    closure.iter().any(|candidate| candidate.name == entry.name)
                }
                _ => false,
            })
            .expect("composition member belongs to a composition cert")
            .inner()
        else {
            unreachable!()
        };
        let add_idx = analysis
            .frag_host_table
            .add_idx
            .expect("plan-backed composition has strict add host");
        let funcs = composition_func_table(closure);
        let lowered = lower_composition_plan(&plan, add_idx, &funcs)
            .expect("composition member plan lowers");
        let code_entry = composition_code_entry_bytes(&plan, *carrier, add_idx, &funcs)
            .expect("composition member plan byte-lowers");
        let host_table = composition_host_table_lean_value(add_idx);
        let func_table = composition_func_table_lean_value(closure);
        let export_name_bytes = render_byte_list(entry.name.as_bytes());
        let binding = format!(
            "({{ funcIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
            entry.self_idx,
            entry.type_idx,
            render_byte_list(&code_entry),
        );
        any = true;
        s.push_str(&format!(
            "/-- Byte-derived composition shape for `{name}`. Callees are named;\n\
                 numeric indices come from the byte-derived function table. -/\n\
             def {name}CompositionPlan : CompositionRawPlan := {plan_value}\n\n\
             example : AverCert.PlanCheck.checkCompositionRawPlan {name}CompositionPlan = true := rfl\n\n\
             example : AverCert.PlanLower.lowerCompositionBody {host_table} {func_table} {name}CompositionPlan =\n  \
               some {body} := rfl\n\n\
             example : AverCert.PlanBytes.lowerCompositionCodeEntry {carrier} {host_table} {func_table} {name}CompositionPlan =\n  \
               some {code_entry} := rfl\n\n\
             example : AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry} =\n  \
               some {binding} := rfl\n\n",
            name = entry.name,
            plan_value = composition_plan_lean_value(&plan),
            body = render_ops_value(&lowered),
            code_entry = render_byte_list(&code_entry),
            binding = binding,
        ));
    }
    for c in &analysis.certs {
        let Some(envelope) = declared_envelope_for_cert(c, model_info, analysis) else {
            continue;
        };
        any = true;
        s.push_str(&format!(
            "/-- Declared ADT envelope for `{name}`: root, carrier and every constructor's\n\
                 flattened type index, shape and payload target. The wall confirms the whole\n\
                 declaration against the module bytes with one type-section walk-match\n\
                 traversal (`DeclaredIndexEnvelope.dWalkPinned`). -/\n\
             def {name}DeclaredEnvelope : AverCert.DeclaredIndexEnvelope.DIdxEnvelope :=\n  \
             {env}\n\n\
             /-- Opaque declared type-section bytes before `{name}`'s constructor entries. -/\n\
             def {name}TypePrefix : List Nat :=\n  \
             {prefix}\n\n",
            name = c.name(),
            env = envelope.lean_env(),
            prefix = envelope.lean_prefix(),
        ));
    }
    if analysis.certs.iter().any(|c| {
        declared_envelope_for_cert(c, model_info, analysis).is_some()
            || matches!(c.inner(), Cert::StringConcatVerbatimMatch { .. })
    }) {
        any = true;
        s.push_str(
            "/-- Located type-section entry cursor: the section payload found by\n\
                 `modulePayload 1`, advanced past the vector-count LEB. A framing\n\
                 locate only — no type entry is parsed. -/\n\
             def declaredTypeCur : Nat × Nat :=\n  \
             (AverCert.DeclaredIndexEnvelope.typeSectionCursor AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen).getD (0, 0)\n\n",
        );
    }
    if !any {
        s.push_str("-- This artifact contains no source/fragment plans.\n\n");
    }
    s.push_str("end AverCert.Plans\n");
    s
}

/// The declared envelope backing a user-ADT claim, when the certificate must
/// carry one: Int-face dispatch (keyed by the first tested variant) and
/// model-bearing named-ADT constructors (keyed by the constructed variant).
/// Analysis keeps only certs whose envelope is extractable, so a `None` here
/// means the cert simply carries no envelope.
fn declared_envelope_for_cert<'a>(
    c: &Cert,
    model_info: &ModelInfo,
    analysis: &'a Analysis,
) -> Option<&'a DeclaredEnvelope> {
    match c.inner() {
        Cert::VariantDispatch { .. } | Cert::WidenedIntMatch { .. } => {
            let plan = int_dispatch_plan_from_cert(c, analysis.frag_host_table)?;
            let first = int_dispatch_test_tags(&plan.body).first().copied()?;
            analysis.declared_envelopes.for_ctor(first)
        }
        Cert::AdtConstructor { struct_idx, .. }
            if adt_constructor_uses_model(c, model_info) =>
        {
            analysis.declared_envelopes.for_ctor(*struct_idx)
        }
        _ => None,
    }
}

struct RenderedArtifactClaims {
    sym_claims: String,
    string_eq_claims: String,
    string_claims: String,
    construct_claims: String,
    recursion_claims: String,
    mutual_claims: String,
    verbatim_claims: String,
    int_dispatch_claims: String,
    field_projection_claims: String,
    composition_members: String,
    composition_claims: String,
    claim_proof_bundles: String,
    obligation_proof: String,
    sym_proof: String,
    string_eq_proof: String,
    string_proof: String,
    construct_proof: String,
    recursion_proof: String,
    mutual_proof: String,
    verbatim_proof: String,
    int_dispatch_proof: String,
    field_projection_proof: String,
    composition_proof: String,
    string_concat_face_proof: String,
    construct_face_proof: String,
    int_dispatch_face_proof: String,
}

/// Render one opaque declared-envelope face theorem per claim plus the
/// aggregate spine `standardFacesChecked` consumes for the family slot.
/// `None` elements are known-face claims closed inline by `repeat' constructor`.
fn render_face_bundles(
    family: &str,
    face_def: &str,
    claims_def: &str,
    extra_unfolds: &str,
    host_table_conj: bool,
    elements: &[Option<(String, String)>],
) -> (String, String) {
    let mut theorems = String::new();
    let mut parts = Vec::with_capacity(elements.len());
    for (index, element) in elements.iter().enumerate() {
        match element {
            Some((plan_ref, proof)) => {
                let theorem = format!("{family}Claim{index}Face");
                theorems.push_str(&format!(
                    "theorem {theorem} :\n  \
                     AverCert.StandardFace.{face_def} AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen ({claims_def}.get ⟨{index}, by decide⟩) {plan_ref} := by\n  \
                     dsimp [{claims_def}, AverCert.StandardFace.{face_def}{extra_unfolds}]\n  \
                     exact {proof}\n\n"
                ));
                if host_table_conj {
                    parts.push(format!("⟨rfl, {theorem}⟩"));
                } else {
                    parts.push(theorem);
                }
            }
            None => parts.push("(by repeat' constructor)".to_string()),
        }
    }
    let aggregate = parts
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |rest, part| {
            format!("⟨{part}, {rest}⟩")
        });
    (theorems, aggregate)
}

/// Render one opaque theorem per concrete claim plus the small constructor
/// spine used by the corresponding per-family theorem.  Indexing the already
/// defined family list avoids repeating the large claim literals in theorem
/// statements; reduction proves that each indexed claim is definitionally the
/// same value consumed by the aggregate predicate.
fn render_per_claim_bundles(
    family: &str,
    predicate: &str,
    claims_def: &str,
    unfolds: &str,
    proofs: &[String],
) -> (String, String) {
    let mut theorems = String::new();
    let mut names = Vec::with_capacity(proofs.len());
    for (index, proof) in proofs.iter().enumerate() {
        let theorem = format!("{family}Claim{index}Accepted");
        theorems.push_str(&format!(
            "theorem {theorem} :\n  {predicate} ({claims_def}.get ⟨{index}, by decide⟩) := by\n  dsimp [{claims_def}, {unfolds}]\n  exact {proof}\n\n"
        ));
        names.push(theorem);
    }
    let aggregate = names
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |rest, theorem| {
            format!("⟨{theorem}, {rest}⟩")
        });
    (theorems, aggregate)
}

/// Render, per claim, the SPLIT acceptance proof: `per_claim(index)` supplies
/// the already-numbered witness `def`s and per-conjunct leaf theorems as one
/// text block plus the aggregate proof term that combines those constants. The
/// aggregate `{family}Claim{index}Accepted` theorem is stated exactly like the
/// opaque one `render_per_claim_bundles` emits — it indexes the same family
/// list and `dsimp`s the same predicate — but its `exact` references the leaf
/// constants instead of inlining one monolithic witness tuple.
///
/// This is the mechanism the expr-fragment beachhead established generalized to
/// the other data-carrying families: emitting the heavy `WInstr` body, the
/// code-entry bytes, and the function binding as their OWN top-level `def`s (so
/// no large literal is duplicated into the aggregate term) and each acceptance
/// conjunct that walks the module bytes as its OWN leaf theorem means every
/// `addDecl` completes and frees before the next is elaborated. The per-claim
/// kernel peak becomes the largest single leaf (a `modBytes` type-section walk)
/// rather than the whole witness tuple held live at once. The leaf conjuncts are
/// stated over the encoded plan `def` the aggregate reduces to, so the
/// aggregate re-runs no byte-decode or lowering work.
fn render_split_bundles(
    family: &str,
    predicate: &str,
    claims_def: &str,
    unfolds: &str,
    count: usize,
    per_claim: impl Fn(usize) -> (String, String),
) -> (String, String) {
    let mut theorems = String::new();
    let mut names = Vec::with_capacity(count);
    for index in 0..count {
        let (declarations, aggregate_proof) = per_claim(index);
        let accepted = format!("{family}Claim{index}Accepted");
        theorems.push_str(&declarations);
        theorems.push_str(&format!(
            "theorem {accepted} :\n  {predicate} ({claims_def}.get ⟨{index}, by decide⟩) := by\n  dsimp [{claims_def}, {unfolds}]\n  exact {aggregate_proof}\n\n"
        ));
        names.push(accepted);
    }
    let aggregate = names
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |rest, theorem| {
            format!("⟨{theorem}, {rest}⟩")
        });
    (theorems, aggregate)
}

/// Structured inputs for one expr-fragment (`sym`) claim's split acceptance
/// proof. The heavy witness data (lowered body, code-entry bytes, function
/// binding) and each acceptance conjunct are emitted as their OWN top-level
/// declarations by `render_sym_claim_bundles`.
struct SymClaimParts {
    name: String,
    lowered_body: String,
    code_entry_bytes: String,
    export_name_bytes: String,
    self_idx: u32,
    type_idx: u32,
    carrier: u32,
}

/// Render, per `sym` claim, named data `def`s + per-conjunct leaf theorems +
/// the aggregate `symFragmentClaim{i}Accepted` theorem that combines the
/// already-checked constants.
///
/// The former shape emitted ONE theorem per claim whose proof was
/// `dsimp [...]; exact ⟨…giant nested tuple…⟩`. A `theorem` submits its entire
/// proof term to the kernel in a single `addDecl`, so the whole witness tuple
/// (the large `WInstr` body list, the code-entry bytes, the binding record, and
/// every conjunct's `isDefEq`) was held live at once — the affine verify peaked
/// at ~6.7 GB there. Splitting the data into `def`s and each conjunct into its
/// own `theorem` means every `addDecl` completes and frees before the next is
/// elaborated, so the per-claim peak becomes the largest single leaf (the
/// `modBytes` decode / type-section walks, each measured ≤ ~1.24 GB) rather
/// than the whole tuple. The aggregate then only references the constants, so
/// its own `exact` re-runs no heavy reduction: the encoded plan the conjuncts
/// are stated over is `AverCert.Plans.{name}Plan`, definitionally the value the
/// aggregate's `dsimp` computes, so unifying them is a structural plan compare,
/// not a byte-carrier walk.
///
/// Only the expr-fragment family is split this way; the other nine still emit
/// one opaque theorem via `render_per_claim_bundles`.
fn render_sym_claim_bundles(parts: &[SymClaimParts], host_table_lean: &str) -> (String, String) {
    let mut theorems = String::new();
    let mut names = Vec::with_capacity(parts.len());
    for (index, part) in parts.iter().enumerate() {
        let SymClaimParts {
            name,
            lowered_body,
            code_entry_bytes,
            export_name_bytes,
            self_idx,
            type_idx,
            carrier,
        } = part;
        let body = format!("symFragmentClaim{index}Body");
        let code_entry = format!("symFragmentClaim{index}CodeEntry");
        let binding = format!("symFragmentClaim{index}Binding");
        let carrier_bound = format!("symFragmentClaim{index}CarrierBound");
        let host_types = format!("symFragmentClaim{index}HostTableFuncTypes");
        let check_plan = format!("symFragmentClaim{index}CheckPlan");
        let lower_body = format!("symFragmentClaim{index}LowerBody");
        let lower_code = format!("symFragmentClaim{index}LowerCodeEntry");
        let func_binding = format!("symFragmentClaim{index}FuncBinding");
        let func_type = format!("symFragmentClaim{index}FuncTypeMatches");
        let nominal = format!("symFragmentClaim{index}NominalTypes");
        let accepted = format!("symFragmentClaim{index}Accepted");
        let plan = format!("AverCert.Plans.{name}Plan");
        theorems.push_str(&format!(
            "-- Witness data for `{name}` as named constants so no large literal is\n\
             -- duplicated across the leaf statements or baked into the aggregate term.\n\
             def {body} : List CertPrelude.WInstr := {lowered_body}\n\n\
             def {code_entry} : AverCert.WasmSlice.ByteSeq := {code_entry_bytes}\n\n\
             def {binding} : AverCert.WasmSlice.FuncBinding :=\n  \
               {{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry} }}\n\n\
             -- One leaf theorem per acceptance conjunct. Each is checked and freed in\n\
             -- its own `addDecl`; the heavy ones are the `modBytes` type-section walks\n\
             -- and the function-binding decode.\n\
             theorem {carrier_bound} :\n  \
               AverCert.AcceptedArtifact.symFragmentCarrierBound AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {carrier} {host_table_lean} {plan} = true := by\n  \
               rfl\n\n\
             theorem {host_types} :\n  \
               AverCert.WasmSlice.hostTableFuncTypesMatch AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {carrier} {host_table_lean} = true := by\n  \
               rfl\n\n\
             theorem {check_plan} :\n  \
               AverCert.PlanCheck.checkExprFragmentRawPlan {plan} = true := by\n  \
               rfl\n\n\
             theorem {lower_body} :\n  \
               AverCert.PlanLower.lowerExprFragmentBody {carrier} {plan} = some {body} := by\n  \
               rfl\n\n\
             theorem {lower_code} :\n  \
               AverCert.PlanBytes.lowerExprFragmentCodeEntry {carrier} {plan} = some {code_entry} := by\n  \
               rfl\n\n\
             theorem {func_binding} :\n  \
               AverCert.WasmSlice.exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {export_name_bytes} {code_entry} = some {binding} := by\n  \
               rfl\n\n\
             theorem {func_type} :\n  \
               AverCert.WasmSlice.exprFragmentFuncTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {binding}.typeIdx {carrier} {plan}.params {plan}.result = true := by\n  \
               rfl\n\n\
             theorem {nominal} :\n  \
               AverCert.WasmSlice.exprFragmentNominalTypesMatch AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {binding}.typeIdx {carrier} {plan} = true := by\n  \
               rfl\n\n\
             -- Aggregate: combine the already-checked leaf constants. `dsimp` only\n\
             -- prepares the goal shape; the `exact` holds no heavy reduction.\n\
             theorem {accepted} :\n  \
               AverCert.AcceptedArtifact.symFragmentClaimAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen (symFragmentClaims.get ⟨{index}, by decide⟩) := by\n  \
               dsimp [symFragmentClaims, AverCert.AcceptedArtifact.symFragmentClaimAccepted, AverCert.AcceptedArtifact.symFragmentPlanAccepted, AverCert.AcceptedArtifact.exprFragmentPlanAccepted, AverCert.ExprFragmentAccepted.accepted]\n  \
               exact ⟨{carrier_bound}, {host_types}, rfl, rfl, ⟨{body}, {code_entry}, {binding}, ⟨⟨{check_plan}, {lower_body}, {lower_code}, {func_binding}⟩, {func_type}, {nominal}, rfl, rfl⟩⟩⟩\n\n"
        ));
        names.push(accepted);
    }
    let aggregate = names
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |rest, theorem| {
            format!("⟨{theorem}, {rest}⟩")
        });
    (theorems, aggregate)
}

/// Structured inputs for one fuel-recursion claim's split acceptance proof.
struct RecursionParts {
    name: String,
    lowered_body: String,
    code_entry_bytes: String,
    export_name_bytes: String,
    host_table: String,
    self_idx: u32,
    type_idx: u32,
    carrier: u32,
}

/// Split the fuel-recursion family exactly as `render_sym_claim_bundles` splits
/// the expr-fragment family: the lowered body, code-entry bytes and function
/// binding become named `def`s, each byte-walking conjunct of
/// `recursionPlanAccepted` becomes its own leaf theorem over
/// `AverCert.Plans.{name}RecursionPlan`, and the aggregate combines them.
fn render_recursion_claim_bundles(parts: &[RecursionParts]) -> (String, String) {
    render_split_bundles(
        "recursion",
        "AverCert.AcceptedArtifact.recursionClaimAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen AverCert.manifest",
        "recursionClaims",
        "AverCert.AcceptedArtifact.recursionClaimAccepted, AverCert.AcceptedArtifact.recursionPlanForExport, AverCert.AcceptedArtifact.recursionPlanAccepted",
        parts.len(),
        |index| {
            let RecursionParts {
                name,
                lowered_body,
                code_entry_bytes,
                export_name_bytes,
                host_table,
                self_idx,
                type_idx,
                carrier,
            } = &parts[index];
            let body = format!("recursionClaim{index}Body");
            let code_entry = format!("recursionClaim{index}CodeEntry");
            let binding = format!("recursionClaim{index}Binding");
            let check_plan = format!("recursionClaim{index}CheckPlan");
            let lower_body = format!("recursionClaim{index}LowerBody");
            let lower_code = format!("recursionClaim{index}LowerCode");
            let func_binding = format!("recursionClaim{index}FuncBinding");
            let check_shape = format!("recursionClaim{index}CheckShape");
            let func_type = format!("recursionClaim{index}FuncType");
            let host_types = format!("recursionClaim{index}HostTypes");
            let plan = format!("AverCert.Plans.{name}RecursionPlan");
            let obligation = format!("AverCert.{name}Ob");
            let declarations = format!(
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
                   rfl\n\n"
            );
            let aggregate_proof = format!(
                "⟨rfl, rfl, {check_plan}, rfl, ⟨{body}, {code_entry}, {binding}, ⟨{lower_body}, {lower_code}, {func_binding}, rfl, {check_shape}, {func_type}, {host_types}, rfl⟩⟩⟩"
            );
            (declarations, aggregate_proof)
        },
    )
}

fn render_artifact_expr_fragment_claims(
    analysis: &Analysis,
    model_info: &ModelInfo,
    host_table_lean: &str,
    struct_table_lean: &str,
) -> RenderedArtifactClaims {
    let mut sym_claims = Vec::new();
    let mut string_eq_claims = Vec::new();
    let mut string_claims = Vec::new();
    let mut construct_claims = Vec::new();
    let mut recursion_claims = Vec::new();
    let mut mutual_claims = Vec::new();
    let mut verbatim_claims = Vec::new();
    let mut int_dispatch_claims = Vec::new();
    let mut field_projection_claims = Vec::new();
    let mut composition_claims = Vec::new();
    let mut sym_parts: Vec<SymClaimParts> = Vec::new();
    let mut string_eq_proofs = Vec::new();
    let mut string_proofs = Vec::new();
    let mut construct_proofs = Vec::new();
    let mut recursion_parts: Vec<RecursionParts> = Vec::new();
    let mut mutual_proofs = Vec::new();
    let mut verbatim_proofs = Vec::new();
    let mut int_dispatch_proofs = Vec::new();
    let mut field_projection_proofs = Vec::new();
    let mut composition_proofs = Vec::new();
    let mut string_concat_faces: Vec<Option<(String, String)>> = Vec::new();
    let mut construct_faces: Vec<Option<(String, String)>> = Vec::new();
    let mut int_dispatch_faces: Vec<Option<(String, String)>> = Vec::new();
    for c in &analysis.certs {
        match c.inner() {
            Cert::ExprFragment {
                name,
                carrier,
                self_idx,
                type_idx,
                source_plan,
                plan,
                ..
            } => {
                let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(plan, *carrier)
                    .expect("certified expr-fragment plan lowers to code-entry bytes");
                let code_entry_bytes = render_byte_list(&code_entry_bytes);
                let lowered_body = lower_expr_fragment_plan(plan, *carrier)
                    .map(|ops| render_ops_value(&ops))
                    .expect("certified expr-fragment plan lowers to WInstr body");
                let export_name_bytes = render_byte_list(name.as_bytes());
                if expr_fragment_source_plan(source_plan, plan).is_some() {
                    sym_claims.push(format!(
                        "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, hostTable := {host_table_lean}, structTable := {struct_table_lean}, plan := AverCert.Plans.{name}SymPlan, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.SymFragmentClaim)",
                        export_name = lean_str(name),
                    ));
                    // Structured inputs for this claim's SPLIT acceptance proof:
                    // the witness data and every conjunct become their own
                    // top-level declarations (see `render_sym_claim_bundles`), so
                    // the kernel checks and frees each in a separate `addDecl`
                    // instead of holding one monolithic witness tuple live.
                    sym_parts.push(SymClaimParts {
                        name: name.clone(),
                        lowered_body,
                        code_entry_bytes,
                        export_name_bytes,
                        self_idx: *self_idx,
                        type_idx: *type_idx,
                        carrier: *carrier,
                    });
                }
            }
            Cert::StringConcatVerbatimMatch {
                name,
                self_idx,
                type_idx,
                carrier,
                string_concat_idx,
                container_ty,
                result_ty,
                ..
            } => {
                let plan = string_concat_plan_from_cert(c)
                    .expect("certified String.concat should project to a source plan");
                let code_entry_bytes = lower_string_concat_plan_code_entry_bytes(
                    &plan,
                    *carrier,
                    *result_ty,
                    *container_ty,
                    *string_concat_idx,
                )
                .expect("certified String.concat plan lowers to code-entry bytes");
                let code_entry_bytes = render_byte_list(&code_entry_bytes);
                let lowered_body = lower_string_concat_plan(
                    &plan,
                    *result_ty,
                    *container_ty,
                    *string_concat_idx,
                )
                .map(|ops| render_ops_value(&ops))
                .expect("certified String.concat plan lowers to WInstr body");
                let export_name_bytes = render_byte_list(name.as_bytes());
                let func_binding = format!(
                    "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
                );
                let carrier_state = render_carrier_state(*carrier);
                // The obligation declares what `decodedCarrierIndex` forces:
                // the real carrier index, or the reserved `0` in a module that
                // provably has no Int carrier struct.
                let obligation_carrier = carrier.unwrap_or(0);
                string_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier_state}, resultTy := {result_ty}, containerTy := {container_ty}, concatFuncIdx := {string_concat_idx}, symPlan := AverCert.Plans.{name}StringConcatSymPlan, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.StringConcatClaim)",
                    export_name = lean_str(name),
                ));
                // Five leading `rfl`s: export name, the carrier-state decode,
                // the obligation's carrier index, the concat host role, and the
                // canonical host wiring.
                string_proofs.push(format!(
                    "⟨rfl, rfl, rfl, rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
                     ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
                ));
                string_concat_faces.push(Some((
                    format!("AverCert.Plans.{name}StringConcatPlan"),
                    format!(
                        "⟨rfl, rfl, ([] : List Nat), (⟨0, {obligation_carrier}, []⟩ : AverCert.DeclaredIndexEnvelope.DIdxEnvelope), \
                         ⟨AverCert.Plans.declaredTypeCur, rfl, Nat.zero_le _, rfl⟩, rfl, rfl, \
                         HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩"
                    ),
                )));
            }
            Cert::StringEqVerbatimMatch {
                name,
                self_idx,
                type_idx,
                carrier,
                string_eq_idx,
                ..
            } => {
                let plan = string_eq_plan_from_cert(c)
                    .expect("certified String.eq should project to a source plan");
                let string_ty = string_eq_string_ty_from_cert(c)
                    .expect("certified String.eq should use string arrays");
                let code_entry_bytes =
                    lower_string_eq_plan_code_entry_bytes(&plan, *carrier, string_ty, *string_eq_idx)
                        .expect("certified String.eq plan lowers to code-entry bytes");
                let code_entry_bytes = render_byte_list(&code_entry_bytes);
                let lowered_body = lower_string_eq_plan(&plan, string_ty, *string_eq_idx)
                    .map(|ops| render_ops_value(&ops))
                    .expect("certified String.eq plan lowers to WInstr body");
                let export_name_bytes = render_byte_list(name.as_bytes());
                let func_binding = format!(
                    "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
                );
                string_eq_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, stringTy := {string_ty}, stringEqFuncIdx := {string_eq_idx}, symPlan := AverCert.Plans.{name}StringEqSymPlan, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.StringEqClaim)",
                    export_name = lean_str(name),
                ));
                string_eq_proofs.push(format!(
                    "⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
                     ⟨rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
                ));
            }
            Cert::AdtConstructor {
                name,
                self_idx,
                type_idx,
                carrier,
                struct_idx,
                field_count,
                elem_ty,
                ..
            } => {
                let Some(sym_plan) = adt_constructor_sym_plan_from_cert(c, model_info) else {
                    continue;
                };
                let Some(plan) = construct_plan_from_cert(c) else {
                    continue;
                };
                let code_entry_bytes =
                    lower_construct_plan_code_entry_bytes(&plan, *carrier, *struct_idx)
                    .expect("certified constructor plan lowers to code-entry bytes");
                let code_entry_bytes = render_byte_list(&code_entry_bytes);
                let lowered_body = lower_construct_plan(&plan, *struct_idx)
                    .map(|ops| render_ops_value(&ops))
                    .expect("certified constructor plan lowers to WInstr body");
                let export_name_bytes = render_byte_list(name.as_bytes());
                let elem_ty = construct_val_type_lean_value(*elem_ty)
                    .expect("certified constructor has a supported byte-level element type");
                let func_binding = format!(
                    "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
                );
                let (struct_type_proof, func_type_proof) =
                    if sym_plan_is_list_construct(&sym_plan) {
                        (
                            format!("Or.inr AverCert.Plans.{name}ConstructStructTypeMatches"),
                            format!("Or.inr AverCert.Plans.{name}ConstructFuncTypeMatches"),
                        )
                    } else {
                        ("Or.inl rfl".to_string(), "Or.inl rfl".to_string())
                    };
                construct_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, structIdx := {struct_idx}, fieldCount := {field_count}, elemTy := {elem_ty}, symPlan := AverCert.Plans.{name}ConstructSymPlan, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.ConstructClaim)",
                    export_name = lean_str(name),
                ));
                construct_proofs.push(format!(
                    "⟨rfl, rfl, rfl, rfl, rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
                     ⟨rfl, rfl, rfl, rfl, {struct_type_proof}, \
                     {func_type_proof}, rfl⟩⟩⟩"
                ));
                if adt_constructor_uses_model(c, model_info) {
                    construct_faces.push(Some((
                        format!("AverCert.Plans.{name}ConstructPlan"),
                        "⟨rfl, rfl, rfl, AverCert.Plans.{name}TypePrefix, AverCert.Plans.{name}DeclaredEnvelope, \
                         by decide, by decide, rfl, rfl, \
                         ⟨AverCert.Plans.declaredTypeCur, rfl, by decide +kernel⟩, \
                         rfl, rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩"
                            .replace("{name}", name),
                    )));
                } else {
                    construct_faces.push(None);
                }
            }
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
            } => {
                // Analysis declines a normalized body whose canonical plan
                // cannot reproduce its exact bytes. Keep this guard as a
                // fail-closed invariant.
                let Some(plan) = recursion_plan_from_cert(c) else {
                    continue;
                };
                let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier)
                    .expect("certified recursion plan lowers to code-entry bytes");
                let code_entry_bytes = render_byte_list(&code_entry_bytes);
                let lowered_body = lower_expr_fragment_plan(&plan, *carrier)
                    .map(|ops| render_ops_value(&ops))
                    .expect("certified recursion plan lowers to WInstr body");
                let export_name_bytes = render_byte_list(name.as_bytes());
                let host_table = recursion_host_table_lean_value(c);
                recursion_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, hostTable := {host_table}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.RecursionClaim)",
                    export_name = lean_str(name),
                ));
                // Split acceptance proof: the witness data and every byte-walking
                // conjunct become their own top-level declarations (see
                // `render_recursion_claim_bundles`) so the kernel checks and frees
                // each separately instead of holding one monolithic witness tuple.
                recursion_parts.push(RecursionParts {
                    name: name.clone(),
                    lowered_body,
                    code_entry_bytes,
                    export_name_bytes,
                    host_table,
                    self_idx: *self_idx,
                    type_idx: *type_idx,
                    carrier: *carrier,
                });
            }
            Cert::MutualRecursion {
                name,
                self_idx,
                carrier,
                box_idx,
                sub_idx,
                position,
                scc,
            } => {
                // Analysis declines any member whose canonical plan cannot
                // reproduce its exact bytes. Keep this guard as a fail-closed
                // invariant.
                let Some(plan) = mutual_plan_from_cert(c) else {
                    continue;
                };
                let member = &scc[*position];
                let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier)
                    .expect("certified mutual member plan lowers to code-entry bytes");
                let code_entry_bytes = render_byte_list(&code_entry_bytes);
                let lowered_body = lower_expr_fragment_plan(&plan, *carrier)
                    .map(|ops| render_ops_value(&ops))
                    .expect("certified mutual member plan lowers to WInstr body");
                let export_name_bytes = render_byte_list(name.as_bytes());
                let host_table = mutual_host_table_lean_value(*box_idx, *sub_idx);
                let member_set = mutual_member_set_lean_value(scc);
                let func_binding = format!(
                    "({{ funcIdx := {self_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)",
                    type_idx = member.type_idx,
                );
                mutual_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, memberSet := {member_set}, hostTable := {host_table}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.MutualRecursionClaim)",
                    export_name = lean_str(name),
                ));
                mutual_proofs.push(format!(
                    "⟨rfl, rfl, rfl, rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
                     ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
                ));
            }
            Cert::VerbatimWidenedMatch {
                name, carrier, ..
            }
            | Cert::VerbatimVariantDispatch {
                name, carrier, ..
            } => {
                // Analysis declines a body whose canonical verbatim plan cannot
                // reproduce its exact bytes. Keep this guard as a fail-closed
                // invariant.
                if verbatim_plan_from_cert(c).is_none() {
                    continue;
                }
                let export_name_bytes = render_byte_list(name.as_bytes());
                verbatim_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.VerbatimClaim)",
                    export_name = lean_str(name),
                ));
                // The code entry, signature and payload conjuncts are the binding
                // (no host/self calls), so the witness is anonymous for the code
                // entry and binding; the final `rfl` pins the canonical locals
                // count (the two preceding `rfl`s discharge the byte-derived
                // signature and payload binds).
                verbatim_proofs.push(
                    "⟨rfl, rfl, rfl, ⟨_, _, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩"
                        .to_string(),
                );
            }
            Cert::VariantDispatch { name, carrier, .. }
            | Cert::WidenedIntMatch { name, carrier, .. } => {
                // Analysis declines a body whose canonical Int-face plan cannot
                // reproduce its exact bytes. Keep this guard as a fail-closed
                // invariant.
                if int_dispatch_plan_from_cert(c, analysis.frag_host_table).is_none() {
                    continue;
                }
                let hosts = int_dispatch_host_table_from_cert(c)
                    .expect("Int-face dispatch cert carries its host table");
                let host_table = int_dispatch_host_table_lean_value(&hosts);
                let export_name_bytes = render_byte_list(name.as_bytes());
                int_dispatch_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, hostTable := {host_table}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.IntDispatchClaim)",
                    export_name = lean_str(name),
                ));
                // The code-entry and signature conjuncts plus the role-table
                // parameterization are the binding, so the witness is anonymous
                // for the body, code entry and binding — each pinned by `rfl`
                // (the three extra leading `rfl`s discharge the host-table
                // distinctness, the host-table declared-function-type pin, and
                // the obligation-wiring bind; the final `rfl` pins the code
                // table with the CANONICAL locals count, no existential).
                int_dispatch_proofs.push(
                    "⟨rfl, rfl, rfl, rfl, rfl, rfl, ⟨_, _, _, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩"
                        .to_string(),
                );
                int_dispatch_faces.push(Some((
                    format!("AverCert.Plans.{name}IntDispatchPlan"),
                    "⟨rfl, rfl, AverCert.Plans.{name}TypePrefix, AverCert.Plans.{name}DeclaredEnvelope, \
                     by decide, by decide, \
                     ⟨AverCert.Plans.declaredTypeCur, rfl, by decide +kernel⟩, \
                     rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩"
                        .replace("{name}", name),
                )));
            }
            Cert::FieldProjection {
                name,
                carrier,
                struct_idx,
                field_count,
                ..
            } => {
                let Some((_plan, result_ty)) = field_projection_plan_from_cert(c) else {
                    continue;
                };
                field_projection_claims.push(format!(
                    "({{ exportNameBytes := {bytes}, exportName := {export_name}, carrier := {carrier}, structIdx := {struct_idx}, fieldCount := {field_count}, resultTy := {result_ty}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.FieldProjectionClaim)",
                    bytes = render_byte_list(name.as_bytes()),
                    export_name = lean_str(name),
                    result_ty = field_projection_result_ty_lean_value(result_ty),
                ));
                field_projection_proofs.push(
                    "⟨rfl, rfl, rfl, ⟨_, _, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩"
                        .to_string(),
                );
            }
            _ => {}
        }
    }
    let composition_members_data = composition_member_plans(analysis);
    let composition_members = composition_members_data
        .iter()
        .map(|(entry, _)| {
            format!(
                "({{ exportNameBytes := {bytes}, exportName := {name}, plan := AverCert.Plans.{ident}CompositionPlan }} : AverCert.AcceptedArtifact.CompositionMemberClaim)",
                bytes = render_byte_list(entry.name.as_bytes()),
                name = lean_str(&entry.name),
                ident = entry.name,
            )
        })
        .collect::<Vec<_>>();
    let global_func_table = composition_members_data
        .iter()
        .map(|(entry, _)| (entry.name.clone(), entry.self_idx))
        .collect::<std::collections::HashMap<_, _>>();
    for cert in &analysis.certs {
        let Cert::Composition {
            name,
            self_idx,
            carrier,
            closure,
            ..
        } = cert.inner()
        else {
            continue;
        };
        let Some(plans) = composition_plans_from_cert(cert, analysis.frag_host_table) else {
            continue;
        };
        let add_idx = analysis
            .frag_host_table
            .add_idx
            .expect("plan-backed composition has strict add host");
        let host_table = composition_host_table_lean_value(add_idx);
        let member_names = format!(
            "[{}]",
            closure
                .iter()
                .map(|entry| lean_str(&entry.name))
                .collect::<Vec<_>>()
                .join(", ")
        );
        composition_claims.push(format!(
            "({{ exportName := {export_name}, carrier := {carrier}, hostTable := {host_table}, memberNames := {member_names}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.CompositionClaim)",
            export_name = lean_str(name),
        ));
        let mut named_proof = "trivial".to_string();
        for (entry, plan) in plans.iter().rev() {
            let body = render_ops_value(
                &lower_composition_plan(plan, add_idx, &global_func_table)
                    .expect("composition member lowers against global byte table"),
            );
            let code_entry = render_byte_list(
                &composition_code_entry_bytes(plan, *carrier, add_idx, &global_func_table)
                    .expect("composition member byte-lowers against global byte table"),
            );
            let binding = format!(
                "({{ funcIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
                entry.self_idx, entry.type_idx, code_entry
            );
            let member_proof = format!(
                "⟨rfl, ⟨({body}), ({code_entry}), {binding}, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩"
            );
            named_proof = format!("⟨{member_proof}, {named_proof}⟩");
        }
        let _ = self_idx;
        composition_proofs.push(format!(
            "⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, {named_proof}⟩⟩⟩⟩⟩⟩⟩"
        ));
    }
    let sym_claims = if sym_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", sym_claims.join(",\n  "))
    };
    let string_claims = if string_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", string_claims.join(",\n  "))
    };
    let string_eq_claims = if string_eq_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", string_eq_claims.join(",\n  "))
    };
    let construct_claims = if construct_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", construct_claims.join(",\n  "))
    };
    let recursion_claims = if recursion_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", recursion_claims.join(",\n  "))
    };
    let mutual_claims = if mutual_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", mutual_claims.join(",\n  "))
    };
    let verbatim_claims = if verbatim_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", verbatim_claims.join(",\n  "))
    };
    let int_dispatch_claims = if int_dispatch_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", int_dispatch_claims.join(",\n  "))
    };
    let field_projection_claims = if field_projection_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", field_projection_claims.join(",\n  "))
    };
    let composition_members = if composition_members.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", composition_members.join(",\n  "))
    };
    let composition_claims = if composition_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", composition_claims.join(",\n  "))
    };
    let obligation_proof_count = sym_parts.len()
        + string_eq_proofs.len()
        + string_proofs.len()
        + construct_proofs.len()
        + recursion_parts.len()
        + mutual_proofs.len()
        + verbatim_proofs.len()
        + int_dispatch_proofs.len()
        + field_projection_proofs.len()
        + composition_proofs.len();
    let obligation_proof = (0..obligation_proof_count).fold("trivial".to_string(), |acc, _| {
        format!("⟨rfl, {acc}⟩")
    });
    // The expr-fragment (`sym`) family emits a SPLIT proof: witness data as
    // named `def`s and each acceptance conjunct as its own leaf theorem, with
    // the aggregate combining the already-checked constants. This keeps the
    // per-claim kernel peak at the largest single leaf instead of the whole
    // witness tuple. The other nine families still emit one opaque theorem.
    let (sym_bundles, sym_proof) = render_sym_claim_bundles(&sym_parts, host_table_lean);
    let (string_bundles, string_proof) = render_per_claim_bundles(
        "stringConcat",
        "AverCert.AcceptedArtifact.stringConcatClaimAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen AverCert.manifest",
        "stringConcatClaims",
        "AverCert.AcceptedArtifact.stringConcatClaimAccepted, AverCert.AcceptedArtifact.stringConcatPlanForExport, AverCert.AcceptedArtifact.stringConcatPlanAccepted, AverCert.AcceptedArtifact.stringConcatCanonicalHost",
        &string_proofs,
    );
    let (string_eq_bundles, string_eq_proof) = render_per_claim_bundles(
        "stringEq",
        "AverCert.AcceptedArtifact.stringEqClaimAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen AverCert.manifest",
        "stringEqClaims",
        "AverCert.AcceptedArtifact.stringEqClaimAccepted, AverCert.AcceptedArtifact.stringEqPlanForExport, AverCert.AcceptedArtifact.stringEqPlanAccepted, AverCert.AcceptedArtifact.stringEqCanonicalHost",
        &string_eq_proofs,
    );
    let construct_claim_count = construct_proofs.len();
    let (construct_bundles, construct_claims_proof) = render_per_claim_bundles(
        "construct",
        "AverCert.AcceptedArtifact.constructClaimAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen AverCert.manifest",
        "constructClaims",
        "AverCert.AcceptedArtifact.constructClaimAccepted, AverCert.AcceptedArtifact.constructPlanForExport, AverCert.AcceptedArtifact.constructPlanAccepted, AverCert.AcceptedArtifact.exprFragmentPlanAccepted, AverCert.ExprFragmentAccepted.accepted",
        &construct_proofs,
    );
    // `acceptedConstructFragments` conjoins per-claim acceptance with unique
    // export-name coverage. Build the `Nodup` proof one list cell at a time so
    // Lean never runs a whole-list decision procedure inside the already deep
    // artifact acceptance term.
    let construct_nodup_proof = (0..construct_claim_count).fold(
        "List.nodup_nil".to_string(),
        |acc, _| format!("List.nodup_cons.mpr ⟨by decide, {acc}⟩"),
    );
    let construct_proof = format!("⟨{construct_claims_proof}, {construct_nodup_proof}⟩");
    let (recursion_bundles, recursion_proof) = render_recursion_claim_bundles(&recursion_parts);
    let (mutual_bundles, mutual_proof) = render_per_claim_bundles(
        "mutual",
        "AverCert.AcceptedArtifact.mutualRecursionClaimAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen AverCert.manifest",
        "mutualRecursionClaims",
        "AverCert.AcceptedArtifact.mutualRecursionClaimAccepted, AverCert.AcceptedArtifact.mutualPlanForExport, AverCert.AcceptedArtifact.mutualPlanAccepted",
        &mutual_proofs,
    );
    let (verbatim_bundles, verbatim_proof) = render_per_claim_bundles(
        "verbatim",
        "AverCert.AcceptedArtifact.verbatimClaimAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen AverCert.manifest",
        "verbatimClaims",
        "AverCert.AcceptedArtifact.verbatimClaimAccepted, AverCert.AcceptedArtifact.verbatimPlanForExport, AverCert.AcceptedArtifact.verbatimPlanAccepted",
        &verbatim_proofs,
    );
    let (int_dispatch_bundles, int_dispatch_proof) = render_per_claim_bundles(
        "intDispatch",
        "AverCert.AcceptedArtifact.intDispatchClaimAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen AverCert.manifest",
        "intDispatchClaims",
        "AverCert.AcceptedArtifact.intDispatchClaimAccepted, AverCert.AcceptedArtifact.intDispatchPlanForExport, AverCert.AcceptedArtifact.intDispatchPlanAccepted, AverCert.AcceptedArtifact.intDispatchCanonicalHost, AverCert.AcceptedArtifact.intDispatchCanonicalSlots",
        &int_dispatch_proofs,
    );
    let (field_projection_bundles, field_projection_proof) = render_per_claim_bundles(
        "fieldProjection",
        "AverCert.AcceptedArtifact.fieldProjectionClaimAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen AverCert.manifest",
        "fieldProjectionClaims",
        "AverCert.AcceptedArtifact.fieldProjectionClaimAccepted, AverCert.AcceptedArtifact.fieldProjectionPlanForExport, AverCert.AcceptedArtifact.fieldProjectionPlanAccepted",
        &field_projection_proofs,
    );
    let (composition_bundles, composition_claims_proof) = render_per_claim_bundles(
        "composition",
        "AverCert.AcceptedArtifact.compositionClaimAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen compositionMembers",
        "compositionClaims",
        "AverCert.AcceptedArtifact.compositionClaimAccepted, AverCert.AcceptedArtifact.compositionFuncTable, AverCert.AcceptedArtifact.compositionMemberBinding, AverCert.AcceptedArtifact.compositionNamedMembersAccepted, AverCert.AcceptedArtifact.compositionMemberPlanAccepted, AverCert.AcceptedArtifact.compositionMemberForName, AverCert.AcceptedArtifact.compositionClosureBound, AverCert.AcceptedArtifact.compositionEdges, AverCert.AcceptedArtifact.compositionPlanCallees, AverCert.AcceptedArtifact.compositionEdgesDescend, AverCert.AcceptedArtifact.compositionReachClosure, AverCert.AcceptedArtifact.compositionReachStep, AverCert.AcceptedArtifact.stringListNodup, AverCert.AcceptedArtifact.stringListSetEq",
        &composition_proofs,
    );
    // `acceptedCompositionFragments` conjoins the per-claim acceptance with the
    // artifact-wide member coverage, manifest-obligation coverage, and unique
    // obligation-export bounds. Each is a decidable `Bool = true` over concrete
    // artifact literals, closed by `rfl`.
    let composition_proof = format!("⟨{composition_claims_proof}, rfl, rfl, rfl⟩");
    let (string_concat_face_bundles, string_concat_face_proof) = render_face_bundles(
        "stringConcat",
        "stringConcatDeclaredFace",
        "stringConcatClaims",
        ", AverCert.AcceptedArtifact.stringConcatCanonicalHost",
        false,
        &string_concat_faces,
    );
    let (construct_face_bundles, construct_face_proof) = render_face_bundles(
        "construct",
        "constructNamedFace",
        "constructClaims",
        ", AverCert.StandardFace.emptyHost",
        false,
        &construct_faces,
    );
    let (int_dispatch_face_bundles, int_dispatch_face_proof) = render_face_bundles(
        "intDispatch",
        "intDispatchDeclaredFace",
        "intDispatchClaims",
        ", AverCert.AcceptedArtifact.intDispatchCanonicalHost, AverCert.AcceptedArtifact.intDispatchCanonicalSlots",
        true,
        &int_dispatch_faces,
    );
    let claim_proof_bundles = [
        sym_bundles,
        string_eq_bundles,
        string_bundles,
        construct_bundles,
        recursion_bundles,
        mutual_bundles,
        verbatim_bundles,
        int_dispatch_bundles,
        field_projection_bundles,
        composition_bundles,
        string_concat_face_bundles,
        construct_face_bundles,
        int_dispatch_face_bundles,
    ]
    .concat();
    RenderedArtifactClaims {
        sym_claims,
        string_eq_claims,
        string_claims,
        construct_claims,
        recursion_claims,
        mutual_claims,
        verbatim_claims,
        int_dispatch_claims,
        field_projection_claims,
        composition_members,
        composition_claims,
        claim_proof_bundles,
        obligation_proof,
        sym_proof,
        string_eq_proof,
        string_proof,
        construct_proof,
        recursion_proof,
        mutual_proof,
        verbatim_proof,
        int_dispatch_proof,
        field_projection_proof,
        composition_proof,
        string_concat_face_proof,
        construct_face_proof,
        int_dispatch_face_proof,
    }
}

fn render_artifact(
    analysis: &Analysis,
    model_info: &ModelInfo,
    host_table_lean: &str,
    struct_table_lean: &str,
) -> String {
    let claims = render_artifact_expr_fragment_claims(
        analysis,
        model_info,
        host_table_lean,
        struct_table_lean,
    );
    let nat_list = |values: &[u32]| {
        format!(
            "[{}]",
            values
                .iter()
                .map(u32::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    let closure_claim = format!(
        "({{ roots := {}, helpers := {}, admitted := {} }} : AverCert.AcceptedArtifact.ClosureClaim)",
        nat_list(&analysis.module_envelope.closure.roots),
        nat_list(&analysis.module_envelope.closure.helpers),
        nat_list(&analysis.module_envelope.closure.admitted),
    );
    // Shared source-side rendering for the few cross-family reductions. Each
    // proof family below gets its own opaque theorem instead of contributing
    // an inline branch to one enormous `acceptedWithFinal` proof term. The
    // expansion deliberately contains no Lean `macro`: Artifact.lean is data
    // from the verifier's perspective and must pass the elaboration-code wall.
    let artifact_dsimp = concat!(
            "  dsimp [data, closureClaim, symFragmentClaims, stringEqClaims, stringConcatClaims, constructClaims, recursionClaims, mutualRecursionClaims, verbatimClaims, intDispatchClaims, fieldProjectionClaims, compositionMembers, compositionClaims, AverCert.AcceptedArtifact.accepted,\n",
            "    AverCert.AcceptedArtifact.subjectMatchesArtifactRoot,\n",
            "    AverCert.AcceptedArtifact.expectedArtifactRoot,\n",
            "    AverCert.AcceptedArtifact.fragmentClaimObligationsInManifest,\n",
            "    AverCert.AcceptedArtifact.claimObligations,\n",
            "    AverCert.AcceptedArtifact.claimObligationsInManifest,\n",
            "    AverCert.AcceptedArtifact.claimObligationExports,\n",
            "    AverCert.AcceptedArtifact.claimsMatchManifest,\n",
            "    AverCert.AcceptedArtifact.decodedNonExprFacts,\n",
            "    AverCert.AcceptedArtifact.decodedNonExprClaimFacts,\n",
            "    AverCert.AcceptedArtifact.decodedStringHostRoles,\n",
            "    AverCert.AcceptedArtifact.stringEqCanonicalHost,\n",
            "    AverCert.AcceptedArtifact.stringConcatCanonicalHost,\n",
            "    AverCert.AcceptedArtifact.decodedClaims,\n",
            "    AverCert.AcceptedArtifact.decodedObligationFacts,\n",
            "    AverCert.AcceptedArtifact.decodedCodeAtAll,\n",
            "    AverCert.AcceptedArtifact.decodedCodeAt,\n",
            "    AverCert.AcceptedArtifact.decodedConstructStructFields,\n",
            "    AverCert.AcceptedArtifact.decodedProjectionStructFields,\n",
            "    AverCert.AcceptedArtifact.decodedCompositionClaims,\n",
            "    AverCert.AcceptedArtifact.decodedCompositionNames,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimPlanPairs,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimEncodedPlanPairs,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimEncodedPlanPair?,\n",
            "    AverCert.AcceptedArtifact.stringEqClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.stringEqManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.stringEqClaimSymPlanPairs,\n",
            "    AverCert.AcceptedArtifact.stringConcatClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.stringConcatManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.stringConcatClaimSymPlanPairs,\n",
            "    AverCert.AcceptedArtifact.constructClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.constructManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.constructClaimSymPlanPairs,\n",
            "    AverCert.AcceptedArtifact.recursionClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.recursionManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.mutualRecursionClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.mutualManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.verbatimClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.verbatimManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.intDispatchClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.intDispatchManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.compositionMemberPlanPairs,\n",
            "    AverCert.AcceptedArtifact.acceptedFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedSymFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedStringEqFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedStringConcatFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedConstructFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedRecursionFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedMutualRecursionFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedVerbatimFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedIntDispatchFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedFieldProjectionFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedCompositionFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedWholeModule,\n",
            "    AverCert.AcceptedArtifact.allClaims,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.symFragmentPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.stringEqClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.stringEqClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.stringEqPlanForExport,\n",
            "    AverCert.AcceptedArtifact.stringEqPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.stringConcatClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.stringConcatClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.stringConcatPlanForExport,\n",
            "    AverCert.AcceptedArtifact.stringConcatPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.constructClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.constructClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.constructPlanForExport,\n",
            "    AverCert.AcceptedArtifact.constructPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.recursionClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.recursionClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.recursionPlanForExport,\n",
            "    AverCert.AcceptedArtifact.recursionPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.mutualRecursionClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.mutualRecursionClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.mutualPlanForExport,\n",
            "    AverCert.AcceptedArtifact.mutualPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.verbatimClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.verbatimClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.verbatimPlanForExport,\n",
            "    AverCert.AcceptedArtifact.verbatimPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.intDispatchClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.intDispatchClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.intDispatchPlanForExport,\n",
            "    AverCert.AcceptedArtifact.intDispatchPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionPlanForExport,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.compositionClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.compositionClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.compositionFuncTable,\n",
            "    AverCert.AcceptedArtifact.compositionMemberBinding,\n",
            "    AverCert.AcceptedArtifact.compositionNamedMembersAccepted,\n",
            "    AverCert.AcceptedArtifact.compositionMemberPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.compositionMemberForName,\n",
            "    AverCert.AcceptedArtifact.compositionClosureBound,\n",
            "    AverCert.AcceptedArtifact.compositionEdges,\n",
            "    AverCert.AcceptedArtifact.compositionPlanCallees,\n",
            "    AverCert.AcceptedArtifact.compositionEdgesDescend,\n",
            "    AverCert.AcceptedArtifact.compositionReachClosure,\n",
            "    AverCert.AcceptedArtifact.compositionReachStep,\n",
            "    AverCert.AcceptedArtifact.stringListNodup,\n",
            "    AverCert.AcceptedArtifact.stringListSetEq,\n",
            "    AverCert.AcceptedArtifact.manifestObligationsClaimed,\n",
            "    AverCert.AcceptedArtifact.manifestObligationExportsUnique,\n",
            "    AverCert.AcceptedArtifact.intDispatchCanonicalHost,\n",
            "    AverCert.AcceptedArtifact.intDispatchCanonicalSlots,\n",
            "    AverCert.AcceptedArtifact.exprFragmentPlanAccepted,\n",
            "    AverCert.ExprFragmentAccepted.accepted]\n"
    );
    let face_dsimp = concat!(
        "  dsimp [AverCert.StandardFace.checkedFaces,\n",
        "    AverCert.Schema.Subject.hostRoles,\n",
        "    AverCert.StandardFace.claimExportsUnique,\n",
        "    AverCert.StandardFace.hostTableBound,\n",
        "    AverCert.StandardFace.decodedRoleIdx,\n",
        "    AverCert.StandardFace.symFragmentMatches,\n",
        "    AverCert.StandardFace.symFragmentFace,\n",
        "    AverCert.StandardFace.stringEqMatches,\n",
        "    AverCert.StandardFace.stringConcatMatches,\n",
        "    AverCert.StandardFace.constructMatches,\n",
        "    AverCert.StandardFace.recursionMatches,\n",
        "    AverCert.StandardFace.mutualMatches,\n",
        "    AverCert.StandardFace.verbatimMatches,\n",
        "    AverCert.StandardFace.intDispatchMatches,\n",
        "    AverCert.StandardFace.fieldProjectionMatches,\n",
        "    AverCert.StandardFace.compositionMatches,\n",
        "    AverCert.StandardFace.StandardFace.Matches,\n",
        "    AverCert.AcceptedArtifact.claimObligationExports,\n",
        "    AverCert.AcceptedArtifact.allClaims,\n",
        "    AverCert.AcceptedArtifact.namedPlanForExport,\n",
        "    AverCert.AcceptedArtifact.stringEqPlanForExport,\n",
        "    AverCert.AcceptedArtifact.stringConcatPlanForExport,\n",
        "    AverCert.AcceptedArtifact.constructPlanForExport,\n",
        "    AverCert.AcceptedArtifact.recursionPlanForExport,\n",
        "    AverCert.AcceptedArtifact.mutualPlanForExport,\n",
        "    AverCert.AcceptedArtifact.verbatimPlanForExport,\n",
        "    AverCert.AcceptedArtifact.intDispatchPlanForExport,\n",
        "    AverCert.AcceptedArtifact.fieldProjectionPlanForExport,\n",
        "    AverCert.AcceptedArtifact.compositionMemberForName,\n",
        "    data, symFragmentClaims, stringEqClaims, stringConcatClaims,\n",
        "    constructClaims, recursionClaims, mutualRecursionClaims,\n",
        "    verbatimClaims, intDispatchClaims, fieldProjectionClaims,\n",
        "    compositionMembers, compositionClaims]\n"
    );
    // `dsimp` closes a reduced `True` goal immediately.  Do not emit a
    // trailing `exact trivial` for an empty family: Lean correctly reports a
    // tactic after goal closure as an error.
    let family_exact = |proof: &str| {
        if proof == "trivial" {
            String::new()
        } else {
            format!("  exact {proof}\n")
        }
    };
    let proof_bundles = format!(
        concat!(
            "theorem claimObligationsBound : AverCert.AcceptedArtifact.fragmentClaimObligationsInManifest data := by\n",
            "{artifact_dsimp}",
            "{obligation_proof_step}\n",
            "theorem claimsMatchManifest : AverCert.AcceptedArtifact.claimsMatchManifest data := by\n",
            "{artifact_dsimp}",
            "  exact ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, rfl⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩\n\n",
            "theorem standardFacesChecked : AverCert.StandardFace.checkedFaces data := by\n",
            "{face_dsimp}",
            "  refine ⟨rfl, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩\n",
            "  · repeat' constructor\n",
            "  · repeat' constructor\n",
            "  · exact {string_concat_face_proof}\n",
            "  · exact {construct_face_proof}\n",
            "  · repeat' constructor\n",
            "  · repeat' constructor\n",
            "  · repeat' constructor\n",
            "  · exact {int_dispatch_face_proof}\n",
            "  · repeat' constructor\n",
            "  · repeat' constructor\n\n",
            "theorem claimAxesChecked : AverCert.ClaimAxes.checked data = true := rfl\n\n",
            "theorem decodedNonExprClaimFacts : AverCert.AcceptedArtifact.decodedNonExprClaimFacts data := by\n",
            "{artifact_dsimp}",
            "  repeat' constructor\n\n",
            "theorem decodedNonExprFacts : AverCert.AcceptedArtifact.decodedNonExprFacts data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.decodedNonExprFacts]\n",
            "  exact ⟨decodedHostRoles, decodedStringHostRoles, decodedNonExprClaimFacts⟩\n\n",
            "theorem symFragmentsAccepted : AverCert.AcceptedArtifact.acceptedSymFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedSymFragments, AverCert.AcceptedArtifact.symFragmentClaimsAccepted, AverCert.AcceptedArtifact.allClaims, data, symFragmentClaims]\n",
            "{sym_proof_step}\n",
            "theorem stringEqFragmentsAccepted : AverCert.AcceptedArtifact.acceptedStringEqFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedStringEqFragments, AverCert.AcceptedArtifact.stringEqClaimsAccepted, AverCert.AcceptedArtifact.allClaims, data, stringEqClaims]\n",
            "{string_eq_proof_step}\n",
            "theorem stringConcatFragmentsAccepted : AverCert.AcceptedArtifact.acceptedStringConcatFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedStringConcatFragments, AverCert.AcceptedArtifact.stringConcatClaimsAccepted, AverCert.AcceptedArtifact.allClaims, data, stringConcatClaims]\n",
            "{string_proof_step}\n",
            "theorem constructFragmentsAccepted : AverCert.AcceptedArtifact.acceptedConstructFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedConstructFragments, AverCert.AcceptedArtifact.constructClaimsAccepted, AverCert.AcceptedArtifact.allClaims, AverCert.AcceptedArtifact.constructClaimExportNames, data, constructClaims]\n",
            "  exact {construct_proof}\n\n",
            "theorem recursionFragmentsAccepted : AverCert.AcceptedArtifact.acceptedRecursionFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedRecursionFragments, AverCert.AcceptedArtifact.recursionClaimsAccepted, AverCert.AcceptedArtifact.allClaims, data, recursionClaims]\n",
            "{recursion_proof_step}\n",
            "theorem mutualFragmentsAccepted : AverCert.AcceptedArtifact.acceptedMutualRecursionFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedMutualRecursionFragments, AverCert.AcceptedArtifact.mutualRecursionClaimsAccepted, AverCert.AcceptedArtifact.allClaims, AverCert.AcceptedArtifact.mutualClaimsFormClosedSccs, AverCert.AcceptedArtifact.mutualClaimEdges, AverCert.AcceptedArtifact.mutualClaimEdge, AverCert.AcceptedArtifact.mutualPlanForExport, AverCert.AcceptedArtifact.mutualPlanTarget, AverCert.AcceptedArtifact.mutualMembersFormClosedSccs, AverCert.AcceptedArtifact.followSccCycle, AverCert.AcceptedArtifact.natEdgeLookup, AverCert.AcceptedArtifact.natListNodup, AverCert.AcceptedArtifact.natListSetEq, data, mutualRecursionClaims]\n",
            "  exact ⟨{mutual_proof}, rfl⟩\n\n",
            "theorem verbatimFragmentsAccepted : AverCert.AcceptedArtifact.acceptedVerbatimFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedVerbatimFragments, AverCert.AcceptedArtifact.verbatimClaimsAccepted, AverCert.AcceptedArtifact.allClaims, data, verbatimClaims]\n",
            "{verbatim_proof_step}\n",
            "theorem intDispatchFragmentsAccepted : AverCert.AcceptedArtifact.acceptedIntDispatchFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedIntDispatchFragments, AverCert.AcceptedArtifact.intDispatchClaimsAccepted, AverCert.AcceptedArtifact.allClaims, data, intDispatchClaims]\n",
            "{int_dispatch_proof_step}\n",
            "theorem fieldProjectionFragmentsAccepted : AverCert.AcceptedArtifact.acceptedFieldProjectionFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedFieldProjectionFragments, AverCert.AcceptedArtifact.fieldProjectionClaimsAccepted, AverCert.AcceptedArtifact.allClaims, data, fieldProjectionClaims]\n",
            "{field_projection_proof_step}\n",
            "theorem compositionFragmentsAccepted : AverCert.AcceptedArtifact.acceptedCompositionFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedCompositionFragments, AverCert.AcceptedArtifact.compositionClaimsAccepted, AverCert.AcceptedArtifact.allClaims, AverCert.AcceptedArtifact.compositionMembersCovered, AverCert.AcceptedArtifact.compositionClaimedNames, AverCert.AcceptedArtifact.manifestObligationsClaimed, AverCert.AcceptedArtifact.claimObligationExports, AverCert.AcceptedArtifact.manifestObligationExportsUnique, AverCert.AcceptedArtifact.stringListNodup, data, compositionMembers, compositionClaims]\n",
            "  exact {composition_proof}\n\n",
            "theorem wholeModuleAccepted : AverCert.AcceptedArtifact.acceptedWholeModule data := by\n",
            "{artifact_dsimp}",
            "  exact ⟨rfl, rfl, rfl, rfl, rfl⟩\n\n",
            "theorem fragmentsAccepted : AverCert.AcceptedArtifact.acceptedFragments data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.acceptedFragments]\n",
            "  exact ⟨symFragmentsAccepted, stringEqFragmentsAccepted, stringConcatFragmentsAccepted, constructFragmentsAccepted, recursionFragmentsAccepted, mutualFragmentsAccepted, verbatimFragmentsAccepted, intDispatchFragmentsAccepted, fieldProjectionFragmentsAccepted, compositionFragmentsAccepted, wholeModuleAccepted⟩\n\n",
            "theorem acceptedWithFinal\n",
            "    (finalCert : AverCert.Schema.Holds AverCert.manifest) :\n",
            "    AverCert.AcceptedArtifact.accepted data := by\n",
            "  dsimp [AverCert.AcceptedArtifact.accepted, AverCert.AcceptedArtifact.subjectMatchesArtifactRoot, AverCert.AcceptedArtifact.expectedArtifactRoot]\n",
            "  exact ⟨finalCert, rfl, claimObligationsBound, claimsMatchManifest, standardFacesChecked, claimAxesChecked, decodedNonExprFacts, fragmentsAccepted⟩\n"
        ),
        obligation_proof_step = family_exact(&claims.obligation_proof),
        sym_proof_step = family_exact(&claims.sym_proof),
        string_eq_proof_step = family_exact(&claims.string_eq_proof),
        string_proof_step = family_exact(&claims.string_proof),
        construct_proof = claims.construct_proof,
        recursion_proof_step = family_exact(&claims.recursion_proof),
        mutual_proof = claims.mutual_proof,
        verbatim_proof_step = family_exact(&claims.verbatim_proof),
        int_dispatch_proof_step = family_exact(&claims.int_dispatch_proof),
        field_projection_proof_step = family_exact(&claims.field_projection_proof),
        composition_proof = claims.composition_proof,
        artifact_dsimp = artifact_dsimp,
        face_dsimp = face_dsimp,
        string_concat_face_proof = claims.string_concat_face_proof,
        construct_face_proof = claims.construct_face_proof,
        int_dispatch_face_proof = claims.int_dispatch_face_proof,
    );
    format!(
         "-- Artifact-carried acceptance root.\n\
         -- This file is useful metadata, not verifier authority: `aver cert verify`\n\
         -- pins its bytes and manifest to checker-owned inputs and\n\
         -- audits `AverCert.Artifact.certificate` through the Lean axiom collector.\n\
         import AcceptedArtifact\n\
         import ArtifactBytes\n\
         import Certificate\n\
         import Manifest\n\
         import Plans\n\
         import AcceptanceSoundness\n\n\
         -- The whole-module big-Nat closure fold is kernel reduction. This\n\
         -- explicit depth budget affects kernel reduction limits only, not soundness or axioms.\n\
         set_option maxRecDepth 200000\n\
         -- Companion budget for the elaborator, matching the one Certificate.lean\n\
         -- already carries. Precautionary rather than a fix for an observed\n\
         -- failure: this proof is one fixed shape repeated per claim, so its\n\
         -- elaboration cost grows with the size of the artifact, and the stock\n\
         -- allowance is the only thing here that was never stated explicitly.\n\
         -- Like the depth budget above it moves a resource limit only: no axiom,\n\
         -- no trusted hypothesis, no change to what the kernel must accept.\n\
         set_option maxHeartbeats 1600000\n\
         set_option linter.unusedSimpArgs false\n\n\
         namespace AverCert.Artifact\n\n\
         def symFragmentClaims : List AverCert.AcceptedArtifact.SymFragmentClaim := {sym_claims_list}\n\n\
         def stringEqClaims : List AverCert.AcceptedArtifact.StringEqClaim := {string_eq_claims_list}\n\n\
         def stringConcatClaims : List AverCert.AcceptedArtifact.StringConcatClaim := {string_claims_list}\n\n\
         def constructClaims : List AverCert.AcceptedArtifact.ConstructClaim := {construct_claims_list}\n\n\
         def recursionClaims : List AverCert.AcceptedArtifact.RecursionClaim := {recursion_claims_list}\n\n\
         def mutualRecursionClaims : List AverCert.AcceptedArtifact.MutualRecursionClaim := {mutual_claims_list}\n\n\
         def verbatimClaims : List AverCert.AcceptedArtifact.VerbatimClaim := {verbatim_claims_list}\n\n\
         def intDispatchClaims : List AverCert.AcceptedArtifact.IntDispatchClaim := {int_dispatch_claims_list}\n\n\
         def fieldProjectionClaims : List AverCert.AcceptedArtifact.FieldProjectionClaim := {field_projection_claims_list}\n\n\
         def compositionMembers : List AverCert.AcceptedArtifact.CompositionMemberClaim := {composition_members_list}\n\n\
         def compositionClaims : List AverCert.AcceptedArtifact.CompositionClaim := {composition_claims_list}\n\n\
         def closureClaim : AverCert.AcceptedArtifact.ClosureClaim := {closure_claim}\n\n\
         {mutual_scc_closure_pins}\
         def data : AverCert.AcceptedArtifact.ArtifactData :=\n  \
           ({{ modBytes := AverCert.ArtifactBytes.modBytes, modLen := AverCert.ArtifactBytes.modLen, manifest := AverCert.manifest, symFragmentClaims := symFragmentClaims, stringEqClaims := stringEqClaims, stringConcatClaims := stringConcatClaims, constructClaims := constructClaims, recursionClaims := recursionClaims, mutualRecursionClaims := mutualRecursionClaims, verbatimClaims := verbatimClaims, intDispatchClaims := intDispatchClaims, fieldProjectionClaims := fieldProjectionClaims, compositionMembers := compositionMembers, compositionClaims := compositionClaims, closureFuel := {closure_fuel}, closureClaim := closureClaim }} : AverCert.AcceptedArtifact.ArtifactData)\n\n\
         theorem decodedHostRoles : AverCert.AcceptedArtifact.decodedHostRoleTable data := by dsimp [AverCert.AcceptedArtifact.decodedHostRoleTable, data]; decide +kernel\n\n\
         theorem decodedStringHostRoles : CertDecode.StringHost.roleTable AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen = some AverCert.manifest.subject.stringHostRoles := by change AverCert.AcceptedArtifact.decodedStringHostRoles data; dsimp [AverCert.AcceptedArtifact.decodedStringHostRoles, data]; rfl\n\n\
         {claim_proof_bundles}\
         {proof_bundles}\n\n\
         {side_conditions}\n\
         end AverCert.Artifact\n",
        sym_claims_list = claims.sym_claims,
        string_eq_claims_list = claims.string_eq_claims,
        string_claims_list = claims.string_claims,
        construct_claims_list = claims.construct_claims,
        recursion_claims_list = claims.recursion_claims,
        mutual_claims_list = claims.mutual_claims,
        verbatim_claims_list = claims.verbatim_claims,
        int_dispatch_claims_list = claims.int_dispatch_claims,
        field_projection_claims_list = claims.field_projection_claims,
        composition_members_list = claims.composition_members,
        composition_claims_list = claims.composition_claims,
        closure_claim = closure_claim,
        closure_fuel = analysis.module_envelope.closure_fuel,
        mutual_scc_closure_pins = render_mutual_scc_closure_pins(analysis),
        claim_proof_bundles = claims.claim_proof_bundles,
        proof_bundles = proof_bundles,
        side_conditions = render_discharge_side_conditions(analysis, model_info)
    )
}

/// Final acceptance wrapper kept separate from artifact data so `Final.lean`
/// can consume the byte/plan acceptance facts without an import cycle.
fn render_artifact_certificate() -> String {
    r#"import Artifact
import Final

namespace AverCert.Artifact

theorem certificate : AverCert.AcceptedArtifact.accepted data :=
  acceptedWithFinal AverCert.Final.cert

#print axioms AverCert.Artifact.certificate

end AverCert.Artifact
"#
    .to_string()
}

/// Per-artifact glue from the hash-parametric audited wall to the real schema.
/// This module deliberately remains outside the sha-pinned wall because its
/// conclusion mentions the generated `CertModule.wasmSha256` through
/// `AverCert.Schema.Holds`.
fn render_artifact_soundness() -> String {
    r#"import Artifact
import AcceptanceSoundness

namespace AverCert.ArtifactSoundness

/-- Instantiate the artifact-independent acceptance wall at this artifact's real
wasm hash.  The remaining semantic bridge assumptions are still explicit. -/
theorem accept_sound_holds
    (hSide : AcceptanceSoundness.dischargeSideConditions AverCert.Artifact.data) :
    AverCert.Schema.Holds AverCert.Artifact.data.manifest := by
  exact AcceptanceSoundness.accept_sound CertModule.wasmSha256 AverCert.Artifact.data rfl
    AverCert.Artifact.claimObligationsBound
    AverCert.Artifact.standardFacesChecked
    AverCert.Artifact.fragmentsAccepted hSide

end AverCert.ArtifactSoundness
"#
    .to_string()
}

/// One redundant-but-honest closure pin per mutual-recursion SCC (emitted for the
/// primary member so each SCC appears once): the byte-derived
/// `(self, cross-target, memberSet)` triples the artifact's claims carry,
/// asserted to form a single closed cycle by the audited
/// `mutualMembersFormClosedSccs`. The artifact's `acceptedWithFinal` proof
/// already binds this closure over the real claims; this concrete pin documents
/// the byte-derived SCC group and gives the checker a self-contained surface
/// that fails closed if the group is not one closed cycle (a member's declared
/// set diverging, an extra/omitted/duplicate member, or a broken cross-edge).
fn render_mutual_scc_closure_pins(analysis: &Analysis) -> String {
    let mut out = String::new();
    for c in &analysis.certs {
        let Cert::MutualRecursion {
            position, scc, name, ..
        } = c.inner()
        else {
            continue;
        };
        if *position != 0 {
            continue;
        }
        if mutual_plan_from_cert(c).is_none() {
            continue;
        }
        let member_set = mutual_member_set_lean_value(scc);
        let members = scc
            .iter()
            .map(|m| format!("({}, {}, {member_set})", m.self_idx, m.cross_idx))
            .collect::<Vec<_>>()
            .join(", ");
        out.push_str(&format!(
            "/-- The byte-derived `{name}` SCC forms one closed mutual-recursion cycle. -/\n\
             example : AverCert.AcceptedArtifact.mutualMembersFormClosedSccs [{members}] = true := rfl\n\n",
        ));
    }
    out
}

/// Render a module carrier state as a Lean `Option Nat` literal. `none` is the
/// byte-proved carrierless state, not a missing value.
fn render_carrier_state(carrier: Option<u32>) -> String {
    match carrier {
        Some(idx) => format!("(some {idx})"),
        None => "none".to_string(),
    }
}

fn render_byte_list(bytes: &[u8]) -> String {
    let parts = bytes
        .iter()
        .map(|b| b.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{parts}]")
}

pub fn render_artifact_bytes_lean(wasm_bytes: &[u8]) -> String {
    format!(
        "-- Exact Wasm module as a little-endian Lean Nat plus byte length.\n\
         -- `aver cert verify` regenerates this file from the artifact it reads;\n\
         -- a cert-supplied file with this name is ignored.\n\
         import WasmSlice\n\n\
         set_option maxRecDepth 200000\n\n\
         namespace AverCert.ArtifactBytes\n\n\
         def modBytes : Nat := {}\n\n\
         def modLen : Nat := {}\n\n\
         end AverCert.ArtifactBytes\n",
        render_byte_nat(wasm_bytes),
        wasm_bytes.len()
    )
}

fn render_byte_nat(bytes: &[u8]) -> String {
    if bytes.is_empty() {
        return "0".to_string();
    }
    let mut numeral = String::with_capacity(2 + bytes.len() * 2);
    numeral.push_str("0x");
    for byte in bytes.iter().rev() {
        numeral.push_str(&format!("{byte:02x}"));
    }
    numeral
}

/// Write one file of the certificate project.
///
/// `name` may be a nested path: a module declared as `Data.Fibonacci` transpiles
/// to `Data/Fibonacci.lean`. The certificate directory is cleared and recreated
/// at the start of rendering, so intermediate directories never survive from a
/// previous run and have to be made here — without this, every project with a
/// dotted module dependency failed to emit a certificate at all.
fn write(dir: &Path, name: &str, content: &str) -> Result<(), String> {
    let path = dir.join(name);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| format!("create directory for {name}: {e}"))?;
    }
    std::fs::write(path, content).map_err(|e| format!("write {name}: {e}"))
}

fn sanitize_model_for_cert(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    for line in content.lines() {
        if line.trim_start().starts_with("deriving ") {
            continue;
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

fn hex(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        s.push_str(&format!("{b:02x}"));
    }
    s
}

fn render_contracts(analysis: &Analysis) -> String {
    let mut s = String::new();
    s.push_str(
        "/-\n  Named runtime-layer contracts consumed by the certificates in this project.\n\n\
         Each is threaded as an explicit HYPOTHESIS of the certificate theorems (the\n\
         `hadd` / `hAdd` / `hSub` / `boxRef` faces in `Certificate.lean`), never as a\n\
         Lean `axiom`, so `#print axioms` on every certificate theorem stays on the\n\
         core whitelist `[propext, Classical.choice, Quot.sound]`. The obligations\n\
         below are the \"prove once per toolchain release\" runtime layer; the\n\
         machine-readable list is `cert-manifest.json`.\n\n",
    );
    if analysis.contracts.is_empty() {
        s.push_str("  (no user function was certified — no runtime contracts consumed)\n");
    } else {
        for c in &analysis.contracts {
            s.push_str(&format!("  * {c}\n"));
        }
    }
    s.push_str("-/\n");
    s
}

fn render_module(analysis: &Analysis, wasm_name: &str, sha: &str) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "-- Emitted user-function bodies as `CertPrelude.WInstr` data, plus the\n\
         -- sha256 of the final `{wasm_name}.wasm` bytes (pinned).\n\
         import CertPrelude\n\nnamespace CertModule\nopen CertPrelude\n\n",
    ));
    s.push_str(&format!(
        "/-- sha256 of the certified `{wasm_name}.wasm` module bytes. -/\n\
         def wasmSha256 : String := \"{sha}\"\n\n",
    ));
    for c in &analysis.certs {
        s.push_str(&render_code_def(c));
        s.push('\n');
        s.push_str(&render_host_def(c));
        s.push('\n');
    }
    s.push_str("end CertModule\n");
    s
}

/// The runtime host-contract wiring for a certified body, as data in
/// `CertModule` so both the certificate proofs and the manifest reference the
/// one definition.
fn render_host_def(c: &Cert) -> String {
    // Host-call expr fragments with the integer-add face wire the byte-derived
    // box/add indices directly.
    if let Some(face) = c.int_add_face() {
        return format!(
            "/-- Runtime host wiring for `{name}` (box + add contracts). -/\n\
             def {name}Host (add : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  \
             else if fn = {add_idx} then some (2, add)\n  else none\n",
            name = c.name(),
            box_idx = face.box_idx,
            add_idx = face.add_idx,
            carrier = c.carrier(),
        );
    }
    match c.inner() {
        Cert::Recursive {
            name,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            combinator,
            ..
        } => {
            let cp = combinator.param();
            format!(
                "/-- Runtime host wiring for `{name}` (box + {cp} + sub contracts). -/\n\
                 def {name}Host ({cp} sub : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
                 if fn = {box_idx} then some (1, boxRef {carrier})\n  \
                 else if fn = {add_idx} then some (2, {cp})\n  \
                 else if fn = {sub_idx} then some (2, sub)\n  else none\n",
            )
        }
        Cert::AccumulatorRecursive {
            name,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (box + add + sub contracts). -/\n\
             def {name}Host (add sub : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  \
             else if fn = {add_idx} then some (2, add)\n  \
             else if fn = {sub_idx} then some (2, sub)\n  else none\n",
        ),
        Cert::AdtConstructor { name, .. }
        | Cert::FieldProjection { name, .. }
        | Cert::VerbatimWidenedMatch { name, .. }
        | Cert::VerbatimVariantDispatch { name, .. }
        | Cert::ExprFragment { name, .. } => format!(
            "/-- Runtime host wiring for `{name}` (no host calls). -/\n\
             def {name}Host : HostTbl := fun _ => none\n",
        ),
        Cert::StringEqVerbatimMatch {
            name,
            string_eq_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (String.eq contract). -/\n\
             def {name}Host (stringEq : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {string_eq_idx} then some (2, stringEq)\n  else none\n",
        ),
        Cert::StringConcatVerbatimMatch {
            name,
            string_concat_idx,
            result_ty,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (String.concat contract). -/\n\
             def {name}Host (stringConcat : Nat → List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {string_concat_idx} then some (1, stringConcat {result_ty})\n  else none\n",
        ),
        Cert::WidenedIntMatch {
            name,
            carrier,
            box_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (box contract for the default `0`). -/\n\
             def {name}Host : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  else none\n",
        ),
        Cert::VariantDispatch {
            name,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => {
            let a = if add_idx.is_some() { "add" } else { "_add" };
            let s = if sub_idx.is_some() { "sub" } else { "_sub" };
            let mut chain = format!("if fn = {box_idx} then some (1, boxRef {carrier})");
            if let Some(i) = add_idx {
                chain.push_str(&format!("\n  else if fn = {i} then some (2, add)"));
            }
            if let Some(i) = sub_idx {
                chain.push_str(&format!("\n  else if fn = {i} then some (2, sub)"));
            }
            format!(
                "/-- Runtime host wiring for `{name}` (box + contracted arithmetic). -/\n\
                 def {name}Host ({a} {s} : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
                 {chain}\n  else none\n",
            )
        }
        Cert::Composition { name, closure, .. } => format!(
            "/-- Runtime host wiring for `{name}`'s call closure (add contract). -/\n\
             def {name}Host (add _sub : List WVal → Option WVal) : HostTbl := fun fn =>\n    {}\n",
            compose_host_arms(closure),
        ),
        // The whole SCC shares ONE host, emitted once by the primary member.
        Cert::MutualRecursion {
            scc,
            position,
            carrier,
            box_idx,
            sub_idx,
            ..
        } => {
            if *position != 0 {
                String::new()
            } else {
                let primary = &scc[0].name;
                format!(
                    "/-- Runtime host wiring for the mutual-recursive SCC `{primary}` (box + sub). -/\n\
                     def {primary}Host (sub : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
                     if fn = {box_idx} then some (1, boxRef {carrier})\n  \
                     else if fn = {sub_idx} then some (2, sub)\n  else none\n",
                )
            }
        }
        Cert::NonRecursive { .. } => unreachable!(),
    }
}

fn render_code_def(c: &Cert) -> String {
    // The SCC shares ONE code table (all members' arms), named after the primary
    // (lowest-`self_idx`) member and emitted once by it.
    if let Cert::MutualRecursion { scc, position, .. } = c.inner() {
        if *position != 0 {
            return String::new();
        }
        let primary = &scc[0].name;
        return format!(
            "/-- Verbatim shared code table for the mutual-recursive SCC `{primary}` \
             (one arm per member). -/\n\
             def {primary}Code : CodeTbl := {value}\n",
            value = render_code_value(c),
        );
    }
    let doc = match c.inner() {
        Cert::Recursive { .. } => "self-recursive",
        Cert::AccumulatorRecursive { .. } => "accumulator self-recursive",
        Cert::AdtConstructor { .. } => "ADT constructor",
        Cert::FieldProjection { .. } => "field projection",
        Cert::WidenedIntMatch { .. } => "widened Int variant match",
        Cert::VerbatimWidenedMatch { .. } => "verbatim widened variant match",
        Cert::VerbatimVariantDispatch { .. } => "verbatim variant dispatch",
        Cert::StringEqVerbatimMatch { .. } => "verbatim String equality match",
        Cert::StringConcatVerbatimMatch { .. } => "verbatim String concatenation match",
        Cert::ExprFragment { .. } => "expr-fragment-v1",
        Cert::VariantDispatch { .. } => "general variant dispatch",
        Cert::Composition { .. } => "cross-function composition, whole call closure",
        Cert::MutualRecursion { .. } => "mutual-recursive SCC",
        Cert::NonRecursive { .. } => unreachable!(),
    };
    format!(
        "/-- Verbatim emitted body of `{name}` ({doc}). -/\n\
         def {name}Code : CodeTbl := {value}\n",
        name = c.name(),
        value = render_code_value(c),
    )
}

// Code-value helpers live in render_code.rs.

#[cfg(test)]
mod render_project_tests {
    use super::model_root_from_stem;

    #[test]
    fn model_roots_become_dotted_lean_module_names() {
        assert_eq!(model_root_from_stem("AverCommon").unwrap(), "AverCommon");
        assert_eq!(
            model_root_from_stem("Apps/Notepad/Store").unwrap(),
            "Apps.Notepad.Store"
        );
        assert!(model_root_from_stem("Apps/../Store").is_err());
        assert!(model_root_from_stem("Apps/Bad Name").is_err());
        assert!(model_root_from_stem("").is_err());
    }
}
