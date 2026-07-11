// ---- rendering -----------------------------------------------------------

/// Write the full `cert/` project. `model_files` are the (path, content) pairs
/// from the reused `aver proof` Lean emission (AverCommon + model modules).
pub fn write_project(
    out_dir: &Path,
    wasm_name: &str,
    wasm_bytes: &[u8],
    analysis: &Analysis,
    model_files: &[(String, String)],
) -> Result<(), String> {
    let cert_dir = out_dir.join("cert");
    std::fs::create_dir_all(&cert_dir).map_err(|e| format!("create cert dir: {e}"))?;

    // Copy in the semantics prelude + toolchain (single source of truth).
    write(&cert_dir, "CertPrelude.lean", CERT_PRELUDE)?;
    write(&cert_dir, "lean-toolchain", LEAN_TOOLCHAIN)?;

    // Copy the model files (AverCommon + <Module>.lean) verbatim.
    let mut model_roots: Vec<String> = Vec::new();
    for (path, content) in model_files {
        if path == "lakefile.lean" || path == "lean-toolchain" {
            continue;
        }
        write(&cert_dir, path, &sanitize_model_for_cert(content))?;
        if let Some(stem) = path.strip_suffix(".lean") {
            model_roots.push(stem.to_string());
        }
    }
    let model_info = ModelInfo::from_files(model_files);

    let sha = {
        let mut h = Sha256::new();
        h.update(wasm_bytes);
        hex(&h.finalize())
    };

    write_fragment_sidecars(&cert_dir, analysis, &model_info)?;

    write(&cert_dir, "Contracts.lean", &render_contracts(analysis))?;
    write(
        &cert_dir,
        "Module.lean",
        &render_module(analysis, wasm_name, &sha),
    )?;
    // Audited statement schema (fixed) + generated manifest literal + the one
    // final theorem that composes the per-export obligations.
    write(&cert_dir, "Schema.lean", CERT_SCHEMA)?;
    write(&cert_dir, "SchemaCore.lean", CERT_SCHEMA_CORE)?;
    write(&cert_dir, "PlanCheck.lean", CERT_PLAN_CHECK)?;
    write(&cert_dir, "PlanLower.lean", CERT_PLAN_LOWER)?;
    write(&cert_dir, "PlanBytes.lean", CERT_PLAN_BYTES)?;
    write(&cert_dir, "WasmSlice.lean", CERT_WASM_SLICE)?;
    write(
        &cert_dir,
        "ExprFragmentAccepted.lean",
        CERT_EXPR_FRAGMENT_ACCEPTED,
    )?;
    write(&cert_dir, "AcceptedArtifact.lean", CERT_ACCEPTED_ARTIFACT)?;
    write(
        &cert_dir,
        "AcceptedArtifactCore.lean",
        CERT_ACCEPTED_ARTIFACT_CORE,
    )?;
    write(
        &cert_dir,
        "ArtifactBytes.lean",
        &render_artifact_bytes_lean(wasm_bytes),
    )?;
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
    write(&cert_dir, "Final.lean", &render_final(analysis))?;
    write(
        &cert_dir,
        "Artifact.lean",
        &render_artifact(analysis, &model_info, &host_table_lean, &struct_table_lean),
    )?;
    write(&cert_dir, "lakefile.lean", &render_lakefile(&model_roots))?;

    // Content hashes the checker re-verifies: the audited schema and the
    // semantics prelude. Pinning these plus the final theorem name and the
    // manifest literal is the whole trust story.
    let schema_sha = sha256_hex(CERT_SCHEMA.as_bytes());
    let schema_core_sha = sha256_hex(CERT_SCHEMA_CORE.as_bytes());
    let prelude_sha = sha256_hex(CERT_PRELUDE.as_bytes());
    let plan_check_sha = sha256_hex(CERT_PLAN_CHECK.as_bytes());
    let plan_lower_sha = sha256_hex(CERT_PLAN_LOWER.as_bytes());
    let plan_bytes_sha = sha256_hex(CERT_PLAN_BYTES.as_bytes());
    let wasm_slice_sha = sha256_hex(CERT_WASM_SLICE.as_bytes());
    let expr_fragment_accepted_sha = sha256_hex(CERT_EXPR_FRAGMENT_ACCEPTED.as_bytes());
    let accepted_artifact_sha = sha256_hex(CERT_ACCEPTED_ARTIFACT.as_bytes());
    let accepted_artifact_core_sha = sha256_hex(CERT_ACCEPTED_ARTIFACT_CORE.as_bytes());
    let manifest_hashes = ManifestHashes {
        schema: &schema_sha,
        schema_core: &schema_core_sha,
        prelude: &prelude_sha,
        plan_check: &plan_check_sha,
        plan_lower: &plan_lower_sha,
        plan_bytes: &plan_bytes_sha,
        wasm_slice: &wasm_slice_sha,
        expr_fragment_accepted: &expr_fragment_accepted_sha,
        accepted_artifact: &accepted_artifact_sha,
        accepted_artifact_core: &accepted_artifact_core_sha,
    };
    std::fs::write(
        cert_dir.join("cert-manifest.json"),
        render_manifest(analysis, &model_info, wasm_name, &sha, &manifest_hashes),
    )
    .map_err(|e| format!("write manifest: {e}"))?;
    Ok(())
}

fn write_fragment_sidecars(
    cert_dir: &Path,
    analysis: &Analysis,
    model_info: &ModelInfo,
) -> Result<(), String> {
    let mut sidecars = Vec::new();
    for c in &analysis.certs {
        match c.inner() {
            Cert::ExprFragment {
                source_plan, plan, ..
            } => {
                if let Some(sym) = expr_fragment_source_plan(source_plan, plan) {
                    sidecars.push(sym_fragment_sidecar(c.name(), &sym));
                } else {
                    sidecars.push(expr_fragment_sidecar(c.name(), plan));
                }
            }
            Cert::StringConcatVerbatimMatch { .. } => {
                let plan = string_concat_plan_from_cert(c)
                    .expect("certified String.concat should project to a source plan");
                let sym_plan = string_concat_sym_plan_from_plan(&plan);
                sidecars.push(sym_fragment_sidecar(c.name(), &sym_plan));
                sidecars.push(string_concat_sidecar(c.name(), &plan));
            }
            Cert::StringEqVerbatimMatch { .. } => {
                let plan = string_eq_plan_from_cert(c)
                    .expect("certified String.eq should project to a source plan");
                let sym_plan = string_eq_sym_plan_from_plan(&plan);
                sidecars.push(sym_fragment_sidecar(c.name(), &sym_plan));
                sidecars.push(string_eq_sidecar(c.name(), &plan));
            }
            Cert::AdtConstructor { .. } => {
                if let Some(sym_plan) = adt_constructor_sym_plan_from_cert(c, model_info) {
                    sidecars.push(sym_fragment_sidecar(c.name(), &sym_plan));
                }
                if let Some(plan) = construct_plan_from_cert(c)
                    && check_construct_plan(&plan).is_ok()
                {
                    sidecars.push(construct_sidecar(c.name(), &plan));
                }
            }
            _ => {}
        }
    }
    if sidecars.is_empty() {
        return Ok(());
    }

    std::fs::create_dir_all(cert_dir.join("fragments"))
        .map_err(|e| format!("create cert fragments dir: {e}"))?;
    for plan in sidecars {
        std::fs::write(cert_dir.join(&plan.path), plan.text)
            .map_err(|e| format!("write {}: {e}", plan.path))?;
    }
    Ok(())
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
         -- v1 still checks/canonical-lowers byte-bound fragments in Rust before\n\
         -- rendering proofs; this module is the stable data surface for the v2\n\
         -- in-kernel checker.\n\
         import Schema\n\
         import PlanCheck\n\n\
         import PlanLower\n\
         import PlanBytes\n\
         import WasmSlice\n\
         import ExprFragmentAccepted\n\
         import ArtifactBytes\n\
         import Module\n\n\
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
            code_idx,
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
            "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
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
             /-- The audited Lean-side Wasm slicer finds `{name}`'s exact code-entry bytes\n\
                 inside the emitted module bytes by export name. -/\n\
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The audited Lean-side Wasm slicer binds `{name}` to its function\n\
                 index, defined-code index, type index and code-entry bytes. -/\n\
             example : AverCert.WasmSlice.funcBindingForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {func_binding} := rfl\n\n\
             /-- The audited Lean-side expr-fragment acceptance predicate aggregates\n\
                 plan checking, semantic lowering, byte lowering and byte-origin binding. -/\n\
             example : AverCert.ExprFragmentAccepted.accepted AverCert.ArtifactBytes.wasmBytes\n  \
               {export_name_bytes} {carrier} {name}Plan\n  \
               {lowered_body}\n  \
               {code_entry_bytes}\n  \
               {func_binding} := by dsimp [AverCert.ExprFragmentAccepted.accepted]; exact ⟨rfl, rfl, rfl, rfl, rfl⟩\n\n",
            plan_value = expr_fragment_plan_lean_value(plan),
            sym_plan = sym_plan,
        ));
    }
    for c in &analysis.certs {
        let Cert::AdtConstructor {
            name,
            self_idx,
            code_idx,
            type_idx,
            carrier,
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
            lower_construct_plan_code_entry_bytes(&construct_plan, *carrier)
        else {
            continue;
        };
        let lowered_body = lower_construct_plan(&construct_plan)
            .map(|ops| render_ops_value(&ops))
            .expect("checked construct plan lowers to WInstr body");
        let code_entry_bytes = render_byte_list(&code_entry_bytes);
        let export_name_bytes = render_byte_list(name.as_bytes());
        let func_binding = format!(
            "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
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
             example : AverCert.PlanLower.lowerConstructBody {name}ConstructPlan =\n  \
               some {lowered_body} := rfl\n\n\
             /-- The audited Lean-side canonical lowerer maps `{name}`'s constructor plan\n\
                 to the same instruction body emitted in `Module.lean`. -/\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerConstructBody {name}ConstructPlan := rfl\n\n\
             /-- The audited Lean-side byte lowerer maps `{name}`'s constructor plan\n\
                 to the exact canonical code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerConstructCodeEntry {carrier} {name}ConstructPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The audited Lean-side Wasm slicer finds `{name}`'s exact constructor\n\
                 code-entry bytes inside the emitted module bytes by export name. -/\n\
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The audited Lean-side Wasm slicer binds `{name}` to its function\n\
                 index, defined-code index, type index and code-entry bytes. -/\n\
             example : AverCert.WasmSlice.funcBindingForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {func_binding} := rfl\n\n",
            sym_plan_value = sym_plan_lean_value(&sym_plan),
            construct_plan_value = construct_plan_lean_value(&construct_plan),
            lowered_body = lowered_body,
        ));
    }
    for c in &analysis.certs {
        let Cert::StringConcatVerbatimMatch {
            name,
            self_idx,
            code_idx,
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
            "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
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
             example : AverCert.PlanBytes.lowerStringConcatCodeEntry {carrier} {result_ty} {container_ty} {string_concat_idx} {name}StringConcatPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The audited Lean-side Wasm slicer finds `{name}`'s exact code-entry bytes\n\
                 inside the emitted module bytes by export name. -/\n\
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The audited Lean-side Wasm slicer binds `{name}` to its function\n\
                 index, defined-code index, type index and code-entry bytes. -/\n\
             example : AverCert.WasmSlice.funcBindingForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {func_binding} := rfl\n\n\
             \n",
            sym_plan_value = sym_plan_lean_value(&sym_plan),
            plan_value = string_concat_plan_lean_value(&plan),
        ));
    }
    for c in &analysis.certs {
        let Cert::StringEqVerbatimMatch {
            name,
            self_idx,
            code_idx,
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
            "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
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
             /-- The audited Lean-side Wasm slicer finds `{name}`'s exact code-entry bytes\n\
                 inside the emitted module bytes by export name. -/\n\
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The audited Lean-side Wasm slicer binds `{name}` to its function\n\
                 index, defined-code index, type index and code-entry bytes. -/\n\
             example : AverCert.WasmSlice.funcBindingForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {func_binding} := rfl\n\n\
             \n",
            sym_plan_value = sym_plan_lean_value(&sym_plan),
            plan_value = string_eq_plan_lean_value(&plan),
        ));
    }
    for c in &analysis.certs {
        let (name, self_idx, type_idx, carrier, box_idx, add_idx, sub_idx) = match c.inner() {
            Cert::Recursive {
                name,
                self_idx,
                type_idx,
                carrier,
                box_idx,
                add_idx,
                sub_idx,
                ..
            }
            | Cert::AccumulatorRecursive {
                name,
                self_idx,
                type_idx,
                carrier,
                box_idx,
                add_idx,
                sub_idx,
                ..
            } => (name, *self_idx, *type_idx, *carrier, *box_idx, *add_idx, *sub_idx),
            _ => continue,
        };
        let Some(plan) = recursion_plan_from_cert(c) else {
            continue;
        };
        let code_entry_bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, carrier)
            .expect("certified recursion plan lowers to code-entry bytes");
        let code_entry_bytes = render_byte_list(&code_entry_bytes);
        let export_name_bytes = render_byte_list(name.as_bytes());
        let host_table = recursion_host_table_lean_value(box_idx, add_idx, sub_idx);
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
             example : AverCert.PlanCheck.checkRecursionPlanShape {self_idx} {host_table} {name}RecursionPlan = true := rfl\n\n\
             /-- The declared function type of `{name}` is the canonical certified\n\
                 signature over the Int carrier. -/\n\
             example : AverCert.WasmSlice.funcTypeMatches AverCert.ArtifactBytes.wasmBytes {type_idx} {arity} {carrier} = true := rfl\n\n\
             /-- The audited recursion lowerer maps `{name}`'s plan to the exact `Module.lean` body. -/\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerRecursionBody {carrier} {name}RecursionPlan := rfl\n\n\
             /-- The audited recursion byte lowerer reproduces `{name}`'s exact code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerRecursionCodeEntry {carrier} {name}RecursionPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The Wasm slicer finds `{name}`'s exact code-entry bytes by export name. -/\n\
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {code_entry_bytes} := rfl\n\n",
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
             example : AverCert.WasmSlice.funcTypeMatches AverCert.ArtifactBytes.wasmBytes {type_idx} 1 {carrier} = true := rfl\n\n\
             /-- The audited mutual lowerer maps `{name}`'s plan to its arm of the shared\n\
                 `{primary}Code` table (the exact `Module.lean` body). -/\n\
             example : (CertModule.{primary}Code {self_idx}).map (fun c => c.body) =\n  \
               AverCert.PlanLower.lowerMutualBody {carrier} {name}MutualPlan := rfl\n\n\
             /-- The audited mutual byte lowerer reproduces `{name}`'s exact code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerMutualCodeEntry {carrier} {name}MutualPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The Wasm slicer finds `{name}`'s exact code-entry bytes by export name. -/\n\
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {code_entry_bytes} := rfl\n\n",
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
        any = true;
        s.push_str(&format!(
            "/-- Byte-derived `verbatim-plan-v1` plan for `{name}` (a `Cod := WVal`\n\
                 verbatim `ref.test`-dispatch match; no host, no representation). -/\n\
             def {name}VerbatimPlan : VerbatimRawPlan := {plan_value}\n\n\
             /-- The audited verbatim-plan checker accepts `{name}`'s byte-derived plan. -/\n\
             example : AverCert.PlanCheck.checkVerbatimRawPlan {name}VerbatimPlan = true := rfl\n\n\
             /-- The audited verbatim lowerer maps `{name}`'s plan to the exact `Module.lean` body. -/\n\
             example : (CertModule.{name}Code {self_idx}).map (fun c => c.body) =\n  \
               some (AverCert.PlanLower.lowerVerbatimBody {name}VerbatimPlan) := rfl\n\n\
             /-- The audited verbatim byte lowerer reproduces `{name}`'s exact code-entry bytes. -/\n\
             example : AverCert.PlanBytes.lowerVerbatimCodeEntry {carrier} {name}VerbatimPlan =\n  \
               some {code_entry_bytes} := rfl\n\n\
             /-- The Wasm slicer finds `{name}`'s exact code-entry bytes by export name. -/\n\
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {code_entry_bytes} := rfl\n\n",
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
             /-- The Wasm slicer finds `{name}`'s exact code-entry bytes by export name. -/\n\
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {code_entry_bytes} := rfl\n\n",
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
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.wasmBytes {export_name_bytes} =\n  \
               some {code_entry} := rfl\n\n",
            name = entry.name,
            plan_value = composition_plan_lean_value(&plan),
            body = render_ops_value(&lowered),
            code_entry = render_byte_list(&code_entry),
        ));
    }
    if !any {
        s.push_str("-- This artifact contains no source/fragment plans.\n\n");
    }
    s.push_str("end AverCert.Plans\n");
    s
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
    composition_members: String,
    composition_claims: String,
    obligation_proof: String,
    sym_proof: String,
    string_eq_proof: String,
    string_proof: String,
    construct_proof: String,
    recursion_proof: String,
    mutual_proof: String,
    verbatim_proof: String,
    int_dispatch_proof: String,
    composition_proof: String,
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
    let mut composition_claims = Vec::new();
    let mut sym_proofs = Vec::new();
    let mut string_eq_proofs = Vec::new();
    let mut string_proofs = Vec::new();
    let mut construct_proofs = Vec::new();
    let mut recursion_proofs = Vec::new();
    let mut mutual_proofs = Vec::new();
    let mut verbatim_proofs = Vec::new();
    let mut int_dispatch_proofs = Vec::new();
    let mut composition_proofs = Vec::new();
    for c in &analysis.certs {
        match c.inner() {
            Cert::ExprFragment {
                name,
                carrier,
                self_idx,
                code_idx,
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
                let func_binding = format!(
                    "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
                );
                let proof = format!(
                    "⟨rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
                     ⟨⟨rfl, rfl, rfl, rfl, rfl⟩, rfl, rfl⟩⟩⟩"
                );
                if expr_fragment_source_plan(source_plan, plan).is_some() {
                    sym_claims.push(format!(
                        "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, hostTable := {host_table_lean}, structTable := {struct_table_lean}, plan := AverCert.Plans.{name}SymPlan, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.SymFragmentClaim)",
                        export_name = lean_str(name),
                    ));
                    sym_proofs.push(proof);
                }
            }
            Cert::StringConcatVerbatimMatch {
                name,
                self_idx,
                code_idx,
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
                    "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
                );
                string_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, resultTy := {result_ty}, containerTy := {container_ty}, concatFuncIdx := {string_concat_idx}, symPlan := AverCert.Plans.{name}StringConcatSymPlan, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.StringConcatClaim)",
                    export_name = lean_str(name),
                ));
                string_proofs.push(format!(
                    "⟨rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
                     ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
                ));
            }
            Cert::StringEqVerbatimMatch {
                name,
                self_idx,
                code_idx,
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
                    "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
                );
                string_eq_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, stringTy := {string_ty}, stringEqFuncIdx := {string_eq_idx}, symPlan := AverCert.Plans.{name}StringEqSymPlan, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.StringEqClaim)",
                    export_name = lean_str(name),
                ));
                string_eq_proofs.push(format!(
                    "⟨rfl, rfl, rfl, rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
                     ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
                ));
            }
            Cert::AdtConstructor {
                name,
                self_idx,
                code_idx,
                type_idx,
                carrier,
                ..
            } => {
                let Some(_sym_plan) = adt_constructor_sym_plan_from_cert(c, model_info) else {
                    continue;
                };
                let Some(plan) = construct_plan_from_cert(c) else {
                    continue;
                };
                let code_entry_bytes = lower_construct_plan_code_entry_bytes(&plan, *carrier)
                    .expect("certified constructor plan lowers to code-entry bytes");
                let code_entry_bytes = render_byte_list(&code_entry_bytes);
                let lowered_body = lower_construct_plan(&plan)
                    .map(|ops| render_ops_value(&ops))
                    .expect("certified constructor plan lowers to WInstr body");
                let export_name_bytes = render_byte_list(name.as_bytes());
                let func_binding = format!(
                    "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
                );
                construct_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, symPlan := AverCert.Plans.{name}ConstructSymPlan, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.ConstructClaim)",
                    export_name = lean_str(name),
                ));
                construct_proofs.push(format!(
                    "⟨rfl, rfl, rfl, rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
                     ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
                ));
            }
            Cert::Recursive {
                name,
                self_idx,
                code_idx,
                type_idx,
                carrier,
                box_idx,
                add_idx,
                sub_idx,
                ..
            }
            | Cert::AccumulatorRecursive {
                name,
                self_idx,
                code_idx,
                type_idx,
                carrier,
                box_idx,
                add_idx,
                sub_idx,
                ..
            } => {
                // Byte-equality gated: a normalized (alias-hop) body has no
                // canonical plan that reproduces its exact bytes — it stays on
                // the legacy witness route with no claim, fail-closed.
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
                let host_table = recursion_host_table_lean_value(*box_idx, *add_idx, *sub_idx);
                let func_binding = format!(
                    "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)"
                );
                recursion_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, hostTable := {host_table}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.RecursionClaim)",
                    export_name = lean_str(name),
                ));
                recursion_proofs.push(format!(
                    "⟨rfl, rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
                     ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
                ));
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
                // Byte-equality gated per member, exactly like the recursion
                // route: a member whose canonical plan cannot reproduce its exact
                // bytes stays on the legacy witness route with no claim.
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
                    "({{ funcIdx := {self_idx}, codeIdx := {code_idx}, typeIdx := {type_idx}, codeEntry := {code_entry_bytes} }} : AverCert.WasmSlice.FuncBinding)",
                    code_idx = member.code_idx,
                    type_idx = member.type_idx,
                );
                mutual_claims.push(format!(
                    "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, memberSet := {member_set}, hostTable := {host_table}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.MutualRecursionClaim)",
                    export_name = lean_str(name),
                ));
                mutual_proofs.push(format!(
                    "⟨rfl, rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
                     ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
                ));
            }
            Cert::VerbatimWidenedMatch {
                name, carrier, ..
            }
            | Cert::VerbatimVariantDispatch {
                name, carrier, ..
            } => {
                // Byte-equality gated: a body byte-noisier than the canonical
                // verbatim dispatch has no plan reproducing its exact bytes — it
                // stays on the legacy witness route with no claim, fail-closed.
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
                    "⟨rfl, rfl, rfl, ⟨_, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩"
                        .to_string(),
                );
            }
            Cert::VariantDispatch { name, carrier, .. }
            | Cert::WidenedIntMatch { name, carrier, .. } => {
                // Byte-equality gated: a body byte-noisier than the canonical
                // Int-face dispatch has no plan reproducing its exact bytes — it
                // stays on the legacy witness route with no claim, fail-closed.
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
                // (the two extra leading `rfl`s discharge the host-table
                // distinctness and obligation-wiring binds; the final `rfl`
                // pins the code table with the CANONICAL locals count, no
                // existential).
                int_dispatch_proofs.push(
                    "⟨rfl, rfl, rfl, rfl, rfl, ⟨_, _, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩"
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
                "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
                entry.self_idx, entry.code_idx, entry.type_idx, code_entry
            );
            let member_proof = format!(
                "⟨rfl, ⟨({body}), ({code_entry}), {binding}, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩"
            );
            named_proof = format!("⟨{member_proof}, {named_proof}⟩");
        }
        let _ = self_idx;
        composition_proofs.push(format!(
            "⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, {named_proof}⟩⟩⟩⟩⟩⟩"
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
    let obligation_proof_count = sym_proofs.len()
        + string_eq_proofs.len()
        + string_proofs.len()
        + construct_proofs.len()
        + recursion_proofs.len()
        + mutual_proofs.len()
        + verbatim_proofs.len()
        + int_dispatch_proofs.len()
        + composition_proofs.len();
    let obligation_proof = (0..obligation_proof_count).fold("trivial".to_string(), |acc, _| {
        format!("⟨rfl, {acc}⟩")
    });
    let sym_proof = sym_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let string_proof = string_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let string_eq_proof =
        string_eq_proofs
            .into_iter()
            .rev()
            .fold("trivial".to_string(), |acc, proof| {
                format!("⟨{proof}, {acc}⟩")
            });
    let construct_proof =
        construct_proofs
            .into_iter()
            .rev()
            .fold("trivial".to_string(), |acc, proof| {
                format!("⟨{proof}, {acc}⟩")
            });
    let recursion_proof =
        recursion_proofs
            .into_iter()
            .rev()
            .fold("trivial".to_string(), |acc, proof| {
                format!("⟨{proof}, {acc}⟩")
            });
    let mutual_proof = mutual_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let verbatim_proof = verbatim_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let int_dispatch_proof =
        int_dispatch_proofs
            .into_iter()
            .rev()
            .fold("trivial".to_string(), |acc, proof| {
                format!("⟨{proof}, {acc}⟩")
            });
    let composition_proof = composition_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    RenderedArtifactClaims {
        sym_claims,
        string_eq_claims,
        string_claims,
        construct_claims,
        recursion_claims,
        mutual_claims,
        verbatim_claims,
        int_dispatch_claims,
        composition_members,
        composition_claims,
        obligation_proof,
        sym_proof,
        string_eq_proof,
        string_proof,
        construct_proof,
        recursion_proof,
        mutual_proof,
        verbatim_proof,
        int_dispatch_proof,
        composition_proof,
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
    let fragment_proof = format!(
        concat!(
            "  dsimp [data, symFragmentClaims, stringEqClaims, stringConcatClaims, constructClaims, recursionClaims, mutualRecursionClaims, verbatimClaims, intDispatchClaims, compositionMembers, compositionClaims, AverCert.AcceptedArtifact.accepted,\n",
            "    AverCert.AcceptedArtifact.subjectMatchesArtifactRoot,\n",
            "    AverCert.AcceptedArtifact.expectedArtifactRoot,\n",
            "    AverCert.AcceptedArtifact.fragmentClaimObligationsInManifest,\n",
            "    AverCert.AcceptedArtifact.claimObligations,\n",
            "    AverCert.AcceptedArtifact.claimObligationsInManifest,\n",
            "    AverCert.AcceptedArtifact.claimObligationExports,\n",
            "    AverCert.AcceptedArtifact.claimsMatchManifest,\n",
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
            "    AverCert.AcceptedArtifact.acceptedCompositionFragments,\n",
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
            "    AverCert.AcceptedArtifact.compositionEdgeLookup,\n",
            "    AverCert.AcceptedArtifact.stringListNodup,\n",
            "    AverCert.AcceptedArtifact.stringListSetEq,\n",
            "    AverCert.AcceptedArtifact.intDispatchCanonicalHost,\n",
            "    AverCert.AcceptedArtifact.intDispatchCanonicalSlots,\n",
            "    AverCert.AcceptedArtifact.exprFragmentPlanAccepted,\n",
            "    AverCert.ExprFragmentAccepted.accepted]\n",
            "  exact ⟨finalCert, ⟨rfl, ⟨{obligation_proof}, ⟨⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, rfl⟩⟩⟩⟩⟩⟩⟩⟩⟩, ⟨{sym_proof}, ⟨{string_eq_proof}, ⟨{string_proof}, ⟨{construct_proof}, ⟨{recursion_proof}, ⟨⟨{mutual_proof}, rfl⟩, ⟨{verbatim_proof}, ⟨{int_dispatch_proof}, {composition_proof}⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩\n"
        ),
        obligation_proof = claims.obligation_proof,
        sym_proof = claims.sym_proof,
        string_eq_proof = claims.string_eq_proof,
        string_proof = claims.string_proof,
        construct_proof = claims.construct_proof,
        recursion_proof = claims.recursion_proof,
        mutual_proof = claims.mutual_proof,
        verbatim_proof = claims.verbatim_proof,
        int_dispatch_proof = claims.int_dispatch_proof
        ,composition_proof = claims.composition_proof
    );
    format!(
         "-- Artifact-carried acceptance root.\n\
         -- This file is useful metadata, not verifier authority: `aver cert verify`\n\
         -- pins `AverCert.Artifact.data` to its checker-reconstructed literal and\n\
         -- audits `AverCert.Artifact.certificate` through the Lean axiom collector.\n\
         import AcceptedArtifact\n\
         import ArtifactBytes\n\
         import Final\n\
         import Manifest\n\
         import Plans\n\n\
         set_option maxRecDepth 200000\n\
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
         def compositionMembers : List AverCert.AcceptedArtifact.CompositionMemberClaim := {composition_members_list}\n\n\
         def compositionClaims : List AverCert.AcceptedArtifact.CompositionClaim := {composition_claims_list}\n\n\
         {mutual_scc_closure_pins}\
         def data : AverCert.AcceptedArtifact.ArtifactData :=\n  \
           ({{ wasmBytes := AverCert.ArtifactBytes.wasmBytes, manifest := AverCert.manifest, symFragmentClaims := symFragmentClaims, stringEqClaims := stringEqClaims, stringConcatClaims := stringConcatClaims, constructClaims := constructClaims, recursionClaims := recursionClaims, mutualRecursionClaims := mutualRecursionClaims, verbatimClaims := verbatimClaims, intDispatchClaims := intDispatchClaims, compositionMembers := compositionMembers, compositionClaims := compositionClaims }} : AverCert.AcceptedArtifact.ArtifactData)\n\n\
         def acceptedWithFinal\n\
             (finalCert : AverCert.Schema.Holds AverCert.manifest) :\n\
             AverCert.AcceptedArtifact.accepted data := by\n\
         {fragment_proof}\n\
         theorem certificate : AverCert.AcceptedArtifact.accepted data :=\n  \
           acceptedWithFinal AverCert.Final.cert\n\n\
         end AverCert.Artifact\n",
        sym_claims_list = claims.sym_claims,
        string_eq_claims_list = claims.string_eq_claims,
        string_claims_list = claims.string_claims,
        construct_claims_list = claims.construct_claims,
        recursion_claims_list = claims.recursion_claims,
        mutual_claims_list = claims.mutual_claims,
        verbatim_claims_list = claims.verbatim_claims,
        int_dispatch_claims_list = claims.int_dispatch_claims,
        composition_members_list = claims.composition_members,
        composition_claims_list = claims.composition_claims,
        mutual_scc_closure_pins = render_mutual_scc_closure_pins(analysis),
        fragment_proof = fragment_proof
    )
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
        "-- Exact Wasm module bytes as Lean data.\n\
         -- `aver cert verify` regenerates this file from the artifact it reads;\n\
         -- a cert-supplied file with this name is ignored.\n\
         import WasmSlice\n\n\
         set_option maxRecDepth 200000\n\n\
         namespace AverCert.ArtifactBytes\n\n\
         def wasmBytes : AverCert.WasmSlice.ByteSeq := {}\n\n\
         end AverCert.ArtifactBytes\n",
        render_byte_list(wasm_bytes)
    )
}

fn write(dir: &Path, name: &str, content: &str) -> Result<(), String> {
    std::fs::write(dir.join(name), content).map_err(|e| format!("write {name}: {e}"))
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
    // Host-call expr fragments with the straight-line integer face wire the
    // byte-derived box/add indices exactly like the legacy straight-line class.
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
        Cert::StraightLine {
            name,
            carrier,
            box_idx,
            add_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (box + add contracts). -/\n\
             def {name}Host (add : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  \
             else if fn = {add_idx} then some (2, add)\n  else none\n",
        ),
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
        Cert::StraightLine { .. } => "straight-line add-constant",
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
