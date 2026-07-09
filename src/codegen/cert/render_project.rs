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

    write_expr_fragment_sidecars(&cert_dir, analysis)?;

    write(&cert_dir, "Contracts.lean", &render_contracts(analysis))?;
    write(
        &cert_dir,
        "Module.lean",
        &render_module(analysis, wasm_name, &sha),
    )?;
    // Audited statement schema (fixed) + generated manifest literal + the one
    // final theorem that composes the per-export obligations.
    write(&cert_dir, "Schema.lean", CERT_SCHEMA)?;
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
        "ArtifactBytes.lean",
        &render_artifact_bytes_lean(wasm_bytes),
    )?;
    write(&cert_dir, "Plans.lean", &render_expr_fragment_plans(analysis))?;
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
    write(&cert_dir, "Artifact.lean", &render_artifact(analysis))?;
    write(&cert_dir, "lakefile.lean", &render_lakefile(&model_roots))?;

    // Content hashes the checker re-verifies: the audited schema and the
    // semantics prelude. Pinning these plus the final theorem name and the
    // manifest literal is the whole trust story.
    let schema_sha = sha256_hex(CERT_SCHEMA.as_bytes());
    let prelude_sha = sha256_hex(CERT_PRELUDE.as_bytes());
    let plan_check_sha = sha256_hex(CERT_PLAN_CHECK.as_bytes());
    let plan_lower_sha = sha256_hex(CERT_PLAN_LOWER.as_bytes());
    let plan_bytes_sha = sha256_hex(CERT_PLAN_BYTES.as_bytes());
    let wasm_slice_sha = sha256_hex(CERT_WASM_SLICE.as_bytes());
    let expr_fragment_accepted_sha = sha256_hex(CERT_EXPR_FRAGMENT_ACCEPTED.as_bytes());
    let accepted_artifact_sha = sha256_hex(CERT_ACCEPTED_ARTIFACT.as_bytes());
    std::fs::write(
        cert_dir.join("cert-manifest.json"),
        render_manifest(
            analysis,
            &model_info,
            wasm_name,
            &sha,
            &schema_sha,
            &prelude_sha,
            &plan_check_sha,
            &plan_lower_sha,
            &plan_bytes_sha,
            &wasm_slice_sha,
            &expr_fragment_accepted_sha,
            &accepted_artifact_sha,
        ),
    )
    .map_err(|e| format!("write manifest: {e}"))?;
    Ok(())
}

fn write_expr_fragment_sidecars(cert_dir: &Path, analysis: &Analysis) -> Result<(), String> {
    let mut sidecars = Vec::new();
    for c in &analysis.certs {
        let Cert::ExprFragment { plan, .. } = c.inner() else {
            continue;
        };
        sidecars.push(expr_fragment_sidecar(c.name(), plan));
        if let Some(sym) = SymPlan::from_expr_fragment_source_subset(plan) {
            sidecars.push(sym_fragment_sidecar(c.name(), &sym));
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

fn render_expr_fragment_plans(analysis: &Analysis) -> String {
    let mut s = String::new();
    s.push_str(
        "-- Compiler-emitted expression-fragment plans as Lean data.\n\
         -- v1 still checks/canonical-lowers these in Rust before rendering proofs;\n\
         -- this module is the stable data surface for the v2 in-kernel checker.\n\
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
        let sym_plan = SymPlan::from_expr_fragment_source_subset(plan)
            .map(|sym| {
                format!(
                    "/-- Source-level `SymPlan` projection for `{name}`. Artifact-level\n\
                     acceptance prefers this claim when the fragment has a direct\n\
                     Aver-level meaning; the encoder below still binds it to `{name}Plan`. -/\n\
                     def {name}SymPlan : SymRawPlan := {sym_plan}\n\n\
                     /-- The audited Lean-side source-plan checker accepts `{name}`'s `SymPlan`. -/\n\
                     example : AverCert.PlanCheck.checkSymRawPlan {name}SymPlan = true := rfl\n\n\
                     /-- The audited Lean-side source encoder maps `{name}`'s `SymPlan`\n\
                         to the representation plan that is bound to bytes below. -/\n\
                     example : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan {name}SymPlan =\n  \
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
    if !any {
        s.push_str("-- This artifact contains no expr-fragment plans.\n\n");
    }
    s.push_str("end AverCert.Plans\n");
    s
}

struct RenderedArtifactClaims {
    sym_claims: String,
    expr_claims: String,
    sym_proof: String,
    expr_proof: String,
}

fn render_artifact_expr_fragment_claims(analysis: &Analysis) -> RenderedArtifactClaims {
    let mut sym_claims = Vec::new();
    let mut expr_claims = Vec::new();
    let mut sym_proofs = Vec::new();
    let mut expr_proofs = Vec::new();
    for c in &analysis.certs {
        let Cert::ExprFragment {
            name,
            carrier,
            self_idx,
            code_idx,
            type_idx,
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
        let proof = format!(
            "⟨rfl, rfl, ⟨({lowered_body}), ({code_entry_bytes}), {func_binding}, \
             ⟨⟨rfl, rfl, rfl, rfl, rfl⟩, rfl, ⟨_, rfl⟩⟩⟩⟩"
        );
        if SymPlan::from_expr_fragment_source_subset(plan).is_some() {
            sym_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, plan := AverCert.Plans.{name}SymPlan, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.SymFragmentClaim)",
                export_name = lean_str(name),
            ));
            sym_proofs.push(proof);
        } else {
            expr_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := {export_name}, carrier := {carrier}, plan := AverCert.Plans.{name}Plan, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.ExprFragmentClaim)",
                export_name = lean_str(name),
            ));
            expr_proofs.push(proof);
        }
    }
    let sym_claims = if sym_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", sym_claims.join(",\n  "))
    };
    let expr_claims = if expr_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", expr_claims.join(",\n  "))
    };
    let sym_proof = sym_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let expr_proof = expr_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    RenderedArtifactClaims {
        sym_claims,
        expr_claims,
        sym_proof,
        expr_proof,
    }
}

fn render_artifact(analysis: &Analysis) -> String {
    let claims = render_artifact_expr_fragment_claims(analysis);
    let fragment_proof = format!(
        concat!(
            "  dsimp [data, symFragmentClaims, exprFragmentClaims, AverCert.AcceptedArtifact.accepted,\n",
            "    AverCert.AcceptedArtifact.subjectMatchesArtifactRoot,\n",
            "    AverCert.AcceptedArtifact.expectedArtifactRoot,\n",
            "    AverCert.AcceptedArtifact.claimsMatchManifest,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimPlanPairs,\n",
            "    AverCert.AcceptedArtifact.exprFragmentClaimPlanPairs,\n",
            "    AverCert.AcceptedArtifact.acceptedFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedSymFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedExprFragments,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.symFragmentPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.exprFragmentClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.exprFragmentClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.exprFragmentPlanAccepted,\n",
            "    AverCert.ExprFragmentAccepted.accepted]\n",
            "  exact ⟨finalCert, ⟨rfl, ⟨⟨rfl, rfl⟩, ⟨{sym_proof}, {expr_proof}⟩⟩⟩⟩\n"
        ),
        sym_proof = claims.sym_proof,
        expr_proof = claims.expr_proof
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
         def exprFragmentClaims : List AverCert.AcceptedArtifact.ExprFragmentClaim := {claims_list}\n\n\
         def data : AverCert.AcceptedArtifact.ArtifactData :=\n  \
           ({{ wasmBytes := AverCert.ArtifactBytes.wasmBytes, manifest := AverCert.manifest, symFragmentClaims := symFragmentClaims, exprFragmentClaims := exprFragmentClaims }} : AverCert.AcceptedArtifact.ArtifactData)\n\n\
         def acceptedWithFinal\n\
             (finalCert : AverCert.Schema.Holds AverCert.manifest) :\n\
             AverCert.AcceptedArtifact.accepted data := by\n\
         {fragment_proof}\n\
         def certificate : AverCert.AcceptedArtifact.accepted data :=\n  \
           acceptedWithFinal AverCert.Final.cert\n\n\
         end AverCert.Artifact\n",
        sym_claims_list = claims.sym_claims,
        claims_list = claims.expr_claims,
        fragment_proof = fragment_proof
    )
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
