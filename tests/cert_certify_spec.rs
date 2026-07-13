//! Integration test for `aver compile --target wasm-gc --certify`.
//!
//! Runs the certificate emitter on a fixture and `lake build`s the emitted
//! `cert/` project, asserting the build succeeds, the certificate theorem is
//! kernel-clean (`#print axioms` on the core whitelist, no `sorryAx`), and the
//! manifest reports the expected certified function.
//!
//! Gated behind the `wasm` feature (the `--certify` path needs the wasm-gc
//! backend) and skipped when `lake` is unavailable, mirroring `proof_spec.rs`.
#![cfg(feature = "wasm")]

use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::process::Command;

fn temp_dir(prefix: &str) -> PathBuf {
    let mut d = std::env::temp_dir();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    d.push(format!("aver-{prefix}-{nanos}"));
    d
}

fn aver_command() -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command.env(
        "AVER_CERT_PRELUDE_CACHE",
        std::env::temp_dir().join("aver-cert-prelude-store"),
    );
    command.env(
        "AVER_CERT_DATA_CACHE",
        std::env::temp_dir().join("aver-cert-data-store"),
    );
    command
}

fn copy_dir_all(src: &std::path::Path, dst: &std::path::Path) {
    std::fs::create_dir_all(dst).unwrap();
    for entry in std::fs::read_dir(src).unwrap() {
        let entry = entry.unwrap();
        let target = dst.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_dir_all(&entry.path(), &target);
        } else {
            std::fs::copy(entry.path(), target).unwrap();
        }
    }
}

fn verify_certificate(wasm: &std::path::Path, cert_dir: &std::path::Path) -> (bool, String) {
    let output = aver_command()
        .arg("cert")
        .arg("verify")
        .arg(wasm)
        .arg(cert_dir)
        .output()
        .expect("expected `aver cert verify` to run");
    (
        output.status.success(),
        format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ),
    )
}

#[test]
fn certify_goal_matrix_manifest_tracks_current_surface() {
    // This fixture is the dashboard for "how much do we certify now?". Larger
    // programs such as examples/data/json.av remain integration side-effects;
    // this test pins the planned numerator/denominator directly. When a backlog
    // goal becomes certifiable, move it from `expected_backlog` into `expected`.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-goals");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/cert_goals.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify goals failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(out_dir.join("cert").join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    let expected_schema_core_sha = aver::codegen::cert::audited_schema_core_sha();
    let expected_decode_sha = aver::codegen::cert::audited_decode_sha();
    let expected_plan_check_sha = aver::codegen::cert::audited_plan_check_sha();
    let expected_plan_lower_sha = aver::codegen::cert::audited_plan_lower_sha();
    let expected_plan_bytes_sha = aver::codegen::cert::audited_plan_bytes_sha();
    let expected_wasm_slice_sha = aver::codegen::cert::audited_wasm_slice_sha();
    let expected_expr_fragment_accepted_sha =
        aver::codegen::cert::audited_expr_fragment_accepted_sha();
    let expected_accepted_artifact_sha = aver::codegen::cert::audited_accepted_artifact_sha();
    let expected_accepted_artifact_core_sha =
        aver::codegen::cert::audited_accepted_artifact_core_sha();
    assert_eq!(
        manifest["schema_core_sha256"].as_str(),
        Some(expected_schema_core_sha.as_str()),
        "manifest should pin the checker-owned SchemaCore.lean"
    );
    assert_eq!(
        manifest["cert_decode_sha256"].as_str(),
        Some(expected_decode_sha.as_str()),
        "manifest should pin the checker-owned CertDecode.lean"
    );
    assert_eq!(
        manifest["plan_check_sha256"].as_str(),
        Some(expected_plan_check_sha.as_str()),
        "manifest should pin the checker-owned PlanCheck.lean"
    );
    assert_eq!(
        manifest["plan_lower_sha256"].as_str(),
        Some(expected_plan_lower_sha.as_str()),
        "manifest should pin the checker-owned PlanLower.lean"
    );
    assert_eq!(
        manifest["plan_bytes_sha256"].as_str(),
        Some(expected_plan_bytes_sha.as_str()),
        "manifest should pin the checker-owned PlanBytes.lean"
    );
    assert_eq!(
        manifest["wasm_slice_sha256"].as_str(),
        Some(expected_wasm_slice_sha.as_str()),
        "manifest should pin the checker-owned WasmSlice.lean"
    );
    assert_eq!(
        manifest["expr_fragment_accepted_sha256"].as_str(),
        Some(expected_expr_fragment_accepted_sha.as_str()),
        "manifest should pin the checker-owned ExprFragmentAccepted.lean"
    );
    assert_eq!(
        manifest["accepted_artifact_sha256"].as_str(),
        Some(expected_accepted_artifact_sha.as_str()),
        "manifest should pin the checker-owned AcceptedArtifact.lean"
    );
    assert_eq!(
        manifest["accepted_artifact_core_sha256"].as_str(),
        Some(expected_accepted_artifact_core_sha.as_str()),
        "manifest should pin the checker-owned AcceptedArtifactCore.lean"
    );
    assert_eq!(
        manifest["artifact_certificate_root"].as_str(),
        Some(aver::codegen::cert::ARTIFACT_CERTIFICATE_ROOT),
        "manifest should expose the artifact-level certificate root"
    );
    assert_eq!(
        manifest["schema_version"].as_u64(),
        Some(60),
        "multiplicative/accumulator recursion migration is certificate schema 60"
    );
    assert_eq!(aver::codegen::cert::CERT_SCHEMA_VERSION, 60);
    let declared_uncertified = manifest["declaredUncertified"].as_array().unwrap();
    assert_eq!(
        declared_uncertified.len(),
        13,
        "all 36 module exports must be certified or explicitly declared"
    );
    assert!(declared_uncertified.iter().all(|entry| {
        entry.as_object().is_some_and(|object| {
            object.len() == 2
                && object
                    .get("name")
                    .and_then(serde_json::Value::as_str)
                    .is_some()
                && object
                    .get("reason")
                    .and_then(serde_json::Value::as_str)
                    .is_some()
        })
    }));
    assert_eq!(manifest["capabilities"], serde_json::json!([]));
    assert_eq!(
        manifest["start"],
        serde_json::json!({"present": false, "function_index": null})
    );
    let wasm = std::fs::read(out_dir.join("cert_goals.wasm")).unwrap();
    let (box_idx, add_idx, mul_idx, sub_idx) =
        aver::codegen::cert::byte_derived_frag_host_role_indices(&wasm).unwrap();
    assert_eq!(
        manifest["hostRoleTable"],
        serde_json::json!({"box": box_idx, "add": add_idx, "mul": mul_idx, "sub": sub_idx}),
        "manifest hostRoleTable must come from the Rust classifier over the emitted bytes"
    );
    let string_roles = aver::codegen::cert::byte_derived_string_host_roles(&wasm).unwrap();
    let string_roles_json = string_roles
        .iter()
        .map(|(function_index, role)| {
            let role = match role {
                aver::codegen::cert::StringHostRole::Eq => "stringEq",
                aver::codegen::cert::StringHostRole::Concat => "stringConcat",
            };
            serde_json::json!({"function_index": function_index, "role": role})
        })
        .collect::<Vec<_>>();
    assert_eq!(
        manifest["stringHostRoles"],
        serde_json::json!(string_roles_json),
        "manifest stringHostRoles must preserve the Rust classifier's full ordered list"
    );

    let actual: BTreeMap<String, String> = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| {
            (
                c["name"].as_str().unwrap().to_string(),
                c["class"].as_str().unwrap().to_string(),
            )
        })
        .collect();
    let expected: BTreeMap<String, String> = [
        ("addTwo", "expr-fragment-v1"),
        ("sumFrom", "self-recursive"),
        ("countDown", "multi-argument self-recursive"),
        ("quad", "cross-function-composition"),
        ("hex16", "cross-function-composition"),
        ("isEven", "mutual-recursive"),
        ("isOdd", "mutual-recursive"),
        ("mkOp", "adt-constructor"),
        ("evalOp", "variant-dispatch"),
        ("userName", "expr-fragment-v1"),
        ("boxInt", "widened-int-match"),
        ("wrapItems", "verbatim-widened-match"),
        ("tagName", "verbatim-variant-dispatch"),
        ("gauge", "variant-dispatch"),
        ("inAsciiDigit", "expr-fragment-v1"),
        ("quoteOrSelf", "verbatim-string-eq"),
        ("shout", "verbatim-string-concat"),
        ("intLessZero", "expr-fragment-v1"),
        ("intEqZero", "expr-fragment-v1"),
        ("boolAndGoal", "expr-fragment-v1"),
        ("floatAddGoal", "expr-fragment-v1"),
        ("floatMulAddGoal", "expr-fragment-v1"),
        ("floatLeGoal", "expr-fragment-v1"),
    ]
    .into_iter()
    .map(|(name, class)| (name.to_string(), class.to_string()))
    .collect();
    assert_eq!(
        actual, expected,
        "certified goal matrix changed; update the numerator deliberately"
    );
    let expr_entries = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|c| c["class"].as_str() == Some("expr-fragment-v1"))
        .collect::<Vec<_>>();
    assert_eq!(
        expr_entries.len(),
        9,
        "expr-fragment sidecar count changed; update this deliberately"
    );
    let mut sym_sidecar_names = BTreeSet::new();
    for entry in expr_entries {
        let name = entry["name"].as_str().unwrap();
        assert!(
            entry.get("fragment").is_none(),
            "{name} should not emit a duplicate byte-bound expr sidecar when a \
             source SymPlan can encode it"
        );
        assert!(
            entry.get("trace").is_none() && entry.get("trace_sha256").is_none(),
            "{name} should not emit trace/replay sidecars after plan-first lowering"
        );
        let source_fragment = &entry["source_fragment"];
        assert_eq!(
            source_fragment["profile"].as_str(),
            Some("sym-fragment-v1"),
            "{name} should carry its source-level SymPlan in `source_fragment`"
        );
        let source_plan = source_fragment["plan"]
            .as_str()
            .expect("source fragment plan path");
        sym_sidecar_names.insert(name.to_string());
        assert!(
            source_plan.starts_with("fragments/") && source_plan.ends_with(".sym-fragment-v1.plan"),
            "{name} should point at a source SymPlan sidecar, got {source_plan}"
        );
        let source_text = std::fs::read_to_string(out_dir.join("cert").join(source_plan))
            .expect("source fragment sidecar exists");
        assert!(
            source_text.starts_with("aver.sym-fragment.plan.v1\nprofile sym-fragment-v1\n"),
            "{name} source sidecar should be the canonical SymPlan:\n{source_text}"
        );
        let expected_source_sha = aver::codegen::cert::sha256_hex(source_text.as_bytes());
        assert_eq!(
            source_fragment["plan_sha256"].as_str(),
            Some(expected_source_sha.as_str()),
            "{name} source sidecar hash should match the sidecar bytes"
        );
    }
    assert!(
        sym_sidecar_names.contains("addTwo")
            && sym_sidecar_names.contains("userName")
            && sym_sidecar_names.contains("floatAddGoal")
            && sym_sidecar_names.contains("floatMulAddGoal")
            && sym_sidecar_names.contains("floatLeGoal")
            && sym_sidecar_names.contains("boolAndGoal")
            && sym_sidecar_names.contains("intLessZero")
            && sym_sidecar_names.contains("intEqZero")
            && sym_sidecar_names.contains("inAsciiDigit"),
        "direct source-level fragments should prefer sym sidecars, got {sym_sidecar_names:?}"
    );
    let plans_lean = std::fs::read_to_string(out_dir.join("cert").join("Plans.lean"))
        .expect("Plans.lean exists");
    assert!(
        plans_lean.contains("def floatAddGoalSymPlan : SymRawPlan"),
        "direct float fragment should render a source-level SymPlan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains(".prim .floatAdd [0, 1]"),
        "floatAddGoal SymPlan should expose source-level float add:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("checkSymRawPlan floatAddGoalSymPlan = true := rfl"),
        "direct SymPlan projection should be accepted by the Lean-side source checker:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("encodeSymRawPlanToExprFragmentRawPlan [(.box, ")
            && plans_lean.contains(" floatAddGoalSymPlan =\n  some floatAddGoalPlan := rfl"),
        "direct SymPlan projection should encode, under the byte-derived host-role table, \
         to the byte-bound ExprFragment plan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("some floatAddGoalPlan := rfl"),
        "SymPlan encoder witness should target the existing ExprFragment plan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("def intLessZeroSymPlan : SymRawPlan"),
        "intLessZero should render a source-level SymPlan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains(".intConstCmp .lt 0 (0 : Int)"),
        "intLessZero SymPlan should expose a source-level Int comparison:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("def userNameSymPlan : SymRawPlan"),
        "userName should render a source-level SymPlan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains(".projectField \"User\" 0 .string 0"),
        "userName SymPlan should expose the source-level field projection:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("[(\"User\", ")
            && plans_lean.contains(" userNameSymPlan =\n  some userNamePlan := rfl"),
        "userName SymPlan should encode, under the byte-derived host-role and struct \
         tables, to the byte-bound ExprFragment plan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("def mkOpConstructSymPlan : SymRawPlan"),
        "legacy ADT constructors should render source-level construct SymPlans:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains(".construct \"Op\" \"add\" [0]"),
        "mkOp construct SymPlan should expose source-level ADT construction:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("def mkOpConstructPlan : ConstructRawPlan"),
        "mkOp should render a target-bound construct plan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("checkConstructRawPlan mkOpConstructPlan = true := rfl"),
        "mkOp construct plan should pass the Lean-side target checker:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains(
            "constructPlanMatchesSymRawPlan\n  mkOpConstructSymPlan mkOpConstructPlan = true := rfl"
        ),
        "mkOp source construct plan should match the target-bound construct plan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("] mkOpConstructSymPlan = none := rfl"),
        "construct SymPlan should remain outside the expr-fragment encoder:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("lowerConstructCodeEntry 23 1 mkOpConstructPlan =\n  some [10, 1, 1, 99, 23, 32, 0, 251, 0, 1, 11] := rfl"),
        "mkOp construct plan should lower to the exact code-entry bytes:\n{plans_lean}"
    );
    let mkop_entry = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"].as_str() == Some("mkOp"))
        .expect("mkOp manifest entry");
    assert!(
        mkop_entry["source_fragment"]["profile"].as_str() == Some("sym-fragment-v1"),
        "mkOp should advertise its source constructor plan in the checked manifest:\n{mkop_entry:?}"
    );
    assert!(
        mkop_entry["fragment"]["profile"].as_str() == Some("construct-v1"),
        "mkOp should advertise its byte-bound constructor plan in the checked manifest:\n{mkop_entry:?}"
    );
    let mkop_source_plan =
        std::fs::read_to_string(out_dir.join("cert/fragments/6d6b4f70.sym-fragment-v1.plan"))
            .expect("mkOp source-only construct sidecar exists");
    assert!(
        mkop_source_plan.contains("construct type=Op ctor=add args=v0"),
        "mkOp sidecar should carry the constructor plan:\n{mkop_source_plan}"
    );
    let mkop_target_plan =
        std::fs::read_to_string(out_dir.join("cert/fragments/6d6b4f70.construct-v1.plan"))
            .expect("mkOp target-bound construct sidecar exists");
    assert!(
        mkop_target_plan.contains("profile construct-v1")
            && !mkop_target_plan.contains("struct ")
            && mkop_target_plan.contains("local index=0"),
        "mkOp target sidecar should carry the struct.new binding:\n{mkop_target_plan}"
    );
    let artifact_lean = std::fs::read_to_string(out_dir.join("cert").join("Artifact.lean"))
        .expect("Artifact.lean exists");
    assert!(
        artifact_lean
            .contains("def symFragmentClaims : List AverCert.AcceptedArtifact.SymFragmentClaim"),
        "artifact should carry source-level fragment claims:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("plan := AverCert.Plans.floatAddGoalSymPlan"),
        "source-projectable fragment should be claimed through SymPlan:\n{artifact_lean}"
    );
    assert!(
        !artifact_lean.contains("plan := AverCert.Plans.floatAddGoalPlan"),
        "source-projectable fragment should not carry a duplicate ExprFragmentClaim:\n{artifact_lean}"
    );
    assert!(
        !artifact_lean.contains("ExprFragmentClaim"),
        "artifact-level expr fragments should be source-first, with no raw ExprFragmentClaim fallback:\n{artifact_lean}"
    );
    assert!(
        !artifact_lean.contains("exprFragmentClaims"),
        "artifact-level expr fragments should not expose a raw exprFragmentClaims list:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("plan := AverCert.Plans.intLessZeroSymPlan"),
        "source-level int fragment should be claimed through SymPlan:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("theorem certificate : AverCert.AcceptedArtifact.accepted data :="),
        "artifact root should be a theorem with the exact AcceptedArtifact target:\n{artifact_lean}"
    );
    assert!(
        !artifact_lean.contains("plan := AverCert.Plans.intLessZeroPlan"),
        "source-level int fragment should not carry a duplicate ExprFragmentClaim:\n{artifact_lean}"
    );
    assert!(
        artifact_lean
            .contains("def constructClaims : List AverCert.AcceptedArtifact.ConstructClaim"),
        "artifact should carry constructor claims:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("symPlan := AverCert.Plans.mkOpConstructSymPlan"),
        "mkOp construct SymPlan should now be an AcceptedArtifact claim:\n{artifact_lean}"
    );

    let planned_goal_names: BTreeSet<String> = [
        "addTwo",
        "sumFrom",
        "countDown",
        "quad",
        "hex16",
        "isEven",
        "isOdd",
        "mkOp",
        "evalOp",
        "userName",
        "boxInt",
        "wrapItems",
        "tagName",
        "gauge",
        "inAsciiDigit",
        "quoteOrSelf",
        "shout",
        "intLessZero",
        "intEqZero",
        "boolAndGoal",
        "floatAddGoal",
        "floatMulAddGoal",
        "floatLeGoal",
        "idGoal",
        "listHeadGoal",
        "sumListGoal",
    ]
    .into_iter()
    .map(str::to_string)
    .collect();
    let expected_backlog: BTreeSet<String> = ["idGoal", "listHeadGoal", "sumListGoal"]
        .into_iter()
        .map(str::to_string)
        .collect();
    assert_eq!(planned_goal_names.len(), 26, "goal denominator changed");
    assert_eq!(actual.len(), 23, "goal numerator changed");

    let contracts: Vec<&str> = manifest["runtime_contracts"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c.as_str().unwrap())
        .collect();
    assert_eq!(
        contracts,
        vec![
            aver::codegen::cert::BOX_CONTRACT,
            aver::codegen::cert::INT_ADD_CONTRACT,
            aver::codegen::cert::INT_SUB_CONTRACT,
            aver::codegen::cert::STRING_EQ_CONTRACT,
            aver::codegen::cert::STRING_CONCAT_CONTRACT,
            aver::codegen::cert::INT_ADD_TOTAL_CONTRACT,
            aver::codegen::cert::INT_SUB_TOTAL_CONTRACT,
            aver::codegen::cert::INT_MUL_TOTAL_CONTRACT,
        ],
        "goal matrix runtime contracts changed"
    );

    let declined_names: BTreeSet<String> = manifest["source_level_only"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c["name"].as_str().unwrap().to_string())
        .collect();
    let planned_declined: BTreeSet<String> = declined_names
        .intersection(&planned_goal_names)
        .cloned()
        .collect();
    assert_eq!(
        planned_declined, expected_backlog,
        "goal backlog changed; update the denominator/numerator deliberately"
    );
    assert!(
        declined_names.contains("double"),
        "composition helper should remain reported as source-level-only: {declined_names:?}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn certify_goal_matrix_lands_v3_wall_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-v3-wall");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/cert_goals.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify goals failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    let audited_modules = [
        (
            "V3ExprFragmentFull.lean",
            aver::codegen::cert::CERT_V3_EXPR_FRAGMENT_FULL,
            "v3_expr_fragment_full_sha256",
            aver::codegen::cert::audited_v3_expr_fragment_full_sha(),
        ),
        (
            "V3StrongFuel.lean",
            aver::codegen::cert::CERT_V3_STRONG_FUEL,
            "v3_strong_fuel_sha256",
            aver::codegen::cert::audited_v3_strong_fuel_sha(),
        ),
        (
            "V3IfElse.lean",
            aver::codegen::cert::CERT_V3_IF_ELSE,
            "v3_if_else_sha256",
            aver::codegen::cert::audited_v3_if_else_sha(),
        ),
        (
            "V3GenericCertified.lean",
            aver::codegen::cert::CERT_V3_GENERIC_CERTIFIED,
            "v3_generic_certified_sha256",
            aver::codegen::cert::audited_v3_generic_certified_sha(),
        ),
        (
            "V3FieldProj.lean",
            aver::codegen::cert::CERT_V3_FIELD_PROJ,
            "v3_field_proj_sha256",
            aver::codegen::cert::audited_v3_field_proj_sha(),
        ),
        (
            "V3ConstructVerbatim.lean",
            aver::codegen::cert::CERT_V3_CONSTRUCT_VERBATIM,
            "v3_construct_verbatim_sha256",
            aver::codegen::cert::audited_v3_construct_verbatim_sha(),
        ),
        (
            "V3DispatchCore.lean",
            aver::codegen::cert::CERT_V3_DISPATCH_CORE,
            "v3_dispatch_core_sha256",
            aver::codegen::cert::audited_v3_dispatch_core_sha(),
        ),
        (
            "V3String.lean",
            aver::codegen::cert::CERT_V3_STRING,
            "v3_string_sha256",
            aver::codegen::cert::audited_v3_string_sha(),
        ),
        (
            "V3RecSpike.lean",
            aver::codegen::cert::CERT_V3_REC_SPIKE,
            "v3_rec_spike_sha256",
            aver::codegen::cert::audited_v3_rec_spike_sha(),
        ),
        (
            "V3MutualGeneric.lean",
            aver::codegen::cert::CERT_V3_MUTUAL_GENERIC,
            "v3_mutual_generic_sha256",
            aver::codegen::cert::audited_v3_mutual_generic_sha(),
        ),
        (
            "V3Composition.lean",
            aver::codegen::cert::CERT_V3_COMPOSITION,
            "v3_composition_sha256",
            aver::codegen::cert::audited_v3_composition_sha(),
        ),
        (
            "V3Master.lean",
            aver::codegen::cert::CERT_V3_MASTER,
            "v3_master_sha256",
            aver::codegen::cert::audited_v3_master_sha(),
        ),
        (
            "V3DischargeExprFragment.lean",
            aver::codegen::cert::CERT_V3_DISCHARGE_EXPR_FRAGMENT,
            "v3_discharge_expr_fragment_sha256",
            aver::codegen::cert::audited_v3_discharge_expr_fragment_sha(),
        ),
        (
            "V3DischargeFieldProj.lean",
            aver::codegen::cert::CERT_V3_DISCHARGE_FIELD_PROJ,
            "v3_discharge_field_proj_sha256",
            aver::codegen::cert::audited_v3_discharge_field_proj_sha(),
        ),
        (
            "V3DischargeConstruct.lean",
            aver::codegen::cert::CERT_V3_DISCHARGE_CONSTRUCT,
            "v3_discharge_construct_sha256",
            aver::codegen::cert::audited_v3_discharge_construct_sha(),
        ),
        (
            "V3DischargeVerbatim.lean",
            aver::codegen::cert::CERT_V3_DISCHARGE_VERBATIM,
            "v3_discharge_verbatim_sha256",
            aver::codegen::cert::audited_v3_discharge_verbatim_sha(),
        ),
        (
            "V3DischargeString.lean",
            aver::codegen::cert::CERT_V3_DISCHARGE_STRING,
            "v3_discharge_string_sha256",
            aver::codegen::cert::audited_v3_discharge_string_sha(),
        ),
        (
            "V3DischargeIntDispatch.lean",
            aver::codegen::cert::CERT_V3_DISCHARGE_INT_DISPATCH,
            "v3_discharge_int_dispatch_sha256",
            aver::codegen::cert::audited_v3_discharge_int_dispatch_sha(),
        ),
        (
            "V3DischargeRecursion.lean",
            aver::codegen::cert::CERT_V3_DISCHARGE_RECURSION,
            "v3_discharge_recursion_sha256",
            aver::codegen::cert::audited_v3_discharge_recursion_sha(),
        ),
        (
            "V3DischargeComposition.lean",
            aver::codegen::cert::CERT_V3_DISCHARGE_COMPOSITION,
            "v3_discharge_composition_sha256",
            aver::codegen::cert::audited_v3_discharge_composition_sha(),
        ),
        (
            "V3AcceptSound.lean",
            aver::codegen::cert::CERT_V3_ACCEPT_SOUND,
            "v3_accept_sound_sha256",
            aver::codegen::cert::audited_v3_accept_sound_sha(),
        ),
    ];
    for (file, embedded, manifest_key, expected_sha) in audited_modules {
        let emitted = std::fs::read_to_string(cert_dir.join(file))
            .unwrap_or_else(|e| panic!("emitted {file} exists: {e}"));
        assert_eq!(
            emitted, embedded,
            "emitted {file} must be byte-identical to its embedded audited source"
        );
        assert_eq!(
            manifest[manifest_key].as_str(),
            Some(expected_sha.as_str()),
            "manifest must pin checker-owned {file}"
        );
    }

    let final_lean =
        std::fs::read_to_string(cert_dir.join("Final.lean")).expect("Final.lean exists");
    assert!(
        final_lean.contains("V3Master.fieldProjection_direct_canonical_discharges \"userName\""),
        "coexistence projection arm must use the audited generic:\n{final_lean}"
    );
    for (name, theorem) in [
        ("wrapItems", "verbatim_canonical_discharges"),
        ("tagName", "verbatim_canonical_discharges"),
        ("quoteOrSelf", "stringEq_canonical_discharges"),
        ("shout", "stringConcat_canonical_discharges"),
    ] {
        assert!(
            final_lean.contains(&format!("V3Master.{theorem} \"{name}\"")),
            "migrated leaf arm must use the audited generic: {name}\n{final_lean}"
        );
    }
    for dispatch_name in ["evalOp", "boxInt", "gauge"] {
        assert!(
            final_lean.contains(&format!(
                "V3Master.intDispatch_canonical_discharges (exportName := \"{dispatch_name}\")"
            )),
            "dispatch arm must use the audited generic: {dispatch_name}\n{final_lean}"
        );
    }
    assert!(
        final_lean.contains("V3Master.construct_canonical_discharges (exportName := \"mkOp\")")
            && final_lean.contains("(hSemantic := CertProofs.mkOp_constructSemanticBridge)"),
        "construct-with-model arm must use the audited discharge and emitted bridge:\n{final_lean}"
    );
    for recursion_name in ["sumFrom", "countDown"] {
        assert!(
            final_lean.contains(&format!("exportName := \"{recursion_name}\""))
                && final_lean.contains("V3Master.recursion_claim_discharges artifact")
                && final_lean.contains(&format!(
                    "CertProofs.{recursion_name}_recursionSemanticBridge"
                )),
            "recursion arm must use the audited discharge and emitted bridge: {recursion_name}\n{final_lean}"
        );
    }
    for bespoke_name in ["addTwo"] {
        assert!(
            final_lean.contains(&format!("CertProofs.{bespoke_name}_")),
            "unmigrated arm changed during coexistence migration: {bespoke_name}\n{final_lean}"
        );
    }
    let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
        .expect("Certificate.lean exists");
    assert!(
        !certificate.contains("userName_wasm_certified")
            && !certificate.contains("userName_simulates")
            && !certificate.contains("wrapItems_wasm_certified")
            && !certificate.contains("wrapItems_simulates")
            && !certificate.contains("tagName_wasm_certified")
            && !certificate.contains("tagName_simulates")
            && !certificate.contains("quoteOrSelf_wasm_certified")
            && !certificate.contains("quoteOrSelf_simulates")
            && !certificate.contains("shout_wasm_certified")
            && !certificate.contains("shout_simulates")
            && !certificate.contains("evalOp_wasm_certified")
            && !certificate.contains("evalOp_simulates")
            && !certificate.contains("boxInt_wasm_certified")
            && !certificate.contains("boxInt_simulates")
            && !certificate.contains("gauge_wasm_certified")
            && !certificate.contains("gauge_simulates")
            && !certificate.contains("mkOp_wasm_certified")
            && !certificate.contains("mkOp_simulates")
            && !certificate.contains("sumFrom_wasm_certified")
            && !certificate.contains("sumFrom_wasm_total")
            && !certificate.contains("sumFrom_simulates")
            && !certificate.contains("sumFromHostRef")
            && !certificate.contains("countDown_wasm_certified")
            && !certificate.contains("countDown_wasm_total")
            && !certificate.contains("countDown_simulates")
            && !certificate.contains("countDownHostRef"),
        "migrated leaf/dispatch/construct/recursion families must not emit bespoke simulations or tripwires:\n{certificate}"
    );
    for dispatch_name in ["evalOp", "boxInt", "gauge"] {
        assert!(
            certificate.contains(&format!(
                "theorem {dispatch_name}_intDispatchSemanticBridge"
            )),
            "dispatch must emit its option-(b) source-model bridge: {dispatch_name}\n{certificate}"
        );
    }
    assert!(
        certificate.contains("theorem mkOp_constructSemanticBridge")
            && certificate.contains("cases n")
            && certificate.contains("V3ConstructVerbatim.constructModelFields"),
        "construct-with-model must emit only its small source-model bridge:\n{certificate}"
    );
    assert!(
        certificate.contains("theorem sumFrom_recursionSemanticBridge")
            && certificate.contains("have hModelFuel")
            && certificate.contains("V3Rec.evalRecUFuel")
            && certificate.contains("⟨n, _, rfl, hv")
            && certificate.contains("⟨[n], ⟨ReprAll.cons hv ReprAll.nil, rfl⟩"),
        "recursion must emit both option-(b) model directions and no evaluator proof:\n{certificate}"
    );
    assert!(
        certificate.contains("theorem countDown_recursionSemanticBridge")
            && certificate.contains("V3Rec.evalRecAFuel")
            && certificate.contains("refine Or.inr")
            && certificate.contains("⟨n, acc, vn, vacc, rfl, hvn, hvacc")
            && certificate.contains("⟨[n, acc],")
            && certificate.contains("ReprAll.cons hvn (ReprAll.cons hvacc ReprAll.nil)"),
        "accumulator recursion must emit both arity-two option-(b) model directions:\n{certificate}"
    );
    let user_name = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"] == "userName")
        .unwrap();
    assert_eq!(
        user_name["theorem"],
        "V3Master.fieldProjection_direct_canonical_discharges"
    );
    let mk_op = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"] == "mkOp")
        .unwrap();
    assert_eq!(mk_op["theorem"], "V3Master.construct_canonical_discharges");
    let sum_from = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"] == "sumFrom")
        .unwrap();
    assert_eq!(sum_from["theorem"], "V3Master.recursion_claim_discharges");
    let count_down = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"] == "countDown")
        .unwrap();
    assert_eq!(count_down["theorem"], "V3Master.recursion_claim_discharges");
    for (name, theorem) in [
        ("wrapItems", "V3Master.verbatim_canonical_discharges"),
        ("tagName", "V3Master.verbatim_canonical_discharges"),
        ("quoteOrSelf", "V3Master.stringEq_canonical_discharges"),
        ("shout", "V3Master.stringConcat_canonical_discharges"),
    ] {
        let entry = manifest["certified"]
            .as_array()
            .unwrap()
            .iter()
            .find(|entry| entry["name"] == name)
            .unwrap();
        assert_eq!(entry["theorem"], theorem);
    }
    for name in ["evalOp", "boxInt", "gauge"] {
        let entry = manifest["certified"]
            .as_array()
            .unwrap()
            .iter()
            .find(|entry| entry["name"] == name)
            .unwrap();
        assert_eq!(
            entry["theorem"],
            "V3Master.intDispatch_canonical_discharges"
        );
    }

    let started = std::time::Instant::now();
    let build = Command::new("lake")
        .current_dir(&cert_dir)
        .arg("build")
        .output()
        .expect("expected `lake build` to run");
    let elapsed = started.elapsed();
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    eprintln!("emitted v3 wall lake build: {:.2?}", elapsed);
    assert!(
        build.status.success(),
        "lake build of emitted v3 wall cert failed after {elapsed:.2?}:\n{combined}"
    );
    assert!(
        combined.contains(
            "'AverCert.Final.cert' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "mixed generic/bespoke Final.cert changed axiom surface:\n{combined}"
    );
    assert!(
        combined.contains(
            "'CertProofs.sumFrom_recursionSemanticBridge' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ) && combined.contains(
            "'CertProofs.countDown_recursionSemanticBridge' depends on axioms: [propext]"
        ) && combined.contains(
            "'V3Master.recursion_claim_discharges' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "recursion bridge/discharge changed axiom surface:\n{combined}"
    );

    let typecheck = cert_dir.join("V3AcceptRealTypecheck.lean");
    std::fs::write(
        &typecheck,
        r#"import V3AcceptReal

example :
    V3Master.dischargeSideConditions AverCert.Artifact.data →
    AverCert.Schema.Holds AverCert.Artifact.data.manifest :=
  AverCert.V3AcceptReal.accept_sound_holds

#print axioms AverCert.V3AcceptReal.accept_sound_holds
"#,
    )
    .expect("write V3AcceptReal typecheck");
    let typecheck_output = Command::new("lake")
        .current_dir(&cert_dir)
        .args(["env", "lean", "V3AcceptRealTypecheck.lean"])
        .output()
        .expect("expected V3AcceptReal typecheck to run");
    let typecheck_combined = format!(
        "{}{}",
        String::from_utf8_lossy(&typecheck_output.stdout),
        String::from_utf8_lossy(&typecheck_output.stderr)
    );
    assert!(
        typecheck_output.status.success(),
        "V3AcceptReal Schema.Holds typecheck failed:\n{typecheck_combined}"
    );
    assert!(
        typecheck_combined.contains(
            "'AverCert.V3AcceptReal.accept_sound_holds' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "V3AcceptReal.accept_sound_holds did not reduce to the exact audited axiom set:\n{typecheck_combined}"
    );
    assert!(
        !combined.contains("sorryAx") && !typecheck_combined.contains("sorryAx"),
        "emitted v3 wall leaked sorryAx:\n{combined}\n{typecheck_combined}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn cert_verify_declines_hostile_leaf_dispatch_construct_and_recursion_models() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping hostile leaf-model test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-hostile-leaf-models");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/cert_goals.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("compile cert_goals for hostile model checks");
    assert!(
        compile.status.success(),
        "hostile-model baseline compile failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("cert_goals.wasm");
    let cert = out_dir.join("cert");
    let (clean_ok, clean_report) = verify_certificate(&wasm, &cert);
    assert!(
        clean_ok,
        "hostile-model baseline must first certify:\n{clean_report}"
    );

    for (label, honest, hostile) in [
        (
            "verbatim",
            "model := tagNameModel }",
            "model := wrapItemsModel }",
        ),
        (
            "string",
            "model := quoteOrSelfModel }",
            "model := shoutModel }",
        ),
        ("dispatch", "model := gauge }", "model := fun _ => 0 }"),
    ] {
        let tampered = temp_dir(&format!("certify-hostile-{label}-model"));
        copy_dir_all(&out_dir, &tampered);
        let manifest = tampered.join("cert/Manifest.lean");
        let source = std::fs::read_to_string(&manifest).unwrap();
        let edited = source.replacen(honest, hostile, 1);
        assert_ne!(
            source, edited,
            "{label} obligation model shape changed; update the hostile-model regression"
        );
        std::fs::write(&manifest, edited).unwrap();

        let (ok, report) =
            verify_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
        assert!(
            !ok && report.contains("DECLINED") && !report.contains("CERTIFIED"),
            "wrong {label} model must make its emitted bridge fail and be DECLINED:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&tampered);
    }

    let tampered = temp_dir("certify-hostile-construct-model-definition");
    copy_dir_all(&out_dir, &tampered);
    let model = tampered.join("cert/CertGoals.lean");
    let source = std::fs::read_to_string(&model).unwrap();
    let honest = "def mkOp (n : Int) : Op :=\n  Op.add n";
    let hostile = "def mkOp (n : Int) : Op :=\n  Op.neg n";
    let edited = source.replacen(honest, hostile, 1);
    assert_ne!(
        source, edited,
        "construct model definition changed; update the hostile-model regression"
    );
    std::fs::write(&model, edited).unwrap();
    let manifest = std::fs::read_to_string(tampered.join("cert/Manifest.lean")).unwrap();
    assert!(
        manifest.contains("model := mkOp }"),
        "construct hostile regression must leave the manifest model reference untouched"
    );

    let (ok, report) =
        verify_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
    assert!(
        !ok && report.contains("DECLINED") && !report.contains("CERTIFIED"),
        "wrong generated construct model definition must fail its emitted bridge and be DECLINED:\n{report}"
    );
    let _ = std::fs::remove_dir_all(&tampered);

    let tampered = temp_dir("certify-hostile-recursion-model-definition");
    copy_dir_all(&out_dir, &tampered);
    let model = tampered.join("cert/CertGoals.lean");
    let source = std::fs::read_to_string(&model).unwrap();
    let honest = "else (n + sumFrom__fuel fuel' (n - 1)))";
    let hostile = "else (2 + sumFrom__fuel fuel' (n - 1)))";
    let edited = source.replacen(honest, hostile, 1);
    assert_ne!(
        source, edited,
        "recursion model definition changed; update the hostile-model regression"
    );
    std::fs::write(&model, edited).unwrap();
    let manifest = std::fs::read_to_string(tampered.join("cert/Manifest.lean")).unwrap();
    assert!(
        manifest.contains("model := fun ns => sumFrom (ns.headD 0)"),
        "recursion hostile regression must leave the manifest model reference untouched"
    );

    let (ok, report) =
        verify_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
    assert!(
        !ok && report.contains("DECLINED") && !report.contains("CERTIFIED"),
        "wrong generated recursion model definition must fail its emitted bridge and be DECLINED:\n{report}"
    );
    let _ = std::fs::remove_dir_all(&tampered);

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn certify_straight_line_fixture_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/certprobe.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    let certified: Vec<&str> = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c["name"].as_str().unwrap())
        .collect();
    assert!(
        certified.contains(&"addTwo"),
        "expected addTwo certified, got {certified:?}"
    );

    let build = Command::new("lake")
        .current_dir(&cert_dir)
        .arg("build")
        .output()
        .expect("expected `lake build` to run");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(
        build.status.success(),
        "lake build of emitted cert failed:\n{combined}"
    );
    // Kernel-clean: the certificate theorem's `#print axioms` must show the
    // core whitelist and never `sorryAx`.
    assert!(
        combined.contains(
            "addTwo_wasm_certified' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "certificate theorem not kernel-clean:\n{combined}"
    );
    assert!(
        !combined.contains("sorryAx"),
        "certificate leaked sorryAx:\n{combined}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn certify_declines_overflowing_multiplication_recursion() {
    // No `lake` needed: this is a pure emitter fail-closed check.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-recdecline");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/recdecline.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    // Must NOT panic / crash — a large multiplier makes the guard overflow, and
    // the classifier declines rather than aborting the emitter.
    assert!(
        compile.status.success(),
        "compile --certify must not crash on an overflowing multiplier:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(out_dir.join("cert").join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    let certified: Vec<&str> = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c["name"].as_str().unwrap())
        .collect();
    let declined: Vec<&str> = manifest["source_level_only"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c["name"].as_str().unwrap())
        .collect();
    assert!(
        !certified.contains(&"wild"),
        "out-of-range multiplier must NOT be certified, got {certified:?}"
    );
    assert!(
        declined.contains(&"wild"),
        "out-of-range multiplier must be declined, got {declined:?}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn certify_fueled_recursion_generality_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify recursion test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-recgen");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/recgen.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    let certified: Vec<&str> = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c["name"].as_str().unwrap())
        .collect();
    // The audited recursion generics cover unary addition/multiplication and
    // the exact two-argument tail-accumulator shape.
    for name in ["sumFrom", "constPlus", "backward", "factorial", "countDown"] {
        assert!(
            certified.contains(&name),
            "expected {name} certified, got {certified:?}"
        );
    }
    let certified_entries = manifest["certified"].as_array().unwrap();
    for name in ["sumFrom", "constPlus", "backward", "factorial", "countDown"] {
        let entry = certified_entries
            .iter()
            .find(|entry| entry["name"] == name)
            .unwrap();
        assert_eq!(entry["policy"], "simulatesModelTotally");
        assert_eq!(entry["level"], "L3");
        assert_eq!(entry["theorem"], "V3Master.recursion_claim_discharges");
        assert_eq!(entry["termination_witness"]["measure"]["kind"], "intNatAbs");
        assert_eq!(entry["termination_witness"]["measure"]["param_index"], 0);
        assert_eq!(entry["termination_witness"]["descent"], -1);
    }
    let contracts = manifest["runtime_contracts"].as_array().unwrap();
    assert!(
        contracts
            .iter()
            .any(|c| { c == aver::codegen::cert::INT_ADD_TOTAL_CONTRACT })
    );
    assert!(
        contracts
            .iter()
            .any(|c| { c == aver::codegen::cert::INT_SUB_TOTAL_CONTRACT })
    );
    assert!(
        contracts
            .iter()
            .any(|c| { c == aver::codegen::cert::INT_MUL_CONTRACT })
    );
    assert!(
        contracts
            .iter()
            .any(|c| { c == aver::codegen::cert::INT_MUL_TOTAL_CONTRACT })
    );

    let build = Command::new("lake")
        .current_dir(&cert_dir)
        .arg("build")
        .output()
        .expect("expected `lake build` to run");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(
        build.status.success(),
        "lake build of emitted recursion cert failed:\n{combined}"
    );
    // Every supported recursion family emits only its small source-model
    // bridge. The evaluator, lowering, and totality proofs stay in the audited
    // wall.
    let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean")).unwrap();
    let final_lean = std::fs::read_to_string(cert_dir.join("Final.lean")).unwrap();
    for name in ["sumFrom", "constPlus", "backward", "factorial", "countDown"] {
        assert!(
            combined.contains(&format!(
                "'CertProofs.{name}_recursionSemanticBridge' depends on axioms:"
            )),
            "recursion bridge for {name} was not axiom-audited:\n{combined}"
        );
        assert!(
            certificate.contains(&format!("theorem {name}_recursionSemanticBridge"))
                && !certificate.contains(&format!("{name}_wasm_certified"))
                && !certificate.contains(&format!("{name}_wasm_total"))
                && !certificate.contains(&format!("{name}_simulates"))
                && !certificate.contains(&format!("{name}HostRef"))
                && final_lean.contains("V3Master.recursion_claim_discharges artifact")
                && final_lean.contains(&format!("CertProofs.{name}_recursionSemanticBridge")),
            "migrated recursion emitted a bespoke proof/tripwire or missed the generic arm for {name}:\n{certificate}\n{final_lean}"
        );
    }
    assert!(
        !certificate.contains("factorial_wasm_certified")
            && !certificate.contains("factorial_wasm_total")
            && !certificate.contains("factorial_simulates")
            && !certificate.contains("factorialHostRef")
            && !certificate.contains("countDown_wasm_certified")
            && !certificate.contains("countDown_wasm_total")
            && !certificate.contains("countDown_simulates")
            && !certificate.contains("countDownHostRef"),
        "migrated multiplication/accumulator recursions retained bespoke proof emission:\n{certificate}"
    );
    assert!(
        combined.contains(
            "'CertProofs.factorial_recursionSemanticBridge' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ) && combined.contains(
            "'CertProofs.countDown_recursionSemanticBridge' depends on axioms: [propext]"
        ),
        "multiplication/accumulator bridges changed axiom surface:\n{combined}"
    );
    assert!(
        combined.contains(
            "'V3Master.recursion_claim_discharges' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ) && combined.contains(
            "'AverCert.Final.cert' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "audited recursion discharge/final theorem changed axiom surface:\n{combined}"
    );
    assert!(
        !combined.contains("sorryAx"),
        "recursion certificate leaked sorryAx:\n{combined}"
    );

    for (name, honest, hostile) in [
        (
            "factorial",
            "else (n * factorial__fuel fuel' (n - 1)))",
            "else (2 * factorial__fuel fuel' (n - 1)))",
        ),
        (
            "countDown",
            "else countDown__fuel fuel' (n - 1) (acc + n))",
            "else countDown__fuel fuel' (n - 1) (acc + 2))",
        ),
    ] {
        let tampered = temp_dir(&format!("certify-hostile-{name}-definition"));
        copy_dir_all(&out_dir, &tampered);
        let model = tampered.join("cert/RecGen.lean");
        let source = std::fs::read_to_string(&model).unwrap();
        let edited = source.replacen(honest, hostile, 1);
        assert_ne!(
            source, edited,
            "{name} source model changed; update the hostile-model regression"
        );
        std::fs::write(&model, edited).unwrap();

        let (ok, report) =
            verify_certificate(&tampered.join("recgen.wasm"), &tampered.join("cert"));
        assert!(
            !ok && report.contains("DECLINED") && !report.contains("CERTIFIED"),
            "wrong generated {name} definition must be caught by its semantic bridge:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&tampered);
    }

    // GuardIso: the bridges are not decorative. A unary bridge claiming the
    // wrong parsed base and an accumulator bridge claiming the unary family
    // must each stop the certificate from building.
    for (name, honest, hostile) in [
        (
            "factorial-shape",
            "({ base := 1, step := .inputSecond } : V3Rec.RecShapeU)",
            "({ base := 2, step := .inputSecond } : V3Rec.RecShapeU)",
        ),
        ("countDown-shape", "refine Or.inr ?_", "refine Or.inl ?_"),
    ] {
        let tampered = temp_dir(&format!("certify-guardiso-{name}"));
        copy_dir_all(&out_dir, &tampered);
        let certificate = tampered.join("cert/Certificate.lean");
        let source = std::fs::read_to_string(&certificate).unwrap();
        let edited = source.replace(honest, hostile);
        assert_ne!(
            source, edited,
            "{name} bridge shape changed; update the GuardIso regression"
        );
        std::fs::write(&certificate, edited).unwrap();

        let (ok, report) =
            verify_certificate(&tampered.join("recgen.wasm"), &tampered.join("cert"));
        assert!(
            !ok && report.contains("DECLINED") && !report.contains("CERTIFIED"),
            "wrong {name} must be constrained by the bridge/parsed byte shape:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&tampered);
    }

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn certify_mutual_recursion_scc_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify mutual-recursion test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // A two-member SCC (`isEven`/`isOdd`) and a three-member cycle
    // (`rotA -> rotB -> rotC -> rotA`), so the ONE shared conjunction proof — its
    // `induction fuel`, its k-way destructuring and its `.2.….1` conjunct
    // projection — is exercised at both k = 2 and k = 3. `primary` is the
    // lowest-`self_idx` member whose `{primary}_mutual_sim` carries the proof.
    let cases: [(&str, &[&str], &str); 2] = [
        (
            "tools/certkit/fixtures/mutual.av",
            &["isEven", "isOdd"],
            "isEven",
        ),
        (
            "tools/certkit/fixtures/mutual3.av",
            &["rotA", "rotB", "rotC"],
            "rotA",
        ),
    ];

    for (fixture, exports, primary) in cases {
        let out_dir = temp_dir("certify-mutual");
        let compile = aver_command()
            .current_dir(&repo_root)
            .arg("compile")
            .arg(fixture)
            .arg("--target")
            .arg("wasm-gc")
            .arg("--certify")
            .arg("-o")
            .arg(&out_dir)
            .output()
            .expect("expected `aver compile --certify` to run");
        assert!(
            compile.status.success(),
            "compile --certify {fixture} failed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        );

        let cert_dir = out_dir.join("cert");
        let manifest: serde_json::Value = serde_json::from_str(
            &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
                .expect("cert-manifest.json exists"),
        )
        .expect("manifest is valid JSON");
        let certified: Vec<&str> = manifest["certified"]
            .as_array()
            .unwrap()
            .iter()
            .map(|c| c["name"].as_str().unwrap())
            .collect();
        // Every member of the SCC is a certified export sharing one proof.
        for name in exports {
            assert!(
                certified.contains(name),
                "expected {name} certified for {fixture}, got {certified:?}"
            );
        }
        for entry in manifest["certified"].as_array().unwrap() {
            if exports.contains(&entry["name"].as_str().unwrap()) {
                assert_eq!(entry["policy"], "simulatesModelTotally");
                assert_eq!(entry["level"], "L3");
                assert_eq!(entry["termination_witness"]["measure"]["kind"], "intNatAbs");
                assert_eq!(entry["termination_witness"]["descent"], -1);
            }
        }

        let build = Command::new("lake")
            .current_dir(&cert_dir)
            .arg("build")
            .output()
            .expect("expected `lake build` to run");
        let combined = format!(
            "{}{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );
        assert!(
            build.status.success(),
            "lake build of emitted mutual cert {fixture} failed:\n{combined}"
        );
        // The whole SCC shares ONE simulation proof, `{primary}_mutual_sim`,
        // kernel-clean on the core whitelist.
        assert!(
            combined.contains(&format!(
                "{primary}_mutual_sim' depends on axioms: [propext, Classical.choice, Quot.sound]"
            )),
            "mutual certificate for {fixture} not kernel-clean:\n{combined}"
        );
        assert!(
            combined.contains(&format!(
                "{primary}_mutual_total' depends on axioms: [propext, Classical.choice, Quot.sound]"
            )),
            "mutual total certificate for {fixture} not kernel-clean:\n{combined}"
        );
        assert!(
            !combined.contains("sorryAx"),
            "mutual certificate {fixture} leaked sorryAx:\n{combined}"
        );

        let _ = std::fs::remove_dir_all(&out_dir);
    }
}

#[test]
fn certify_verbatim_variant_dispatch_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify verbatim-variant-dispatch test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-strdispatch");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/strdispatch.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    let certified: Vec<&str> = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c["name"].as_str().unwrap())
        .collect();
    // A match whose every arm is a distinct String literal is certified as a
    // verbatim variant dispatch (`Cod := WVal`, `verbatimRepr`) over the
    // byte-exact data-segment constants — no new representation, no schema change.
    assert!(
        certified.contains(&"tagName"),
        "expected tagName certified, got {certified:?}"
    );
    let tag_name = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"] == "tagName")
        .unwrap();
    assert_eq!(
        tag_name["theorem"],
        "V3Master.verbatim_canonical_discharges"
    );
    let final_lean =
        std::fs::read_to_string(cert_dir.join("Final.lean")).expect("Final.lean exists");
    assert!(
        final_lean.contains("V3Master.verbatim_canonical_discharges \"tagName\""),
        "verbatim Final.cert arm must use the audited generic:\n{final_lean}"
    );
    let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
        .expect("Certificate.lean exists");
    assert!(
        !certificate.contains("tagName_wasm_certified")
            && !certificate.contains("tagName_simulates"),
        "verbatim dispatch must not emit bespoke proofs:\n{certificate}"
    );

    let build = Command::new("lake")
        .current_dir(&cert_dir)
        .arg("build")
        .output()
        .expect("expected `lake build` to run");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(
        build.status.success(),
        "lake build of emitted verbatim-variant-dispatch cert failed:\n{combined}"
    );
    assert!(
        combined.contains(
            "'AverCert.Final.cert' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "generic verbatim certificate not kernel-clean:\n{combined}"
    );
    assert!(
        !combined.contains("sorryAx"),
        "verbatim variant dispatch certificate leaked sorryAx:\n{combined}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn certify_string_eq_host_contract_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify String.eq host-contract test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-stringeq");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/stringeq.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    let certified: Vec<&str> = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c["name"].as_str().unwrap())
        .collect();
    assert!(
        certified.contains(&"quoteOrSelf"),
        "expected quoteOrSelf certified, got {certified:?}"
    );
    let contracts: Vec<&str> = manifest["runtime_contracts"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c.as_str().unwrap())
        .collect();
    assert!(
        contracts.contains(&aver::codegen::cert::STRING_EQ_CONTRACT),
        "String.eq host contract missing from manifest, got {contracts:?}"
    );
    let quote_class = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"].as_str() == Some("quoteOrSelf"))
        .and_then(|c| c["class"].as_str())
        .unwrap_or("<missing>");
    assert_eq!(
        quote_class, "verbatim-string-eq",
        "quoteOrSelf should render its inner class, got {quote_class}"
    );
    let quote_entry = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"].as_str() == Some("quoteOrSelf"))
        .expect("quoteOrSelf manifest entry exists");
    assert_eq!(
        quote_entry["theorem"],
        "V3Master.stringEq_canonical_discharges"
    );
    assert_eq!(
        quote_entry["source_fragment"]["profile"].as_str(),
        Some("sym-fragment-v1"),
        "quoteOrSelf should expose a source-level SymPlan sidecar"
    );
    assert_eq!(
        quote_entry["fragment"]["profile"].as_str(),
        Some("string-eq-v1"),
        "quoteOrSelf should expose a target-bound String.eq sidecar"
    );
    let plans_lean =
        std::fs::read_to_string(cert_dir.join("Plans.lean")).expect("Plans.lean exists");
    assert!(
        plans_lean.contains("def quoteOrSelfStringEqSymPlan : SymRawPlan"),
        "String.eq cert should render a source-level SymPlan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("def quoteOrSelfStringEqPlan : StringEqRawPlan"),
        "String.eq cert should render a Lean-data StringEqRawPlan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("stringEqPlanMatchesSymRawPlan")
            && plans_lean
                .contains("quoteOrSelfStringEqSymPlan quoteOrSelfStringEqPlan = true := rfl"),
        "String.eq SymPlan should be matched to the byte-bound equality plan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("checkStringEqRawPlan quoteOrSelfStringEqPlan = true := rfl"),
        "String.eq Lean plan should be checked by the Lean-side structural checker:\n{plans_lean}"
    );
    let manifest_lean =
        std::fs::read_to_string(cert_dir.join("Manifest.lean")).expect("Manifest.lean exists");
    assert!(
        manifest_lean.contains("(\"quoteOrSelf\", Plans.quoteOrSelfStringEqSymPlan)"),
        "manifest should pin the String.eq source SymPlan list:\n{manifest_lean}"
    );
    assert!(
        manifest_lean
            .contains("stringEqPlans := [(\"quoteOrSelf\", Plans.quoteOrSelfStringEqPlan)]"),
        "manifest should pin the String.eq plan list:\n{manifest_lean}"
    );
    let artifact_lean =
        std::fs::read_to_string(cert_dir.join("Artifact.lean")).expect("Artifact.lean exists");
    assert!(
        artifact_lean.contains("def stringEqClaims : List AverCert.AcceptedArtifact.StringEqClaim"),
        "artifact should carry source-level String.eq claims:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("symPlan := AverCert.Plans.quoteOrSelfStringEqSymPlan"),
        "String.eq artifact claim should carry the source-level SymPlan:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("stringEqFuncIdx :=") && artifact_lean.contains("stringTy :="),
        "String.eq artifact claim should carry lowering indices:\n{artifact_lean}"
    );
    let final_lean =
        std::fs::read_to_string(cert_dir.join("Final.lean")).expect("Final.lean exists");
    assert!(
        final_lean.contains("V3Master.stringEq_canonical_discharges \"quoteOrSelf\""),
        "String.eq Final.cert arm must use the audited generic:\n{final_lean}"
    );
    let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
        .expect("Certificate.lean exists");
    assert!(
        !certificate.contains("quoteOrSelf_wasm_certified")
            && !certificate.contains("quoteOrSelf_simulates"),
        "String.eq must not emit bespoke proofs:\n{certificate}"
    );

    let build = Command::new("lake")
        .current_dir(&cert_dir)
        .arg("build")
        .output()
        .expect("expected `lake build` to run");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(
        build.status.success(),
        "lake build of emitted String.eq host-contract cert failed:\n{combined}"
    );
    assert!(
        combined.contains(
            "'AverCert.Final.cert' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "generic String.eq host-contract certificate not kernel-clean:\n{combined}"
    );
    assert!(
        !combined.contains("sorryAx"),
        "String.eq host-contract certificate leaked sorryAx:\n{combined}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn certify_string_concat_host_contract_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify String.concat host-contract test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-stringconcat");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/stringconcat.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    let certified: Vec<&str> = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c["name"].as_str().unwrap())
        .collect();
    assert!(
        certified.contains(&"shout"),
        "expected shout certified, got {certified:?}"
    );
    let contracts: Vec<&str> = manifest["runtime_contracts"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c.as_str().unwrap())
        .collect();
    assert!(
        contracts.contains(&aver::codegen::cert::STRING_CONCAT_CONTRACT),
        "String.concat host contract missing from manifest, got {contracts:?}"
    );
    let shout_entry = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"].as_str() == Some("shout"))
        .expect("shout manifest entry");
    assert_eq!(
        shout_entry["theorem"],
        "V3Master.stringConcat_canonical_discharges"
    );
    let shout_class = shout_entry["class"].as_str().unwrap_or("<missing>");
    assert_eq!(
        shout_class, "verbatim-string-concat",
        "shout should render its concat class, got {shout_class}"
    );
    let source_fragment = &shout_entry["source_fragment"];
    assert_eq!(
        source_fragment["profile"].as_str(),
        Some("sym-fragment-v1"),
        "shout should carry a source-level SymPlan sidecar"
    );
    let sym_plan = source_fragment["plan"]
        .as_str()
        .expect("shout source plan path");
    assert!(
        sym_plan.starts_with("fragments/") && sym_plan.ends_with(".sym-fragment-v1.plan"),
        "shout should point at a source SymPlan sidecar, got {sym_plan}"
    );
    let sym_plan_text = std::fs::read_to_string(cert_dir.join(sym_plan))
        .expect("String.concat SymPlan sidecar exists");
    assert!(
        sym_plan_text.starts_with(
            "aver.sym-fragment.plan.v1\nprofile sym-fragment-v1\nparams string\nresult string\n"
        ),
        "shout should also emit the source-level SymPlan sidecar:\n{sym_plan_text}"
    );
    assert!(
        sym_plan_text.contains("const.string hex=21")
            && sym_plan_text.contains("prim op=string.concat args=v0,v1"),
        "shout SymPlan sidecar should expose source string concat without target data indices:\n{sym_plan_text}"
    );
    let expected_sym_sha = aver::codegen::cert::sha256_hex(sym_plan_text.as_bytes());
    assert_eq!(
        source_fragment["plan_sha256"].as_str(),
        Some(expected_sym_sha.as_str()),
        "shout source sidecar hash should match the sidecar bytes"
    );

    let fragment = &shout_entry["fragment"];
    assert_eq!(
        fragment["profile"].as_str(),
        Some("string-concat-v1"),
        "shout should carry a byte-bound String.concat plan sidecar"
    );
    let plan = fragment["plan"].as_str().expect("shout plan path");
    assert!(
        plan.starts_with("fragments/") && plan.ends_with(".string-concat-v1.plan"),
        "shout should point at a string-concat sidecar, got {plan}"
    );
    let plan_text =
        std::fs::read_to_string(cert_dir.join(plan)).expect("String.concat sidecar exists");
    assert!(
        plan_text.starts_with(
            "aver.string-fragment.plan.v1\nprofile string-concat-v1\nparams string\nresult string\n"
        ),
        "shout sidecar should be the canonical string source plan:\n{plan_text}"
    );
    assert!(
        plan_text.contains("input index=0") && plan_text.contains("suffix data=0 hex=21"),
        "shout sidecar should preserve the input plus literal suffix:\n{plan_text}"
    );
    let expected_sha = aver::codegen::cert::sha256_hex(plan_text.as_bytes());
    assert_eq!(
        fragment["plan_sha256"].as_str(),
        Some(expected_sha.as_str()),
        "shout sidecar hash should match the sidecar bytes"
    );
    let plans_lean =
        std::fs::read_to_string(cert_dir.join("Plans.lean")).expect("Plans.lean exists");
    assert!(
        plans_lean.contains("def shoutStringConcatSymPlan : SymRawPlan"),
        "String.concat cert should render a source-level SymPlan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains(".constStringBytes [33]")
            && plans_lean.contains(".prim .stringConcat [0, 1]"),
        "String.concat SymPlan should expose source-level string concat:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("checkSymRawPlan shoutStringConcatSymPlan = true := rfl"),
        "String.concat SymPlan should be checked by the Lean-side source checker:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("stringConcatPlanMatchesSymRawPlan")
            && plans_lean.contains("shoutStringConcatSymPlan shoutStringConcatPlan = true := rfl"),
        "String.concat SymPlan should be matched to the byte-bound concat plan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("def shoutStringConcatPlan : StringConcatRawPlan"),
        "String.concat cert should render a Lean-data StringConcatRawPlan:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("suffixes := [({ dataIdx := 0, bytes := [33] } : StringConcatChunk)]"),
        "String.concat Lean plan should expose the literal suffix bytes and data segment binding:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("checkStringConcatRawPlan shoutStringConcatPlan = true := rfl"),
        "String.concat Lean plan should be checked by the Lean-side structural checker:\n{plans_lean}"
    );
    let manifest_lean =
        std::fs::read_to_string(cert_dir.join("Manifest.lean")).expect("Manifest.lean exists");
    assert!(
        manifest_lean.contains("(\"shout\", Plans.shoutStringConcatSymPlan)"),
        "manifest should pin the String.concat source SymPlan list:\n{manifest_lean}"
    );
    assert!(
        manifest_lean.contains("stringConcatPlans := [(\"shout\", Plans.shoutStringConcatPlan)]"),
        "manifest should pin the String.concat plan list:\n{manifest_lean}"
    );
    let artifact_lean =
        std::fs::read_to_string(cert_dir.join("Artifact.lean")).expect("Artifact.lean exists");
    assert!(
        artifact_lean
            .contains("def stringConcatClaims : List AverCert.AcceptedArtifact.StringConcatClaim"),
        "artifact should carry source-level String.concat claims:\n{artifact_lean}"
    );
    assert!(
        !artifact_lean.contains("plan := AverCert.Plans.shoutStringConcatPlan"),
        "String.concat artifact claim should not duplicate the target StringConcatRawPlan:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("symPlan := AverCert.Plans.shoutStringConcatSymPlan"),
        "String.concat artifact claim should carry the source-level SymPlan:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("concatFuncIdx :=") && artifact_lean.contains("resultTy :="),
        "String.concat artifact claim should carry lowering indices:\n{artifact_lean}"
    );
    let final_lean =
        std::fs::read_to_string(cert_dir.join("Final.lean")).expect("Final.lean exists");
    assert!(
        final_lean.contains("V3Master.stringConcat_canonical_discharges \"shout\""),
        "String.concat Final.cert arm must use the audited generic:\n{final_lean}"
    );
    let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
        .expect("Certificate.lean exists");
    assert!(
        !certificate.contains("shout_wasm_certified") && !certificate.contains("shout_simulates"),
        "String.concat must not emit bespoke proofs:\n{certificate}"
    );

    let build = Command::new("lake")
        .current_dir(&cert_dir)
        .arg("build")
        .output()
        .expect("expected `lake build` to run");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(
        build.status.success(),
        "lake build of emitted String.concat host-contract cert failed:\n{combined}"
    );
    assert!(
        combined.contains(
            "'AverCert.Final.cert' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "generic String.concat host-contract certificate not kernel-clean:\n{combined}"
    );
    assert!(
        !combined.contains("sorryAx"),
        "String.concat host-contract certificate leaked sorryAx:\n{combined}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn certify_composition_fixture_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify composition test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-compose");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/compose.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    // `quad` calls `double` twice: the cross-function composition class carries
    // the whole call closure in one shared code table and cites the callee.
    let certified: Vec<&str> = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c["name"].as_str().unwrap())
        .collect();
    assert!(
        certified.contains(&"quad"),
        "expected quad certified (composition), got {certified:?}"
    );
    // A chain calling a chain (hex16 -> quad -> double) must certify through
    // the same shared table — this is the nested-composition coverage the
    // review asked to lock in.
    assert!(
        certified.contains(&"hex16"),
        "expected hex16 certified (nested composition), got {certified:?}"
    );
    let class = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"] == "quad")
        .and_then(|c| c["class"].as_str())
        .unwrap_or("");
    assert_eq!(class, "cross-function-composition", "wrong class for quad");
    let build = Command::new("lake")
        .current_dir(&cert_dir)
        .arg("build")
        .output()
        .expect("expected `lake build` to run");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(
        build.status.success(),
        "lake build of emitted composition cert failed:\n{combined}"
    );
    // Kernel-clean: the caller theorem cites its callee's simulation lemma and
    // stays on the core whitelist; no `sorryAx` leaks through the composition.
    assert!(
        combined.contains(
            "quad_wasm_certified' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "composition certificate theorem not kernel-clean:\n{combined}"
    );
    assert!(
        !combined.contains("sorryAx"),
        "composition certificate leaked sorryAx:\n{combined}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn certify_nonrecursive_adt_witnesses_lake_build_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify ADT test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    let cases = [
        (
            "tools/certkit/fixtures/opteval.av",
            "opteval",
            vec!["mk", "eval"],
        ),
        ("examples/core/user_record.av", "user-record", vec!["greet"]),
        (
            "tools/certkit/fixtures/tupleproj.av",
            "tuple-proj",
            vec!["pairFst", "pairSnd"],
        ),
        (
            "tools/certkit/fixtures/widenedmatch.av",
            "widened-match",
            vec!["boxInt"],
        ),
        (
            "tools/certkit/fixtures/rangepred.av",
            "range-pred",
            vec!["inAsciiDigit"],
        ),
        (
            "tools/certkit/fixtures/verbatimwiden.av",
            "verbatim-widen",
            vec!["wrapItems"],
        ),
        (
            "tools/certkit/fixtures/f64verbatim.av",
            "f64-verbatim",
            vec!["floatOrZero"],
        ),
        // Out-of-template variant dispatch: four constructors, mixed arm
        // semantics (negation, offset addition, identity, non-zero default) —
        // provable only through the structural walker, not a shape template.
        (
            "tools/certkit/fixtures/signalgauge.av",
            "signal-gauge",
            vec!["gauge"],
        ),
        (
            "tools/certkit/fixtures/intdispatchgen.av",
            "int-dispatch-gen",
            vec!["boxInt", "gauge"],
        ),
        // Payload-first subtraction, constant-first addition, and payload
        // variants elided into the wildcard default.
        ("tools/certkit/fixtures/meter.av", "meter", vec!["readout"]),
    ];

    for (input, prefix, expected) in cases {
        let out_dir = temp_dir(prefix);
        let compile = aver_command()
            .current_dir(&repo_root)
            .arg("compile")
            .arg(input)
            .arg("--target")
            .arg("wasm-gc")
            .arg("--certify")
            .arg("-o")
            .arg(&out_dir)
            .output()
            .expect("expected `aver compile --certify` to run");
        assert!(
            compile.status.success(),
            "compile --certify failed for {input}:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        );

        let cert_dir = out_dir.join("cert");
        let manifest: serde_json::Value = serde_json::from_str(
            &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
                .expect("cert-manifest.json exists"),
        )
        .expect("manifest is valid JSON");
        let certified: Vec<&str> = manifest["certified"]
            .as_array()
            .unwrap()
            .iter()
            .map(|c| c["name"].as_str().unwrap())
            .collect();
        for &name in &expected {
            assert!(
                certified.contains(&name),
                "expected {name} certified for {input}, got {certified:?}"
            );
        }
        let dispatch_entries = manifest["certified"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|entry| {
                matches!(
                    entry["class"].as_str(),
                    Some("variant-dispatch" | "widened-int-match")
                )
            })
            .collect::<Vec<_>>();
        let model_construct_entries = manifest["certified"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|entry| {
                entry["class"] == "adt-constructor"
                    && entry["theorem"] == "V3Master.construct_canonical_discharges"
            })
            .collect::<Vec<_>>();
        if !model_construct_entries.is_empty() {
            let final_lean =
                std::fs::read_to_string(cert_dir.join("Final.lean")).expect("Final.lean exists");
            let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
                .expect("Certificate.lean exists");
            for entry in &model_construct_entries {
                let name = entry["name"].as_str().unwrap();
                assert!(
                    final_lean.contains(&format!(
                        "V3Master.construct_canonical_discharges (exportName := \"{name}\")"
                    )) && final_lean.contains(&format!(
                        "(hSemantic := CertProofs.{name}_constructSemanticBridge)"
                    )),
                    "construct-with-model Final.cert arm must pass the audited discharge and bridge for {name}:\n{final_lean}"
                );
                assert!(
                    certificate.contains(&format!("theorem {name}_constructSemanticBridge"))
                        && !certificate.contains(&format!("{name}_wasm_certified"))
                        && !certificate.contains(&format!("{name}_simulates")),
                    "construct-with-model must emit only its option-(b) bridge for {name}:\n{certificate}"
                );
            }
        }
        if !dispatch_entries.is_empty() {
            let final_lean =
                std::fs::read_to_string(cert_dir.join("Final.lean")).expect("Final.lean exists");
            let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
                .expect("Certificate.lean exists");
            for entry in &dispatch_entries {
                let name = entry["name"].as_str().unwrap();
                assert_eq!(
                    entry["theorem"],
                    "V3Master.intDispatch_canonical_discharges"
                );
                assert!(
                    final_lean.contains(&format!(
                        "V3Master.intDispatch_canonical_discharges (exportName := \"{name}\")"
                    )) && final_lean.contains("(hRoot := by exact ⟨"),
                    "dispatch Final.cert arm must pass the audited discharge and root witness for {name}:\n{final_lean}"
                );
                assert!(
                    certificate.contains(&format!("theorem {name}_intDispatchSemanticBridge"))
                        && !certificate.contains(&format!("{name}_wasm_certified"))
                        && !certificate.contains(&format!("{name}_simulates")),
                    "dispatch must emit only its option-(b) bridge for {name}:\n{certificate}"
                );
            }
        }
        if prefix == "tuple-proj" {
            let entries = manifest["certified"].as_array().unwrap();
            for name in ["pairFst", "pairSnd"] {
                let entry = entries.iter().find(|entry| entry["name"] == name).unwrap();
                assert_eq!(
                    entry["theorem"], "V3Master.fieldProjection_canonical_discharges",
                    "projection metadata must name the audited generic leaf theorem"
                );
            }
            let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
                .expect("Certificate.lean exists");
            assert!(
                !certificate.contains("pairFst_wasm_certified")
                    && !certificate.contains("pairFst_simulates")
                    && !certificate.contains("pairSnd_wasm_certified")
                    && !certificate.contains("pairSnd_simulates"),
                "field projections must not emit bespoke proofs:\n{certificate}"
            );
            let final_lean =
                std::fs::read_to_string(cert_dir.join("Final.lean")).expect("Final.lean exists");
            for name in ["pairFst", "pairSnd"] {
                assert!(
                    final_lean.contains(&format!(
                        "V3Master.fieldProjection_canonical_discharges \"{name}\""
                    )),
                    "Final.cert must use the audited generic for {name}:\n{final_lean}"
                );
            }
        }
        let build = Command::new("lake")
            .current_dir(&cert_dir)
            .arg("build")
            .output()
            .expect("expected `lake build` to run");
        let combined = format!(
            "{}{}",
            String::from_utf8_lossy(&build.stdout),
            String::from_utf8_lossy(&build.stderr)
        );
        assert!(
            build.status.success(),
            "lake build of emitted ADT cert failed for {input}:\n{combined}"
        );
        assert!(
            !combined.contains("sorryAx"),
            "ADT certificate leaked sorryAx for {input}:\n{combined}"
        );
        if !model_construct_entries.is_empty() {
            assert!(
                combined.contains(
                    "'V3Master.construct_canonical_discharges' depends on axioms: [propext, Classical.choice, Quot.sound]"
                ),
                "audited construct discharge changed axiom surface:\n{combined}"
            );
            for entry in &model_construct_entries {
                let name = entry["name"].as_str().unwrap();
                assert!(
                    combined.contains(&format!(
                        "'CertProofs.{name}_constructSemanticBridge' depends on axioms: [propext, Quot.sound]"
                    )),
                    "construct source-model bridge changed axiom surface for {name}:\n{combined}"
                );
            }
        }
        if prefix == "tuple-proj"
            || !dispatch_entries.is_empty()
            || !model_construct_entries.is_empty()
        {
            assert!(
                combined.contains(
                    "'AverCert.Final.cert' depends on axioms: [propext, Classical.choice, Quot.sound]"
                ),
                "generic field-projection/dispatch/construct holds proofs changed axiom surface:\n{combined}"
            );
        }

        let _ = std::fs::remove_dir_all(&out_dir);
    }
}

/// The s33 heap-type boundary: 16 nominal sum roots plus 46 user variant
/// structs push the Int carrier to wasm type index 64, the first index whose
/// signed s33 encoding (`c0 00`)
/// differs from unsigned LEB (`40`). The recursion plan claim binds the
/// carrier index inside local declarations, the value-if block type and the
/// declared function type, so a lowerer that emitted unsigned LEB would fail
/// its own byte-equality examples here.
#[test]
fn certify_carrier_at_type_index_64_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping s33 boundary test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-manytypes");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/manytypes.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    assert_eq!(
        manifest["carrier_type_index"].as_u64(),
        Some(64),
        "fixture must pin the carrier exactly at the s33 boundary index 64; \
         adjust the fixture's variant count if the emitter's type layout changed"
    );
    let sum_big = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"] == "sumBig")
        .unwrap();
    assert_eq!(sum_big["policy"], "simulatesModelTotally");
    assert_eq!(sum_big["level"], "L3");
    assert_eq!(sum_big["theorem"], "V3Master.recursion_claim_discharges");
    let build = Command::new("lake")
        .current_dir(&cert_dir)
        .arg("build")
        .output()
        .expect("expected `lake build` to run");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(
        build.status.success(),
        "lake build of the s33 boundary cert failed:\n{combined}"
    );
    assert!(
        combined.contains(
            "'CertProofs.sumBig_recursionSemanticBridge' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ) && combined.contains(
            "'V3Master.recursion_claim_discharges' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ) && combined.contains(
            "'AverCert.Final.cert' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "boundary certificate not kernel-clean:\n{combined}"
    );
    assert!(
        !combined.contains("sorryAx"),
        "boundary certificate leaked sorryAx:\n{combined}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}
