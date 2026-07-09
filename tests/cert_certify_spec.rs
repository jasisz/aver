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

#[test]
fn certify_goal_matrix_manifest_tracks_current_surface() {
    // This fixture is the dashboard for "how much do we certify now?". Larger
    // programs such as examples/data/json.av remain integration side-effects;
    // this test pins the planned numerator/denominator directly. When a backlog
    // goal becomes certifiable, move it from `expected_backlog` into `expected`.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-goals");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
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
    let expected_plan_check_sha = aver::codegen::cert::audited_plan_check_sha();
    let expected_plan_lower_sha = aver::codegen::cert::audited_plan_lower_sha();
    let expected_plan_bytes_sha = aver::codegen::cert::audited_plan_bytes_sha();
    let expected_wasm_slice_sha = aver::codegen::cert::audited_wasm_slice_sha();
    let expected_expr_fragment_accepted_sha =
        aver::codegen::cert::audited_expr_fragment_accepted_sha();
    let expected_accepted_artifact_sha = aver::codegen::cert::audited_accepted_artifact_sha();
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
        manifest["artifact_certificate_root"].as_str(),
        Some(aver::codegen::cert::ARTIFACT_CERTIFICATE_ROOT),
        "manifest should expose the artifact-level certificate root"
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
        ("addTwo", "straight-line"),
        ("sumFrom", "self-recursive"),
        ("countDown", "multi-argument self-recursive"),
        ("quad", "cross-function-composition"),
        ("hex16", "cross-function-composition"),
        ("isEven", "mutual-recursive"),
        ("isOdd", "mutual-recursive"),
        ("mkOp", "adt-constructor"),
        ("evalOp", "variant-dispatch"),
        ("userName", "field-projection"),
        ("boxInt", "widened-int-match"),
        ("wrapItems", "verbatim-widened-match"),
        ("tagName", "verbatim-variant-dispatch"),
        ("gauge", "variant-dispatch"),
        ("inAsciiDigit", "expr-fragment-v1"),
        ("quoteOrSelf", "verbatim-widened-match"),
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
        7,
        "expr-fragment sidecar count changed; update this deliberately"
    );
    let mut sym_sidecar_names = BTreeSet::new();
    let mut expr_sidecar_names = BTreeSet::new();
    for entry in expr_entries {
        let name = entry["name"].as_str().unwrap();
        let fragment = &entry["fragment"];
        let profile = fragment["profile"]
            .as_str()
            .expect("fragment profile string");
        let plan = fragment["plan"].as_str().expect("fragment plan path");
        let (suffix, header) = match profile {
            "sym-fragment-v1" => {
                sym_sidecar_names.insert(name.to_string());
                (
                    ".sym-fragment-v1.plan",
                    "aver.sym-fragment.plan.v1\nprofile sym-fragment-v1\n",
                )
            }
            "expr-fragment-v1" => {
                expr_sidecar_names.insert(name.to_string());
                (
                    ".expr-fragment-v1.plan",
                    "aver.expr-fragment.plan.v1\nprofile expr-fragment-v1\n",
                )
            }
            other => panic!("{name} should pin a known fragment profile, got {other}"),
        };
        assert!(
            plan.starts_with("fragments/") && plan.ends_with(suffix),
            "{name} should point at a fragment plan sidecar, got {plan}"
        );
        assert!(
            !plan.contains(".."),
            "{name} sidecar path must stay inside cert/fragments"
        );
        let text = std::fs::read_to_string(out_dir.join("cert").join(plan))
            .expect("fragment sidecar exists");
        assert!(
            text.starts_with(header),
            "{name} sidecar should be the canonical {profile} plan:\n{text}"
        );
        let expected_sha = aver::codegen::cert::sha256_hex(text.as_bytes());
        assert_eq!(
            fragment["plan_sha256"].as_str(),
            Some(expected_sha.as_str()),
            "{name} sidecar hash should match the sidecar bytes"
        );
        assert!(
            fragment.get("trace").is_none() && fragment.get("trace_sha256").is_none(),
            "{name} should not emit trace/replay sidecars after plan-first lowering"
        );
    }
    assert!(
        sym_sidecar_names.contains("floatAddGoal")
            && sym_sidecar_names.contains("floatMulAddGoal")
            && sym_sidecar_names.contains("floatLeGoal")
            && sym_sidecar_names.contains("boolAndGoal")
            && sym_sidecar_names.contains("intLessZero")
            && sym_sidecar_names.contains("intEqZero")
            && sym_sidecar_names.contains("inAsciiDigit"),
        "direct source-level fragments should prefer sym sidecars, got {sym_sidecar_names:?}"
    );
    assert!(
        expr_sidecar_names.is_empty(),
        "all current expr fragments should now have source-level SymPlan sidecars, got fallback expr sidecars {expr_sidecar_names:?}"
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
        plans_lean.contains("encodeSymRawPlanToExprFragmentRawPlan floatAddGoalSymPlan"),
        "direct SymPlan projection should encode to the byte-bound ExprFragment plan:\n{plans_lean}"
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
        artifact_lean
            .contains("def exprFragmentClaims : List AverCert.AcceptedArtifact.ExprFragmentClaim"),
        "artifact should keep a representation-level fallback claim list:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("plan := AverCert.Plans.intLessZeroSymPlan"),
        "source-level int fragment should be claimed through SymPlan:\n{artifact_lean}"
    );
    assert!(
        !artifact_lean.contains("plan := AverCert.Plans.intLessZeroPlan"),
        "source-level int fragment should not carry a duplicate ExprFragmentClaim:\n{artifact_lean}"
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
        "listHeadGoal",
        "sumListGoal",
    ]
    .into_iter()
    .map(str::to_string)
    .collect();
    let expected_backlog: BTreeSet<String> = ["listHeadGoal", "sumListGoal"]
        .into_iter()
        .map(str::to_string)
        .collect();
    assert_eq!(planned_goal_names.len(), 25, "goal denominator changed");
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
fn certify_straight_line_fixture_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
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
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
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
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
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
    // One fuel-induction arm covers: a non-zero base (`sumFrom`), a constant
    // combinator operand (`constPlus`), a reversed operand order (`backward`),
    // and the two-argument tail accumulator (`countDown`) — none of which the
    // old fixed-shape recursion templates admitted.
    for name in ["sumFrom", "constPlus", "backward", "factorial", "countDown"] {
        assert!(
            certified.contains(&name),
            "expected {name} certified, got {certified:?}"
        );
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
        "lake build of emitted recursion cert failed:\n{combined}"
    );
    // Kernel-clean on every recognised shape, including the ones the previous
    // templates could not express.
    for name in ["sumFrom", "constPlus", "backward", "factorial", "countDown"] {
        assert!(
            combined.contains(&format!(
                "{name}_wasm_certified' depends on axioms: [propext, Classical.choice, Quot.sound]"
            )),
            "recursion certificate for {name} not kernel-clean:\n{combined}"
        );
    }
    assert!(
        !combined.contains("sorryAx"),
        "recursion certificate leaked sorryAx:\n{combined}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn certify_mutual_recursion_scc_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify mutual-recursion test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");

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
        let compile = Command::new(aver_bin)
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
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
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
            "tagName_wasm_certified' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "verbatim variant dispatch certificate not kernel-clean:\n{combined}"
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
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
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
        quote_class, "verbatim-widened-match",
        "quoteOrSelf should render its inner class, got {quote_class}"
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
            "quoteOrSelf_wasm_certified' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "String.eq host-contract certificate not kernel-clean:\n{combined}"
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
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
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
    let shout_class = shout_entry["class"].as_str().unwrap_or("<missing>");
    assert_eq!(
        shout_class, "verbatim-string-concat",
        "shout should render its concat class, got {shout_class}"
    );
    let fragment = &shout_entry["fragment"];
    assert_eq!(
        fragment["profile"].as_str(),
        Some("string-concat-v1"),
        "shout should carry a source-level String.concat plan sidecar"
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
    let sym_plan = "fragments/73686f7574.sym-fragment-v1.plan";
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
        artifact_lean.contains("plan := AverCert.Plans.shoutStringConcatPlan"),
        "String.concat artifact claim should point at the checked source plan:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("symPlan := AverCert.Plans.shoutStringConcatSymPlan"),
        "String.concat artifact claim should carry the source-level SymPlan:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("concatFuncIdx :=") && artifact_lean.contains("resultTy :="),
        "String.concat artifact claim should carry lowering indices:\n{artifact_lean}"
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
            "shout_wasm_certified' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "String.concat host-contract certificate not kernel-clean:\n{combined}"
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
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
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
    let aver_bin = env!("CARGO_BIN_EXE_aver");

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
        // Out-of-template variant dispatch: four constructors, mixed arm
        // semantics (negation, offset addition, identity, non-zero default) —
        // provable only through the structural walker, not a shape template.
        (
            "tools/certkit/fixtures/signalgauge.av",
            "signal-gauge",
            vec!["gauge"],
        ),
        // Payload-first subtraction, constant-first addition, and payload
        // variants elided into the wildcard default.
        ("tools/certkit/fixtures/meter.av", "meter", vec!["readout"]),
    ];

    for (input, prefix, expected) in cases {
        let out_dir = temp_dir(prefix);
        let compile = Command::new(aver_bin)
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
        for name in expected {
            assert!(
                certified.contains(&name),
                "expected {name} certified for {input}, got {certified:?}"
            );
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

        let _ = std::fs::remove_dir_all(&out_dir);
    }
}
