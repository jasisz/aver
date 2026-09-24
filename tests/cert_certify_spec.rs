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

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;

use aver_cmd::aver_command;

#[path = "support/cert_wall.rs"]
mod cert_wall;
#[path = "support/scratch_dir.rs"]
mod scratch_dir;

use cert_wall::materialize as materialize_wall;
use scratch_dir::{ScratchDir, temp_dir};
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::process::Command;

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

fn check_certificate(wasm: &std::path::Path, cert_dir: &std::path::Path) -> (bool, String) {
    let output = aver_command()
        .arg("cert")
        .arg("check")
        .arg(wasm)
        .arg(cert_dir)
        .output()
        .expect("expected `aver cert check` to run");
    (
        output.status.success(),
        format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ),
    )
}

/// `lake build` the package's acceptance root (`ArtifactCertificate`: the
/// artifact data, every byte fact and the final theorem) in isolation, and
/// assert it succeeds with both roots on the core axiom whitelist.
fn assert_certificate_target_builds(cert_dir: &std::path::Path, case: &str) {
    materialize_wall(cert_dir);
    let output = Command::new("lake")
        .current_dir(cert_dir)
        .args(["build", "ArtifactCertificate"])
        .output()
        .expect("lake builds the isolated ArtifactCertificate target");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        output.status.success(),
        "honest ArtifactCertificate target must build ({case}):\n{combined}"
    );
    for root in ["AverCert.Final.cert", "AverCert.Artifact.certificate"] {
        assert!(
            combined.contains(&format!(
                "'{root}' depends on axioms: [propext, Classical.choice, Quot.sound]"
            )),
            "{root} must stay on the core axiom whitelist ({case}):\n{combined}"
        );
    }
    assert!(
        !combined.contains("sorryAx"),
        "the acceptance root leaked sorryAx ({case}):\n{combined}"
    );
    trim_lean_build_tree(cert_dir);
}

/// Removes the `.lake` build tree from an emitted certificate package once a
/// `lake` step there has succeeded.
///
/// The build tree is ~117 MB of the ~119 MB a certificate test writes, and it
/// is dead weight as soon as the build's output has been captured: every later
/// step reads emitted package files or stages verification in a fresh
/// directory. `ScratchDir` already removes everything on drop; this early trim
/// is for the run that never reaches a drop — a killed process then strands
/// the ~2 MB package instead of the whole build tree — and it keeps
/// `copy_dir_all` from duplicating the build tree into each tampered copy.
fn trim_lean_build_tree(cert_dir: &std::path::Path) {
    let _ = std::fs::remove_dir_all(cert_dir.join(".lake"));
}

fn assert_plans_lean_is_the_only_public_plan_data(
    cert_dir: &std::path::Path,
    manifest: &serde_json::Value,
) {
    assert!(
        cert_dir.join("Plans.lean").is_file(),
        "Plans.lean must be the package's authoritative plan DATA"
    );
    // DATA and nothing else. The anonymous `example`s that used to restate each
    // plan's passage through the audited checkers, lowerers and byte slicer are
    // redundant with the acceptance predicates, and `Plans.lean` — unlike
    // `Artifact.lean` and `Certificate.lean` — raises no heartbeat budget, so on
    // a large module they were the only declarations that could fail the build.
    let plans_lean =
        std::fs::read_to_string(cert_dir.join("Plans.lean")).expect("Plans.lean exists");
    assert!(
        !plans_lean.contains("\nexample "),
        "Plans.lean must carry plan DATA and no example declarations:\n{plans_lean}"
    );
    assert!(
        !cert_dir.join("ArtifactBytes.lean").exists(),
        "ArtifactBytes.lean is checker-generated from Wasm, not public package DATA"
    );
    assert!(
        !cert_dir.join("ArtifactComponentBytes.lean").exists(),
        "ArtifactComponentBytes.lean is checker-generated from the delivered artifact, not public package DATA"
    );
    assert!(
        !cert_dir.join("fragments").exists(),
        "the public package must not duplicate Plans.lean as fragment sidecars"
    );

    for entry in manifest["certified"]
        .as_array()
        .expect("certified report is an array")
    {
        let name = entry["name"].as_str().unwrap_or("<missing>");
        let fields = entry
            .as_object()
            .unwrap_or_else(|| panic!("certified report entry for {name} is an object"));
        for removed in ["source_fragment", "fragment", "plan_sha256"] {
            assert!(
                !fields.contains_key(removed),
                "{name} must not expose removed public plan metadata `{removed}`"
            );
        }
        assert!(
            fields.keys().all(|field| matches!(
                field.as_str(),
                "name"
                    | "class"
                    | "facets"
                    | "policy"
                    | "level"
                    | "theorem"
                    | "termination_witness"
            )),
            "{name} manifest entry must remain envelope/report metadata only: {fields:?}"
        );
    }
}

/// The function index `wasmparser` reads for the function export `name`.
fn wasm_export_index(bytes: &[u8], name: &str) -> Option<u32> {
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        if let Ok(wasmparser::Payload::ExportSection(reader)) = payload {
            for export in reader {
                let export = export.expect("export parses");
                if export.name == name && export.kind == wasmparser::ExternalKind::Func {
                    return Some(export.index);
                }
            }
        }
    }
    None
}

/// Compile `fixture` (a path under the repository root, with any extra
/// compiler arguments) with `--certify` and return the scratch directory and
/// the parsed public manifest.
fn certify_fixture(fixture: &str, extra: &[&str], prefix: &str) -> (ScratchDir, serde_json::Value) {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir(prefix);
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg(fixture)
        .args(extra)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify {fixture} failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let manifest = serde_json::from_str(
        &std::fs::read_to_string(out_dir.join("cert").join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    (out_dir, manifest)
}

/// The certified entry named `name` in a public manifest.
fn certified_entry<'m>(manifest: &'m serde_json::Value, name: &str) -> &'m serde_json::Value {
    manifest["certified"]
        .as_array()
        .expect("certified report is an array")
        .iter()
        .find(|entry| entry["name"] == name)
        .unwrap_or_else(|| panic!("`{name}` must be certified: {manifest:#}"))
}

/// Assert `name` is certified with the one plan class, the given facets and
/// the given policy (`true` for the total, L3 policy with the canonical
/// termination witness).
fn assert_certified_as(manifest: &serde_json::Value, name: &str, facets: &[&str], total: bool) {
    let entry = certified_entry(manifest, name);
    assert_eq!(entry["class"], "source-plan-v1", "{name}: {entry}");
    assert_eq!(
        entry["theorem"], "AcceptanceSoundness.fn_claim_discharges",
        "{name}: {entry}"
    );
    assert_eq!(
        entry["facets"],
        serde_json::json!(facets),
        "{name}: {entry}"
    );
    if total {
        assert_eq!(entry["policy"], "simulatesModelTotally", "{name}: {entry}");
        assert_eq!(entry["level"], "L3", "{name}: {entry}");
        assert_eq!(entry["termination_witness"]["measure"]["kind"], "intNatAbs");
        assert_eq!(entry["termination_witness"]["measure"]["param_index"], 0);
        assert_eq!(entry["termination_witness"]["descent"], -1);
    } else {
        assert_eq!(entry["policy"], "simulatesModel", "{name}: {entry}");
        assert_eq!(entry["level"], "L1", "{name}: {entry}");
        assert!(
            entry.get("termination_witness").is_none(),
            "{name}: {entry}"
        );
    }
}

/// The plan block `def fnN : FnPlan := …` of export `name` in `Plans.lean`.
fn export_plan_block(plans_lean: &str, name: &str) -> String {
    let head = format!("⟨\"{name}\", ");
    let at = plans_lean
        .find(&head)
        .unwrap_or_else(|| panic!("Plans.lean has no fnPlans entry for `{name}`"));
    let entry_end = plans_lean[at..].find('⟩').unwrap() + at;
    let def = plans_lean[at..entry_end]
        .rsplit(',')
        .next()
        .unwrap()
        .trim()
        .to_string();
    let def_head = format!("def {def} : FnPlan :=");
    let start = plans_lean
        .find(&def_head)
        .unwrap_or_else(|| panic!("Plans.lean has no `{def_head}`"));
    let end = plans_lean[start..].find("\n\n").unwrap() + start;
    plans_lean[start..end].to_string()
}

/// Build the package's acceptance root, then elaborate `probe` (a Lean
/// source importing `ArtifactCertificate`) against it with `lake env lean`,
/// and assert it elaborates. The probe states facts about the package as
/// `example`s the kernel decides, so a wrong fact is an elaboration error.
fn assert_kernel_probe_holds(cert_dir: &std::path::Path, probe: &str, case: &str) {
    materialize_wall(cert_dir);
    let build = Command::new("lake")
        .current_dir(cert_dir)
        .args(["build", "ArtifactCertificate"])
        .output()
        .expect("lake builds the acceptance root");
    assert!(
        build.status.success(),
        "the acceptance root must build before the {case} probe:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    std::fs::write(cert_dir.join("KernelProbe.lean"), probe).unwrap();
    let check = Command::new("lake")
        .current_dir(cert_dir)
        .args(["env", "lean", "KernelProbe.lean"])
        .output()
        .expect("lake env lean runs the probe");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    assert!(
        check.status.success() && !combined.contains("error"),
        "the {case} kernel probe must elaborate:\n{combined}"
    );
    trim_lean_build_tree(cert_dir);
}

fn instantiate_float_probe(
    wasm: &[u8],
    canonicalize_nans: bool,
) -> (wasmtime::Store<()>, wasmtime::Instance) {
    let mut config = wasmtime::Config::new();
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    config.cranelift_nan_canonicalization(canonicalize_nans);
    config.max_wasm_stack(8 * 1024 * 1024);
    config.async_stack_size(12 * 1024 * 1024);
    let engine = wasmtime::Engine::new(&config).expect("Wasmtime Float probe engine");
    let module = wasmtime::Module::new(&engine, wasm).expect("generated cert goals Wasm");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[])
        .expect("instantiate generated cert goals Wasm");
    (store, instance)
}

fn is_arithmetic_nan_bits(bits: u64) -> bool {
    bits & 0x7ff0_0000_0000_0000 == 0x7ff0_0000_0000_0000 && bits & 0x0008_0000_0000_0000 != 0
}

/// A scratch directory survives its test only as long as the test's scope.
///
/// Each certificate test writes about 119 MB under its scratch directory, most
/// of it the `cert/.lake` build tree, and a failing certificate test is the one
/// a developer re-runs. Cleanup therefore has to happen on the failing path
/// too, so it hangs off `Drop` rather than a trailing statement that unwinding
/// skips. The `create_dir_all` here is deliberate: it keeps the check from
/// passing vacuously against a helper that only names a directory.
#[test]
fn a_scratch_directory_is_removed_when_its_test_panics() {
    let recorded = std::sync::Mutex::new(PathBuf::new());
    let outcome = std::panic::catch_unwind(|| {
        let out_dir = temp_dir("certify-panic-cleanup");
        std::fs::create_dir_all(&out_dir).unwrap();
        *recorded.lock().unwrap() = out_dir.to_path_buf();
        std::fs::write(out_dir.join("cert-artifact"), "scratch\n").unwrap();
        panic!("stand-in for a failing certificate assertion");
    });

    assert!(outcome.is_err(), "the stand-in failure must unwind");
    let scratch = recorded.lock().unwrap().clone();
    assert!(
        !scratch.exists(),
        "a panicking certificate test must not leave {} behind",
        scratch.display()
    );
}

/// The trim after a successful Lean build must delete exactly the build tree:
/// `.lake` gone so a killed run strands ~2 MB instead of ~119 MB, and the
/// certificate package files still in place.
#[test]
fn trimming_the_lean_build_tree_keeps_the_certificate_package() {
    let out_dir = temp_dir("certify-lake-trim");
    let cert_dir = out_dir.join("cert");
    let build_tree = cert_dir.join(".lake").join("build");
    std::fs::create_dir_all(&build_tree).unwrap();
    std::fs::write(build_tree.join("stand-in.olean"), "build output\n").unwrap();
    std::fs::write(cert_dir.join("cert-manifest.json"), "{}\n").unwrap();

    trim_lean_build_tree(&cert_dir);

    assert!(
        !cert_dir.join(".lake").exists(),
        "a successful build's .lake tree must be removed early, so a killed run strands the certificate package and not the Lean build tree"
    );
    assert!(
        cert_dir.join("cert-manifest.json").is_file(),
        "trimming the build tree must leave the certificate package intact"
    );
}

#[test]
fn certify_exits_nonzero_when_the_certificate_package_cannot_be_replaced() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-package-write-failure");
    std::fs::create_dir_all(&out_dir).unwrap();
    std::fs::write(out_dir.join("cert"), "not a directory\n").unwrap();

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/certification/add_one.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    assert!(
        !compile.status.success(),
        "certificate package emission failure must fail the command:\n{report}"
    );
    assert!(
        out_dir.join("add_one.wasm").is_file(),
        "the regression must reach certificate emission after writing the Wasm artifact"
    );
    assert!(
        report.contains("certificate: replace cert dir"),
        "the real package replacement failure must remain visible:\n{report}"
    );
}

#[test]
fn certify_goal_matrix_manifest_tracks_current_surface() {
    // This fixture is the dashboard for "how much do we certify now?". Larger
    // programs such as examples/data/json.av remain integration side-effects;
    // this test pins the planned numerator/denominator directly. When a backlog
    // goal becomes certifiable, move it from `expected_backlog` into `expected`.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-goals");
    let stale_fragments = out_dir.join("cert").join("fragments");
    std::fs::create_dir_all(&stale_fragments).unwrap();
    std::fs::write(stale_fragments.join("v0.plan"), "stale v0 sidecar\n").unwrap();
    std::fs::write(
        out_dir.join("cert").join("ArtifactBytes.lean"),
        "-- stale checker-owned file\n",
    )
    .unwrap();
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
    assert_plans_lean_is_the_only_public_plan_data(&out_dir.join("cert"), &manifest);
    assert_eq!(
        manifest["format"],
        serde_json::json!({
            "version": aver::codegen::cert::wall::FORMAT_VERSION,
            "wall_id": aver::codegen::cert::wall::current_id(),
        }),
        "manifest should identify the one byte-exact wall resolved by the checker"
    );
    assert!(
        manifest
            .as_object()
            .unwrap()
            .keys()
            .all(|key| { !key.ends_with("_sha256") || matches!(key.as_str(), "wasm_sha256") }),
        "the wall id replaces per-module checker hash pins"
    );
    assert_eq!(
        manifest["artifact_certificate_root"].as_str(),
        Some(aver::codegen::cert::ARTIFACT_CERTIFICATE_ROOT),
        "manifest should expose the artifact-level certificate root"
    );
    assert_eq!(
        manifest["schema_version"].as_u64(),
        Some(9),
        "schema 9 is the one plan grammar with its plan-equals-source and law-claim surfaces"
    );
    assert_eq!(
        manifest["target"].as_str(),
        Some(aver::codegen::cert::ARTIFACT_TARGET),
        "the artifact target is explicit before target-specific envelope validation"
    );
    assert_eq!(
        manifest["profile"].as_str(),
        Some(aver::codegen::cert::PROFILE_ID),
        "the first public byte profile is pinned exactly"
    );
    assert_eq!(
        manifest["abi"].as_str(),
        Some(aver::codegen::cert::RUNTIME_ABI),
        "the wasm-gc runtime ABI is pinned exactly"
    );
    assert_eq!(aver::codegen::cert::CERT_SCHEMA_VERSION, 9);
    // Every certified export carries a plan-equals-source bridge, in obligation
    // order, unless one of its parameters has no decoder in this version — in
    // which case it is listed, with that reason, under `sourceBridgesDeclined`.
    // The two lists partition the certified exports exactly.
    let certified_names: Vec<&str> = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|entry| entry["name"].as_str().unwrap())
        .collect();
    let bridges = manifest["sourceBridges"].as_array().unwrap();
    let bridged: Vec<&str> = bridges
        .iter()
        .map(|entry| entry["export"].as_str().unwrap())
        .collect();
    let bridge_declined: BTreeMap<&str, &str> = manifest["sourceBridgesDeclined"]
        .as_array()
        .unwrap()
        .iter()
        .map(|entry| {
            (
                entry["export"].as_str().unwrap(),
                entry["reason"].as_str().unwrap(),
            )
        })
        .collect();
    assert_eq!(
        bridged,
        certified_names
            .iter()
            .copied()
            .filter(|name| !bridge_declined.contains_key(name))
            .collect::<Vec<_>>(),
        "every certified export without a declined bridge must carry one, in obligation order"
    );
    assert_eq!(
        bridge_declined,
        BTreeMap::from([
            (
                "wrapItems",
                "a `list` argument has no decoder in this version"
            ),
            (
                "floatLeGoal",
                "a `float` argument has no decoder in this version"
            ),
            (
                "floatGeGoal",
                "a `float` argument has no decoder in this version"
            ),
            (
                "floatLtGoal",
                "a `float` argument has no decoder in this version"
            ),
            (
                "floatGtGoal",
                "a `float` argument has no decoder in this version"
            ),
            (
                "floatEqGoal",
                "a `float` argument has no decoder in this version"
            ),
        ]),
        "the declined bridges pin the decoder boundary; move them deliberately"
    );
    // The entry carries STRUCTURE, never statement text: the checker renders
    // the statement from it. A `statement` key here would be a claim the
    // package chose its own wording for, which is exactly what this surface
    // stopped transporting — the exact-object gate is what enforces it.
    fn assert_closed_encoder(encoder: &serde_json::Value) {
        let kind = encoder["kind"].as_str().unwrap();
        assert!(
            matches!(
                kind,
                "int" | "bool" | "float" | "string" | "record" | "sum" | "option" | "list"
            ),
            "the encoder kind set is closed: {kind}"
        );
        if kind == "record" {
            let lean_type = encoder["type"].as_str().unwrap();
            assert!(lean_type.starts_with("_root_."));
            for field in encoder["fields"].as_array().unwrap() {
                let accessor = field["accessor"].as_str().unwrap();
                assert!(
                    accessor.starts_with(&format!("{lean_type}.")),
                    "an accessor must be a field of the declared type: {accessor}"
                );
                assert_closed_encoder(&field["encoder"]);
            }
        }
        if kind == "sum" {
            let lean_type = encoder["type"].as_str().unwrap();
            assert!(lean_type.starts_with("_root_."));
            for ctor in encoder["ctors"].as_array().unwrap() {
                let name = ctor["ctor"].as_str().unwrap();
                assert!(
                    name.starts_with(&format!("{lean_type}.")),
                    "a constructor must belong to the declared type: {name}"
                );
                for field in ctor["fields"].as_array().unwrap() {
                    assert_closed_encoder(field);
                }
            }
        }
    }
    let bridge_proof = std::fs::read_to_string(out_dir.join("cert").join("BridgeProof.lean"))
        .expect("a bridged package emits BridgeProof.lean");
    for entry in bridges {
        let export = entry["export"].as_str().unwrap();
        let object = entry.as_object().unwrap();
        assert_eq!(
            object.len(),
            7,
            "a source-bridge entry is matched exactly: {object:?}"
        );
        assert!(
            !object.contains_key("statement"),
            "the bridge surface must not transport statement text: {object:?}"
        );
        assert_eq!(
            entry["theorem"].as_str(),
            Some(format!("AverCert.Bridge.{export}").as_str())
        );
        assert_eq!(
            entry["corollary"].as_str(),
            Some(format!("AverCert.Bridge.{export}_certified").as_str())
        );
        assert!(
            matches!(entry["kind"].as_str(), Some("exact" | "adequate")),
            "the statement kind set is closed: {object:?}"
        );
        for encoder in entry["params"]
            .as_array()
            .unwrap()
            .iter()
            .chain(std::iter::once(&entry["result"]))
        {
            assert_closed_encoder(encoder);
        }
        // The statement the package's own bridge proofs carry is the one the
        // checker renders from the entry above — the producer writes it through
        // the same function the verifier pins with.
        assert!(
            bridge_proof.contains(&format!("_root_.AverCert.Bridge.{export} :")),
            "the emitted bridge proofs must state the bridge of `{export}`"
        );
    }
    let declared_uncertified = manifest["declaredUncertified"].as_array().unwrap();
    assert_eq!(
        declared_uncertified.len(),
        15,
        "all 43 module exports must be certified or explicitly declared"
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
    // Exercise the actual generated exports with raw positive/negative,
    // quiet/signalling, non-canonical NaN payloads in both operand positions.
    // The ordinary WebAssembly profile may return any arithmetic NaN here,
    // while Wasmtime's deterministic/canonicalizing profile returns the
    // positive canonical NaN. That set-valued boundary is why Float-producing
    // arithmetic is source-level-only until its certificate codomain becomes
    // relational. The Bool comparisons remain sign/payload-independent.
    const RAW_NANS: [(&str, u64); 4] = [
        ("positive qNaN payload", 0x7ff8_0000_0000_0001),
        ("negative qNaN payload", 0xfff8_0000_0000_0042),
        ("positive sNaN payload", 0x7ff0_0000_0000_0001),
        ("negative sNaN payload", 0xfff0_0000_0000_0042),
    ];
    const ONE: u64 = 0x3ff0_0000_0000_0000;
    const TWO: u64 = 0x4000_0000_0000_0000;
    const POSITIVE_ZERO: u64 = 0x0000_0000_0000_0000;
    const NEGATIVE_ZERO: u64 = 0x8000_0000_0000_0000;
    const CANONICAL_NAN: u64 = 0x7ff8_0000_0000_0000;
    // Every ordered Float comparison the plan grammar admits, paired with the
    // source operator it comes from. The wall states each of these with Lean's
    // own `Float` `<=` / `<` / `==`, which are opaque extern symbols the kernel
    // cannot unfold: "Lean `Float` ordered comparison behaves as the Wasm f64
    // ordered comparison" is the one premise held EMPIRICALLY, right here.
    // Admitting another float comparison opcode without adding it to this table
    // puts it on that unproved bridge with no engine-level coverage.
    const FLOAT_ORDERED_CMP: [(&str, &str); 5] = [
        ("floatLeGoal", "<="),
        ("floatGeGoal", ">="),
        ("floatLtGoal", "<"),
        ("floatGtGoal", ">"),
        ("floatEqGoal", "=="),
    ];
    // (case, lhs bits, rhs bits, expected i32 per FLOAT_ORDERED_CMP entry in
    // that order). The ordered pairs are the control: a harness that answered
    // "false" everywhere would still pass a NaN-only table. The signed-zero rows
    // are where IEEE equality and bit equality disagree (`-0.0 == 0.0` is true
    // while the bit patterns differ), which is exactly where a Lean/Wasm
    // divergence could hide. The NaN rows use the canonical quiet NaN; the
    // sign/payload variants are swept separately by RAW_NANS below.
    const CMP_CASES: [(&str, u64, u64, [i32; 5]); 8] = [
        ("1.0 vs 2.0 (ordered control)", ONE, TWO, [1, 0, 1, 0, 0]),
        ("2.0 vs 1.0 (ordered control)", TWO, ONE, [0, 1, 0, 1, 0]),
        ("1.0 vs 1.0 (equal control)", ONE, ONE, [1, 1, 0, 0, 1]),
        ("qNaN on the left", CANONICAL_NAN, ONE, [0, 0, 0, 0, 0]),
        ("qNaN on the right", ONE, CANONICAL_NAN, [0, 0, 0, 0, 0]),
        (
            "qNaN on both sides",
            CANONICAL_NAN,
            CANONICAL_NAN,
            [0, 0, 0, 0, 0],
        ),
        ("-0.0 vs 0.0", NEGATIVE_ZERO, POSITIVE_ZERO, [1, 1, 0, 0, 1]),
        ("0.0 vs -0.0", POSITIVE_ZERO, NEGATIVE_ZERO, [1, 1, 0, 0, 1]),
    ];
    for canonicalize_nans in [false, true] {
        let profile = if canonicalize_nans {
            "canonicalizing"
        } else {
            "general"
        };
        let (mut store, instance) = instantiate_float_probe(&wasm, canonicalize_nans);
        let add = instance
            .get_typed_func::<(f64, f64), f64>(&mut store, "floatAddGoal")
            .expect("floatAddGoal export");
        let mul_add = instance
            .get_typed_func::<(f64, f64), f64>(&mut store, "floatMulAddGoal")
            .expect("floatMulAddGoal export");
        let comparisons = FLOAT_ORDERED_CMP.map(|(export, _)| {
            instance
                .get_typed_func::<(f64, f64), i32>(&mut store, export)
                .unwrap_or_else(|error| panic!("{export} export: {error}"))
        });

        for (nan_name, nan_bits) in RAW_NANS {
            let nan = f64::from_bits(nan_bits);
            let one = f64::from_bits(ONE);
            for (position, raw_args) in [
                ("lhs", (nan, one)),
                ("rhs", (one, nan)),
                ("both sides", (nan, nan)),
            ] {
                for (export, function) in [("floatAddGoal", &add), ("floatMulAddGoal", &mul_add)] {
                    let result_bits = function
                        .call(&mut store, raw_args)
                        .unwrap_or_else(|error| {
                            panic!("run {profile} {export} with {nan_name} on {position}: {error}")
                        })
                        .to_bits();
                    if canonicalize_nans {
                        assert_eq!(
                            result_bits, CANONICAL_NAN,
                            "canonicalizing {export} must return the positive canonical NaN \
                             for {nan_name} on {position}"
                        );
                    } else {
                        assert!(
                            is_arithmetic_nan_bits(result_bits),
                            "general {export} must return an allowed arithmetic NaN for \
                             {nan_name} on {position}, got 0x{result_bits:016x}"
                        );
                    }
                }
                for (index, (export, operator)) in FLOAT_ORDERED_CMP.into_iter().enumerate() {
                    assert_eq!(
                        comparisons[index]
                            .call(&mut store, raw_args)
                            .unwrap_or_else(|error| {
                                panic!(
                                    "run {profile} {export} with {nan_name} on {position}: {error}"
                                )
                            }),
                        0,
                        "ordered `{operator}` ({export}) must be false for {nan_name} on \
                         {position} in the {profile} profile"
                    );
                }
            }
        }

        for (case, lhs_bits, rhs_bits, expected) in CMP_CASES {
            let args = (f64::from_bits(lhs_bits), f64::from_bits(rhs_bits));
            for (index, (export, operator)) in FLOAT_ORDERED_CMP.into_iter().enumerate() {
                // The host's own IEEE-754 comparison is the second reading of
                // the same relation Lean's extern `Float` primitives compile
                // to, so a mistyped row in the table is caught here rather than
                // being mistaken for a Wasm divergence below.
                let host = i32::from(match operator {
                    "<=" => args.0 <= args.1,
                    ">=" => args.0 >= args.1,
                    "<" => args.0 < args.1,
                    ">" => args.0 > args.1,
                    "==" => args.0 == args.1,
                    other => panic!("no host reading for float comparison `{other}`"),
                });
                assert_eq!(
                    host, expected[index],
                    "pinned IEEE expectation for `{operator}` on {case} disagrees with the host"
                );
                assert_eq!(
                    comparisons[index]
                        .call(&mut store, args)
                        .unwrap_or_else(|error| {
                            panic!("run {profile} {export} on {case}: {error}")
                        }),
                    expected[index],
                    "{export} (`{operator}`) diverged from the IEEE ordered comparison on \
                     {case} in the {profile} profile"
                );
            }
        }
    }
    // The name-bound helper roles follow the export section; the wall pins
    // them there (and every role to its template) in `decodedHostRoleTable`.
    for (role, export) in [
        ("box", "__rt_aint_from_i64"),
        ("toIndex", "__aint_to_index"),
        ("cmp", "__aint_cmp"),
    ] {
        assert_eq!(
            manifest["hostRoleTable"][role].as_u64(),
            wasm_export_index(&wasm, export).map(u64::from),
            "the declared `{role}` role must be the `{export}` export"
        );
    }
    for role in ["add", "sub", "mul", "divmod"] {
        assert!(
            manifest["hostRoleTable"][role].is_u64(),
            "the goals module carries the `{role}` helper, so the role must be declared"
        );
    }
    // Both String helpers are classified, in function order.
    let string_roles: Vec<&str> = manifest["stringHostRoles"]
        .as_array()
        .unwrap()
        .iter()
        .map(|entry| entry["role"].as_str().unwrap())
        .collect();
    assert_eq!(string_roles, ["stringEq", "stringConcat"]);

    // Every certified export has the one plan class; the facets say which
    // grammar features its call closure uses, and the wall derives them
    // (`ClaimAxes.reportFacets`), so the checker witness pins every one.
    let actual: BTreeMap<String, Vec<String>> = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| {
            assert_eq!(c["class"], "source-plan-v1", "one plan class for {c}");
            assert_eq!(
                c["theorem"], "AcceptanceSoundness.fn_claim_discharges",
                "one discharge theorem for {c}"
            );
            (
                c["name"].as_str().unwrap().to_string(),
                c["facets"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|facet| facet.as_str().unwrap().to_string())
                    .collect(),
            )
        })
        .collect();
    let facets = |list: &[&str]| list.iter().map(|f| f.to_string()).collect::<Vec<_>>();
    let expected: BTreeMap<String, Vec<String>> = [
        ("addTwo", facets(&[])),
        ("sumFrom", facets(&["recursive", "calls"])),
        ("countDown", facets(&["recursive", "calls"])),
        ("double", facets(&[])),
        ("quad", facets(&["calls"])),
        ("hex16", facets(&["calls"])),
        ("isEven", facets(&["recursive", "mutual", "calls"])),
        ("isOdd", facets(&["recursive", "mutual", "calls"])),
        ("mkOp", facets(&["variants"])),
        ("evalOp", facets(&["variants"])),
        ("userName", facets(&["records"])),
        ("boxInt", facets(&["variants"])),
        ("wrapItems", facets(&["variants"])),
        ("tagName", facets(&["variants", "strings"])),
        ("gauge", facets(&["variants"])),
        ("inAsciiDigit", facets(&[])),
        ("quoteOrSelf", facets(&["strings"])),
        ("shout", facets(&["strings"])),
        ("intLessZero", facets(&[])),
        ("intEqZero", facets(&[])),
        ("boolAndGoal", facets(&[])),
        ("inWindowGoal", facets(&[])),
        ("floatLeGoal", facets(&[])),
        ("floatGeGoal", facets(&[])),
        ("floatLtGoal", facets(&[])),
        ("floatGtGoal", facets(&[])),
        ("floatEqGoal", facets(&[])),
        // `idGoal` (the identity) became certifiable when the one grammar
        // replaced the families: a bare parameter read is a plan like any
        // other (numerator moved deliberately).
        ("idGoal", facets(&[])),
    ]
    .into_iter()
    .map(|(name, facets)| (name.to_string(), facets))
    .collect();
    assert_eq!(
        actual, expected,
        "certified goal matrix changed; update the numerator deliberately"
    );
    // The plan of every certified export is in `Plans.lean`, keyed by its
    // function index in `fnPlans`, and Float arithmetic has no plan at all.
    let plans_lean = std::fs::read_to_string(out_dir.join("cert").join("Plans.lean"))
        .expect("Plans.lean exists");
    for name in actual.keys() {
        assert!(
            plans_lean.contains(&format!("⟨\"{name}\", true, ")),
            "{name} must have its entry in the authoritative fnPlans:\n{plans_lean}"
        );
    }
    for (shape, what) in [
        (
            "(.binOp .add (.local 0) (.literal (.int 2)))",
            "addTwo adds two",
        ),
        (
            "(.binOp .gte (.local 0) (.literal (.int 48))) (.binOp .lte (.local 0) (.literal (.int 57)))",
            "inAsciiDigit keeps both bounds",
        ),
        (
            "(.binOp .eq (.local 0) (.literal (.int 0)))",
            "intEqZero compares with zero",
        ),
        (
            "(.ifThenElse (.local 0) (.local 1) (.literal (.bool false)))",
            "boolAndGoal short-circuits",
        ),
        (
            "(.call (.builtin .boolAnd) [",
            "inWindowGoal is the eager Bool.and",
        ),
        (
            "(.binOp .lte (.local 0) (.local 1))",
            "floatLeGoal compares two floats",
        ),
        (
            "(.project 1 0 (.local 0))",
            "userName projects the name field",
        ),
        (
            "(.construct (.user 0 0) (.sum 0) [(.local 0)])",
            "mkOp builds the first constructor",
        ),
    ] {
        assert!(
            plans_lean.contains(shape),
            "{what}: `{shape}` missing:\n{plans_lean}"
        );
    }
    assert!(
        !plans_lean.contains("floatAddGoal") && !plans_lean.contains("floatMulAddGoal"),
        "Float arithmetic must carry no plan:\n{plans_lean}"
    );
    let artifact_certificate =
        std::fs::read_to_string(out_dir.join("cert").join("ArtifactCertificate.lean"))
            .expect("ArtifactCertificate.lean exists");
    assert!(
        artifact_certificate
            .contains("theorem certificate : AverCert.AcceptedArtifact.accepted data :="),
        "artifact root should be a theorem with the exact AcceptedArtifact target:\n{artifact_certificate}"
    );

    let planned_goal_names: BTreeSet<String> = [
        "addTwo",
        "double",
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
        "inWindowGoal",
        "floatAddGoal",
        "floatMulAddGoal",
        "floatLeGoal",
        "floatGeGoal",
        "floatLtGoal",
        "floatGtGoal",
        "floatEqGoal",
        "idGoal",
        "listHeadGoal",
        "sumListGoal",
    ]
    .into_iter()
    .map(str::to_string)
    .collect();
    let expected_backlog: BTreeSet<String> = [
        "floatAddGoal",
        "floatMulAddGoal",
        "listHeadGoal",
        "sumListGoal",
    ]
    .into_iter()
    .map(str::to_string)
    .collect();
    assert_eq!(planned_goal_names.len(), 32, "goal denominator changed");
    assert_eq!(actual.len(), 28, "goal numerator changed");

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
        ],
        "additive/accumulator/mutual L3 must not declare Int.mul totality"
    );
    let manifest_lean =
        std::fs::read_to_string(out_dir.join("cert/Manifest.lean")).expect("Manifest.lean");
    assert!(
        manifest_lean.contains("target := \"wasm-gc\"")
            && manifest_lean.contains("profile := \"AverUserProfile/v1\""),
        "Lean manifest must pin the same public target/profile identity as JSON"
    );
    // The obligations, and with them every policy axis, are the wall's
    // derivation from the plans; the manifest carries no obligation data.
    assert!(
        manifest_lean.contains(
            "obligations := AverCert.AcceptedArtifact.obligationsOf subject Plans.types Plans.fnPlans"
        ),
        "the obligations must be the ones the wall derives:\n{manifest_lean}"
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
    // Float-producing arithmetic has no plan: the grammar types a Float
    // comparison (a Bool, independent of NaN payloads) but no Float result,
    // since general Wasm allows several NaN sign/payload results where the
    // model would name one.
    for (name, expected) in [
        ("floatAddGoal", "plan does not type in the one grammar"),
        ("floatMulAddGoal", "plan does not type in the one grammar"),
        ("listHeadGoal", "Match pattern EmptyList"),
        ("sumListGoal", "Match pattern EmptyList"),
    ] {
        let reason = manifest["source_level_only"]
            .as_array()
            .unwrap()
            .iter()
            .find(|entry| entry["name"].as_str() == Some(name))
            .and_then(|entry| entry["reason"].as_str())
            .unwrap_or_else(|| panic!("{name} should carry a source-level-only reason"));
        assert_eq!(reason, expected, "{name} must name its blocker");
    }
}

#[test]
fn certify_goal_matrix_lands_acceptance_wall_kernel_clean() {
    if !lean_required::lake_available() {
        eprintln!("skipping certify test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-acceptance-wall");
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
    let audited_modules = aver::codegen::cert::wall::SOURCES;
    for entry in std::fs::read_dir(&cert_dir).expect("read emitted certificate directory") {
        let name = entry
            .expect("read emitted certificate entry")
            .file_name()
            .to_string_lossy()
            .into_owned();
        assert!(
            !name.starts_with("V3"),
            "emitted certificate must not expose a historical V3 module: {name}"
        );
    }
    assert!(
        manifest
            .as_object()
            .expect("certificate manifest is an object")
            .keys()
            .all(|key| !key.starts_with("v3_")),
        "certificate manifest must not expose historical v3 keys: {manifest}"
    );
    for source in audited_modules {
        assert!(
            !cert_dir.join(source.name).exists(),
            "checker-owned wall source {} must be resolved by wall_id, not copied",
            source.name,
        );
    }
    for checker_owned in ["lean-toolchain", "lakefile.lean"] {
        assert!(
            !cert_dir.join(checker_owned).exists(),
            "checker-owned {checker_owned} must not be copied into the certificate package"
        );
    }
    assert_eq!(
        manifest["format"]["wall_id"].as_str(),
        Some(aver::codegen::cert::wall::current_id()),
        "one aggregate identity replaces the audited module hash fields"
    );

    // One final theorem, proved by the ONE soundness theorem from the
    // acceptance's byte facts — no per-export proof of any kind is emitted.
    let final_lean =
        std::fs::read_to_string(cert_dir.join("Final.lean")).expect("Final.lean exists");
    assert!(
        final_lean.contains("theorem AverCert.Final.cert : AverCert.Schema.Holds manifest :=")
            && final_lean.contains("AcceptanceSoundness.accept_sound")
            && final_lean.contains("AverCert.Artifact.plans_ok"),
        "Final.cert must be the single accept-sound application:\n{final_lean}"
    );
    for file in ["Final.lean", "Artifact.lean", "Manifest.lean", "Plans.lean"] {
        let text = std::fs::read_to_string(cert_dir.join(file)).unwrap();
        assert!(
            !text.contains("_claim_discharges")
                && !text.contains("_canonical_discharges")
                && !text.contains("CertProofs.")
                && !text.contains("_simulates"),
            "{file} must not carry per-export proofs of the retired families:\n{text}"
        );
    }
    assert!(
        !cert_dir.join("Certificate.lean").exists() && !cert_dir.join("Contracts.lean").exists(),
        "the retired per-family proof modules must not be emitted"
    );
    // The artifact root states exactly the accepted-artifact proposition over
    // the package data, and prints its axioms for the checker to audit.
    let artifact_certificate = std::fs::read_to_string(cert_dir.join("ArtifactCertificate.lean"))
        .expect("ArtifactCertificate.lean exists");
    assert!(
        artifact_certificate
            .contains("theorem certificate : AverCert.AcceptedArtifact.accepted data :=")
            && artifact_certificate.contains("AverCert.Final.cert")
            && artifact_certificate.contains("#print axioms AverCert.Artifact.certificate"),
        "the artifact root must package the final theorem and the byte facts:\n{artifact_certificate}"
    );
    // Every byte fact is decided by the kernel against the checker-staged
    // bytes, one declaration each.
    let artifact_lean =
        std::fs::read_to_string(cert_dir.join("Artifact.lean")).expect("Artifact.lean exists");
    for fact in [
        "theorem plans_ok",
        "theorem roles_ok",
        "theorem axes_ok",
        "theorem strings_ok",
        "theorem framing_ok",
        "theorem exports_ok",
        "theorem imports_ok",
        "theorem start_ok",
        "theorem closure_ok",
        "theorem envelope_ok",
    ] {
        assert!(
            artifact_lean.contains(fact),
            "Artifact.lean must carry `{fact}`:\n{artifact_lean}"
        );
    }
    assert!(
        artifact_lean.contains("modBytes := AverCert.ArtifactBytes.modBytes"),
        "the artifact data must read the checker-staged bytes:\n{artifact_lean}"
    );
    // The goals package builds its acceptance root kernel-clean.
    assert_certificate_target_builds(&cert_dir, "cert goals acceptance wall");
}

// Hostile-model soundness gates.
//
// These tests all share one baseline artifact and differ only in which single
// tamper they apply before demanding a verdict. They used to be one test that
// ran every verification sequentially: each `check_certificate` call is a full
// kernel-checked certificate verification while the baseline `aver compile
// --certify` costs a fraction of a second. Splitting the tamper vectors into
// separate tests — each redoing the cheap setup — lets CI run the expensive
// verifications in parallel lanes.
//
// A certificate's obligations are DERIVED by the wall from the plans
// (`AcceptedArtifact.obligationsOf`): the model of every export is its plan's
// meaning, never producer data. So there are two kinds of hostile model:
//
// * a hostile OBLIGATION model — the manifest's obligation list replaced by a
//   rewritten one. The acceptance requires the obligations to be exactly the
//   derived ones (`obligationsDerived`), so the package is DECLINED;
// * a hostile SOURCE model — the generated Lean definition of the Aver source
//   function rewritten. The export's certified claim does not mention it, so
//   the export stays certified; what the source definition feeds is the
//   plan-equals-source bridge, and that bridge must then NOT be credited.

/// Hostile obligation models, as `(label, export whose model is replaced,
/// export whose plan meaning replaces it)`. `None` for the second export
/// replaces the model with the everywhere-undefined one.
///
/// This list is the single source of truth for which obligation models the
/// gate covers, and it deliberately stays a list. The shard tests below select
/// entries by `idx % HOSTILE_MANIFEST_MODEL_SHARDS`, never by name, so an
/// entry appended here is automatically exercised by exactly one existing
/// shard: no new test function to write, no CI filter to update.
const HOSTILE_MANIFEST_MODELS: &[(&str, &str, Option<&str>)] = &[
    ("variant tag", "tagName", Some("wrapItems")),
    ("string", "quoteOrSelf", Some("shout")),
    ("dispatch", "gauge", None),
];

/// How many parallel shards `HOSTILE_MANIFEST_MODELS` is spread over: one test
/// function per shard. Keep it at most the list length so no shard runs empty
/// (an empty shard would pass vacuously); the shard runner asserts that.
const HOSTILE_MANIFEST_MODEL_SHARDS: usize = 3;

/// Hostile rewrites of the generated mutual-recursion source definitions in
/// `cert/AverModel/CertGoals.lean`, as `(name, honest, hostile)`.
/// Index-sharded for the same reason as `HOSTILE_MANIFEST_MODELS`.
const HOSTILE_MUTUAL_MODELS: &[(&str, &str, &str)] = &[
    (
        "isEven",
        "else isOdd__fuel fuel' (n - 1))",
        "else isOdd__fuel fuel' (n - 2))",
    ),
    (
        "isOdd",
        "else isEven__fuel fuel' (n - 1))",
        "else isEven__fuel fuel' (n - 2))",
    ),
];

/// How many parallel shards `HOSTILE_MUTUAL_MODELS` is spread over.
const HOSTILE_MUTUAL_MODEL_SHARDS: usize = 2;

/// Compiles the shared hostile-model baseline and asserts the untampered
/// certificate still passes the developer preflight with every bridge the
/// goals fixture carries credited.
///
/// Every hostile-model test runs this itself rather than trusting a baseline
/// established in some other test (and therefore some other CI lane), so each
/// one can fail honestly on its own.
///
/// Returns `None` when `lake` is unavailable; the caller then skips, as before.
fn hostile_models_baseline(prefix: &str) -> Option<ScratchDir> {
    if !lean_required::lake_available() {
        eprintln!("skipping hostile-model test: `lake` not available");
        return None;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir(prefix);
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
    let (clean_ok, clean_report) = check_certificate(&wasm, &cert);
    assert!(
        clean_ok,
        "hostile-model baseline must first pass trusted-olean preflight:\n{clean_report}"
    );
    assert!(
        clean_report.contains("CHECKED") && !clean_report.contains("CERTIFIED"),
        "developer preflight must never emit the certification verdict:\n{clean_report}"
    );
    assert!(
        clean_report.contains("source-bridges: 22 of 22 credited"),
        "the honest baseline credits every goals bridge:\n{clean_report}"
    );

    Some(out_dir)
}

/// The `fnPlans` function index of export `name` in `Plans.lean`.
fn planned_function_index(plans_lean: &str, name: &str) -> u32 {
    let head = format!("⟨\"{name}\", true, ");
    let at = plans_lean
        .find(&head)
        .unwrap_or_else(|| panic!("Plans.lean has no entry for `{name}`"))
        + head.len();
    plans_lean[at..]
        .split(',')
        .next()
        .unwrap()
        .trim()
        .parse()
        .expect("fnPlans function index")
}

/// Runs the `HOSTILE_MANIFEST_MODELS` entries that belong to `shard`.
fn assert_hostile_manifest_model_shard_is_declined(shard: usize) {
    assert!(
        shard < HOSTILE_MANIFEST_MODEL_SHARDS
            && HOSTILE_MANIFEST_MODEL_SHARDS <= HOSTILE_MANIFEST_MODELS.len(),
        "shard {shard} of {HOSTILE_MANIFEST_MODEL_SHARDS} covers no hostile manifest model: keep the shard count at most the list length, one test function per shard"
    );
    let Some(out_dir) =
        hostile_models_baseline(&format!("certify-hostile-manifest-models-{shard}"))
    else {
        return;
    };

    // Index-sharded rather than name-selected: every entry of the list lands in
    // exactly one shard by construction, including entries added later.
    for (idx, &(label, victim, donor)) in HOSTILE_MANIFEST_MODELS.iter().enumerate() {
        if idx % HOSTILE_MANIFEST_MODEL_SHARDS != shard {
            continue;
        }

        let tampered = temp_dir(&format!(
            "certify-hostile-{}-model",
            label.replace(' ', "-")
        ));
        copy_dir_all(&out_dir, &tampered);
        let plans = std::fs::read_to_string(tampered.join("cert/Plans.lean")).unwrap();
        let hostile_model = match donor {
            Some(donor) => format!(
                "fun fuel => AverCert.AcceptedArtifact.modelOf Plans.fnPlans fuel {}",
                planned_function_index(&plans, donor)
            ),
            None => "fun _ _ => none".to_string(),
        };
        let manifest = tampered.join("cert/Manifest.lean");
        let source = std::fs::read_to_string(&manifest).unwrap();
        let honest = "obligations := AverCert.AcceptedArtifact.obligationsOf subject Plans.types Plans.fnPlans";
        let hostile = format!(
            "obligations := (AverCert.AcceptedArtifact.obligationsOf subject Plans.types Plans.fnPlans).map \
             (fun o => if o.export_ = \"{victim}\" then {{ o with model := {hostile_model} }} else o)"
        );
        let edited = source.replacen(honest, &hostile, 1);
        assert_ne!(
            source, edited,
            "{label} obligation shape changed; update the hostile-model regression"
        );
        std::fs::write(&manifest, edited).unwrap();

        let (ok, report) =
            check_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
        assert!(
            !ok && report.contains("CHECK FAILED") && !report.contains("CERTIFIED"),
            "a wrong {label} obligation model must be DECLINED:\n{report}"
        );
    }
}

/// Rewrite one generated source definition of the goals model and check the
/// package: every export stays certified, and exactly the bridges in
/// `lost` lose their credit, each named with the axiom that sank it.
fn assert_hostile_source_model_loses_bridges(prefix: &str, edits: &[(&str, &str)], lost: &[&str]) {
    let Some(out_dir) = hostile_models_baseline(prefix) else {
        return;
    };
    let tampered = temp_dir(&format!("{prefix}-tampered"));
    copy_dir_all(&out_dir, &tampered);
    let model = tampered.join("cert/AverModel/CertGoals.lean");
    let mut source = std::fs::read_to_string(&model).unwrap();
    for (honest, hostile) in edits {
        let edited = source.replacen(honest, hostile, 1);
        assert_ne!(
            source, edited,
            "the source definition `{honest}` changed; update the hostile-model regression"
        );
        source = edited;
    }
    std::fs::write(&model, source).unwrap();
    assert_eq!(
        std::fs::read(tampered.join("cert_goals.wasm")).unwrap(),
        std::fs::read(out_dir.join("cert_goals.wasm")).unwrap(),
        "the hostile model check must isolate the mutation to generated source data"
    );

    let (ok, report) = check_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
    assert!(
        ok && report.contains("28 checked exports"),
        "a wrong source definition must not touch the export verdict:\n{report}"
    );
    assert!(
        report.contains(&format!(
            "source-bridges: {} of 22 credited",
            22 - lost.len()
        )),
        "exactly the bridges through the wrong definition must lose their credit:\n{report}"
    );
    for name in lost {
        assert!(
            report.contains(&format!(
                "source-bridge not credited: {name} (proof depends on sorryAx)"
            )),
            "the bridge of `{name}` must not be credited:\n{report}"
        );
    }
}

/// The untampered hostile-model baseline is preflight-clean and its isolated
/// acceptance root builds, so a verdict in any hostile-model test below is
/// the tamper's doing and not a broken fixture.
#[test]
fn cert_hostile_model_baseline_is_preflight_clean_and_lake_builds() {
    let Some(out_dir) = hostile_models_baseline("certify-hostile-leaf-models") else {
        return;
    };

    let build_green = temp_dir("certify-hostile-build-green");
    copy_dir_all(&out_dir, &build_green);
    assert_certificate_target_builds(&build_green.join("cert"), "hostile-model baseline");
}

/// Hostile source model: the generated leaf definition `inAsciiDigit` accepts
/// one code point too many (`c <= 58` instead of `c <= 57`). The export's
/// certified model is its plan, so it stays certified; its plan-equals-source
/// bridge, which would identify the plan with this definition, is not credited.
#[test]
fn cert_hostile_model_source_leaf_definition_loses_its_bridge() {
    assert_hostile_source_model_loses_bridges(
        "certify-hostile-expr-fragment",
        &[(
            "def inAsciiDigit (c : Int) : Bool :=\n  (if (c >= 48) then (c <= 57)",
            "def inAsciiDigit (c : Int) : Bool :=\n  (if (c >= 48) then (c <= 58)",
        )],
        &["inAsciiDigit"],
    );
}

/// Hostile obligation models, shard 0: `HOSTILE_MANIFEST_MODELS` entries
/// 0, 3, 6, ... — today `tagName` given `wrapItems`' plan meaning.
#[test]
fn cert_hostile_model_manifest_obligation_shard_0_of_3_is_declined() {
    assert_hostile_manifest_model_shard_is_declined(0);
}

/// Hostile obligation models, shard 1: `HOSTILE_MANIFEST_MODELS` entries
/// 1, 4, 7, ... — today `quoteOrSelf` given `shout`'s plan meaning.
#[test]
fn cert_hostile_model_manifest_obligation_shard_1_of_3_is_declined() {
    assert_hostile_manifest_model_shard_is_declined(1);
}

/// Hostile obligation models, shard 2: `HOSTILE_MANIFEST_MODELS` entries
/// 2, 5, 8, ... — today `gauge` given the everywhere-undefined model.
#[test]
fn cert_hostile_model_manifest_obligation_shard_2_of_3_is_declined() {
    assert_hostile_manifest_model_shard_is_declined(2);
}

/// Hostile model: `mkOp`'s plan builds the SECOND constructor of `Op` while
/// the bytes build the first. The plan still types, but its lowering is not
/// the code entry, so the package is DECLINED.
#[test]
fn cert_hostile_model_declared_construct_index_is_declined() {
    let Some(out_dir) = hostile_models_baseline("certify-hostile-construct-baseline") else {
        return;
    };

    let tampered = temp_dir("certify-hostile-construct-plan");
    copy_dir_all(&out_dir, &tampered);
    let plans_path = tampered.join("cert/Plans.lean");
    let source = std::fs::read_to_string(&plans_path).unwrap();
    let honest = "(.construct (.user 0 0) (.sum 0) [(.local 0)])";
    let hostile = "(.construct (.user 0 1) (.sum 0) [(.local 0)])";
    assert!(
        export_plan_block(&source, "mkOp").contains(honest),
        "construct plan shape changed; update the hostile-model regression"
    );
    std::fs::write(&plans_path, source.replacen(honest, hostile, 1)).unwrap();

    let (ok, report) = check_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
    assert!(
        !ok && report.contains("CHECK FAILED") && !report.contains("CERTIFIED"),
        "a wrong constructor must be DECLINED:\n{report}"
    );
}

/// Runs the `HOSTILE_MUTUAL_MODELS` entries that belong to `shard`. A mutual
/// source definition feeds both members' bridges (each member's bridge is
/// proved through the whole call group), so both lose their credit.
fn assert_hostile_mutual_model_shard_loses_bridges(shard: usize) {
    assert!(
        shard < HOSTILE_MUTUAL_MODEL_SHARDS
            && HOSTILE_MUTUAL_MODEL_SHARDS <= HOSTILE_MUTUAL_MODELS.len(),
        "shard {shard} of {HOSTILE_MUTUAL_MODEL_SHARDS} covers no hostile mutual model: keep the shard count at most the list length, one test function per shard"
    );
    for (idx, &(name, honest, hostile)) in HOSTILE_MUTUAL_MODELS.iter().enumerate() {
        if idx % HOSTILE_MUTUAL_MODEL_SHARDS != shard {
            continue;
        }
        assert_hostile_source_model_loses_bridges(
            &format!("certify-hostile-{name}-model-definition"),
            &[(honest, hostile)],
            &["isEven", "isOdd"],
        );
    }
}

/// Hostile mutual-recursion source definitions, shard 0:
/// `HOSTILE_MUTUAL_MODELS` entries 0, 2, 4, ... — today `isEven` recursing
/// on `n - 2`.
#[test]
fn cert_hostile_model_mutual_recursion_shard_0_of_2_loses_its_bridges() {
    assert_hostile_mutual_model_shard_loses_bridges(0);
}

/// Hostile mutual-recursion source definitions, shard 1:
/// `HOSTILE_MUTUAL_MODELS` entries 1, 3, 5, ... — today `isOdd` recursing on
/// `n - 2`.
#[test]
fn cert_hostile_model_mutual_recursion_shard_1_of_2_loses_its_bridges() {
    assert_hostile_mutual_model_shard_loses_bridges(1);
}

/// Guard the shard counts against drifting away from the test functions.
///
/// The shard runners already fail when a list shrinks below its shard count.
/// The opposite direction is the silent one: RAISING a `*_SHARDS` constant
/// without adding the matching `shard_N_of_M` test means every entry whose
/// index has that remainder is simply never checked, and every remaining test
/// still passes. Nothing in the type system ties a constant to the number of
/// `#[test]` functions, so this reads the source of this file and counts them.
///
/// Deliberately outside the `cert_hostile_model_` prefix: it needs no baseline
/// and belongs on the fast lane, not on a kernel-heavy one.
#[test]
fn certify_hostile_model_shards_all_have_test_functions() {
    let source = include_str!("cert_certify_spec.rs");
    for (family, shards, verdict) in [
        (
            "manifest_obligation",
            HOSTILE_MANIFEST_MODEL_SHARDS,
            "is_declined",
        ),
        (
            "mutual_recursion",
            HOSTILE_MUTUAL_MODEL_SHARDS,
            "loses_its_bridges",
        ),
    ] {
        for shard in 0..shards {
            let expected =
                format!("fn cert_hostile_model_{family}_shard_{shard}_of_{shards}_{verdict}");
            assert!(
                source.contains(&expected),
                "hostile {family} shard {shard} of {shards} has no test function, so entries with \
                 idx % {shards} == {shard} are never checked; add `{expected}`"
            );
        }
    }
}

/// Hostile source model: the generated fueled self-recursion `sumFrom`
/// accumulates the constant `2` instead of `n`. `sumFrom`'s bridge is the one
/// proved through that definition, so it alone loses its credit.
#[test]
fn cert_hostile_model_fueled_recursion_definition_loses_its_bridge() {
    assert_hostile_source_model_loses_bridges(
        "certify-hostile-recursion",
        &[("else (n + sumFrom (n - 1)))", "else (2 + sumFrom (n - 1)))")],
        &["sumFrom"],
    );
}

#[test]
fn certify_straight_line_fixture_lake_builds_kernel_clean() {
    if !lean_required::lake_available() {
        eprintln!("skipping certify test: `lake` not available");
        return;
    }

    let (out_dir, manifest) =
        certify_fixture("tools/certkit/fixtures/certprobe.av", &[], "certify");
    assert_certified_as(&manifest, "addTwo", &[], false);
    // The package's acceptance root builds on the core axiom whitelist, with
    // no `sorryAx` anywhere in it.
    assert_certificate_target_builds(&out_dir.join("cert"), "straight-line certprobe");
}

/// A recursion multiplying by a large constant is certified at L3 with the
/// `.mul` totality role (its multiplier fits the i64 band, so the plan types
/// and lowers like any other literal); a multiplier outside the i64 band has
/// no plan at all (`Literal BigInt`), and the producer declines it by name
/// instead of aborting.
#[test]
fn certify_declines_overflowing_multiplication_recursion() {
    // No `lake` needed: this is a pure emitter fail-closed check.
    let (_out_dir, manifest) = certify_fixture(
        "tools/certkit/fixtures/recdecline.av",
        &[],
        "certify-recdecline",
    );
    // Deliberate change: `wild` (multiplier 10^13) used to be declined by the
    // retired recursion family's overflow guard. The one grammar certifies it
    // totally, conditional on the multiplication contract.
    assert_certified_as(&manifest, "wild", &["recursive", "calls"], true);
    let contracts: Vec<&str> = manifest["runtime_contracts"]
        .as_array()
        .unwrap()
        .iter()
        .map(|c| c.as_str().unwrap())
        .collect();
    assert!(
        contracts.contains(&aver::codegen::cert::INT_MUL_TOTAL_CONTRACT),
        "a multiplying L3 recursion must be conditional on Int.mul totality: {contracts:?}"
    );

    // The decline-without-panic case: a multiplier outside the i64 band.
    let out_dir = temp_dir("certify-bigmul");
    std::fs::create_dir_all(&out_dir).unwrap();
    let source = out_dir.join("bigmul.av");
    std::fs::write(
        &source,
        "module BigMul\n    intent =\n        \"A multiplier outside the i64 band.\"\n    exposes [huge]\n\n\
         fn huge(n: Int) -> Int\n    ? \"Multiplier 10^20.\"\n    match n <= 0\n        true -> 1\n        \
         false -> 100000000000000000000 * huge(n - 1)\n\nverify huge\n    huge(0) => 1\n",
    )
    .unwrap();
    let (_big_dir, big) = certify_fixture(source.to_str().unwrap(), &[], "certify-bigmul-out");
    assert!(
        big["certified"].as_array().unwrap().is_empty(),
        "a multiplier outside the i64 band must NOT be certified: {big:#}"
    );
    assert_eq!(
        big["source_level_only"],
        serde_json::json!([{"name": "huge", "reason": "Literal BigInt"}]),
        "the out-of-band multiplier must be declined by its MIR node"
    );
}

#[test]
fn certify_fueled_recursion_generality_lake_builds_kernel_clean() {
    if !lean_required::lake_available() {
        eprintln!("skipping certify recursion test: `lake` not available");
        return;
    }

    let (out_dir, manifest) =
        certify_fixture("tools/certkit/fixtures/recgen.av", &[], "certify-recgen");
    let cert_dir = out_dir.join("cert");
    // Additive, constant-step, trailing-addition, multiplicative and
    // two-argument accumulator recursions are all total plans.
    for name in ["sumFrom", "constPlus", "backward", "factorial", "countDown"] {
        assert_certified_as(&manifest, name, &["recursive", "calls"], true);
    }
    let contracts = manifest["runtime_contracts"].as_array().unwrap();
    for contract in [
        aver::codegen::cert::INT_ADD_TOTAL_CONTRACT,
        aver::codegen::cert::INT_SUB_TOTAL_CONTRACT,
        aver::codegen::cert::INT_MUL_CONTRACT,
        aver::codegen::cert::INT_MUL_TOTAL_CONTRACT,
    ] {
        assert!(
            contracts.iter().any(|c| c == contract),
            "recgen must be conditional on `{contract}`: {contracts:?}"
        );
    }

    // Load-bearing contract check: the derived totality role selects the
    // multiplication premise only for the multiplying recursion, and the
    // schema guards that premise by the role.
    materialize_wall(&cert_dir);
    let schema_core = std::fs::read_to_string(cert_dir.join("SchemaCore.lean")).unwrap();
    assert!(
        schema_core.contains("mul : role = .mul →"),
        "HostTotal must require multiplication totality only under the `.mul` role"
    );
    assert_kernel_probe_holds(
        &cert_dir,
        "import ArtifactCertificate\n\
         set_option maxRecDepth 200000\n\
         open AverCert\n\n\
         example : AverCert.manifest.obligations.map (·.export_) =\n    \
         [\"sumFrom\", \"constPlus\", \"backward\", \"factorial\", \"countDown\"] := by decide +kernel\n\
         example : AverCert.manifest.obligations.map (·.totalityRole) =\n    \
         [.addSub, .addSub, .addSub, .mul, .addSub] := by decide +kernel\n\
         example : AverCert.manifest.obligations.map (·.policy) =\n    \
         [.simulatesModelTotally, .simulatesModelTotally, .simulatesModelTotally,\n     \
         .simulatesModelTotally, .simulatesModelTotally] := by decide +kernel\n",
        "recgen totality roles",
    );

    // A wrong generated SOURCE definition leaves the export certified (its
    // model is its plan) but costs exactly its plan-equals-source bridge.
    for (name, honest, hostile) in [
        (
            "factorial",
            "else (n * factorial (n - 1)))",
            "else (2 * factorial (n - 1)))",
        ),
        (
            "countDown",
            "else countDown (n - 1) (acc + n))",
            "else countDown (n - 1) (acc + 2))",
        ),
    ] {
        let tampered = temp_dir(&format!("certify-hostile-{name}-definition"));
        copy_dir_all(&out_dir, &tampered);
        let model = tampered.join("cert/AverModel/RecGen.lean");
        let source = std::fs::read_to_string(&model).unwrap();
        let edited = source.replacen(honest, hostile, 1);
        assert_ne!(
            source, edited,
            "{name} source model changed; update the hostile-model regression"
        );
        std::fs::write(&model, edited).unwrap();

        let (ok, report) = check_certificate(&tampered.join("recgen.wasm"), &tampered.join("cert"));
        assert!(
            ok && report.contains("5 checked exports")
                && report.contains("source-bridges: 4 of 5 credited")
                && report.contains(&format!(
                    "source-bridge not credited: {name} (proof depends on sorryAx)"
                )),
            "a wrong {name} source definition must cost exactly its bridge:\n{report}"
        );
    }
}

#[test]
fn certify_mutual_recursion_scc_lake_builds_kernel_clean() {
    if !lean_required::lake_available() {
        eprintln!("skipping certify mutual-recursion test: `lake` not available");
        return;
    }

    // A two-member SCC (`isEven`/`isOdd`) and a three-member cycle
    // (`rotA -> rotB -> rotC -> rotA`): every member is certified totally,
    // and all members of the cycle share ONE call group in `fnPlans`.
    let cases: [(&str, &[&str]); 2] = [
        ("tools/certkit/fixtures/mutual.av", &["isEven", "isOdd"]),
        (
            "tools/certkit/fixtures/mutual3.av",
            &["rotA", "rotB", "rotC"],
        ),
    ];

    for (fixture, exports) in cases {
        let (out_dir, manifest) = certify_fixture(fixture, &[], "certify-mutual");
        let cert_dir = out_dir.join("cert");
        for name in exports {
            assert_certified_as(&manifest, name, &["recursive", "mutual", "calls"], true);
        }
        let plans = std::fs::read_to_string(cert_dir.join("Plans.lean")).unwrap();
        let groups: BTreeSet<String> = exports
            .iter()
            .map(|name| {
                let head = format!("⟨\"{name}\", true, ");
                let at = plans.find(&head).unwrap() + head.len();
                plans[at..].split(',').nth(1).unwrap().trim().to_string()
            })
            .collect();
        assert_eq!(
            groups.len(),
            1,
            "all members of the cycle must share one call group in {fixture}:\n{plans}"
        );
        assert_certificate_target_builds(&cert_dir, fixture);
    }
}

#[test]
fn certify_verbatim_variant_dispatch_lake_builds_kernel_clean() {
    if !lean_required::lake_available() {
        eprintln!("skipping certify verbatim-variant-dispatch test: `lake` not available");
        return;
    }

    // A match whose every arm is a distinct String literal: each arm reads its
    // literal from a passive data segment the type table declares, and the
    // wall confirms every declared segment against the data section.
    let (out_dir, manifest) = certify_fixture(
        "tools/certkit/fixtures/strdispatch.av",
        &[],
        "certify-strdispatch",
    );
    let cert_dir = out_dir.join("cert");
    assert_certified_as(&manifest, "tagName", &["variants", "strings"], false);
    let plans = std::fs::read_to_string(cert_dir.join("Plans.lean")).unwrap();
    let block = export_plan_block(&plans, "tagName");
    assert!(
        block.contains(".match_") && block.contains("(.literal (.str ["),
        "tagName's plan must dispatch to String literals:\n{block}"
    );
    assert!(
        plans.contains("strSegs := [(["),
        "the type table must declare the literal data segments:\n{plans}"
    );
    assert_certificate_target_builds(&cert_dir, "verbatim variant dispatch");
}

#[test]
fn certify_string_eq_host_contract_lake_builds_kernel_clean() {
    let (out_dir, manifest) = certify_fixture(
        "tools/certkit/fixtures/stringeq.av",
        &[],
        "certify-stringeq",
    );
    let cert_dir = out_dir.join("cert");
    assert_plans_lean_is_the_only_public_plan_data(&cert_dir, &manifest);
    assert_certified_as(&manifest, "quoteOrSelf", &["strings"], false);
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
    // The String.eq helper is classified and declared in the subject.
    let string_roles = manifest["stringHostRoles"].as_array().unwrap();
    assert_eq!(
        string_roles.len(),
        1,
        "one String.eq helper: {string_roles:?}"
    );
    assert_eq!(string_roles[0]["role"], "stringEq");
    let helper = string_roles[0]["function_index"].as_u64().unwrap();
    let manifest_lean = std::fs::read_to_string(cert_dir.join("Manifest.lean")).unwrap();
    assert!(
        manifest_lean.contains(&format!("stringHostRoles := [({helper}, .eq)]")),
        "the Lean subject must declare the same String.eq helper:\n{manifest_lean}"
    );
    // The plan keeps the needle, the hit literal and the default input.
    let plans = std::fs::read_to_string(cert_dir.join("Plans.lean")).unwrap();
    let block = export_plan_block(&plans, "quoteOrSelf");
    assert!(
        block.contains("(.litStr [34])")
            && block.contains("(.literal (.str [92, 34]))")
            && block.contains("(.cons .wild (.local 0) .nil)"),
        "String.eq plan must preserve the needle, the hit literal and the default:\n{block}"
    );
    assert!(
        plans.contains("strSegs := [([34], 0), ([92, 34], 1)]"),
        "both literals must be declared against their data segments:\n{plans}"
    );
    // That the plan types, lowers to exactly the code entry and reads its
    // literals from the declared segments is what acceptance proves; a green
    // run on this fixture is pinned by
    // `cert_verify_declines_tampered_string_eq_helper_shape`'s honest baseline.
}

#[test]
fn certify_string_concat_host_contract_lake_builds_kernel_clean() {
    if !lean_required::lake_available() {
        eprintln!("skipping certify String.concat host-contract test: `lake` not available");
        return;
    }

    let (out_dir, manifest) = certify_fixture(
        "tools/certkit/fixtures/stringconcat.av",
        &[],
        "certify-stringconcat",
    );
    let cert_dir = out_dir.join("cert");
    assert_plans_lean_is_the_only_public_plan_data(&cert_dir, &manifest);
    assert_certified_as(&manifest, "shout", &["strings"], false);
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
    let string_roles = manifest["stringHostRoles"].as_array().unwrap();
    assert_eq!(
        string_roles
            .iter()
            .filter(|entry| entry["role"] == "stringConcat")
            .count(),
        1,
        "one String.concat helper: {string_roles:?}"
    );
    let plans = std::fs::read_to_string(cert_dir.join("Plans.lean")).unwrap();
    let block = export_plan_block(&plans, "shout");
    assert!(
        block.contains("(.binOp .add (.local 0) (.literal (.str [33])))"),
        "shout's plan must concatenate the literal suffix:\n{block}"
    );
    assert!(
        plans.contains("strSegs := [([33], 0)]") && plans.contains("strVec := some "),
        "the suffix literal and the concatenation container must be declared:\n{plans}"
    );
    assert_certificate_target_builds(&cert_dir, "String.concat host contract");
}

#[test]
fn certify_composition_fixture_lake_builds_kernel_clean() {
    if !lean_required::lake_available() {
        eprintln!("skipping certify composition test: `lake` not available");
        return;
    }

    let (out_dir, manifest) =
        certify_fixture("tools/certkit/fixtures/compose.av", &[], "certify-compose");
    let cert_dir = out_dir.join("cert");
    // `quad` calls `double` twice, and `hex16` calls `quad` twice: a chain
    // calling a chain. Every call reaches a planned function of an EARLIER
    // call group, and every callee is itself byte-checked.
    assert_certified_as(&manifest, "double", &[], false);
    assert_certified_as(&manifest, "quad", &["calls"], false);
    assert_certified_as(&manifest, "hex16", &["calls"], false);
    let plans = std::fs::read_to_string(cert_dir.join("Plans.lean")).unwrap();
    assert!(
        export_plan_block(&plans, "quad")
            .contains("(.call (.fn 1) [(.call (.fn 1) [(.local 0)])])")
            && export_plan_block(&plans, "hex16")
                .contains("(.call (.fn 2) [(.call (.fn 2) [(.local 0)])])"),
        "the composition plans must call their callees by function index:\n{plans}"
    );
    assert_certificate_target_builds(&cert_dir, "composition");
}

/// Non-recursive ADT witness fixtures, as `(source, prefix, expected exports
/// with their facets)`.
///
/// Each entry compiles its own certificate package and `lake build`s its
/// acceptance root, so the list used to be ten full Lean builds run back to
/// back inside a single test — the longest serial chain in the certify suite.
///
/// This list is the single source of truth for which fixtures the gate covers,
/// and it deliberately stays a list. The shard tests below select entries by
/// `idx % NONRECURSIVE_ADT_WITNESS_SHARDS`, never by name, so a fixture
/// appended here is automatically exercised by exactly one existing shard: no
/// new test function to write, no CI filter to update, nothing to forget.
const NONRECURSIVE_ADT_WITNESS_CASES: &[(&str, &str, &[(&str, &[&str])])] = &[
    (
        "tools/certkit/fixtures/opteval.av",
        "opteval",
        &[("mk", &["variants"]), ("eval", &["variants"])],
    ),
    (
        "examples/core/user_record.av",
        "user-record",
        &[("greet", &["records"]), ("isAdult", &["records"])],
    ),
    (
        "tools/certkit/fixtures/tupleproj.av",
        "tuple-proj",
        &[("pairFst", &["records"]), ("pairSnd", &["records"])],
    ),
    (
        "tools/certkit/fixtures/widenedmatch.av",
        "widened-match",
        &[("boxInt", &["variants"])],
    ),
    (
        "tools/certkit/fixtures/rangepred.av",
        "range-pred",
        &[("inAsciiDigit", &[])],
    ),
    (
        "tools/certkit/fixtures/verbatimwiden.av",
        "verbatim-widen",
        &[("wrapItems", &["variants"])],
    ),
    (
        "tools/certkit/fixtures/f64verbatim.av",
        "f64-verbatim",
        &[("floatOrZero", &["variants", "floats"])],
    ),
    // Four constructors, mixed arm semantics (negation, offset addition,
    // identity, non-zero default).
    (
        "tools/certkit/fixtures/signalgauge.av",
        "signal-gauge",
        &[("gauge", &["variants"])],
    ),
    (
        "tools/certkit/fixtures/intdispatchgen.av",
        "int-dispatch-gen",
        &[("boxInt", &["variants"]), ("gauge", &["variants"])],
    ),
    // Payload-first subtraction, constant-first addition, and payload
    // variants elided into the wildcard default.
    (
        "tools/certkit/fixtures/meter.av",
        "meter",
        &[("readout", &["variants"])],
    ),
];

/// How many parallel shards `NONRECURSIVE_ADT_WITNESS_CASES` is spread over:
/// one test function per shard. Keep it at most the list length so no shard
/// runs empty (an empty shard would pass vacuously); the runner asserts that.
const NONRECURSIVE_ADT_WITNESS_SHARDS: usize = 4;

/// Runs the `NONRECURSIVE_ADT_WITNESS_CASES` entries that belong to `shard`.
fn assert_nonrecursive_adt_witness_shard_lake_builds_kernel_clean(shard: usize) {
    assert!(
        shard < NONRECURSIVE_ADT_WITNESS_SHARDS
            && NONRECURSIVE_ADT_WITNESS_SHARDS <= NONRECURSIVE_ADT_WITNESS_CASES.len(),
        "shard {shard} of {NONRECURSIVE_ADT_WITNESS_SHARDS} covers no ADT witness fixture: keep the shard count at most the list length, one test function per shard"
    );
    if !lean_required::lake_available() {
        eprintln!("skipping certify ADT test: `lake` not available");
        return;
    }

    // Index-sharded rather than name-selected: every entry of the list lands in
    // exactly one shard by construction, including entries added later.
    for (idx, &(input, prefix, expected)) in NONRECURSIVE_ADT_WITNESS_CASES.iter().enumerate() {
        if idx % NONRECURSIVE_ADT_WITNESS_SHARDS != shard {
            continue;
        }
        let (out_dir, manifest) = certify_fixture(input, &[], prefix);
        for &(name, facets) in expected {
            assert_certified_as(&manifest, name, facets, false);
        }
        assert_certificate_target_builds(&out_dir.join("cert"), &format!("ADT cert for {input}"));
    }
}

/// Non-recursive ADT witnesses, shard 0: `NONRECURSIVE_ADT_WITNESS_CASES`
/// entries 0, 4, 8, ... — today `opteval`, `range-pred` and `int-dispatch-gen`.
#[test]
fn cert_adt_witness_shard_0_of_4_lake_builds_kernel_clean() {
    assert_nonrecursive_adt_witness_shard_lake_builds_kernel_clean(0);
}

/// Non-recursive ADT witnesses, shard 1: `NONRECURSIVE_ADT_WITNESS_CASES`
/// entries 1, 5, 9, ... — today `user-record`, `verbatim-widen` and `meter`.
#[test]
fn cert_adt_witness_shard_1_of_4_lake_builds_kernel_clean() {
    assert_nonrecursive_adt_witness_shard_lake_builds_kernel_clean(1);
}

/// Non-recursive ADT witnesses, shard 2: `NONRECURSIVE_ADT_WITNESS_CASES`
/// entries 2, 6, 10, ... — today `tuple-proj` and `f64-verbatim`.
#[test]
fn cert_adt_witness_shard_2_of_4_lake_builds_kernel_clean() {
    assert_nonrecursive_adt_witness_shard_lake_builds_kernel_clean(2);
}

/// Non-recursive ADT witnesses, shard 3: `NONRECURSIVE_ADT_WITNESS_CASES`
/// entries 3, 7, 11, ... — today `widened-match` and `signal-gauge`.
#[test]
fn cert_adt_witness_shard_3_of_4_lake_builds_kernel_clean() {
    assert_nonrecursive_adt_witness_shard_lake_builds_kernel_clean(3);
}

/// Guard the ADT witness shard count against drifting away from its test
/// functions.
///
/// The shard runner already fails when the list shrinks below its shard count.
/// The opposite direction is the silent one: RAISING
/// `NONRECURSIVE_ADT_WITNESS_SHARDS` without adding the matching
/// `shard_N_of_M` test means every fixture whose index has that remainder is
/// simply never built, and every remaining test still passes. Nothing in the
/// type system ties a constant to the number of `#[test]` functions, so this
/// reads the source of this file and counts them.
///
/// Deliberately outside the `cert_adt_witness_` prefix: it needs no `lake` and
/// belongs on the fast lane, not on a kernel-heavy one.
#[test]
fn certify_adt_witness_shards_all_have_test_functions() {
    let source = include_str!("cert_certify_spec.rs");
    let shards = NONRECURSIVE_ADT_WITNESS_SHARDS;
    for shard in 0..shards {
        let expected =
            format!("fn cert_adt_witness_shard_{shard}_of_{shards}_lake_builds_kernel_clean");
        assert!(
            source.contains(&expected),
            "ADT witness shard {shard} of {shards} has no test function, so fixtures with \
             idx % {shards} == {shard} are never built; add `{expected}`"
        );
    }
}

/// Single-use let-renamed certificate shapes: a let-renamed Option match
/// (`named`), a let-named integer increment (`addTwoNamed`), and a let-named
/// comparison feeding the branch (`inRangeNamed`). The MIR optimizer performs
/// no copy propagation, so each keeps its `Let` node; all three must certify
/// and the emitted package must close under lake.
#[test]
fn certify_let_named_shapes_certify_and_lake_build() {
    let (out_dir, manifest) = certify_fixture(
        "tools/certkit/fixtures/letnamed.av",
        &[],
        "certify-letnamed",
    );
    assert_certified_as(&manifest, "named", &["variants"], false);
    assert_certified_as(&manifest, "addTwoNamed", &[], false);
    assert_certified_as(&manifest, "inRangeNamed", &[], false);
    assert!(
        manifest["source_level_only"]
            .as_array()
            .is_none_or(|declined| declined.is_empty()),
        "no let-named shape may decline to source-level-only: {manifest:#}"
    );

    if !lean_required::lake_available() {
        eprintln!("skipping letnamed lake build: `lake` not available");
        return;
    }
    assert_certificate_target_builds(&out_dir.join("cert"), "let-named shapes");
}

/// Arity-3 Int/Bool plan: three Int params, a branch on the first and a
/// constant comparison of the second or third. The plan grammar is n-ary
/// throughout; this pins the three-parameter signature, the three-argument
/// source bridge, and that the package closes under lake.
#[test]
fn certify_arity_three_fragment_certifies_and_lake_builds() {
    let (out_dir, manifest) =
        certify_fixture("tools/certkit/fixtures/arity3.av", &[], "certify-arity3");
    assert_certified_as(&manifest, "tripleCheck", &[], false);
    let plans = std::fs::read_to_string(out_dir.join("cert").join("Plans.lean"))
        .expect("Plans.lean exists");
    assert!(
        export_plan_block(&plans, "tripleCheck").contains("sig := ⟨[.int, .int, .int], .bool⟩"),
        "tripleCheck's plan must take three Int parameters:\n{plans}"
    );
    let bridge = manifest["sourceBridges"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["export"] == "tripleCheck")
        .unwrap_or_else(|| panic!("tripleCheck must carry a source bridge: {manifest:#}"));
    assert_eq!(
        bridge["params"],
        serde_json::json!([{"kind": "int"}, {"kind": "int"}, {"kind": "int"}]),
        "the bridge must decode all three parameters"
    );

    if !lean_required::lake_available() {
        eprintln!("skipping arity3 lake build: `lake` not available");
        return;
    }
    assert_certificate_target_builds(&out_dir.join("cert"), "arity-3 plan");
}

/// The s33 heap-type boundary: 16 nominal sum roots plus 46 user variant
/// structs push the Int carrier to wasm type index 64, the first index whose
/// signed s33 encoding (`c0 00`) differs from unsigned LEB (`40`). The plan's
/// lowering writes the carrier index inside local declarations, block types
/// and the declared function type, so a lowerer that emitted unsigned LEB
/// would fail its own byte equality here.
#[test]
fn certify_carrier_at_type_index_64_lake_builds_kernel_clean() {
    if !lean_required::lake_available() {
        eprintln!("skipping s33 boundary test: `lake` not available");
        return;
    }

    let (out_dir, manifest) = certify_fixture(
        "tools/certkit/fixtures/manytypes.av",
        &[],
        "certify-manytypes",
    );
    assert_eq!(
        manifest["carrier_type_index"].as_u64(),
        Some(64),
        "fixture must pin the carrier exactly at the s33 boundary index 64; \
         adjust the fixture's variant count if the emitter's type layout changed"
    );
    assert_certified_as(&manifest, "sumBig", &["recursive", "calls"], true);
    assert_certificate_target_builds(&out_dir.join("cert"), "the s33 boundary cert");
}

/// A module with no Int carrier certifies exactly its carrier-free classes and
/// declares the carrierless state truthfully.
///
/// This test used to assert the opposite half — that `greet` and `shout` were
/// NOT certified, with the exact no-Int-helper decline reason — as an
/// anti-false-positive tripwire. Certifying them is now the deliberate result of
/// teaching `string-concat-v1` to lower in both carrier states, so the tripwire
/// is INVERTED here rather than deleted, and its original intent is kept as the
/// explicit assertion below that no integer-family class appears: those classes
/// all cite an arith host role, an admitted arith table requires a byte-derived
/// carrier struct, and this module has neither. A carrier-free class appearing
/// here is the reviewed new fact; an integer-family class appearing here would
/// still be a false positive.
#[test]
fn certify_certifies_carrier_free_classes_in_a_module_without_int_helper() {
    // No `lake` needed: this is a pure emitter no-abort check. hello.av has
    // zero Int arithmetic, so its emitted module carries neither the Int
    // carrier type nor the `__rt_aint_from_i64` box helper export. The
    // certificate producer must still emit the `cert/` package with every
    // export either certified or declared uncertified with a readable reason
    // — never exit 1 with a whole-module error.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-no-int-helper");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/core/hello.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    assert!(
        compile.status.success(),
        "compile --certify must not abort on a module without the Int box helper:\n{report}"
    );
    assert!(
        !report.contains("module has no __rt_aint_from_i64 box helper"),
        "the whole-module abort must be gone:\n{report}"
    );
    assert!(
        out_dir.join("hello.wasm").is_file(),
        "wasm artifact must be written"
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    assert!(
        cert_dir.join("Plans.lean").is_file(),
        "the certificate package must be written next to the wasm artifact"
    );

    // Every module export appears either as certified or as declared
    // uncertified with a non-empty human-readable reason.
    let certified: BTreeSet<String> = manifest["certified"]
        .as_array()
        .expect("certified report is an array")
        .iter()
        .map(|c| c["name"].as_str().unwrap().to_string())
        .collect();
    let declared: BTreeMap<String, String> = manifest["declaredUncertified"]
        .as_array()
        .expect("declaredUncertified report is an array")
        .iter()
        .map(|entry| {
            (
                entry["name"]
                    .as_str()
                    .expect("entry has a name")
                    .to_string(),
                entry["reason"]
                    .as_str()
                    .expect("entry has a reason")
                    .to_string(),
            )
        })
        .collect();
    for (name, reason) in &declared {
        assert!(
            !reason.trim().is_empty(),
            "declared-uncertified export `{name}` must carry a readable reason"
        );
    }
    for export in ["greet", "shout", "main"] {
        assert!(
            certified.contains(export) || declared.contains_key(export),
            "export `{export}` must be certified or declared uncertified, got \
             certified={certified:?} declared={declared:?}"
        );
    }

    // hello.wasm has no Int carrier at all. The manifest must truthfully
    // declare the ABSENCE of the host-role table (`null`), never a fabricated
    // all-null table, because the in-kernel pin equates the manifest value with
    // the byte decoder's result and the decoder resolves no table for a
    // carrierless module. These facts are MORE load-bearing than they were when
    // this module could carry no claims at all: they are now what keeps the
    // arith roles unciteable while carrier-free claims ride alongside them.
    assert_eq!(
        manifest["carrier_type_index"],
        serde_json::Value::Null,
        "hello.wasm must not declare an Int carrier type"
    );
    assert_eq!(
        manifest["hostRoleTable"],
        serde_json::Value::Null,
        "a carrierless module has no host-role table; the manifest must say so"
    );
    let manifest_lean =
        std::fs::read_to_string(cert_dir.join("Manifest.lean")).expect("Manifest.lean exists");
    assert!(
        manifest_lean.contains("hostRoleTable := (none : Option CertDecode.AddSub.Roles)"),
        "the Lean manifest must declare the absent host-role table as `none`"
    );
    let plans = std::fs::read_to_string(cert_dir.join("Plans.lean")).expect("Plans.lean exists");
    assert!(
        plans.contains("carrier := none, mag := none"),
        "the type table must declare the absent carrier:\n{plans}"
    );

    // Both String concatenations certify: their plans read no carrier and
    // lower to no arithmetic helper call.
    for export in ["greet", "shout"] {
        assert_certified_as(&manifest, export, &["strings"], false);
    }
    // The original tripwire's intent, preserved: no plan in a carrierless
    // module may use Int arithmetic, since the absent role table lowers every
    // arithmetic call to an index no code entry encodes.
    for export in ["greet", "shout"] {
        let block = export_plan_block(&plans, export);
        assert!(
            [".int ", ".int]", ".int,", ".int)"]
                .iter()
                .all(|int| !block.contains(int)),
            "`{export}` must not mention an Int in a carrierless module:\n{block}"
        );
    }

    assert!(
        !certified.contains("main"),
        "`main` is an effectful zero-argument export and must not certify"
    );
    // `main` prints. That is what stops it being a pure plan, and it is what
    // the report must say.
    assert_eq!(
        declared.get("main").map(String::as_str),
        Some("fn declares effects"),
        "`main` must decline for the effect it performs, not for its parameter \
         count and not for the missing Int helper"
    );
}

/// Each declined export must be told what is actually stopping it. The plan
/// printer declines by naming the MIR node it has no grammar for (a builtin,
/// a pattern, an operator, an interpolation part), the producer declines a
/// plan that does not type or a call to an unplanned function by saying so,
/// and an effectful function by its effects. The Fibonacci example carries
/// one export per blocker, so it pins the vocabulary at once.
#[test]
fn certify_declines_name_the_blocker_that_actually_applies() {
    let (_out_dir, manifest) = certify_fixture(
        "examples/data/fibonacci.av",
        &[],
        "certify-decline-blockers",
    );
    let declared: BTreeMap<String, String> = manifest["declaredUncertified"]
        .as_array()
        .expect("declaredUncertified report is an array")
        .iter()
        .map(|entry| {
            (
                entry["name"]
                    .as_str()
                    .expect("entry has a name")
                    .to_string(),
                entry["reason"]
                    .as_str()
                    .expect("entry has a reason")
                    .to_string(),
            )
        })
        .collect();

    for (export, reason) in [
        // The printer names the MIR node it has no grammar for.
        ("finalizeFibStats", "Call Builtin(List.reverse)"),
        ("nthOrZero", "Match pattern EmptyList"),
        ("goldenApprox", "BinOp Div"),
        ("showGolden", "InterpolatedStr (a part is not a String)"),
        // A printed plan the one grammar does not type.
        ("absF", "plan does not type in the one grammar"),
        // A call to a function without a certified plan.
        (
            "buildFibStats",
            "calls function 8, which has no certified plan",
        ),
        // An effectful function has no pure plan at all.
        ("main", "fn declares effects"),
        ("printStats", "fn declares effects"),
    ] {
        assert_eq!(
            declared.get(export).map(String::as_str),
            Some(reason),
            "`{export}` must decline with the blocker that actually applies to it"
        );
    }
    // The pure recursions that used to decline on the retired families'
    // arity limits are certified plans now.
    for name in ["fibTR", "fib", "fibSpec", "bigger"] {
        assert!(
            !declared.contains_key(name),
            "`{name}` must be certified, not declined: {declared:?}"
        );
    }
}

/// This test used to duplicate, line for line, `aver cert verify` on the same
/// carrierless `hello.av` compile: the same carrierless check with the
/// identical "hello.av must stay carrierless for this test to mean anything"
/// message, the same `aver cert verify` invocation, the same
/// CERTIFIED/2-exports/greet+shout/no-DECLINED assertions.
/// `cert_verify_certifies_string_concat_in_a_carrierless_module`
/// (`tests/cert_verify_spec.rs`) is that same pipeline with the identical
/// carrierless check plus strictly more: it also pins the type table's
/// `carrier := none`, the manifest's null `hostRoleTable`/`carrier_type_index`,
/// the one plan class for both exports, and two carrier-claim tampers. Restating it here
/// paid for a second full Lean verification in CI for no additional guarantee,
/// so this comment stands in for the test: if that one is ever deleted or
/// repointed at a module that is no longer carrierless, the carrierless
/// acceptance-pin coverage this test used to provide is lost.
#[cfg(test)]
const _CARRIERLESS_ACCEPTANCE_PIN_COVERAGE_NOTE: () = ();

/// The admission-only path keeps its coverage — it just no longer has a reason
/// to live in this file. `empty_cert_is_admission_only_and_exits_nonzero`
/// (`tests/cert_verify_spec.rs`) runs the same `compile --certify` then
/// `cert verify` pipeline on `tools/certkit/fixtures/certempty.av`, whose only
/// export measures a `String` through `String.len` — a builtin the plan grammar
/// has no node for (`Call Builtin(String.len)`) — and pins the same banner, the
/// same nonzero exit, and the absence of the green path. Restating it here would duplicate a full Lean verification
/// in a second CI lane for no additional guarantee, so this comment stands in
/// for the test: if that one is ever deleted or repointed at a module that
/// certifies something, the admission-only verdict loses its only coverage.
#[cfg(test)]
const _ADMISSION_ONLY_COVERAGE_NOTE: () = ();

/// The two Int comparison helpers are exported exactly when the emitted code
/// calls them, and each role decides on its own. A named export is a
/// tree-shaking root, so an unconditional export would keep the comparison
/// helper (and, for `__aint_cmp`, its shared sub-routines) in every module
/// that touches `Int` — `const_cmp_bound_check_dces_aint_cmp_helpers` in
/// `tests/wasm_gc_carrier_i64_differential.rs` measures that side in bytes.
/// What this pins is the certificate surface: the declared role table follows
/// the exports, so a module that compares nothing declares both roles `null`
/// truthfully, and a module that compares binds exactly the helpers it calls.
///
/// The literal case is the load-bearing one for the size claim: `a >= 100`
/// lowers to the specialized carrier-shape test with no call at all, so it
/// must not export the helper the general `a >= b` does.
#[test]
fn comparison_helper_exports_follow_the_emitted_calls() {
    let cases: [(&str, &str, bool, bool); 5] = [
        (
            "nocompare",
            "fn f(a: Int, b: Int) -> Int\n    a + b\n",
            false,
            false,
        ),
        (
            "eqonly",
            "fn f(a: Int, b: Int) -> Bool\n    a == b\n",
            false,
            true,
        ),
        (
            "cmponly",
            "fn f(a: Int, b: Int) -> Bool\n    a >= b\n",
            true,
            false,
        ),
        (
            "bothcompare",
            "fn f(a: Int, b: Int) -> Bool\n    a >= b\n\nfn g(a: Int, b: Int) -> Bool\n    a == b\n",
            true,
            true,
        ),
        (
            "literalcompare",
            "fn f(a: Int) -> Bool\n    a >= 100\n",
            false,
            false,
        ),
    ];
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-comparison-export-liveness");
    std::fs::create_dir_all(&out_dir).unwrap();
    for (name, body, wants_cmp, wants_eq) in cases {
        let source = format!(
            "module Probe\n    intent =\n        \"Comparison host-role export liveness probe.\"\n    effects []\n\n{body}"
        );
        let av = out_dir.join(format!("{name}.av"));
        std::fs::write(&av, source).unwrap();
        let build = out_dir.join(name);
        let compile = aver_command()
            .current_dir(&repo_root)
            .arg("compile")
            .arg(&av)
            .arg("--target")
            .arg("wasm-gc")
            .arg("--certify")
            .arg("-o")
            .arg(&build)
            .output()
            .expect("aver compile --certify runs");
        assert!(
            compile.status.success(),
            "{name} --certify failed:\n{}{}",
            String::from_utf8_lossy(&compile.stdout),
            String::from_utf8_lossy(&compile.stderr)
        );
        let wasm = std::fs::read(build.join(format!("{name}.wasm"))).unwrap();
        let box_idx = wasm_export_index(&wasm, "__rt_aint_from_i64");
        let cmp_idx = wasm_export_index(&wasm, "__aint_cmp");
        let eq_idx = wasm_export_index(&wasm, "__aint_eq");
        assert!(
            box_idx.is_some(),
            "{name} touches Int, so the carrier box helper must stay exported"
        );
        assert_eq!(
            cmp_idx.is_some(),
            wants_cmp,
            "{name}: `__aint_cmp` export must follow whether the emitted code calls it"
        );
        assert_eq!(
            eq_idx.is_some(),
            wants_eq,
            "{name}: `__aint_eq` export must follow whether the emitted code calls it"
        );
        // The certificate's declared table says exactly the same thing: an
        // unexported helper is an honestly absent role, never a role bound to
        // a function nothing names (`cmp` is pinned to its export name by the
        // wall; `eq` to its template, and the producer declares it only at
        // the helper the emitted code calls).
        let manifest: serde_json::Value = serde_json::from_str(
            &std::fs::read_to_string(build.join("cert").join("cert-manifest.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(
            manifest["hostRoleTable"]["cmp"],
            cmp_idx.map_or(serde_json::Value::Null, |index| serde_json::json!(index)),
            "{name}: declared `cmp` role must match the export section"
        );
        assert_eq!(
            manifest["hostRoleTable"]["eq"],
            eq_idx.map_or(serde_json::Value::Null, |index| serde_json::json!(index)),
            "{name}: declared `eq` role must match the export section"
        );
    }
}

#[test]
fn certify_add_one_output_is_unchanged_when_the_int_helper_is_present() {
    // Regression guard for the optional-helper change: a module that DOES
    // export `__rt_aint_from_i64` must certify exactly as before. The
    // expectations below are the pre-change add_one certification facts;
    // if the carrierless handling ever leaks into the carriered path (a
    // missing certification, a reclassification, or a `null` host-role
    // table), this fails.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-add-one-regression");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/certification/add_one.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    assert!(
        compile.status.success(),
        "add_one --certify failed:\n{report}"
    );
    assert!(
        report.contains("1 certified"),
        "add_one must report exactly one certified export:\n{report}"
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");

    // The certified entry, in the one plan class (deliberately moved from the
    // retired `expr-fragment-v1` class and its compute-face theorem; the
    // manifest no longer carries the declared-only `dom`/`cod` strings).
    assert_eq!(
        manifest["certified"],
        serde_json::json!([{
            "name": "addOne",
            "class": "source-plan-v1",
            "facets": [],
            "policy": "simulatesModel",
            "level": "L1",
            "theorem": "AcceptanceSoundness.fn_claim_discharges",
        }]),
        "the add_one certification must be unchanged by the optional-helper handling"
    );
    assert_eq!(
        manifest["runtime_contracts"],
        serde_json::json!([
            "__rt_aint_from_i64 (box i64 -> carrier)",
            "Int.add (carrier add = exact integer addition on represented values; result canonical)",
        ]),
        "add_one's runtime contracts must be unchanged"
    );

    // The host-role table stays a concrete OBJECT — never `null` — and every
    // arithmetic role is bound for the full Int runtime, the name-bound ones
    // at their exports.
    let wasm = std::fs::read(out_dir.join("add_one.wasm")).expect("wasm artifact exists");
    let table = &manifest["hostRoleTable"];
    for role in ["box", "add", "mul", "sub", "toIndex", "divmod"] {
        assert!(
            table[role].is_u64(),
            "add_one carries the full Int arithmetic runtime; `{role}` must bind: {table}"
        );
    }
    assert_eq!(
        table["box"].as_u64(),
        wasm_export_index(&wasm, "__rt_aint_from_i64").map(u64::from)
    );
    assert_eq!(
        table["toIndex"].as_u64(),
        wasm_export_index(&wasm, "__aint_to_index").map(u64::from)
    );
    // `addOne` compares nothing, so the module never calls either comparison
    // helper and therefore does not export one. Both roles are declared absent
    // — truthfully, against bytes that really lack the export.
    assert!(
        table["cmp"].is_null() && table["eq"].is_null(),
        "add_one has no Int comparison; both comparison roles must be absent: {table}"
    );
    assert!(
        manifest["carrier_type_index"].is_u64(),
        "add_one must declare its Int carrier type index"
    );

    // And the Lean subject binds the same table as `some`.
    let manifest_lean =
        std::fs::read_to_string(cert_dir.join("Manifest.lean")).expect("Manifest.lean exists");
    let expected_roles = format!(
        "hostRoleTable := some ({{ box := some {}, add := some {}, mul := some {}, sub := some {}, toIndex := some {}, cmp := none, eq := none, divmod := some {} }} : CertDecode.AddSub.Roles)",
        table["box"], table["add"], table["mul"], table["sub"], table["toIndex"], table["divmod"],
    );
    assert!(
        manifest_lean.contains(&expected_roles),
        "Manifest.lean must pin the declared table, got:\n{manifest_lean}"
    );

    // Golden comparison, independent of the production path that derived the
    // fields above: the emitted envelope and the two authoritative Lean data
    // files are compared verbatim against a committed snapshot. Any change to
    // the emitted certificate content for add_one — a re-keyed manifest, a
    // shifted role table, a new wall identity, a reordered plan — must show
    // up as a reviewed snapshot update, never ride in silently. Refresh with
    // `INSTA_UPDATE=always` (or `cargo insta review`) after an intended
    // producer or wall change.
    let manifest_json = std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
        .expect("cert-manifest.json exists");
    let plans_lean =
        std::fs::read_to_string(cert_dir.join("Plans.lean")).expect("Plans.lean exists");
    let golden = format!(
        "== cert-manifest.json ==\n{manifest_json}\n== Manifest.lean ==\n{manifest_lean}\n== Plans.lean ==\n{plans_lean}"
    );
    insta::assert_snapshot!("add_one_certificate_package", golden);
}

#[cfg(feature = "wasip2")]
#[test]
fn certify_wasip2_component_package_snapshot() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-wasip2-component-snapshot");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tests/fixtures/wasip2_carrierless.av")
        .arg("--target")
        .arg("wasip2")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --target wasip2 --certify runs");
    assert!(
        compile.status.success(),
        "wasip2 certificate emission failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert_dir = out_dir.join("cert");
    let manifest_json = std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
        .expect("cert-manifest.json exists");
    let artifact_lean =
        std::fs::read_to_string(cert_dir.join("Artifact.lean")).expect("Artifact.lean exists");
    let artifact_data = artifact_lean
        .lines()
        .find(|line| line.contains("wasip2ComponentEnvelope := some"))
        .expect("Artifact.data carries the wasip2 envelope");
    let golden = format!(
        "== cert-manifest.json ==\n{manifest_json}\n== Artifact.data envelope ==\n{artifact_data}"
    );
    insta::assert_snapshot!("wasip2_component_certificate_package", golden);
}

#[test]
fn certify_nested_module_models_close_end_to_end() {
    // A project with a dotted module dependency emits its dependency's source
    // model at a nested path (`AverModel/Nested/Deep/Util.lean`). The bridge
    // modules must import it by its dotted module name (never the
    // path-shaped `import AverModel/Nested/Deep/Util`) and name its functions
    // by the QUALIFIED name the model declares, and the package must build.
    if !lean_required::lake_available() {
        eprintln!("skipping nested-module certify test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-nested-modules");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/nestedmods/app.av")
        .arg("--module-root")
        .arg("tools/certkit/fixtures/nestedmods")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    assert!(
        compile.status.success(),
        "nestedmods --certify failed:\n{report}"
    );
    assert!(
        report.contains("6 certified"),
        "nestedmods must certify the entry exports and all three nested-module exports:\n{report}"
    );
    assert!(
        report.contains("Nested_Deep_Util_combine")
            && report.contains("Nested_Deep_Util_bump")
            && report.contains("Nested_Deep_Util_tally"),
        "nestedmods must certify the exports whose models live in the nested module:\n{report}"
    );

    let cert_dir = out_dir.join("cert");
    assert!(
        cert_dir
            .join("AverModel")
            .join("Nested")
            .join("Deep")
            .join("Util.lean")
            .is_file(),
        "the nested dependency model must be emitted at its nested path"
    );
    for file in ["Bridge.lean", "BridgeDefs.lean"] {
        let contents = std::fs::read_to_string(cert_dir.join(file))
            .unwrap_or_else(|_| panic!("{file} exists"));
        assert!(
            contents.contains("import AverModel.Nested.Deep.Util"),
            "{file} must import the nested model by its dotted module name:\n{contents}"
        );
        assert!(
            !contents.contains("import AverModel/Nested"),
            "{file} must not emit a path-shaped import line:\n{contents}"
        );
    }
    // The flattened wasm export name keys the PLAN; the bridge names the
    // source function by the qualified name the model file declares.
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json")).unwrap(),
    )
    .unwrap();
    let tally = manifest["sourceBridges"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["export"] == "Nested_Deep_Util_tally")
        .expect("the nested recursion carries a bridge");
    assert_eq!(tally["model"], "Nested.Deep.Util.tally");
    let bridge = std::fs::read_to_string(cert_dir.join("Bridge.lean")).unwrap();
    assert!(
        bridge.contains("_root_.Nested.Deep.Util.tally x0")
            && !bridge.contains("Nested_Deep_Util_tally x0"),
        "the bridge must cite the qualified source function, never the flattened name:\n{bridge}"
    );

    assert_certificate_target_builds(&cert_dir, "nested module models");
}

/// The producer offers a plan only when its own twin of the wall's checks
/// accepts it, so no plan it ships is one the verifier then rejects.
/// `validClockValue` (`match value >= 0 { false -> false; true -> value < limit }`)
/// becomes `if a >= 0 { a < b } else { false }` after the MIR
/// `bool_match_to_if` rewrite: two Int comparisons nested inside a
/// conditional, the shape the retired families could not place on any face
/// (it used to decline on the legacy byte-classifier route). The one grammar
/// types and lowers it, so it is certified, and the package the producer
/// emits for it checks.
#[test]
fn certify_leaves_a_faceless_host_call_shape_on_the_legacy_route() {
    let (out_dir, manifest) = certify_fixture(
        "tools/certkit/fixtures/clockrange.av",
        &[],
        "cert-clockrange-gate",
    );
    assert_certified_as(&manifest, "validClockValue", &[], false);
    let plans = std::fs::read_to_string(out_dir.join("cert").join("Plans.lean")).unwrap();
    let block = export_plan_block(&plans, "validClockValue");
    assert!(
        block.contains(".ifThenElse (.binOp .gte (.local 0) (.literal (.int 0)))")
            && block.contains("(.binOp .lt (.local 0) (.local 1))"),
        "the nested comparison shape must be the plan:\n{block}"
    );

    if !lean_required::lake_available() {
        eprintln!("skipping clockrange check: `lake` not available");
        return;
    }
    let (ok, report) = check_certificate(&out_dir.join("clockrange.wasm"), &out_dir.join("cert"));
    assert!(
        ok && report.contains("1 checked export"),
        "the producer's offer must be accepted by the wall:\n{report}"
    );
}

/// First contact with a real program tree: the certify pipeline must emit a
/// package for `projects/payment_ops` that builds and reaches a `check`
/// verdict. Regression net for the 2026-08-31 finding that every projects/
/// package failed `aver-cert check`: the carried model lacked `BEq`
/// instances for user enums and its `decreasing_by` was too weak for list
/// recursion under an `if` inside a match arm.
#[test]
fn cert_projects_payment_ops_package_checks() {
    if !lean_required::lake_available() {
        eprintln!("skipping certify projects test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-projects-payment-ops");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("projects/payment_ops/main.av")
        .arg("--module-root")
        .arg("projects/payment_ops")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify payment_ops failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let (ok, report) = check_certificate(&out_dir.join("main.wasm"), &out_dir.join("cert"));
    assert!(
        ok,
        "aver cert check on the payment_ops package failed:\n{report}"
    );
    assert!(
        report.contains("CHECKED"),
        "payment_ops check verdict does not say CHECKED:\n{report}"
    );
    assert!(
        report.contains("59 checked exports"),
        "payment_ops must keep the fifty-nine exports it certifies:\n{report}"
    );
    // The project's single `verify … law` is universal by design but its
    // emitted proof ladder has no `String.replace` theory and lands on its
    // `sorry` floor. That law is not credited — and the exports beside it
    // still are. An uncredited law never sinks a package.
    assert!(
        report.contains("law-claims: 0 of 1 credited"),
        "payment_ops declares one law-claim its proof ladder cannot close:\n{report}"
    );
    assert!(
        report.contains(
            "law-claim not credited: Infra.Codec.unescapeField.escapedRoundtrip \
             (proof depends on sorryAx)"
        ),
        "the uncredited law must be named with the axiom that sank it:\n{report}"
    );
}
