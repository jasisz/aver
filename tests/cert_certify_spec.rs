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

#[path = "support/cert_wall.rs"]
mod cert_wall;
#[path = "support/scratch_dir.rs"]
mod scratch_dir;

use cert_wall::materialize as materialize_wall;
use scratch_dir::{ScratchDir, temp_dir};
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::process::Command;

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

fn assert_certificate_target_builds(cert_dir: &std::path::Path, case: &str) {
    materialize_wall(cert_dir);
    let output = Command::new("lake")
        .current_dir(cert_dir)
        .args(["build", "Certificate"])
        .output()
        .expect("lake builds the isolated Certificate target");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        output.status.success(),
        "honest Certificate target must build before the hostile edit ({case}):\n{combined}"
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

/// Runs `lake build` in an emitted certificate package, asserts it succeeded,
/// and hands back the combined build output for the caller's kernel-audit
/// assertions. The successful build's `.lake` tree is trimmed straight away;
/// see `trim_lean_build_tree`.
fn lake_build_package(cert_dir: &std::path::Path, case: &str) -> String {
    let build = Command::new("lake")
        .current_dir(cert_dir)
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
        "lake build of {case} failed:\n{combined}"
    );
    trim_lean_build_tree(cert_dir);
    combined
}

fn lean_obligation_def<'a>(manifest_lean: &'a str, name: &str) -> &'a str {
    let marker = format!("abbrev {name}Ob : Schema.Obligation :=");
    let start = manifest_lean
        .find(&marker)
        .unwrap_or_else(|| panic!("missing {name} obligation in emitted Manifest.lean"));
    manifest_lean[start..]
        .split_once("\n\n")
        .map_or(&manifest_lean[start..], |(definition, _)| definition)
}

fn assert_plans_lean_is_the_only_public_plan_data(
    cert_dir: &std::path::Path,
    manifest: &serde_json::Value,
) {
    assert!(
        cert_dir.join("Plans.lean").is_file(),
        "Plans.lean must be the package's authoritative plan DATA"
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
                    | "policy"
                    | "level"
                    | "dom"
                    | "cod"
                    | "theorem"
                    | "termination_witness"
            )),
            "{name} manifest entry must remain envelope/report metadata only: {fields:?}"
        );
    }
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
        Some(6),
        "schema 6 adds the component-envelope byte binding while preserving target/profile/ABI pins"
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
    assert_eq!(aver::codegen::cert::CERT_SCHEMA_VERSION, 6);
    let declared_uncertified = manifest["declaredUncertified"].as_array().unwrap();
    assert_eq!(
        declared_uncertified.len(),
        17,
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
    let (box_idx, add_idx, mul_idx, sub_idx, to_index_idx, cmp_idx, eq_idx) =
        aver::codegen::cert::byte_derived_frag_host_role_indices(&wasm).unwrap();
    assert_eq!(
        manifest["hostRoleTable"],
        serde_json::json!({"box": box_idx, "add": add_idx, "mul": mul_idx, "sub": sub_idx, "toIndex": to_index_idx, "cmp": cmp_idx, "eq": eq_idx}),
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
        ("evalOp", "int-dispatch"),
        ("userName", "expr-fragment-v1"),
        ("boxInt", "int-dispatch"),
        ("wrapItems", "verbatim-dispatch"),
        ("tagName", "verbatim-dispatch"),
        ("gauge", "int-dispatch"),
        ("inAsciiDigit", "expr-fragment-v1"),
        ("quoteOrSelf", "verbatim-string-eq"),
        ("shout", "verbatim-string-concat"),
        ("intLessZero", "expr-fragment-v1"),
        ("intEqZero", "expr-fragment-v1"),
        ("boolAndGoal", "expr-fragment-v1"),
        // Eager `Bool.and` over two integer bounds: the `i32.and` fragment
        // primitive over encoded comparisons (numerator moved deliberately).
        ("inWindowGoal", "expr-fragment-v1"),
        ("floatLeGoal", "expr-fragment-v1"),
        ("floatGeGoal", "expr-fragment-v1"),
        ("floatLtGoal", "expr-fragment-v1"),
        ("floatGtGoal", "expr-fragment-v1"),
        ("floatEqGoal", "expr-fragment-v1"),
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
        12,
        "expr-fragment report count changed; update this deliberately"
    );
    let expr_names = expr_entries
        .into_iter()
        .map(|entry| entry["name"].as_str().unwrap().to_string())
        .collect::<BTreeSet<_>>();
    assert_eq!(
        expr_names,
        [
            "addTwo",
            "userName",
            "inAsciiDigit",
            "intLessZero",
            "intEqZero",
            "boolAndGoal",
            "inWindowGoal",
            "floatLeGoal",
            "floatGeGoal",
            "floatLtGoal",
            "floatGtGoal",
            "floatEqGoal",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        "expr-fragment report membership changed"
    );
    let plans_lean = std::fs::read_to_string(out_dir.join("cert").join("Plans.lean"))
        .expect("Plans.lean exists");
    for name in [
        "addTwo",
        "userName",
        "inAsciiDigit",
        "intLessZero",
        "intEqZero",
        "boolAndGoal",
        "inWindowGoal",
        "floatLeGoal",
        "floatGeGoal",
        "floatLtGoal",
        "floatGtGoal",
        "floatEqGoal",
    ] {
        assert!(
            plans_lean.contains(&format!("def {name}SymPlan : SymRawPlan"))
                && plans_lean.contains(&format!("def {name}Plan : ExprFragmentRawPlan")),
            "{name} must keep source and byte-bound plans in authoritative Plans.lean:\n{plans_lean}"
        );
    }
    assert!(
        plans_lean.contains(".constInt (2 : Int)") && plans_lean.contains(".prim .intAdd [0, 1]"),
        "addTwo SymPlan should expose source-level addition by two:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains(".intConstCmp .ge 0 (48 : Int)")
            && plans_lean.contains(".intConstCmp .le 0 (57 : Int)"),
        "inAsciiDigit SymPlan should preserve both source-level bounds:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains(".intConstCmp .eq 0 (0 : Int)"),
        "intEqZero SymPlan should preserve equality with zero:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("def boolAndGoalSymPlan : SymRawPlan")
            && plans_lean.contains("kind := .ifElse 0"),
        "boolAndGoal SymPlan should preserve source-level short-circuiting:\n{plans_lean}"
    );
    // Each param use gets its own node (the `.le` bound reads node 2, a second
    // `.param 0`), mirroring the emitter's per-use `local.get`.
    assert!(
        plans_lean.contains(".prim .boolAnd [1, 3]")
            && plans_lean.contains(".intConstCmp .ge 0 (-100 : Int)")
            && plans_lean.contains(".intConstCmp .le 2 (100 : Int)"),
        "inWindowGoal SymPlan should expose the eager source-level conjunction over both bounds:\n{plans_lean}"
    );
    assert!(
        !plans_lean.contains("floatAddGoalPlan")
            && !plans_lean.contains("floatMulAddGoalPlan")
            && plans_lean.contains("def floatLeGoalSymPlan : SymRawPlan")
            && plans_lean.contains(".prim .floatLe [0, 1]"),
        "only the payload-independent Float comparison should render a certificate plan:\n{plans_lean}"
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
        plans_lean.contains(
            "def mkOpConstructPlan : ConstructRawPlan := ({ profile := \"construct-v1\", arity := 1, fields := [.local 0] } : ConstructRawPlan)"
        ),
        "mkOp should render its concrete target-bound constructor DATA in Plans.lean:\n{plans_lean}"
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
    assert_eq!(mkop_entry["class"], "adt-constructor");
    assert_eq!(
        mkop_entry["theorem"], "AcceptanceSoundness.construct_canonical_discharges",
        "the JSON envelope reports mkOp's checked claim but carries no plan DATA"
    );
    let artifact_lean = std::fs::read_to_string(out_dir.join("cert").join("Artifact.lean"))
        .expect("Artifact.lean exists");
    assert!(
        artifact_lean
            .contains("def symFragmentClaims : List AverCert.AcceptedArtifact.SymFragmentClaim"),
        "artifact should carry source-level fragment claims:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("plan := AverCert.Plans.floatLeGoalSymPlan")
            && !artifact_lean.contains("floatAddGoalSymPlan")
            && !artifact_lean.contains("floatMulAddGoalSymPlan"),
        "only the NaN-payload-independent Float comparison should reach artifact claims:\n{artifact_lean}"
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
    let artifact_certificate =
        std::fs::read_to_string(out_dir.join("cert").join("ArtifactCertificate.lean"))
            .expect("ArtifactCertificate.lean exists");
    assert!(
        artifact_certificate
            .contains("theorem certificate : AverCert.AcceptedArtifact.accepted data :="),
        "artifact root should be a theorem with the exact AcceptedArtifact target:\n{artifact_certificate}"
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
        "idGoal",
        "listHeadGoal",
        "sumListGoal",
    ]
    .into_iter()
    .map(str::to_string)
    .collect();
    assert_eq!(planned_goal_names.len(), 31, "goal denominator changed");
    assert_eq!(actual.len(), 26, "goal numerator changed");

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
    for name in ["sumFrom", "countDown", "isEven", "isOdd"] {
        let obligation = lean_obligation_def(&manifest_lean, name);
        assert!(
            !obligation.contains("totalityRole := .mul") && !obligation.contains("Int.mul"),
            "non-multiplicative L3 obligation {name} gained mul totality:\n{obligation}"
        );
    }

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
    for name in ["floatAddGoal", "floatMulAddGoal"] {
        let reason = manifest["source_level_only"]
            .as_array()
            .unwrap()
            .iter()
            .find(|entry| entry["name"].as_str() == Some(name))
            .and_then(|entry| entry["reason"].as_str())
            .unwrap_or_else(|| panic!("{name} should carry a source-level-only reason"));
        assert!(
            reason.contains("general Wasm allows multiple NaN sign/payload")
                && reason.contains("exact-bit Float output needs a relational result model"),
            "{name} should expose the exact semantic boundary: {reason}"
        );
    }
    assert!(
        declined_names.contains("double"),
        "composition helper should remain reported as source-level-only: {declined_names:?}"
    );
}

#[test]
fn certify_goal_matrix_lands_acceptance_wall_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
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

    let final_lean =
        std::fs::read_to_string(cert_dir.join("Final.lean")).expect("Final.lean exists");
    assert!(
        final_lean.contains("import ArtifactSoundness")
            && final_lean.contains("AverCert.ArtifactSoundness.accept_sound_holds")
            && final_lean.contains("AverCert.Artifact.dischargeSideConditions"),
        "Final.cert must be the single accept-sound capstone application:\n{final_lean}"
    );
    assert!(
        !final_lean.contains("all_goals")
            && !final_lean.contains("first |")
            && !final_lean.contains("_claim_discharges")
            && !final_lean.contains("_canonical_discharges")
            && !final_lean.contains("CertProofs."),
        "Final.cert must not retain generated per-obligation coexistence routing:\n{final_lean}"
    );
    let artifact_lean =
        std::fs::read_to_string(cert_dir.join("Artifact.lean")).expect("Artifact.lean exists");
    for side_condition in [
        "exprFragmentSideConditions",
        "stringEqSideConditions",
        "constructSideConditions",
        "recursionSideConditions",
        "mutualSideConditions",
        "verbatimSideConditions",
        "fieldProjectionSideConditions",
        "compositionSideConditions",
    ] {
        assert!(
            artifact_lean.contains(side_condition),
            "accept-sound side condition missing from Artifact.lean: {side_condition}\n{artifact_lean}"
        );
    }
    assert!(
        artifact_lean.contains(
            "AcceptanceSoundness.fieldProjection_direct_canonical_discharges \"userName\""
        ),
        "field-projection-faced expr claim must use its audited generic:\n{artifact_lean}"
    );
    for (name, bridge_kind) in [
        ("addTwo", "exprFragmentSemanticBridge"),
        ("inAsciiDigit", "exprFragmentSemanticBridge"),
        ("intLessZero", "exprFragmentSemanticBridge"),
        ("intEqZero", "exprFragmentSemanticBridge"),
        ("boolAndGoal", "exprFragmentSemanticBridge"),
        ("sumFrom", "recursionSemanticBridge"),
        ("countDown", "recursionSemanticBridge"),
        ("isEven", "mutualSemanticBridge"),
        ("isOdd", "mutualSemanticBridge"),
        ("quad", "compositionSemanticBridge"),
        ("hex16", "compositionSemanticBridge"),
    ] {
        assert!(
            artifact_lean.contains(&format!("CertProofs.{name}_{bridge_kind}")),
            "migrated family bridge must feed accept_sound: {name}\n{artifact_lean}"
        );
    }
    for face in [
        "theorem constructClaim0Face",
        "theorem intDispatchClaim0Face",
        "theorem intDispatchClaim1Face",
        "theorem intDispatchClaim2Face",
        "theorem stringConcatClaim0Face",
    ] {
        assert!(
            artifact_lean.contains(face),
            "declared-envelope face theorem missing from Artifact.lean: {face}\n{artifact_lean}"
        );
    }
    for float_name in [
        "floatLeGoal",
        "floatGeGoal",
        "floatLtGoal",
        "floatGtGoal",
        "floatEqGoal",
    ] {
        // The float arm sits at position five of six in
        // `exprFragmentSideCondition` (the record-parameter arm follows it),
        // so its payload carries one `Or.inl` inside the four `Or.inr`s.
        assert!(
            artifact_lean.contains(&format!(
                "Or.inr (Or.inr (Or.inr (Or.inr (Or.inl ⟨rfl, CertProofs.{float_name}_simulates⟩))))"
            )),
            "certified Float comparison must use the bespoke accept-sound residual: {float_name}\n{artifact_lean}"
        );
    }
    assert!(
        !artifact_lean.contains("floatAddGoal_simulates")
            && !artifact_lean.contains("floatMulAddGoal_simulates"),
        "NaN-nondeterministic Float results must not reach artifact side conditions:\n{artifact_lean}"
    );
    let artifact_certificate = std::fs::read_to_string(cert_dir.join("ArtifactCertificate.lean"))
        .expect("ArtifactCertificate.lean exists");
    assert!(
        artifact_certificate.contains("acceptedWithFinal AverCert.Final.cert")
            && artifact_certificate.contains("#print axioms AverCert.Artifact.certificate"),
        "accepted-artifact wrapper must remain outside the acyclic Artifact -> ArtifactSoundness -> Final path:\n{artifact_certificate}"
    );
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
            && !certificate.contains("countDownHostRef")
            && !certificate.contains("addTwo_wasm_certified")
            && !certificate.contains("addTwo_simulates")
            && !certificate.contains("addTwoHostRef")
            && !certificate.contains("quad_wasm_certified")
            && !certificate.contains("quad_simulates")
            && !certificate.contains("quadHostRef")
            && !certificate.contains("hex16_wasm_certified")
            && !certificate.contains("hex16_simulates")
            && !certificate.contains("hex16HostRef")
            && !certificate.contains("inAsciiDigit_wasm_certified")
            && !certificate.contains("inAsciiDigit_simulates")
            && !certificate.contains("inAsciiDigitHostRef")
            && !certificate.contains("intLessZero_wasm_certified")
            && !certificate.contains("intLessZero_simulates")
            && !certificate.contains("intLessZeroHostRef")
            && !certificate.contains("intEqZero_wasm_certified")
            && !certificate.contains("intEqZero_simulates")
            && !certificate.contains("intEqZeroHostRef")
            && !certificate.contains("boolAndGoal_wasm_certified")
            && !certificate.contains("boolAndGoal_simulates")
            && !certificate.contains("boolAndGoalHostRef")
            && !certificate.contains("isEven_simulates")
            && !certificate.contains("isOdd_simulates")
            && !certificate.contains("isEven_wasm")
            && !certificate.contains("isOdd_wasm")
            && !certificate.contains("isEven_mutual_sim")
            && !certificate.contains("isEven_mutual_total")
            && !certificate.contains("isEvenHostRef"),
        "migrated leaf/dispatch/construct/recursion/expr-fragment/composition/mutual families must not emit bespoke simulations or tripwires:\n{certificate}"
    );
    for expr_fragment_name in [
        "addTwo",
        "inAsciiDigit",
        "intLessZero",
        "intEqZero",
        "boolAndGoal",
    ] {
        assert!(
            certificate.contains(&format!(
                "theorem {expr_fragment_name}_exprFragmentClaimAccepted"
            )) && certificate.contains(&format!(
                "theorem {expr_fragment_name}_exprFragmentSemanticBridge"
            )),
            "integer/Bool expr-fragment must emit claim acceptance plus its small semantic bridge: {expr_fragment_name}\n{certificate}"
        );
    }
    for composition_name in ["quad", "hex16"] {
        assert!(
            certificate.contains(&format!(
                "theorem {composition_name}_compositionClaimAccepted"
            )) && certificate.contains(&format!(
                "theorem {composition_name}_compositionSemanticBridge"
            )),
            "integer composition must emit claim acceptance plus its small semantic bridge: {composition_name}\n{certificate}"
        );
    }
    for float_name in [
        "floatLeGoal",
        "floatGeGoal",
        "floatLtGoal",
        "floatGtGoal",
        "floatEqGoal",
    ] {
        assert!(
            certificate.contains(&format!("theorem {float_name}_wasm_certified"))
                && certificate.contains(&format!("theorem {float_name}_simulates")),
            "Float comparison proof must remain on the bespoke surface: {float_name}\n{certificate}"
        );
    }
    assert!(
        !certificate.contains("floatAddGoal_wasm_certified")
            && !certificate.contains("floatMulAddGoal_wasm_certified"),
        "exact-bit Float arithmetic proofs must not be emitted:\n{certificate}"
    );
    for dispatch_name in ["evalOp", "boxInt", "gauge"] {
        assert!(
            !certificate.contains(&format!(
                "theorem {dispatch_name}_intDispatchSemanticBridge"
            )),
            "dispatch bridges are derived from the declared-envelope face; no bespoke bridge: {dispatch_name}\n{certificate}"
        );
    }
    assert!(
        !certificate.contains("theorem mkOp_constructSemanticBridge"),
        "named constructor bridges are derived from the declared-envelope face:\n{certificate}"
    );
    assert!(
        certificate.contains("theorem sumFrom_recursionSemanticBridge")
            && certificate.contains("have hModelFuel")
            && certificate.contains("RecursionSoundness.evalRecUFuel")
            && certificate.contains("⟨n, _, rfl, hv")
            && !certificate.contains("⟨[n], ⟨ReprAll.cons hv ReprAll.nil, rfl⟩"),
        "recursion must emit only the used option-(b) model direction and no evaluator proof:\n{certificate}"
    );
    assert!(
        certificate.contains("theorem countDown_recursionSemanticBridge")
            && certificate.contains("RecursionSoundness.evalRecAFuel")
            && certificate.contains("refine Or.inr")
            && certificate.contains("⟨n, acc, vn, vacc, rfl, hvn, hvacc")
            && !certificate.contains("⟨[n, acc],")
            && !certificate.contains("ReprAll.cons hvn (ReprAll.cons hvacc ReprAll.nil)"),
        "accumulator recursion must emit only the used arity-two model direction:\n{certificate}"
    );
    for mutual_name in ["isEven", "isOdd"] {
        assert!(
            certificate.contains(&format!("theorem {mutual_name}_mutualSemanticBridge"))
                && certificate.contains("MutualRecursionSoundness.evalMutualUFuel")
                && certificate.contains("have hModelFuel")
                && certificate.contains("refine ⟨n, v, rfl, hv")
                && !certificate.contains("⟨[n], ⟨ReprAll.cons hv ReprAll.nil, rfl⟩"),
            "mutual export must emit only the used option-(b) model direction: {mutual_name}\n{certificate}"
        );
    }
    let user_name = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"] == "userName")
        .unwrap();
    assert_eq!(
        user_name["theorem"],
        "AcceptanceSoundness.fieldProjection_direct_canonical_discharges"
    );
    let mk_op = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"] == "mkOp")
        .unwrap();
    assert_eq!(
        mk_op["theorem"],
        "AcceptanceSoundness.construct_canonical_discharges"
    );
    let sum_from = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"] == "sumFrom")
        .unwrap();
    assert_eq!(
        sum_from["theorem"],
        "AcceptanceSoundness.recursion_claim_discharges"
    );
    let count_down = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|entry| entry["name"] == "countDown")
        .unwrap();
    assert_eq!(
        count_down["theorem"],
        "AcceptanceSoundness.recursion_claim_discharges"
    );
    for name in [
        "addTwo",
        "inAsciiDigit",
        "intLessZero",
        "intEqZero",
        "boolAndGoal",
    ] {
        let entry = manifest["certified"]
            .as_array()
            .unwrap()
            .iter()
            .find(|entry| entry["name"] == name)
            .unwrap();
        assert_eq!(
            entry["theorem"],
            "AcceptanceSoundness.exprFragment_claim_discharges"
        );
    }
    for name in ["quad", "hex16"] {
        let entry = manifest["certified"]
            .as_array()
            .unwrap()
            .iter()
            .find(|entry| entry["name"] == name)
            .unwrap();
        assert_eq!(
            entry["theorem"],
            "AcceptanceSoundness.composition_claim_discharges_with_bridge"
        );
    }
    for name in ["isEven", "isOdd"] {
        let entry = manifest["certified"]
            .as_array()
            .unwrap()
            .iter()
            .find(|entry| entry["name"] == name)
            .unwrap();
        assert_eq!(
            entry["theorem"],
            "AcceptanceSoundness.mutual_claim_discharges"
        );
    }
    for (name, theorem) in [
        (
            "wrapItems",
            "AcceptanceSoundness.verbatim_canonical_discharges",
        ),
        (
            "tagName",
            "AcceptanceSoundness.verbatim_canonical_discharges",
        ),
        (
            "quoteOrSelf",
            "AcceptanceSoundness.stringEq_canonical_discharges",
        ),
        (
            "shout",
            "AcceptanceSoundness.stringConcat_canonical_discharges",
        ),
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
            "AcceptanceSoundness.intDispatch_canonical_discharges"
        );
    }

    let started = std::time::Instant::now();
    materialize_wall(&cert_dir);
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
    eprintln!("emitted acceptance wall lake build: {:.2?}", elapsed);
    assert!(
        build.status.success(),
        "lake build of emitted acceptance wall cert failed after {elapsed:.2?}:\n{combined}"
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
            "'AcceptanceSoundness.recursion_claim_discharges' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "recursion bridge/discharge changed axiom surface:\n{combined}"
    );
    for bridge in [
        "addTwo_exprFragmentSemanticBridge",
        "inAsciiDigit_exprFragmentSemanticBridge",
        "intLessZero_exprFragmentSemanticBridge",
        "intEqZero_exprFragmentSemanticBridge",
        "boolAndGoal_exprFragmentSemanticBridge",
        "quad_compositionSemanticBridge",
        "hex16_compositionSemanticBridge",
    ] {
        assert!(
            combined.contains(&format!(
                "'CertProofs.{bridge}' depends on axioms: [propext, Classical.choice, Quot.sound]"
            )),
            "migrated integer/Bool bridge changed axiom surface: {bridge}\n{combined}"
        );
    }
    assert!(
        combined.contains(
            "'CertProofs.isEven_mutualSemanticBridge' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ) && combined.contains(
            "'CertProofs.isOdd_mutualSemanticBridge' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ) && combined.contains(
            "'AcceptanceSoundness.mutual_claim_discharges' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "mutual bridge/discharge changed axiom surface:\n{combined}"
    );

    let typecheck = cert_dir.join("ArtifactSoundnessTypecheck.lean");
    std::fs::write(
        &typecheck,
        r#"import ArtifactSoundness

example :
    AcceptanceSoundness.dischargeSideConditions AverCert.Artifact.data →
    AverCert.Schema.Holds AverCert.Artifact.data.manifest :=
  AverCert.ArtifactSoundness.accept_sound_holds

#print axioms AverCert.ArtifactSoundness.accept_sound_holds
"#,
    )
    .expect("write ArtifactSoundness typecheck");
    let typecheck_output = Command::new("lake")
        .current_dir(&cert_dir)
        .args(["env", "lean", "ArtifactSoundnessTypecheck.lean"])
        .output()
        .expect("expected ArtifactSoundness typecheck to run");
    let typecheck_combined = format!(
        "{}{}",
        String::from_utf8_lossy(&typecheck_output.stdout),
        String::from_utf8_lossy(&typecheck_output.stderr)
    );
    assert!(
        typecheck_output.status.success(),
        "ArtifactSoundness Schema.Holds typecheck failed:\n{typecheck_combined}"
    );
    // The typecheck was the last `lake` step; the remaining assertions read
    // output already captured above, so the build tree is dead weight now.
    trim_lean_build_tree(&cert_dir);
    assert!(
        typecheck_combined.contains(
            "'AverCert.ArtifactSoundness.accept_sound_holds' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "ArtifactSoundness.accept_sound_holds did not reduce to the exact audited axiom set:\n{typecheck_combined}"
    );
    assert!(
        !combined.contains("sorryAx") && !typecheck_combined.contains("sorryAx"),
        "emitted acceptance wall leaked sorryAx:\n{combined}\n{typecheck_combined}"
    );
}

// Hostile-model soundness gates.
//
// These tests all share one baseline artifact and differ only in which single
// tamper they apply before demanding a DECLINE. They used to be one test that
// ran every verification sequentially: each `check_certificate` call is a full
// kernel-checked certificate verification (~95s locally, minutes on CI) while
// the baseline `aver compile --certify` costs a fraction of a second. Splitting
// the tamper vectors into separate tests — each redoing the cheap setup — lets
// CI run the expensive verifications in parallel lanes.

/// Hostile obligation-model rewrites applied to the emitted `cert/Manifest.lean`,
/// as `(label, honest, hostile)`.
///
/// This list is the single source of truth for which manifest obligation models
/// the gate covers, and it deliberately stays a list. The shard tests below
/// select entries by `idx % HOSTILE_MANIFEST_MODEL_SHARDS`, never by name, so an
/// entry appended here is automatically exercised by exactly one existing shard:
/// no new test function to write, no CI filter to update, nothing to forget.
const HOSTILE_MANIFEST_MODELS: &[(&str, &str, &str)] = &[
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
    (
        "dispatch",
        "model := AverCert.DeclaredIndexEnvelope.dEnvStructModel Plans.gaugeDeclaredEnvelope Plans.gaugeIntDispatchPlan.body }",
        "model := fun _ => 0 }",
    ),
];

/// How many parallel shards `HOSTILE_MANIFEST_MODELS` is spread over: one test
/// function per shard. Keep it at most the list length so no shard runs empty
/// (an empty shard would pass vacuously); the shard runner asserts that.
const HOSTILE_MANIFEST_MODEL_SHARDS: usize = 3;

/// Hostile rewrites of the generated mutual-recursion model definitions in
/// `cert/CertGoals.lean`, as `(name, honest, hostile)`. Index-sharded for the
/// same reason as `HOSTILE_MANIFEST_MODELS`: adding a member of the mutual SCC
/// here is enough to get it covered.
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
/// certificate still passes the developer preflight.
///
/// Every hostile-model test runs this itself rather than trusting a baseline
/// established in some other test (and therefore some other CI lane), so each
/// one can fail honestly on its own. The compile is ~0.26s, so duplicating the
/// setup per shard is nearly free.
///
/// Returns `None` when `lake` is unavailable; the caller then skips, as before.
fn hostile_models_baseline(prefix: &str) -> Option<ScratchDir> {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping hostile leaf-model test: `lake` not available");
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

    Some(out_dir)
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
    for (idx, &(label, honest, hostile)) in HOSTILE_MANIFEST_MODELS.iter().enumerate() {
        if idx % HOSTILE_MANIFEST_MODEL_SHARDS != shard {
            continue;
        }

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
            check_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
        assert!(
            !ok && report.contains("CHECK FAILED") && !report.contains("CERTIFIED"),
            "wrong {label} model must make its emitted bridge fail and be DECLINED:\n{report}"
        );
    }
}

/// Runs the `HOSTILE_MUTUAL_MODELS` entries that belong to `shard`.
fn assert_hostile_mutual_model_shard_is_declined(shard: usize) {
    assert!(
        shard < HOSTILE_MUTUAL_MODEL_SHARDS
            && HOSTILE_MUTUAL_MODEL_SHARDS <= HOSTILE_MUTUAL_MODELS.len(),
        "shard {shard} of {HOSTILE_MUTUAL_MODEL_SHARDS} covers no hostile mutual model: keep the shard count at most the list length, one test function per shard"
    );
    let Some(out_dir) = hostile_models_baseline(&format!("certify-hostile-mutual-models-{shard}"))
    else {
        return;
    };

    // Index-sharded rather than name-selected: see the note on the manifest
    // shard runner above.
    for (idx, &(name, honest, hostile)) in HOSTILE_MUTUAL_MODELS.iter().enumerate() {
        if idx % HOSTILE_MUTUAL_MODEL_SHARDS != shard {
            continue;
        }

        let tampered = temp_dir(&format!("certify-hostile-{name}-model-definition"));
        copy_dir_all(&out_dir, &tampered);
        let model = tampered.join("cert/CertGoals.lean");
        let source = std::fs::read_to_string(&model).unwrap();
        let edited = source.replacen(honest, hostile, 1);
        assert_ne!(
            source, edited,
            "{name} model definition changed; update the hostile-model regression"
        );
        std::fs::write(&model, edited).unwrap();
        let manifest = std::fs::read_to_string(tampered.join("cert/Manifest.lean")).unwrap();
        assert!(
            manifest.contains(&format!("model := fun ns => CertGoals.{name} (ns.headD 0)")),
            "{name} hostile regression must leave the manifest model reference untouched"
        );

        let (ok, report) =
            check_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
        assert!(
            !ok && report.contains("CHECK FAILED") && !report.contains("CERTIFIED"),
            "wrong generated {name} definition must fail the mutual semantic bridge and be DECLINED:\n{report}"
        );
    }
}

/// The untampered hostile-model baseline is preflight-clean and its isolated
/// `Certificate` target builds, so a DECLINE in any hostile-model test below is
/// the tamper's doing and not a broken fixture.
#[test]
fn cert_hostile_model_baseline_is_preflight_clean_and_lake_builds() {
    let Some(out_dir) = hostile_models_baseline("certify-hostile-leaf-models") else {
        return;
    };

    let build_green = temp_dir("certify-hostile-mutual-build-green");
    copy_dir_all(&out_dir, &build_green);
    assert_certificate_target_builds(&build_green.join("cert"), "mutual hostile-model baseline");
}

/// Hostile model: the generated expression-fragment leaf definition `addTwo`
/// in `cert/CertGoals.lean` computes `x + 3` instead of `x + 2`, with the wasm
/// bytes and the manifest's model reference left untouched.
#[test]
fn cert_hostile_model_expr_fragment_leaf_definition_is_declined() {
    let Some(out_dir) = hostile_models_baseline("certify-hostile-expr-fragment-baseline") else {
        return;
    };
    let wasm = out_dir.join("cert_goals.wasm");

    let tampered = temp_dir("certify-hostile-expr-fragment-model-definition");
    copy_dir_all(&out_dir, &tampered);
    let model = tampered.join("cert/CertGoals.lean");
    let source = std::fs::read_to_string(&model).unwrap();
    let honest = "def addTwo (x : Int) : Int :=\n  (x + 2)";
    let hostile = "def addTwo (x : Int) : Int :=\n  (x + 3)";
    let edited = source.replacen(honest, hostile, 1);
    assert_ne!(
        source, edited,
        "expr-fragment model definition changed; update the hostile-model regression"
    );
    std::fs::write(&model, edited).unwrap();
    assert_eq!(
        std::fs::read(tampered.join("cert_goals.wasm")).unwrap(),
        std::fs::read(&wasm).unwrap(),
        "hostile expr-fragment check must isolate the mutation to generated source data"
    );
    let manifest = std::fs::read_to_string(tampered.join("cert/Manifest.lean")).unwrap();
    assert!(
        manifest.contains("model := fun ns => CertGoals.addTwo (ns.headD 0)"),
        "expr-fragment hostile regression must leave the manifest model reference untouched"
    );

    let (ok, report) = check_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
    assert!(
        !ok && report.contains("CHECK FAILED") && !report.contains("CERTIFIED"),
        "wrong generated expr-fragment model definition must fail its emitted bridge and be DECLINED:\n{report}"
    );
}

/// Hostile manifest obligation models, shard 0: `HOSTILE_MANIFEST_MODELS`
/// entries 0, 3, 6, ... — today the `verbatim` variant-tag model.
#[test]
fn cert_hostile_model_manifest_obligation_shard_0_of_3_is_declined() {
    assert_hostile_manifest_model_shard_is_declined(0);
}

/// Hostile manifest obligation models, shard 1: `HOSTILE_MANIFEST_MODELS`
/// entries 1, 4, 7, ... — today the `string` host-contract model.
#[test]
fn cert_hostile_model_manifest_obligation_shard_1_of_3_is_declined() {
    assert_hostile_manifest_model_shard_is_declined(1);
}

/// Hostile manifest obligation models, shard 2: `HOSTILE_MANIFEST_MODELS`
/// entries 2, 5, 8, ... — today the `dispatch` declared-envelope model.
#[test]
fn cert_hostile_model_manifest_obligation_shard_2_of_3_is_declined() {
    assert_hostile_manifest_model_shard_is_declined(2);
}

/// Hostile model: the declared constructor index for `mkOp`.
///
/// The declared-envelope wiring derives the constructor obligation model from
/// the emitted plan (`dEnvCtorModel Plans.mkOpDeclaredEnvelope 1`), so the
/// generated `def mkOp` source definition is no longer load-bearing for the
/// obligation. The hostile vector moves with the model: declare the WRONG hit
/// constructor index, so the still well-typed model claims `mkOp` builds the
/// second constructor while the accepted claim and module bytes pin the first.
#[test]
fn cert_hostile_model_declared_construct_index_is_declined() {
    let Some(out_dir) = hostile_models_baseline("certify-hostile-construct-baseline") else {
        return;
    };

    let tampered = temp_dir("certify-hostile-construct-model-definition");
    copy_dir_all(&out_dir, &tampered);
    let manifest_path = tampered.join("cert/Manifest.lean");
    let source = std::fs::read_to_string(&manifest_path).unwrap();
    let honest = "model := AverCert.DeclaredIndexEnvelope.dEnvCtorModel Plans.mkOpDeclaredEnvelope 1 (by decide) }";
    let hostile = "model := AverCert.DeclaredIndexEnvelope.dEnvCtorModel Plans.mkOpDeclaredEnvelope 2 (by decide) }";
    let edited = source.replacen(honest, hostile, 1);
    assert_ne!(
        source, edited,
        "construct obligation model shape changed; update the hostile-model regression"
    );
    std::fs::write(&manifest_path, edited).unwrap();
    let artifact = std::fs::read_to_string(tampered.join("cert/Artifact.lean")).unwrap();
    assert!(
        artifact.contains("exportName := \"mkOp\", carrier := 23, structIdx := 1"),
        "construct hostile regression must leave the accepted claim pinned at the honest constructor index"
    );
    let envelope = std::fs::read_to_string(tampered.join("cert/Plans.lean")).unwrap();
    assert!(
        envelope.contains("def mkOpDeclaredEnvelope : AverCert.DeclaredIndexEnvelope.DIdxEnvelope :=\n  ⟨0, 23, [⟨1, .hit, 23⟩, ⟨2, .hit, 23⟩, ⟨3, .unit, 0⟩]⟩"),
        "construct hostile regression needs a second declared hit constructor for the wrong-index vector"
    );

    let (ok, report) = check_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
    assert!(
        !ok && report.contains("CHECK FAILED") && !report.contains("CERTIFIED"),
        "wrong declared constructor index must fail its emitted bridge and be DECLINED:\n{report}"
    );
}

/// Hostile mutual-recursion model definitions, shard 0: `HOSTILE_MUTUAL_MODELS`
/// entries 0, 2, 4, ... — today `isEven` recursing on `n - 2`.
#[test]
fn cert_hostile_model_mutual_recursion_shard_0_of_2_is_declined() {
    assert_hostile_mutual_model_shard_is_declined(0);
}

/// Hostile mutual-recursion model definitions, shard 1: `HOSTILE_MUTUAL_MODELS`
/// entries 1, 3, 5, ... — today `isOdd` recursing on `n - 2`.
#[test]
fn cert_hostile_model_mutual_recursion_shard_1_of_2_is_declined() {
    assert_hostile_mutual_model_shard_is_declined(1);
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
    for (family, shards) in [
        ("manifest_obligation", HOSTILE_MANIFEST_MODEL_SHARDS),
        ("mutual_recursion", HOSTILE_MUTUAL_MODEL_SHARDS),
    ] {
        for shard in 0..shards {
            let expected =
                format!("fn cert_hostile_model_{family}_shard_{shard}_of_{shards}_is_declined");
            assert!(
                source.contains(&expected),
                "hostile {family} shard {shard} of {shards} has no test function, so entries with \
                 idx % {shards} == {shard} are never checked; add `{expected}`"
            );
        }
    }
}

/// Hostile model: the generated fueled self-recursion `sumFrom` accumulates the
/// constant `2` instead of `n`.
#[test]
fn cert_hostile_model_fueled_recursion_definition_is_declined() {
    let Some(out_dir) = hostile_models_baseline("certify-hostile-recursion-baseline") else {
        return;
    };

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
        manifest.contains("model := fun ns => CertGoals.sumFrom (ns.headD 0)"),
        "recursion hostile regression must leave the manifest model reference untouched"
    );

    let (ok, report) = check_certificate(&tampered.join("cert_goals.wasm"), &tampered.join("cert"));
    assert!(
        !ok && report.contains("CHECK FAILED") && !report.contains("CERTIFIED"),
        "wrong generated recursion model definition must fail its emitted bridge and be DECLINED:\n{report}"
    );
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

    materialize_wall(&cert_dir);
    let combined = lake_build_package(&cert_dir, "emitted cert");
    // Kernel-clean: the certificate theorem's `#print axioms` must show the
    // core whitelist and never `sorryAx`.
    assert!(
        combined.contains(
            "addTwo_exprFragmentSemanticBridge' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "certificate theorem not kernel-clean:\n{combined}"
    );
    assert!(
        !combined.contains("sorryAx"),
        "certificate leaked sorryAx:\n{combined}"
    );
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
        assert_eq!(
            entry["theorem"],
            "AcceptanceSoundness.recursion_claim_discharges"
        );
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

    // Load-bearing contract check: the emitted obligation selects the extra
    // premise only for the byte-pinned multiplicative recursion.  The schema's
    // add/sub branch itself contains no multiplication-totality binder.
    let manifest_lean = std::fs::read_to_string(cert_dir.join("Manifest.lean")).unwrap();
    for name in ["sumFrom", "constPlus", "backward", "countDown"] {
        let obligation = lean_obligation_def(&manifest_lean, name);
        assert!(
            !obligation.contains("totalityRole := .mul") && !obligation.contains("Int.mul"),
            "{name} must retain the add/sub-only total premise surface:\n{obligation}"
        );
    }
    let factorial_obligation = lean_obligation_def(&manifest_lean, "factorial");
    assert!(
        factorial_obligation.contains("totalityRole := .mul"),
        "factorial must select the byte-checked mul-totality role:\n{factorial_obligation}"
    );
    materialize_wall(&cert_dir);
    let schema_core = std::fs::read_to_string(cert_dir.join("SchemaCore.lean")).unwrap();
    let totality = schema_core
        .split_once("def Obligation.holdsTotal")
        .expect("holdsTotal definition")
        .1;
    let (add_sub_branch, mul_and_rest) = totality
        .split_once("| .mul =>")
        .expect("role-sensitive mul branch");
    assert!(
        add_sub_branch.contains("| .addSub =>") && !add_sub_branch.contains("_hMulTot"),
        "add/sub holdsTotal branch must have no mul-totality premise:\n{add_sub_branch}"
    );
    assert!(
        mul_and_rest.contains("_hMulTot"),
        "mul holdsTotal branch must carry the premise it consumes"
    );

    let combined = lake_build_package(&cert_dir, "emitted recursion cert");
    // Every supported recursion family emits only its small source-model
    // bridge. The evaluator, lowering, and totality proofs stay in the audited
    // wall.
    let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean")).unwrap();
    let artifact_lean = std::fs::read_to_string(cert_dir.join("Artifact.lean")).unwrap();
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
                && artifact_lean.contains("AcceptanceSoundness.recursionSemanticBridges data")
                && artifact_lean.contains(&format!("CertProofs.{name}_recursionSemanticBridge")),
            "migrated recursion emitted a bespoke proof/tripwire or missed the accept-sound side condition for {name}:\n{certificate}\n{artifact_lean}"
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
            "'AcceptanceSoundness.recursion_claim_discharges' depends on axioms: [propext, Classical.choice, Quot.sound]"
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

        let (ok, report) = check_certificate(&tampered.join("recgen.wasm"), &tampered.join("cert"));
        assert!(
            !ok && report.contains("CHECK FAILED") && !report.contains("CERTIFIED"),
            "wrong generated {name} definition must be caught by its semantic bridge:\n{report}"
        );
    }

    // GuardIso: the bridges are not decorative. A unary bridge claiming the
    // wrong parsed base and an accumulator bridge claiming the unary family
    // must each stop the certificate from building.
    for (name, honest, hostile) in [
        (
            "factorial-shape",
            "({ base := 1, step := .inputSecond } : RecursionSoundness.RecShapeU)",
            "({ base := 2, step := .inputSecond } : RecursionSoundness.RecShapeU)",
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

        let (ok, report) = check_certificate(&tampered.join("recgen.wasm"), &tampered.join("cert"));
        assert!(
            !ok && report.contains("CHECK FAILED") && !report.contains("CERTIFIED"),
            "wrong {name} must be constrained by the bridge/parsed byte shape:\n{report}"
        );
    }
}

#[test]
fn certify_mutual_recursion_scc_lake_builds_kernel_clean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify mutual-recursion test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // A two-member SCC (`isEven`/`isOdd`) and a three-member cycle
    // (`rotA -> rotB -> rotC -> rotA`) exercise the plan-derived `AdmittedScc`
    // and simultaneous source-fuel bridge at k = 2 and k = 3.
    let cases: [(&str, &[&str]); 2] = [
        ("tools/certkit/fixtures/mutual.av", &["isEven", "isOdd"]),
        (
            "tools/certkit/fixtures/mutual3.av",
            &["rotA", "rotB", "rotC"],
        ),
    ];

    for (fixture, exports) in cases {
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
        // Every member of the SCC is a certified export using the same audited
        // SCC package and its own source-model bridge.
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
                assert_eq!(
                    entry["theorem"],
                    "AcceptanceSoundness.mutual_claim_discharges"
                );
            }
        }

        materialize_wall(&cert_dir);
        let combined = lake_build_package(&cert_dir, &format!("emitted mutual cert {fixture}"));
        let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean")).unwrap();
        let artifact_lean = std::fs::read_to_string(cert_dir.join("Artifact.lean")).unwrap();
        for name in exports {
            assert!(
                combined.contains(&format!(
                    "'CertProofs.{name}_mutualSemanticBridge' depends on axioms: [propext, Classical.choice, Quot.sound]"
                )),
                "mutual bridge for {name} in {fixture} not kernel-clean:\n{combined}"
            );
            assert!(
                certificate.contains(&format!("theorem {name}_mutualSemanticBridge"))
                    && !certificate.contains(&format!("{name}_simulates"))
                    && !certificate.contains(&format!("{name}_wasm"))
                    && artifact_lean.contains("AcceptanceSoundness.mutualSemanticBridges data")
                    && artifact_lean.contains(&format!("CertProofs.{name}_mutualSemanticBridge")),
                "migrated mutual export retained bespoke proof/tripwire emission or missed the accept-sound side condition: {name}\n{certificate}\n{artifact_lean}"
            );
        }
        assert!(
            !certificate.contains("native_decide"),
            "generic mutual discharge must not emit a native-decide tripwire:\n{certificate}"
        );
        assert!(
            combined.contains(
                "'AverCert.Final.cert' depends on axioms: [propext, Classical.choice, Quot.sound]"
            ),
            "mutual Final.cert changed axiom surface for {fixture}:\n{combined}"
        );
        assert!(
            !combined.contains("sorryAx"),
            "mutual certificate {fixture} leaked sorryAx:\n{combined}"
        );
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
        "AcceptanceSoundness.verbatim_canonical_discharges"
    );
    let artifact_lean =
        std::fs::read_to_string(cert_dir.join("Artifact.lean")).expect("Artifact.lean exists");
    assert!(
        artifact_lean.contains("theorem verbatimSideConditions")
            && artifact_lean.contains("AcceptanceSoundness.verbatimSemanticBridges data"),
        "verbatim bridge must feed the accept-sound aggregate:\n{artifact_lean}"
    );
    let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
        .expect("Certificate.lean exists");
    assert!(
        !certificate.contains("tagName_wasm_certified")
            && !certificate.contains("tagName_simulates"),
        "verbatim dispatch must not emit bespoke proofs:\n{certificate}"
    );

    materialize_wall(&cert_dir);
    let combined = lake_build_package(&cert_dir, "emitted verbatim-variant-dispatch cert");
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
}

#[test]
fn certify_string_eq_host_contract_lake_builds_kernel_clean() {
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
    assert_plans_lean_is_the_only_public_plan_data(&cert_dir, &manifest);
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
        "AcceptanceSoundness.stringEq_canonical_discharges"
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
        plans_lean.contains(".constStringBytes [34]")
            && plans_lean.contains(".prim .stringEq [0, 1]")
            && plans_lean.contains(".constStringBytes [92, 34]"),
        "String.eq source plan DATA should preserve the needle, comparison and hit literal:\n{plans_lean}"
    );
    assert!(
        plans_lean.contains("needle := ({ dataIdx := 0, bytes := [34] } : StringEqChunk)")
            && plans_lean
                .contains("hit := .literal ({ dataIdx := 1, bytes := [92, 34] } : StringEqChunk)")
            && plans_lean.contains("default := .input"),
        "String.eq target plan DATA should preserve its byte/data-segment bindings:\n{plans_lean}"
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
    assert!(
        artifact_lean.contains("theorem stringEqSideConditions")
            && artifact_lean.contains("AcceptanceSoundness.stringEqSemanticBridges data"),
        "String.eq bridge must feed the accept-sound aggregate:\n{artifact_lean}"
    );
    let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
        .expect("Certificate.lean exists");
    assert!(
        !certificate.contains("quoteOrSelf_wasm_certified")
            && !certificate.contains("quoteOrSelf_simulates"),
        "String.eq must not emit bespoke proofs:\n{certificate}"
    );

    // Classification and plan-shape regressions must remain visible on CI
    // workers without Lean. Only the final kernel build needs `lake`.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping String.eq certificate kernel build: `lake` not available");
        return;
    }

    materialize_wall(&cert_dir);
    let combined = lake_build_package(&cert_dir, "emitted String.eq host-contract cert");
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
    assert_plans_lean_is_the_only_public_plan_data(&cert_dir, &manifest);
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
        "AcceptanceSoundness.stringConcat_canonical_discharges"
    );
    let shout_class = shout_entry["class"].as_str().unwrap_or("<missing>");
    assert_eq!(
        shout_class, "verbatim-string-concat",
        "shout should render its concat class, got {shout_class}"
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
    assert!(
        artifact_lean.contains("theorem stringConcatClaim0ExportFuncType")
            && artifact_lean.contains("theorem stringConcatClaim0HelperFuncType"),
        "String.concat must carry export/helper function-type proof leaves:\n{artifact_lean}"
    );
    assert!(
        artifact_lean.contains("theorem stringConcatClaim0Face")
            && artifact_lean.contains("AverCert.StandardFace.stringConcatDeclaredFace"),
        "String.concat must carry its semantic standard face:\n{artifact_lean}"
    );
    let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
        .expect("Certificate.lean exists");
    assert!(
        !certificate.contains("shout_wasm_certified") && !certificate.contains("shout_simulates"),
        "String.concat must not emit bespoke proofs:\n{certificate}"
    );

    materialize_wall(&cert_dir);
    let combined = lake_build_package(&cert_dir, "emitted String.concat host-contract cert");
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
    materialize_wall(&cert_dir);
    let combined = lake_build_package(&cert_dir, "emitted composition cert");
    // Kernel-clean: the caller theorem cites its callee's simulation lemma and
    // stays on the core whitelist; no `sorryAx` leaks through the composition.
    assert!(
        combined.contains(
            "quad_compositionSemanticBridge' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "composition certificate theorem not kernel-clean:\n{combined}"
    );
    assert!(
        !combined.contains("sorryAx"),
        "composition certificate leaked sorryAx:\n{combined}"
    );
}

/// Non-recursive ADT witness fixtures, as `(source, prefix, expected exports)`.
///
/// Each entry compiles its own certificate package and `lake build`s it, so the
/// list used to be ten full Lean builds run back to back inside a single test —
/// the longest serial chain in the certify suite and the reason its `rest` lane
/// was the slowest one left after the hostile-model split.
///
/// This list is the single source of truth for which fixtures the gate covers,
/// and it deliberately stays a list. The shard tests below select entries by
/// `idx % NONRECURSIVE_ADT_WITNESS_SHARDS`, never by name, so a fixture
/// appended here is automatically exercised by exactly one existing shard: no
/// new test function to write, no CI filter to update, nothing to forget.
const NONRECURSIVE_ADT_WITNESS_CASES: &[(&str, &str, &[&str])] = &[
    (
        "tools/certkit/fixtures/opteval.av",
        "opteval",
        &["mk", "eval"],
    ),
    ("examples/core/user_record.av", "user-record", &["greet"]),
    (
        "tools/certkit/fixtures/tupleproj.av",
        "tuple-proj",
        &["pairFst", "pairSnd"],
    ),
    (
        "tools/certkit/fixtures/widenedmatch.av",
        "widened-match",
        &["boxInt"],
    ),
    (
        "tools/certkit/fixtures/rangepred.av",
        "range-pred",
        &["inAsciiDigit"],
    ),
    (
        "tools/certkit/fixtures/verbatimwiden.av",
        "verbatim-widen",
        &["wrapItems"],
    ),
    (
        "tools/certkit/fixtures/f64verbatim.av",
        "f64-verbatim",
        &["floatOrZero"],
    ),
    // Out-of-template variant dispatch: four constructors, mixed arm
    // semantics (negation, offset addition, identity, non-zero default) —
    // provable only through the structural walker, not a shape template.
    (
        "tools/certkit/fixtures/signalgauge.av",
        "signal-gauge",
        &["gauge"],
    ),
    (
        "tools/certkit/fixtures/intdispatchgen.av",
        "int-dispatch-gen",
        &["boxInt", "gauge"],
    ),
    // Payload-first subtraction, constant-first addition, and payload
    // variants elided into the wildcard default.
    ("tools/certkit/fixtures/meter.av", "meter", &["readout"]),
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
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping certify ADT test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // Index-sharded rather than name-selected: every entry of the list lands in
    // exactly one shard by construction, including entries added later.
    for (idx, &(input, prefix, expected)) in NONRECURSIVE_ADT_WITNESS_CASES.iter().enumerate() {
        if idx % NONRECURSIVE_ADT_WITNESS_SHARDS != shard {
            continue;
        }

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
        for &name in expected {
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
                    && entry["theorem"] == "AcceptanceSoundness.construct_canonical_discharges"
            })
            .collect::<Vec<_>>();
        if !model_construct_entries.is_empty() {
            let artifact_lean = std::fs::read_to_string(cert_dir.join("Artifact.lean"))
                .expect("Artifact.lean exists");
            let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
                .expect("Certificate.lean exists");
            for entry in &model_construct_entries {
                let name = entry["name"].as_str().unwrap();
                assert!(
                    artifact_lean.contains("AverCert.StandardFace.constructNamedFace")
                        && artifact_lean.contains(&format!("exportName := \"{name}\"")),
                    "construct-with-model must carry its declared-envelope face for {name}:\n{artifact_lean}"
                );
                assert!(
                    !certificate.contains(&format!("theorem {name}_constructSemanticBridge"))
                        && !certificate.contains(&format!("{name}_wasm_certified"))
                        && !certificate.contains(&format!("{name}_simulates")),
                    "construct-with-model must not emit a bespoke bridge for {name}:\n{certificate}"
                );
            }
        }
        if !dispatch_entries.is_empty() {
            let artifact_lean = std::fs::read_to_string(cert_dir.join("Artifact.lean"))
                .expect("Artifact.lean exists");
            let certificate = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
                .expect("Certificate.lean exists");
            for entry in &dispatch_entries {
                let name = entry["name"].as_str().unwrap();
                assert_eq!(
                    entry["theorem"],
                    "AcceptanceSoundness.intDispatch_canonical_discharges"
                );
                assert!(
                    artifact_lean.contains("AverCert.StandardFace.intDispatchDeclaredFace")
                        && artifact_lean.contains(&format!("exportName := \"{name}\"")),
                    "dispatch must carry its declared-envelope face for {name}:\n{artifact_lean}"
                );
                assert!(
                    !certificate.contains(&format!("theorem {name}_intDispatchSemanticBridge"))
                        && !certificate.contains(&format!("{name}_wasm_certified"))
                        && !certificate.contains(&format!("{name}_simulates")),
                    "dispatch must not emit a bespoke bridge for {name}:\n{certificate}"
                );
            }
        }
        if prefix == "tuple-proj" {
            let entries = manifest["certified"].as_array().unwrap();
            for name in ["pairFst", "pairSnd"] {
                let entry = entries.iter().find(|entry| entry["name"] == name).unwrap();
                assert_eq!(
                    entry["theorem"], "AcceptanceSoundness.fieldProjection_canonical_discharges",
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
            let artifact_lean = std::fs::read_to_string(cert_dir.join("Artifact.lean"))
                .expect("Artifact.lean exists");
            for name in ["pairFst", "pairSnd"] {
                assert!(
                    artifact_lean
                        .contains("AcceptanceSoundness.fieldProjectionSemanticBridges data")
                        && artifact_lean.contains(&format!("exportName := \"{name}\"")),
                    "field projection must feed the audited accept-sound side condition for {name}:\n{artifact_lean}"
                );
            }
        }
        materialize_wall(&cert_dir);
        let combined = lake_build_package(&cert_dir, &format!("emitted ADT cert for {input}"));
        assert!(
            !combined.contains("sorryAx"),
            "ADT certificate leaked sorryAx for {input}:\n{combined}"
        );
        if !model_construct_entries.is_empty() {
            assert!(
                combined.contains(
                    "'AcceptanceSoundness.construct_canonical_discharges' depends on axioms: [propext, Classical.choice, Quot.sound]"
                ),
                "audited construct discharge changed axiom surface:\n{combined}"
            );
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
/// no copy propagation, so each keeps its `Let` node; the plan producer
/// inlines the proven single-use binding at its use site. All three must
/// certify as expression fragments and the emitted package must close under
/// lake.
#[test]
fn certify_let_named_shapes_certify_and_lake_build() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-letnamed");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/letnamed.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify letnamed failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(out_dir.join("cert").join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    let classes: BTreeMap<String, String> = manifest["certified"]
        .as_array()
        .expect("certified report is an array")
        .iter()
        .map(|entry| {
            (
                entry["name"].as_str().unwrap().to_string(),
                entry["class"].as_str().unwrap().to_string(),
            )
        })
        .collect();
    for name in ["named", "addTwoNamed", "inRangeNamed"] {
        assert_eq!(
            classes.get(name).map(String::as_str),
            Some("expr-fragment-v1"),
            "{name} must certify through the plan path; manifest classes: {classes:?}"
        );
    }
    assert!(
        manifest["source_level_only"]
            .as_array()
            .is_none_or(|declined| declined.is_empty()),
        "no let-named shape may decline to source-level-only: {manifest:#}"
    );

    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping letnamed lake build: `lake` not available");
        return;
    }
    assert_certificate_target_builds(&out_dir.join("cert"), "let-named shapes");
}

/// Arity-3 integer/Bool expression fragment through the audited generic
/// bridge: three Int params, a branch on the first and a constant comparison
/// of the second or third (three comparisons total — each adds a `by_cases`,
/// so goal count stays at 2^3). The Lean wall is n-ary throughout
/// (`FragParams.denote` right-nested products); this pins the renderer's
/// generalized source model (`fun p => f p.1 p.2.1 p.2.2`) and product
/// unpacking, and the package must close under lake.
#[test]
fn certify_arity_three_fragment_certifies_and_lake_builds() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-arity3");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/arity3.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "compile --certify arity3 failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(out_dir.join("cert").join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    let triple = manifest["certified"]
        .as_array()
        .expect("certified report is an array")
        .iter()
        .find(|entry| entry["name"] == "tripleCheck")
        .unwrap_or_else(|| panic!("tripleCheck must certify: {manifest:#}"));
    assert_eq!(triple["class"], "expr-fragment-v1");
    let manifest_lean = std::fs::read_to_string(out_dir.join("cert").join("Manifest.lean"))
        .expect("Manifest.lean exists");
    assert!(
        manifest_lean.contains("model := fun p => Arity3Probe.tripleCheck p.1 p.2.1 p.2.2"),
        "arity-3 obligation must uncurry over the right-nested product:\n{manifest_lean}"
    );
    let certificate = std::fs::read_to_string(out_dir.join("cert").join("Certificate.lean"))
        .expect("Certificate.lean exists");
    assert!(
        certificate.contains("rcases p with ⟨a0, a1, a2⟩"),
        "arity-3 bridge must unpack the right-nested product domain:\n{certificate}"
    );

    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping arity3 lake build: `lake` not available");
        return;
    }
    assert_certificate_target_builds(&out_dir.join("cert"), "arity-3 fragment");
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
    assert_eq!(
        sum_big["theorem"],
        "AcceptanceSoundness.recursion_claim_discharges"
    );
    materialize_wall(&cert_dir);
    let combined = lake_build_package(&cert_dir, "the s33 boundary cert");
    assert!(
        combined.contains(
            "'CertProofs.sumBig_recursionSemanticBridge' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ) && combined.contains(
            "'AcceptanceSoundness.recursion_claim_discharges' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ) && combined.contains(
            "'AverCert.Final.cert' depends on axioms: [propext, Classical.choice, Quot.sound]"
        ),
        "boundary certificate not kernel-clean:\n{combined}"
    );
    assert!(
        !combined.contains("sorryAx"),
        "boundary certificate leaked sorryAx:\n{combined}"
    );
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
        manifest_lean.contains("hostRoleTable := none,"),
        "the Lean manifest must declare the absent host-role table as `none`"
    );

    // The reviewed new fact: both String concatenations certify, in the exact
    // carrier-free class. `string-concat-v1` reads no carrier and cites no arith
    // role, so it is the one family that lowers in this state.
    let classes: BTreeMap<String, String> = manifest["certified"]
        .as_array()
        .expect("certified report is an array")
        .iter()
        .map(|c| {
            (
                c["name"].as_str().expect("entry has a name").to_string(),
                c["class"].as_str().expect("entry has a class").to_string(),
            )
        })
        .collect();
    for export in ["greet", "shout"] {
        assert_eq!(
            classes.get(export).map(String::as_str),
            Some("verbatim-string-concat"),
            "`{export}` must certify as the carrier-free String.concat class, got \
             certified={classes:?} declared={declared:?}"
        );
    }

    // The original tripwire's intent, preserved: every class that cites an
    // arith host role stays impossible here. An admitted arith table requires
    // `carrierState` to name a carrier struct, and this module's type section
    // names none, so none of these can appear however the classifier changes.
    const INTEGER_FAMILY_CLASSES: &[&str] = &[
        "self-recursive",
        "multi-argument self-recursive",
        "mutual-recursive",
        "int-dispatch",
        "cross-function-composition",
    ];
    for (name, class) in &classes {
        assert!(
            !INTEGER_FAMILY_CLASSES.contains(&class.as_str()),
            "`{name}` certified as `{class}` in a module with no Int carrier; \
             an integer-family class here is a false positive until reviewed"
        );
    }

    assert!(
        !certified.contains("main"),
        "`main` is an effectful zero-argument export and must not certify"
    );
    // `main` prints. That is what stops it being a pure simulation of the
    // source model, and it is what the report must say: the parameter count it
    // used to blame was never the blocker, since a zero-argument export with no
    // effects declines for its arity only when nothing else applies.
    assert_eq!(
        declared.get("main").map(String::as_str),
        Some(
            "calls the host capability `aver.console_print`; \
             certified templates simulate pure bodies, never effects"
        ),
        "`main` must decline for the effect it performs, not for its parameter \
         count and not for the missing Int helper"
    );
}

/// Each declined export must be told what is actually stopping it. The Fibonacci
/// example carries one export per blocker the classifier can distinguish, so it
/// pins the whole vocabulary at once — and in particular pins that a parameter
/// count is reported for `fibTR`, whose body really is a pure three-argument
/// recursion, and for nothing else here.
#[test]
fn certify_declines_name_the_blocker_that_actually_applies() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-decline-blockers");

    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/data/fibonacci.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile --certify` to run");
    assert!(
        compile.status.success(),
        "fibonacci --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(out_dir.join("cert").join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
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
        // A pure three-argument tail recursion: nothing but the accumulator
        // template's two-argument limit stands in its way, so the report says
        // which family it missed instead of calling the signature unsupported.
        (
            "fibTR",
            "takes 3 parameters; the arity-free templates did not match this body, \
             and the recursion, ADT-construction, variant-dispatch, String and \
             composition templates take one or two arguments",
        ),
        (
            "main",
            "calls the host capability `aver.console_print`; \
             certified templates simulate pure bodies, never effects",
        ),
        (
            "absF",
            "body uses the wasm instruction `F64Sub`, which is outside the certified fragment",
        ),
        (
            "finalizeFibStats",
            "parameter 1 is a user record, variant or list value; a value of that shape \
             is certified only by the projection, variant-dispatch and String templates, \
             and this body matches none of them",
        ),
        (
            "buildFibStats",
            "returns a user record, variant or list value; a value of that shape is built \
             only by the constructor, projection, variant-dispatch and String templates, \
             and this body matches none of them",
        ),
        (
            "fib",
            "calls other user functions; only the composition and mutual-recursion \
             templates cross function boundaries, and this body fits neither",
        ),
    ] {
        assert_eq!(
            declared.get(export).map(String::as_str),
            Some(reason),
            "`{export}` must decline with the blocker that actually applies to it"
        );
    }
}

/// End-to-end on a module WITHOUT the Int box helper: emit the package, then
/// run the REAL verification.
///
/// This test used to pin the admission-only verdict for `hello.av` — exit 1,
/// the zero-certified banner. `string-concat-v1` now lowers in the carrierless
/// state, so that module certifies two exports and the verdict is CERTIFIED.
/// Only the zero-certified half of the old expectation is obsolete; what the
/// test exists for is unchanged and is asserted more strongly below, because
/// the pipeline succeeding is now witnessed by a green CERTIFIED rather than
/// merely by the absence of `DECLINED`. The admission-only path itself is still
/// real behaviour and keeps its own coverage in
/// `certify_then_verify_module_with_no_certifiable_export_is_admission_only`.
#[test]
fn certify_then_verify_carrierless_module_acceptance_pin_closes() {
    // The end-to-end regression whose absence let the carrierless flow ship
    // with an unprovable acceptance pin. The Lean acceptance must build green.
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping carrierless verify test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certify-verify-no-int-helper");

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
    assert!(
        compile.status.success(),
        "hello --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    // The module really is the carrierless one this test is about.
    let bytes = std::fs::read(out_dir.join("hello.wasm")).unwrap();
    let (box_idx, ..) = aver::codegen::cert::byte_derived_frag_host_role_indices(&bytes)
        .expect("hello.wasm classifies");
    assert_eq!(
        box_idx, None,
        "hello.av must stay carrierless for this test to mean anything"
    );

    let verify = aver_command()
        .arg("cert")
        .arg("verify")
        .arg(out_dir.join("hello.wasm"))
        .arg(out_dir.join("cert"))
        .output()
        .expect("expected `aver cert verify` to run");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&verify.stdout),
        String::from_utf8_lossy(&verify.stderr)
    );

    // The acceptance pin closes, and now says so positively: the pipeline
    // succeeding is witnessed by a green verdict, not by the absence of a
    // decline. The `DECLINED` assertion is kept alongside it because the two
    // fail differently — a crash or a timeout could produce neither string.
    assert_eq!(
        verify.status.code(),
        Some(0),
        "the carrierless package must verify green:\n{combined}"
    );
    assert!(
        combined.contains("CERTIFIED"),
        "the carrierless package must reach the CERTIFIED verdict:\n{combined}"
    );
    assert!(
        combined.contains("2 certified exports"),
        "the summary must count the two String.concat exports:\n{combined}"
    );
    for export in ["greet", "shout"] {
        assert!(
            combined.contains(export),
            "`{export}` must appear in the certified list:\n{combined}"
        );
    }
    assert!(
        !combined.contains("DECLINED"),
        "the carrierless package must not be DECLINED — its acceptance pin must close:\n{combined}"
    );
}

/// The admission-only path keeps its coverage — it just no longer has a reason
/// to live in this file. `empty_cert_is_admission_only_and_exits_nonzero`
/// (`tests/cert_verify_spec.rs`) runs the same `compile --certify` then
/// `cert verify` pipeline on `tools/certkit/fixtures/certempty.av`, whose only
/// export is a two-argument `Int` add that no certified template admits, and
/// pins the same banner, the same nonzero exit and the absence of the green
/// path. Restating it here would duplicate a full Lean verification in a second
/// CI lane for no additional guarantee, so this comment stands in for the test:
/// if that one is ever deleted or repointed at a module that certifies
/// something, the admission-only verdict loses its only coverage.
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
        let (box_idx, _, _, _, _, cmp_idx, eq_idx) =
            aver::codegen::cert::byte_derived_frag_host_role_indices(&wasm).unwrap();
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
        // The certificate's declared table is byte-derived, so it says exactly
        // the same thing — an unexported helper is an honestly absent role,
        // never a role bound to a function nothing names.
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

    // The certified entry, byte for byte the pre-change classification.
    assert_eq!(
        manifest["certified"],
        serde_json::json!([{
            "name": "addOne",
            "class": "expr-fragment-v1",
            "policy": "simulatesModel",
            "level": "L1",
            "dom": "List Int",
            "cod": "Int",
            "theorem": "AcceptanceSoundness.exprFragment_claim_discharges",
        }]),
        "the add_one certification must be unchanged by the optional-helper handling"
    );
    assert_eq!(
        manifest["runtime_contracts"],
        serde_json::json!([
            "__rt_aint_from_i64 (box i64 -> carrier)",
            "Int.add (carrier add = exact integer addition on represented values)",
        ]),
        "add_one's runtime contracts must be unchanged"
    );

    // The host-role table stays the exact byte-derived OBJECT — never `null`
    // — and every arithmetic role is bound for the full Int runtime.
    let wasm = std::fs::read(out_dir.join("add_one.wasm")).expect("wasm artifact exists");
    let (box_idx, add_idx, mul_idx, sub_idx, to_index_idx, cmp_idx, eq_idx) =
        aver::codegen::cert::byte_derived_frag_host_role_indices(&wasm).unwrap();
    assert!(
        box_idx.is_some()
            && add_idx.is_some()
            && mul_idx.is_some()
            && sub_idx.is_some()
            && to_index_idx.is_some(),
        "add_one carries the full Int arithmetic runtime; every arithmetic role must bind"
    );
    // `addOne` compares nothing, so the module never calls either comparison
    // helper and therefore does not export one. Both roles are declared absent
    // — truthfully, against bytes that really lack the export — which is the
    // only reading the wall accepts.
    assert!(
        cmp_idx.is_none() && eq_idx.is_none(),
        "add_one has no Int comparison; both comparison roles must be absent"
    );
    assert!(
        manifest["carrier_type_index"].is_u64(),
        "add_one must declare its Int carrier type index"
    );
    assert_eq!(
        manifest["hostRoleTable"],
        serde_json::json!({"box": box_idx, "add": add_idx, "mul": mul_idx, "sub": sub_idx, "toIndex": to_index_idx, "cmp": cmp_idx, "eq": eq_idx}),
        "a module with the Int helper must keep the concrete host-role table"
    );

    // And the Lean manifest binds the same table as `some` — the acceptance
    // equality for carriered modules is exactly as strong as before.
    let manifest_lean =
        std::fs::read_to_string(cert_dir.join("Manifest.lean")).expect("Manifest.lean exists");
    let optional = |idx: Option<u32>| match idx {
        Some(index) => format!("some {index}"),
        None => "none".to_string(),
    };
    let expected_roles = format!(
        "hostRoleTable := some ({{ box := some {}, add := some {}, mul := some {}, sub := some {}, toIndex := some {}, cmp := {}, eq := {} }} : CertDecode.AddSub.Roles),",
        box_idx.unwrap(),
        add_idx.unwrap(),
        mul_idx.unwrap(),
        sub_idx.unwrap(),
        to_index_idx.unwrap(),
        optional(cmp_idx),
        optional(eq_idx),
    );
    assert!(
        manifest_lean.contains(&expected_roles),
        "Manifest.lean must pin the byte-derived table, got:\n{manifest_lean}"
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
    // A project with a dotted module dependency emits its dependency model at
    // a nested path (`Nested/Deep/Util.lean`). The certificate must import it
    // by its dotted module name (`import Nested.Deep.Util`, never the
    // path-shaped `import Nested/Deep/Util`) and the package must build.
    if Command::new("lake").arg("--version").output().is_err() {
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
        report.contains("3 certified"),
        "nestedmods must certify the entry export and both nested-module exports:\n{report}"
    );
    assert!(
        report.contains("Nested_Deep_Util_bump") && report.contains("Nested_Deep_Util_tally"),
        "nestedmods must certify the exports whose models live in the nested module:\n{report}"
    );

    let cert_dir = out_dir.join("cert");
    assert!(
        cert_dir
            .join("Nested")
            .join("Deep")
            .join("Util.lean")
            .is_file(),
        "the nested dependency model must be emitted at its nested path"
    );
    for file in ["Manifest.lean", "Certificate.lean"] {
        let contents = std::fs::read_to_string(cert_dir.join(file))
            .unwrap_or_else(|_| panic!("{file} exists"));
        assert!(
            contents.contains("import Nested.Deep.Util"),
            "{file} must import the nested model by its dotted module name:\n{contents}"
        );
        assert!(
            !contents.contains("import Nested/Deep/Util"),
            "{file} must not emit a path-shaped import line:\n{contents}"
        );
    }
    // The nested-module export's obligation must cite its model by the
    // QUALIFIED name the model file declares (inside `namespace
    // Nested.Deep.Util`), never by the flattened wasm export name — the
    // flattened form is not a Lean identifier in the model and fails the
    // package build.
    let manifest_lean =
        std::fs::read_to_string(cert_dir.join("Manifest.lean")).expect("Manifest.lean exists");
    assert!(
        manifest_lean.contains("model := fun ns => Nested.Deep.Util.bump (ns.headD 0)")
            && manifest_lean.contains("model := fun ns => Nested.Deep.Util.tally (ns.headD 0)"),
        "each nested export's obligation must cite the qualified model name:\n{manifest_lean}"
    );
    assert!(
        !manifest_lean.contains("model := fun ns => Nested_Deep_Util_bump")
            && !manifest_lean.contains("model := fun ns => Nested_Deep_Util_tally"),
        "the obligation must never cite the flattened export name as the model:\n{manifest_lean}"
    );
    // The nested recursion bridge must also cite the model's qualified fuel
    // form (`Nested.Deep.Util.tally__fuel`), never a flattened one.
    let certificate_lean = std::fs::read_to_string(cert_dir.join("Certificate.lean"))
        .expect("Certificate.lean exists");
    assert!(
        certificate_lean.contains("Nested.Deep.Util.tally__fuel"),
        "the recursion bridge must cite the qualified fuel model:\n{certificate_lean}"
    );
    assert!(
        !certificate_lean.contains("Nested_Deep_Util_tally__fuel"),
        "the recursion bridge must never cite a flattened fuel model:\n{certificate_lean}"
    );

    assert_certificate_target_builds(&cert_dir, "nested module models");
}

/// The producer's face gate must refuse a host-call-bearing plan that lands on
/// no admitted face, exactly as the verifier sidecar does.
///
/// `validClockValue` (`match value >= 0 { false -> false; true -> value < limit }`)
/// becomes `if a >= 0 { a < b } else { false }` after the MIR `bool_match_to_if`
/// rewrite: TWO `__aint_cmp` calls nested inside a conditional. Neither Int
/// comparison face covers that node list, and the wall's generic
/// expression-fragment gate rejects every `.hostCall` node outright, so a plan
/// here can only ever decline.
///
/// The observable difference is the DECLINE REASON, and it is the whole point:
/// with the gate the export falls back to the legacy byte-classifier and
/// declines with its template message; without the gate the producer ships a
/// plan claim that the verifier then rejects ("producer fragment plan
/// rejected"). The emitted module bytes are the same either way — the plan's
/// canonical lowering reproduces the emitter's bytes for this shape — so bytes
/// alone cannot witness the gate; the certificate surface can.
#[test]
fn certify_leaves_a_faceless_host_call_shape_on_the_legacy_route() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-clockrange-gate");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/clockrange.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify clockrange failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert_dir = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");

    let certified: BTreeSet<String> = manifest["certified"]
        .as_array()
        .expect("certified report is an array")
        .iter()
        .map(|c| c["name"].as_str().unwrap().to_string())
        .collect();
    assert!(
        !certified.contains("validClockValue"),
        "a faceless host-call shape must not be certified: {certified:?}"
    );

    let reason = manifest["declaredUncertified"]
        .as_array()
        .expect("declaredUncertified report is an array")
        .iter()
        .find(|entry| entry["name"].as_str() == Some("validClockValue"))
        .and_then(|entry| entry["reason"].as_str())
        .expect("validClockValue is declared uncertified with a reason")
        .to_string();
    assert!(
        reason.contains("does not match a certified template"),
        "the export must decline on the legacy byte-classifier route, \
         meaning the producer never selected a plan for it; got: {reason}"
    );
    assert!(
        !reason.contains("producer fragment plan rejected"),
        "the producer emitted a plan the verifier then refused — the face gate \
         is not mirroring the sidecar; got: {reason}"
    );

    // No plan claim reached the certificate at all.
    let plans = std::fs::read_to_string(cert_dir.join("Plans.lean")).expect("Plans.lean exists");
    assert!(
        !plans.contains("validClockValue"),
        "an unplanned export must carry no plan in the certificate:\n{plans}"
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
    if Command::new("lake").arg("--version").output().is_err() {
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
}
