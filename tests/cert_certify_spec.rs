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
