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
