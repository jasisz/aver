//! Integration tests for `aver cert verify` — the tripwires ARE the product.
//!
//! Compiles a fixture with `--certify`, confirms `aver cert verify` accepts it
//! end to end, then confirms it fails closed on each of the four tampering
//! classes with the expected reason:
//!   (a) one flipped wasm byte           → artifact hash mismatch
//!   (b) a corrupted `Module.lean` body  → lake build failure
//!   (c) a trivialized final theorem     → not the approved statement
//!   (d) a tampered `Schema.lean`        → not the audited version
//!
//! Gated behind `wasm` (the `--certify` path needs the wasm-gc backend) and
//! skipped when `lake` is unavailable, mirroring `cert_certify_spec.rs`.
#![cfg(feature = "wasm")]

use std::path::{Path, PathBuf};
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

fn copy_dir(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).unwrap();
    for entry in std::fs::read_dir(src).unwrap() {
        let entry = entry.unwrap();
        let to = dst.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_dir(&entry.path(), &to);
        } else {
            std::fs::copy(entry.path(), &to).unwrap();
        }
    }
}

fn aver_verify(artifact: &Path, cert_dir: &Path) -> (bool, String) {
    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("cert")
        .arg("verify")
        .arg(artifact)
        .arg(cert_dir)
        .output()
        .expect("aver cert verify runs");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    (out.status.success(), combined)
}

#[test]
fn cert_verify_accepts_and_tripwires_fail_closed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cert verify test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certverify");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    // Emit the recursive fixture's certificate.
    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/certprobe2.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("certprobe2.wasm");
    let cert = out_dir.join("cert");

    // Happy path: the freshly emitted certificate verifies end to end. This
    // also warms the `.lake` build cache the tamper cases below reuse.
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(ok, "expected clean certificate to verify, got:\n{report}");
    assert!(report.contains("CERTIFIED"), "missing CERTIFIED:\n{report}");

    // (a) One flipped wasm byte → hash mismatch, before any build.
    {
        let dir = temp_dir("neg-a");
        copy_dir(&out_dir, &dir);
        let w = dir.join("certprobe2.wasm");
        let mut bytes = std::fs::read(&w).unwrap();
        let mid = bytes.len() / 2;
        bytes[mid] ^= 0x01;
        std::fs::write(&w, &bytes).unwrap();
        let (ok, out) = aver_verify(&w, &dir.join("cert"));
        assert!(!ok, "flipped wasm byte must fail:\n{out}");
        assert!(out.contains("hash mismatch"), "wrong reason (a):\n{out}");
    }

    // (b) A corrupted Module.lean instruction → lake build failure.
    {
        let dir = temp_dir("neg-b");
        copy_dir(&out_dir, &dir);
        let m = dir.join("cert").join("Module.lean");
        let src = std::fs::read_to_string(&m).unwrap();
        let corrupted = src.replacen(".i64Const 0, .i64LeS", ".i64Const 999, .i64LeS", 1);
        assert_ne!(src, corrupted, "fixture body shape changed");
        std::fs::write(&m, corrupted).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "corrupted Module.lean must fail:\n{out}");
        assert!(out.contains("did not build"), "wrong reason (b):\n{out}");
    }

    // (c) A trivialized final theorem (same name, `: True := trivial`) → the
    //     approved statement line is gone.
    {
        let dir = temp_dir("neg-c");
        copy_dir(&out_dir, &dir);
        let f = dir.join("cert").join("Final.lean");
        let trivial = "import Certificate\nimport Manifest\nimport Schema\n\n\
             theorem AverCert.Final.cert : True := trivial\n\n\
             #print axioms AverCert.Final.cert\n";
        std::fs::write(&f, trivial).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "trivialized final theorem must fail:\n{out}");
        assert!(out.contains("approved shape"), "wrong reason (c):\n{out}");
    }

    // (d) A tampered Schema.lean → not the audited version.
    {
        let dir = temp_dir("neg-d");
        copy_dir(&out_dir, &dir);
        let s = dir.join("cert").join("Schema.lean");
        let mut src = std::fs::read_to_string(&s).unwrap();
        src.push_str("\n-- tampered\n");
        std::fs::write(&s, src).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "tampered Schema.lean must fail:\n{out}");
        assert!(out.contains("audited version"), "wrong reason (d):\n{out}");
    }

    let _ = std::fs::remove_dir_all(&out_dir);
}
