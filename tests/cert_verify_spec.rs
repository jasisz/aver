//! Integration tests for `aver cert verify` — the tripwires ARE the product.
//!
//! Compiles a fixture with `--certify`, confirms `aver cert verify` accepts it
//! end to end, then confirms it fails closed on each tampering class with the
//! expected reason:
//!   (a) one flipped wasm byte           → artifact hash mismatch
//!   (b) a corrupted `Module.lean` body  → lake build failure
//!   (c) a trivialized final theorem     → kernel witness rejects the type
//!   (d) a tampered `Schema.lean`        → not the audited version
//!   (e) A1 hash rebind: foreign bytes + a matching `wasm_sha256` in the JSON
//!       → the kernel witness rejects the hash binding
//!   (f) A2 comment smuggle: the approved statement in a comment plus a
//!       `: True := trivial` theorem → the kernel witness rejects the type
//! plus a separate empty-cert test: zero certified exports must NOT print the
//! green path and must exit nonzero.
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
    //     kernel witness ascribes `Final.cert` to `Holds manifest`, so `True`
    //     is rejected.
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
        assert!(out.contains("does not bind"), "wrong reason (c):\n{out}");
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

    // (e) A1 hash rebind: replace the artifact with arbitrary bytes and edit
    //     ONLY `wasm_sha256` in the JSON to match. The fast JSON pre-check now
    //     passes; the kernel witness rejects it because the theorems talk about
    //     the ORIGINAL hash, not the checker-computed one.
    {
        let dir = temp_dir("neg-e");
        copy_dir(&out_dir, &dir);
        let foreign = b"\x00\xde\xad\xbe\xef arbitrary not-a-wasm bytes".to_vec();
        std::fs::write(dir.join("certprobe2.wasm"), &foreign).unwrap();
        let sha = aver::codegen::cert::sha256_hex(&foreign);
        let mf = dir.join("cert").join("cert-manifest.json");
        let json = std::fs::read_to_string(&mf).unwrap();
        let mut m: serde_json::Value = serde_json::from_str(&json).unwrap();
        m["wasm_sha256"] = serde_json::Value::String(sha);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "A1 hash rebind must fail:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (e):\n{out}");
        // The witness names the exact face the kernel rejected.
        assert!(
            out.contains("artifactHash"),
            "witness not exercised (e):\n{out}"
        );
    }

    // (f) A2 comment smuggle: the approved statement line present only in a
    //     COMMENT, plus a `theorem AverCert.Final.cert : True := trivial`. The
    //     deleted substring check would have passed; the kernel witness ascribes
    //     `Final.cert` to `Holds manifest`, and `True ≠ Holds manifest`.
    {
        let dir = temp_dir("neg-f");
        copy_dir(&out_dir, &dir);
        let f = dir.join("cert").join("Final.lean");
        let smuggled = "import Certificate\nimport Manifest\nimport Schema\n\n\
             -- theorem AverCert.Final.cert : AverCert.Schema.Holds manifest := by trivial\n\
             theorem AverCert.Final.cert : True := trivial\n\n\
             #print axioms AverCert.Final.cert\n";
        std::fs::write(&f, smuggled).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "A2 comment smuggle must fail:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (f):\n{out}");
        assert!(out.contains("Holds"), "witness not exercised (f):\n{out}");
    }

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A cert with zero certified exports is an admission, not a certification: it
/// must NOT print the green CERTIFIED path and must exit nonzero (fail-closed).
#[test]
fn empty_cert_is_admission_only_and_exits_nonzero() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping empty-cert test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certempty");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/certempty.av")
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

    let (ok, out) = aver_verify(&out_dir.join("certempty.wasm"), &out_dir.join("cert"));
    assert!(!ok, "empty cert must exit nonzero:\n{out}");
    assert!(
        out.contains("NO CERTIFIED EXPORTS"),
        "empty cert must be reported as admission-only:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED /") && !out.contains("\u{1b}[32m"),
        "empty cert must not print the green CERTIFIED path:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}
