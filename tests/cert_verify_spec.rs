//! Integration tests for `aver cert verify` — the tripwires ARE the product.
//!
//! Compiles a fixture with `--certify`, confirms `aver cert verify` accepts it
//! end to end, then confirms it fails closed on each tampering class:
//!   (a) one flipped wasm byte           → artifact hash mismatch
//!   (b) a corrupted `Module.lean` body  → lake build failure
//!   (c) a trivialized final theorem     → kernel witness rejects the type
//!   (d) a swapped `Schema.lean`         → IGNORED: the checker builds against
//!       its own embedded audited schema, so a cert-supplied schema (weakened
//!       or not) has no effect and the genuine cert still verifies
//!   (e) A1 hash rebind: foreign bytes + a matching `wasm_sha256` in the JSON
//!       → the kernel witness rejects the hash binding
//!   (f) A2 comment smuggle: the approved statement in a comment plus a
//!       `: True := trivial` theorem → the kernel witness rejects the type
//!   (g) A3 build-tree subversion: a decoy `Holds := True` behind a redirected
//!       `srcDir` plus a weak `trivial` final → the checker ignores the cert's
//!       lakefile/srcDir and builds the final against the embedded schema, so
//!       the weak proof fails closed
//!   (h) A3 olean cache: a poisoned `.lake` cache shipped in the cert →
//!       IGNORED: the checker builds in a fresh dir, so the genuine cert still
//!       verifies (the shipped cache is never consumed)
//!   (i) A4 report forgery: appending a fabricated certified export/contract to
//!       ONLY `cert-manifest.json` → the report names/count/contracts are
//!       candidates the kernel witness binds to the proven manifest with `rfl`,
//!       so a lying JSON makes a binding fail and the cert is DECLINED
//!   (j) drift, JSON claims one export MORE than the manifest → DECLINED
//!   (k) drift, JSON claims one export FEWER than the manifest → DECLINED
//!   (l) charset gate: a candidate name carrying a control char → DECLINED
//!       before any splice into the witness
//!   (m) evil axiom: `Final.cert` proved from a smuggled `axiom` → the witness
//!       axiom collector throws on the non-whitelisted axiom
//!   (n) A7 filename gate: a cert file whose name is not a Lean module
//!       identifier → DECLINED (no lakefile-root injection)
//!   (o) A8 token scan: a data file carrying `#eval` → DECLINED (brittle wall)
//!   (p) C2 bytes-vs-data: a `Module.lean` body that builds green and passes the
//!       kernel witness but does NOT decode from the bytes → DECLINED by
//!       re-derivation from the hash-verified bytes (would be CERTIFIED without)
//! plus a separate empty-cert test: zero certified exports must NOT print the
//! green path and must exit nonzero, and the A5 report-line injection payload
//! (in the manifest and/or JSON) is rejected by the charset gate.
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
    aver_cert(&["verify"], artifact, cert_dir)
}

fn aver_cert(sub: &[&str], artifact: &Path, cert_dir: &Path) -> (bool, String) {
    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("cert")
        .args(sub)
        .arg(artifact)
        .arg(cert_dir)
        .output()
        .expect("aver cert runs");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    (out.status.success(), combined)
}

/// A weakened schema whose `Holds` is trivially `True`. Used by the A3 decoy;
/// it defines the same surface the data modules import so the decoy tree would
/// build under the OLD (cert-controlled) build path.
const WEAK_SCHEMA: &str = "import CertPrelude\nimport Module\n\
namespace AverCert.Schema\nopen CertPrelude\n\
structure Subject where\n  artifactHash : String\n  profile : String\n  abi : String\n  \
exports : List String\n  contracts : List String\n\
inductive Policy where\n  | simulatesModel\n\
structure Obligation where\n  export_ : String\n  policy : Policy\n  carrier : Nat\n  \
code : CodeTbl\n  host : (List WVal -> Option WVal) -> (List WVal -> Option WVal) -> HostTbl\n  \
self : Nat\n  model : Int -> Int\n\
def Obligation.holds (_o : Obligation) : Prop := True\n\
structure Manifest where\n  subject : Subject\n  obligations : List Obligation\n\
def Holds (_m : Manifest) : Prop := True\n\
end AverCert.Schema\n";

const WEAK_FINAL: &str = "import Certificate\nimport Manifest\nimport Schema\n\n\
theorem AverCert.Final.cert : AverCert.Schema.Holds manifest := trivial\n\n\
#print axioms AverCert.Final.cert\n";

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

    // Happy path: the freshly emitted certificate verifies end to end. The
    // checker builds in its OWN fresh temp dir, so nothing here is cached for
    // the tamper cases below.
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(ok, "expected clean certificate to verify, got:\n{report}");
    assert!(report.contains("CERTIFIED"), "missing CERTIFIED:\n{report}");
    assert!(
        report.contains("1 certified export"),
        "expected exactly one certified export:\n{report}"
    );
    // The certified bodies were re-derived from the hash-verified bytes and
    // matched against Module.lean: the CERTIFIED report names that guarantee.
    assert!(
        report.contains("artifact-decode: bytes re-derive to the certified bodies"),
        "missing artifact-decode line on the happy path:\n{report}"
    );

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

    // (c) A trivialized final theorem (same name, `: True := trivial`) → it
    //     builds, but the kernel witness ascribes `Final.cert` to
    //     `Holds manifest`, so `True` is rejected.
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

    // (d) A swapped Schema.lean is IGNORED: the checker builds against its own
    //     embedded audited schema, never the cert's. Even a weakened schema in
    //     the cert dir has no effect, so the genuine cert still verifies.
    {
        let dir = temp_dir("neg-d");
        copy_dir(&out_dir, &dir);
        std::fs::write(dir.join("cert").join("Schema.lean"), WEAK_SCHEMA).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(ok, "cert-supplied Schema.lean must be ignored:\n{out}");
        assert!(
            out.contains("CERTIFIED"),
            "genuine cert should verify (d):\n{out}"
        );
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
    //     kernel witness ascribes `Final.cert` to `Holds manifest`, and
    //     `True ≠ Holds manifest`.
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

    // (g) A3 build-tree subversion: point the cert's lakefile `srcDir` at a
    //     hidden decoy tree whose `Holds := True`, and weaken the final proof to
    //     `trivial`. Under the OLD (cert-controlled) build this passed. The
    //     checker now ignores the cert's lakefile/srcDir and builds `Final.lean`
    //     against its OWN embedded schema, so `trivial` fails closed.
    {
        let dir = temp_dir("neg-g");
        copy_dir(&out_dir, &dir);
        let cert = dir.join("cert");
        // Decoy build tree with a weakened schema, reached via srcDir redirect.
        let hidden = cert.join("hidden");
        copy_dir(&out_dir.join("cert"), &hidden);
        std::fs::write(hidden.join("Schema.lean"), WEAK_SCHEMA).unwrap();
        // The (visible) final proof only holds against the trivial `Holds`.
        std::fs::write(cert.join("Final.lean"), WEAK_FINAL).unwrap();
        // Redirect the cert's own lakefile at the decoy tree.
        let lf = cert.join("lakefile.lean");
        let src = std::fs::read_to_string(&lf).unwrap();
        let redirected = src.replace("srcDir := \".\"", "srcDir := \"hidden\"");
        assert_ne!(src, redirected, "lakefile srcDir shape changed");
        std::fs::write(&lf, redirected).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &cert);
        assert!(!ok, "A3 srcDir subversion must fail:\n{out}");
        assert!(out.contains("did not build"), "wrong reason (g):\n{out}");
    }

    // (h) A3 olean cache: ship a poisoned `.lake` cache in the cert. The checker
    //     builds in a fresh dir and never consumes it, so a genuine cert still
    //     verifies (a checker that reused the cache would choke on the garbage).
    {
        let dir = temp_dir("neg-h");
        copy_dir(&out_dir, &dir);
        let lib = dir.join("cert").join(".lake").join("build").join("lib");
        std::fs::create_dir_all(&lib).unwrap();
        std::fs::write(lib.join("Schema.olean"), b"GARBAGE-NOT-AN-OLEAN").unwrap();
        std::fs::write(lib.join("Final.olean"), b"GARBAGE").unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(ok, "shipped .lake cache must be ignored:\n{out}");
        assert!(
            out.contains("CERTIFIED"),
            "genuine cert should verify (h):\n{out}"
        );
    }

    // (i) A4 report forgery: append a fabricated certified export + contract to
    //     ONLY the JSON. The report names/count/contracts are now candidates the
    //     kernel witness binds to the proven Lean manifest with `rfl`, so a JSON
    //     that claims an export or contract the manifest does not have makes a
    //     binding fail: the cert is DECLINED and the forged names never appear.
    {
        let dir = temp_dir("neg-i");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let json = std::fs::read_to_string(&mf).unwrap();
        let mut m: serde_json::Value = serde_json::from_str(&json).unwrap();
        m["certified"]
            .as_array_mut()
            .unwrap()
            .push(serde_json::json!({
                "name": "withdrawAll",
                "class": "straight-line",
                "policy": "simulatesModel",
                "level": "L1",
                "theorem": "CertProofs.withdrawAll_wasm_certified"
            }));
        m["runtime_contracts"]
            .as_array_mut()
            .unwrap()
            .push(serde_json::Value::String("FAKE contract injected".into()));
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        // Every .lean and hash is byte-identical; only the JSON changed.
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "padded JSON must be DECLINED, not credited:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (i):\n{out}");
        // The declined diagnostic echoes the rejected candidate; what matters is
        // that the forged export is never CERTIFIED (credited).
        assert!(
            !out.contains("CERTIFIED"),
            "forged export credited (i):\n{out}"
        );
    }

    // (j) Drift, JSON claims MORE than the manifest: a second certified entry
    //     whose name is charset-clean but absent from the obligations. The
    //     `obligations.length = N` / export-name bindings fail closed.
    {
        let dir = temp_dir("neg-j");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["certified"].as_array_mut().unwrap().push(serde_json::json!({
            "name": "phantom", "class": "straight-line", "policy": "simulatesModel", "level": "L1"
        }));
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "JSON claiming an extra export must fail (j):\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (j):\n{out}");
        assert!(
            !out.contains("CERTIFIED"),
            "phantom export credited (j):\n{out}"
        );
    }

    // (k) Drift, JSON claims FEWER than the manifest: an empty `certified` while
    //     the manifest still proves one obligation. `length = 0 := rfl` fails.
    {
        let dir = temp_dir("neg-k");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["certified"] = serde_json::Value::Array(vec![]);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "JSON dropping a real export must fail (k):\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (k):\n{out}");
    }

    // (l) Charset gate: a certified name carrying a control character (decoded
    //     from the JSON) is rejected before any splice, so it can never reach
    //     the Lean witness.
    {
        let dir = temp_dir("neg-l");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["certified"][0]["name"] = serde_json::Value::String("sumTo\nevil := by rfl".into());
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "control char in a candidate must fail (l):\n{out}");
        assert!(out.contains("charset"), "wrong reason (l):\n{out}");
    }

    // (m) Evil axiom: prove `Final.cert` from a smuggled `axiom evil`. The build
    //     succeeds (an axiom is valid Lean), but the witness runs the kernel's
    //     axiom collector over the ascribed constant and throws on `evil`.
    {
        let dir = temp_dir("neg-m");
        copy_dir(&out_dir, &dir);
        let f = dir.join("cert").join("Final.lean");
        let evil = "import Certificate\nimport Manifest\nimport Schema\n\n\
             open AverCert AverCert.Schema\n\n\
             axiom evil : AverCert.Schema.Holds AverCert.manifest\n\
             theorem AverCert.Final.cert : AverCert.Schema.Holds manifest := evil\n";
        std::fs::write(&f, evil).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "axiom-backed final theorem must fail (m):\n{out}");
        assert!(
            out.contains("non-whitelisted axiom"),
            "witness axiom collector not exercised (m):\n{out}"
        );
    }

    // (n) A7 filename gate: a cert data file whose name is not a plain Lean
    //     module identifier (a space here) is rejected before staging, so it
    //     cannot inject tokens into the checker-authored lakefile roots.
    {
        let dir = temp_dir("neg-n");
        copy_dir(&out_dir, &dir);
        std::fs::write(
            dir.join("cert").join("bad name.lean"),
            "-- inert\ndef x : Nat := 0\n",
        )
        .unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "hostile cert file name must fail (n):\n{out}");
        assert!(
            out.contains("module identifier"),
            "wrong reason (n):\n{out}"
        );
    }

    // (o) A8 token scan: a cert data file carrying an elaboration-executes-code
    //     token is rejected before it is staged (deliberately brittle wall).
    {
        let dir = temp_dir("neg-o");
        copy_dir(&out_dir, &dir);
        let c = dir.join("cert").join("Contracts.lean");
        let mut src = std::fs::read_to_string(&c).unwrap();
        src.push_str("\n#eval IO.println \"pwned\"\n");
        std::fs::write(&c, src).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "code-executing token in a data file must fail (o):\n{out}"
        );
        assert!(
            out.contains("execute code") && out.contains("#eval"),
            "wrong reason (o):\n{out}"
        );
    }

    // (p) C2 bytes-vs-data divergence: a Module.lean whose `sumToCode` body does
    //     NOT decode from the real bytes. The locals count in the CodeTbl entry
    //     is bumped 1 -> 2 (an extra, unused local), which the recursive proof
    //     tolerates: the cert still `lake build`s AND passes the kernel witness
    //     (hash, count, names, `Holds manifest`, axioms all check). Only the
    //     re-derivation from the hash-verified bytes catches that the declared
    //     body diverges from what the bytes decode to. The wasm bytes are
    //     untouched, so the hash stays consistent — the mismatch is purely in the
    //     attacker-editable Lean data. Without re-derivation this cert would be
    //     CERTIFIED green (the vacuity/divergence residual C2); with it, DECLINED.
    {
        let dir = temp_dir("neg-p");
        copy_dir(&out_dir, &dir);
        let m = dir.join("cert").join("Module.lean");
        let src = std::fs::read_to_string(&m).unwrap();
        let corrupted = src.replacen("some ⟨1, 1,", "some ⟨1, 2,", 1);
        assert_ne!(src, corrupted, "fixture recursive body shape changed");
        std::fs::write(&m, corrupted).unwrap();
        // wasm bytes are untouched: the hash still matches the pinned value.
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "a body that does not re-derive must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("do not re-derive the certified body of `sumTo`"),
            "wrong reason (p): expected the re-derivation mismatch:\n{out}"
        );
        // The mismatch is caught by re-derivation, AFTER the cert built and the
        // kernel witness passed — not by lake build or a hash/witness binding.
        assert!(
            !out.contains("did not build") && !out.contains("does not bind"),
            "case (p) must be caught by re-derivation, not the build/witness:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "a diverging body must never be credited (p):\n{out}"
        );
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

    // `explain` / `inspect` share verify's fail-closed exit contract: an
    // admission-only cert (zero certified exports) must not exit green or show a
    // green CERTIFIED header from either subcommand.
    for sub in [["explain"], ["inspect"]] {
        let (ok_e, out_e) = aver_cert(&sub, &out_dir.join("certempty.wasm"), &out_dir.join("cert"));
        assert!(!ok_e, "empty cert `{}` must exit nonzero:\n{out_e}", sub[0]);
        assert!(
            out_e.contains("NO CERTIFIED EXPORTS") && !out_e.contains("\u{1b}[32m"),
            "empty cert `{}` must report admission-only, not green CERTIFIED:\n{out_e}",
            sub[0]
        );
    }

    // A5 report-line injection (the BANK verbatim attack), Manifest + JSON:
    // stash the fabricated `AVERCERT-EXPORT\tstealAllFunds` report line in the
    // subject `contracts`, in BOTH the Lean manifest and (consistently) the
    // JSON, over an empty obligations list. There is no report parser anymore,
    // and the newline/tab in the candidate is rejected by the charset gate
    // before any splice: DECLINED, and `stealAllFunds` is never credited.
    {
        let dir = temp_dir("certempty-a5");
        copy_dir(&out_dir, &dir);
        let man = dir.join("cert").join("Manifest.lean");
        let src = std::fs::read_to_string(&man).unwrap();
        let payload_lean = "[\"x\\nAVERCERT-EXPORT\\tstealAllFunds\\tsimulatesModel\"]";
        let poisoned = src.replacen(
            "contracts := []",
            &format!("contracts := {payload_lean}"),
            1,
        );
        assert_ne!(src, poisoned, "empty-cert manifest contracts shape changed");
        std::fs::write(&man, poisoned).unwrap();
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["runtime_contracts"] = serde_json::Value::Array(vec![serde_json::Value::String(
            "x\nAVERCERT-EXPORT\tstealAllFunds\tsimulatesModel".into(),
        )]);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certempty.wasm"), &dir.join("cert"));
        assert!(!ok, "A5 injection payload must fail:\n{out}");
        assert!(
            out.contains("charset"),
            "wrong reason (A5 manifest):\n{out}"
        );
        // The charset diagnostic echoes the rejected value; the property is that
        // the payload is never CERTIFIED.
        assert!(
            !out.contains("CERTIFIED"),
            "A5 payload credited an export:\n{out}"
        );
    }

    // A5 JSON-only variant: the same payload only in the JSON (manifest left
    // empty). Still DECLINED by the charset gate.
    {
        let dir = temp_dir("certempty-a5json");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["runtime_contracts"] = serde_json::Value::Array(vec![serde_json::Value::String(
            "x\nAVERCERT-EXPORT\tstealAllFunds\tsimulatesModel".into(),
        )]);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certempty.wasm"), &dir.join("cert"));
        assert!(!ok, "A5 JSON-only payload must fail:\n{out}");
        assert!(out.contains("charset"), "wrong reason (A5 json):\n{out}");
        assert!(
            !out.contains("CERTIFIED"),
            "A5 JSON-only payload credited an export:\n{out}"
        );
    }

    // A4 empty-cert honesty: a JSON padded with a fabricated certified export
    // now fails the kernel binding (the count is `obligations.length = N` by
    // rfl, and the empty manifest proves zero), so it is DECLINED, not credited.
    let mf = out_dir.join("cert").join("cert-manifest.json");
    let json = std::fs::read_to_string(&mf).unwrap();
    let mut m: serde_json::Value = serde_json::from_str(&json).unwrap();
    m["certified"]
        .as_array_mut()
        .unwrap()
        .push(serde_json::json!({
            "name": "withdrawAll", "policy": "simulatesModel", "level": "L1"
        }));
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
    let (ok, out) = aver_verify(&out_dir.join("certempty.wasm"), &out_dir.join("cert"));
    assert!(!ok, "padded empty cert must still exit nonzero:\n{out}");
    assert!(
        out.contains("does not bind") && !out.contains("CERTIFIED"),
        "padded JSON must be DECLINED, not credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}
