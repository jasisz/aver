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
//!   (p) bytes-vs-data, body divergence: a `Module.lean` `sumToCode` whose
//!       locals count is bumped 1→2. It still builds green AND passes the old
//!       report bindings, but the checker pins `manifest.obligations.map (·.code)`
//!       to the bytes-derived lambda with `rfl`, so the diverging body fails the
//!       kernel witness → DECLINED ("does not bind"), never CERTIFIED
//!   (q) shadow decoy: the active `sumToCode` mutated (locals 1→2) PLUS a full
//!       honest body re-planted in a `namespace Shadow`. The decoy text does not
//!       change `o.code`, so the code `rfl` still fails → DECLINED
//!   (r) comment decoy: the active `sumToCode` mutated PLUS a full honest body in
//!       a `/- … -/` block comment. Dead text; the code `rfl` fails → DECLINED
//!   (s) code decouple: `manifest` points `code` at a decoy `wrongCode` that
//!       always traps (making `holds` vacuous and trivially provable) while the
//!       honest `sumToCode` is dead. Builds green; the code `rfl` binds `o.code`
//!       to the bytes, not `wrongCode`, so it fails → DECLINED
//!   (t) self decouple (vacuity): `manifest` sets `self` to a wrong index, so the
//!       code-table lookup misses and `wFuncN` traps (vacuous, provable `holds`).
//!       Builds green; the self `rfl` binds `o.self` to the byte index → DECLINED
//!   (u) String.eq helper shape: a byte-level mutation inside the exact
//!       compiler-generated helper, with the wasm hash rebound, makes the
//!       checker re-derive a different host table and the kernel witness fails
//!       the host binding → DECLINED
//!   (v) String.eq contract drift: deleting the byte-required contract from
//!       both `Manifest.lean` and `cert-manifest.json` still fails because the
//!       checker re-derives runtime contracts from the wasm bytes → DECLINED
//!   (w) String.concat helper shape / contract drift: same fail-closed checks
//!       for the concat helper's byte-exact host-contract recognition
//!   (x) expr-fragment sidecar drift: mutating the emitted canonical plan either
//!       fails its sidecar hash pin or, with the hash rebound, fails plan-first
//!       canonical lowering against the actual wasm body
//!   (y) plan-check TCB pin drift: rebinding `plan_check_sha256` to a different
//!       checker is rejected before Lean build, so certs cannot ship their own
//!       weakened `PlanCheck.lean`
//!   (z) plan-lower TCB pin drift: rebinding `plan_lower_sha256` to a different
//!       checker is rejected before Lean build, so certs cannot ship their own
//!       weakened `PlanLower.lean`
//!   (aa) plan-bytes TCB pin drift: rebinding `plan_bytes_sha256` to a different
//!       checker is rejected before Lean build, so certs cannot ship their own
//!       weakened `PlanBytes.lean`
//!   (ab) wasm-slice TCB pin drift: rebinding `wasm_slice_sha256` to a different
//!       checker is rejected before Lean build, so certs cannot ship their own
//!       weakened `WasmSlice.lean`
//!   (ac) expr-fragment-accepted TCB pin drift: rebinding
//!       `expr_fragment_accepted_sha256` to a different checker is rejected
//!       before Lean build
//!   (ad) accepted-artifact TCB pin drift: rebinding
//!       `accepted_artifact_sha256` to a different checker is rejected before
//!       Lean build
//!   (ae) artifact-bytes decoy: cert-supplied `ArtifactBytes.lean` is ignored;
//!       the checker regenerates it from the actual artifact bytes it read
//!   (af) artifact-data decoy: cert-supplied `Artifact.lean` data is pinned to
//!       the checker-reconstructed artifact data with `rfl`
//!   (ag) artifact-root axiom: the artifact-carried bridge proof is the axiom
//!       audit root, so a smuggled axiom there is rejected
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
inductive ReprAll (R : Int -> WVal -> Prop) : List Int -> List WVal -> Prop\n\
  | nil : ReprAll R [] []\n\
  | cons {n v ns vs} : R n v -> ReprAll R ns vs -> ReprAll R (n :: ns) (v :: vs)\n\
structure CarrierSpec (C : Nat) where\n  Repr : Int -> WVal -> Prop\n  \
car : forall n v, Repr n v -> (exists s sg, v = .structv C [.i64v s, .null, .i32v sg]) \\/ (exists s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg])\n  \
smallIntro : forall k : Int, Repr k (carrierSmall C k)\n  \
smallElim : forall n s sg, Repr n (.structv C [.i64v s, .null, .i32v sg]) -> s = n\n  \
bigElim : forall n s lty les sg, Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) -> ((sg < 0) <-> (n < 0)) /\\ n != 0\n\
def intRepr (S : CarrierSpec C) : Int -> WVal -> Prop := S.Repr\n\
def boolRepr (_S : CarrierSpec C) (b : Bool) (w : WVal) : Prop := w = b32 b\n\
def verbatimRepr (_S : CarrierSpec C) (v : WVal) (w : WVal) : Prop := w = v\n\
structure SymRawPlan where\n\
structure ExprFragmentRawPlan where\n\
structure Obligation where\n  export_ : String\n  policy : Policy\n  carrier : Nat\n  \
code : CodeTbl\n  host : (List WVal -> Option WVal) -> (List WVal -> Option WVal) -> (List WVal -> Option WVal) -> (List WVal -> Option WVal) -> (Nat -> List WVal -> Option WVal) -> HostTbl\n  \
self : Nat\n  Dom : Type\n  Cod : Type\n  domRepr : CarrierSpec carrier -> Dom -> List WVal -> Prop\n  codRepr : CarrierSpec carrier -> Cod -> WVal -> Prop\n  model : Dom -> Cod\n\
def Obligation.holds (_o : Obligation) : Prop := True\n\
structure Manifest where\n  subject : Subject\n  obligations : List Obligation\n\
  symFragmentPlans : List (String × SymRawPlan)\n  exprFragmentPlans : List (String × ExprFragmentRawPlan)\n\
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
        report.contains("2 certified exports"),
        "expected exactly two certified exports:\n{report}"
    );
    // Each obligation's code/host/self/carrier was pinned to the bytes-derived
    // values by `rfl` inside the kernel witness: the CERTIFIED report names that
    // guarantee (artifact-decode).
    assert!(
        report.contains("artifact-decode:") && report.contains("kernel-pinned"),
        "missing artifact-decode line on the happy path:\n{report}"
    );

    // Schema v3 is a breaking cert-data shape. The checker rejects v1 manifests
    // honestly instead of trying to reinterpret them under the v3 schema.
    {
        let dir = temp_dir("neg-schema-v1");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["schema_version"] = serde_json::json!(1);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "schema v1 cert must be rejected:\n{out}");
        assert!(
            out.contains("unsupported certificate schema_version 1"),
            "wrong reason for schema v1 rejection:\n{out}"
        );
    }

    // The plan checker is audited TCB, not artifact data. A cert may carry a
    // `PlanCheck.lean` file for human audit, but the verifier builds against
    // its embedded copy and rejects a manifest pin for any other copy.
    {
        let dir = temp_dir("neg-plancheck-pin");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["plan_check_sha256"] = serde_json::json!("not-the-audited-plan-check");
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "plan-check hash drift must be rejected:\n{out}");
        assert!(
            out.contains("plan-check hash mismatch"),
            "wrong reason for plan-check hash drift:\n{out}"
        );
    }

    // The plan lowerer is audited TCB too. A cert may carry `PlanLower.lean`
    // for auditability, but verification only accepts the embedded checker
    // copy and its exact hash.
    {
        let dir = temp_dir("neg-planlower-pin");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["plan_lower_sha256"] = serde_json::json!("not-the-audited-plan-lower");
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "plan-lower hash drift must be rejected:\n{out}");
        assert!(
            out.contains("plan-lower hash mismatch"),
            "wrong reason for plan-lower hash drift:\n{out}"
        );
    }

    // The plan byte encoder is audited TCB as well. It is the Lean-side
    // canonical RawPlan -> code-entry byte encoder, so its manifest pin must
    // match the checker-owned copy.
    {
        let dir = temp_dir("neg-planbytes-pin");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["plan_bytes_sha256"] = serde_json::json!("not-the-audited-plan-bytes");
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "plan-bytes hash drift must be rejected:\n{out}");
        assert!(
            out.contains("plan-bytes hash mismatch"),
            "wrong reason for plan-bytes hash drift:\n{out}"
        );
    }

    // The relevant Wasm byte slicer is audited TCB too. It binds checker-read
    // module bytes to export-named code-entry bytes inside Lean, so its manifest
    // pin must match the checker-owned copy.
    {
        let dir = temp_dir("neg-wasmslice-pin");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["wasm_slice_sha256"] = serde_json::json!("not-the-audited-wasm-slice");
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "wasm-slice hash drift must be rejected:\n{out}");
        assert!(
            out.contains("wasm-slice hash mismatch"),
            "wrong reason for wasm-slice hash drift:\n{out}"
        );
    }

    // The aggregate expr-fragment acceptance predicate is audited TCB. A cert
    // cannot swap it for a weaker definition and rebind the manifest.
    {
        let dir = temp_dir("neg-expr-fragment-accepted-pin");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["expr_fragment_accepted_sha256"] =
            serde_json::json!("not-the-audited-expr-fragment-accepted");
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "expr-fragment-accepted hash drift must be rejected:\n{out}"
        );
        assert!(
            out.contains("expr-fragment-accepted hash mismatch"),
            "wrong reason for expr-fragment-accepted hash drift:\n{out}"
        );
    }

    // The artifact-acceptance bridge is audited TCB too. A cert cannot swap the
    // obligation bridge for a weaker definition and rebind the manifest.
    {
        let dir = temp_dir("neg-accepted-artifact-pin");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["accepted_artifact_sha256"] = serde_json::json!("not-the-audited-accepted-artifact");
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "accepted-artifact hash drift must be rejected:\n{out}");
        assert!(
            out.contains("accepted-artifact hash mismatch"),
            "wrong reason for accepted-artifact hash drift:\n{out}"
        );
    }

    // The artifact-carried data root is useful metadata, not authority. Even
    // when a fixture has no expr-fragment claims, the checker pins
    // `AverCert.Artifact.data` to its own reconstruction before accepting the
    // artifact-level root.
    {
        let dir = temp_dir("neg-artifact-data-pin");
        copy_dir(&out_dir, &dir);
        let artifact = dir.join("cert").join("Artifact.lean");
        let src = std::fs::read_to_string(&artifact).unwrap();
        let corrupted = src.replacen(
            "wasmBytes := AverCert.ArtifactBytes.wasmBytes",
            "wasmBytes := []",
            1,
        );
        assert_ne!(src, corrupted, "Artifact.lean data shape changed");
        std::fs::write(&artifact, corrupted).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "tampered Artifact.lean data must fail:\n{out}");
        assert!(
            out.contains("AverCert.Artifact.data") || out.contains("does not bind"),
            "wrong reason for artifact data tamper:\n{out}"
        );
    }

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

    // (a2) A byte flipped inside the newly certified `countDown` body is still a
    //      hard decline. This is intentionally caught by the artifact hash
    //      before the checker spends time building Lean.
    {
        let dir = temp_dir("neg-a2-countdown-body");
        copy_dir(&out_dir, &dir);
        let w = dir.join("certprobe2.wasm");
        let mut bytes = std::fs::read(&w).unwrap();
        let count_down_prefix = [
            0x20, 0x01, 0x05, 0x20, 0x00, 0x42, 0x01, 0x10, 0x07, 0x10, 0x09, 0x20, 0x01, 0x20,
            0x00, 0x10, 0x08, 0x12, 0x02,
        ];
        let off = bytes
            .windows(count_down_prefix.len())
            .position(|win| win == count_down_prefix)
            .expect("countDown body prefix should be present in wasm");
        bytes[off + 1] ^= 0x01;
        std::fs::write(&w, &bytes).unwrap();
        let (ok, out) = aver_verify(&w, &dir.join("cert"));
        assert!(!ok, "countDown body-byte flip must fail:\n{out}");
        assert!(
            out.contains("hash mismatch"),
            "wrong reason for countDown body-byte flip:\n{out}"
        );
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
    //     artifact-carried self-check root imports `Final.cert`, so the cert
    //     build fails before the checker witness can ascribe `Final.cert` to
    //     `Holds manifest`.
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
        assert!(out.contains("did not build"), "wrong reason (c):\n{out}");
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

    // (e) A1 hash rebind: replace the artifact with a DIFFERENT but genuine cert
    //     module (certprobe's wasm) and edit ONLY `wasm_sha256` in the JSON to
    //     match it. The fast JSON pre-check passes and the swapped module still
    //     disassembles, so the checker reaches the kernel witness — which rejects
    //     it because the theorems (and `CertModule.wasmSha256`) talk about the
    //     ORIGINAL hash, not the checker-computed one.
    {
        let dir = temp_dir("neg-e");
        copy_dir(&out_dir, &dir);
        let foreign_out = temp_dir("neg-e-foreign");
        let fc = Command::new(aver_bin)
            .current_dir(&repo_root)
            .arg("compile")
            .arg("tools/certkit/fixtures/certprobe.av")
            .arg("--target")
            .arg("wasm-gc")
            .arg("--certify")
            .arg("-o")
            .arg(&foreign_out)
            .output()
            .expect("aver compile --certify runs");
        assert!(fc.status.success(), "foreign fixture compile failed");
        let foreign = std::fs::read(foreign_out.join("certprobe.wasm")).unwrap();
        std::fs::write(dir.join("certprobe2.wasm"), &foreign).unwrap();
        let sha = aver::codegen::cert::sha256_hex(&foreign);
        let mf = dir.join("cert").join("cert-manifest.json");
        let json = std::fs::read_to_string(&mf).unwrap();
        let mut m: serde_json::Value = serde_json::from_str(&json).unwrap();
        m["wasm_sha256"] = serde_json::Value::String(sha);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        let _ = std::fs::remove_dir_all(&foreign_out);
        assert!(!ok, "A1 hash rebind must fail:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (e):\n{out}");
        // The witness names the exact face the kernel rejected.
        assert!(
            out.contains("CertModule.wasmSha256"),
            "witness not exercised (e):\n{out}"
        );
    }

    // (f) A2 comment smuggle: the approved statement line present only in a
    //     COMMENT, plus a `theorem AverCert.Final.cert : True := trivial`. The
    //     artifact-carried self-check root imports `Final.cert`, so this now
    //     fails at cert build time before the checker witness.
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
        assert!(out.contains("did not build"), "wrong reason (f):\n{out}");
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

    // (p) bytes-vs-data divergence: a Module.lean whose `sumToCode` body does NOT
    //     decode from the real bytes. The locals count in the CodeTbl entry is
    //     bumped 1 -> 2 (an extra, unused local), which the recursive proof
    //     tolerates: the cert still `lake build`s AND passes the old report
    //     bindings (hash, count, names). The checker now splices the bytes-derived
    //     code lambda into the witness and pins `manifest.obligations.map (·.code)`
    //     to it with `rfl`; the bumped body (locals 2) is not the byte-derived one
    //     (locals 1), so the kernel witness fails: DECLINED, never CERTIFIED. The
    //     wasm bytes are untouched, so the hash stays consistent — the mismatch is
    //     purely in the attacker-editable Lean data.
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
        // Caught by the kernel witness (the code binding), AFTER the cert built
        // green — not by lake build or a hash binding.
        assert!(out.contains("does not bind"), "wrong reason (p):\n{out}");
        assert!(
            !out.contains("did not build"),
            "case (p) must build green and be caught by the witness:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "a diverging body must never be credited (p):\n{out}"
        );
    }

    // (q) Shadow decoy (the reproduced bypass): mutate the ACTIVE `sumToCode`
    //     (locals 1→2) and re-plant a byte-identical honest body in a
    //     `namespace Shadow`. The old substring check matched the honest text in
    //     `Shadow` and passed; the code `rfl` pins `o.code` — which is the active,
    //     mutated `CertModule.sumToCode`, not the shadow — so it fails: DECLINED.
    {
        let dir = temp_dir("neg-q");
        copy_dir(&out_dir, &dir);
        let m = dir.join("cert").join("Module.lean");
        let src = std::fs::read_to_string(&m).unwrap();
        let mutated = src.replacen("some ⟨1, 1,", "some ⟨1, 2,", 1);
        assert_ne!(src, mutated, "fixture recursive body shape changed");
        let shadow = format!("namespace Shadow\n{HONEST_SUMTO_CODE}\nend Shadow\n\nend CertModule");
        let planted = mutated.replacen("end CertModule", &shadow, 1);
        assert_ne!(mutated, planted, "shadow decoy not planted");
        std::fs::write(&m, planted).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "shadow decoy must be DECLINED:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (q):\n{out}");
        assert!(
            !out.contains("CERTIFIED"),
            "shadow decoy credited (q):\n{out}"
        );
    }

    // (r) Comment decoy: mutate the active `sumToCode` and re-plant a byte-honest
    //     body inside a `/- … -/` block comment. Dead text; `o.code` is still the
    //     mutated active def, so the code `rfl` fails: DECLINED.
    {
        let dir = temp_dir("neg-r");
        copy_dir(&out_dir, &dir);
        let m = dir.join("cert").join("Module.lean");
        let src = std::fs::read_to_string(&m).unwrap();
        let mutated = src.replacen("some ⟨1, 1,", "some ⟨1, 2,", 1);
        assert_ne!(src, mutated, "fixture recursive body shape changed");
        let comment = format!("/- honest decoy:\n{HONEST_SUMTO_CODE}\n-/\n\nend CertModule");
        let planted = mutated.replacen("end CertModule", &comment, 1);
        std::fs::write(&m, planted).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "comment decoy must be DECLINED:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (r):\n{out}");
        assert!(
            !out.contains("CERTIFIED"),
            "comment decoy credited (r):\n{out}"
        );
    }

    // (s) Code decouple: point the obligation's `code` at a decoy `wrongCode` that
    //     always traps, so `holds` is vacuous and trivially provable, while the
    //     honest `sumToCode` is left dead. The cert builds green (the vacuous proof
    //     replaces the honest one). The code `rfl` binds `o.code` to the
    //     bytes-derived lambda, not `wrongCode`, so the witness fails: DECLINED.
    {
        let dir = temp_dir("neg-s");
        copy_dir(&out_dir, &dir);
        let cert = dir.join("cert");
        let m = cert.join("Module.lean");
        let src = std::fs::read_to_string(&m).unwrap();
        let with_decoy = src.replacen(
            "end CertModule",
            "/-- decoy: always traps, so `holds` is vacuous. -/\n\
             def wrongCode : CodeTbl := fun _ => none\nend CertModule",
            1,
        );
        std::fs::write(&m, with_decoy).unwrap();
        let man = cert.join("Manifest.lean");
        let msrc = std::fs::read_to_string(&man).unwrap();
        let decoupled = msrc.replacen(
            "code := CertModule.sumToCode",
            "code := CertModule.wrongCode",
            1,
        );
        assert_ne!(msrc, decoupled, "manifest code field shape changed");
        std::fs::write(&man, decoupled).unwrap();
        let vac = VACUOUS_SIMULATES.replace(
            "@BODY@",
            "simp only [sumToOb, CertModule.wrongCode, wFuncN, reduceCtorEq] at hrun",
        );
        replace_simulates(&cert, &vac);
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &cert);
        assert!(!ok, "code decouple must be DECLINED:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (s):\n{out}");
        assert!(
            !out.contains("did not build"),
            "case (s) must build green and be caught by the witness:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "code decouple credited (s):\n{out}"
        );
    }

    // (t) Self decouple (vacuity): set the obligation's `self` to a wrong index so
    //     `code self` misses the table and `wFuncN` traps — `holds` is vacuous and
    //     provable. Builds green; the self `rfl` binds `o.self` to the byte index,
    //     so the witness fails: DECLINED.
    {
        let dir = temp_dir("neg-t");
        copy_dir(&out_dir, &dir);
        let cert = dir.join("cert");
        let man = cert.join("Manifest.lean");
        let msrc = std::fs::read_to_string(&man).unwrap();
        let decoupled = msrc.replacen("self := 1,", "self := 999,", 1);
        assert_ne!(msrc, decoupled, "manifest self field shape changed");
        std::fs::write(&man, decoupled).unwrap();
        let vac = VACUOUS_SIMULATES.replace(
            "@BODY@",
            "simp only [sumToOb, CertModule.sumToCode, wFuncN,\n      \
             show (999 = 1) = False by decide, if_false, reduceCtorEq] at hrun",
        );
        replace_simulates(&cert, &vac);
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &cert);
        assert!(!ok, "self decouple must be DECLINED:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (t):\n{out}");
        assert!(
            !out.contains("did not build"),
            "case (t) must build green and be caught by the witness:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "self decouple credited (t):\n{out}"
        );
    }

    // (u) Export-name relabel: keep the byte-bound honest body/self/carrier, but
    //     relabel the first obligation (and the JSON) to a duplicate export name
    //     (`countDown`). The certified export names are re-derived from the
    //     module's export section and pinned by `rfl`, so the label list no
    //     longer matches the export table → DECLINED.
    {
        let dir = temp_dir("neg-u");
        copy_dir(&out_dir, &dir);
        let man = dir.join("cert").join("Manifest.lean");
        let mt = std::fs::read_to_string(&man)
            .unwrap()
            .replace("export_ := \"sumTo\"", "export_ := \"countDown\"")
            .replace("exports := [\"sumTo\"]", "exports := [\"countDown\"]");
        std::fs::write(&man, mt).unwrap();
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        for c in m["certified"].as_array_mut().unwrap() {
            if c["name"] == serde_json::json!("sumTo") {
                c["name"] = serde_json::json!("countDown");
            }
        }
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "export-name relabel must be DECLINED (u):\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (u):\n{out}");
        assert!(!out.contains("CERTIFIED"), "relabel credited (u):\n{out}");
    }

    // (v) M1 semantic-face vacuity — `Dom := Empty`. Empty the sumTo obligation's
    //     domain so `holds` quantifies over no inhabitant, and swap in a vacuous
    //     `ns.elim` proof. It builds green AND passes the code/host/self/carrier
    //     bindings, but the witness proves `Nonempty o.Dom` over every obligation
    //     and `Empty` has no such instance → DECLINED (the panel's M1 attack).
    {
        let dir = temp_dir("neg-v-empty-dom");
        copy_dir(&out_dir, &dir);
        let man = dir.join("cert").join("Manifest.lean");
        let msrc = std::fs::read_to_string(&man).unwrap();
        let edited = msrc.replacen(SUMTO_FACE, SUMTO_FACE_EMPTY_DOM, 1);
        assert_ne!(msrc, edited, "sumToOb face shape changed; update the test");
        std::fs::write(&man, edited).unwrap();
        replace_simulates(
            &dir.join("cert"),
            "theorem sumTo_simulates : AverCert.Schema.Obligation.holds sumToOb := by\n  \
             intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel ns vs w hrepr hrun\n  \
             exact ns.elim",
        );
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "Dom := Empty vacuity must be DECLINED (v):\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (v):\n{out}");
        assert!(
            !out.contains("did not build"),
            "case (v) must build green and be caught by the witness:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "empty-domain cert credited (v):\n{out}"
        );
    }

    // (w) M2 semantic-face vacuity — `codRepr := fun _ _ _ => True` plus a wrong
    //     model. The codomain representation is trivialised, so `holds` is
    //     provable by `trivial` regardless of what the body computes; the model
    //     is changed to a wrong constant to show the false green. It builds green,
    //     but the witness pins `codRepr` to `intRepr` by `HEq.rfl` → DECLINED
    //     (the panel's M2 attack).
    {
        let dir = temp_dir("neg-w-true-codrepr");
        copy_dir(&out_dir, &dir);
        let man = dir.join("cert").join("Manifest.lean");
        let msrc = std::fs::read_to_string(&man).unwrap();
        let edited = msrc.replacen(SUMTO_FACE, SUMTO_FACE_TRUE_CODREPR, 1);
        assert_ne!(msrc, edited, "sumToOb face shape changed; update the test");
        std::fs::write(&man, edited).unwrap();
        replace_simulates(
            &dir.join("cert"),
            "theorem sumTo_simulates : AverCert.Schema.Obligation.holds sumToOb := by\n  \
             intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel ns vs w hrepr hrun\n  \
             trivial",
        );
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "codRepr := True vacuity must be DECLINED (w):\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (w):\n{out}");
        assert!(
            !out.contains("did not build"),
            "case (w) must build green and be caught by the witness:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "true-codRepr cert credited (w):\n{out}"
        );
    }

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// The byte-honest sumTo obligation face emitted for certprobe2 (the standard
/// integer-class form), and the two panel face-vacuity mutations of it.
const SUMTO_FACE: &str = "Dom := List Int, Cod := Int,\n    \
    domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = 1,\n    \
    codRepr := fun S n w => intRepr S n w,\n    \
    model := fun ns => sumTo (ns.headD 0) }";
const SUMTO_FACE_EMPTY_DOM: &str = "Dom := Empty, Cod := Int,\n    \
    domRepr := fun _ (e : Empty) _ => e.elim,\n    \
    codRepr := fun S n w => intRepr S n w,\n    \
    model := fun (e : Empty) => e.elim }";
const SUMTO_FACE_TRUE_CODREPR: &str = "Dom := List Int, Cod := Int,\n    \
    domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = 1,\n    \
    codRepr := fun _ _ _ => True,\n    \
    model := fun _ => 999 }";

/// The byte-honest `sumToCode` body for certprobe2, used verbatim as a decoy in
/// the shadow/comment cases (planting the honest TEXT must not change `o.code`).
const HONEST_SUMTO_CODE: &str = "/-- Verbatim emitted body of `sumTo` (self-recursive). -/\n\
    def sumToCode : CodeTbl := fun fn =>\n  \
    if fn = 1 then some ⟨1, 1,\n    \
    [ .localGet 0, .localSet 1,\n      \
    .localGet 1, .structGet 2 1, .refIsNull,\n      \
    .ifElse [.localGet 1, .structGet 2 0, .i64Const 0, .i64LeS]\n              \
    [.localGet 1, .structGet 2 2, .i32Const 0, .i32LtS],\n      \
    .ifElse [.i64Const 0, .call 7]\n              \
    [.localGet 0, .localGet 0, .i64Const 1, .call 7, .call 9, .call 1, .call 8] ]⟩\n  \
    else none";

/// A vacuous replacement proof of `sumTo_simulates` (the emitted honest proof
/// references the honest `sumToCode`/`self`, so a decoupled obligation needs its
/// own proof to build green). `@BODY@` is filled with the `simp` that discharges
/// the trapped `wFuncN`.
const VACUOUS_SIMULATES: &str = "theorem sumTo_simulates : AverCert.Schema.Obligation.holds sumToOb := by\n  \
    intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel ns vs w hrepr hrun\n  \
    exfalso\n  \
    cases fuel with\n  \
    | zero => simp only [wFuncN, reduceCtorEq] at hrun\n  \
    | succ f =>\n      @BODY@";

/// Swap the emitted `sumTo_simulates` proof in `Certificate.lean` for a
/// (vacuous) replacement, matching the exact emitted block.
fn replace_simulates(cert_dir: &Path, replacement: &str) {
    let c = cert_dir.join("Certificate.lean");
    let src = std::fs::read_to_string(&c).unwrap();
    let old = "theorem sumTo_simulates : AverCert.Schema.Obligation.holds sumToOb := by\n  \
        intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel ns vs w hrepr hrun\n  \
        simp only [sumToOb, AverCert.Schema.Obligation.holds] at hrun ⊢\n  \
        obtain ⟨hrepr, harity⟩ := hrepr\n  \
        cases hrepr with\n  \
        | nil =>\n      \
        simp at harity\n  \
        | cons hv htail =>\n      \
        rename_i n v ns vs\n      \
        cases htail with\n      \
        | nil =>\n          \
        simpa [AverCert.Schema.intRepr] using sumTo_wasm_certified S.Repr S.car S.smallIntro S.smallElim S.bigElim\n            \
        add sub hadd hsub fuel n v w hv hrun\n      \
        | cons _ _ =>\n          \
        simp at harity";
    assert!(
        src.contains(old),
        "emitted sumTo_simulates block shape changed; update the test"
    );
    std::fs::write(&c, src.replacen(old, replacement, 1)).unwrap();
}

#[test]
fn cert_verify_declines_tampered_array_new_data_operands() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping array.new_data tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-json-data");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/data/json.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "json compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("json.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(ok, "expected clean json certificate to verify:\n{report}");
    assert!(
        report.contains("12 certified exports"),
        "json should certify the widened data-segment functions:\n{report}"
    );

    let dir = temp_dir("cert-json-data-tamper");
    copy_dir(&out_dir, &dir);
    let w = dir.join("json.wasm");
    let mut bytes = std::fs::read(&w).unwrap();
    // i32.const 0; i32.const 0; array.new_data type16 seg11 (the empty string
    // literal). Changing the length operand to 1 violates the decoder's
    // fail-closed data-segment guard while keeping the module parseable.
    let pat = [0x41, 0x00, 0x41, 0x00, 0xfb, 0x09, 0x10, 0x0b];
    let mut hits = 0usize;
    for i in 0..bytes.len().saturating_sub(pat.len()) {
        if bytes[i..].starts_with(&pat) {
            bytes[i + 3] = 0x01;
            hits += 1;
        }
    }
    assert!(
        hits > 0,
        "expected empty array.new_data literal in json wasm"
    );
    std::fs::write(&w, &bytes).unwrap();

    let old_hash = {
        let mf = dir.join("cert").join("cert-manifest.json");
        let m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["wasm_sha256"].as_str().unwrap().to_string()
    };
    let new_hash = aver::codegen::cert::sha256_hex(&bytes);
    for file in ["Module.lean", "Manifest.lean"] {
        let path = dir.join("cert").join(file);
        let src = std::fs::read_to_string(&path).unwrap();
        assert!(src.contains(&old_hash), "{file} should pin the old hash");
        std::fs::write(&path, src.replace(&old_hash, &new_hash)).unwrap();
    }
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    m["wasm_sha256"] = serde_json::Value::String(new_hash);
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

    let (ok, out) = aver_verify(&w, &dir.join("cert"));
    let _ = std::fs::remove_dir_all(&out_dir);
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        !ok,
        "tampered array.new_data operands must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("does not bind"),
        "wrong reason for array.new_data tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered data segment credited:\n{out}"
    );
}

#[test]
fn cert_verify_declines_tampered_expr_fragment_sidecar() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping expr-fragment sidecar tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-expr-sidecar");
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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify goals failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("cert_goals.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(ok, "expected clean goals certificate to verify:\n{report}");

    let artifact_bytes_decoy_dir = temp_dir("cert-expr-artifact-bytes-decoy");
    copy_dir(&out_dir, &artifact_bytes_decoy_dir);
    std::fs::write(
        artifact_bytes_decoy_dir
            .join("cert")
            .join("ArtifactBytes.lean"),
        "namespace AverCert.ArtifactBytes\n\ndef wasmBytes : List Nat := []\n\nend AverCert.ArtifactBytes\n",
    )
    .unwrap();
    let (ok, out) = aver_verify(
        &artifact_bytes_decoy_dir.join("cert_goals.wasm"),
        &artifact_bytes_decoy_dir.join("cert"),
    );
    assert!(
        ok,
        "cert-supplied ArtifactBytes.lean must be ignored and regenerated:\n{out}"
    );

    let artifact_axiom_tamper_dir = temp_dir("cert-expr-artifact-axiom-tamper");
    copy_dir(&out_dir, &artifact_axiom_tamper_dir);
    let artifact_axiom_tamper_wasm = artifact_axiom_tamper_dir.join("cert_goals.wasm");
    let artifact_axiom_tamper_cert = artifact_axiom_tamper_dir.join("cert");
    let artifact_lean = artifact_axiom_tamper_cert.join("Artifact.lean");
    let artifact_text = std::fs::read_to_string(&artifact_lean).unwrap();
    let def_start = artifact_text
        .find("def acceptedWithFinal")
        .expect("Artifact.lean should define acceptedWithFinal");
    let end_marker = "end AverCert.Artifact\n";
    let def_end = artifact_text
        .find(end_marker)
        .expect("Artifact.lean should close namespace");
    let evil_bridge = "axiom artifactEvil : ∀ (finalCert : AverCert.Schema.Holds AverCert.manifest), AverCert.AcceptedArtifact.accepted data\n\n\
def acceptedWithFinal\n    (finalCert : AverCert.Schema.Holds AverCert.manifest) :\n    AverCert.AcceptedArtifact.accepted data := artifactEvil finalCert\n\n";
    let mut tampered_artifact = String::new();
    tampered_artifact.push_str(&artifact_text[..def_start]);
    tampered_artifact.push_str(evil_bridge);
    tampered_artifact.push_str(&artifact_text[def_end..]);
    std::fs::write(&artifact_lean, tampered_artifact).unwrap();
    let (ok, out) = aver_verify(&artifact_axiom_tamper_wasm, &artifact_axiom_tamper_cert);
    assert!(
        !ok,
        "artifact-carried axiom bridge must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("non-whitelisted axiom") && out.contains("artifactEvil"),
        "wrong reason for artifact bridge axiom:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "artifact-carried axiom bridge credited:\n{out}"
    );

    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(cert.join("cert-manifest.json")).unwrap())
            .unwrap();
    let expr_entry = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["class"].as_str() == Some("expr-fragment-v1"))
        .expect("at least one expr-fragment sidecar");
    let plan = expr_entry["fragment"]["plan"]
        .as_str()
        .expect("expr-fragment plan path");
    assert!(
        expr_entry["fragment"].get("trace").is_none()
            && expr_entry["fragment"].get("trace_sha256").is_none(),
        "expr-fragment manifests should not emit trace/replay sidecars"
    );
    let float_entry = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"].as_str() == Some("floatAddGoal"))
        .expect("floatAddGoal expr-fragment sidecar");
    let float_plan = float_entry["fragment"]["plan"]
        .as_str()
        .expect("floatAddGoal plan path");

    let plan_dir = temp_dir("cert-expr-plan-sidecar");
    copy_dir(&out_dir, &plan_dir);
    let plan_wasm = plan_dir.join("cert_goals.wasm");
    let plan_cert = plan_dir.join("cert");
    let plan_sidecar = plan_cert.join(plan);
    let mut plan_text = std::fs::read_to_string(&plan_sidecar).unwrap();
    plan_text.push_str("tamper extra-node\n");
    std::fs::write(&plan_sidecar, plan_text).unwrap();

    let (ok, out) = aver_verify(&plan_wasm, &plan_cert);
    assert!(
        !ok,
        "tampered expr-fragment plan sidecar must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("sidecar") && out.contains("hash mismatch"),
        "wrong reason for expr-fragment plan sidecar tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered expr-fragment plan sidecar credited:\n{out}"
    );

    let planfirst_tamper_dir = temp_dir("cert-expr-planfirst-tamper");
    copy_dir(&out_dir, &planfirst_tamper_dir);
    let planfirst_tamper_wasm = planfirst_tamper_dir.join("cert_goals.wasm");
    let planfirst_tamper_cert = planfirst_tamper_dir.join("cert");
    let float_plan_sidecar = planfirst_tamper_cert.join(float_plan);
    let float_plan_text = std::fs::read_to_string(&float_plan_sidecar).unwrap();
    let tampered_float_plan = if float_plan_text.contains("op=float.add") {
        float_plan_text.replacen("op=float.add", "op=float.mul", 1)
    } else {
        float_plan_text.replacen("op=f64.add", "op=f64.mul", 1)
    };
    assert_ne!(
        float_plan_text, tampered_float_plan,
        "floatAddGoal plan shape changed"
    );
    std::fs::write(&float_plan_sidecar, &tampered_float_plan).unwrap();
    let planfirst_tamper_mf = planfirst_tamper_cert.join("cert-manifest.json");
    let mut planfirst_tamper_manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&planfirst_tamper_mf).unwrap()).unwrap();
    let planfirst_tamper_entry = planfirst_tamper_manifest["certified"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|c| c["name"].as_str() == Some("floatAddGoal"))
        .expect("floatAddGoal sidecar");
    planfirst_tamper_entry["fragment"]["plan_sha256"] = serde_json::Value::String(
        aver::codegen::cert::sha256_hex(tampered_float_plan.as_bytes()),
    );
    std::fs::write(
        &planfirst_tamper_mf,
        serde_json::to_string_pretty(&planfirst_tamper_manifest).unwrap(),
    )
    .unwrap();

    let (ok, out) = aver_verify(&planfirst_tamper_wasm, &planfirst_tamper_cert);
    assert!(
        !ok,
        "tampered plan-first expr-fragment must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("plan-first canonical lowering"),
        "wrong reason for plan-first plan tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered plan-first expr-fragment credited:\n{out}"
    );

    let lean_plan_tamper_dir = temp_dir("cert-expr-lean-plan-tamper");
    copy_dir(&out_dir, &lean_plan_tamper_dir);
    let lean_plan_tamper_wasm = lean_plan_tamper_dir.join("cert_goals.wasm");
    let lean_plan_tamper_cert = lean_plan_tamper_dir.join("cert");
    let plans_lean = lean_plan_tamper_cert.join("Plans.lean");
    let plans_text = std::fs::read_to_string(&plans_lean).unwrap();
    let tampered_plans_text = plans_text.replacen(".f64Add [0, 1]", ".f64Mul [0, 1]", 1);
    assert_ne!(
        plans_text, tampered_plans_text,
        "Plans.lean floatAddGoal shape changed"
    );
    std::fs::write(&plans_lean, tampered_plans_text).unwrap();

    let (ok, out) = aver_verify(&lean_plan_tamper_wasm, &lean_plan_tamper_cert);
    assert!(!ok, "tampered Lean RawPlan data must be DECLINED:\n{out}");
    let old_body_pin_failed =
        out.contains("PlanLower.lowerExprFragmentBody") && out.contains("floatAddGoalCode");
    let plan_byte_or_aggregate_pin_failed = out.contains("PlanBytes.lowerExprFragmentCodeEntry")
        || out.contains("ExprFragmentAccepted.accepted");
    assert!(
        old_body_pin_failed || plan_byte_or_aggregate_pin_failed,
        "wrong reason for Lean RawPlan tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered Lean RawPlan data credited:\n{out}"
    );

    let lean_bytes_tamper_dir = temp_dir("cert-expr-lean-bytes-tamper");
    copy_dir(&out_dir, &lean_bytes_tamper_dir);
    let lean_bytes_tamper_wasm = lean_bytes_tamper_dir.join("cert_goals.wasm");
    let lean_bytes_tamper_cert = lean_bytes_tamper_dir.join("cert");
    let plans_lean = lean_bytes_tamper_cert.join("Plans.lean");
    let plans_text = std::fs::read_to_string(&plans_lean).unwrap();
    let honest_bytes = "some [10, 1, 1, 99, 18, 32, 0, 32, 1, 160, 11]";
    let tampered_bytes = "some [10, 1, 1, 99, 18, 32, 0, 32, 1, 161, 11]";
    assert!(
        plans_text.contains(honest_bytes),
        "Plans.lean floatAddGoal byte pin changed"
    );
    std::fs::write(
        &plans_lean,
        plans_text.replacen(honest_bytes, tampered_bytes, 1),
    )
    .unwrap();

    let (ok, out) = aver_verify(&lean_bytes_tamper_wasm, &lean_bytes_tamper_cert);
    assert!(
        !ok,
        "tampered Lean code-entry byte pin must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("PlanBytes.lowerExprFragmentCodeEntry") && out.contains("floatAddGoalPlan"),
        "wrong reason for Lean code-entry byte pin tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered Lean code-entry byte pin credited:\n{out}"
    );

    let lean_slice_tamper_dir = temp_dir("cert-expr-lean-slice-tamper");
    copy_dir(&out_dir, &lean_slice_tamper_dir);
    let lean_slice_tamper_wasm = lean_slice_tamper_dir.join("cert_goals.wasm");
    let lean_slice_tamper_cert = lean_slice_tamper_dir.join("cert");
    let plans_lean = lean_slice_tamper_cert.join("Plans.lean");
    let plans_text = std::fs::read_to_string(&plans_lean).unwrap();
    let marker = "__HONEST_FLOATADD_BYTES__";
    let slice_tampered_bytes = "some [11, 1, 1, 99, 18, 32, 0, 32, 1, 160, 11]";
    assert!(
        plans_text.matches(honest_bytes).count() >= 2,
        "Plans.lean should contain both PlanBytes and WasmSlice floatAddGoal byte pins"
    );
    let marked_first = plans_text.replacen(honest_bytes, marker, 1);
    let tampered_second = marked_first
        .replacen(honest_bytes, slice_tampered_bytes, 1)
        .replace(marker, honest_bytes);
    std::fs::write(&plans_lean, tampered_second).unwrap();

    let (ok, out) = aver_verify(&lean_slice_tamper_wasm, &lean_slice_tamper_cert);
    assert!(
        !ok,
        "tampered Lean WasmSlice byte-origin pin must be DECLINED:\n{out}"
    );
    // A false `rfl` over the full `ArtifactBytes.wasmBytes` literal can fail
    // either as a normal `WasmSlice.codeEntryForExport` type mismatch or as a
    // Lean stack overflow while reducing the huge byte list. Both are
    // fail-closed build failures for this untrusted emitted audit example.
    assert!(
        out.contains("WasmSlice.codeEntryForExport")
            || (out.contains("Plans") && out.contains("Stack overflow")),
        "wrong reason for Lean WasmSlice byte-origin pin tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered Lean WasmSlice byte-origin pin credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
    let _ = std::fs::remove_dir_all(&artifact_bytes_decoy_dir);
    let _ = std::fs::remove_dir_all(&artifact_axiom_tamper_dir);
    let _ = std::fs::remove_dir_all(&plan_dir);
    let _ = std::fs::remove_dir_all(&planfirst_tamper_dir);
    let _ = std::fs::remove_dir_all(&lean_plan_tamper_dir);
    let _ = std::fs::remove_dir_all(&lean_bytes_tamper_dir);
    let _ = std::fs::remove_dir_all(&lean_slice_tamper_dir);
}

#[test]
fn cert_verify_declines_tampered_string_eq_helper_shape() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping String.eq helper tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-stringeq");
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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "stringeq compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("stringeq.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(
        ok,
        "expected clean stringeq certificate to verify:\n{report}"
    );
    assert!(
        report.contains("2 certified exports"),
        "stringeq should certify quoteOrSelf plus bump:\n{report}"
    );
    assert!(
        report.contains("quoteOrSelf  class: verbatim widened match"),
        "quoteOrSelf should reuse the verbatim widened face, not introduce a new class:\n{report}"
    );
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
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

    {
        let dir = temp_dir("cert-stringeq-contract-drift");
        copy_dir(&out_dir, &dir);
        let man = dir.join("cert").join("Manifest.lean");
        let src = std::fs::read_to_string(&man).unwrap();
        let needle = format!(", \"{}\"", aver::codegen::cert::STRING_EQ_CONTRACT);
        assert!(
            src.contains(&needle),
            "Manifest.lean should contain String.eq contract"
        );
        std::fs::write(&man, src.replace(&needle, "")).unwrap();

        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        let contracts = m["runtime_contracts"].as_array_mut().unwrap();
        let before = contracts.len();
        contracts.retain(|c| c.as_str() != Some(aver::codegen::cert::STRING_EQ_CONTRACT));
        assert_eq!(
            contracts.len(),
            before - 1,
            "JSON manifest should contain one String.eq contract"
        );
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

        let (ok, out) = aver_verify(&dir.join("stringeq.wasm"), &dir.join("cert"));
        let _ = std::fs::remove_dir_all(&dir);
        assert!(!ok, "deleted String.eq contract must be DECLINED:\n{out}");
        assert!(
            out.contains("does not bind"),
            "wrong reason for deleted String.eq contract:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "deleted String.eq contract credited:\n{out}"
        );
    }

    let dir = temp_dir("cert-stringeq-tamper");
    copy_dir(&out_dir, &dir);
    let w = dir.join("stringeq.wasm");
    let mut bytes = std::fs::read(&w).unwrap();
    // Tail of the compiler-generated String.eq helper loop:
    // local.get 3; i32.const 1; i32.add; local.set 3; br 0; end; end;
    // i32.const 1; end. Flipping the final true literal keeps the wasm
    // parseable but makes the helper fail the exact host matcher.
    let pat = [
        0x20, 0x03, 0x41, 0x01, 0x6a, 0x21, 0x03, 0x0c, 0x00, 0x0b, 0x0b, 0x41, 0x01, 0x0b,
    ];
    let hits: Vec<usize> = bytes
        .windows(pat.len())
        .enumerate()
        .filter_map(|(i, win)| (win == pat).then_some(i))
        .collect();
    assert_eq!(
        hits.len(),
        1,
        "expected exactly one String.eq helper loop tail, got {hits:?}"
    );
    bytes[hits[0] + 12] = 0x00;
    std::fs::write(&w, &bytes).unwrap();

    let old_hash = {
        let mf = dir.join("cert").join("cert-manifest.json");
        let m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["wasm_sha256"].as_str().unwrap().to_string()
    };
    let new_hash = aver::codegen::cert::sha256_hex(&bytes);
    for file in ["Module.lean", "Manifest.lean"] {
        let path = dir.join("cert").join(file);
        let src = std::fs::read_to_string(&path).unwrap();
        assert!(src.contains(&old_hash), "{file} should pin the old hash");
        std::fs::write(&path, src.replace(&old_hash, &new_hash)).unwrap();
    }
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    m["wasm_sha256"] = serde_json::Value::String(new_hash);
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

    let (ok, out) = aver_verify(&w, &dir.join("cert"));
    let _ = std::fs::remove_dir_all(&out_dir);
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        !ok,
        "tampered String.eq helper shape must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("does not bind"),
        "wrong reason for String.eq helper tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered String.eq helper credited:\n{out}"
    );
}

#[test]
fn cert_verify_declines_tampered_string_concat_helper_shape() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping String.concat helper tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-stringconcat");
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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "stringconcat compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("stringconcat.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(
        ok,
        "expected clean stringconcat certificate to verify:\n{report}"
    );
    assert!(
        report.contains("2 certified exports"),
        "stringconcat should certify shout plus bump:\n{report}"
    );
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
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
    let shout_class = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"].as_str() == Some("shout"))
        .and_then(|c| c["class"].as_str())
        .unwrap_or("<missing>");
    assert_eq!(
        shout_class, "verbatim-string-concat",
        "shout should render its concat class, got {shout_class}"
    );

    {
        let dir = temp_dir("cert-stringconcat-contract-drift");
        copy_dir(&out_dir, &dir);
        let man = dir.join("cert").join("Manifest.lean");
        let src = std::fs::read_to_string(&man).unwrap();
        let needle = format!(", \"{}\"", aver::codegen::cert::STRING_CONCAT_CONTRACT);
        assert!(
            src.contains(&needle),
            "Manifest.lean should contain String.concat contract"
        );
        std::fs::write(&man, src.replace(&needle, "")).unwrap();

        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        let contracts = m["runtime_contracts"].as_array_mut().unwrap();
        let before = contracts.len();
        contracts.retain(|c| c.as_str() != Some(aver::codegen::cert::STRING_CONCAT_CONTRACT));
        assert_eq!(
            contracts.len(),
            before - 1,
            "JSON manifest should contain one String.concat contract"
        );
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

        let (ok, out) = aver_verify(&dir.join("stringconcat.wasm"), &dir.join("cert"));
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            !ok,
            "deleted String.concat contract must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("does not bind"),
            "wrong reason for deleted String.concat contract:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "deleted String.concat contract credited:\n{out}"
        );
    }

    let dir = temp_dir("cert-stringconcat-tamper");
    copy_dir(&out_dir, &dir);
    let w = dir.join("stringconcat.wasm");
    let mut bytes = std::fs::read(&w).unwrap();
    // First String.concat helper loop:
    // local.get 2; local.get 3; i32.ge_u; br_if 1; local.get 1; local.get 0; local.get 2.
    // Changing the exit branch depth to 0 keeps the wasm parseable but makes the
    // byte-exact helper matcher reject the function.
    let pat = [
        0x20, 0x02, 0x20, 0x03, 0x4f, 0x0d, 0x01, 0x20, 0x01, 0x20, 0x00, 0x20, 0x02,
    ];
    let hits: Vec<usize> = bytes
        .windows(pat.len())
        .enumerate()
        .filter_map(|(i, win)| (win == pat).then_some(i))
        .collect();
    assert_eq!(
        hits.len(),
        1,
        "expected exactly one String.concat first-loop prefix, got {hits:?}"
    );
    bytes[hits[0] + 6] = 0x00;
    std::fs::write(&w, &bytes).unwrap();

    let old_hash = {
        let mf = dir.join("cert").join("cert-manifest.json");
        let m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["wasm_sha256"].as_str().unwrap().to_string()
    };
    let new_hash = aver::codegen::cert::sha256_hex(&bytes);
    for file in ["Module.lean", "Manifest.lean"] {
        let path = dir.join("cert").join(file);
        let src = std::fs::read_to_string(&path).unwrap();
        assert!(src.contains(&old_hash), "{file} should pin the old hash");
        std::fs::write(&path, src.replace(&old_hash, &new_hash)).unwrap();
    }
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    m["wasm_sha256"] = serde_json::Value::String(new_hash);
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

    let (ok, out) = aver_verify(&w, &dir.join("cert"));
    let _ = std::fs::remove_dir_all(&out_dir);
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        !ok,
        "tampered String.concat helper shape must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("does not bind"),
        "wrong reason for String.concat helper tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered String.concat helper credited:\n{out}"
    );
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

/// An ADT class carries its witness body in `Module.lean` exactly like the
/// integer classes: mutating the emitted `greetCode` (field-projection witness)
/// so it no longer decodes from the bytes still builds green, but the checker
/// pins `manifest.obligations.map (·.code)` to the byte-derived lambda by `rfl`,
/// so the diverging body fails the kernel witness → DECLINED, never CERTIFIED.
#[test]
fn adt_witness_body_mutation_is_declined() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping ADT witness-mutation test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-adt-mut");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/core/user_record.av")
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

    // Bump the field-projection witness body's local count 1 -> 2 (an extra,
    // unused local): the projection proof tolerates it, so the cert builds AND
    // passes the report bindings, but the mutated body is not the byte-derived
    // one, so the code `rfl` fails.
    let m = out_dir.join("cert").join("Module.lean");
    let src = std::fs::read_to_string(&m).unwrap();
    let mutated = src.replacen("some ⟨1, 1,", "some ⟨1, 2,", 1);
    assert_ne!(src, mutated, "emitted greetCode header shape changed");
    std::fs::write(&m, mutated).unwrap();

    let (ok, out) = aver_verify(&out_dir.join("user_record.wasm"), &out_dir.join("cert"));
    assert!(!ok, "mutated ADT witness body must be DECLINED:\n{out}");
    assert!(out.contains("does not bind"), "wrong reason:\n{out}");
    assert!(
        !out.contains("did not build"),
        "must build green and be caught by the witness:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "mutated ADT witness credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn variant_dispatch_body_mutation_is_declined() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping variant-dispatch witness-mutation test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-vd-mut");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/signalgauge.av")
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

    // Bump the dispatch witness body's local count (an extra, unused local):
    // the walker proof tolerates it, so the cert builds green, but the mutated
    // body is not the byte-derived one, so the code `rfl` fails.
    let m = out_dir.join("cert").join("Module.lean");
    let src = std::fs::read_to_string(&m).unwrap();
    let start = src.find("some ⟨1, ").expect("a unary code-table header") + "some ⟨1, ".len();
    let len = src[start..].find(',').expect("locals count terminator");
    let nlocals: u32 = src[start..start + len]
        .trim()
        .parse()
        .expect("locals count");
    let header = format!("some ⟨1, {nlocals},");
    let bumped = format!("some ⟨1, {},", nlocals + 1);
    let mutated = src.replacen(&header, &bumped, 1);
    assert_ne!(src, mutated, "emitted gaugeCode header shape changed");
    std::fs::write(&m, mutated).unwrap();

    let (ok, out) = aver_verify(&out_dir.join("signalgauge.wasm"), &out_dir.join("cert"));
    assert!(
        !ok,
        "mutated dispatch witness body must be DECLINED:\n{out}"
    );
    assert!(out.contains("does not bind"), "wrong reason:\n{out}");
    assert!(
        !out.contains("CERTIFIED"),
        "mutated dispatch witness credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

#[test]
fn composition_callee_mutation_is_declined() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping composition-mutation test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-compose-mut");
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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    // Bump the CALLEE (double) entry's local count in the caller's shared
    // multi-entry table: the extra unused local keeps the cert's own build
    // green, so only the checker's whole-table code `rfl` — which re-derives
    // every closure entry from the bytes — can catch the decoupling. This is
    // the load-bearing tripwire for cross-function composition.
    let m = out_dir.join("cert").join("Module.lean");
    let src = std::fs::read_to_string(&m).unwrap();
    let mutated = src.replacen(
        "if fn = 1 then some ⟨1, 1,",
        "if fn = 1 then some ⟨1, 2,",
        1,
    );
    assert_ne!(
        src, mutated,
        "emitted shared-table callee header shape changed"
    );
    std::fs::write(&m, mutated).unwrap();

    let (ok, out) = aver_verify(&out_dir.join("compose.wasm"), &out_dir.join("cert"));
    assert!(
        !ok,
        "mutated composition callee entry must be DECLINED:\n{out}"
    );
    assert!(out.contains("does not bind"), "wrong reason:\n{out}");
    assert!(
        !out.contains("CERTIFIED"),
        "tampered composition cert must not verify:\n{out}"
    );
}
