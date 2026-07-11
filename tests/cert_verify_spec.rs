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

fn aver_command() -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command.env(
        "AVER_CERT_PRELUDE_CACHE",
        std::env::temp_dir().join("aver-cert-prelude-store"),
    );
    command
}

fn aver_verify(artifact: &Path, cert_dir: &Path) -> (bool, String) {
    aver_cert(&["verify"], artifact, cert_dir)
}

fn aver_cert(sub: &[&str], artifact: &Path, cert_dir: &Path) -> (bool, String) {
    let out = aver_command()
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

fn aver_verify_clean_cache(artifact: &Path, cert_dir: &Path) -> (bool, String) {
    // Clean-cache litmus for the production verifier path: keep exactly this
    // positive end-to-end verification independent of the test-only store.
    let out = aver_command()
        .env_remove("AVER_CERT_PRELUDE_CACHE")
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

fn compile_cert_goals(prefix: &str) -> (PathBuf, PathBuf, PathBuf) {
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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify goals failed:
{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("cert_goals.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(
        ok,
        "expected clean goals certificate to verify:
{report}"
    );
    assert!(
        report.contains("CERTIFIED"),
        "clean goals certificate should be certified:
{report}"
    );
    (out_dir, wasm, cert)
}

fn source_fragment_plan_path(cert_dir: &Path, export_name: &str) -> String {
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert_dir.join("cert-manifest.json")).unwrap(),
    )
    .unwrap();
    manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"].as_str() == Some(export_name))
        .and_then(|c| c["source_fragment"]["plan"].as_str())
        .unwrap_or_else(|| panic!("{export_name} should have a source fragment plan"))
        .to_string()
}

fn rebind_source_fragment_plan_sha(cert_dir: &Path, export_name: &str, plan_text: &str) {
    let mf = cert_dir.join("cert-manifest.json");
    let mut manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    let entry = manifest["certified"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|c| c["name"].as_str() == Some(export_name))
        .unwrap_or_else(|| panic!("{export_name} manifest entry should exist"));
    entry["source_fragment"]["plan_sha256"] =
        serde_json::Value::String(aver::codegen::cert::sha256_hex(plan_text.as_bytes()));
    std::fs::write(&mf, serde_json::to_string_pretty(&manifest).unwrap()).unwrap();
}

fn assert_source_plan_byte_mismatch(export_name: &str, out: &str) {
    assert!(
        out.contains("source plan-first canonical lowering")
            && out.contains("does not match the actual wasm code-entry"),
        "wrong reason for {export_name} source plan tamper:
{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered {export_name} source plan credited:
{out}"
    );
}

/// A weakened schema whose `Holds` is trivially `True`. Used by the A3 decoy;
/// it defines the same surface the data modules import so the decoy tree would
/// build under the OLD (cert-controlled) build path.
const WEAK_SCHEMA: &str = "import CertPrelude\nimport Module\n\
namespace AverCert.Schema\nopen CertPrelude\n\
structure Subject where\n  artifactHash : String\n  profile : String\n  abi : String\n  artifactRoot : String\n  \
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
structure StringEqRawPlan where\n\
structure StringConcatRawPlan where\n\
structure ConstructRawPlan where\n\
structure ExprFragmentRawPlan where\n\
structure Obligation where\n  export_ : String\n  policy : Policy\n  carrier : Nat\n  \
code : CodeTbl\n  host : (List WVal -> Option WVal) -> (List WVal -> Option WVal) -> (List WVal -> Option WVal) -> (List WVal -> Option WVal) -> (Nat -> List WVal -> Option WVal) -> HostTbl\n  \
self : Nat\n  Dom : Type\n  Cod : Type\n  domRepr : CarrierSpec carrier -> Dom -> List WVal -> Prop\n  codRepr : CarrierSpec carrier -> Cod -> WVal -> Prop\n  model : Dom -> Cod\n\
def Obligation.holds (_o : Obligation) : Prop := True\n\
structure Manifest where\n  subject : Subject\n  obligations : List Obligation\n\
  symFragmentPlans : List (String × SymRawPlan)\n  stringEqPlans : List (String × StringEqRawPlan)\n  stringConcatPlans : List (String × StringConcatRawPlan)\n  constructPlans : List (String × ConstructRawPlan)\n  exprFragmentPlans : List (String × ExprFragmentRawPlan)\n\
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

    // Emit the recursive fixture's certificate.
    let compile = aver_command()
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

    // Happy path: the freshly emitted certificate verifies end to end through
    // the production clean-cache path. Tamper cases below use the test store.
    let (ok, report) = aver_verify_clean_cache(&wasm, &cert);
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

    // The cert schema version is a breaking cert-data shape. The checker rejects
    // old manifests honestly instead of trying to reinterpret them under the
    // current schema.
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

    // The artifact-level certificate root is pinned as routing metadata. A
    // consumer should not have to guess which theorem is the self-check root.
    {
        let dir = temp_dir("neg-artifact-root-pin");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["artifact_certificate_root"] = serde_json::json!("AverCert.Final.cert");
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "wrong artifact certificate root must be rejected:\n{out}"
        );
        assert!(
            out.contains("artifact certificate root mismatch"),
            "wrong reason for artifact root drift:\n{out}"
        );
    }

    // The Lean manifest must also name the artifact-level certificate root.
    // JSON consistency alone is not enough; the checker witness pins the
    // proven manifest literal and the artifact predicate checks the same root.
    {
        let dir = temp_dir("neg-lean-artifact-root-pin");
        copy_dir(&out_dir, &dir);
        let manifest = dir.join("cert").join("Manifest.lean");
        let src = std::fs::read_to_string(&manifest).unwrap();
        let poisoned = src.replacen(
            "artifactRoot := \"AverCert.Artifact.certificate\"",
            "artifactRoot := \"AverCert.Final.cert\"",
            1,
        );
        assert_ne!(src, poisoned, "Manifest.lean artifactRoot shape changed");
        std::fs::write(&manifest, poisoned).unwrap();
        let (ok, out) = aver_verify(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "wrong Lean artifact root must be rejected:\n{out}");
        assert!(
            out.contains("manifest.subject.artifactRoot"),
            "wrong reason for Lean artifact root drift:\n{out}"
        );
    }

    // The artifact-carried data root is useful metadata, not authority. Since
    // the recursion exports carry byte-origin plan claims, an `Artifact.lean`
    // whose `data` points at empty bytes can no longer even prove its own
    // claims (`codeEntryForExport [] … = some …` has no `rfl`), so the tamper
    // dies at the cert's own build — before the checker's `AverCert.Artifact.data`
    // reconstruction pin, which remains the authority for claim-free certs.
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
            out.contains("did not build")
                || out.contains("AverCert.Artifact.data")
                || out.contains("does not bind"),
            "wrong reason for artifact data tamper:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "tampered artifact data credited:\n{out}"
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
    //     disassembles. The recursion exports carry byte-origin plan claims that
    //     bind the checker-staged `ArtifactBytes` (the actual, swapped bytes), so
    //     the cert's own build now fails before the checker witness even runs —
    //     an even earlier fail-closed decline than the witness hash face.
    {
        let dir = temp_dir("neg-e");
        copy_dir(&out_dir, &dir);
        let foreign_out = temp_dir("neg-e-foreign");
        let fc = aver_command()
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
        assert!(
            out.contains("did not build") || out.contains("does not bind"),
            "wrong reason (e):\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "hash rebind credited (e):\n{out}"
        );
    }

    // (e2) A1 hash rebind against a CLAIM-FREE cert: `certempty` proves zero
    //      obligations and ships no plan claims, so its Lean data builds green
    //      over any staged bytes. Appending an inert custom section changes the
    //      artifact hash without perturbing any byte-derived fact, and the JSON
    //      pin is rebound to match — so ONLY the kernel witness's hash faces can
    //      catch the swap: the theorems (and `CertModule.wasmSha256`) talk about
    //      the ORIGINAL hash, not the checker-computed one. This keeps the
    //      witness hash face exercised now that claim-covered certs die earlier.
    {
        let empty_out = temp_dir("neg-e2-empty");
        let ec = aver_command()
            .current_dir(&repo_root)
            .arg("compile")
            .arg("tools/certkit/fixtures/certempty.av")
            .arg("--target")
            .arg("wasm-gc")
            .arg("--certify")
            .arg("-o")
            .arg(&empty_out)
            .output()
            .expect("aver compile --certify runs");
        assert!(ec.status.success(), "certempty fixture compile failed");
        let w = empty_out.join("certempty.wasm");
        let mut foreign = std::fs::read(&w).unwrap();
        // Inert trailing custom section (id 0, size 2, name "x", empty payload).
        foreign.extend_from_slice(&[0x00, 0x02, 0x01, 0x78]);
        std::fs::write(&w, &foreign).unwrap();
        let sha = aver::codegen::cert::sha256_hex(&foreign);
        let mf = empty_out.join("cert").join("cert-manifest.json");
        let json = std::fs::read_to_string(&mf).unwrap();
        let mut m: serde_json::Value = serde_json::from_str(&json).unwrap();
        m["wasm_sha256"] = serde_json::Value::String(sha);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&w, &empty_out.join("cert"));
        let _ = std::fs::remove_dir_all(&empty_out);
        assert!(!ok, "A1 hash rebind on claim-free cert must fail:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (e2):\n{out}");
        // The witness names the exact face the kernel rejected.
        assert!(
            out.contains("CertModule.wasmSha256"),
            "witness not exercised (e2):\n{out}"
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
    //     honest `sumToCode` is left dead. The artifact-carried recursion claim
    //     pins `obligation.code` to the plan-lowered body, so the decoupled
    //     obligation now fails the cert's OWN build — an even earlier fail-closed
    //     decline than the checker witness's code `rfl` (which remains exercised
    //     by cases (p)/(q)/(r), whose nlocals-only mutations build green).
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
        assert!(
            out.contains("did not build") || out.contains("does not bind"),
            "wrong reason (s):\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "code decouple credited (s):\n{out}"
        );
    }

    // (t) Self decouple (vacuity): set the obligation's `self` to a wrong index so
    //     `code self` misses the table and `wFuncN` traps — `holds` is vacuous and
    //     provable. The artifact-carried recursion claim pins
    //     `binding.funcIdx = obligation.self` to the byte-derived function
    //     binding, so the decoupled `self` now fails the cert's OWN build — an
    //     even earlier fail-closed decline than the checker witness's self `rfl`.
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
        assert!(
            out.contains("did not build") || out.contains("does not bind"),
            "wrong reason (t):\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "self decouple credited (t):\n{out}"
        );
    }

    // (u) Export-name relabel: keep the byte-bound honest body/self/carrier, but
    //     relabel the first obligation (and the JSON) to a duplicate export name
    //     (`countDown`). The artifact-carried recursion claim pins
    //     `obligation.export_` to the claimed export name, so the relabel now
    //     fails the cert's OWN build; the checker witness's re-derived export
    //     list `rfl` remains the backstop for claim-free certs → DECLINED.
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
        assert!(
            out.contains("did not build") || out.contains("does not bind"),
            "wrong reason (u):\n{out}"
        );
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
    [ .localGet 0, .structGet 2 1, .refIsNull,\n      \
    .ifElse [.localGet 0, .structGet 2 0, .i64Const 0, .i64LeS]\n              \
    [.localGet 0, .structGet 2 2, .i32Const 0, .i32LtS],\n      \
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

    let compile = aver_command()
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
    let compile_report = format!(
        "{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    assert!(
        compile.status.success(),
        "json compile --certify failed:
{compile_report}"
    );
    assert!(
        compile_report.contains("(12 certified, 76 source-level-only)"),
        "json certificate KPI denominator changed:
{compile_report}"
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
    // The empty-string literal this vector tampers is now byte-origin-pinned by
    // `jsonStr`'s `verbatim-plan-v1` claim, so the tamper is caught one stage
    // earlier — the shipped `Plans.lean`/`Artifact.lean` byte-equality pins fail
    // during the checker's `lake build` ("did not build") rather than at the
    // later kernel-witness obligation binding ("does not bind"). Either is a
    // fail-closed decline; accept both so the assertion tracks the tamper being
    // rejected, not which in-kernel gate rejects it.
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
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

    let claim_without_manifest_ob_dir = temp_dir("cert-expr-claim-without-obligation");
    copy_dir(&out_dir, &claim_without_manifest_ob_dir);
    let claim_without_manifest_ob_wasm = claim_without_manifest_ob_dir.join("cert_goals.wasm");
    let claim_without_manifest_ob_cert = claim_without_manifest_ob_dir.join("cert");
    let manifest_lean = claim_without_manifest_ob_cert.join("Manifest.lean");
    let manifest_text = std::fs::read_to_string(&manifest_lean).unwrap();
    let marker = "obligations := [";
    let start = manifest_text
        .find(marker)
        .expect("Manifest.lean should render obligations")
        + marker.len();
    let end = start
        + manifest_text[start..]
            .find("] }")
            .expect("Manifest.lean obligations list should close");
    let mut weakened_manifest = String::new();
    weakened_manifest.push_str(&manifest_text[..start]);
    weakened_manifest.push_str(&manifest_text[end..]);
    assert_ne!(
        manifest_text, weakened_manifest,
        "Manifest.lean obligations shape changed"
    );
    std::fs::write(&manifest_lean, weakened_manifest).unwrap();
    std::fs::write(
        claim_without_manifest_ob_cert.join("Final.lean"),
        concat!(
            "import Certificate\n",
            "import Manifest\n",
            "import Schema\n\n",
            "set_option maxRecDepth 1000000\n",
            "set_option linter.unusedSimpArgs false\n\n",
            "open AverCert AverCert.Schema\n\n",
            "theorem AverCert.Final.cert : AverCert.Schema.Holds manifest := by\n",
            "  refine ⟨rfl, ?_⟩\n",
            "  intro o ho\n",
            "  simp only [manifest, List.mem_nil_iff, List.not_mem_nil] at ho\n",
            "\n",
            "#print axioms AverCert.Final.cert\n",
        ),
    )
    .unwrap();
    let (ok, out) = aver_verify(
        &claim_without_manifest_ob_wasm,
        &claim_without_manifest_ob_cert,
    );
    assert!(
        !ok,
        "expr-fragment claim without manifest obligation must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("fragmentClaimObligationsInManifest")
            || out.contains("manifest.obligations).contains")
            || out.contains("AverCert.Artifact.certificate")
            || out.contains("Artifact.lean"),
        "wrong reason for missing manifest obligation:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "expr-fragment claim without manifest obligation credited:\n{out}"
    );

    let artifact_obligation_tamper_dir = temp_dir("cert-expr-artifact-obligation-tamper");
    copy_dir(&out_dir, &artifact_obligation_tamper_dir);
    let artifact_obligation_tamper_wasm = artifact_obligation_tamper_dir.join("cert_goals.wasm");
    let artifact_obligation_tamper_cert = artifact_obligation_tamper_dir.join("cert");
    let artifact_lean = artifact_obligation_tamper_cert.join("Artifact.lean");
    let artifact_text = std::fs::read_to_string(&artifact_lean).unwrap();
    let needle = "obligation := AverCert.";
    let start = artifact_text
        .find(needle)
        .expect("Artifact.lean should render at least one claim obligation")
        + needle.len();
    let ob_end = start
        + artifact_text[start..]
            .find("Ob")
            .expect("claim obligation should reference a generated obligation")
        + "Ob".len();
    let ob_ref = &artifact_text[start..ob_end];
    let base = format!("AverCert.{ob_ref}");
    let original = format!("obligation := {base}");
    let tampered = format!(
        "obligation := {{ {base} with host := fun add sub mul stringEq stringConcat fn => if fn = {base}.self + 999999 then none else {base}.host add sub mul stringEq stringConcat fn }}"
    );
    let tampered_artifact = artifact_text.replacen(&original, &tampered, 1);
    assert_ne!(
        artifact_text, tampered_artifact,
        "Artifact.lean claim obligation shape changed"
    );
    std::fs::write(&artifact_lean, tampered_artifact).unwrap();
    let (ok, out) = aver_verify(
        &artifact_obligation_tamper_wasm,
        &artifact_obligation_tamper_cert,
    );
    assert!(
        !ok,
        "artifact claim obligation not structurally bound to manifest must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("fragmentClaimObligationsInManifest")
            || out.contains("List.find?")
            || out.contains("AverCert.Artifact.data")
            || out.contains("Artifact.lean")
            || out.contains("does not bind"),
        "wrong reason for artifact claim obligation tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "artifact claim obligation tamper credited:\n{out}"
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
    let evil_bridge = concat!(
        "axiom artifactEvil : ∀ (finalCert : AverCert.Schema.Holds AverCert.manifest), ",
        "AverCert.AcceptedArtifact.accepted data\n\n",
        "def acceptedWithFinal\n",
        "    (finalCert : AverCert.Schema.Holds AverCert.manifest) :\n",
        "    AverCert.AcceptedArtifact.accepted data := artifactEvil finalCert\n\n",
        "theorem certificate : AverCert.AcceptedArtifact.accepted data := ",
        "acceptedWithFinal AverCert.Final.cert\n\n",
        "#print axioms AverCert.Artifact.certificate\n\n",
    );
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
        .expect("at least one expr-fragment manifest entry");
    assert!(
        expr_entry.get("fragment").is_none(),
        "source-projectable expr fragments should not emit duplicate target sidecars"
    );
    assert!(
        expr_entry.get("trace").is_none() && expr_entry.get("trace_sha256").is_none(),
        "expr-fragment manifests should not emit trace/replay sidecars"
    );
    let source_plan = expr_entry["source_fragment"]["plan"]
        .as_str()
        .expect("expr-fragment source plan path");
    assert!(
        source_plan.ends_with(".sym-fragment-v1.plan"),
        "expr-fragment source plan should be a SymPlan sidecar, got {source_plan}"
    );
    let float_entry = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"].as_str() == Some("floatAddGoal"))
        .expect("floatAddGoal expr-fragment entry");
    assert!(
        float_entry.get("fragment").is_none(),
        "floatAddGoal should be verified from its source SymPlan only"
    );
    let float_source_plan = float_entry["source_fragment"]["plan"]
        .as_str()
        .expect("floatAddGoal source plan path");

    let source_plan_tamper_dir = temp_dir("cert-expr-source-plan-tamper");
    copy_dir(&out_dir, &source_plan_tamper_dir);
    let source_plan_tamper_wasm = source_plan_tamper_dir.join("cert_goals.wasm");
    let source_plan_tamper_cert = source_plan_tamper_dir.join("cert");
    let source_sidecar = source_plan_tamper_cert.join(float_source_plan);
    let source_text = std::fs::read_to_string(&source_sidecar).unwrap();
    let tampered_source = source_text.replacen("op=float.add", "op=float.mul", 1);
    assert_ne!(
        source_text, tampered_source,
        "floatAddGoal source plan shape changed"
    );
    std::fs::write(&source_sidecar, &tampered_source).unwrap();
    let source_plan_tamper_mf = source_plan_tamper_cert.join("cert-manifest.json");
    let mut source_plan_tamper_manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&source_plan_tamper_mf).unwrap()).unwrap();
    let source_plan_tamper_entry = source_plan_tamper_manifest["certified"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|c| c["name"].as_str() == Some("floatAddGoal"))
        .expect("floatAddGoal sidecar");
    source_plan_tamper_entry["source_fragment"]["plan_sha256"] =
        serde_json::Value::String(aver::codegen::cert::sha256_hex(tampered_source.as_bytes()));
    std::fs::write(
        &source_plan_tamper_mf,
        serde_json::to_string_pretty(&source_plan_tamper_manifest).unwrap(),
    )
    .unwrap();

    let (ok, out) = aver_verify(&source_plan_tamper_wasm, &source_plan_tamper_cert);
    assert!(
        !ok,
        "tampered expr-fragment source SymPlan must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("source plan-first canonical lowering")
            && out.contains("does not match the actual wasm code-entry"),
        "wrong reason for expr-fragment source SymPlan tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered expr-fragment source SymPlan credited:\n{out}"
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
    let _ = std::fs::remove_dir_all(&claim_without_manifest_ob_dir);
    let _ = std::fs::remove_dir_all(&artifact_obligation_tamper_dir);
    let _ = std::fs::remove_dir_all(&artifact_axiom_tamper_dir);
    let _ = std::fs::remove_dir_all(&lean_plan_tamper_dir);
    let _ = std::fs::remove_dir_all(&lean_bytes_tamper_dir);
    let _ = std::fs::remove_dir_all(&lean_slice_tamper_dir);
}

#[test]
fn cert_verify_declines_expr_fragment_operand_swap_sidecar() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping expr-fragment operand-swap sidecar test: `lake` not available");
        return;
    }

    let (out_dir, wasm, cert) = compile_cert_goals("cert-expr-operand-swap");
    let plan_path = source_fragment_plan_path(&cert, "floatLeGoal");
    let sidecar = cert.join(plan_path);
    let source_text = std::fs::read_to_string(&sidecar).unwrap();
    let tampered = source_text.replacen(
        "prim op=float.le args=v0,v1",
        "prim op=float.le args=v1,v0",
        1,
    );
    assert_ne!(
        source_text, tampered,
        "floatLeGoal source plan operand shape changed"
    );
    std::fs::write(&sidecar, &tampered).unwrap();
    rebind_source_fragment_plan_sha(&cert, "floatLeGoal", &tampered);

    let (ok, out) = aver_verify(&wasm, &cert);
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        !ok,
        "operand-swapped expr-fragment source plan must be DECLINED:
{out}"
    );
    // The operand swap is caught by the verifier's canonical plan lowering
    // (stack-order check on the swapped prim arguments) before the byte
    // comparison stage — an earlier, equally fail-closed gate of the same
    // plan-first path.
    assert!(
        out.contains("source plan sidecar does not check against wasm"),
        "wrong reason for floatLeGoal operand swap:
{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "operand-swapped expr-fragment source plan credited:
{out}"
    );
}

/// Byte-derived host-role indices for the goals module, read from the
/// checker-rendered `addTwoPlan` in `Plans.lean` (`.hostCall .box N`,
/// `.hostCall .add M`). Used to compose host-call tamper plans without
/// hardcoding module layout.
fn add_two_host_indices(cert_dir: &Path) -> (u32, u32) {
    let plans = std::fs::read_to_string(cert_dir.join("Plans.lean")).expect("Plans.lean exists");
    let extract = |tag: &str| -> u32 {
        let at = plans
            .find(tag)
            .unwrap_or_else(|| panic!("Plans.lean should contain `{tag}`"));
        plans[at + tag.len()..]
            .split_whitespace()
            .next()
            .and_then(|tok| tok.parse::<u32>().ok())
            .unwrap_or_else(|| panic!("`{tag}` should be followed by a function index"))
    };
    (extract(".hostCall .box "), extract(".hostCall .add "))
}

/// Rebind `addTwo`'s manifest entry from the emitted `source_fragment` SymPlan
/// to an attacker-supplied representation `fragment` sidecar with the given
/// text (sha rebound so only the plan-vs-bytes gates decide).
fn rebind_add_two_to_repr_fragment(cert_dir: &Path, plan_text: &str) {
    let plan_path = "fragments/61646454776f.expr-fragment-v1.plan";
    std::fs::write(cert_dir.join(plan_path), plan_text).unwrap();
    let mf = cert_dir.join("cert-manifest.json");
    let mut manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    let entry = manifest["certified"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|c| c["name"].as_str() == Some("addTwo"))
        .expect("addTwo manifest entry should exist");
    entry.as_object_mut().unwrap().remove("source_fragment");
    entry["fragment"] = serde_json::json!({
        "profile": "expr-fragment-v1",
        "plan": plan_path,
        "plan_sha256": aver::codegen::cert::sha256_hex(plan_text.as_bytes()),
    });
    std::fs::write(&mf, serde_json::to_string_pretty(&manifest).unwrap()).unwrap();
}

fn add_two_host_call_plan_text(box_idx: u32, add_call_idx: u32) -> String {
    format!(
        "aver.expr-fragment.plan.v1\nprofile expr-fragment-v1\nparams int-carrier\n\
         result int-carrier\nbody\nblock result=v3\n  v0 ty=int-carrier local index=0\n  \
         v1 ty=i64 const.i64 value=2\n  v2 ty=int-carrier hostcall role=box func={box_idx} args=v1\n  \
         v3 ty=int-carrier hostcall role=add func={add_call_idx} args=v0,v2\nend\n"
    )
}

#[test]
fn cert_verify_declines_expr_fragment_host_role_swap() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping expr-fragment host-role swap test: `lake` not available");
        return;
    }

    let (out_dir, wasm, cert) = compile_cert_goals("cert-expr-host-role-swap");
    let (box_idx, add_idx) = add_two_host_indices(&cert);
    assert_ne!(
        box_idx, add_idx,
        "goals module should have distinct host roles"
    );
    // The `add` host call is rebound to a helper that IS in the module (the
    // box constructor) but does not realise the `add` role: the byte-derived
    // role table must decline the swap fail-closed.
    let tampered = add_two_host_call_plan_text(box_idx, box_idx);
    rebind_add_two_to_repr_fragment(&cert, &tampered);

    let (ok, out) = aver_verify(&wasm, &cert);
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        !ok,
        "host-role-swapped expr-fragment plan must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("plan sidecar does not check against wasm") && out.contains("cites function"),
        "wrong reason for addTwo host-role swap:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "host-role-swapped expr-fragment plan credited:\n{out}"
    );
}

#[test]
fn cert_verify_declines_expr_fragment_host_call_outside_role_table() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping expr-fragment host-call outside-table test: `lake` not available");
        return;
    }

    let (out_dir, wasm, cert) = compile_cert_goals("cert-expr-host-outside-table");
    let (box_idx, _add_idx) = add_two_host_indices(&cert);
    // A callee index that resolves through NO role in the byte-derived table:
    // the Rust checker declines before any byte comparison or kernel work.
    let tampered = add_two_host_call_plan_text(box_idx, 9999);
    rebind_add_two_to_repr_fragment(&cert, &tampered);

    let (ok, out) = aver_verify(&wasm, &cert);
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        !ok,
        "expr-fragment plan citing a callee outside the role table must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("plan sidecar does not check against wasm")
            && out.contains("cites function 9999"),
        "wrong reason for addTwo outside-table host call:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "outside-table expr-fragment plan credited:\n{out}"
    );
}

#[test]
fn cert_verify_declines_expr_fragment_extra_instruction_sidecar() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping expr-fragment extra-instruction sidecar test: `lake` not available");
        return;
    }

    let (out_dir, wasm, cert) = compile_cert_goals("cert-expr-extra-instr");
    let plan_path = source_fragment_plan_path(&cert, "floatAddGoal");
    let sidecar = cert.join(plan_path);
    let source_text = std::fs::read_to_string(&sidecar).unwrap();
    let honest_block = concat!(
        "block result=v2
",
        "  v0 ty=float param index=0
",
        "  v1 ty=float param index=1
",
        "  v2 ty=float prim op=float.add args=v0,v1
",
        "end
",
    );
    let tampered_block = concat!(
        "block result=v4
",
        "  v0 ty=float param index=0
",
        "  v1 ty=float param index=1
",
        "  v2 ty=float prim op=float.add args=v0,v1
",
        "  v3 ty=float const.float bits=0x0000000000000000
",
        "  v4 ty=float prim op=float.add args=v2,v3
",
        "end
",
    );
    let tampered = source_text.replacen(honest_block, tampered_block, 1);
    assert_ne!(
        source_text, tampered,
        "floatAddGoal source plan block shape changed"
    );
    std::fs::write(&sidecar, &tampered).unwrap();
    rebind_source_fragment_plan_sha(&cert, "floatAddGoal", &tampered);

    let (ok, out) = aver_verify(&wasm, &cert);
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        !ok,
        "extra-instruction expr-fragment source plan must be DECLINED:
{out}"
    );
    assert_source_plan_byte_mismatch("floatAddGoal", &out);
}

#[test]
fn cert_verify_declines_expr_fragment_bad_bool01_raw_plan() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping expr-fragment bad Bool01 raw-plan test: `lake` not available");
        return;
    }

    let (out_dir, wasm, cert) = compile_cert_goals("cert-expr-bad-bool01");
    let plans = cert.join("Plans.lean");
    let plans_text = std::fs::read_to_string(&plans).unwrap();
    let def_start = plans_text
        .find("def floatLeGoalPlan : ExprFragmentRawPlan :=")
        .expect("Plans.lean should define floatLeGoalPlan");
    let def_end = def_start
        + plans_text[def_start..]
            .find(
                "

/-- Source-level `SymPlan` projection for `floatLeGoal`",
            )
            .expect("floatLeGoalPlan should be followed by its SymPlan");
    let target = "{ id := 0, ty := .boolI32, kind := .constBool true }";
    let replacement = "{ id := 0, ty := .boolI32, kind := .constI32 (2 : Int) }";
    let plan_def = &plans_text[def_start..def_end];
    assert!(
        plan_def.contains(target),
        "floatLeGoalPlan Bool01 constant shape changed"
    );
    let tampered_plan_def = plan_def.replacen(target, replacement, 1);
    let mut tampered = String::new();
    tampered.push_str(&plans_text[..def_start]);
    tampered.push_str(&tampered_plan_def);
    tampered.push_str(&plans_text[def_end..]);
    std::fs::write(&plans, tampered).unwrap();

    let (ok, out) = aver_verify(&wasm, &cert);
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        !ok,
        "bad Bool01 raw plan must be DECLINED:
{out}"
    );
    // PlanCheck rejects the ill-typed Bool01 node (`constI32` is inferred
    // `rawI32`; `sameTy` fails against the declared `boolI32`), so the
    // Plans.lean examples fail to elaborate. The verify report keeps only the
    // tail of the lake output (`tail(.., 20)` in cert_cmd), so the earliest
    // error (the `checkExprFragmentRawPlan` example) can be cut when the later
    // byte-pin errors fill the tail — accept either attribution of the same
    // fail-closed Plans.lean build failure.
    assert!(
        out.contains("did not build")
            && (out.contains("checkExprFragmentRawPlan") || out.contains("Plans")),
        "bad Bool01 raw plan should fail the Plans.lean checks:
{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "bad Bool01 raw plan credited:
{out}"
    );
}

#[test]
fn cert_verify_declines_tampered_string_eq_helper_shape() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping String.eq helper tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-stringeq");

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
        report.contains("quoteOrSelf  class: verbatim string equality match"),
        "quoteOrSelf should report the byte-derived String.eq class:\n{report}"
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
        quote_class, "verbatim-string-eq",
        "quoteOrSelf should render the String.eq class, got {quote_class}"
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
        out.contains("do not re-derive a String.eq certificate"),
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
    let shout_fragment = &shout_entry["fragment"];
    assert_eq!(
        shout_fragment["profile"].as_str(),
        Some("string-concat-v1"),
        "shout should carry a byte-bound String.concat sidecar"
    );
    let shout_plan = shout_fragment["plan"]
        .as_str()
        .expect("shout string-concat plan path")
        .to_string();
    let shout_source_fragment = &shout_entry["source_fragment"];
    assert_eq!(
        shout_source_fragment["profile"].as_str(),
        Some("sym-fragment-v1"),
        "shout should carry a source-level SymPlan sidecar"
    );
    let shout_source_plan = shout_source_fragment["plan"]
        .as_str()
        .expect("shout source SymPlan path")
        .to_string();

    {
        let dir = temp_dir("cert-stringconcat-source-sidecar-tamper");
        copy_dir(&out_dir, &dir);
        let sidecar = dir.join("cert").join(&shout_source_plan);
        let plan_text = std::fs::read_to_string(&sidecar).unwrap();
        let tampered_plan = plan_text.replacen("const.string hex=21", "const.string hex=3f", 1);
        assert_ne!(
            plan_text, tampered_plan,
            "String.concat SymPlan sidecar shape changed"
        );
        std::fs::write(&sidecar, &tampered_plan).unwrap();

        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        let entry = m["certified"]
            .as_array_mut()
            .unwrap()
            .iter_mut()
            .find(|c| c["name"].as_str() == Some("shout"))
            .expect("shout manifest entry");
        entry["source_fragment"]["plan_sha256"] =
            serde_json::Value::String(aver::codegen::cert::sha256_hex(tampered_plan.as_bytes()));
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

        let (ok, out) = aver_verify(&dir.join("stringconcat.wasm"), &dir.join("cert"));
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            !ok,
            "tampered String.concat SymPlan sidecar must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("source SymPlan sidecar")
                && out.contains("canonical byte-derived source plan"),
            "wrong reason for String.concat SymPlan sidecar tamper:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "tampered String.concat SymPlan sidecar credited:\n{out}"
        );
    }

    {
        let dir = temp_dir("cert-stringconcat-sidecar-tamper");
        copy_dir(&out_dir, &dir);
        let sidecar = dir.join("cert").join(&shout_plan);
        let plan_text = std::fs::read_to_string(&sidecar).unwrap();
        let tampered_plan = plan_text.replacen("suffix data=0 hex=21", "suffix data=0 hex=3f", 1);
        assert_ne!(
            plan_text, tampered_plan,
            "String.concat sidecar shape changed"
        );
        std::fs::write(&sidecar, &tampered_plan).unwrap();

        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        let entry = m["certified"]
            .as_array_mut()
            .unwrap()
            .iter_mut()
            .find(|c| c["name"].as_str() == Some("shout"))
            .expect("shout manifest entry");
        entry["fragment"]["plan_sha256"] =
            serde_json::Value::String(aver::codegen::cert::sha256_hex(tampered_plan.as_bytes()));
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

        let (ok, out) = aver_verify(&dir.join("stringconcat.wasm"), &dir.join("cert"));
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            !ok,
            "tampered String.concat sidecar must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("string-concat sidecar")
                && out.contains("canonical byte-derived concat plan"),
            "wrong reason for String.concat sidecar tamper:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "tampered String.concat sidecar credited:\n{out}"
        );
    }

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
        out.contains("does not bind")
            || out.contains("do not re-derive a String.concat certificate"),
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

    let compile = aver_command()
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

    let compile = aver_command()
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

    let compile = aver_command()
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

/// S2a field-projection tamper: flipping the projected field index 0 -> 1 in
/// `userName`'s source SymPlan sidecar (with the sidecar sha rebound in the
/// JSON manifest, so only the plan-vs-bytes gate is in play) lowers to
/// `struct.get ty 1` — canonical code-entry bytes that do not match the
/// module — and must be DECLINED, never silently re-admitted through the
/// legacy field-projection classifier.
#[test]
fn cert_verify_declines_flipped_field_projection_sidecar() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping field-projection sidecar tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-proj-sidecar");

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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify goals failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(out_dir.join("cert").join("cert-manifest.json")).unwrap(),
    )
    .unwrap();
    let user_entry = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"].as_str() == Some("userName"))
        .expect("userName manifest entry");
    assert_eq!(
        user_entry["class"].as_str(),
        Some("expr-fragment-v1"),
        "userName should be plan-first before tampering"
    );
    let source_plan = user_entry["source_fragment"]["plan"]
        .as_str()
        .expect("userName source plan path")
        .to_string();

    let tamper_dir = temp_dir("cert-proj-sidecar-flip");
    copy_dir(&out_dir, &tamper_dir);
    let tamper_wasm = tamper_dir.join("cert_goals.wasm");
    let tamper_cert = tamper_dir.join("cert");
    let sidecar_path = tamper_cert.join(&source_plan);
    let sidecar_text = std::fs::read_to_string(&sidecar_path).unwrap();
    let tampered_text = sidecar_text.replacen(
        "project.field type=User field=0",
        "project.field type=User field=1",
        1,
    );
    assert_ne!(
        sidecar_text, tampered_text,
        "userName sidecar shape changed"
    );
    std::fs::write(&sidecar_path, &tampered_text).unwrap();
    let mf = tamper_cert.join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    let entry = m["certified"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|c| c["name"].as_str() == Some("userName"))
        .expect("userName sidecar entry");
    entry["source_fragment"]["plan_sha256"] =
        serde_json::Value::String(aver::codegen::cert::sha256_hex(tampered_text.as_bytes()));
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

    let (ok, out) = aver_verify(&tamper_wasm, &tamper_cert);
    assert!(
        !ok,
        "flipped field-projection sidecar must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("source plan-first canonical lowering")
            && out.contains("does not match the actual wasm code-entry"),
        "wrong reason for flipped projection field:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "flipped field-projection sidecar credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
    let _ = std::fs::remove_dir_all(&tamper_dir);
}

/// Cross-vendor review follow-up (S2a): source-level TYPE NAMES in projection
/// claims carry the MODEL trust story — the kernel-checked content is the
/// byte-derived struct identity (type index + field index), while `named:`
/// types are producer-asserted annotations (docs/certification.md, "Read
/// surface"). What the checker DOES enforce is consistency across the
/// artifact: a PARTIAL relabel (the `userName` sidecar renamed
/// `named:User`/`type=User` -> `Other` with its sha rebound, but the
/// Lean-side `Plans.lean`/`Artifact.lean` claims untouched) diverges from the
/// checker-rendered plan terms, fails the kernel `rfl` pins, and must be
/// DECLINED. A FULLY coordinated relabel of every surface at once is a
/// read-surface change equivalent to shipping a different model — out of
/// scope for a decline (and deliberately NOT asserted as accepted here);
/// binding names to true source provenance is the planned provenance flip.
#[test]
fn cert_verify_declines_relabeled_projection_source_types() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping projection relabel tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-proj-relabel");

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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify goals failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(out_dir.join("cert").join("cert-manifest.json")).unwrap(),
    )
    .unwrap();
    let user_entry = manifest["certified"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"].as_str() == Some("userName"))
        .expect("userName manifest entry");
    let source_plan = user_entry["source_fragment"]["plan"]
        .as_str()
        .expect("userName source plan path")
        .to_string();

    let tamper_dir = temp_dir("cert-proj-relabel-partial");
    copy_dir(&out_dir, &tamper_dir);
    let tamper_wasm = tamper_dir.join("cert_goals.wasm");
    let tamper_cert = tamper_dir.join("cert");
    let sidecar_path = tamper_cert.join(&source_plan);
    let sidecar_text = std::fs::read_to_string(&sidecar_path).unwrap();
    // A sidecar-internally CONSISTENT rename (params + node type + projection
    // owner), so the Rust-side intra-plan consistency checks pass and the
    // decline is forced by the cross-file kernel pins alone.
    let tampered_text = sidecar_text
        .replace("named:User", "named:Other")
        .replace("type=User", "type=Other");
    assert_ne!(
        sidecar_text, tampered_text,
        "userName sidecar shape changed"
    );
    std::fs::write(&sidecar_path, &tampered_text).unwrap();
    let mf = tamper_cert.join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    let entry = m["certified"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|c| c["name"].as_str() == Some("userName"))
        .expect("userName sidecar entry");
    entry["source_fragment"]["plan_sha256"] =
        serde_json::Value::String(aver::codegen::cert::sha256_hex(tampered_text.as_bytes()));
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

    let (ok, out) = aver_verify(&tamper_wasm, &tamper_cert);
    assert!(
        !ok,
        "partially relabeled projection source types must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("does not bind"),
        "wrong reason for partial source-type relabel:\n{out}"
    );
    assert!(
        !out.contains("did not build"),
        "partial relabel must be caught by the kernel witness, not a broken build:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "partially relabeled projection source types credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
    let _ = std::fs::remove_dir_all(&tamper_dir);
}

/// A tampered byte-first `recursion-plan-v1` plan is declined. Each vector
/// mutates the fuel-recursion plan for `sumFrom` in the shipped `Plans.lean`
/// while leaving the wasm untouched, so the plan no longer canonically lowers to
/// `sumFrom`'s real code-entry bytes. The checker rebuilds the shipped plan (its
/// `rfl` chain is pinned to the honest bytes) and its kernel witness proves
/// `accepted` over `manifest.recursionPlans`, so either gate rejects the plan.
#[test]
fn cert_verify_declines_tampered_recursion_plan() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping recursion-plan tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-recursion-plan");
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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "recgen compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("recgen.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(ok, "honest recursion certificate should verify:\n{report}");

    let honest = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    // (a) descent role/target swap: `sub(n, box 1)` becomes `add(n, box 1)`, so
    //     the descent computes `n + 1` instead of `n - 1` (byte `10 0c -> 10 0b`).
    // (b) self-call retargeted at another user function (`backward`, `10 01 -> 10 03`).
    // (c) base literal changed (`i64.const 7 -> 5`).
    // (d) BYTE-IDENTICAL relabel: the descent's `sub` host call is relabelled as
    //     a non-tail self-call at the sub helper's index. Lowering emits the
    //     same `10 0c` either way, so every byte-equality face still holds; only
    //     the in-kernel context-sensitive grammar (`checkRecursionPlanShape`,
    //     which pins self-call targets to the export's own byte-derived index
    //     and host calls to the role table) rejects it.
    let tampers: [(&str, &str, &str); 4] = [
        (
            "descent role swap",
            ".hostCall .sub 12 [1, 3]",
            ".hostCall .add 11 [1, 3]",
        ),
        (
            "self-call retarget",
            ".selfCall false 1 [4]",
            ".selfCall false 3 [4]",
        ),
        (
            "base literal change",
            ".constI64 (7 : Int)",
            ".constI64 (5 : Int)",
        ),
        (
            "byte-identical self-call mislabel",
            ".hostCall .sub 12 [1, 3]",
            ".selfCall false 12 [1, 3]",
        ),
    ];
    for (label, from, to) in tampers {
        assert!(
            honest.contains(from),
            "recgen Plans.lean recursion-plan shape changed ({label}); update the test"
        );
        let dir = temp_dir("cert-recursion-plan-tamper");
        copy_dir(&out_dir, &dir);
        let tampered_plans = dir.join("cert").join("Plans.lean");
        let src = std::fs::read_to_string(&tampered_plans).unwrap();
        std::fs::write(&tampered_plans, src.replacen(from, to, 1)).unwrap();
        let (ok, report) = aver_verify(&dir.join("recgen.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "{label}: tampered recursion plan must be declined:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A tampered byte-first `mutual-plan-v1` plan is declined. Each vector mutates
/// the mutual-member plan for `isEven` in the shipped `Plans.lean` while leaving
/// the wasm untouched, so the member plan no longer canonically lowers to
/// `isEven`'s real code-entry bytes in the shared SCC code table. The checker
/// rebuilds the shipped plan (its `rfl` chain is pinned to the honest bytes) and
/// its kernel witness proves `accepted` over `manifest.mutualPlans`, so either
/// gate rejects the plan. This is the S4 generalisation of the recursion tamper
/// test: `isEven`'s step arm tail-calls a SIBLING SCC member (`isOdd`, index 2),
/// not itself.
#[test]
fn cert_verify_declines_tampered_mutual_plan() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping mutual-plan tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-mutual-plan");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/mutual.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "mutual compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("mutual.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(ok, "honest mutual certificate should verify:\n{report}");

    let honest = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    // (a) member-call retargeted OUTSIDE the byte-derived SCC set ({1, 2}): the
    //     tail cross-call to `isOdd` (index 2) becomes a call to index 5, which
    //     is neither a member nor `isEven` itself. The context-sensitive grammar
    //     (`checkMutualPlanShape`, `5 ∉ [1, 2]`) AND the byte gate both reject.
    // (b) tail/non-tail flag flipped (`return_call 12 -> call 10`): the grammar
    //     requires a TAIL member-call, and the bytes change, so both gates reject.
    // (c) base literal changed (`i64.const 1 -> 5`): the base arm boxes the wrong
    //     literal, so the member's bytes diverge (byte gate rejects).
    // (d) member-call MISLABELLED as a self-call: the cross-call to `isOdd`
    //     (index 2) is retargeted at `isEven`'s OWN index (1). Index 1 IS in the
    //     SCC set, so the grammar check alone would pass — but `isEven`'s real
    //     bytes tail-call index 2, so the byte-equality gate rejects it. This is
    //     the defence-in-depth case: the shape check accepts, the byte gate does
    //     not.
    // (The byte-IDENTICAL `.hostCall .sub` -> `.selfCall false` relabel — which
    //  ONLY `checkMutualPlanShape` distinguishes — is NOT tested here: routed
    //  through `aver cert verify` it is also caught by the manifest-plan
    //  equality pin and the standalone shape example, so it would not isolate the
    //  shape guard. It is a DIRECT Lean assertion in
    //  `mutual_scc_kernel_guards_are_isolating` instead.)
    let tampers: [(&str, &str, &str); 4] = [
        (
            "member-call outside SCC",
            ".selfCall true 2 [3]",
            ".selfCall true 5 [3]",
        ),
        (
            "tail flag flip",
            ".selfCall true 2 [3]",
            ".selfCall false 2 [3]",
        ),
        (
            "base literal change",
            ".constI64 (1 : Int) }, { id := 1, ty := .intCarrier, kind := .hostCall .box 7 [0] }",
            ".constI64 (5 : Int) }, { id := 1, ty := .intCarrier, kind := .hostCall .box 7 [0] }",
        ),
        (
            "member-call mislabelled as self-call",
            ".selfCall true 2 [3]",
            ".selfCall true 1 [3]",
        ),
    ];
    for (label, from, to) in tampers {
        assert!(
            honest.contains(from),
            "mutual Plans.lean mutual-plan shape changed ({label}); update the test"
        );
        let dir = temp_dir("cert-mutual-plan-tamper");
        copy_dir(&out_dir, &dir);
        let tampered_plans = dir.join("cert").join("Plans.lean");
        let src = std::fs::read_to_string(&tampered_plans).unwrap();
        std::fs::write(&tampered_plans, src.replacen(from, to, 1)).unwrap();
        let (ok, report) = aver_verify(&dir.join("mutual.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "{label}: tampered mutual plan must be declined:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A mutual-recursion artifact whose per-member byte-origin claims are each
/// individually honest but whose declared `memberSet` is wrong is declined by
/// the REAL acceptance-proof closure conjunct (`mutualClaimsFormClosedSccs`,
/// wired into `acceptedMutualRecursionFragments`) — not by byte equality and not
/// by the per-claim shape check. Each vector mutates ONE claim's `memberSet` in
/// the cert's own `Artifact.lean` while keeping the member's own call target in
/// the set, so `checkMutualPlanShape` still ACCEPTS and every code-entry byte is
/// untouched; only the closure's `memberSet == byte-derived cycle` check rejects.
/// Building the cert's own `acceptedWithFinal` proof with `lake` (no checker data
/// pin) isolates that conjunct. Graph-structural rejections that cannot be
/// expressed as a `memberSet` edit (dangling / non-closing / rho-tail / one-node
/// / disjoint-SCCs) are proven directly in
/// `mutual_scc_kernel_guards_are_isolating`.
#[test]
fn cert_verify_declines_broken_mutual_scc_membership() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping mutual-SCC closure test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // (fixture, [(label, from, to)]). Each `from`/`to` mutates ONE claim's
    // `memberSet` in `Artifact.lean` — checked by the acceptance-proof closure.
    // The per-claim shape check still passes (the member's own call target stays
    // in the set), so the closure conjunct is the sole rejector. No code-entry
    // byte changes.
    type Tamper = (&'static str, &'static str, &'static str);
    let cases: [(&str, Vec<Tamper>); 2] = [
        (
            "tools/certkit/fixtures/mutual.av",
            vec![
                // memberSet gains a non-member (extra); closure length check.
                (
                    "extra member",
                    "memberSet := [1, 2]",
                    "memberSet := [1, 2, 3]",
                ),
                // memberSet drops a member but keeps the call target (omission);
                // shape check passes, closure cycle-set check fails.
                ("omitted member", "memberSet := [1, 2]", "memberSet := [2]"),
                // memberSet repeats a member (duplicate); closure length check.
                (
                    "duplicate member",
                    "memberSet := [1, 2]",
                    "memberSet := [1, 2, 2]",
                ),
                // one member declares a set inconsistent with the byte-derived
                // cycle (keeps its own target so the shape check still passes).
                (
                    "inconsistent set",
                    "memberSet := [1, 2]",
                    "memberSet := [2, 4]",
                ),
            ],
        ),
        (
            "tools/certkit/fixtures/mutual3.av",
            vec![
                (
                    "extra member",
                    "memberSet := [1, 2, 3]",
                    "memberSet := [1, 2, 3, 4]",
                ),
                (
                    "inconsistent set",
                    "memberSet := [1, 2, 3]",
                    "memberSet := [2, 3, 5]",
                ),
            ],
        ),
    ];

    let lake_ok = |cert: &Path| -> bool {
        Command::new("lake")
            .arg("build")
            .current_dir(cert)
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
    };

    for (fixture, vectors) in cases {
        let out_dir = temp_dir("cert-mutual-scc");
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
            .expect("aver compile --certify runs");
        assert!(
            compile.status.success(),
            "{fixture} compile --certify failed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        );
        let cert = out_dir.join("cert");
        // Honest cert must build (and populates the `.lake` cache so each tamper
        // below only rebuilds the leaf `Artifact` module).
        assert!(lake_ok(&cert), "honest {fixture} cert must lake-build");

        let artifact = cert.join("Artifact.lean");
        let honest = std::fs::read_to_string(&artifact).unwrap();
        for (label, from, to) in vectors {
            assert!(
                honest.contains(from),
                "{fixture} Artifact.lean SCC shape changed ({label}); update the test"
            );
            std::fs::write(&artifact, honest.replacen(from, to, 1)).unwrap();
            let ok = lake_ok(&cert);
            std::fs::write(&artifact, &honest).unwrap(); // restore before asserting
            assert!(
                !ok,
                "{fixture} {label}: broken mutual-SCC membership must be declined in-kernel"
            );
        }
        let _ = std::fs::remove_dir_all(&out_dir);
    }
}

/// GUARD-ISOLATING direct Lean assertions for the two mutual-recursion kernel
/// guards, elaborated with `lake env lean` against the audited cert modules — no
/// `aver cert verify`, no sibling defence in the path. Each assertion is
/// constructed so it holds ONLY because its target guard fires (verified by
/// weakening each guard in a throwaway copy: weakening `checkMutualPlanShape` to
/// the generic checker breaks solely the relabel-rejection line; weakening
/// `mutualMembersFormClosedSccs` to `true` breaks solely the closure
/// reject/wrapper lines — nothing else moves).
///
/// FIX A (`checkMutualPlanShape`): the descent `.hostCall .sub 9` relabelled
/// byte-identically as a non-tail `.selfCall false 9` — the ONLY thing
/// `checkMutualPlanShape` catches that the generic checker + byte lowering do
/// not. Asserts (i) `checkMutualRawPlan` ACCEPTS it, (ii) its `WInstr` body and
/// code-entry bytes are IDENTICAL to the honest plan, (iii) `checkMutualPlanShape`
/// REJECTS it. Sibling rejectors avoided: the byte-equality face (ii proves it is
/// blind here) and the generic typed-block checker (i proves it accepts).
///
/// FIX B (`mutualMembersFormClosedSccs` / `mutualClaimsFormClosedSccs`): a
/// truth-table over synthetic `(self, target, memberSet)` groups where every
/// rejected group keeps each member's target IN its `memberSet` (so the per-claim
/// shape check would ACCEPT — the closure is the sole rejector): dangling /
/// dropped-member, rho-tail, duplicate self, one-node cycle, disjoint SCCs
/// claimed as one group. Plus a wrapper case against the REAL acceptance conjunct
/// `mutualClaimsFormClosedSccs` (fed a minimal manifest + claim) proving it
/// extracts the byte-pinned edge from `obligation.self` + `mutualPlanTarget` and
/// refutes a dangling group while the per-claim shape check passes.
#[test]
fn mutual_scc_kernel_guards_are_isolating() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping mutual-guard isolation test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // The honest `isEven` mutual-plan literal (carrier 2, box=call 7, sub=call 9,
    // tail cross-call to isOdd=index 2), kept byte-for-byte in sync with the
    // emitter's `mutual_plan_lean_value`. The `checkMutualPlanShape honestPlan =
    // true` and `checkMutualRawPlan` assertions below fail loudly if it drifts.
    const HONEST: &str = r#"{ profile := "mutual-plan-v1", params := [.intCarrier], result := .intCarrier, body := ({ nodes := [{ id := 0, ty := .intCarrier, kind := .local 0 }, { id := 1, ty := .ref, kind := .structGet 1 0 }, { id := 2, ty := .boolI32, kind := .refIsNull 1 }, { id := 3, ty := .boolI32, kind := .ifElse 2 ({ nodes := [{ id := 0, ty := .intCarrier, kind := .local 0 }, { id := 1, ty := .i64, kind := .structGet 0 0 }, { id := 2, ty := .i64, kind := .constI64 (0 : Int) }, { id := 3, ty := .boolI32, kind := .prim .i64LeS [1, 2] }], result := 3 } : FragBlock) ({ nodes := [{ id := 0, ty := .intCarrier, kind := .local 0 }, { id := 1, ty := .rawI32, kind := .structGet 2 0 }, { id := 2, ty := .boolI32, kind := .constBool false }, { id := 3, ty := .boolI32, kind := .prim .i32LtS [1, 2] }], result := 3 } : FragBlock) }, { id := 4, ty := .intCarrier, kind := .ifElse 3 ({ nodes := [{ id := 0, ty := .i64, kind := .constI64 (1 : Int) }, { id := 1, ty := .intCarrier, kind := .hostCall .box 7 [0] }], result := 1 } : FragBlock) ({ nodes := [{ id := 0, ty := .intCarrier, kind := .local 0 }, { id := 1, ty := .i64, kind := .constI64 (1 : Int) }, { id := 2, ty := .intCarrier, kind := .hostCall .box 7 [1] }, { id := 3, ty := .intCarrier, kind := .hostCall .sub 9 [0, 2] }, { id := 4, ty := .intCarrier, kind := .selfCall true 2 [3] }], result := 4 } : FragBlock) }], result := 4 } : FragBlock) }"#;
    // Byte-identical relabel: the descent's `sub` host call becomes a non-tail
    // self-call at the SAME index; both lower to `10 09` and the same `WInstr`.
    let relabeled = HONEST.replace(".hostCall .sub 9 [0, 2]", ".selfCall false 9 [0, 2]");
    assert_ne!(
        relabeled, HONEST,
        "relabel target string drifted; update the test"
    );

    let mut lean = String::new();
    lean.push_str("import Schema\nimport PlanCheck\nimport PlanLower\nimport PlanBytes\nimport AcceptedArtifact\n\n");
    lean.push_str("open AverCert.Schema\nopen AverCert.AcceptedArtifact\n\n");
    lean.push_str("def honestPlan : MutualRawPlan := ");
    lean.push_str(HONEST);
    lean.push_str("\ndef relabeledPlan : MutualRawPlan := ");
    lean.push_str(&relabeled);
    lean.push_str("\n\n");
    // FIX A.
    lean.push_str("example : AverCert.PlanCheck.checkMutualRawPlan relabeledPlan = true := rfl\n");
    lean.push_str("example : AverCert.PlanLower.lowerMutualBody 2 relabeledPlan = AverCert.PlanLower.lowerMutualBody 2 honestPlan := rfl\n");
    lean.push_str("example : AverCert.PlanBytes.lowerMutualCodeEntry 2 relabeledPlan = AverCert.PlanBytes.lowerMutualCodeEntry 2 honestPlan := rfl\n");
    lean.push_str("example : AverCert.PlanCheck.checkMutualPlanShape [1, 2] [(.box, 7), (.sub, 9)] honestPlan = true := rfl\n");
    lean.push_str("example : AverCert.PlanCheck.checkMutualPlanShape [1, 2] [(.box, 7), (.sub, 9)] relabeledPlan = false := rfl\n\n");
    // FIX B: closure truth-table (each reject keeps target in memberSet).
    lean.push_str(
        "example : mutualMembersFormClosedSccs [(1, 2, [1, 2]), (2, 1, [1, 2])] = true := rfl\n",
    );
    lean.push_str("example : mutualMembersFormClosedSccs [(1, 2, [1, 2, 3]), (2, 3, [1, 2, 3]), (3, 1, [1, 2, 3])] = true := rfl\n");
    lean.push_str("example : mutualMembersFormClosedSccs [(1, 2, [1, 2]), (2, 1, [1, 2]), (3, 4, [3, 4]), (4, 3, [3, 4])] = true := rfl\n");
    lean.push_str("example : mutualMembersFormClosedSccs [(1, 2, [1, 2])] = false := rfl\n");
    lean.push_str("example : mutualMembersFormClosedSccs [(1, 2, [1, 2, 3]), (2, 3, [1, 2, 3]), (3, 2, [1, 2, 3])] = false := rfl\n");
    lean.push_str(
        "example : mutualMembersFormClosedSccs [(1, 2, [1, 2]), (1, 2, [1, 2])] = false := rfl\n",
    );
    lean.push_str("example : mutualMembersFormClosedSccs [(1, 1, [1])] = false := rfl\n");
    lean.push_str("example : mutualMembersFormClosedSccs [(1, 2, [1, 2, 3, 4]), (2, 1, [1, 2, 3, 4]), (3, 4, [1, 2, 3, 4]), (4, 3, [1, 2, 3, 4])] = false := rfl\n\n");
    // FIX B: the REAL acceptance conjunct rejects a dangling group; shape passes.
    lean.push_str("def dummyOb (nm : String) (s : Nat) : Obligation :=\n  { export_ := nm, policy := .simulatesModel, carrier := 2, code := fun _ => none,\n    host := fun _ _ _ _ _ => fun _ => none, self := s, Dom := Unit, Cod := Unit,\n    domRepr := fun _ _ _ => True, codRepr := fun _ _ _ => True, model := fun _ => () }\n\n");
    lean.push_str("def manifestS : Manifest :=\n  { subject := { artifactHash := \"\", profile := \"\", abi := \"\", artifactRoot := \"\", exports := [], contracts := [] },\n    symFragmentPlans := [], stringEqPlans := [], stringConcatPlans := [], constructPlans := [],\n    exprFragmentPlans := [], recursionPlans := [], mutualPlans := [(\"a\", honestPlan)], verbatimPlans := [], intDispatchPlans := [], obligations := [] }\n\n");
    lean.push_str("def claimsS : List MutualRecursionClaim :=\n  [ { exportNameBytes := [], exportName := \"a\", carrier := 2, memberSet := [1, 2],\n      hostTable := [(.box, 7), (.sub, 9)], obligation := dummyOb \"a\" 1 } ]\n\n");
    lean.push_str("example : AverCert.PlanCheck.checkMutualPlanShape [1, 2] [(.box, 7), (.sub, 9)] honestPlan = true := rfl\n");
    lean.push_str("example : mutualClaimEdges manifestS claimsS = some [(1, 2, [1, 2])] := rfl\n");
    lean.push_str(
        "example : ¬ mutualClaimsFormClosedSccs manifestS claimsS := fun h => nomatch h\n",
    );

    let out_dir = temp_dir("cert-mutual-guard-iso");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/mutual.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "mutual compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    // Build the audited modules so `lake env lean` can resolve the imports.
    let honest_build = Command::new("lake")
        .arg("build")
        .current_dir(&cert)
        .output()
        .expect("lake build runs");
    assert!(honest_build.status.success(), "honest cert must lake-build");

    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let elab = Command::new("lake")
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .current_dir(&cert)
        .output()
        .expect("lake env lean runs");
    assert!(
        elab.status.success(),
        "guard-isolation assertions must all hold:\n{}\n{}",
        String::from_utf8_lossy(&elab.stdout),
        String::from_utf8_lossy(&elab.stderr)
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A tampered byte-first `verbatim-plan-v1` plan is declined, and the four spike
/// tamper vectors are shown to be guard-isolating. Each vector mutates the
/// verbatim `ref.test`-dispatch plan for `wrapItems`/`tagName` in the shipped
/// `Plans.lean` while leaving the wasm untouched, so the plan no longer
/// canonically lowers to the export's real code-entry bytes. For verbatim
/// `Cod := WVal` matches there are NO host/self calls to bind, so the
/// byte-equality gate is the WHOLE soundness binding: both the shipped
/// `Plans.lean` `lowerVerbatimCodeEntry`/`codeEntryForExport` `rfl` pins and the
/// checker's `manifest.verbatimPlans` `rfl` pin reject the tampered plan. The
/// `GuardIso.lean` block below isolates each vector by proving in-kernel
/// (`by decide`) that its lowered code entry diverges from the honest one — the
/// spike's four vectors lifted onto the real cert.
#[test]
fn cert_verify_declines_tampered_verbatim_plan() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping verbatim-plan tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-verbatim-plan");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/verbatimgen.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "verbatimgen compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("verbatimgen.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(ok, "honest verbatim certificate should verify:\n{report}");

    let honest = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    // (a) wrong `ref.test` type index: `wrapItems` tests struct type 0 -> 1.
    // (b) swapped dispatch cascade: `tagName` tests tags 2 <-> 3.
    // (c) wrong `array.new_data` data-segment index: `tagName`'s "alpha" 0 -> 9.
    // (d) wrong `ref.null` result heap type: `wrapItems` 8 -> 18.
    // (e) equal-length payload collision: `tagName`'s "alpha" -> "alphb" (same
    //     length, same data index). The code-entry lowering pins only the payload
    //     LENGTH, so every byte-equality pin stays green; ONLY the acceptance
    //     predicate's `verbatimPayloadsBound` conjunct (payload bytes vs the
    //     byte-pinned data segment) declines it. Deleting that conjunct makes this
    //     verify — the regression this vector guards.
    let tampers: [(&str, &str, &str); 5] = [
        (
            "ref.test type index",
            ".test 0 (.project 0 0) (.leaf (.refNull))",
            ".test 1 (.project 0 0) (.leaf (.refNull))",
        ),
        (
            "swapped dispatch cascade",
            ".test 2 (.arrayNewData 5 0 [97, 108, 112, 104, 97]) (.test 3",
            ".test 3 (.arrayNewData 5 0 [97, 108, 112, 104, 97]) (.test 2",
        ),
        (
            "array.new_data data index",
            ".arrayNewData 5 0 [97, 108, 112, 104, 97]",
            ".arrayNewData 5 9 [97, 108, 112, 104, 97]",
        ),
        (
            "ref.null heap type",
            "resultHeapTy := 8",
            "resultHeapTy := 18",
        ),
        (
            "equal-length payload collision",
            ".arrayNewData 5 0 [97, 108, 112, 104, 97]",
            ".arrayNewData 5 0 [97, 108, 112, 104, 98]",
        ),
    ];
    for (label, from, to) in tampers {
        assert!(
            honest.contains(from),
            "verbatimgen Plans.lean verbatim-plan shape changed ({label}); update the test"
        );
        let dir = temp_dir("cert-verbatim-plan-tamper");
        copy_dir(&out_dir, &dir);
        let tampered_plans = dir.join("cert").join("Plans.lean");
        let src = std::fs::read_to_string(&tampered_plans).unwrap();
        std::fs::write(&tampered_plans, src.replacen(from, to, 1)).unwrap();
        let (ok, report) = aver_verify(&dir.join("verbatimgen.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "{label}: tampered verbatim plan must be declined:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    // Guard-isolation: prove in-kernel that each vector diverges the lowered
    // code-entry bytes (so the byte-equality gate — the whole binding — catches
    // it), mirroring the spike's four `by decide` vectors on the real plans.
    // carrier 7; `wrapItems` result heap 8, `tagName` string-array type 5.
    let mut lean = String::new();
    lean.push_str("import Schema\nimport PlanCheck\nimport PlanLower\nimport PlanBytes\n\n");
    lean.push_str("open AverCert.Schema\nopen AverCert.PlanBytes\n\n");
    lean.push_str("def honestWrap : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 2, fieldLocal := 1, resultHeapTy := 8, body := .test 0 (.project 0 0) (.leaf (.refNull)) }\n");
    lean.push_str("def honestTag : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 1, fieldLocal := 0, resultHeapTy := 5, body := .test 2 (.arrayNewData 5 0 [97, 108, 112, 104, 97]) (.test 3 (.arrayNewData 5 1 [98, 101, 116, 97]) (.leaf (.arrayNewData 5 2 [103, 97, 109, 109, 97]))) }\n\n");
    lean.push_str("example : AverCert.PlanCheck.checkVerbatimRawPlan honestWrap = true := rfl\n");
    lean.push_str("example : AverCert.PlanCheck.checkVerbatimRawPlan honestTag = true := rfl\n\n");
    lean.push_str("def tamper1 : VerbatimRawPlan := { honestWrap with body := .test 1 (.project 0 0) (.leaf (.refNull)) }\n");
    lean.push_str("example : lowerVerbatimCodeEntry 7 tamper1 ≠ lowerVerbatimCodeEntry 7 honestWrap := by decide\n");
    lean.push_str("def tamper2 : VerbatimRawPlan := { honestTag with body := .test 3 (.arrayNewData 5 0 [97, 108, 112, 104, 97]) (.test 2 (.arrayNewData 5 1 [98, 101, 116, 97]) (.leaf (.arrayNewData 5 2 [103, 97, 109, 109, 97]))) }\n");
    lean.push_str("example : lowerVerbatimCodeEntry 7 tamper2 ≠ lowerVerbatimCodeEntry 7 honestTag := by decide\n");
    lean.push_str("def tamper3 : VerbatimRawPlan := { honestTag with body := .test 2 (.arrayNewData 5 9 [97, 108, 112, 104, 97]) (.test 3 (.arrayNewData 5 1 [98, 101, 116, 97]) (.leaf (.arrayNewData 5 2 [103, 97, 109, 109, 97]))) }\n");
    lean.push_str("example : lowerVerbatimCodeEntry 7 tamper3 ≠ lowerVerbatimCodeEntry 7 honestTag := by decide\n");
    lean.push_str("def tamper4 : VerbatimRawPlan := { honestWrap with resultHeapTy := 18 }\n");
    lean.push_str("example : lowerVerbatimCodeEntry 7 tamper4 ≠ lowerVerbatimCodeEntry 7 honestWrap := by decide\n");

    let honest_build = Command::new("lake")
        .arg("build")
        .current_dir(&cert)
        .output()
        .expect("lake build runs");
    assert!(honest_build.status.success(), "honest cert must lake-build");
    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let elab = Command::new("lake")
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .current_dir(&cert)
        .output()
        .expect("lake env lean runs");
    assert!(
        elab.status.success(),
        "verbatim guard-isolation assertions must all hold:\n{}\n{}",
        String::from_utf8_lossy(&elab.stdout),
        String::from_utf8_lossy(&elab.stderr)
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// ACCEPTANCE-LEVEL guard-isolation for the two verbatim binds that the
/// byte-equality gate does NOT cover, elaborated with `lake env lean` against the
/// audited cert modules — no `aver cert verify`, no sibling defence in the path.
/// The verbatim family has no host/self calls, so before these binds the code
/// entry was the whole binding; but the code entry omits the function SIGNATURE
/// (a second parameter leaves the locals + body bytes identical) and the
/// `array.new_data` payload CONTENTS (only the segment index and length are
/// encoded). Each assertion is constructed so it holds ONLY because its target
/// guard fires (verified by weakening each guard in a throwaway copy: weakening
/// `verbatimFuncTypeMatches` to `true` breaks solely the binary-arity reject line;
/// weakening `verbatimPayloadsBound`/`verbatimLeafPayloadBound` to `true` breaks
/// solely the equal-length-collision reject line; reverting `checkVerbatimLeaf`'s
/// `arrayNewData` arm to `true` breaks solely the out-of-range reject line —
/// nothing else moves).
///
/// SIGNATURE guard: two minimal modules identical in every section EXCEPT the
/// type section (the second appends a second `eqref` parameter). The
/// func/export/code/data sections — hence the byte-derived `FuncBinding` and code
/// entry the byte-equality gate reads — are byte-for-byte identical, so the two
/// sibling conjuncts (`funcBindingForExport`, `codeEntryForExport`) return the
/// SAME value and only `verbatimFuncTypeMatches` distinguishes unary from binary.
///
/// PAYLOAD guard: an equal-length payload substitution (`"alpha"` -> `"alphb"`)
/// that the structural checker (`checkVerbatimRawPlan`) and the byte lowering
/// (`lowerVerbatimCodeEntry`) are proven BLIND to (both accept / both lower to the
/// same bytes); only `verbatimPayloadsBound`, comparing against the byte-pinned
/// data segment, rejects it. Plus the FIX 2(c) out-of-range payload reject.
#[test]
fn verbatim_kernel_guards_are_isolating() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping verbatim-guard isolation test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    let mut lean = String::new();
    lean.push_str("import Schema\nimport PlanCheck\nimport PlanBytes\nimport WasmSlice\nimport AcceptedArtifact\n\n");
    lean.push_str("open AverCert\nopen AverCert.Schema\n");
    lean.push_str("set_option maxRecDepth 100000\n\n");
    // Minimal modules: header, type section, then a shared func/export/code/data
    // tail. `f` is func 0 of type 0; code entry `[2, 0, 11]`; data segment 0 is
    // "alpha" (passive). Only the type section differs between the two.
    lean.push_str("def hdr : List Nat := [0, 97, 115, 109, 1, 0, 0, 0]\n");
    lean.push_str("def unaryType : List Nat := [1, 7, 1, 96, 1, 109, 1, 99, 5]\n");
    lean.push_str("def binaryType : List Nat := [1, 8, 1, 96, 2, 109, 109, 1, 99, 5]\n");
    lean.push_str("def tailSecs : List Nat := [3, 2, 1, 0, 7, 5, 1, 1, 102, 0, 0, 10, 4, 1, 2, 0, 11, 11, 8, 1, 1, 5, 97, 108, 112, 104, 97]\n");
    lean.push_str("def unaryMod : List Nat := hdr ++ unaryType ++ tailSecs\n");
    lean.push_str("def binaryMod : List Nat := hdr ++ binaryType ++ tailSecs\n");
    lean.push_str("def nameF : List Nat := [102]\n\n");
    // SIGNATURE isolation: the byte-equality gate's inputs are identical...
    lean.push_str("example : WasmSlice.funcBindingForExport unaryMod nameF = WasmSlice.funcBindingForExport binaryMod nameF := rfl\n");
    lean.push_str("example : WasmSlice.codeEntryForExport unaryMod nameF = WasmSlice.codeEntryForExport binaryMod nameF := rfl\n");
    // ...and only the signature guard tells unary from binary.
    lean.push_str("example : WasmSlice.verbatimFuncTypeMatches unaryMod 0 5 = true := rfl\n");
    lean.push_str("example : WasmSlice.verbatimFuncTypeMatches binaryMod 0 5 = false := rfl\n\n");
    // PAYLOAD isolation: segment 0 is "alpha".
    lean.push_str(
        "example : WasmSlice.dataSegmentBytes unaryMod 0 = some [97, 108, 112, 104, 97] := rfl\n",
    );
    lean.push_str("def planAlpha : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 1, fieldLocal := 0, resultHeapTy := 5, body := .leaf (.arrayNewData 5 0 [97, 108, 112, 104, 97]) }\n");
    lean.push_str("def planAlphB : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 1, fieldLocal := 0, resultHeapTy := 5, body := .leaf (.arrayNewData 5 0 [97, 108, 112, 104, 98]) }\n");
    // The structural checker and byte lowering are BLIND to the payload content...
    lean.push_str("example : PlanCheck.checkVerbatimRawPlan planAlpha = true := rfl\n");
    lean.push_str("example : PlanCheck.checkVerbatimRawPlan planAlphB = true := rfl\n");
    lean.push_str("example : PlanBytes.lowerVerbatimCodeEntry 7 planAlpha = PlanBytes.lowerVerbatimCodeEntry 7 planAlphB := rfl\n");
    // ...so only `verbatimPayloadsBound` rejects the equal-length collision.
    lean.push_str(
        "example : AcceptedArtifact.verbatimPayloadsBound unaryMod planAlpha.body = true := rfl\n",
    );
    lean.push_str("example : AcceptedArtifact.verbatimPayloadsBound unaryMod planAlphB.body = false := rfl\n\n");
    // FIX 2(c): an out-of-range payload element is rejected up front.
    lean.push_str("example : PlanCheck.checkVerbatimRawPlan { profile := \"verbatim-plan-v1\", scrutineeLocal := 1, fieldLocal := 0, resultHeapTy := 5, body := .leaf (.arrayNewData 5 0 [256]) } = false := rfl\n\n");

    // NULLABILITY isolation (re-review FIX 2): the certified verbatim signature is
    // `[eqref] -> [(ref null resultHeapTy)]` — the `0x63` nullable form the
    // `ref.null` default requires. A non-null `0x64` result is rejected. The only
    // byte differing between `unaryMod` and `nonNullMod` is `0x63 -> 0x64`, so the
    // byte-derived binding and code entry are IDENTICAL (the reftype is never in
    // the code entry) — only `checkVerbatimFuncType` tells them apart.
    lean.push_str(
        "example : WasmSlice.checkVerbatimFuncType 5 [96, 1, 109, 1, 99, 5] = true := rfl\n",
    );
    lean.push_str(
        "example : WasmSlice.checkVerbatimFuncType 5 [96, 1, 109, 1, 100, 5] = false := rfl\n",
    );
    lean.push_str(
        "def nonNullMod : List Nat := hdr ++ [1, 7, 1, 96, 1, 109, 1, 100, 5] ++ tailSecs\n",
    );
    lean.push_str("example : WasmSlice.funcBindingForExport nonNullMod nameF = WasmSlice.funcBindingForExport unaryMod nameF := rfl\n");
    lean.push_str("example : WasmSlice.codeEntryForExport nonNullMod nameF = WasmSlice.codeEntryForExport unaryMod nameF := rfl\n");
    lean.push_str("example : WasmSlice.verbatimFuncTypeMatches nonNullMod 0 5 = false := rfl\n\n");

    // PARSER STRICTNESS isolation (re-review FIX 3): the type-section and
    // data-section walkers parse EVERY declared entry/segment and require EXACT
    // payload exhaustion, so a valid entry followed by trailing bytes, or a count
    // that does not match the bytes, declines — and an over-wide LEB is rejected
    // by the width cap. The honest single-entry sections still match.
    // Type section: a trailing `0xff` after the one valid func type.
    lean.push_str("def trailingTypeMod : List Nat := hdr ++ [1, 8, 1, 96, 1, 109, 1, 99, 5, 255] ++ tailSecs\n");
    lean.push_str(
        "example : WasmSlice.verbatimFuncTypeMatches trailingTypeMod 0 5 = false := rfl\n",
    );
    // Type section: count claims 2 rectypes but only 1 is present.
    lean.push_str("def countMismatchTypeMod : List Nat := hdr ++ [1, 7, 2, 96, 1, 109, 1, 99, 5] ++ tailSecs\n");
    lean.push_str(
        "example : WasmSlice.verbatimFuncTypeMatches countMismatchTypeMod 0 5 = false := rfl\n",
    );
    // Data section: a trailing `0xff` after the one valid segment.
    lean.push_str(
        "def dataTrailMod : List Nat := hdr ++ [11, 9, 1, 1, 5, 97, 108, 112, 104, 97, 255]\n",
    );
    lean.push_str("example : WasmSlice.dataSegmentBytes dataTrailMod 0 = none := rfl\n");
    // Data section: count claims 2 segments but only 1 is present.
    lean.push_str(
        "def dataCountMismatchMod : List Nat := hdr ++ [11, 8, 2, 1, 5, 97, 108, 112, 104, 97]\n",
    );
    lean.push_str("example : WasmSlice.dataSegmentBytes dataCountMismatchMod 0 = none := rfl\n");
    // Over-wide (6-byte) unsigned LEB32 exceeds the u32 width cap and declines.
    lean.push_str("example : WasmSlice.readUleb32 [128, 128, 128, 128, 128, 0] = none := rfl\n");

    let out_dir = temp_dir("cert-verbatim-guard-iso");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/verbatimgen.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "verbatimgen compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let honest_build = Command::new("lake")
        .arg("build")
        .current_dir(&cert)
        .output()
        .expect("lake build runs");
    assert!(honest_build.status.success(), "honest cert must lake-build");

    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let elab = Command::new("lake")
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .current_dir(&cert)
        .output()
        .expect("lake env lean runs");
    assert!(
        elab.status.success(),
        "verbatim signature/payload guard-isolation assertions must all hold:\n{}\n{}",
        String::from_utf8_lossy(&elab.stdout),
        String::from_utf8_lossy(&elab.stderr)
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A tampered byte-first `int-dispatch-v1` plan is declined, and the tamper
/// vectors are shown to be guard-isolating. Each vector mutates the Int-face
/// `ref.test`-dispatch plan for `boxInt`/`gauge` in the shipped `Plans.lean`
/// while leaving the wasm untouched, so the plan no longer canonically lowers
/// to the export's real code-entry bytes. The plan names host helpers by ROLE
/// only (the byte-derived role table parameterizes the lowerers), so every
/// semantic field of the plan — tags, arm order, roles, constants, operand
/// order, and the default — reaches the lowered bytes and is caught by the
/// byte-equality gate: both the shipped `Plans.lean`
/// `lowerIntDispatchCodeEntry`/`codeEntryForExport` `rfl` pins and the
/// checker's `manifest.intDispatchPlans` `rfl` pin reject the tampered plan.
/// The `GuardIso.lean` block below isolates each byte-reaching vector by
/// proving in-kernel (`by decide`) that its lowered code entry diverges from
/// the honest one, and the profile vector by proving the lowering BLIND to it
/// (`rfl`) while only `checkIntDispatchRawPlan` rejects it.
#[test]
fn cert_verify_declines_tampered_int_dispatch_plan() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping int-dispatch-plan tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-int-dispatch-plan");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/intdispatchgen.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "intdispatchgen compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("intdispatchgen.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(
        ok,
        "honest int-dispatch certificate should verify:\n{report}"
    );

    let honest = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    // (a) wrong `ref.test` tag: `boxInt` tests struct type 0 -> 1.
    // (b) swapped dispatch cascade: `gauge` tests tags 3 <-> 4.
    // (c) role swap: `gauge`'s Lo arm combinator `sub` -> `add`. The role table
    //     maps roles to DISTINCT indices, so the swap changes the host call
    //     byte — the exact discrimination `hostTableIndicesDistinct` protects.
    // (d) wrong arm constant: `gauge`'s Hi arm `x + 9` -> `x + 8`.
    // (e) flipped operand order: `gauge`'s Hi arm payload-first -> const-first.
    // (f) wrong default constant: `gauge`'s Off arm `7` -> `8`.
    // (g) wrong profile string: rejected by `checkIntDispatchRawPlan` (the
    //     lowering is blind to the profile, so the byte gate alone would pass).
    let tampers: [(&str, &str, &str); 7] = [
        (
            "ref.test tag",
            ".test 0 (.proj) (.default (0))",
            ".test 1 (.proj) (.default (0))",
        ),
        (
            "swapped dispatch cascade",
            ".test 3 (.hostOp .sub (0) true) (.test 4",
            ".test 4 (.hostOp .sub (0) true) (.test 3",
        ),
        (
            "host role swap",
            "(.hostOp .sub (0) true)",
            "(.hostOp .add (0) true)",
        ),
        (
            "arm constant",
            "(.hostOp .add (9) false)",
            "(.hostOp .add (8) false)",
        ),
        (
            "operand order flip",
            "(.hostOp .add (9) false)",
            "(.hostOp .add (9) true)",
        ),
        ("default constant", "(.default (7))", "(.default (8))"),
        (
            "profile string",
            "def boxIntIntDispatchPlan : IntDispatchRawPlan := { profile := \"int-dispatch-v1\"",
            "def boxIntIntDispatchPlan : IntDispatchRawPlan := { profile := \"int-dispatch-v2\"",
        ),
    ];
    for (label, from, to) in tampers {
        assert!(
            honest.contains(from),
            "intdispatchgen Plans.lean plan shape changed ({label}); update the test"
        );
        let dir = temp_dir("cert-int-dispatch-plan-tamper");
        copy_dir(&out_dir, &dir);
        let tampered_plans = dir.join("cert").join("Plans.lean");
        let src = std::fs::read_to_string(&tampered_plans).unwrap();
        std::fs::write(&tampered_plans, src.replacen(from, to, 1)).unwrap();
        let (ok, report) = aver_verify(&dir.join("intdispatchgen.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "{label}: tampered int-dispatch plan must be declined:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    // Guard-isolation: prove in-kernel that each byte-reaching vector diverges
    // the lowered code-entry bytes (so the byte-equality gate catches it), and
    // that the profile vector — which the lowering is provably BLIND to — is
    // rejected exactly by the structural checker. carrier 9; role table
    // box 7 / add 8 / sub 9 (the fixture's byte-derived table).
    let mut lean = String::new();
    lean.push_str("import Schema\nimport PlanCheck\nimport PlanLower\nimport PlanBytes\n\n");
    lean.push_str("open AverCert.Schema\nopen AverCert.PlanBytes\n\n");
    lean.push_str("def tbl : List (HostRole × Nat) := [(.box, 7), (.add, 8), (.sub, 9)]\n");
    lean.push_str("def honestBox : IntDispatchRawPlan := { profile := \"int-dispatch-v1\", body := .test 0 (.proj) (.default (0)) }\n");
    lean.push_str("def honestGauge : IntDispatchRawPlan := { profile := \"int-dispatch-v1\", body := .test 3 (.hostOp .sub (0) true) (.test 4 (.hostOp .add (9) false) (.test 5 (.proj) (.default (7)))) }\n\n");
    lean.push_str("example : AverCert.PlanCheck.checkIntDispatchRawPlan honestBox = true := rfl\n");
    lean.push_str(
        "example : AverCert.PlanCheck.checkIntDispatchRawPlan honestGauge = true := rfl\n\n",
    );
    lean.push_str("def tamper1 : IntDispatchRawPlan := { honestBox with body := .test 1 (.proj) (.default (0)) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 9 tbl tamper1 ≠ lowerIntDispatchCodeEntry 9 tbl honestBox := by decide\n");
    lean.push_str("def tamper2 : IntDispatchRawPlan := { honestGauge with body := .test 4 (.hostOp .sub (0) true) (.test 3 (.hostOp .add (9) false) (.test 5 (.proj) (.default (7)))) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 9 tbl tamper2 ≠ lowerIntDispatchCodeEntry 9 tbl honestGauge := by decide\n");
    lean.push_str("def tamper3 : IntDispatchRawPlan := { honestGauge with body := .test 3 (.hostOp .add (0) true) (.test 4 (.hostOp .add (9) false) (.test 5 (.proj) (.default (7)))) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 9 tbl tamper3 ≠ lowerIntDispatchCodeEntry 9 tbl honestGauge := by decide\n");
    lean.push_str("def tamper4 : IntDispatchRawPlan := { honestGauge with body := .test 3 (.hostOp .sub (0) true) (.test 4 (.hostOp .add (8) false) (.test 5 (.proj) (.default (7)))) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 9 tbl tamper4 ≠ lowerIntDispatchCodeEntry 9 tbl honestGauge := by decide\n");
    lean.push_str("def tamper5 : IntDispatchRawPlan := { honestGauge with body := .test 3 (.hostOp .sub (0) true) (.test 4 (.hostOp .add (9) true) (.test 5 (.proj) (.default (7)))) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 9 tbl tamper5 ≠ lowerIntDispatchCodeEntry 9 tbl honestGauge := by decide\n");
    lean.push_str("def tamper6 : IntDispatchRawPlan := { honestGauge with body := .test 3 (.hostOp .sub (0) true) (.test 4 (.hostOp .add (9) false) (.test 5 (.proj) (.default (8)))) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 9 tbl tamper6 ≠ lowerIntDispatchCodeEntry 9 tbl honestGauge := by decide\n");
    // Profile vector: the lowering is BLIND to the profile string, so the byte
    // gate alone would accept it — only the structural checker rejects it.
    lean.push_str(
        "def tamper7 : IntDispatchRawPlan := { honestBox with profile := \"int-dispatch-v2\" }\n",
    );
    lean.push_str("example : lowerIntDispatchCodeEntry 9 tbl tamper7 = lowerIntDispatchCodeEntry 9 tbl honestBox := rfl\n");
    lean.push_str("example : AverCert.PlanCheck.checkIntDispatchRawPlan tamper7 = false := rfl\n");

    let honest_build = Command::new("lake")
        .arg("build")
        .current_dir(&cert)
        .output()
        .expect("lake build runs");
    assert!(honest_build.status.success(), "honest cert must lake-build");
    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let elab = Command::new("lake")
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .current_dir(&cert)
        .output()
        .expect("lake env lean runs");
    assert!(
        elab.status.success(),
        "int-dispatch guard-isolation assertions must all hold:\n{}\n{}",
        String::from_utf8_lossy(&elab.stdout),
        String::from_utf8_lossy(&elab.stderr)
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// ACCEPTANCE-LEVEL guard-isolation for the two Int-face dispatch binds that
/// the byte-equality gate does NOT cover, elaborated with `lake env lean`
/// against the audited cert modules — no `aver cert verify`, no sibling
/// defence in the path.
///
/// SIGNATURE guard (`verbatimFuncTypeMatches … carrier`, reused with the Int
/// carrier as the result heap type): two minimal modules identical in every
/// section EXCEPT the type section (the second appends a second `eqref`
/// parameter). The func/export/code sections — hence the byte-derived
/// `FuncBinding` and code entry the byte-equality gate reads — are
/// byte-for-byte identical, so the two sibling conjuncts
/// (`funcBindingForExport`, `codeEntryForExport`) return the SAME value and
/// only the signature conjunct distinguishes unary from binary.
///
/// HOST-TABLE DISTINCTNESS guard (`hostTableIndicesDistinct`): the plan names
/// helpers by ROLE and the byte lowering substitutes table indices, so under a
/// DUPLICATED table (add and sub claiming one index) two plans differing only
/// in an arm's role are proven to lower to IDENTICAL bytes (`rfl` — the byte
/// gate is blind); under the honest distinct table they are proven to diverge
/// (`by decide`). Only `hostTableIndicesDistinct` rejects the duplicated
/// table, restoring the gate's discrimination.
///
/// Each assertion is constructed so it holds ONLY because its target guard
/// fires (weaken-confirmed: replacing the `verbatimFuncTypeMatches` conjunct
/// with `true` in a throwaway copy breaks solely the binary-arity reject line;
/// replacing `hostTableIndicesDistinct` with `true` breaks solely the
/// duplicated-table reject line — nothing else moves).
#[test]
fn int_dispatch_kernel_guards_are_isolating() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping int-dispatch guard isolation test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    let mut lean = String::new();
    lean.push_str(
        "import Schema\nimport PlanCheck\nimport PlanBytes\nimport WasmSlice\nimport AcceptedArtifact\n\n",
    );
    lean.push_str("open AverCert\nopen AverCert.Schema\n");
    lean.push_str("set_option maxRecDepth 100000\n\n");
    // Minimal modules: header, type section, then a shared func/export/code
    // tail. `f` is func 0 of type 0; code entry `[2, 0, 11]`. Only the type
    // section differs between the two: type 0 is `[eqref] -> [(ref null 5)]`
    // vs `[eqref, eqref] -> [(ref null 5)]` (5 standing for the carrier).
    lean.push_str("def hdr : List Nat := [0, 97, 115, 109, 1, 0, 0, 0]\n");
    lean.push_str("def unaryType : List Nat := [1, 7, 1, 96, 1, 109, 1, 99, 5]\n");
    lean.push_str("def binaryType : List Nat := [1, 8, 1, 96, 2, 109, 109, 1, 99, 5]\n");
    lean.push_str(
        "def tailSecs : List Nat := [3, 2, 1, 0, 7, 5, 1, 1, 102, 0, 0, 10, 4, 1, 2, 0, 11]\n",
    );
    lean.push_str("def unaryMod : List Nat := hdr ++ unaryType ++ tailSecs\n");
    lean.push_str("def binaryMod : List Nat := hdr ++ binaryType ++ tailSecs\n");
    lean.push_str("def nameF : List Nat := [102]\n\n");
    // SIGNATURE isolation: the byte-equality gate's inputs are identical...
    lean.push_str("example : WasmSlice.funcBindingForExport unaryMod nameF = WasmSlice.funcBindingForExport binaryMod nameF := rfl\n");
    lean.push_str("example : WasmSlice.codeEntryForExport unaryMod nameF = WasmSlice.codeEntryForExport binaryMod nameF := rfl\n");
    // ...and only the signature guard tells unary from binary.
    lean.push_str("example : WasmSlice.verbatimFuncTypeMatches unaryMod 0 5 = true := rfl\n");
    lean.push_str("example : WasmSlice.verbatimFuncTypeMatches binaryMod 0 5 = false := rfl\n\n");
    // HOST-TABLE DISTINCTNESS isolation: two plans differing ONLY in the arm's
    // host ROLE.
    lean.push_str("def planAdd : IntDispatchRawPlan := { profile := \"int-dispatch-v1\", body := .test 3 (.hostOp .add (2) false) (.default (0)) }\n");
    lean.push_str("def planSub : IntDispatchRawPlan := { profile := \"int-dispatch-v1\", body := .test 3 (.hostOp .sub (2) false) (.default (0)) }\n");
    lean.push_str("def dupTbl : List (HostRole × Nat) := [(.box, 7), (.add, 8), (.sub, 8)]\n");
    lean.push_str("def distinctTbl : List (HostRole × Nat) := [(.box, 7), (.add, 8), (.sub, 9)]\n");
    // The structural checker is blind to the role either way...
    lean.push_str("example : PlanCheck.checkIntDispatchRawPlan planAdd = true := rfl\n");
    lean.push_str("example : PlanCheck.checkIntDispatchRawPlan planSub = true := rfl\n");
    // ...under a duplicated table the byte lowering is blind to it too...
    lean.push_str("example : PlanBytes.lowerIntDispatchCodeEntry 5 dupTbl planAdd = PlanBytes.lowerIntDispatchCodeEntry 5 dupTbl planSub := rfl\n");
    // ...under the honest distinct table the byte gate discriminates...
    lean.push_str("example : PlanBytes.lowerIntDispatchCodeEntry 5 distinctTbl planAdd ≠ PlanBytes.lowerIntDispatchCodeEntry 5 distinctTbl planSub := by decide\n");
    // ...and only the distinctness guard rejects the duplicated table.
    lean.push_str("example : PlanCheck.hostTableIndicesDistinct dupTbl = false := rfl\n");
    lean.push_str("example : PlanCheck.hostTableIndicesDistinct distinctTbl = true := rfl\n\n");
    // A role missing from the table fail-closes the lowering entirely (the
    // plan cannot conjure a callee out of a missing contract).
    lean.push_str("example : PlanBytes.lowerIntDispatchCodeEntry 5 [(.box, 7), (.add, 8)] planSub = none := rfl\n");

    let out_dir = temp_dir("cert-int-dispatch-guard-iso");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/intdispatchgen.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "intdispatchgen compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let honest_build = Command::new("lake")
        .arg("build")
        .current_dir(&cert)
        .output()
        .expect("lake build runs");
    assert!(honest_build.status.success(), "honest cert must lake-build");

    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let elab = Command::new("lake")
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .current_dir(&cert)
        .output()
        .expect("lake env lean runs");
    assert!(
        elab.status.success(),
        "int-dispatch signature/host-table guard-isolation assertions must all hold:\n{}\n{}",
        String::from_utf8_lossy(&elab.stdout),
        String::from_utf8_lossy(&elab.stderr)
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}
