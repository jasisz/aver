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
//!   (s) migrated-recursion code decouple: `sumTo`'s obligation points at a
//!       decoy `wrongCode`; the generic `recursionClaimAccepted` byte binding
//!       rejects it without any bespoke simulation-proof swap → DECLINED
//!   (t) migrated-recursion self decouple: `sumTo`'s obligation uses a wrong
//!       function index; the generic recursion claim binds it to the byte index
//!       and fails closed without a bespoke proof → DECLINED
//!   (u) String.eq helper shape: a byte-level mutation inside the exact
//!       compiler-generated helper, with the wasm hash rebound, breaks the
//!       Lean byte-origin/host binding → DECLINED
//!   (v) String.eq contract drift: deleting the plan-required contract from
//!       both `Manifest.lean` and `cert-manifest.json` fails `ClaimAxes` → DECLINED
//!   (w) String.concat helper shape / contract drift: same fail-closed checks
//!       for the concat helper's byte-exact host-contract recognition
//!   (x) plan DATA drift: mutating `Plans.lean` fails its structural,
//!       canonical-lowering, or exact-byte binding inside Lean
//!   (y) package-format drift: an unknown `format.version` is rejected rather
//!       than reinterpreted under the current parser
//!   (z) wall drift: an unknown aggregate `wall_id` is rejected before Lean
//!       build; resolution has no filesystem, environment, or network fallback
//!   (aa) package authority: `Plans.lean` is the sole public plan DATA module;
//!       a hostile `ArtifactBytes.lean` decoy is ignored and checker-generated
//!       from the artifact
//!   (ab) artifact-data decoy: cert-supplied `Artifact.data` must bind its
//!       byte/manifest fields and satisfy the checker-owned Lean predicate
//!   (ac) artifact-root axiom: the artifact-carried bridge proof is the axiom
//!       audit root, so a smuggled axiom there is rejected
//! plus a separate empty-cert test: zero certified exports must NOT print the
//! green path and must exit nonzero, and the A5 report-line injection payload
//! (in the manifest and/or JSON) is rejected by the charset gate.
//!
//! Gated behind `wasm` (the `--certify` path needs the wasm-gc backend) and
//! skipped when `lake` is unavailable, mirroring `cert_certify_spec.rs`.
#![cfg(feature = "wasm")]

#[path = "support/cert_wall.rs"]
mod cert_wall;

use cert_wall::materialize as materialize_wall;
use std::path::{Path, PathBuf};
use std::process::Command;

fn temp_dir(prefix: &str) -> PathBuf {
    // Nanos alone collide when parallel tests request dirs in the same tick
    // (observed as spurious NotFound failures under RUST_TEST_THREADS=6);
    // the process id and a per-process counter make the name unique.
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let mut d = std::env::temp_dir();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let pid = std::process::id();
    let seq = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    d.push(format!("aver-{prefix}-{nanos}-{pid}-{seq}"));
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
    command.env(
        "AVER_CERT_DATA_CACHE",
        std::env::temp_dir().join("aver-cert-data-store"),
    );
    command
}

fn lake_for_cert(cert_dir: &Path) -> Command {
    materialize_wall(cert_dir);
    let mut command = Command::new("lake");
    command.current_dir(cert_dir);
    command
}

fn aver_check(artifact: &Path, cert_dir: &Path) -> (bool, String) {
    aver_cert(&["check"], artifact, cert_dir)
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
        .env("AVER_CERT_DATA_CACHE", "0")
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

fn find_named_file(root: &Path, name: &str) -> Option<PathBuf> {
    for entry in std::fs::read_dir(root).ok()? {
        let entry = entry.ok()?;
        let path = entry.path();
        if entry.file_type().ok()?.is_dir() {
            if let Some(found) = find_named_file(&path, name) {
                return Some(found);
            }
        } else if entry.file_name() == name {
            return Some(path);
        }
    }
    None
}

#[test]
fn cert_verify_rebuilds_after_cached_olean_corruption() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cert DATA-cache corruption test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-data-cache-corruption-artifact");
    let cache_dir = temp_dir("cert-data-cache-corruption-store");
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
        .expect("compile mutual fixture for DATA-cache corruption test");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let check = || {
        Command::new(env!("CARGO_BIN_EXE_aver"))
            .env(
                "AVER_CERT_PRELUDE_CACHE",
                std::env::temp_dir().join("aver-cert-prelude-store"),
            )
            .env("AVER_CERT_DATA_CACHE", &cache_dir)
            .arg("cert")
            .arg("check")
            .arg(out_dir.join("mutual.wasm"))
            .arg(out_dir.join("cert"))
            .output()
            .expect("check mutual fixture with isolated DATA cache")
    };
    let first = check();
    assert!(
        first.status.success(),
        "initial cached preflight failed:\n{}{}",
        String::from_utf8_lossy(&first.stdout),
        String::from_utf8_lossy(&first.stderr)
    );

    let artifact_olean = find_named_file(&cache_dir, "Artifact.olean")
        .expect("successful verify should publish cached Artifact.olean");
    let mut corrupted = std::fs::read(&artifact_olean).unwrap();
    assert!(!corrupted.is_empty(), "Artifact.olean must not be empty");
    corrupted[0] ^= 0xff;
    std::fs::write(&artifact_olean, &corrupted).unwrap();

    let second = check();
    let second_report = format!(
        "{}{}",
        String::from_utf8_lossy(&second.stdout),
        String::from_utf8_lossy(&second.stderr)
    );
    assert!(
        second.status.success(),
        "corrupt cached olean must be rejected and rebuilt (or fail closed):\n{second_report}"
    );
    assert!(
        second_report.contains("CHECKED") && !second_report.contains("CERTIFIED"),
        "rebuilt preflight did not produce the trusted-olean verdict:\n{second_report}"
    );
    assert_ne!(
        std::fs::read(&artifact_olean).unwrap(),
        corrupted,
        "corrupted Artifact.olean survived integrity validation"
    );

    let _ = std::fs::remove_dir_all(out_dir);
    let _ = std::fs::remove_dir_all(cache_dir);
}

fn set_named_code_nlocals_to_zero(
    module: &Path,
    export_name: &str,
    arity: u32,
    canonical_nlocals: u32,
) {
    let src = std::fs::read_to_string(module).unwrap();
    let def_marker = format!("def {export_name}Code : CodeTbl");
    let start = src
        .find(&def_marker)
        .unwrap_or_else(|| panic!("{export_name} code table should exist"));
    let end = start
        + src[start..]
            .find("\n\n/-- Runtime host wiring")
            .unwrap_or_else(|| panic!("{export_name} code table should have a bounded definition"));
    let header = format!("some ⟨{arity}, {canonical_nlocals},");
    let zero_header = format!("some ⟨{arity}, 0,");
    let code_def = &src[start..end];
    assert!(
        code_def.contains(&header),
        "{export_name} canonical locals-count header changed; update the test"
    );
    let zeroed = code_def.replacen(&header, &zero_header, 1);
    let mut tampered = String::with_capacity(src.len());
    tampered.push_str(&src[..start]);
    tampered.push_str(&zeroed);
    tampered.push_str(&src[end..]);
    std::fs::write(module, tampered).unwrap();
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
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(
        ok,
        "expected clean goals certificate to verify:
{report}"
    );
    assert!(
        report.contains("CHECKED") && !report.contains("CERTIFIED"),
        "clean goals certificate should pass trusted-olean preflight:
{report}"
    );
    (out_dir, wasm, cert)
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
    // The green report names the actual boundary: exact bytes and manifest pass
    // the checker-owned Lean acceptance predicate.
    assert!(
        report.contains("artifact-check:") && report.contains("checker-owned Lean predicate"),
        "missing artifact-check line on the happy path:\n{report}"
    );

    // The cert schema version is a breaking cert-data shape. The checker rejects
    // unsupported manifests instead of trying to reinterpret them under the
    // current schema.
    {
        let dir = temp_dir("neg-schema-v99");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["schema_version"] = serde_json::json!(99);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "schema v99 cert must be rejected:\n{out}");
        assert!(
            out.contains("unsupported certificate schema_version 99"),
            "wrong reason for schema v99 rejection:\n{out}"
        );
    }

    // The package format is independently versioned from the Lean statement
    // schema. Unknown versions are never reinterpreted as the current shape.
    {
        let dir = temp_dir("neg-format-version");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["format"]["version"] = serde_json::json!(2);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "format version drift must be rejected:\n{out}");
        assert!(
            out.contains("unsupported certificate format version 2"),
            "wrong reason for format version drift:\n{out}"
        );
    }

    // One aggregate identifier commits to every checker-owned Lean source and
    // the exact toolchain. Resolution is embedded-only: no path, URL, or
    // ambient installation is consulted for an unknown wall.
    {
        let dir = temp_dir("neg-wall-id");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["format"]["wall_id"] = serde_json::json!("sha256:deadbeef");
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "unknown wall id must be rejected:\n{out}");
        assert!(
            out.contains("unsupported certificate wall `sha256:deadbeef`"),
            "wrong reason for unknown wall rejection:\n{out}"
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "wrong Lean artifact root must be rejected:\n{out}");
        assert!(
            out.contains("manifest.subject.artifactRoot"),
            "wrong reason for Lean artifact root drift:\n{out}"
        );
    }

    // The artifact-carried data root is useful metadata, not authority. Since
    // the recursion exports carry byte-origin plan claims, an `Artifact.lean`
    // whose `data` points at zero module bytes can no longer even prove its own
    // claims (`exactFuncBindingForExport 0 modLen name code = some …` has no `rfl`), so the tamper
    // dies at the cert's own build — before the checker witness pins
    // `AverCert.Artifact.data` to the actual bytes and manifest.
    {
        let dir = temp_dir("neg-artifact-data-pin");
        copy_dir(&out_dir, &dir);
        let artifact = dir.join("cert").join("Artifact.lean");
        let src = std::fs::read_to_string(&artifact).unwrap();
        let corrupted = src.replacen(
            "modBytes := AverCert.ArtifactBytes.modBytes",
            "modBytes := 0",
            1,
        );
        assert_ne!(src, corrupted, "Artifact.lean data shape changed");
        std::fs::write(&artifact, corrupted).unwrap();
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
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
        let (ok, out) = aver_check(&w, &dir.join("cert"));
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
        let (ok, out) = aver_check(&w, &dir.join("cert"));
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(ok, "cert-supplied Schema.lean must be ignored:\n{out}");
        assert!(
            out.contains("CHECKED") && !out.contains("CERTIFIED"),
            "genuine cert should pass trusted-olean preflight (d):\n{out}"
        );
    }

    // (e) A1 hash rebind: replace the artifact with a DIFFERENT but genuine cert
    //     module (certprobe's wasm) and edit ONLY `wasm_sha256` in the JSON to
    //     match it. The fast JSON pre-check passes and the swapped module still
    //     validates as Wasm. The recursion exports carry byte-origin plan claims that
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
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
    //      catch the swap: the theorems (and the manifest artifact-hash face) talk about
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
        let (ok, out) = aver_check(&w, &empty_out.join("cert"));
        let _ = std::fs::remove_dir_all(&empty_out);
        assert!(!ok, "A1 hash rebind on claim-free cert must fail:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (e2):\n{out}");
        // The witness names the exact face the kernel rejected.
        assert!(
            out.contains("AverCert.manifest.subject.artifactHash"),
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
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
        let redirected = "import Lake\nopen Lake DSL\n\npackage «hostile»\n\n\
             @[default_target]\nlean_lib «Hostile» where\n  srcDir := \"hidden\"\n  roots := #[`Final]\n";
        std::fs::write(&lf, redirected).unwrap();
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &cert);
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(ok, "shipped .lake cache must be ignored:\n{out}");
        assert!(
            out.contains("CHECKED") && !out.contains("CERTIFIED"),
            "genuine cert should pass trusted-olean preflight (h):\n{out}"
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
                "theorem": "CertProofs.withdrawAll_wasm_certified",
                "dom": "List Int",
                "cod": "Int"
            }));
        m["runtime_contracts"]
            .as_array_mut()
            .unwrap()
            .push(serde_json::Value::String("FAKE contract injected".into()));
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        // Every .lean and hash is byte-identical; only the JSON changed.
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "padded JSON must be DECLINED, not credited:\n{out}");
        assert!(out.contains("does not bind"), "wrong reason (i):\n{out}");
        // The declined diagnostic echoes the rejected candidate; what matters is
        // that the forged export is never CERTIFIED (credited).
        assert!(
            !out.contains("CERTIFIED"),
            "forged export credited (i):\n{out}"
        );
    }

    // Class labels are paired with exports by Lean, not compared as two bags.
    // Swapping the two distinct recursion labels while preserving names and
    // order must therefore fail the atomic `reportEntries` binding.
    {
        let dir = temp_dir("neg-i-report-pair-swap");
        copy_dir(&out_dir, &dir);
        let mf = dir.join("cert/cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        let certified = m["certified"].as_array_mut().unwrap();
        assert!(certified.len() >= 2);
        let first = certified[0]["class"].as_str().unwrap().to_string();
        let second = certified[1]["class"].as_str().unwrap().to_string();
        assert_ne!(first, second, "fixture needs two distinct report classes");
        certified[0]["class"] = serde_json::Value::String(second);
        certified[1]["class"] = serde_json::Value::String(first);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "swapped export/class pairs must be DECLINED:\n{out}");
        assert!(
            out.contains("does not bind"),
            "wrong report-pair decline:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "swapped classes were credited:\n{out}"
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
        m["certified"]
            .as_array_mut()
            .unwrap()
            .push(serde_json::json!({
                "name": "phantom",
                "class": "straight-line",
                "policy": "simulatesModel",
                "level": "L1",
                "dom": "List Int",
                "cod": "Int"
            }));
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "control char in a candidate must fail (l):\n{out}");
        assert!(out.contains("printable ASCII"), "wrong reason (l):\n{out}");
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "hostile cert file name must fail (n):\n{out}");
        assert!(
            out.contains("bad name.lean") && out.contains("^[A-Za-z][A-Za-z0-9_]*\\.lean$"),
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "code-executing token in a data file must fail (o):\n{out}"
        );
        assert!(
            out.contains("elaboration-executing") && out.contains("#eval"),
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "a body that does not bind to the artifact bytes must be DECLINED:\n{out}"
        );
        // The acceptance predicate now pins the locals count exactly, so this
        // mutation can trip either the shipped artifact's own acceptance `rfl`
        // during the lake build ("did not build") or the later checker-witness
        // code binding ("does not bind"). Both are the same fail-closed
        // decline; the earlier stage is the stronger constraint.
        assert!(
            out.contains("does not bind") || out.contains("did not build"),
            "wrong reason (p):\n{out}"
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "shadow decoy must be DECLINED:\n{out}");
        // The locals-count mutation can trip the shipped artifact's own
        // acceptance `rfl` at lake build ("did not build") or the later
        // checker-witness code binding ("does not bind") — the shadow decoy
        // fools neither stage.
        assert!(
            out.contains("does not bind") || out.contains("did not build"),
            "wrong reason (q):\n{out}"
        );
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "comment decoy must be DECLINED:\n{out}");
        // Same stage-agnostic decline as (q): the locals-count pin can trip at
        // the artifact's own lake build or at the checker witness.
        assert!(
            out.contains("does not bind") || out.contains("did not build"),
            "wrong reason (r):\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "comment decoy credited (r):\n{out}"
        );
    }

    // (s) Migrated-recursion code decouple: point `sumTo`'s obligation at a
    //     decoy `wrongCode`, leaving the byte-honest `sumToCode` dead. `sumTo`
    //     deliberately has no bespoke `sumTo_simulates` proof now: the generic
    //     recursion bridge's `recursionClaimAccepted` must bind the obligation's
    //     code directly to the byte-derived plan and fail closed.
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &cert);
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

    // (t) Migrated-recursion self decouple: set `sumTo`'s obligation `self` to a
    //     wrong index. The generic recursion bridge's `recursionClaimAccepted`
    //     binds `obligation.self` to the byte-derived function index, so this
    //     must fail closed without any bespoke simulation-proof replacement.
    {
        let dir = temp_dir("neg-t");
        copy_dir(&out_dir, &dir);
        let cert = dir.join("cert");
        let man = cert.join("Manifest.lean");
        let msrc = std::fs::read_to_string(&man).unwrap();
        let decoupled = msrc.replacen("self := 1,", "self := 999,", 1);
        assert_ne!(msrc, decoupled, "manifest self field shape changed");
        std::fs::write(&man, decoupled).unwrap();
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &cert);
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
    //     fails the cert's OWN build; the checker witness's manifest export
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
        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
        assert!(!ok, "export-name relabel must be DECLINED (u):\n{out}");
        assert!(
            out.contains("did not build") || out.contains("does not bind"),
            "wrong reason (u):\n{out}"
        );
        assert!(!out.contains("CERTIFIED"), "relabel credited (u):\n{out}");
    }

    // (v) Migrated accumulator-recursion code decouple: point `countDown` at an
    //     always-trapping code table. There is no bespoke simulation theorem to
    //     swap after the migration; the audited generic claim/bridge must bind
    //     the arity-two obligation directly to the byte-derived plan.
    {
        let dir = temp_dir("neg-v-countdown-code");
        copy_dir(&out_dir, &dir);
        let cert = dir.join("cert");
        let module = cert.join("Module.lean");
        let src = std::fs::read_to_string(&module).unwrap();
        let with_decoy = src.replacen(
            "end CertModule",
            "/-- decoy: always traps, so `holds` is vacuous. -/\n\
             def wrongCode : CodeTbl := fun _ => none\nend CertModule",
            1,
        );
        assert_ne!(src, with_decoy, "module end marker shape changed");
        std::fs::write(&module, with_decoy).unwrap();

        let manifest = cert.join("Manifest.lean");
        let msrc = std::fs::read_to_string(&manifest).unwrap();
        let decoupled = msrc.replacen(
            "code := CertModule.countDownCode",
            "code := CertModule.wrongCode",
            1,
        );
        assert_ne!(msrc, decoupled, "countDown code field shape changed");
        std::fs::write(&manifest, decoupled).unwrap();

        let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &cert);
        assert!(!ok, "countDown code decouple must be DECLINED (v):\n{out}");
        assert!(
            out.contains("did not build") || out.contains("does not bind"),
            "wrong reason (v):\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "countDown code decouple credited (v):\n{out}"
        );
    }

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A valid custom section pads a real module to the 130,460-byte scale where
/// the old monolithic `List Nat` pin exhausted default heartbeats. The big-Nat
/// pin must close, while a one-byte-flipped expected entry must fail `rfl`.
#[test]
fn big_nat_code_entry_pin_closes_at_130kb_and_flipped_byte_fails() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping big-Nat scale regression: `lake` not available");
        return;
    }

    fn read_uleb(bytes: &[u8], cursor: &mut usize) -> usize {
        let mut value = 0usize;
        let mut shift = 0usize;
        loop {
            let byte = bytes[*cursor];
            *cursor += 1;
            value |= usize::from(byte & 0x7f) << shift;
            if byte < 0x80 {
                return value;
            }
            shift += 7;
        }
    }

    fn write_uleb(mut value: usize) -> Vec<u8> {
        let mut out = Vec::new();
        loop {
            let mut byte = (value & 0x7f) as u8;
            value >>= 7;
            if value != 0 {
                byte |= 0x80;
            }
            out.push(byte);
            if value == 0 {
                return out;
            }
        }
    }

    fn section_start(bytes: &[u8], target: u8) -> usize {
        let mut cursor = 8usize;
        while cursor < bytes.len() {
            let start = cursor;
            let id = bytes[cursor];
            cursor += 1;
            let size = read_uleb(bytes, &mut cursor);
            if id == target {
                return start;
            }
            cursor += size;
        }
        panic!("section {target} not found");
    }

    fn section_payload(bytes: &[u8], target: u8) -> (usize, usize) {
        let mut cursor = section_start(bytes, target) + 1;
        let size = read_uleb(bytes, &mut cursor);
        (cursor, size)
    }

    fn render_list(bytes: &[u8]) -> String {
        format!(
            "[{}]",
            bytes
                .iter()
                .map(u8::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    let (out_dir, wasm_path, cert) = compile_cert_goals("cert-bignat-scale");
    let original = std::fs::read(&wasm_path).unwrap();
    let code_start = section_start(&original, 10);

    const TARGET_LEN: usize = 130_460;
    let custom_payload_len = TARGET_LEN - original.len() - 4;
    let mut custom_payload = vec![0u8; custom_payload_len];
    custom_payload[..4].copy_from_slice(&[3, b'p', b'a', b'd']);
    let custom_size = write_uleb(custom_payload.len());
    assert_eq!(custom_size.len(), 3, "scale fixture framing assumption");

    let mut padded = Vec::with_capacity(TARGET_LEN);
    padded.extend_from_slice(&original[..code_start]);
    padded.push(0);
    padded.extend_from_slice(&custom_size);
    padded.extend_from_slice(&custom_payload);
    padded.extend_from_slice(&original[code_start..]);
    assert_eq!(padded.len(), TARGET_LEN);
    for payload in wasmparser::Parser::new(0).parse_all(&padded) {
        payload.expect("130KB custom-section-padded artifact must parse");
    }
    std::fs::write(out_dir.join("cert_goals_padded.wasm"), &padded).unwrap();

    let mut imported_funcs = 0u32;
    let mut add_two_func = None;
    for payload in wasmparser::Parser::new(0).parse_all(&padded) {
        match payload.expect("padded artifact must parse") {
            wasmparser::Payload::ImportSection(reader) => {
                for group in reader {
                    for import in group.expect("import group must parse") {
                        let (_, import) = import.expect("import must parse");
                        if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                            imported_funcs += 1;
                        }
                    }
                }
            }
            wasmparser::Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export.expect("export must parse");
                    if export.name == "addTwo" && export.kind == wasmparser::ExternalKind::Func {
                        add_two_func = Some(export.index);
                    }
                }
            }
            _ => {}
        }
    }
    let code_idx = add_two_func
        .expect("addTwo export")
        .checked_sub(imported_funcs)
        .expect("addTwo is defined") as usize;
    let (code_payload, _) = section_payload(&padded, 10);
    let mut cursor = code_payload;
    let count = read_uleb(&padded, &mut cursor);
    assert!(code_idx < count);
    let mut code_entry = Vec::new();
    for current in 0..count {
        let entry_start = cursor;
        let size = read_uleb(&padded, &mut cursor);
        let entry_end = cursor + size;
        if current == code_idx {
            code_entry.extend_from_slice(&padded[entry_start..entry_end]);
        }
        cursor = entry_end;
    }
    assert!(!code_entry.is_empty());

    let artifact_defs = aver::codegen::cert::render_artifact_bytes_lean(&padded)
        .replace("AverCert.ArtifactBytes", "LargeBytes");
    let positive = format!(
        "{artifact_defs}\n\
         theorem largePin : AverCert.WasmSlice.codeEntryForExport LargeBytes.modBytes LargeBytes.modLen [97, 100, 100, 84, 119, 111] = some {} := rfl\n\
         #print axioms largePin\n",
        render_list(&code_entry)
    );
    std::fs::write(cert.join("LargePin.lean"), positive).unwrap();
    let prebuild = lake_for_cert(&cert)
        .current_dir(&cert)
        .args(["build", "WasmSlice"])
        .output()
        .expect("build audited WasmSlice dependency");
    assert!(
        prebuild.status.success(),
        "WasmSlice dependency must build:\n{}{}",
        String::from_utf8_lossy(&prebuild.stdout),
        String::from_utf8_lossy(&prebuild.stderr)
    );
    let started = std::time::Instant::now();
    let large_pin = lake_for_cert(&cert)
        .current_dir(&cert)
        .args([
            "env",
            "lean",
            "-o",
            ".lake/build/lib/lean/LargePin.olean",
            "LargePin.lean",
        ])
        .output()
        .expect("elaborate 130KB big-Nat pin");
    let elapsed = started.elapsed();
    assert!(
        large_pin.status.success(),
        "130KB big-Nat code-entry pin must close (elapsed {elapsed:?}):\n{}{}",
        String::from_utf8_lossy(&large_pin.stdout),
        String::from_utf8_lossy(&large_pin.stderr)
    );
    eprintln!("130KB big-Nat code-entry pin closed in {elapsed:?}");

    let mut flipped = code_entry.clone();
    flipped[0] ^= 1;
    let negative = format!(
        "import LargePin\n\
         set_option maxRecDepth 200000\n\
         example : AverCert.WasmSlice.codeEntryForExport LargeBytes.modBytes LargeBytes.modLen [97, 100, 100, 84, 119, 111] = some {} := rfl\n",
        render_list(&flipped)
    );
    std::fs::write(cert.join("LargePinBad.lean"), negative).unwrap();
    let bad_pin = lake_for_cert(&cert)
        .current_dir(&cert)
        .args(["env", "lean", "LargePinBad.lean"])
        .output()
        .expect("elaborate flipped-byte negative control");
    let bad_output = format!(
        "{}{}",
        String::from_utf8_lossy(&bad_pin.stdout),
        String::from_utf8_lossy(&bad_pin.stderr)
    );
    assert!(
        !bad_pin.status.success() && bad_output.contains("rfl"),
        "one-byte-flipped expected code entry must fail rfl:\n{bad_output}"
    );

    let _ = std::fs::remove_dir_all(out_dir);
}

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
    let verify_hint = format!(
        "verify: aver cert verify {} {}",
        out_dir.join("json.wasm").display(),
        out_dir.join("cert").display()
    );
    assert!(
        compile_report.contains(&verify_hint),
        "compile should print a copyable verifier command:\n{compile_report}"
    );

    let wasm = out_dir.join("json.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(ok, "expected clean json certificate to verify:\n{report}");
    assert!(
        report.contains("12 checked exports"),
        "json should certify the widened data-segment functions:\n{report}"
    );

    let dir = temp_dir("cert-json-data-tamper");
    copy_dir(&out_dir, &dir);
    let w = dir.join("json.wasm");
    let mut bytes = std::fs::read(&w).unwrap();
    // i32.const 0; i32.const 0; array.new_data type19 seg11 (the empty string
    // literal). Changing the length operand to 1 violates the decoder's
    // fail-closed data-segment guard while keeping the module parseable.
    let pat = [0x41, 0x00, 0x41, 0x00, 0xfb, 0x09, 0x13, 0x0b];
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

    let (ok, out) = aver_check(&w, &dir.join("cert"));
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

// Plans.lean-authority soundness gates.
//
// These tests share one baseline artifact and differ only in which single
// tamper they apply to the emitted certificate package before demanding a
// DECLINE. They used to be one test that ran every verification sequentially.
// Each of these tampers edits Lean sources, so the decline only surfaces after
// a full certificate verification (minutes on CI), while the shared
// `aver compile --certify` baseline costs a fraction of a second. Splitting the
// tamper vectors into separate tests behind the `cert_plans_authority_` prefix
// — each redoing the cheap setup — lets CI run the expensive verifications in
// parallel lanes, the same way `cert_certify_spec.rs` runs its
// `cert_hostile_model_` family. Prefix in, prefix out: the dedicated lanes
// select this prefix and the `rest` lanes exclude exactly it, so a gate added
// here is run exactly once and needs no workflow edit.

/// Compiles the shared Plans.lean-authority baseline package.
///
/// Every gate below runs this itself rather than depending on a baseline built
/// by another test (and therefore another CI lane), so each one fails on its
/// own terms. The compile is a fraction of a second, so duplicating the setup
/// per gate is nearly free — unlike the verification each gate then performs.
///
/// The honest package's own verdict is asserted once, by
/// `cert_plans_authority_accepts_clean_certificate_and_pins_public_plan_data`,
/// rather than per gate: that keeps the number of full verifications the same
/// as before the split. Every tamper gate additionally pins the REASON its
/// tamper is declined, so a fixture that stopped verifying for an unrelated
/// reason surfaces as a wrong-reason failure rather than as a vacuous pass.
///
/// Returns `None` when `lake` is unavailable; the caller then skips, as before.
fn plans_authority_baseline(prefix: &str) -> Option<PathBuf> {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping Plans.lean authority test: `lake` not available");
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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify goals failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    Some(out_dir)
}

/// The honest goals package verifies, and `Plans.lean` is its only public plan
/// DATA: no checker-generated `ArtifactBytes.lean`, no fragment sidecars, and
/// no plan metadata leaking into the public manifest.
#[test]
fn cert_plans_authority_accepts_clean_certificate_and_pins_public_plan_data() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority") else {
        return;
    };

    let wasm = out_dir.join("cert_goals.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(ok, "expected clean goals certificate to verify:\n{report}");
    assert!(
        cert.join("Plans.lean").is_file(),
        "Plans.lean is public plan DATA"
    );
    assert!(
        !cert.join("ArtifactBytes.lean").exists(),
        "ArtifactBytes.lean is checker-generated, not package DATA"
    );
    assert!(
        !cert.join("fragments").exists(),
        "the public package must not duplicate Plans.lean as sidecars"
    );
    let public_manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(cert.join("cert-manifest.json")).unwrap())
            .unwrap();
    for entry in public_manifest["certified"].as_array().unwrap() {
        assert!(entry.get("fragment").is_none());
        assert!(entry.get("source_fragment").is_none());
        assert!(entry.get("plan_sha256").is_none());
    }

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A cert-supplied `ArtifactBytes.lean` is a checker-owned filename, so a
/// package that ships a decoy under that name must still be ACCEPTED with the
/// decoy ignored — the verifier regenerates it from the artifact bytes it read.
#[test]
fn cert_plans_authority_ignores_cert_supplied_artifact_bytes_decoy() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-artifact-bytes-decoy")
    else {
        return;
    };

    // The honest package does not carry ArtifactBytes, but an adversarial
    // package may add a decoy. The verifier must still generate the module from
    // the artifact bytes it read and ignore this checker-owned filename.
    let artifact_bytes_decoy_dir = temp_dir("cert-expr-artifact-bytes-decoy");
    copy_dir(&out_dir, &artifact_bytes_decoy_dir);
    std::fs::write(
        artifact_bytes_decoy_dir.join("cert/ArtifactBytes.lean"),
        "namespace AverCert.ArtifactBytes\n\ndef modBytes : Nat := 0\ndef modLen : Nat := 0\n\nend AverCert.ArtifactBytes\n",
    )
    .unwrap();
    let (ok, out) = aver_check(
        &artifact_bytes_decoy_dir.join("cert_goals.wasm"),
        &artifact_bytes_decoy_dir.join("cert"),
    );
    assert!(
        ok,
        "cert-supplied ArtifactBytes.lean must be ignored and regenerated:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&artifact_bytes_decoy_dir);
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// The expr-fragment acceptance path pins the carrier scratch local declared by
/// the canonical byte lowering, so a code table claiming zero locals is
/// DECLINED even with honest bytes and an honest plan.
#[test]
fn cert_plans_authority_declines_zero_locals_expr_fragment_code() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-zero-locals") else {
        return;
    };

    // Honest bytes and plan, but the standalone obligation code table claims
    // zero locals. The expr-fragment acceptance path must pin the one carrier
    // scratch local declared by the canonical byte lowering.
    {
        let dir = temp_dir("cert-expr-zero-locals");
        copy_dir(&out_dir, &dir);
        set_named_code_nlocals_to_zero(&dir.join("cert/Module.lean"), "addTwo", 1, 1);
        let (ok, report) = aver_check(&dir.join("cert_goals.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "expr-fragment zero-locals code must be DECLINED:\n{report}"
        );
        // Pin WHY it declined, like every sibling gate here. On its own lane a
        // bare `!ok` would also be satisfied by a fixture that stopped building
        // for an unrelated reason, which would retire this gate silently while
        // the test still passed. In the monolith the shared clean check ahead
        // of this block ruled that out; standing alone, it has to say so itself.
        assert!(
            !report.contains("CERTIFIED"),
            "zero-locals tamper must not report any certified export:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// An expr-fragment claim whose obligation is not carried by the manifest must
/// be DECLINED: emptying the manifest obligation list and re-proving the
/// weakened `Final.cert` must not buy acceptance.
#[test]
fn cert_plans_authority_declines_claim_without_manifest_obligation() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-claim-without-obligation")
    else {
        return;
    };

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
    let (ok, out) = aver_check(
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
            || out.contains("closureIsolation")
            || out.contains("closureClaim")
            || out.contains("AverCert.Artifact.certificate")
            || out.contains("Artifact.lean"),
        "wrong reason for missing manifest obligation:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "expr-fragment claim without manifest obligation credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&claim_without_manifest_ob_dir);
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A claim obligation that is no longer structurally the manifest's obligation
/// — same name, host table wrapped so it differs — must be DECLINED.
#[test]
fn cert_plans_authority_declines_artifact_claim_obligation_tamper() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-artifact-obligation-tamper")
    else {
        return;
    };

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
        "obligation := {{ {base} with host := fun add sub mul stringEq stringConcat toIndex fn => if fn = {base}.self + 999999 then none else {base}.host add sub mul stringEq stringConcat toIndex fn }}"
    );
    let tampered_artifact = artifact_text.replacen(&original, &tampered, 1);
    assert_ne!(
        artifact_text, tampered_artifact,
        "Artifact.lean claim obligation shape changed"
    );
    std::fs::write(&artifact_lean, tampered_artifact).unwrap();
    let (ok, out) = aver_check(
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

    let _ = std::fs::remove_dir_all(&artifact_obligation_tamper_dir);
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A package that bridges its acceptance with a carried `axiom` must be
/// DECLINED by the axiom whitelist, naming the offending axiom.
#[test]
fn cert_plans_authority_declines_artifact_carried_axiom_bridge() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-artifact-axiom-tamper")
    else {
        return;
    };

    let artifact_axiom_tamper_dir = temp_dir("cert-expr-artifact-axiom-tamper");
    copy_dir(&out_dir, &artifact_axiom_tamper_dir);
    let artifact_axiom_tamper_wasm = artifact_axiom_tamper_dir.join("cert_goals.wasm");
    let artifact_axiom_tamper_cert = artifact_axiom_tamper_dir.join("cert");
    let artifact_lean = artifact_axiom_tamper_cert.join("Artifact.lean");
    let artifact_text = std::fs::read_to_string(&artifact_lean).unwrap();
    let def_start = artifact_text
        .find("theorem acceptedWithFinal")
        .or_else(|| artifact_text.find("def acceptedWithFinal"))
        .expect("Artifact.lean should declare acceptedWithFinal");
    let end_marker = "\n\n/-! ### Artifact semantic side conditions consumed by AcceptanceSoundness.accept_sound -/";
    let def_end = artifact_text
        .find(end_marker)
        .expect("Artifact.lean should render accept-sound side conditions after acceptedWithFinal");
    let evil_bridge = concat!(
        "axiom artifactEvil : ∀ (finalCert : AverCert.Schema.Holds AverCert.manifest), ",
        "AverCert.AcceptedArtifact.accepted data\n\n",
        "theorem acceptedWithFinal\n",
        "    (finalCert : AverCert.Schema.Holds AverCert.manifest) :\n",
        "    AverCert.AcceptedArtifact.accepted data := artifactEvil finalCert\n\n",
    );
    let mut tampered_artifact = String::new();
    tampered_artifact.push_str(&artifact_text[..def_start]);
    tampered_artifact.push_str(evil_bridge);
    tampered_artifact.push_str(&artifact_text[def_end..]);
    std::fs::write(&artifact_lean, tampered_artifact).unwrap();
    let (ok, out) = aver_check(&artifact_axiom_tamper_wasm, &artifact_axiom_tamper_cert);
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

    let _ = std::fs::remove_dir_all(&artifact_axiom_tamper_dir);
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// `Plans.lean` is the authoritative plan DATA, so reordering a raw plan's
/// operands there must be DECLINED against the module bytes.
#[test]
fn cert_plans_authority_declines_tampered_lean_raw_plan() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-lean-plan-tamper") else {
        return;
    };

    let lean_plan_tamper_dir = temp_dir("cert-expr-lean-plan-tamper");
    copy_dir(&out_dir, &lean_plan_tamper_dir);
    let lean_plan_tamper_wasm = lean_plan_tamper_dir.join("cert_goals.wasm");
    let lean_plan_tamper_cert = lean_plan_tamper_dir.join("cert");
    let plans_lean = lean_plan_tamper_cert.join("Plans.lean");
    let plans_text = std::fs::read_to_string(&plans_lean).unwrap();
    let tampered_plans_text = plans_text.replacen(".f64Le [0, 1]", ".f64Le [1, 0]", 1);
    assert_ne!(
        plans_text, tampered_plans_text,
        "Plans.lean floatLeGoal shape changed"
    );
    std::fs::write(&plans_lean, tampered_plans_text).unwrap();

    let (ok, out) = aver_check(&lean_plan_tamper_wasm, &lean_plan_tamper_cert);
    assert!(!ok, "tampered Lean RawPlan data must be DECLINED:\n{out}");
    let old_body_pin_failed =
        out.contains("PlanLower.lowerExprFragmentBody") && out.contains("floatLeGoalCode");
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

    let _ = std::fs::remove_dir_all(&lean_plan_tamper_dir);
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// The code-entry byte pin in `Plans.lean` is checked against the module, so a
/// single flipped opcode byte in that pin must be DECLINED.
#[test]
fn cert_plans_authority_declines_tampered_code_entry_byte_pin() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-lean-bytes-tamper") else {
        return;
    };

    let lean_bytes_tamper_dir = temp_dir("cert-expr-lean-bytes-tamper");
    copy_dir(&out_dir, &lean_bytes_tamper_dir);
    let lean_bytes_tamper_wasm = lean_bytes_tamper_dir.join("cert_goals.wasm");
    let lean_bytes_tamper_cert = lean_bytes_tamper_dir.join("cert");
    let plans_lean = lean_bytes_tamper_cert.join("Plans.lean");
    let plans_text = std::fs::read_to_string(&plans_lean).unwrap();
    let honest_bytes =
        "some [18, 1, 1, 99, 23, 32, 0, 32, 1, 101, 4, 127, 65, 1, 5, 65, 0, 11, 11]";
    let tampered_bytes =
        "some [18, 1, 1, 99, 23, 32, 0, 32, 1, 102, 4, 127, 65, 1, 5, 65, 0, 11, 11]";
    assert!(
        plans_text.contains(honest_bytes),
        "Plans.lean floatLeGoal byte pin changed"
    );
    std::fs::write(
        &plans_lean,
        plans_text.replacen(honest_bytes, tampered_bytes, 1),
    )
    .unwrap();

    let (ok, out) = aver_check(&lean_bytes_tamper_wasm, &lean_bytes_tamper_cert);
    assert!(
        !ok,
        "tampered Lean code-entry byte pin must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("PlanBytes.lowerExprFragmentCodeEntry") && out.contains("floatLeGoalPlan"),
        "wrong reason for Lean code-entry byte pin tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered Lean code-entry byte pin credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&lean_bytes_tamper_dir);
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// The `WasmSlice` byte-origin pin ties the plan's bytes to their position in
/// the module, so flipping a byte in that exact-slice argument must be
/// DECLINED.
#[test]
fn cert_plans_authority_declines_tampered_wasm_slice_byte_origin_pin() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-lean-slice-tamper") else {
        return;
    };

    let lean_slice_tamper_dir = temp_dir("cert-expr-lean-slice-tamper");
    copy_dir(&out_dir, &lean_slice_tamper_dir);
    let lean_slice_tamper_wasm = lean_slice_tamper_dir.join("cert_goals.wasm");
    let lean_slice_tamper_cert = lean_slice_tamper_dir.join("cert");
    let plans_lean = lean_slice_tamper_cert.join("Plans.lean");
    let plans_text = std::fs::read_to_string(&plans_lean).unwrap();
    let honest_exact_arg =
        "[18, 1, 1, 99, 23, 32, 0, 32, 1, 101, 4, 127, 65, 1, 5, 65, 0, 11, 11] =\n";
    let slice_tampered_exact_arg =
        "[18, 1, 1, 99, 23, 32, 0, 32, 1, 102, 4, 127, 65, 1, 5, 65, 0, 11, 11] =\n";
    assert!(
        plans_text.contains(honest_exact_arg),
        "Plans.lean should contain the exact WasmSlice floatLeGoal byte pin"
    );
    let tampered_exact = plans_text.replacen(honest_exact_arg, slice_tampered_exact_arg, 1);
    std::fs::write(&plans_lean, tampered_exact).unwrap();

    let (ok, out) = aver_check(&lean_slice_tamper_wasm, &lean_slice_tamper_cert);
    assert!(
        !ok,
        "tampered Lean WasmSlice byte-origin pin must be DECLINED:\n{out}"
    );
    // A false `rfl` over the full `ArtifactBytes.modBytes` numeral can fail
    // either as a normal `WasmSlice.exactFuncBindingForExport` type mismatch or as a
    // Lean stack overflow while reducing the huge byte list. Both are
    // fail-closed build failures for this untrusted emitted audit example.
    assert!(
        out.contains("WasmSlice.exactFuncBindingForExport")
            || (out.contains("Plans") && out.contains("Stack overflow")),
        "wrong reason for Lean WasmSlice byte-origin pin tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered Lean WasmSlice byte-origin pin credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&lean_slice_tamper_dir);
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// Byte-derived host-role indices for the goals module, read from the emitted
/// `addTwoPlan` in `Plans.lean` (`.hostCall .box N`,
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

#[test]
fn cert_verify_declines_host_role_relabel_in_plans_lean() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping Plans.lean host-role relabel test: `lake` not available");
        return;
    }

    let (out_dir, wasm, cert) = compile_cert_goals("cert-expr-host-role-swap");
    let (box_idx, add_idx) = add_two_host_indices(&cert);
    assert_ne!(
        box_idx, add_idx,
        "goals module should have distinct host roles"
    );
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let honest = format!(".hostCall .add {add_idx} [0, 2]");
    let relabeled = format!(".hostCall .box {add_idx} [0, 2]");
    assert!(text.contains(&honest), "addTwo raw plan shape changed");
    std::fs::write(&plans, text.replacen(&honest, &relabeled, 1)).unwrap();

    let (ok, out) = aver_check(&wasm, &cert);
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        !ok,
        "host-role-relabeled Plans.lean must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "wrong reason for addTwo role relabel:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "host-role-relabeled expr-fragment plan credited:\n{out}"
    );
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

    let (ok, out) = aver_check(&wasm, &cert);
    let _ = std::fs::remove_dir_all(&out_dir);
    assert!(
        !ok,
        "bad Bool01 raw plan must be DECLINED:
{out}"
    );
    // PlanCheck rejects the ill-typed Bool01 node (`constI32` is inferred
    // `rawI32`; `sameTy` fails against the declared `boolI32`), so the
    // Plans.lean examples fail to elaborate. The verify report keeps only the
    // tail of the lake output (`tail(.., 20)` in the standalone verifier), so the earliest
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
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(
        ok,
        "expected clean stringeq certificate to verify:\n{report}"
    );
    assert!(
        report.contains("2 checked exports"),
        "stringeq should certify quoteOrSelf plus bump:\n{report}"
    );
    assert!(
        report.contains("quoteOrSelf  policy: simulatesModel  class: String.eq leaf"),
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

        let (ok, out) = aver_check(&dir.join("stringeq.wasm"), &dir.join("cert"));
        let _ = std::fs::remove_dir_all(&dir);
        assert!(!ok, "deleted String.eq contract must be DECLINED:\n{out}");
        assert!(
            out.contains("does not bind") || out.contains("did not build"),
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

    let (ok, out) = aver_check(&w, &dir.join("cert"));
    let _ = std::fs::remove_dir_all(&out_dir);
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        !ok,
        "tampered String.eq helper shape must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
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
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(
        ok,
        "expected clean stringconcat certificate to verify:\n{report}"
    );

    // Honest bytes and plan, zero locals in the obligation only: String.concat
    // canonically declares one carrier scratch local.
    {
        let dir = temp_dir("cert-stringconcat-zero-locals");
        copy_dir(&out_dir, &dir);
        set_named_code_nlocals_to_zero(&dir.join("cert/Module.lean"), "shout", 1, 1);
        let (ok, report) = aver_check(&dir.join("stringconcat.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "String.concat zero-locals code must be DECLINED:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }
    assert!(
        report.contains("2 checked exports"),
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
    assert!(shout_entry.get("fragment").is_none());
    assert!(shout_entry.get("source_fragment").is_none());

    {
        let dir = temp_dir("cert-stringconcat-source-plan-tamper");
        copy_dir(&out_dir, &dir);
        let plans = dir.join("cert/Plans.lean");
        let plan_text = std::fs::read_to_string(&plans).unwrap();
        let tampered_plan =
            plan_text.replacen(".constStringBytes [33]", ".constStringBytes [63]", 1);
        assert_ne!(
            plan_text, tampered_plan,
            "String.concat SymPlan DATA shape changed"
        );
        std::fs::write(&plans, &tampered_plan).unwrap();

        let (ok, out) = aver_check(&dir.join("stringconcat.wasm"), &dir.join("cert"));
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            !ok,
            "tampered String.concat SymPlan DATA must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("did not build") || out.contains("does not bind"),
            "wrong reason for String.concat SymPlan DATA tamper:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "tampered String.concat SymPlan DATA credited:\n{out}"
        );
    }

    {
        let dir = temp_dir("cert-stringconcat-plan-tamper");
        copy_dir(&out_dir, &dir);
        let plans = dir.join("cert/Plans.lean");
        let plan_text = std::fs::read_to_string(&plans).unwrap();
        let tampered_plan = plan_text.replacen("bytes := [33]", "bytes := [63]", 1);
        assert_ne!(
            plan_text, tampered_plan,
            "String.concat target plan DATA shape changed"
        );
        std::fs::write(&plans, &tampered_plan).unwrap();

        let (ok, out) = aver_check(&dir.join("stringconcat.wasm"), &dir.join("cert"));
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            !ok,
            "tampered String.concat target plan DATA must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("did not build") || out.contains("does not bind"),
            "wrong reason for String.concat target plan DATA tamper:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "tampered String.concat target plan DATA credited:\n{out}"
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

        let (ok, out) = aver_check(&dir.join("stringconcat.wasm"), &dir.join("cert"));
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            !ok,
            "deleted String.concat contract must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("does not bind") || out.contains("did not build"),
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

    let (ok, out) = aver_check(&w, &dir.join("cert"));
    let _ = std::fs::remove_dir_all(&out_dir);
    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        !ok,
        "tampered String.concat helper shape must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
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

    let (checked, check_out) = aver_check(&out_dir.join("certempty.wasm"), &out_dir.join("cert"));
    assert!(
        !checked,
        "empty cert preflight must exit nonzero:\n{check_out}"
    );
    assert!(
        check_out.contains("NO CHECKED EXPORTS") && !check_out.contains("CERTIFIED"),
        "empty cert preflight must not emit a certification verdict:\n{check_out}"
    );

    // `explain` and its `inspect` alias normalize to the same strict route in
    // the standalone verifier. That routing contract has a cheap binary unit
    // test; do not repeat two full fresh-environment replays here.

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
        let (ok, out) = aver_check(&dir.join("certempty.wasm"), &dir.join("cert"));
        assert!(!ok, "A5 injection payload must fail:\n{out}");
        assert!(
            out.contains("printable ASCII"),
            "wrong reason (A5 manifest):\n{out}"
        );
        // The character-set diagnostic echoes the rejected value; the property is that
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
        let (ok, out) = aver_check(&dir.join("certempty.wasm"), &dir.join("cert"));
        assert!(!ok, "A5 JSON-only payload must fail:\n{out}");
        assert!(
            out.contains("printable ASCII"),
            "wrong reason (A5 json):\n{out}"
        );
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
            "name": "withdrawAll",
            "class": "straight-line",
            "policy": "simulatesModel",
            "level": "L1",
            "dom": "List Int",
            "cod": "Int"
        }));
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
    let (ok, out) = aver_check(&out_dir.join("certempty.wasm"), &out_dir.join("cert"));
    assert!(!ok, "padded empty cert must still exit nonzero:\n{out}");
    assert!(
        out.contains("does not bind") && !out.contains("CERTIFIED"),
        "padded JSON must be DECLINED, not credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// The manifest `dom`/`cod` strings are declared display metadata: no
/// checker-witness line pins them, so editing them cannot fail verification.
/// Precisely because they are unpinned, the trusted CHECKED/CERTIFIED report
/// must never echo them. Sentinels planted in `cert-manifest.json` must leave
/// the trusted check green, stay out of the complete trusted report output,
/// and surface in `explain` only on the line explicitly labeled as
/// manifest-declared and not kernel-pinned.
#[test]
fn unpinned_manifest_dom_cod_never_reach_the_trusted_report() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping unpinned manifest-face report test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-unpinned-manifest-face");
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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let wasm = out_dir.join("certprobe.wasm");
    let cert = out_dir.join("cert");

    // Sentinels that cannot collide with any real report text. They stay
    // printable ASCII on purpose: the charset gate must keep admitting them so
    // the test exercises the report boundary, not the input gate.
    const DOM_SENTINEL: &str = "TAMPERED_DOM_SENTINEL";
    const COD_SENTINEL: &str = "TAMPERED_COD_SENTINEL";
    let mf = cert.join("cert-manifest.json");
    let mut manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    let certified = manifest["certified"].as_array_mut().unwrap();
    assert!(
        !certified.is_empty(),
        "fixture must certify at least one export"
    );
    for entry in certified.iter_mut() {
        entry["dom"] = serde_json::Value::String(DOM_SENTINEL.into());
        entry["cod"] = serde_json::Value::String(COD_SENTINEL.into());
    }
    std::fs::write(&mf, serde_json::to_string_pretty(&manifest).unwrap()).unwrap();

    // Unpinned by design: only the JSON display strings changed, so the same
    // trusted check the tamper cases use must still pass.
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(
        ok,
        "editing the unpinned dom/cod strings must not fail the trusted check:\n{report}"
    );
    assert!(
        report.contains("CHECKED") && report.contains("addTwo") && report.contains("class: "),
        "trusted report lost its verdict or kernel-pinned face line:\n{report}"
    );
    // The complete end-to-end trusted output — verdict, summary, and every
    // per-export face line — must never echo the unpinned manifest strings.
    assert!(
        !report.contains(DOM_SENTINEL) && !report.contains(COD_SENTINEL),
        "trusted report echoed an unpinned manifest dom/cod string:\n{report}"
    );

    // `explain` shows the declared face, but only under the explicit
    // manifest-declared label; the sentinels must never leak anywhere else.
    let (ok, explain) = aver_cert(&["explain"], &wasm, &cert);
    assert!(
        ok,
        "explain must accept the certificate with edited dom/cod:\n{explain}"
    );
    assert!(
        explain.contains(DOM_SENTINEL) && explain.contains(COD_SENTINEL),
        "explain must still show the declared manifest face:\n{explain}"
    );
    for line in explain.lines() {
        if line.contains(DOM_SENTINEL) || line.contains(COD_SENTINEL) {
            assert!(
                line.contains("manifest face (declared, not kernel-pinned)"),
                "dom/cod escaped the labeled manifest-face line:\n{line}\n\nfull explain output:\n{explain}"
            );
        }
    }

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

    let (ok, out) = aver_check(&out_dir.join("user_record.wasm"), &out_dir.join("cert"));
    assert!(!ok, "mutated ADT witness body must be DECLINED:\n{out}");
    // The acceptance predicate now pins the locals count exactly, so this
    // mutation can trip either the shipped artifact's own acceptance `rfl`
    // during the lake build ("did not build") or the later checker-witness
    // code binding ("does not bind"). Both are the same fail-closed decline;
    // the earlier stage is the stronger constraint.
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
        "wrong reason:\n{out}"
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

    let (ok, out) = aver_check(&out_dir.join("signalgauge.wasm"), &out_dir.join("cert"));
    assert!(
        !ok,
        "mutated dispatch witness body must be DECLINED:\n{out}"
    );
    // `gauge` now carries an `int-dispatch-v1` claim whose acceptance pins the
    // code table's locals count to the canonical byte-derived value, so the
    // mutation is caught one stage earlier — the shipped `Artifact.lean`
    // acceptance `rfl` fails during the checker's `lake build` ("did not
    // build") rather than at the later kernel-witness code binding ("does not
    // bind"). Either is a fail-closed decline; accept both so the assertion
    // tracks the tamper being rejected, not which in-kernel gate rejects it.
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
        "wrong reason:\n{out}"
    );
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
    // green, so the Lean whole-table code binding to actual bytes catches the
    // decoupling. This is
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

    let (ok, out) = aver_check(&out_dir.join("compose.wasm"), &out_dir.join("cert"));
    assert!(
        !ok,
        "mutated composition callee entry must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
        "wrong reason:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered composition cert must not verify:\n{out}"
    );
}

/// Orphan-member coverage tamper: drop the `hex16` composition CLAIM from the
/// emitted certificate while leaving `hex16` in `compositionMembers` (and thus
/// in `manifest.compositionPlans`). `hex16` is then a member reachable from no
/// claimed root — an unconstrained entry that `compositionNamedMembersAccepted`
/// never byte-checks. The `compositionMembersCovered` coverage conjunct in
/// `acceptedCompositionFragments` requires every member to be named by some
/// root, so the cert's own `acceptedWithFinal` proof fails to build; DECLINED.
#[test]
fn composition_orphan_member_is_declined() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping composition orphan-member test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-compose-orphan");

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

    // Drop only the `hex16` composition claim; `hex16` remains an orphan member
    // of `compositionMembers`. Every other artifact surface is untouched, so
    // the ONLY failing conjunct is member coverage.
    let a = out_dir.join("cert").join("Artifact.lean");
    let src = std::fs::read_to_string(&a).unwrap();
    let hex16_claim = ",\n  ({ exportName := \"hex16\", carrier := 2, hostTable := [(.add, 9)], memberNames := [\"double\", \"quad\", \"hex16\"], obligation := AverCert.hex16Ob } : AverCert.AcceptedArtifact.CompositionClaim)";
    assert!(
        src.contains(hex16_claim),
        "emitted compositionClaims shape changed; update the orphan test"
    );
    std::fs::write(&a, src.replacen(hex16_claim, "", 1)).unwrap();

    let (ok, out) = aver_check(&out_dir.join("compose.wasm"), &out_dir.join("cert"));
    assert!(!ok, "orphan composition member must be DECLINED:\n{out}");
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
        "wrong reason:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "orphan-member composition cert must not verify:\n{out}"
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}

fn run_manifest_obligation_guard_iso(prefix: &str, lean: &str) {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping manifest-obligation GuardIso test: `lake` not available");
        return;
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
        .expect("compile cert_goals fixture for manifest-obligation GuardIso");
    assert!(
        compile.status.success(),
        "cert_goals compile failed for manifest-obligation GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let build = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build cert_goals certificate before manifest-obligation GuardIso");
    assert!(
        build.status.success(),
        "cert_goals certificate failed before manifest-obligation GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let check = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .output()
        .expect("run manifest-obligation GuardIso");
    assert!(
        check.status.success(),
        "manifest-obligation GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
}

/// The old acceptance surface did not constrain the whole host builder. An
/// obligation could therefore trap at its first host call and satisfy partial
/// correctness vacuously. The standard face must be the single guard that
/// rejects that otherwise accepted artifact.
#[test]
fn standard_face_host_guard_is_isolating() {
    run_manifest_obligation_guard_iso(
        "cert-standard-face-host-guard-iso",
        include_str!("fixtures/cert_standard_face_guard_iso.lean"),
    );
}

/// An otherwise valid artifact with one extra manifest obligation is rejected
/// only by `manifestObligationsClaimed`; the literal one-conjunct-weakened copy
/// accepts it, while every byte-derived binding and code entry stays identical.
#[test]
fn manifest_unclaimed_obligation_guard_is_isolating() {
    let lean = r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

theorem claimObligationsInManifest_append
    (manifestObligations extras claims : List Obligation)
    (h : AcceptedArtifact.claimObligationsInManifest manifestObligations claims) :
    AcceptedArtifact.claimObligationsInManifest
      (manifestObligations ++ extras) claims := by
  induction claims with
  | nil => trivial
  | cons obligation rest ih =>
      rcases h with ⟨hfind, hrest⟩
      constructor
      · simpa [List.find?_append, hfind]
      · exact ih hrest

def acceptedCompositionWithoutClaimCoverage
    (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.compositionClaimsAccepted artifact.modBytes artifact.modLen
      artifact.compositionMembers artifact.compositionClaims ∧
    AcceptedArtifact.compositionMembersCovered artifact.compositionMembers
      artifact.compositionClaims = true ∧
    AcceptedArtifact.manifestObligationExportsUnique artifact = true

def acceptedFragmentsWithoutClaimCoverage
    (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.acceptedSymFragments artifact ∧
  AcceptedArtifact.acceptedStringEqFragments artifact ∧
  AcceptedArtifact.acceptedStringConcatFragments artifact ∧
  AcceptedArtifact.acceptedConstructFragments artifact ∧
  AcceptedArtifact.acceptedRecursionFragments artifact ∧
  AcceptedArtifact.acceptedMutualRecursionFragments artifact ∧
  AcceptedArtifact.acceptedVerbatimFragments artifact ∧
  AcceptedArtifact.acceptedIntDispatchFragments artifact ∧
  AcceptedArtifact.acceptedFieldProjectionFragments artifact ∧
  acceptedCompositionWithoutClaimCoverage artifact

def acceptedWithoutClaimCoverage
    (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  Schema.Holds artifact.manifest ∧
  AcceptedArtifact.subjectMatchesArtifactRoot artifact ∧
  AcceptedArtifact.fragmentClaimObligationsInManifest artifact ∧
  AcceptedArtifact.claimsMatchManifest artifact ∧
  AverCert.StandardFace.checkedFaces artifact ∧
  AverCert.ClaimAxes.checked artifact = true ∧
  AcceptedArtifact.decodedNonExprFacts artifact ∧
  acceptedFragmentsWithoutClaimCoverage artifact

def unclaimedOb : Obligation :=
  { AverCert.addTwoOb with export_ := "unclaimedAddTwo" }

def unclaimedManifest : Manifest :=
  { AverCert.manifest with
      obligations := AverCert.manifest.obligations ++ [unclaimedOb] }

def unclaimedArtifact : AcceptedArtifact.ArtifactData :=
  { Artifact.data with manifest := unclaimedManifest }

theorem unclaimedFinal : Schema.Holds unclaimedManifest := by
  refine ⟨Final.cert.1, ?_⟩
  intro o ho
  have ho' : o ∈ AverCert.manifest.obligations ∨ o = unclaimedOb := by
    simpa [unclaimedManifest] using ho
  rcases ho' with ho | rfl
  · exact Final.cert.2 o ho
  · have hadd := Final.cert.2 AverCert.addTwoOb (by simp [AverCert.manifest])
    simpa [unclaimedOb, Obligation.holds] using hadd

example : AcceptedArtifact.manifestObligationsClaimed unclaimedArtifact = false := rfl
example : AcceptedArtifact.manifestObligationExportsUnique unclaimedArtifact = true := rfl

example : ∀ nameBytes,
    WasmSlice.funcBindingForExport unclaimedArtifact.modBytes unclaimedArtifact.modLen nameBytes =
      WasmSlice.funcBindingForExport Artifact.data.modBytes Artifact.data.modLen nameBytes := by
  intro nameBytes
  rfl

example : ∀ nameBytes,
    WasmSlice.codeEntryForExport unclaimedArtifact.modBytes unclaimedArtifact.modLen nameBytes =
      WasmSlice.codeEntryForExport Artifact.data.modBytes Artifact.data.modLen nameBytes := by
  intro nameBytes
  rfl

example : ¬ AcceptedArtifact.accepted unclaimedArtifact := by
  intro h
  rcases h with ⟨_, _, _, _, _, _, _, hfragments⟩
  rcases hfragments with ⟨_, _, _, _, _, _, _, _, _, hcomposition, _⟩
  have hclaimed := hcomposition.2.2.1
  change false = true at hclaimed
  contradiction

example : acceptedWithoutClaimCoverage unclaimedArtifact := by
  rcases Artifact.certificate with
    ⟨_, hsubject, hobs, hmatch, hfaces, haxes, hdecoded, hsym, hstringEq, hstringConcat, hconstruct,
      hrecursion, hmutual, hverbatim, hintDispatch, hfieldProjection,
      hcompositionAccepted, _⟩
  rcases hcompositionAccepted with ⟨hcomposition, hmembersCovered, _, _⟩
  refine ⟨unclaimedFinal, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · exact hsubject
  · simpa [unclaimedArtifact, unclaimedManifest,
      AcceptedArtifact.fragmentClaimObligationsInManifest,
      AcceptedArtifact.claimObligations] using
      claimObligationsInManifest_append AverCert.manifest.obligations
        [unclaimedOb] (AcceptedArtifact.claimObligations Artifact.data) hobs
  · exact hmatch
  · change AverCert.StandardFace.checkedFaces Artifact.data
    exact hfaces
  · change AverCert.ClaimAxes.checked Artifact.data = true
    exact haxes
  · exact hdecoded
  · exact hsym
  · exact hstringEq
  · exact hstringConcat
  · exact hconstruct
  · exact hrecursion
  · exact hmutual
  · exact hverbatim
  · exact hintDispatch
  · exact hfieldProjection
  · exact hcomposition
  · exact hmembersCovered
  · rfl
"#;
    run_manifest_obligation_guard_iso("cert-manifest-unclaimed-guard-iso", lean);
}

/// A second, mutated obligation behind the honest obligation with the same
/// export name is rejected only by `manifestObligationExportsUnique`; removing
/// just that conjunct accepts it without changing any byte-derived surface.
#[test]
fn manifest_duplicate_obligation_export_guard_is_isolating() {
    let lean = r#"import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option maxRecDepth 300000

theorem claimObligationsInManifest_append
    (manifestObligations extras claims : List Obligation)
    (h : AcceptedArtifact.claimObligationsInManifest manifestObligations claims) :
    AcceptedArtifact.claimObligationsInManifest
      (manifestObligations ++ extras) claims := by
  induction claims with
  | nil => trivial
  | cons obligation rest ih =>
      rcases h with ⟨hfind, hrest⟩
      constructor
      · simpa [List.find?_append, hfind]
      · exact ih hrest

def inertCode : CodeTbl := fun _ => none

def duplicateOb : Obligation :=
  { AverCert.addTwoOb with code := inertCode, self := 999 }

theorem duplicateObHolds : duplicateOb.holds := by
  intro S add sub mul stringEq stringConcat toIndex hadd hsub hmul hStringEq
    hStringConcat _hToIndex fuel x vs w hdom hrun
  cases fuel <;> simp [duplicateOb, inertCode, wFuncN] at hrun

def duplicateManifest : Manifest :=
  { AverCert.manifest with
      obligations := AverCert.manifest.obligations ++ [duplicateOb] }

def duplicateArtifact : AcceptedArtifact.ArtifactData :=
  { Artifact.data with manifest := duplicateManifest }

theorem duplicateFinal : Schema.Holds duplicateManifest := by
  refine ⟨Final.cert.1, ?_⟩
  intro o ho
  change o ∈ AverCert.manifest.obligations ++ [duplicateOb] at ho
  rcases List.mem_append.mp ho with ho | ho
  · exact Final.cert.2 o ho
  · simp only [List.mem_singleton] at ho
    subst o
    exact duplicateObHolds

def acceptedCompositionWithoutUniqueExports
    (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.compositionClaimsAccepted artifact.modBytes artifact.modLen
      artifact.compositionMembers artifact.compositionClaims ∧
    AcceptedArtifact.compositionMembersCovered artifact.compositionMembers
      artifact.compositionClaims = true ∧
    AcceptedArtifact.manifestObligationsClaimed artifact = true

def acceptedFragmentsWithoutUniqueExports
    (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  AcceptedArtifact.acceptedSymFragments artifact ∧
  AcceptedArtifact.acceptedStringEqFragments artifact ∧
  AcceptedArtifact.acceptedStringConcatFragments artifact ∧
  AcceptedArtifact.acceptedConstructFragments artifact ∧
  AcceptedArtifact.acceptedRecursionFragments artifact ∧
  AcceptedArtifact.acceptedMutualRecursionFragments artifact ∧
  AcceptedArtifact.acceptedVerbatimFragments artifact ∧
  AcceptedArtifact.acceptedIntDispatchFragments artifact ∧
  AcceptedArtifact.acceptedFieldProjectionFragments artifact ∧
  acceptedCompositionWithoutUniqueExports artifact

def acceptedWithoutUniqueExports
    (artifact : AcceptedArtifact.ArtifactData) : Prop :=
  Schema.Holds artifact.manifest ∧
  AcceptedArtifact.subjectMatchesArtifactRoot artifact ∧
  AcceptedArtifact.fragmentClaimObligationsInManifest artifact ∧
  AcceptedArtifact.claimsMatchManifest artifact ∧
  AverCert.StandardFace.checkedFaces artifact ∧
  AverCert.ClaimAxes.checked artifact = true ∧
  AcceptedArtifact.decodedNonExprFacts artifact ∧
  acceptedFragmentsWithoutUniqueExports artifact

example : AcceptedArtifact.manifestObligationsClaimed duplicateArtifact = true := rfl
example : AcceptedArtifact.manifestObligationExportsUnique duplicateArtifact = false := rfl

example : ∀ nameBytes,
    WasmSlice.funcBindingForExport duplicateArtifact.modBytes duplicateArtifact.modLen nameBytes =
      WasmSlice.funcBindingForExport Artifact.data.modBytes Artifact.data.modLen nameBytes := by
  intro nameBytes
  rfl

example : ∀ nameBytes,
    WasmSlice.codeEntryForExport duplicateArtifact.modBytes duplicateArtifact.modLen nameBytes =
      WasmSlice.codeEntryForExport Artifact.data.modBytes Artifact.data.modLen nameBytes := by
  intro nameBytes
  rfl

example : ¬ AcceptedArtifact.accepted duplicateArtifact := by
  intro h
  rcases h with ⟨_, _, _, _, _, _, _, hfragments⟩
  rcases hfragments with ⟨_, _, _, _, _, _, _, _, _, hcomposition, _⟩
  have hunique := hcomposition.2.2.2
  change false = true at hunique
  contradiction

example : acceptedWithoutUniqueExports duplicateArtifact := by
  rcases Artifact.certificate with
    ⟨_, hsubject, hobs, hmatch, hfaces, haxes, hdecoded, hsym, hstringEq, hstringConcat, hconstruct,
      hrecursion, hmutual, hverbatim, hintDispatch, hfieldProjection,
      hcompositionAccepted, _⟩
  rcases hcompositionAccepted with ⟨hcomposition, hmembersCovered, _, _⟩
  refine ⟨duplicateFinal, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · exact hsubject
  · simpa [duplicateArtifact, duplicateManifest,
      AcceptedArtifact.fragmentClaimObligationsInManifest,
      AcceptedArtifact.claimObligations] using
      claimObligationsInManifest_append AverCert.manifest.obligations
        [duplicateOb] (AcceptedArtifact.claimObligations Artifact.data) hobs
  · exact hmatch
  · change AverCert.StandardFace.checkedFaces Artifact.data
    exact hfaces
  · change AverCert.ClaimAxes.checked Artifact.data = true
    exact haxes
  · exact hdecoded
  · exact hsym
  · exact hstringEq
  · exact hstringConcat
  · exact hconstruct
  · exact hrecursion
  · exact hmutual
  · exact hverbatim
  · exact hintDispatch
  · exact hfieldProjection
  · exact hcomposition
  · exact hmembersCovered
  · rfl
"#;
    run_manifest_obligation_guard_iso("cert-manifest-duplicate-guard-iso", lean);
}

/// S5 guard isolation, including executed weaken confirmations. Each negative
/// is rejected by one named audited guard; the adjacent `weak*` definition is
/// the throwaway one-guard-removed copy and accepts exactly that negative.
#[test]
fn composition_plan_guards_are_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping composition GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-compose-guard-iso");
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
        .expect("compile composition fixture");
    assert!(
        compile.status.success(),
        "composition fixture compile failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let build = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build composition certificate before GuardIso");
    assert!(
        build.status.success(),
        "composition certificate build failed before GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    let lean = r#"import Artifact
open CertPrelude AverCert.Schema
set_option maxRecDepth 200000

def funcs : List (String × Nat) := [("double", 1), ("hex16", 3), ("quad", 2)]
def hosts : List (HostRole × Nat) := [(.add, 9)]
def badProfile : CompositionRawPlan := { profile := "composition-plan-v2", shape := .selfSum }
def missingTarget : CompositionRawPlan := { profile := "composition-plan-v1", shape := .chain ["missing"] }
def wrongFuncs : List (String × Nat) := [("double", 8), ("hex16", 3), ("quad", 2)]

-- PlanCheck profile guard: fixed rejects; one-guard-weakened copy accepts.
def weakRawCheck (_ : CompositionRawPlan) : Bool := true
example : AverCert.PlanCheck.checkCompositionRawPlan badProfile = false := rfl
example : weakRawCheck badProfile = true := rfl

-- Strict singleton-add host-table guard.
def weakHostCheck (_ : List (HostRole × Nat)) : Bool := true
example : AverCert.PlanCheck.checkCompositionHostTable [(.sub, 9)] = false := rfl
example : weakHostCheck [(.sub, 9)] = true := rfl

-- Semantic lowerer target-resolution guard.
def weakLower (_ : CompositionRawPlan) : Option (List WInstr) := some [.localGet 0]
example : AverCert.PlanLower.lowerCompositionBody hosts funcs missingTarget = none := rfl
example : weakLower missingTarget = some [.localGet 0] := rfl

-- Exact byte lowering: a wrong byte-derived name→index binding changes bytes;
-- the weakened copy deliberately returns the honest lowering.
def weakBytes (p : CompositionRawPlan) :=
  AverCert.PlanBytes.lowerCompositionCodeEntry 2 hosts funcs p
example : AverCert.PlanBytes.lowerCompositionCodeEntry 2 hosts wrongFuncs AverCert.Plans.quadCompositionPlan ≠
  AverCert.PlanBytes.lowerCompositionCodeEntry 2 hosts funcs AverCert.Plans.quadCompositionPlan := by decide
example : weakBytes AverCert.Plans.quadCompositionPlan =
  AverCert.PlanBytes.lowerCompositionCodeEntry 2 hosts funcs AverCert.Plans.quadCompositionPlan := rfl

-- Wasm export binding, code-entry equality, and exact unary carrier signature.
example : (AverCert.WasmSlice.funcBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen [113,117,97,100]).map
    (fun b => (b.funcIdx, b.codeEntry)) = some (2,
      (AverCert.PlanBytes.lowerCompositionCodeEntry 2 hosts funcs AverCert.Plans.quadCompositionPlan).get!) := rfl
example : AverCert.WasmSlice.funcTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 5 1 2 = true := rfl
example : AverCert.WasmSlice.funcTypeMatches AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 5 2 2 = false := rfl
def weakSignature (_ _ _ : Nat) : Bool := true
example : weakSignature 5 2 2 = true := rfl

-- Byte-derived transitive closure: omission, extra membership, and a cycle all
-- fail; weakening only this closure guard accepts each negative.
def weakClosure (_ : String) (_ : List String)
    (_ : List AverCert.AcceptedArtifact.CompositionMemberClaim)
    (_ : List (String × Nat)) : Bool := true
example : AverCert.AcceptedArtifact.compositionClosureBound "hex16" ["quad", "hex16"]
    AverCert.Artifact.compositionMembers funcs = false := rfl
example : AverCert.AcceptedArtifact.compositionClosureBound "quad" ["double", "quad", "hex16"]
    AverCert.Artifact.compositionMembers funcs = false := rfl
def cycleMembers : List AverCert.AcceptedArtifact.CompositionMemberClaim := [
  { exportNameBytes := [100,111,117,98,108,101], exportName := "double",
    plan := { profile := "composition-plan-v1", shape := .chain ["quad"] } },
  { exportNameBytes := [113,117,97,100], exportName := "quad", plan := AverCert.Plans.quadCompositionPlan }]
example : AverCert.AcceptedArtifact.compositionClosureBound "quad" ["double", "quad"]
    cycleMembers [("double", 1), ("quad", 2)] = false := rfl
example : weakClosure "hex16" ["quad", "hex16"] AverCert.Artifact.compositionMembers funcs = true := rfl
example : weakClosure "quad" ["double", "quad", "hex16"] AverCert.Artifact.compositionMembers funcs = true := rfl
example : weakClosure "quad" ["double", "quad"] cycleMembers [("double", 1), ("quad", 2)] = true := rfl

-- Root binding and exact canonical locals count in the shared CodeTbl.
example : AverCert.quadOb.self = 2 := rfl
example : ¬ AverCert.quadOb.self = 99 := by decide
def weakRoot (_ _ : Nat) : Bool := true
example : weakRoot AverCert.quadOb.self 99 = true := rfl
example : (AverCert.quadOb.code 1).map (fun c => c.nlocals) = some 1 := rfl
example : ¬ (AverCert.quadOb.code 1).map (fun c => c.nlocals) = some 0 := by decide
def weakLocals (_ : Option Nat) : Bool := true
example : weakLocals ((AverCert.quadOb.code 1).map (fun c => c.nlocals)) = true := rfl

-- Extensional canonical-host guard: the honest builder is definitionally
-- equal; a nowhere-defined builder differs at the byte-derived add slot.
example : AverCert.quadOb.host = AverCert.AcceptedArtifact.intDispatchCanonicalHost 2 hosts := rfl
def badHost : (List WVal → Option WVal) → (List WVal → Option WVal) →
    (List WVal → Option WVal) → (List WVal → Option WVal) →
    (Nat → List WVal → Option WVal) →
    (List WVal → Option WVal) → HostTbl := fun _ _ _ _ _ _ _ => none
def addProbe : List WVal → Option WVal := fun _ => some .null
example : (badHost addProbe addProbe addProbe addProbe (fun _ _ => none) addProbe) 9 ≠
    (AverCert.AcceptedArtifact.intDispatchCanonicalHost 2 hosts
      addProbe addProbe addProbe addProbe (fun _ _ => none) addProbe) 9 := by
  simp [badHost, addProbe, hosts, AverCert.AcceptedArtifact.intDispatchCanonicalHost,
    AverCert.AcceptedArtifact.intDispatchCanonicalSlots]
def weakHostEquality (_ _ : HostTbl) : Bool := true
example : weakHostEquality
    ((badHost addProbe addProbe addProbe addProbe (fun _ _ => none) addProbe))
    ((AverCert.AcceptedArtifact.intDispatchCanonicalHost 2 hosts
      addProbe addProbe addProbe addProbe (fun _ _ => none) addProbe)) = true := rfl

-- Manifest/claim plan-pair equality guard.
def relabeledMembers : List AverCert.AcceptedArtifact.CompositionMemberClaim :=
  [{ exportNameBytes := [100,111,117,98,108,101], exportName := "alias",
     plan := AverCert.Plans.doubleCompositionPlan }]
example : AverCert.AcceptedArtifact.compositionMemberPlanPairs relabeledMembers =
    [("alias", AverCert.Plans.doubleCompositionPlan)] := rfl
example : AverCert.AcceptedArtifact.compositionMemberPlanPairs relabeledMembers ≠
    [("double", AverCert.Plans.doubleCompositionPlan)] := by decide
def weakManifest (_ : List (String × CompositionRawPlan)) : Bool := true
example : weakManifest (AverCert.AcceptedArtifact.compositionMemberPlanPairs relabeledMembers) = true := rfl

-- Member-coverage guard: `compositionMembers` must be the union of the claimed
-- roots' reachable closures. Honest members are covered; dropping the `hex16`
-- claim leaves `hex16` an orphan member reachable from no root, so coverage
-- fails. Weakening only this guard accepts the orphan.
def orphanClaims : List AverCert.AcceptedArtifact.CompositionClaim :=
  [{ exportName := "quad", carrier := 2, hostTable := [(.add, 9)],
     memberNames := ["double", "quad"], obligation := AverCert.quadOb }]
example : AverCert.AcceptedArtifact.compositionMembersCovered
    AverCert.Artifact.compositionMembers AverCert.Artifact.compositionClaims = true := rfl
example : AverCert.AcceptedArtifact.compositionMembersCovered
    AverCert.Artifact.compositionMembers orphanClaims = false := rfl
def weakCoverage (_ : List AverCert.AcceptedArtifact.CompositionMemberClaim)
    (_ : List AverCert.AcceptedArtifact.CompositionClaim) : Bool := true
example : weakCoverage AverCert.Artifact.compositionMembers orphanClaims = true := rfl
"#;
    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let check = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .output()
        .expect("run composition GuardIso");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    assert!(
        check.status.success(),
        "composition GuardIso failed:\n{combined}"
    );
    let _ = std::fs::remove_dir_all(out_dir);
}

/// `field-projection-v1` guard isolation with executed weaken confirmations.
/// Every negative is rejected by the named fixed guard and accepted by the
/// adjacent throwaway copy with exactly that guard removed.
#[test]
fn field_projection_plan_guards_are_isolated_and_weaken_confirmed() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping field-projection GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-field-projection-guard-iso");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/tupleproj.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("compile tuple projection fixture");
    assert!(
        compile.status.success(),
        "tuple projection fixture compile failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let build = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build tuple projection certificate before GuardIso");
    assert!(
        build.status.success(),
        "tuple projection certificate build failed before GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    let lean = r#"import Artifact
open CertPrelude AverCert.Schema AverCert.WasmSlice
set_option maxRecDepth 400000

def honest : FieldProjectionRawPlan := AverCert.Plans.pairFstFieldProjectionPlan
def badProfile : FieldProjectionRawPlan := { profile := "field-projection-v2", fieldIdx := 0 }
def badField : FieldProjectionRawPlan := { profile := "field-projection-v1", fieldIdx := 1 }
def nameBytes : List Nat := [112,97,105,114,70,115,116]

-- === PREDICATE-LEVEL weakened copies of `fieldProjectionPlanAccepted` ===
-- Each drops EXACTLY one security-critical conjunct; the adversarial claim below
-- is ACCEPTED under the weakened copy but the shipped predicate rejects it at
-- exactly that conjunct. This replaces the earlier constant-`Bool` weakenings,
-- which never demonstrated acceptance against all remaining guards.

-- (d) BYTE-EQUALITY GATE dropped (the exact binding lookup is weakened to a
-- name-only `funcBindingForExport`).
def fpAccept_dropBytes (modBytes modLen : Nat) (exportNameBytes : ByteSeq)
    (exportName : String) (carrier structIdx fieldCount : Nat)
    (resultTy : FieldProjectionResultTy)
    (plan : FieldProjectionRawPlan) (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧ obligation.carrier = carrier ∧
  AverCert.PlanCheck.checkFieldProjectionRawPlan fieldCount plan = true ∧
  ∃ body codeEntry binding,
    AverCert.PlanLower.lowerFieldProjectionBody structIdx fieldCount plan = some body ∧
    AverCert.PlanBytes.lowerFieldProjectionCodeEntry carrier structIdx fieldCount resultTy plan = some codeEntry ∧
    funcBindingForExport modBytes modLen exportNameBytes = some binding ∧
    projectionStructTypeMatches modBytes modLen structIdx fieldCount plan.fieldIdx resultTy = true ∧
    projectionFuncTypeMatches modBytes modLen binding.typeIdx structIdx resultTy = true ∧
    obligation.self = binding.funcIdx ∧
    obligation.code binding.funcIdx = some { arity := 1, nlocals := 3, body := body }

-- (e) STRUCT SELECTED-FIELD-TYPE dropped (`projectionStructTypeMatches`).
def fpAccept_dropStruct (modBytes modLen : Nat) (exportNameBytes : ByteSeq)
    (exportName : String) (carrier structIdx fieldCount : Nat)
    (resultTy : FieldProjectionResultTy)
    (plan : FieldProjectionRawPlan) (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧ obligation.carrier = carrier ∧
  AverCert.PlanCheck.checkFieldProjectionRawPlan fieldCount plan = true ∧
  ∃ body codeEntry binding,
    AverCert.PlanLower.lowerFieldProjectionBody structIdx fieldCount plan = some body ∧
    AverCert.PlanBytes.lowerFieldProjectionCodeEntry carrier structIdx fieldCount resultTy plan = some codeEntry ∧
    exactFuncBindingForExport modBytes modLen exportNameBytes codeEntry = some binding ∧
    projectionFuncTypeMatches modBytes modLen binding.typeIdx structIdx resultTy = true ∧
    obligation.self = binding.funcIdx ∧
    obligation.code binding.funcIdx = some { arity := 1, nlocals := 3, body := body }

-- Complicit obligation whose code table returns the WRONG-field body, so the
-- code-table conjunct (h) is neutralized and the byte-equality gate (d) is the
-- SOLE remaining binder of the field index (the sole plan datum).
def badBodyCode : CodeTbl := fun fn =>
  if fn = 1 then some ⟨1, 3, [.localGet 0, .localSet 2, .localGet 2, .refCast 3, .structGet 3 1, .localSet 1, .localGet 1]⟩ else none
def badBodyOb : Obligation := { AverCert.pairFstOb with code := badBodyCode }

-- Struct-mutated module: flip struct 3 field 0 nullable-ref (0x63) -> non-null
-- (0x64) at module offset 31. The code section and the func-type entry are
-- byte-unchanged, so (d) and (f) still pass; only (e) reads the mutated struct
-- field type.
def structMut : Nat :=
  AverCert.ArtifactBytes.modBytes + (1 <<< (8 * 31))

-- Obligation export-name and carrier binds.
def weakExport (_ _ : String) : Bool := true
example : AverCert.pairFstOb.export_ = "pairFst" := rfl
example : ¬ AverCert.pairFstOb.export_ = "alias" := by decide
example : weakExport AverCert.pairFstOb.export_ "alias" = true := rfl
def weakCarrier (_ _ : Nat) : Bool := true
example : AverCert.pairFstOb.carrier = 2 := rfl
example : ¬ AverCert.pairFstOb.carrier = 3 := by decide
example : weakCarrier AverCert.pairFstOb.carrier 3 = true := rfl

-- Profile guard.
def weakProfile (_ : FieldProjectionRawPlan) : Bool := true
example : AverCert.PlanCheck.checkFieldProjectionRawPlan 2 badProfile = false := rfl
example : weakProfile badProfile = true := rfl

-- Byte-derived exact field-count guard.
def weakFieldCount (_ : Nat) (_ : FieldProjectionRawPlan) : Bool := true
example : AverCert.PlanCheck.checkFieldProjectionRawPlan 3 honest = false := rfl
example : weakFieldCount 3 honest = true := rfl

-- Projected-field range guard.
def outOfRange : FieldProjectionRawPlan := { profile := "field-projection-v1", fieldIdx := 2 }
def weakFieldRange (_ : Nat) (_ : FieldProjectionRawPlan) : Bool := true
example : AverCert.PlanCheck.checkFieldProjectionRawPlan 2 outOfRange = false := rfl
example : weakFieldRange 2 outOfRange = true := rfl

-- Semantic lowerer inherits the structural guard.
def weakLower (_ : FieldProjectionRawPlan) : Option (List WInstr) :=
  AverCert.PlanLower.lowerFieldProjectionBody 3 2 honest
example : AverCert.PlanLower.lowerFieldProjectionBody 3 2 badProfile = none := rfl
example : weakLower badProfile = AverCert.PlanLower.lowerFieldProjectionBody 3 2 honest := rfl

-- CANONICAL BYTE LOWERING (d) / FIELD-INDEX BINDING — genuine predicate-level
-- isolation. The field index is the sole plan datum; it reaches the ULEB
-- immediate of the code entry, so the byte-equality gate binds it. With a
-- complicit obligation (`badBodyOb`) supplying the wrong-field body, the
-- code-table conjunct (h) is neutralized, leaving (d) as the sole guard on the
-- field index: the wrong-field claim is ACCEPTED once (d) is dropped...
example : fpAccept_dropBytes AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen nameBytes "pairFst"
    2 3 2 (.nullableRef 2) badField badBodyOb :=
  ⟨rfl, rfl, rfl, _, _, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
-- ...and the shipped predicate rejects it at exactly (d): the honest module
-- code entry differs from the wrong-field canonical bytes.
example : (funcBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen nameBytes).map (fun b => b.codeEntry) ≠
  AverCert.PlanBytes.lowerFieldProjectionCodeEntry 2 3 2 (.nullableRef 2) badField := by decide
example : (exactFuncBindingForExport
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen nameBytes
    (AverCert.PlanBytes.lowerFieldProjectionCodeEntry
      2 3 2 (.nullableRef 2) honest).get!).isSome = true := rfl
example : exactFuncBindingForExport
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen nameBytes
    (AverCert.PlanBytes.lowerFieldProjectionCodeEntry
      2 3 2 (.nullableRef 2) badField).get! = none := rfl
-- FIELD-INDEX defense-in-depth: (h) is a redundant-but-defensive sibling. When
-- the obligation is the HONEST byte-derived one, its code table pins the field-0
-- body (the honest lowering), so a wrong field index also fails (h) — the field
-- index is bound by both the byte gate (d) and the code-table body (h). The
-- honest obligation commits to the field-0 body:
example : (AverCert.pairFstOb.code 1).map (fun c => c.body) =
  some ((AverCert.PlanLower.lowerFieldProjectionBody 3 2 honest).getD []) := rfl
-- and the field index is a real byte-level datum (field 0 vs 1 diverges):
example : AverCert.PlanBytes.lowerFieldProjectionCodeEntry 2 3 2 (.nullableRef 2) badField ≠
  AverCert.PlanBytes.lowerFieldProjectionCodeEntry 2 3 2 (.nullableRef 2) honest := by decide

-- Export-name/function-binding guard.
def weakBinding (_ : List Nat) :=
  AverCert.WasmSlice.funcBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
    [112,97,105,114,70,115,116]
example : AverCert.WasmSlice.funcBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
    [109,105,115,115,105,110,103] = none := rfl
example : weakBinding [109,105,115,115,105,110,103] =
  AverCert.WasmSlice.funcBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen
    [112,97,105,114,70,115,116] := rfl

-- STRUCT SELECTED-FIELD-TYPE (e) — genuine predicate-level isolation. This guard
-- cross-checks the module's actual struct field type against the claimed result
-- type; only a module whose struct field type diverges from its func signature
-- (an internally inconsistent module, unreachable via claim data alone) exhibits
-- it. `structMut` mutates struct 3 field 0 to a non-null ref, keeping the code
-- section and func-type entry byte-identical. The honest claim is ACCEPTED once
-- (e) is dropped (the byte gate (d) and signature (f) still pass over the
-- mutated module because neither reads the struct's field types)...
example : fpAccept_dropStruct structMut AverCert.ArtifactBytes.modLen nameBytes "pairFst"
    2 3 2 (.nullableRef 2) honest AverCert.pairFstOb :=
  ⟨rfl, rfl, rfl, _, _, _, rfl, rfl, rfl, rfl, rfl, rfl⟩
-- ...and the shipped predicate rejects the mutated module at exactly (e):
example : AverCert.WasmSlice.projectionStructTypeMatches structMut AverCert.ArtifactBytes.modLen 3 2 0 (.nullableRef 2) = false := rfl
-- The siblings (d) byte gate and (f) signature are provably BLIND to the struct
-- field-type mutation (they read the code and func-type sections):
example : (funcBindingForExport structMut AverCert.ArtifactBytes.modLen nameBytes).map (fun b => b.codeEntry) =
  (funcBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen nameBytes).map (fun b => b.codeEntry) := rfl
example : exactFuncBindingForExport structMut AverCert.ArtifactBytes.modLen nameBytes
    (AverCert.PlanBytes.lowerFieldProjectionCodeEntry
      2 3 2 (.nullableRef 2) honest).get! =
  exactFuncBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen nameBytes
    (AverCert.PlanBytes.lowerFieldProjectionCodeEntry
      2 3 2 (.nullableRef 2) honest).get! := rfl
example : AverCert.WasmSlice.projectionFuncTypeMatches structMut AverCert.ArtifactBytes.modLen 6 3 (.nullableRef 2) = true := rfl
-- The prior claim-data struct checks remain (structIdx / count / result ref) —
-- these are also caught by the byte gate / signature, so this is the guard's
-- redundant-but-defensive cross-check surface.
example : AverCert.WasmSlice.projectionStructTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 3 2 0 (.nullableRef 2) = true := rfl
example : AverCert.WasmSlice.projectionStructTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 4 2 0 (.nullableRef 2) = false := rfl
example : AverCert.WasmSlice.projectionStructTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 3 3 0 (.nullableRef 2) = false := rfl
example : AverCert.WasmSlice.projectionStructTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 3 2 0 (.nullableRef 1) = false := rfl

-- The exported function must be unary over the claimed struct and return the
-- selected byte-derived reference type.
def weakSignature (_ _ : Nat) (_ : FieldProjectionResultTy) : Bool := true
example : AverCert.WasmSlice.projectionFuncTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 6 3 (.nullableRef 2) = true := rfl
example : AverCert.WasmSlice.projectionFuncTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 6 4 (.nullableRef 2) = false := rfl
example : AverCert.WasmSlice.projectionFuncTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 6 3 (.nullableRef 1) = false := rfl
example : weakSignature 6 4 (.nullableRef 2) = true := rfl
example : weakSignature 6 3 (.nullableRef 1) = true := rfl

-- Obligation self and exact canonical locals count.
def weakSelf (_ _ : Nat) : Bool := true
example : AverCert.pairFstOb.self = 1 := rfl
example : ¬ AverCert.pairFstOb.self = 2 := by decide
example : weakSelf AverCert.pairFstOb.self 2 = true := rfl
def weakLocals (_ : Option Nat) : Bool := true
example : (AverCert.pairFstOb.code 1).map (fun c => c.nlocals) = some 3 := rfl
example : ¬ (AverCert.pairFstOb.code 1).map (fun c => c.nlocals) = some 0 := by decide
example : weakLocals ((AverCert.pairFstOb.code 1).map (fun c => c.nlocals)) = true := rfl

-- Manifest/claim pairing guard.
def relabeled : List AverCert.AcceptedArtifact.FieldProjectionClaim :=
  [{ exportNameBytes := [112,97,105,114,70,115,116], exportName := "alias",
     carrier := 2, structIdx := 3, fieldCount := 2, resultTy := .nullableRef 2,
     obligation := AverCert.pairFstOb }]
example : AverCert.AcceptedArtifact.fieldProjectionClaimExportNames relabeled = ["alias"] := rfl
example : AverCert.AcceptedArtifact.fieldProjectionClaimExportNames relabeled ≠
    AverCert.AcceptedArtifact.fieldProjectionManifestPlanNames AverCert.manifest := by decide
def weakManifest (_ : List String) : Bool := true
example : weakManifest (AverCert.AcceptedArtifact.fieldProjectionClaimExportNames relabeled) = true := rfl
"#;
    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let check = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .output()
        .expect("run field-projection GuardIso");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    assert!(
        check.status.success(),
        "field-projection GuardIso failed:\n{combined}"
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// The projection index is authoritative Lean plan DATA. Flipping it while
/// leaving the artifact unchanged must fail the source/target and byte pins.
#[test]
fn cert_verify_declines_flipped_field_projection_plan() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping field-projection plan tamper test: `lake` not available");
        return;
    }

    let (out_dir, wasm, cert) = compile_cert_goals("cert-proj-plan-flip");
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let tampered = text.replacen(".structGetUser 20 0 0", ".structGetUser 20 1 0", 1);
    assert_ne!(text, tampered, "userName raw projection plan shape changed");
    std::fs::write(&plans, tampered).unwrap();

    let (ok, out) = aver_check(&wasm, &cert);
    assert!(
        !ok,
        "flipped field-projection plan must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "wrong reason for flipped projection field:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "flipped field-projection plan credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A partial source-type relabel inside authoritative `Plans.lean` must not
/// change the byte-bound representation plan it purports to explain.
#[test]
fn cert_verify_declines_relabeled_projection_source_types() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping projection relabel tamper test: `lake` not available");
        return;
    }

    let (out_dir, wasm, cert) = compile_cert_goals("cert-proj-relabel");
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let tampered = text
        .replacen("(.named \"User\")", "(.named \"Other\")", 2)
        .replacen(".projectField \"User\"", ".projectField \"Other\"", 1);
    assert_ne!(text, tampered, "userName SymPlan type labels changed");
    std::fs::write(&plans, tampered).unwrap();

    let (ok, out) = aver_check(&wasm, &cert);
    assert!(
        !ok,
        "partially relabeled projection source types must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "wrong reason for partial source-type relabel:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "partially relabeled projection source types credited:\n{out}"
    );

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// A tampered byte-first `recursion-plan-v1` plan is declined. The vectors
/// exercise additive, multiplicative, and accumulator plans in the shipped
/// `Plans.lean` while leaving the wasm untouched. The checker rebuilds the
/// shipped plan (its `rfl` chain is pinned to the honest bytes) and its kernel
/// witness proves `accepted` over `manifest.recursionPlans`, so either gate
/// rejects the plan. The factorial vector deliberately preserves the lowered
/// bytes: its multiply call is assigned the wrong role at the same index.
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
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(ok, "honest recursion certificate should verify:\n{report}");

    // Honest bytes and plan, zero locals in the obligation only: recursion
    // canonically declares one carrier scratch local.
    {
        let dir = temp_dir("cert-recursion-zero-locals");
        copy_dir(&out_dir, &dir);
        set_named_code_nlocals_to_zero(&dir.join("cert/Module.lean"), "sumFrom", 1, 1);
        let (ok, report) = aver_check(&dir.join("recgen.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "recursion zero-locals code must be DECLINED:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

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
    let tampers: [(&str, &str, &str); 6] = [
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
        (
            "byte-identical multiply role mislabel",
            ".hostCall .mul 13 [0, 5]",
            ".hostCall .add 13 [0, 5]",
        ),
        (
            "accumulator threading swap",
            ".selfCall true 5 [3, 6]",
            ".selfCall true 5 [3, 4]",
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
        let (ok, report) = aver_check(&dir.join("recgen.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "{label}: tampered recursion plan must be declined:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    let _ = std::fs::remove_dir_all(&out_dir);
}

/// GuardIso for the L3 witness: hostile measure/descent claims keep the honest
/// bytes and obligation bindings, fail exactly at `checkTerm`, and are accepted
/// by a literal one-conjunct-weakened copy. Total policy without a witness also
/// fails closed.
#[test]
fn recursion_termination_witness_guard_is_isolating() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping termination-witness GuardIso test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-recursion-termination-guard-iso");
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
        .expect("compile recgen fixture for termination-witness GuardIso");
    assert!(
        compile.status.success(),
        "recgen compile failed for termination-witness GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let build = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build recgen certificate before termination-witness GuardIso");
    assert!(
        build.status.success(),
        "recgen certificate failed before termination-witness GuardIso:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    std::fs::write(
        cert.join("GuardIso.lean"),
        include_str!("fixtures/cert_termination_guard_iso.lean"),
    )
    .unwrap();
    let check = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .output()
        .expect("run termination-witness GuardIso");
    assert!(
        check.status.success(),
        "termination-witness GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// Mutual L3 GuardIso: changing the witness on only one SCC obligation reaches
/// the mutual termination conjunct, and deleting just that conjunct accepts the
/// otherwise byte-identical hostile claim.
#[test]
fn mutual_termination_witness_guard_is_isolating() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping mutual termination-witness GuardIso: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-mutual-termination-guard-iso");
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
        .expect("compile mutual fixture for termination GuardIso");
    assert!(
        compile.status.success(),
        "mutual compile failed for termination GuardIso:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let build = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build mutual certificate before termination GuardIso");
    assert!(
        build.status.success(),
        "honest mutual certificate must build"
    );
    std::fs::write(
        cert.join("GuardIso.lean"),
        include_str!("fixtures/cert_mutual_termination_guard_iso.lean"),
    )
    .unwrap();
    let check = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("GuardIso.lean")
        .output()
        .expect("run mutual termination GuardIso");
    assert!(
        check.status.success(),
        "mutual termination GuardIso failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}

/// The JSON policy/witness is transport data. Unsupported shapes fail strict
/// decoding; supported-but-wrong values fail the Lean bindings, and even a
/// coordinated JSON + Manifest rewrite fails canonical `ClaimAxes`.
#[test]
fn cert_verify_declines_tampered_termination_manifest() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping termination manifest round-trip test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-termination-manifest-roundtrip");
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
        .expect("compile recgen fixture for termination manifest round-trip");
    assert!(compile.status.success());
    let wasm = out_dir.join("recgen.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(ok, "honest totality manifest should verify:\n{report}");

    for (label, mutate, expected) in [
        ("wrong descent", 0_u8, "does not bind"),
        ("unknown measure", 1_u8, "unsupported termination measure"),
        ("missing witness", 2_u8, "is missing `termination_witness`"),
    ] {
        let dir = temp_dir("cert-termination-manifest-tamper");
        copy_dir(&out_dir, &dir);
        let path = dir.join("cert/cert-manifest.json");
        let mut manifest: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
        let entry = manifest["certified"]
            .as_array_mut()
            .unwrap()
            .iter_mut()
            .find(|entry| entry["name"] == "sumFrom")
            .unwrap();
        match mutate {
            0 => entry["termination_witness"]["descent"] = serde_json::json!(1),
            1 => entry["termination_witness"]["measure"]["kind"] = serde_json::json!("lex"),
            2 => {
                entry.as_object_mut().unwrap().remove("termination_witness");
            }
            _ => unreachable!(),
        }
        std::fs::write(&path, serde_json::to_vec_pretty(&manifest).unwrap()).unwrap();
        let (ok, report) = aver_check(&dir.join("recgen.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "{label}: hostile totality manifest verified:\n{report}"
        );
        assert!(
            report.contains(expected),
            "{label}: wrong decline reason, expected `{expected}`:\n{report}"
        );
        if label == "wrong descent" {
            assert!(
                report.contains("checker-owned Lean witness"),
                "witness decline must identify the Lean binding:\n{report}"
            );
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    // Coordinate the JSON envelope with the Lean manifest so the witness's
    // transport bindings still agree. Canonical recursion axes must then reject
    // the producer-selected policy/termination inside Lean.
    {
        let dir = temp_dir("cert-claim-axes-descent-tamper");
        copy_dir(&out_dir, &dir);
        let manifest_lean = dir.join("cert/Manifest.lean");
        let text = std::fs::read_to_string(&manifest_lean).unwrap();
        let tampered = text.replacen("descent := (-1)", "descent := (1)", 1);
        assert_ne!(text, tampered, "sumFrom termination shape changed");
        std::fs::write(&manifest_lean, tampered).unwrap();

        let manifest_json = dir.join("cert/cert-manifest.json");
        let mut json: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&manifest_json).unwrap()).unwrap();
        let entry = json["certified"]
            .as_array_mut()
            .unwrap()
            .iter_mut()
            .find(|entry| entry["name"] == "sumFrom")
            .unwrap();
        entry["termination_witness"]["descent"] = serde_json::json!(1);
        std::fs::write(&manifest_json, serde_json::to_vec_pretty(&json).unwrap()).unwrap();

        let (ok, report) = aver_check(&dir.join("recgen.wasm"), &dir.join("cert"));
        assert!(!ok, "noncanonical coordinated descent verified:\n{report}");
        assert!(
            report.contains("did not build") || report.contains("does not bind"),
            "wrong ClaimAxes descent decline:\n{report}"
        );
        let _ = std::fs::remove_dir_all(dir);
    }

    {
        let dir = temp_dir("cert-claim-axes-policy-tamper");
        copy_dir(&out_dir, &dir);
        let manifest_lean = dir.join("cert/Manifest.lean");
        let text = std::fs::read_to_string(&manifest_lean).unwrap();
        let honest = "policy := .simulatesModelTotally, termination? := some ({ measure := .intNatAbs 0, descent := (-1) } : AverCert.Schema.TerminationWitness)";
        let tampered = text.replacen(honest, "policy := .simulatesModel, termination? := none", 1);
        assert_ne!(text, tampered, "sumFrom policy shape changed");
        std::fs::write(&manifest_lean, tampered).unwrap();

        let manifest_json = dir.join("cert/cert-manifest.json");
        let mut json: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&manifest_json).unwrap()).unwrap();
        let entry = json["certified"]
            .as_array_mut()
            .unwrap()
            .iter_mut()
            .find(|entry| entry["name"] == "sumFrom")
            .unwrap();
        entry["policy"] = serde_json::json!("simulatesModel");
        entry.as_object_mut().unwrap().remove("termination_witness");
        std::fs::write(&manifest_json, serde_json::to_vec_pretty(&json).unwrap()).unwrap();

        let (ok, report) = aver_check(&dir.join("recgen.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "coordinated partial-recursion policy verified:\n{report}"
        );
        assert!(
            report.contains("did not build") || report.contains("does not bind"),
            "wrong ClaimAxes policy decline:\n{report}"
        );
        let _ = std::fs::remove_dir_all(dir);
    }
    let _ = std::fs::remove_dir_all(out_dir);
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
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(ok, "honest mutual certificate should verify:\n{report}");
    let certificate = std::fs::read_to_string(cert.join("Certificate.lean")).unwrap();
    assert!(
        certificate.contains("theorem isEven_mutualSemanticBridge")
            && certificate.contains("theorem isOdd_mutualSemanticBridge")
            && !certificate.contains("isEven_simulates")
            && !certificate.contains("isOdd_simulates")
            && !certificate.contains("isEven_wasm")
            && !certificate.contains("isOdd_wasm"),
        "migrated mutual family must expose only option-(b) bridges:\n{certificate}"
    );

    // Honest bytes and plan, zero locals in the obligation only: every mutual
    // member canonically declares one carrier scratch local.
    {
        let dir = temp_dir("cert-mutual-zero-locals");
        copy_dir(&out_dir, &dir);
        set_named_code_nlocals_to_zero(&dir.join("cert/Module.lean"), "isEven", 1, 1);
        let (ok, report) = aver_check(&dir.join("mutual.wasm"), &dir.join("cert"));
        assert!(!ok, "mutual zero-locals code must be DECLINED:\n{report}");
        let _ = std::fs::remove_dir_all(&dir);
    }

    // The migrated family has no bespoke simulation theorem to replace with a
    // vacuous proof. Pointing the obligation at a trapping decoy must now fail
    // directly in the generic mutual claim's byte/plan acceptance.
    {
        let dir = temp_dir("cert-mutual-code-decouple");
        copy_dir(&out_dir, &dir);
        let module = dir.join("cert/Module.lean");
        let source = std::fs::read_to_string(&module).unwrap();
        let edited = source.replacen(
            "end CertModule",
            "/-- decoy: always traps, so an unbound simulation would be vacuous. -/\n\
             def wrongCode : CodeTbl := fun _ => none\nend CertModule",
            1,
        );
        assert_ne!(source, edited, "mutual Module.lean end marker changed");
        std::fs::write(&module, edited).unwrap();

        let manifest = dir.join("cert/Manifest.lean");
        let source = std::fs::read_to_string(&manifest).unwrap();
        let edited = source.replacen(
            "code := CertModule.isEvenCode",
            "code := CertModule.wrongCode",
            1,
        );
        assert_ne!(source, edited, "isEven obligation code field changed");
        std::fs::write(&manifest, edited).unwrap();

        let (ok, report) = aver_check(&dir.join("mutual.wasm"), &dir.join("cert"));
        assert!(
            !ok && !report.contains("CERTIFIED"),
            "mutual code decouple must be DECLINED by generic acceptance:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    // Likewise the obligation's selected member is part of the generic claim;
    // a wrong self index cannot be hidden behind a bespoke mutual proof.
    {
        let dir = temp_dir("cert-mutual-self-decouple");
        copy_dir(&out_dir, &dir);
        let manifest = dir.join("cert/Manifest.lean");
        let source = std::fs::read_to_string(&manifest).unwrap();
        let honest = "code := CertModule.isEvenCode, host := fun _ sub _ _ _ _ => CertModule.isEvenHost sub, self := 1,";
        let hostile = "code := CertModule.isEvenCode, host := fun _ sub _ _ _ _ => CertModule.isEvenHost sub, self := 5,";
        let edited = source.replacen(honest, hostile, 1);
        assert_ne!(source, edited, "isEven obligation self field changed");
        std::fs::write(&manifest, edited).unwrap();

        let (ok, report) = aver_check(&dir.join("mutual.wasm"), &dir.join("cert"));
        assert!(
            !ok && !report.contains("CERTIFIED"),
            "mutual self decouple must be DECLINED by generic acceptance:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

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
        let (ok, report) = aver_check(&dir.join("mutual.wasm"), &dir.join("cert"));
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
        lake_for_cert(cert)
            .arg("build")
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
    lean.push_str("def dummyOb (nm : String) (s : Nat) : Obligation :=\n  { export_ := nm, policy := .simulatesModel, carrier := 2, code := fun _ => none,\n    host := fun _ _ _ _ _ _ => fun _ => none, self := s, Dom := Unit, Cod := Unit,\n    domRepr := fun _ _ _ => True, codRepr := fun _ _ _ => True, model := fun _ => () }\n\n");
    lean.push_str("def manifestS : Manifest :=\n  { subject := { artifactHash := \"\", profile := \"\", abi := \"\", artifactRoot := \"\", exports := [], declaredUncertified := [], capabilities := [], start := none, hostRoleTable := some { box := none, add := none, mul := none, sub := none, toIndex := none }, arithParams := none, stringHostRoles := [], contracts := [] },\n    symFragmentPlans := [], stringEqPlans := [], stringConcatPlans := [], constructPlans := [],\n    exprFragmentPlans := [], recursionPlans := [], mutualPlans := [(\"a\", honestPlan)], compositionPlans := [], verbatimPlans := [], intDispatchPlans := [], fieldProjectionPlans := [], obligations := [] }\n\n");
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
    let honest_build = lake_for_cert(&cert)
        .arg("build")
        .current_dir(&cert)
        .output()
        .expect("lake build runs");
    assert!(honest_build.status.success(), "honest cert must lake-build");

    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let elab = lake_for_cert(&cert)
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
/// `Plans.lean` `lowerVerbatimCodeEntry`/`exactFuncBindingForExport` `rfl` pins and the
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
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(ok, "honest verbatim certificate should verify:\n{report}");

    // Honest bytes and plan, zero locals in the obligation only. `wrapItems`
    // projects a field, so its canonical verbatim layout has three locals.
    {
        let dir = temp_dir("cert-verbatim-zero-locals");
        copy_dir(&out_dir, &dir);
        set_named_code_nlocals_to_zero(&dir.join("cert/Module.lean"), "wrapItems", 1, 3);
        let (ok, report) = aver_check(&dir.join("verbatimgen.wasm"), &dir.join("cert"));
        assert!(!ok, "verbatim zero-locals code must be DECLINED:\n{report}");
        let _ = std::fs::remove_dir_all(&dir);
    }

    let honest = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    // (a) wrong `ref.test` type index: `wrapItems` tests struct type 1 -> 2.
    // (b) swapped dispatch cascade: `tagName` tests tags 4 <-> 5.
    // (c) wrong `array.new_data` data-segment index: `tagName`'s "alpha" 0 -> 9.
    // (d) wrong `ref.null` result heap type: `wrapItems` 10 -> 18.
    // (e) equal-length payload collision: `tagName`'s "alpha" -> "alphb" (same
    //     length, same data index). The code-entry lowering pins only the payload
    //     LENGTH, so every byte-equality pin stays green; ONLY the acceptance
    //     predicate's `verbatimPayloadsBound` conjunct (payload bytes vs the
    //     byte-pinned data segment) declines it. Deleting that conjunct makes this
    //     verify — the regression this vector guards.
    let tampers: [(&str, &str, &str); 5] = [
        (
            "ref.test type index",
            ".test 1 (.project 1 0) (.leaf (.refNull))",
            ".test 2 (.project 1 0) (.leaf (.refNull))",
        ),
        (
            "swapped dispatch cascade",
            ".test 4 (.arrayNewData 7 0 [97, 108, 112, 104, 97]) (.test 5",
            ".test 5 (.arrayNewData 7 0 [97, 108, 112, 104, 97]) (.test 4",
        ),
        (
            "array.new_data data index",
            ".arrayNewData 7 0 [97, 108, 112, 104, 97]",
            ".arrayNewData 7 9 [97, 108, 112, 104, 97]",
        ),
        (
            "ref.null heap type",
            "resultSig := .refNull 10",
            "resultSig := .refNull 18",
        ),
        (
            "equal-length payload collision",
            ".arrayNewData 7 0 [97, 108, 112, 104, 97]",
            ".arrayNewData 7 0 [97, 108, 112, 104, 98]",
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
        let (ok, report) = aver_check(&dir.join("verbatimgen.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "{label}: tampered verbatim plan must be declined:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    // Guard-isolation: prove in-kernel that each vector diverges the lowered
    // code-entry bytes (so the byte-equality gate — the whole binding — catches
    // it), mirroring the spike's four `by decide` vectors on the real plans.
    // carrier 9; `wrapItems` result heap 10, `tagName` string-array type 7.
    let mut lean = String::new();
    lean.push_str("import Schema\nimport PlanCheck\nimport PlanLower\nimport PlanBytes\n\n");
    lean.push_str("open AverCert.Schema\nopen AverCert.PlanBytes\n\n");
    lean.push_str("def honestWrap : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 2, fieldLocal := 1, resultSig := .refNull 10, body := .test 1 (.project 1 0) (.leaf (.refNull)) }\n");
    lean.push_str("def honestTag : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 1, fieldLocal := 0, resultSig := .refNull 7, body := .test 4 (.arrayNewData 7 0 [97, 108, 112, 104, 97]) (.test 5 (.arrayNewData 7 1 [98, 101, 116, 97]) (.leaf (.arrayNewData 7 2 [103, 97, 109, 109, 97]))) }\n\n");
    lean.push_str("example : AverCert.PlanCheck.checkVerbatimRawPlan honestWrap = true := rfl\n");
    lean.push_str("example : AverCert.PlanCheck.checkVerbatimRawPlan honestTag = true := rfl\n\n");
    lean.push_str("def tamper1 : VerbatimRawPlan := { honestWrap with body := .test 2 (.project 1 0) (.leaf (.refNull)) }\n");
    lean.push_str("example : lowerVerbatimCodeEntry 9 tamper1 ≠ lowerVerbatimCodeEntry 9 honestWrap := by decide\n");
    lean.push_str("def tamper2 : VerbatimRawPlan := { honestTag with body := .test 5 (.arrayNewData 7 0 [97, 108, 112, 104, 97]) (.test 4 (.arrayNewData 7 1 [98, 101, 116, 97]) (.leaf (.arrayNewData 7 2 [103, 97, 109, 109, 97]))) }\n");
    lean.push_str("example : lowerVerbatimCodeEntry 9 tamper2 ≠ lowerVerbatimCodeEntry 9 honestTag := by decide\n");
    lean.push_str("def tamper3 : VerbatimRawPlan := { honestTag with body := .test 4 (.arrayNewData 7 9 [97, 108, 112, 104, 97]) (.test 5 (.arrayNewData 7 1 [98, 101, 116, 97]) (.leaf (.arrayNewData 7 2 [103, 97, 109, 109, 97]))) }\n");
    lean.push_str("example : lowerVerbatimCodeEntry 9 tamper3 ≠ lowerVerbatimCodeEntry 9 honestTag := by decide\n");
    lean.push_str(
        "def tamper4 : VerbatimRawPlan := { honestWrap with resultSig := .refNull 18 }\n",
    );
    lean.push_str("example : lowerVerbatimCodeEntry 9 tamper4 ≠ lowerVerbatimCodeEntry 9 honestWrap := by decide\n");

    let honest_build = lake_for_cert(&cert)
        .arg("build")
        .current_dir(&cert)
        .output()
        .expect("lake build runs");
    assert!(honest_build.status.success(), "honest cert must lake-build");
    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let elab = lake_for_cert(&cert)
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

/// A compiler-produced, wasmparser-valid scalar-f64 widened match must travel
/// through the plan-backed verbatim bridge end to end. Both the f64 immediate
/// and the declared result kind remain bound to the emitted artifact bytes.
#[test]
fn cert_verify_scalar_f64_verbatim_fixture_and_tampers() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping scalar-f64 verbatim test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-f64-verbatim");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/f64verbatim.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "f64verbatim compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("f64verbatim.wasm");
    let cert = out_dir.join("cert");
    let honest_bytes = std::fs::read(&wasm).unwrap();
    wasmparser::Validator::new()
        .validate_all(&honest_bytes)
        .expect("compiler-produced f64verbatim wasm must validate");

    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    assert!(
        plans.contains(
            "def floatOrZeroVerbatimPlan : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 2, fieldLocal := 1, resultSig := .f64Scalar, body := .test 1 (.project 1 0) (.leaf (.f64Bits 0)) }"
        ),
        "floatOrZero plan must pin the scalar-f64 result and zero default"
    );
    let module = std::fs::read_to_string(cert.join("Module.lean")).unwrap();
    assert!(
        module.contains("if fn = 1 then some ⟨1, 3,"),
        "floatOrZero code obligation must bind nlocals = 3"
    );

    let (ok, report) = aver_check(&wasm, &cert);
    assert!(
        ok,
        "honest scalar-f64 certificate should pass trusted-olean preflight:\n{report}"
    );
    assert!(
        report.contains("CHECKED") && !report.contains("CERTIFIED"),
        "honest scalar-f64 preflight must report CHECKED only:\n{report}"
    );

    let mut imported_funcs = 0u32;
    let mut export_func = None;
    let mut code_ordinal = 0u32;
    let mut f64_immediate_offsets = Vec::new();
    let mut type_section_range = None;
    for payload in wasmparser::Parser::new(0).parse_all(&honest_bytes) {
        match payload.expect("compiler-produced wasm must parse") {
            wasmparser::Payload::TypeSection(reader) => {
                type_section_range = Some(reader.range());
            }
            wasmparser::Payload::ImportSection(reader) => {
                for group in reader {
                    for import in group.expect("import group must parse") {
                        let (_, import) = import.expect("import must parse");
                        if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                            imported_funcs += 1;
                        }
                    }
                }
            }
            wasmparser::Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export.expect("export must parse");
                    if export.name == "floatOrZero" && export.kind == wasmparser::ExternalKind::Func
                    {
                        export_func = Some(export.index);
                    }
                }
            }
            wasmparser::Payload::CodeSectionEntry(body) => {
                let target_ordinal = export_func
                    .expect("floatOrZero export must precede the code section")
                    .checked_sub(imported_funcs)
                    .expect("floatOrZero must be a defined function");
                if code_ordinal == target_ordinal {
                    let mut operators = body.get_operators_reader().unwrap();
                    while !operators.eof() {
                        let opcode_offset = operators.original_position();
                        let operator = operators.read().expect("operator must parse");
                        if matches!(
                            operator,
                            wasmparser::Operator::F64Const { value } if value.bits() == 0
                        ) {
                            assert_eq!(honest_bytes[opcode_offset], 0x44, "expected f64.const");
                            f64_immediate_offsets.push(opcode_offset + 1);
                        }
                    }
                }
                code_ordinal += 1;
            }
            _ => {}
        }
    }
    assert_eq!(
        f64_immediate_offsets.len(),
        1,
        "floatOrZero must contain exactly one zero f64.const immediate"
    );

    // (a) Flip one bit in the body-level f64.const immediate. This remains a
    // valid wasm module but no longer agrees with the byte-derived plan.
    {
        let dir = temp_dir("cert-f64-immediate-tamper");
        copy_dir(&out_dir, &dir);
        let tampered_wasm = dir.join("f64verbatim.wasm");
        let mut bytes = honest_bytes.clone();
        bytes[f64_immediate_offsets[0]] ^= 1;
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("an f64 immediate bit flip must preserve wasm validity");
        std::fs::write(&tampered_wasm, bytes).unwrap();
        let (ok, report) = aver_check(&tampered_wasm, &dir.join("cert"));
        assert!(
            !ok,
            "tampered f64.const immediate must be DECLINED:\n{report}"
        );
        assert!(
            !report.contains("CERTIFIED"),
            "tampered f64.const immediate must never be credited:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    // (b) Change the target signature's f64 result byte to the nullable-ref
    // prefix while leaving the certificate plan claiming F64Scalar.
    {
        let dir = temp_dir("cert-f64-result-type-tamper");
        copy_dir(&out_dir, &dir);
        let tampered_wasm = dir.join("f64verbatim.wasm");
        let range = type_section_range.expect("type section must exist");
        let signature = [0x60, 0x01, 0x63, 0x00, 0x01, 0x7c];
        let matches = honest_bytes[range.clone()]
            .windows(signature.len())
            .enumerate()
            .filter_map(|(offset, bytes)| (bytes == signature).then_some(range.start + offset))
            .collect::<Vec<_>>();
        assert_eq!(
            matches.len(),
            1,
            "floatOrZero's nominal-root-ref -> f64 signature must be unique"
        );
        let mut bytes = honest_bytes.clone();
        bytes[matches[0] + signature.len() - 1] = 0x63;
        std::fs::write(&tampered_wasm, bytes).unwrap();
        let (ok, report) = aver_check(&tampered_wasm, &dir.join("cert"));
        assert!(!ok, "tampered f64 result type must be DECLINED:\n{report}");
        assert!(
            !report.contains("CERTIFIED"),
            "ref-typed artifact with an F64Scalar plan must never be credited:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

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
/// type section (the second appends a second nominal-root parameter). The
/// func/export/code/data sections — hence both the raw binding and code entry —
/// are byte-for-byte identical. Therefore the exact binding lookup returns the
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
    lean.push_str(
        r#"private theorem regressionCodeEntryByFuncIndex_of_binding
    (modBytes modLen funcIdx : Nat) (binding : WasmSlice.FuncBinding)
    (hBinding : WasmSlice.funcBindingByFuncIndex modBytes modLen funcIdx = some binding) :
    WasmSlice.codeEntryByFuncIndex modBytes modLen funcIdx = some binding.codeEntry := by
  unfold WasmSlice.funcBindingByFuncIndex at hBinding
  unfold WasmSlice.codeEntryByFuncIndex
  cases hCodeIdx : WasmSlice.codeIndexByFuncIndex modBytes modLen funcIdx with
  | none => simp_all
  | some codeIdx =>
      cases hType : WasmSlice.typeIndexByCodeIndex modBytes modLen codeIdx with
      | none => simp_all
      | some typeIdx =>
          cases hCode : WasmSlice.codeEntryByCodeIndex modBytes modLen codeIdx with
          | none => simp_all
          | some codeEntry =>
              simp_all
              subst binding
              rfl

private theorem regressionCodeEntryForExport_of_binding
    (modBytes modLen : Nat) (targetName : WasmSlice.ByteSeq)
    (binding : WasmSlice.FuncBinding)
    (hBinding : WasmSlice.funcBindingForExport modBytes modLen targetName = some binding) :
    WasmSlice.codeEntryForExport modBytes modLen targetName = some binding.codeEntry := by
  unfold WasmSlice.funcBindingForExport at hBinding
  unfold WasmSlice.codeEntryForExport
  cases hExport : WasmSlice.exportFuncIndex modBytes modLen targetName with
  | none => simp [hExport] at hBinding
  | some funcIdx =>
      simp only [hExport] at hBinding ⊢
      exact regressionCodeEntryByFuncIndex_of_binding
        modBytes modLen funcIdx binding hBinding

theorem exactBindingPreservesLegacyPins
    (modBytes modLen : Nat) (targetName expectedCode : WasmSlice.ByteSeq)
    (binding : WasmSlice.FuncBinding) :
    WasmSlice.exactFuncBindingForExport
        modBytes modLen targetName expectedCode = some binding ↔
      WasmSlice.codeEntryForExport modBytes modLen targetName = some expectedCode ∧
      WasmSlice.funcBindingForExport modBytes modLen targetName = some binding ∧
      binding.codeEntry = expectedCode := by
  constructor
  · intro hExact
    unfold WasmSlice.exactFuncBindingForExport at hExact
    have hFiltered := Option.filter_eq_some_iff.mp hExact
    have hLookup : WasmSlice.funcBindingForExport modBytes modLen targetName =
        some binding := hFiltered.1
    have hCode : binding.codeEntry = expectedCode := by
      simpa using hFiltered.2
    refine ⟨?_, hLookup, hCode⟩
    simpa [hCode] using
      regressionCodeEntryForExport_of_binding modBytes modLen targetName binding hLookup
  · rintro ⟨_hEntry, hLookup, hCode⟩
    unfold WasmSlice.exactFuncBindingForExport
    rw [hLookup]
    simp [hCode]

"#,
    );
    lean.push_str("def packLE : List Nat → Nat | [] => 0 | b :: bs => b + (packLE bs <<< 8)\n\n");
    lean.push_str("def decodeTestType (bytes : List Nat) : Option CertDecode.TypeEntry :=\n  match CertDecode.readTypeEntry (packLE bytes) bytes.length with\n  | some (entry, _, 0) => some entry\n  | _ => none\n\n");
    // Minimal modules: header, type section, then a shared func/export/code/data
    // tail. `f` is func 0 of type 0; code entry `[2, 0, 11]`; data segment 0 is
    // "alpha" (passive). Only the type section differs between the two.
    lean.push_str("def hdr : List Nat := [0, 97, 115, 109, 1, 0, 0, 0]\n");
    lean.push_str("def unaryType : List Nat := [1, 8, 1, 96, 1, 99, 4, 1, 99, 5]\n");
    lean.push_str("def binaryType : List Nat := [1, 10, 1, 96, 2, 99, 4, 99, 4, 1, 99, 5]\n");
    lean.push_str("def tailSecs : List Nat := [3, 2, 1, 0, 7, 5, 1, 1, 102, 0, 0, 10, 4, 1, 2, 0, 11, 11, 8, 1, 1, 5, 97, 108, 112, 104, 97]\n");
    lean.push_str("def unaryMod : List Nat := hdr ++ unaryType ++ tailSecs\n");
    lean.push_str("def binaryMod : List Nat := hdr ++ binaryType ++ tailSecs\n");
    lean.push_str("def nameF : List Nat := [102]\n\n");
    // SIGNATURE isolation: the byte-equality gate's inputs are identical...
    lean.push_str("example : WasmSlice.funcBindingForExport (packLE unaryMod) unaryMod.length nameF = WasmSlice.funcBindingForExport (packLE binaryMod) binaryMod.length nameF := rfl\n");
    lean.push_str("example : WasmSlice.codeEntryForExport (packLE unaryMod) unaryMod.length nameF = WasmSlice.codeEntryForExport (packLE binaryMod) binaryMod.length nameF := rfl\n");
    lean.push_str("example : WasmSlice.exactFuncBindingForExport (packLE unaryMod) unaryMod.length nameF [2, 0, 11] = WasmSlice.exactFuncBindingForExport (packLE binaryMod) binaryMod.length nameF [2, 0, 11] := rfl\n");
    lean.push_str("example : WasmSlice.exactFuncBindingForExport (packLE unaryMod) unaryMod.length nameF [3, 0, 11] = none := rfl\n");
    // ...and only the signature guard tells unary from binary.
    lean.push_str(
        "example : WasmSlice.verbatimFuncTypeMatches (packLE unaryMod) unaryMod.length 0 (.refNull 5) = true := rfl\n",
    );
    lean.push_str(
        "example : WasmSlice.verbatimFuncTypeMatches (packLE binaryMod) binaryMod.length 0 (.refNull 5) = false := rfl\n\n",
    );
    // PAYLOAD isolation: segment 0 is "alpha".
    lean.push_str(
        "example : WasmSlice.dataSegmentBytes (packLE unaryMod) unaryMod.length 0 = some [97, 108, 112, 104, 97] := rfl\n",
    );
    lean.push_str("def planAlpha : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 1, fieldLocal := 0, resultSig := .refNull 5, body := .test 1 (.arrayNewData 5 0 [97, 108, 112, 104, 97]) (.leaf .refNull) }\n");
    lean.push_str("def planAlphB : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 1, fieldLocal := 0, resultSig := .refNull 5, body := .test 1 (.arrayNewData 5 0 [97, 108, 112, 104, 98]) (.leaf .refNull) }\n");
    // The structural checker and byte lowering are BLIND to the payload content...
    lean.push_str("example : PlanCheck.checkVerbatimRawPlan planAlpha = true := rfl\n");
    lean.push_str("example : PlanCheck.checkVerbatimRawPlan planAlphB = true := rfl\n");
    lean.push_str("example : PlanCheck.checkVerbatimPlan 2 planAlpha = true := rfl\n");
    lean.push_str("example : PlanCheck.checkVerbatimPlan 2 planAlphB = true := rfl\n");
    lean.push_str("example : PlanBytes.lowerVerbatimCodeEntry 7 planAlpha = PlanBytes.lowerVerbatimCodeEntry 7 planAlphB := rfl\n");
    // ...so only `verbatimPayloadsBound` rejects the equal-length collision.
    lean.push_str(
        "example : AcceptedArtifact.verbatimPayloadsBound (packLE unaryMod) unaryMod.length planAlpha.body = true := rfl\n",
    );
    lean.push_str("example : AcceptedArtifact.verbatimPayloadsBound (packLE unaryMod) unaryMod.length planAlphB.body = false := rfl\n\n");
    // Full admission additionally closes the two generic-proof preconditions:
    // a dispatch root and in-range scratch locals. The raw checker deliberately
    // remains the byte-facing grammar check.
    lean.push_str("def leafRootPlan : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 1, fieldLocal := 0, resultSig := .refNull 5, body := .leaf .refNull }\n");
    lean.push_str("example : PlanCheck.checkVerbatimRawPlan leafRootPlan = true := rfl\n");
    lean.push_str("example : PlanCheck.checkVerbatimPlan 2 leafRootPlan = false := rfl\n");
    lean.push_str("def oobScrutineePlan : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 9, fieldLocal := 0, resultSig := .refNull 5, body := .test 1 .refNull (.leaf .refNull) }\n");
    lean.push_str("example : PlanCheck.checkVerbatimRawPlan oobScrutineePlan = true := rfl\n");
    lean.push_str("example : PlanCheck.checkVerbatimPlan 2 oobScrutineePlan = false := rfl\n");
    lean.push_str("def oobFieldPlan : VerbatimRawPlan := { profile := \"verbatim-plan-v1\", scrutineeLocal := 1, fieldLocal := 10, resultSig := .refNull 5, body := .test 1 .refNull (.leaf .refNull) }\n");
    lean.push_str("example : PlanCheck.checkVerbatimRawPlan oobFieldPlan = true := rfl\n");
    lean.push_str("example : PlanCheck.checkVerbatimPlan 2 oobFieldPlan = false := rfl\n");
    // FIX 2(c): an out-of-range payload element is rejected up front.
    lean.push_str("example : PlanCheck.checkVerbatimRawPlan { profile := \"verbatim-plan-v1\", scrutineeLocal := 1, fieldLocal := 0, resultSig := .refNull 5, body := .test 1 (.arrayNewData 5 0 [256]) (.leaf .refNull) } = false := rfl\n\n");

    // NULLABILITY isolation (re-review FIX 2): the certified verbatim signature is
    // one nominal-root ref -> `[(ref null resultHeapTy)]` — the `0x63` nullable form the
    // `ref.null` default requires. A non-null `0x64` result is rejected. The only
    // byte differing between `unaryMod` and `nonNullMod` is `0x63 -> 0x64`, so the
    // byte-derived binding and code entry are IDENTICAL (the reftype is never in
    // the code entry) — only `checkVerbatimFuncType` tells them apart.
    lean.push_str(
        "example : (decodeTestType [96, 1, 99, 4, 1, 99, 5]).map (WasmSlice.checkVerbatimFuncType (.refNull 5)) = some true := rfl\n",
    );
    lean.push_str(
        "example : (decodeTestType [96, 1, 99, 4, 1, 100, 5]).map (WasmSlice.checkVerbatimFuncType (.refNull 5)) = some false := rfl\n",
    );
    lean.push_str(
        "def nonNullMod : List Nat := hdr ++ [1, 8, 1, 96, 1, 99, 4, 1, 100, 5] ++ tailSecs\n",
    );
    lean.push_str("example : WasmSlice.funcBindingForExport (packLE nonNullMod) nonNullMod.length nameF = WasmSlice.funcBindingForExport (packLE unaryMod) unaryMod.length nameF := rfl\n");
    lean.push_str("example : WasmSlice.codeEntryForExport (packLE nonNullMod) nonNullMod.length nameF = WasmSlice.codeEntryForExport (packLE unaryMod) unaryMod.length nameF := rfl\n");
    lean.push_str("example : WasmSlice.exactFuncBindingForExport (packLE nonNullMod) nonNullMod.length nameF [2, 0, 11] = WasmSlice.exactFuncBindingForExport (packLE unaryMod) unaryMod.length nameF [2, 0, 11] := rfl\n");
    lean.push_str(
        "example : WasmSlice.verbatimFuncTypeMatches (packLE nonNullMod) nonNullMod.length 0 (.refNull 5) = false := rfl\n\n",
    );

    // ABSTRACT-PARAM isolation: after `0x63` a NEGATIVE s33 heap type encodes an
    // abstract heap type (long-form eqref is `0x63 0x6D`, s33 -19), not a concrete
    // nominal root, so the signature guard fail-closes. The module differs from
    // `unaryMod` only in that param byte (`4 -> 109`), so the byte-derived binding
    // and code entry are IDENTICAL — only `checkVerbatimFuncType` tells them apart.
    lean.push_str(
        "example : (decodeTestType [96, 1, 99, 109, 1, 99, 5]).map (WasmSlice.checkVerbatimFuncType (.refNull 5)) = some false := rfl\n",
    );
    lean.push_str(
        "example : (decodeTestType [96, 1, 99, 109, 1, 124]).map (WasmSlice.checkVerbatimFuncType .f64Scalar) = some false := rfl\n",
    );
    lean.push_str(
        "def abstractParamMod : List Nat := hdr ++ [1, 8, 1, 96, 1, 99, 109, 1, 99, 5] ++ tailSecs\n",
    );
    lean.push_str("example : WasmSlice.funcBindingForExport (packLE abstractParamMod) abstractParamMod.length nameF = WasmSlice.funcBindingForExport (packLE unaryMod) unaryMod.length nameF := rfl\n");
    lean.push_str("example : WasmSlice.codeEntryForExport (packLE abstractParamMod) abstractParamMod.length nameF = WasmSlice.codeEntryForExport (packLE unaryMod) unaryMod.length nameF := rfl\n");
    lean.push_str("example : WasmSlice.exactFuncBindingForExport (packLE abstractParamMod) abstractParamMod.length nameF [2, 0, 11] = WasmSlice.exactFuncBindingForExport (packLE unaryMod) unaryMod.length nameF [2, 0, 11] := rfl\n");
    lean.push_str(
        "example : WasmSlice.verbatimFuncTypeMatches (packLE abstractParamMod) abstractParamMod.length 0 (.refNull 5) = false := rfl\n\n",
    );

    // PARSER STRICTNESS isolation (re-review FIX 3): the type-section and
    // data-section walkers parse EVERY declared entry/segment and require EXACT
    // payload exhaustion, so a valid entry followed by trailing bytes, or a count
    // that does not match the bytes, declines — and an over-wide LEB is rejected
    // by the width cap. The honest single-entry sections still match.
    // Type section: a trailing `0xff` after the one valid func type.
    lean.push_str("def trailingTypeMod : List Nat := hdr ++ [1, 9, 1, 96, 1, 99, 4, 1, 99, 5, 255] ++ tailSecs\n");
    lean.push_str(
        "example : WasmSlice.verbatimFuncTypeMatches (packLE trailingTypeMod) trailingTypeMod.length 0 (.refNull 5) = false := rfl\n",
    );
    // Type section: count claims 2 rectypes but only 1 is present.
    lean.push_str("def countMismatchTypeMod : List Nat := hdr ++ [1, 8, 2, 96, 1, 99, 4, 1, 99, 5] ++ tailSecs\n");
    lean.push_str(
        "example : WasmSlice.verbatimFuncTypeMatches (packLE countMismatchTypeMod) countMismatchTypeMod.length 0 (.refNull 5) = false := rfl\n",
    );
    // Data section: a trailing `0xff` after the one valid segment.
    lean.push_str(
        "def dataTrailMod : List Nat := hdr ++ [11, 9, 1, 1, 5, 97, 108, 112, 104, 97, 255]\n",
    );
    lean.push_str("example : WasmSlice.dataSegmentBytes (packLE dataTrailMod) dataTrailMod.length 0 = none := rfl\n");
    // Data section: count claims 2 segments but only 1 is present.
    lean.push_str(
        "def dataCountMismatchMod : List Nat := hdr ++ [11, 8, 2, 1, 5, 97, 108, 112, 104, 97]\n",
    );
    lean.push_str("example : WasmSlice.dataSegmentBytes (packLE dataCountMismatchMod) dataCountMismatchMod.length 0 = none := rfl\n");
    // Over-wide (6-byte) unsigned LEB32 exceeds the u32 width cap and declines.
    lean.push_str("example : WasmSlice.readUleb32 [128, 128, 128, 128, 128, 0] = none := rfl\n");
    lean.push_str("example : WasmSlice.readS33 [128, 128, 128, 128, 128, 0] = none := rfl\n");
    lean.push_str(
        "example : WasmSlice.readS33 [255, 255, 255, 255, 15] = some (4294967295, []) := rfl\n",
    );
    lean.push_str("example : WasmSlice.readS33 [128, 128, 128, 128, 16] = none := rfl\n");
    lean.push_str(
        "example : WasmSlice.readS33 [128, 128, 128, 128, 112] = some (-4294967296, []) := rfl\n",
    );
    lean.push_str("example : WasmSlice.readS33 [128, 128, 128, 128, 96] = none := rfl\n");

    // F64 RESULT-KIND isolation. These two tiny modules have identical
    // func/export/code sections and differ only in the byte-derived type result:
    // `[f64]` versus `[(ref null 5)]`. The weakened predicate below is a literal
    // copy of `verbatimPlanAccepted` with exactly its
    // `verbatimFuncTypeMatches` conjunct removed. It accepts the f64 plan against
    // the ref module; the shipped predicate rejects exactly at that conjunct.
    // Direct checks also prove both cross-kind directions reject.
    lean.push_str(
        r#"
def isoHdr : List Nat := [0, 97, 115, 109, 1, 0, 0, 0]
def isoF64Type : List Nat := [1, 7, 1, 96, 1, 99, 4, 1, 124]
def isoRefType : List Nat := [1, 8, 1, 96, 1, 99, 4, 1, 99, 5]
def isoTail : List Nat :=
  [3, 2, 1, 0, 7, 5, 1, 1, 102, 0, 0,
   10, 46, 1, 44, 3, 1, 124, 1, 109, 1, 99, 5, 32, 0, 33, 2,
   32, 2, 251, 20, 1, 4, 124, 32, 2, 251, 22, 1, 251, 2, 1, 0,
   33, 1, 32, 1, 5, 68, 0, 0, 0, 0, 0, 0, 0, 0, 11, 11]
def isoF64Mod : List Nat := isoHdr ++ isoF64Type ++ isoTail
def isoRefMod : List Nat := isoHdr ++ isoRefType ++ isoTail
def isoNameF : List Nat := [102]
def isoExpectedBinding : WasmSlice.FuncBinding :=
  { funcIdx := 0, typeIdx := 0,
    codeEntry := [44, 3, 1, 124, 1, 109, 1, 99, 5, 32, 0, 33, 2,
                  32, 2, 251, 20, 1, 4, 124, 32, 2, 251, 22, 1,
                  251, 2, 1, 0, 33, 1, 32, 1, 5, 68, 0, 0, 0, 0,
                  0, 0, 0, 0, 11, 11] }

def isoF64Plan : VerbatimRawPlan :=
  { profile := "verbatim-plan-v1", scrutineeLocal := 2, fieldLocal := 1,
    resultSig := .f64Scalar,
    body := .test 1 (.project 1 0) (.leaf (.f64Bits 0)) }

def isoCode : CertPrelude.CodeTbl := fun i : Nat =>
  if i = 0 then
    some ({ arity := 1, nlocals := 3,
            body := AverCert.PlanLower.lowerVerbatimBody isoF64Plan } : CertPrelude.WCode)
  else none

def isoOb : Obligation :=
  { export_ := "f", policy := .simulatesModel, carrier := 5,
    code := isoCode, host := fun _ _ _ _ _ _ => (fun _ : Nat => none), self := 0,
    Dom := Unit, Cod := Unit,
    domRepr := fun _ _ _ => True, codRepr := fun _ _ _ => True,
    model := fun _ => () }

def weakVerbatimPlanAcceptedWithoutResultSig
    (modBytes modLen : Nat) (exportNameBytes : WasmSlice.ByteSeq) (exportName : String)
    (carrier : Nat) (plan : VerbatimRawPlan) (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    PlanCheck.checkVerbatimPlan (AcceptedArtifact.verbatimNLocals plan) plan = true ∧
    ∃ codeEntry binding,
      PlanBytes.lowerVerbatimCodeEntry carrier plan = some codeEntry ∧
      WasmSlice.exactFuncBindingForExport
        modBytes modLen exportNameBytes codeEntry = some binding ∧
      binding.funcIdx = obligation.self ∧
      AcceptedArtifact.verbatimPayloadsBound modBytes modLen plan.body = true ∧
      obligation.code binding.funcIdx =
        some { arity := 1, nlocals := AcceptedArtifact.verbatimNLocals plan,
               body := PlanLower.lowerVerbatimBody plan }

example : WasmSlice.verbatimFuncTypeMatches (packLE isoF64Mod) isoF64Mod.length 0 .f64Scalar = true := rfl
example : WasmSlice.verbatimFuncTypeMatches (packLE isoF64Mod) isoF64Mod.length 0 (.refNull 5) = false := rfl
example : WasmSlice.verbatimFuncTypeMatches (packLE isoRefMod) isoRefMod.length 0 (.refNull 5) = true := rfl
example : WasmSlice.verbatimFuncTypeMatches (packLE isoRefMod) isoRefMod.length 0 .f64Scalar = false := rfl

example : weakVerbatimPlanAcceptedWithoutResultSig
    (packLE isoRefMod) isoRefMod.length isoNameF "f" 5 isoF64Plan isoOb := by
  refine ⟨rfl, rfl, rfl, ⟨_, _, rfl, rfl, rfl, rfl, ?_⟩⟩
  simp [packLE, isoRefMod, isoHdr, isoRefType, isoTail, isoOb, isoCode,
    AcceptedArtifact.verbatimNLocals, isoF64Plan,
    PlanCheck.dispatchHasProjection]
  decide

example : ¬ AcceptedArtifact.verbatimPlanAccepted
    (packLE isoRefMod) isoRefMod.length isoNameF "f" 5 isoF64Plan isoOb := by
  intro h
  rcases h with ⟨_, _, _, ⟨codeEntry, binding, hlower, hbinding, _, hsig, _, _⟩⟩
  have lowerKnown : PlanBytes.lowerVerbatimCodeEntry 5 isoF64Plan =
      some isoExpectedBinding.codeEntry := by rfl
  rw [lowerKnown] at hlower
  injection hlower with hcode
  subst codeEntry
  have known : WasmSlice.exactFuncBindingForExport
      (packLE isoRefMod) isoRefMod.length isoNameF isoExpectedBinding.codeEntry =
        some isoExpectedBinding := by rfl
  rw [known] at hbinding
  injection hbinding with hb
  subst binding
  change WasmSlice.verbatimFuncTypeMatches (packLE isoRefMod) isoRefMod.length 0 .f64Scalar = true at hsig
  have cross : WasmSlice.verbatimFuncTypeMatches (packLE isoRefMod) isoRefMod.length 0 .f64Scalar = false := rfl
  rw [cross] at hsig
  contradiction
"#,
    );

    // Regression-only plan checker controls belong in the test witness, not in
    // the checker-owned proof wall.
    lean.push_str(
        r#"
def offGrammarSymPlan : SymRawPlan :=
  { profile := "sym-fragment-v1", params := [.string, .string], result := .bool,
    body := { nodes := [
      { id := 0, ty := .string, kind := .param 0 },
      { id := 1, ty := .string, kind := .param 1 },
      { id := 2, ty := .bool, kind := .prim .stringEq [0, 1] }], result := 2 } }

example : PlanCheck.checkSymRawPlan offGrammarSymPlan = true := rfl
example : PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
    [] [] offGrammarSymPlan = none := rfl

def illTypedExprPlan : ExprFragmentRawPlan :=
  { profile := "expr-fragment-v1", params := [], result := .boolI32,
    body := { nodes := [
      { id := 0, ty := .boolI32, kind := .constBool true },
      { id := 1, ty := .boolI32, kind := .prim .i64Eq [0, 0] }], result := 1 } }

example : PlanCheck.checkExprFragmentRawPlan illTypedExprPlan = false := rfl
example : PlanCheck.checkConstructRawPlan
    ({ profile := "construct-v1", arity := 1,
       fields := [.local 9] } : ConstructRawPlan) = false := rfl
"#,
    );

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
    let honest_build = lake_for_cert(&cert)
        .arg("build")
        .current_dir(&cert)
        .output()
        .expect("lake build runs");
    assert!(honest_build.status.success(), "honest cert must lake-build");

    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let elab = lake_for_cert(&cert)
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
/// `lowerIntDispatchCodeEntry`/`exactFuncBindingForExport` `rfl` pins and the
/// checker's `manifest.intDispatchPlans` `rfl` pin reject the tampered plan.
/// The `GuardIso.lean` block below isolates each byte-reaching vector by
/// proving in-kernel (`by decide`) that its lowered code entry diverges from
/// the honest one, and the profile vector by proving the lowering BLIND to it
/// (`rfl`) while only `checkIntDispatchRawPlan` rejects it. A rootless plan is
/// likewise lowerable but rejected at admission, before the generic theorem.
/// Two further
/// vectors target the binds the byte gate cannot see: (h) a ZERO-LOCALS code
/// table (honest bytes/plan/wiring; the vacuity attack the exact
/// `nlocals := armCount + 2` bind closes), (i) a coordinated ROLE/TABLE
/// PERMUTATION across `Plans.lean` and the `Artifact.lean` claim (byte- and
/// sibling-blind; rejected only by the host-builder equality bind
/// `obligation.host = intDispatchCanonicalHost carrier hostTable`), and (j) a
/// SAMPLED-PROBE ESCAPE: a box slot behaving canonically only at one input and
/// trapping on every real constant (vacuity via host trap) — extensionally
/// unequal to the canonical builder, so the equality bind declines it.
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
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(
        ok,
        "honest int-dispatch certificate should verify:\n{report}"
    );

    let honest = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    // (a) wrong `ref.test` tag: `boxInt` tests struct type 1 -> 2.
    // (b) swapped dispatch cascade: `gauge` tests tags 5 <-> 6.
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
            ".test 1 (.proj) (.default (0))",
            ".test 2 (.proj) (.default (0))",
        ),
        (
            "swapped dispatch cascade",
            ".test 5 (.hostOp .sub (0) true) (.test 6",
            ".test 6 (.hostOp .sub (0) true) (.test 5",
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
        let (ok, report) = aver_check(&dir.join("intdispatchgen.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "{label}: tampered int-dispatch plan must be declined:\n{report}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    // (h) ZERO-LOCALS vacuity vector: honest bytes, honest plan, honest wiring,
    // but the obligation's code table claims nlocals := 0 — the body would trap
    // on its first `local.set`, making the partial-correctness obligation
    // vacuously true. The acceptance predicate pins the code table's locals
    // count to the CANONICAL byte-derived value (armCount + 2), so this must be
    // DECLINED (an existentially-free nlocals accepted it; weaken-confirmed).
    {
        let dir = temp_dir("cert-int-dispatch-zero-locals");
        copy_dir(&out_dir, &dir);
        let module = dir.join("cert").join("Module.lean");
        let src = std::fs::read_to_string(&module).unwrap();
        assert!(
            src.contains("some ⟨1, 3,"),
            "boxIntCode locals-count header shape changed; update the test"
        );
        std::fs::write(&module, src.replacen("some ⟨1, 3,", "some ⟨1, 0,", 1)).unwrap();
        let (ok, report) = aver_check(&dir.join("intdispatchgen.wasm"), &dir.join("cert"));
        assert!(!ok, "zero-locals code table must be DECLINED:\n{report}");
        let _ = std::fs::remove_dir_all(&dir);
    }

    // (i) ROLE/TABLE PERMUTATION vector: swap the two arm roles in the plan AND
    // permute the claimed host-role table consistently, in every surface the
    // artifact ships (Plans.lean pins + the Artifact.lean claim). The pair
    // lowers byte-identically and the table stays distinct, so the byte gate,
    // the structural checker and the distinctness guard are all blind — only
    // the host-builder equality bind rejects it.
    {
        let dir = temp_dir("cert-int-dispatch-role-permutation");
        copy_dir(&out_dir, &dir);
        let plans = dir.join("cert").join("Plans.lean");
        let src = std::fs::read_to_string(&plans).unwrap();
        assert!(
            src.contains("(.hostOp .sub (0) true)")
                && src.contains("(.hostOp .add (9) false)")
                && src.contains("[(.box, 7), (.add, 8), (.sub, 9)]"),
            "intdispatchgen Plans.lean gauge shape changed; update the test"
        );
        let src = src
            .replace("(.hostOp .sub (0) true)", "(.hostOp .add (0) true)")
            .replace("(.hostOp .add (9) false)", "(.hostOp .sub (9) false)")
            .replace(
                "[(.box, 7), (.add, 8), (.sub, 9)]",
                "[(.box, 7), (.add, 9), (.sub, 8)]",
            );
        std::fs::write(&plans, src).unwrap();
        let artifact = dir.join("cert").join("Artifact.lean");
        let src = std::fs::read_to_string(&artifact).unwrap();
        assert!(
            src.contains("hostTable := [(.box, 7), (.add, 8), (.sub, 9)]"),
            "intdispatchgen Artifact.lean gauge claim shape changed; update the test"
        );
        let src = src.replace(
            "hostTable := [(.box, 7), (.add, 8), (.sub, 9)]",
            "hostTable := [(.box, 7), (.add, 9), (.sub, 8)]",
        );
        std::fs::write(&artifact, src).unwrap();
        let (ok, report) = aver_check(&dir.join("intdispatchgen.wasm"), &dir.join("cert"));
        assert!(!ok, "role/table permutation must be DECLINED:\n{report}");
        let _ = std::fs::remove_dir_all(&dir);
    }

    // (j) SAMPLED-PROBE ESCAPE vector (attack (b)): replace the box slot of
    // `boxIntHost` with a function behaving canonically ONLY at `i64 0` and
    // trapping (`none`) on every other constant — the honest body's boxed
    // default would trap at runtime, making partial correctness vacuous. Any
    // point probe of the slot at `i64 0` accepts it; the EXTENSIONAL
    // host-builder equality (`obligation.host = intDispatchCanonicalHost …`)
    // rejects it, so this must be DECLINED.
    {
        let dir = temp_dir("cert-int-dispatch-sneaky-box");
        copy_dir(&out_dir, &dir);
        let module = dir.join("cert").join("Module.lean");
        let src = std::fs::read_to_string(&module).unwrap();
        let from = "def boxIntHost : HostTbl := fun fn =>\n  if fn = 7 then some (1, boxRef 11)\n  else none";
        let to = "def boxIntHost : HostTbl := fun fn =>\n  if fn = 7 then some (1, fun args => match args with | [WVal.i64v 0] => boxRef 11 args | _ => none)\n  else none";
        assert!(
            src.contains(from),
            "boxIntHost shape changed; update the test"
        );
        std::fs::write(&module, src.replacen(from, to, 1)).unwrap();
        let (ok, report) = aver_check(&dir.join("intdispatchgen.wasm"), &dir.join("cert"));
        assert!(!ok, "sampled-probe-escape host must be DECLINED:\n{report}");
        let _ = std::fs::remove_dir_all(&dir);
    }

    // Guard-isolation: prove in-kernel that each byte-reaching vector diverges
    // the lowered code-entry bytes (so the byte-equality gate catches it), and
    // that the profile vector — which the lowering is provably BLIND to — is
    // rejected exactly by the structural checker. carrier 11; role table
    // box 7 / add 8 / sub 9 (the fixture's byte-derived table).
    let mut lean = String::new();
    lean.push_str("import Schema\nimport PlanCheck\nimport PlanLower\nimport PlanBytes\n\n");
    lean.push_str("open AverCert.Schema\nopen AverCert.PlanBytes\n\n");
    lean.push_str("def tbl : List (HostRole × Nat) := [(.box, 7), (.add, 8), (.sub, 9)]\n");
    lean.push_str("def honestBox : IntDispatchRawPlan := { profile := \"int-dispatch-v1\", body := .test 1 (.proj) (.default (0)) }\n");
    lean.push_str("def honestGauge : IntDispatchRawPlan := { profile := \"int-dispatch-v1\", body := .test 5 (.hostOp .sub (0) true) (.test 6 (.hostOp .add (9) false) (.test 7 (.proj) (.default (7)))) }\n\n");
    lean.push_str("example : AverCert.PlanCheck.checkIntDispatchRawPlan honestBox = true := rfl\n");
    lean.push_str(
        "example : AverCert.PlanCheck.checkIntDispatchRawPlan honestGauge = true := rfl\n\n",
    );
    lean.push_str("def tamper1 : IntDispatchRawPlan := { honestBox with body := .test 2 (.proj) (.default (0)) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 11 tbl tamper1 ≠ lowerIntDispatchCodeEntry 11 tbl honestBox := by decide\n");
    lean.push_str("def tamper2 : IntDispatchRawPlan := { honestGauge with body := .test 6 (.hostOp .sub (0) true) (.test 5 (.hostOp .add (9) false) (.test 7 (.proj) (.default (7)))) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 11 tbl tamper2 ≠ lowerIntDispatchCodeEntry 11 tbl honestGauge := by decide\n");
    lean.push_str("def tamper3 : IntDispatchRawPlan := { honestGauge with body := .test 5 (.hostOp .add (0) true) (.test 6 (.hostOp .add (9) false) (.test 7 (.proj) (.default (7)))) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 11 tbl tamper3 ≠ lowerIntDispatchCodeEntry 11 tbl honestGauge := by decide\n");
    lean.push_str("def tamper4 : IntDispatchRawPlan := { honestGauge with body := .test 5 (.hostOp .sub (0) true) (.test 6 (.hostOp .add (8) false) (.test 7 (.proj) (.default (7)))) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 11 tbl tamper4 ≠ lowerIntDispatchCodeEntry 11 tbl honestGauge := by decide\n");
    lean.push_str("def tamper5 : IntDispatchRawPlan := { honestGauge with body := .test 5 (.hostOp .sub (0) true) (.test 6 (.hostOp .add (9) true) (.test 7 (.proj) (.default (7)))) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 11 tbl tamper5 ≠ lowerIntDispatchCodeEntry 11 tbl honestGauge := by decide\n");
    lean.push_str("def tamper6 : IntDispatchRawPlan := { honestGauge with body := .test 5 (.hostOp .sub (0) true) (.test 6 (.hostOp .add (9) false) (.test 7 (.proj) (.default (8)))) }\n");
    lean.push_str("example : lowerIntDispatchCodeEntry 11 tbl tamper6 ≠ lowerIntDispatchCodeEntry 11 tbl honestGauge := by decide\n");
    // Profile vector: the lowering is BLIND to the profile string, so the byte
    // gate alone would accept it — only the structural checker rejects it.
    lean.push_str(
        "def tamper7 : IntDispatchRawPlan := { honestBox with profile := \"int-dispatch-v2\" }\n",
    );
    lean.push_str("example : lowerIntDispatchCodeEntry 11 tbl tamper7 = lowerIntDispatchCodeEntry 11 tbl honestBox := rfl\n");
    lean.push_str("example : AverCert.PlanCheck.checkIntDispatchRawPlan tamper7 = false := rfl\n");
    // A default-only cascade has bytes, but cannot satisfy the generic proof's
    // initial-stack boundary; admission rejects it before theorem application.
    lean.push_str("def rootless : IntDispatchRawPlan := { profile := \"int-dispatch-v1\", body := .default 0 }\n");
    lean.push_str("example : (lowerIntDispatchCodeEntry 11 tbl rootless).isSome = true := rfl\n");
    lean.push_str("example : AverCert.PlanCheck.checkIntDispatchRawPlan rootless = false := rfl\n");

    let honest_build = lake_for_cert(&cert)
        .arg("build")
        .current_dir(&cert)
        .output()
        .expect("lake build runs");
    assert!(honest_build.status.success(), "honest cert must lake-build");
    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let elab = lake_for_cert(&cert)
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
/// section EXCEPT the type section (the second appends a second nominal-root
/// parameter). The func/export/code sections — hence the byte-derived
/// raw `FuncBinding` and code entry are byte-for-byte identical, so the exact
/// binding lookup returns the SAME value and only the signature conjunct
/// distinguishes unary from binary.
///
/// HOST-TABLE DISTINCTNESS guard (`hostTableIndicesDistinct`): the plan names
/// helpers by ROLE and the byte lowering substitutes table indices, so under a
/// DUPLICATED table (add and sub claiming one index) two plans differing only
/// in an arm's role are proven to lower to IDENTICAL bytes (`rfl` — the byte
/// gate is blind); under the honest distinct table they are proven to diverge
/// (`by decide`). Only `hostTableIndicesDistinct` rejects the duplicated
/// table, restoring the gate's discrimination.
///
/// HOST-BUILDER EQUALITY guard
/// (`obligation.host = intDispatchCanonicalHost carrier hostTable`):
/// distinctness alone leaves the table a free witness — swapping two arm roles
/// in the plan AND permuting the table consistently cancels out
/// byte-identically, keeps the table distinct, and passes the structural
/// checker. The guard requires the WHOLE obligation host builder to equal the
/// canonical builder for the claimed table (extensional, mirroring in-kernel
/// the checker's whole-host `rfl` pin) — a sampled probe would leave unsampled
/// slot behaviour free (a slot acting as its role only on the probed inputs:
/// attack (b)). The permuted pair is proven byte-identical (`rfl`),
/// sibling-blind (`rfl`), the honest tables close the equality by `rfl`, and
/// the permuted/sneaky builders are rejected exactly by the equality conjunct
/// (each `≠` exhibited through a distinguishing input).
///
/// LOCALS-COUNT bind (`nlocals := armCount + 2` in `intDispatchPlanAccepted`):
/// a code table claiming ZERO locals for the honest body traps on its first
/// `local.set`, making the partial-correctness obligation vacuously true; the
/// acceptance pins the count to the canonical byte-derived value, and the
/// zero-locals table is proven to diverge from it.
///
/// Each assertion is constructed so it holds ONLY because its target guard
/// fires (weaken-confirmed: replacing the `verbatimFuncTypeMatches` conjunct
/// with `true` in a throwaway copy breaks solely the binary-arity reject line;
/// replacing `hostTableIndicesDistinct` with `true` breaks solely the
/// duplicated-table reject line; replacing the host-builder equality conjunct
/// with `True` accepts the permuted-pair acceptance witness that the equality
/// rejects; reverting the locals count to an existential accepts the
/// zero-locals vacuity obligation that the exact bind rejects — nothing else
/// moves).
#[test]
fn list_constructor_kernel_guards_are_isolating() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping List-constructor guard isolation test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-list-constructor-guard-iso");
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
        .expect("compile JSON constructor guard fixture");
    assert!(
        compile.status.success(),
        "JSON constructor guard fixture failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let cert = out_dir.join("cert");
    let build = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("build")
        .output()
        .expect("build JSON certificate before constructor guard transcript");
    assert!(
        build.status.success(),
        "JSON certificate build failed before constructor guard transcript:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    let wasm = std::fs::read(out_dir.join("json.wasm")).unwrap();
    let mut imported_func_count = 0;
    let mut singleton_func_idx = None;
    let mut code_idx = 0;
    let mut tamper_offset = None;
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        match payload.expect("parse JSON constructor guard fixture") {
            wasmparser::Payload::ImportSection(reader) => {
                for group in reader {
                    for import in group.expect("read import group") {
                        let (_, import) = import.expect("read import");
                        if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                            imported_func_count += 1;
                        }
                    }
                }
            }
            wasmparser::Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export.expect("read export");
                    if export.name == "singletonJsonEntries"
                        && export.kind == wasmparser::ExternalKind::Func
                    {
                        singleton_func_idx = Some(export.index);
                    }
                }
            }
            wasmparser::Payload::CodeSectionEntry(body) => {
                if Some(imported_func_count + code_idx) == singleton_func_idx {
                    let range = body.range();
                    let offset = range.start + 1;
                    assert!(
                        offset < range.end,
                        "code entry must contain an interior byte"
                    );
                    tamper_offset = Some(offset);
                }
                code_idx += 1;
            }
            _ => {}
        }
    }
    let tamper_offset = tamper_offset.expect("code entry for singletonJsonEntries export");
    assert_ne!(wasm[tamper_offset], 24, "tamper must change the code byte");
    let lean = r#"import Artifact
open CertPrelude AverCert.Schema AverCert.WasmSlice
set_option maxRecDepth 400000

def honestSym := AverCert.Plans.singletonJsonEntriesConstructSymPlan
def honestSingleton := AverCert.Plans.singletonJsonEntriesConstructPlan
def honestPrependSym := AverCert.Plans.prependJsonEntryConstructSymPlan
def honestClaim := AverCert.Artifact.constructClaims[0]
def permutedPrepend : ConstructRawPlan :=
  { profile := "construct-v1", arity := 2, fields := [.local 1, .local 0] }

-- SymCheckAttack: the empty-tail node lies about its element type while every
-- source/target-origin check remains green. Dropping ONLY checkSymRawPlan
-- accepts it; the shipped conjunction rejects it.
def badEmptySym : SymRawPlan :=
  { honestSym with body :=
      { nodes := [
          { id := 0, ty := (.app2 "Tuple" .string (.named "Json")), kind := .param 0 },
          { id := 1, ty := (.app1 "List" (.app2 "Tuple" .string (.named "Json"))),
            kind := .emptyList .string },
          { id := 2, ty := (.app1 "List" (.app2 "Tuple" .string (.named "Json"))),
            kind := .construct "List" "::" [0, 1] }], result := 2 } }
def sourceAccepted (s : SymRawPlan) (p : ConstructRawPlan) : Bool :=
  AverCert.PlanCheck.checkSymRawPlan s &&
  AverCert.PlanCheck.checkConstructRawPlan p &&
  AverCert.PlanCheck.constructPlanMatchesSymRawPlan s p
def sourceDropSym (s : SymRawPlan) (p : ConstructRawPlan) : Bool :=
  AverCert.PlanCheck.checkConstructRawPlan p &&
  AverCert.PlanCheck.constructPlanMatchesSymRawPlan s p
example : sourceDropSym badEmptySym honestSingleton = true := rfl
example : sourceAccepted badEmptySym honestSingleton = false := rfl

-- PermAttack: the target plan uses every parameter once but swaps head/tail.
-- Dropping ONLY the source/target matcher accepts it; shipped rejects it.
def sourceDropMatch (s : SymRawPlan) (p : ConstructRawPlan) : Bool :=
  AverCert.PlanCheck.checkSymRawPlan s &&
  AverCert.PlanCheck.checkConstructRawPlan p
example : sourceDropMatch honestPrependSym permutedPrepend = true := rfl
example : sourceAccepted honestPrependSym permutedPrepend = false := rfl

-- TypeAttack: the claimed element representation is read from both the list
-- struct and the exported signature. A coherent symbolic relabel cannot turn
-- the fixture's concrete `(ref null 39)` element into another byte type.
example : AverCert.WasmSlice.listConstructStructTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 28 (.nullableRef 39) = true := rfl
example : AverCert.WasmSlice.listConstructFuncTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 63 1 28 (.nullableRef 39) = true := rfl
example : AverCert.WasmSlice.listConstructStructTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 28 .eqref = false := rfl
example : AverCert.WasmSlice.listConstructFuncTypeMatches
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen 63 1 28 .eqref = false := rfl

-- CountAttack: predicate-level copy of constructPlanAccepted dropping ONLY
-- `plan.fields.length = fieldCount`.
def constructDropCount
    (modBytes modLen : Nat) (exportNameBytes : ByteSeq) (exportName : String)
    (carrier structIdx : Nat) (elemTy : ConstructValType)
    (symPlan : SymRawPlan) (plan : ConstructRawPlan) (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧ obligation.carrier = carrier ∧
  AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
  AverCert.PlanCheck.constructPlanMatchesSymRawPlan symPlan plan = true ∧
  AverCert.PlanCheck.checkConstructRawPlan plan = true ∧
  ∃ body codeEntry binding,
    AverCert.PlanLower.lowerConstructBody structIdx plan = some body ∧
    AverCert.PlanBytes.lowerConstructCodeEntry carrier structIdx plan = some codeEntry ∧
    AverCert.WasmSlice.exactFuncBindingForExport
      modBytes modLen exportNameBytes codeEntry = some binding ∧
    binding.funcIdx = obligation.self ∧
    AverCert.WasmSlice.listConstructStructTypeMatches modBytes modLen structIdx elemTy = true ∧
    AverCert.WasmSlice.listConstructFuncTypeMatches
      modBytes modLen binding.typeIdx plan.arity structIdx elemTy = true ∧
    obligation.code binding.funcIdx = some { arity := plan.arity, nlocals := 1, body := body }
example : constructDropCount AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen honestClaim.exportNameBytes
    honestClaim.exportName honestClaim.carrier honestClaim.structIdx honestClaim.elemTy
    honestClaim.symPlan honestSingleton honestClaim.obligation :=
  ⟨rfl, rfl, rfl, rfl, rfl, _, _, _, rfl, rfl, rfl, rfl,
    AverCert.Plans.singletonJsonEntriesConstructStructTypeMatches,
    AverCert.Plans.singletonJsonEntriesConstructFuncTypeMatches, rfl⟩
example : ¬ AverCert.AcceptedArtifact.constructPlanAccepted
    AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen honestClaim.exportNameBytes honestClaim.exportName
    honestClaim.carrier honestClaim.structIdx 3 honestClaim.elemTy honestClaim.symPlan
    honestSingleton honestClaim.obligation := by
  intro h
  rcases h with ⟨_, _, _, _, _, hcount, _⟩
  exact (by decide : honestSingleton.fields.length ≠ 3) hcount

-- ByteAttack: predicate-level copy dropping ONLY the exact binding lookup,
-- while retaining the deliberately weaker export-name lookup.
def tamperedBytes :=
  let shift := 8 * __BYTE_OFFSET__
  AverCert.ArtifactBytes.modBytes -
      (((AverCert.ArtifactBytes.modBytes >>> shift) &&& 0xff) <<< shift) +
    (24 <<< shift)
def constructDropByteExact
    (modBytes modLen : Nat) (exportNameBytes : ByteSeq) (exportName : String)
    (carrier structIdx fieldCount : Nat) (elemTy : ConstructValType)
    (symPlan : SymRawPlan) (plan : ConstructRawPlan) (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧ obligation.carrier = carrier ∧
  AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
  AverCert.PlanCheck.constructPlanMatchesSymRawPlan symPlan plan = true ∧
  AverCert.PlanCheck.checkConstructRawPlan plan = true ∧ plan.fields.length = fieldCount ∧
  ∃ body codeEntry binding,
    AverCert.PlanLower.lowerConstructBody structIdx plan = some body ∧
    AverCert.PlanBytes.lowerConstructCodeEntry carrier structIdx plan = some codeEntry ∧
    AverCert.WasmSlice.funcBindingForExport modBytes modLen exportNameBytes = some binding ∧
    binding.funcIdx = obligation.self ∧
    AverCert.WasmSlice.listConstructStructTypeMatches modBytes modLen structIdx elemTy = true ∧
    AverCert.WasmSlice.listConstructFuncTypeMatches
      modBytes modLen binding.typeIdx plan.arity structIdx elemTy = true ∧
    obligation.code binding.funcIdx = some { arity := plan.arity, nlocals := 1, body := body }
example : constructDropByteExact tamperedBytes AverCert.ArtifactBytes.modLen honestClaim.exportNameBytes honestClaim.exportName
    honestClaim.carrier honestClaim.structIdx honestClaim.fieldCount honestClaim.elemTy
    honestClaim.symPlan honestSingleton honestClaim.obligation :=
  ⟨rfl, rfl, rfl, rfl, rfl, rfl, _, _, _, rfl, rfl, rfl, rfl,
    AverCert.Plans.singletonJsonEntriesConstructStructTypeMatches,
    AverCert.Plans.singletonJsonEntriesConstructFuncTypeMatches, rfl⟩
example : ¬ AverCert.AcceptedArtifact.constructPlanAccepted tamperedBytes AverCert.ArtifactBytes.modLen
    honestClaim.exportNameBytes honestClaim.exportName honestClaim.carrier honestClaim.structIdx
    honestClaim.fieldCount honestClaim.elemTy honestClaim.symPlan honestSingleton
    honestClaim.obligation := by
  intro h
  rcases h with ⟨_, _, _, _, _, _, _, codeEntry, binding, _, hlower, hexact, _⟩
  have hcode :
      (AverCert.PlanBytes.lowerConstructCodeEntry
        honestClaim.carrier honestClaim.structIdx honestSingleton).get! = codeEntry := by
    simpa using congrArg Option.get! hlower
  rw [← hcode] at hexact
  have rejected : AverCert.WasmSlice.exactFuncBindingForExport
      tamperedBytes AverCert.ArtifactBytes.modLen honestClaim.exportNameBytes
      (AverCert.PlanBytes.lowerConstructCodeEntry
        honestClaim.carrier honestClaim.structIdx honestSingleton).get! = none := by rfl
  rw [rejected] at hexact
  contradiction

-- ZeroAttack: the canonical byte lowering declares exactly one carrier local.
-- A zero-local code table keeps the honest body but is accepted only when the
-- exact locals conjunct is dropped.
def zeroCode : CodeTbl := fun fn =>
  (CertModule.singletonJsonEntriesCode fn).map (fun c => { c with nlocals := 0 })
def localsAccepted (code : CodeTbl) : Bool :=
  match code 20 with
  | some c => c.arity == 1 && c.nlocals == 1 && c.body.length == 3
  | none => false
def localsDropCount (code : CodeTbl) : Bool :=
  match code 20 with
  | some c => c.arity == 1 && c.body.length == 3
  | none => false
example : localsDropCount zeroCode = true := rfl
example : localsAccepted zeroCode = false := rfl

-- OrphanAttack: family-local predicate copies. Each weakens exactly one of the
-- two coverage guards while retaining per-claim constructPlanAccepted checks.
def orphanPlans : List (String × ConstructRawPlan) :=
  AverCert.manifest.constructPlans ++ [("orphan", honestSingleton)]
def constructFamilyDropNames (manifest : Manifest) (claims : List AverCert.AcceptedArtifact.ConstructClaim) : Prop :=
  AverCert.AcceptedArtifact.constructClaimsAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen manifest claims ∧
  (AverCert.AcceptedArtifact.constructClaimExportNames claims).Nodup
def orphanManifest : Manifest := { AverCert.manifest with constructPlans := orphanPlans }
example : constructFamilyDropNames orphanManifest AverCert.Artifact.constructClaims := by
  constructor
  · constructor
    · exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, _, _, _, rfl, rfl, rfl, rfl,
        Or.inr AverCert.Plans.singletonJsonEntriesConstructStructTypeMatches,
        Or.inr AverCert.Plans.singletonJsonEntriesConstructFuncTypeMatches, rfl⟩
    · constructor
      · exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, _, _, _, rfl, rfl, rfl, rfl,
          Or.inr AverCert.Plans.prependJsonEntryConstructStructTypeMatches,
          Or.inr AverCert.Plans.prependJsonEntryConstructFuncTypeMatches, rfl⟩
      · trivial
  · decide

def dupClaims := [honestClaim, honestClaim]
def dupPlans := [(honestClaim.exportName, honestSingleton),
  (honestClaim.exportName, permutedPrepend)]
def dupManifest : Manifest := { AverCert.manifest with constructPlans := dupPlans }
def constructFamilyDropNodup (manifest : Manifest) (claims : List AverCert.AcceptedArtifact.ConstructClaim) : Prop :=
  AverCert.AcceptedArtifact.constructClaimsAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen manifest claims ∧
  AverCert.AcceptedArtifact.constructClaimExportNames claims =
    AverCert.AcceptedArtifact.constructManifestPlanNames manifest
example : constructFamilyDropNodup dupManifest dupClaims := by
  constructor
  · constructor
    · exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, _, _, _, rfl, rfl, rfl, rfl,
        Or.inr AverCert.Plans.singletonJsonEntriesConstructStructTypeMatches,
        Or.inr AverCert.Plans.singletonJsonEntriesConstructFuncTypeMatches, rfl⟩
    · constructor
      · exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, _, _, _, rfl, rfl, rfl, rfl,
          Or.inr AverCert.Plans.singletonJsonEntriesConstructStructTypeMatches,
          Or.inr AverCert.Plans.singletonJsonEntriesConstructFuncTypeMatches, rfl⟩
      · trivial
  · decide
example : ¬ (AverCert.AcceptedArtifact.constructClaimExportNames dupClaims).Nodup := by decide
"#.replace("__BYTE_OFFSET__", &tamper_offset.to_string());
    std::fs::write(cert.join("ListConstructGuardIso.lean"), lean).unwrap();
    let check = lake_for_cert(&cert)
        .current_dir(&cert)
        .arg("env")
        .arg("lean")
        .arg("ListConstructGuardIso.lean")
        .output()
        .expect("run List constructor guard isolation transcript");
    assert!(
        check.status.success(),
        "List constructor guard isolation transcript failed:\n{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    );
    let _ = std::fs::remove_dir_all(out_dir);
}

#[test]
fn int_dispatch_kernel_guards_are_isolating() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping int-dispatch guard isolation test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    let mut lean = String::new();
    lean.push_str(
        "import Schema\nimport PlanCheck\nimport PlanBytes\nimport WasmSlice\nimport AcceptedArtifact\nimport Manifest\nimport Module\nimport Plans\n\n",
    );
    lean.push_str("open AverCert\nopen AverCert.Schema\nopen CertPrelude\n");
    lean.push_str("set_option maxRecDepth 100000\n\n");
    lean.push_str("def packLE : List Nat → Nat | [] => 0 | b :: bs => b + (packLE bs <<< 8)\n\n");
    // Minimal modules: header, type section, then a shared func/export/code
    // tail. `f` is func 0 of type 0; code entry `[2, 0, 11]`. Only the type
    // section differs between the two: type 0 is one nominal-root ref ->
    // `[(ref null 5)]` vs two nominal-root refs -> `[(ref null 5)]`.
    // NOTE: these hand-built modules have a result-bearing signature with a
    // body returning nothing, so they would FAIL the wasmparser `validate_all`
    // chokepoint — they demonstrate slicer/guard DISCRIMINATION only, not
    // end-to-end admission; the E2E tamper vectors in
    // `cert_verify_declines_tampered_int_dispatch_plan` close the end-to-end
    // gap on a real validated artifact.
    lean.push_str("def hdr : List Nat := [0, 97, 115, 109, 1, 0, 0, 0]\n");
    lean.push_str("def unaryType : List Nat := [1, 8, 1, 96, 1, 99, 4, 1, 99, 5]\n");
    lean.push_str("def binaryType : List Nat := [1, 10, 1, 96, 2, 99, 4, 99, 4, 1, 99, 5]\n");
    lean.push_str(
        "def tailSecs : List Nat := [3, 2, 1, 0, 7, 5, 1, 1, 102, 0, 0, 10, 4, 1, 2, 0, 11]\n",
    );
    lean.push_str("def unaryMod : List Nat := hdr ++ unaryType ++ tailSecs\n");
    lean.push_str("def binaryMod : List Nat := hdr ++ binaryType ++ tailSecs\n");
    lean.push_str("def nameF : List Nat := [102]\n\n");
    // SIGNATURE isolation: the byte-equality gate's inputs are identical...
    lean.push_str("example : WasmSlice.funcBindingForExport (packLE unaryMod) unaryMod.length nameF = WasmSlice.funcBindingForExport (packLE binaryMod) binaryMod.length nameF := rfl\n");
    lean.push_str("example : WasmSlice.codeEntryForExport (packLE unaryMod) unaryMod.length nameF = WasmSlice.codeEntryForExport (packLE binaryMod) binaryMod.length nameF := rfl\n");
    lean.push_str("example : WasmSlice.exactFuncBindingForExport (packLE unaryMod) unaryMod.length nameF [2, 0, 11] = WasmSlice.exactFuncBindingForExport (packLE binaryMod) binaryMod.length nameF [2, 0, 11] := rfl\n");
    // ...and only the signature guard tells unary from binary.
    lean.push_str(
        "example : WasmSlice.verbatimFuncTypeMatches (packLE unaryMod) unaryMod.length 0 (.refNull 5) = true := rfl\n",
    );
    lean.push_str(
        "example : WasmSlice.verbatimFuncTypeMatches (packLE binaryMod) binaryMod.length 0 (.refNull 5) = false := rfl\n\n",
    );
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
    lean.push_str("example : PlanBytes.lowerIntDispatchCodeEntry 5 [(.box, 7), (.add, 8)] planSub = none := rfl\n\n");

    // HOST-BUILDER EQUALITY isolation, on the REAL fixture obligations (role
    // table box 7 / add 8 / sub 9). The acceptance requires the whole
    // `obligation.host` to EQUAL the canonical builder for the claimed table —
    // extensionally, so no unsampled slot behaviour is left free.
    lean.push_str("def honestTbl : List (HostRole × Nat) := [(.box, 7), (.add, 8), (.sub, 9)]\n");
    lean.push_str("def permTbl : List (HostRole × Nat) := [(.box, 7), (.add, 9), (.sub, 8)]\n");
    lean.push_str("def honestGauge : IntDispatchRawPlan := { profile := \"int-dispatch-v1\", body := .test 3 (.hostOp .sub (0) true) (.test 4 (.hostOp .add (9) false) (.test 5 (.proj) (.default (7)))) }\n");
    lean.push_str("def permGauge : IntDispatchRawPlan := { profile := \"int-dispatch-v1\", body := .test 3 (.hostOp .add (0) true) (.test 4 (.hostOp .sub (9) false) (.test 5 (.proj) (.default (7)))) }\n");
    // A distinguishing observer for the ≠ proofs (NOT part of the acceptance —
    // the guard is the whole-builder equality; this only exhibits one input on
    // which two unequal builders differ).
    lean.push_str(concat!(
        "def hostBuilderProbe\n",
        "    (h : (List WVal → Option WVal) → (List WVal → Option WVal) →\n",
        "         (List WVal → Option WVal) → (List WVal → Option WVal) →\n",
        "         (Nat → List WVal → Option WVal) →\n",
        "         (List WVal → Option WVal) → CertPrelude.HostTbl)\n",
        "    (idx : Nat) (args : List WVal) : Option Int :=\n",
        "  match h (fun _ => some (.i64v 1)) (fun _ => some (.i64v 2)) (fun _ => some (.i64v 3)) (fun _ => some (.i64v 4)) (fun _ _ => some (.i64v 5)) (fun _ => some (.i64v 6)) idx with\n",
        "  | some (_, f) =>\n",
        "      match f args with\n",
        "      | some (.i64v k) => some k\n",
        "      | some (.structv _ (.i64v k :: _)) => some (1000 + k)\n",
        "      | some _ => some 999\n",
        "      | none => none\n",
        "  | none => none\n\n",
    ));
    // The permuted (plan, table) pair lowers BYTE-IDENTICALLY...
    lean.push_str("example : PlanBytes.lowerIntDispatchCodeEntry 11 permTbl permGauge = PlanBytes.lowerIntDispatchCodeEntry 11 honestTbl honestGauge := rfl\n");
    // ...and every sibling guard is blind to the permutation...
    lean.push_str("example : PlanCheck.checkIntDispatchRawPlan permGauge = true := rfl\n");
    lean.push_str("example : PlanCheck.hostTableIndicesDistinct permTbl = true := rfl\n");
    // ...the honest tables close the equality by `rfl` (whole-builder defeq,
    // incl. the box-only widened match)...
    lean.push_str("example : AverCert.gaugeOb.host = AcceptedArtifact.intDispatchCanonicalHost 11 honestTbl := rfl\n");
    lean.push_str("example : AverCert.boxIntOb.host = AcceptedArtifact.intDispatchCanonicalHost 11 [(.box, 7)] := rfl\n");
    // ...and ONLY the equality conjunct rejects the permuted table: the honest
    // obligation wires index 8 to the add slot, the permuted canonical wires it
    // to sub, and `hostBuilderProbe` exhibits the divergence.
    lean.push_str("example : hostBuilderProbe AverCert.gaugeOb.host 8 [] = some 1 := rfl\n");
    lean.push_str("example : hostBuilderProbe (AcceptedArtifact.intDispatchCanonicalHost 11 permTbl) 8 [] = some 2 := rfl\n");
    lean.push_str(concat!(
        "example : AverCert.gaugeOb.host ≠ AcceptedArtifact.intDispatchCanonicalHost 11 permTbl := by\n",
        "  intro h\n",
        "  have honest : hostBuilderProbe AverCert.gaugeOb.host 8 [] = some 1 := rfl\n",
        "  rw [h] at honest\n",
        "  exact absurd honest (by decide)\n\n",
    ));
    // SAMPLED-PROBE ESCAPE (attack (b)): a host builder whose box slot behaves
    // canonically ONLY at one probed input (`i64 0`) and traps on every real
    // constant — a point probe accepts it (equal observation at the probe
    // point), the EXTENSIONAL equality rejects it.
    lean.push_str(concat!(
        "def sneakyBoxHost :\n",
        "    (List WVal → Option WVal) → (List WVal → Option WVal) →\n",
        "    (List WVal → Option WVal) → (List WVal → Option WVal) →\n",
        "    (Nat → List WVal → Option WVal) →\n",
        "    (List WVal → Option WVal) → CertPrelude.HostTbl :=\n",
        "  fun _ _ _ _ _ _ => fun fn =>\n",
        "    if fn = 7 then\n",
        "      some (1, fun args => match args with\n",
        "        | [WVal.i64v 0] => CertPrelude.boxRef 11 args\n",
        "        | _ => none)\n",
        "    else none\n",
    ));
    // At the probe point the sneaky builder is indistinguishable from canonical...
    lean.push_str("example : hostBuilderProbe sneakyBoxHost 7 [.i64v 0] = hostBuilderProbe (AcceptedArtifact.intDispatchCanonicalHost 11 [(.box, 7)]) 7 [.i64v 0] := rfl\n");
    // ...but it traps on a real constant where canonical boxes it...
    lean.push_str("example : hostBuilderProbe sneakyBoxHost 7 [.i64v 7] = none := rfl\n");
    lean.push_str("example : hostBuilderProbe (AcceptedArtifact.intDispatchCanonicalHost 11 [(.box, 7)]) 7 [.i64v 7] = some 1007 := rfl\n");
    // ...so the extensional equality rejects it.
    lean.push_str(concat!(
        "example : sneakyBoxHost ≠ AcceptedArtifact.intDispatchCanonicalHost 11 [(.box, 7)] := by\n",
        "  intro h\n",
        "  have sneaky : hostBuilderProbe sneakyBoxHost 7 [.i64v 7] = none := rfl\n",
        "  rw [h] at sneaky\n",
        "  exact absurd sneaky (by decide)\n\n",
    ));

    // LOCALS-COUNT bind: the acceptance pins the code table's locals count to
    // the canonical byte-derived value; a zero-locals table (whose body traps
    // on its first `local.set`) diverges from it.
    lean.push_str("example : (CertModule.boxIntCode 1).map (fun c => c.nlocals) =\n  some (PlanCheck.intDispatchArmCount Plans.boxIntIntDispatchPlan.body + 2) := rfl\n");
    lean.push_str("def zeroLocalsBox : CertPrelude.CodeTbl :=\n  fun fn => (CertModule.boxIntCode fn).map (fun c => { c with nlocals := 0 })\n");
    lean.push_str("example : ¬ ((zeroLocalsBox 1).map (fun c => c.nlocals) =\n  some (PlanCheck.intDispatchArmCount Plans.boxIntIntDispatchPlan.body + 2)) := by decide\n");

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
    let honest_build = lake_for_cert(&cert)
        .arg("build")
        .current_dir(&cert)
        .output()
        .expect("lake build runs");
    assert!(honest_build.status.success(), "honest cert must lake-build");

    std::fs::write(cert.join("GuardIso.lean"), lean).unwrap();
    let elab = lake_for_cert(&cert)
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

/// End-to-end acceptance and fail-closed tamper coverage for the fused
/// `Option.withDefault(Vector.get(vec, idx), d)` face: a `cellAt`-shaped
/// export reaches CERTIFIED, and each of the three template holes an attacker
/// could try to move — the literal default, the array type index, and the
/// to-index/box helper wiring — is pinned by the byte-equality gate (with the
/// audited encoder equality and the nominal type gate behind it), so a
/// consistent rewrite of the attacker-editable plan data is DECLINED, never
/// re-credited.
#[test]
fn cert_verify_accepts_fused_vector_read_and_declines_three_tampers() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping fused vector-read verify test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-fused-vector-read");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/cell_at.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify cell_at failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let wasm = out_dir.join("cell_at.wasm");
    let cert = out_dir.join("cert");

    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(ok, "fused vector read must verify CERTIFIED:\n{report}");
    assert!(
        report.contains("CERTIFIED") && report.contains("cellAt"),
        "verdict must credit cellAt:\n{report}"
    );

    // Recover the emitted template holes from the public plan data so the
    // tampers stay robust to shifting module indices.
    let plans_text = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    let marker = ".vectorGetOrDefault ";
    let at = plans_text
        .find(marker)
        .expect("Plans.lean carries the fused vector-read node");
    let tail = &plans_text[at + marker.len()..];
    let mut holes = tail.split_whitespace();
    let arr_ty: u32 = holes.next().unwrap().parse().expect("arrTy hole");
    let to_index_idx: u32 = holes.next().unwrap().parse().expect("toIndex hole");
    let box_idx: u32 = holes.next().unwrap().parse().expect("box hole");
    assert_ne!(to_index_idx, box_idx, "helper roles must be distinct");

    let tamper = |name: &str, edit: &dyn Fn(&str) -> String| {
        let dir = temp_dir(&format!("cert-fused-vector-read-{name}"));
        copy_dir(&out_dir, &dir);
        let plans = dir.join("cert").join("Plans.lean");
        let text = std::fs::read_to_string(&plans).unwrap();
        let edited = edit(&text);
        assert_ne!(text, edited, "tamper `{name}` must change Plans.lean");
        std::fs::write(&plans, edited).unwrap();
        let (ok, out) = aver_verify(&dir.join("cell_at.wasm"), &dir.join("cert"));
        assert!(!ok, "tamper `{name}` must be DECLINED:\n{out}");
        assert!(
            out.contains("DECLINED"),
            "tamper `{name}` must report a decline verdict, not an error:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "tamper `{name}` must never re-credit the export:\n{out}"
        );
        let _ = std::fs::remove_dir_all(&dir);
    };

    // (1) Flip the literal default consistently in the source AND encoded
    //     plans: the encoder equality still holds, but the rendered bytes
    //     (`i64.const 1`) no longer match the module (`i64.const 0`).
    tamper("default-literal", &|text| {
        text.replace(
            &format!(".vectorGetOrDefault {arr_ty} {to_index_idx} {box_idx} (0 : Int)"),
            &format!(".vectorGetOrDefault {arr_ty} {to_index_idx} {box_idx} (1 : Int)"),
        )
        .replace(
            ".vectorGetOrDefault \"Vector<Int>\" (0 : Int)",
            ".vectorGetOrDefault \"Vector<Int>\" (1 : Int)",
        )
    });

    // (2) Flip the array type index (plan node + struct table): the rendered
    //     `array.get` immediate and the nominal array-element gate both break.
    tamper("array-type", &|text| {
        text.replace(
            &format!(".vectorGetOrDefault {arr_ty} {to_index_idx} {box_idx}"),
            &format!(
                ".vectorGetOrDefault {} {to_index_idx} {box_idx}",
                arr_ty + 1
            ),
        )
        .replace(
            &format!("(\"Vector<Int>\", {arr_ty})"),
            &format!("(\"Vector<Int>\", {})", arr_ty + 1),
        )
    });

    // (3) Swap the to-index and box helper indices in the encoded plan: the
    //     audited encoder (driven by the byte-derived role table) can no
    //     longer reproduce the claimed representation plan.
    tamper("helper-swap", &|text| {
        text.replace(
            &format!(".vectorGetOrDefault {arr_ty} {to_index_idx} {box_idx}"),
            &format!(".vectorGetOrDefault {arr_ty} {box_idx} {to_index_idx}"),
        )
    });

    let _ = std::fs::remove_dir_all(&out_dir);
}
