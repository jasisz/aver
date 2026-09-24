//! Integration tests for `aver cert verify` — the tripwires ARE the product.
//!
//! Compiles a fixture with `--certify`, confirms `aver cert verify` accepts it
//! end to end, then confirms it fails closed on each tampering class. Each class
//! is one `cert_tripwire_` test carrying the letter tag used below:
//!   (a) one flipped wasm byte           → artifact hash mismatch
//!   (b) a package `Module.lean` is ignored (checker-owned)
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
//!   (p) bytes-vs-data, plan divergence: a `Plans.lean` plan of `sumTo` that
//!       still types but lowers to a different body than the function's code
//!       entry → the per-plan acceptance fails → DECLINED, never CERTIFIED
//!   (q) shadow decoy: the active plan mutated PLUS its byte-honest text
//!       re-planted in a `namespace Shadow`. `fnPlans` names the active plan,
//!       so the decoy changes nothing → DECLINED
//!   (r) comment decoy: the active plan mutated PLUS its honest text in a
//!       `/- … -/` block comment. Dead text → DECLINED
//!   (s) plan decouple: `sumTo`'s `fnPlans` entry names a decoy plan that
//!       types at its signature; the wall lowers whatever plan the entry names
//!       and pins it to the export's code entry → DECLINED
//!   (t) self decouple: `sumTo`'s entry declares a wrong function index; the
//!       export binding compares it with the bound function's → DECLINED
//!   (u) export-name relabel of an honest plan to a duplicate name → DECLINED
//!   (v) plan decouple on the two-argument accumulator `countDown` → DECLINED
//!   (w) rebound single-byte tampers of the bytes the wall pins beyond the
//!       plans — the divmod helper template, a string literal's data segment,
//!       a constructor struct's finality — each DECLINED inside Lean
//!   (x) plan DATA drift: mutating `Plans.lean` fails its typing, lowering,
//!       or exact-byte binding inside Lean
//!   (y) package-format drift: an unknown `format.version` is rejected rather
//!       than reinterpreted under the current parser
//!   (z) wall drift: an unknown aggregate `wall_id` is rejected before Lean
//!       build; resolution has no filesystem, environment, or network fallback
//!   (aa) package authority: `Plans.lean` is the sole public plan DATA module;
//!       a hostile `ArtifactBytes.lean` decoy is ignored and checker-generated
//!       from the artifact
//!   (ab) artifact-data decoy: cert-supplied `Artifact.data` must bind its
//!       byte/manifest fields and satisfy the checker-owned Lean predicate
//!   (ac) artifact-root axiom: an acceptance fact proved from a carried axiom
//!       is rejected by the axiom audit of the artifact root
//! plus a separate empty-cert test: zero certified exports must NOT print the
//! green path and must exit nonzero, and the A5 report-line injection payload
//! (in the manifest and/or JSON) is rejected by the charset gate.
//!
//! Gated behind `wasm` (the `--certify` path needs the wasm-gc backend) and
//! skipped when `lake` is unavailable, mirroring `cert_certify_spec.rs`.
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
use std::path::{Path, PathBuf};
use std::process::Command;

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

fn rebind_cert_wasm_hash(dir: &Path, bytes: &[u8]) {
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    let old_hash = m["wasm_sha256"].as_str().unwrap().to_string();
    let new_hash = aver::codegen::cert::sha256_hex(bytes);
    let path = dir.join("cert").join("Manifest.lean");
    let src = std::fs::read_to_string(&path).unwrap();
    assert!(
        src.contains(&old_hash),
        "Manifest.lean should pin the old hash"
    );
    std::fs::write(&path, src.replace(&old_hash, &new_hash)).unwrap();
    m["wasm_sha256"] = serde_json::Value::String(new_hash);
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
}

fn replace_once(path: &Path, needle: &str, replacement: &str) {
    let src = std::fs::read_to_string(path).unwrap();
    assert!(
        src.contains(needle),
        "{} should contain `{needle}`",
        path.display()
    );
    std::fs::write(path, src.replacen(needle, replacement, 1)).unwrap();
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
    if !lean_required::lake_available() {
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
}

/// The `fnPlans` entry of export `name` in an emitted `Plans.lean`:
/// `⟨"name", exported, funcIdx, group, planDef⟩`.
fn plan_entry(plans: &str, name: &str) -> String {
    let head = format!("⟨\"{name}\", ");
    let at = plans
        .find(&head)
        .unwrap_or_else(|| panic!("Plans.lean has no fnPlans entry for `{name}`"));
    let end = plans[at..].find('⟩').expect("the entry closes") + at + '⟩'.len_utf8();
    plans[at..end].to_string()
}

/// The fields of a `fnPlans` entry: `(exported, funcIdx, group, planDef)`.
fn plan_entry_fields(plans: &str, name: &str) -> (bool, u32, u32, String) {
    let entry = plan_entry(plans, name);
    let inner = entry
        .trim_start_matches('⟨')
        .trim_end_matches('⟩')
        .to_string();
    let fields: Vec<&str> = inner.split(',').map(str::trim).collect();
    assert_eq!(fields.len(), 5, "fnPlans entry shape changed: {entry}");
    (
        fields[1] == "true",
        fields[2].parse().expect("function index"),
        fields[3].parse().expect("call group"),
        fields[4].to_string(),
    )
}

/// The full `def {def} : FnPlan := …` block of an emitted `Plans.lean` (doc
/// comment excluded), up to the blank line that ends it.
fn plan_def_block(plans: &str, def: &str) -> String {
    let head = format!("def {def} : FnPlan :=");
    let at = plans
        .find(&head)
        .unwrap_or_else(|| panic!("Plans.lean has no `{head}`"));
    let end = plans[at..].find("\n\n").expect("the plan block ends") + at;
    plans[at..end].to_string()
}

/// Rewrite, inside the plan block of export `name` only, the first `from` to
/// `to`. Panics when the block does not contain `from`, so a changed plan
/// shape fails here and not as a vacuous decline.
fn tamper_export_plan(plans_path: &Path, name: &str, from: &str, to: &str) {
    let plans = std::fs::read_to_string(plans_path).unwrap();
    let (_, _, _, def) = plan_entry_fields(&plans, name);
    let block = plan_def_block(&plans, &def);
    assert!(
        block.contains(from),
        "the plan of `{name}` no longer contains `{from}`:\n{block}"
    );
    let tampered = block.replacen(from, to, 1);
    std::fs::write(plans_path, plans.replacen(&block, &tampered, 1)).unwrap();
}

/// Compile `fixture` (a path under the repository root) with `--certify` into
/// a scratch directory, and assert the honest package passes the developer
/// preflight. Returns the directory; the artifact is `{stem}.wasm` in it.
fn compile_checked_fixture(fixture: &str, prefix: &str) -> ScratchDir {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir(prefix);
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
        "{fixture} compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let stem = Path::new(fixture).file_stem().unwrap().to_string_lossy();
    let (ok, report) = aver_check(&out_dir.join(format!("{stem}.wasm")), &out_dir.join("cert"));
    assert!(ok, "the honest {fixture} package must check:\n{report}");
    out_dir
}

/// One tamper of an emitted package: `(label, file, from, to)`, where `file`
/// is a package file name and `from` must occur in it. The special file name
/// `plan:<export>` edits only the plan block of that export in `Plans.lean`.
type PackageTamper<'a> = (&'a str, &'a str, &'a str, &'a str);

/// Apply each tamper to a fresh copy of `out_dir` and assert the developer
/// preflight DECLINES it, never crediting an export. Every tamper here edits
/// Lean data the acceptance reads, so the decline is a failed build of the
/// package or a failed checker-witness pin.
fn assert_package_tampers_decline(out_dir: &Path, wasm_name: &str, tampers: &[PackageTamper]) {
    for &(label, file, from, to) in tampers {
        let dir = temp_dir("cert-package-tamper");
        copy_dir(out_dir, &dir);
        let cert = dir.join("cert");
        if let Some(export) = file.strip_prefix("plan:") {
            tamper_export_plan(&cert.join("Plans.lean"), export, from, to);
        } else {
            replace_once(&cert.join(file), from, to);
        }
        let (ok, out) = aver_check(&dir.join(wasm_name), &cert);
        assert!(
            !ok,
            "{label}: the tampered package must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("did not build") || out.contains("does not bind"),
            "{label}: wrong decline reason:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "{label}: the tampered package credited an export:\n{out}"
        );
    }
}

fn compile_cert_goals(prefix: &str) -> (ScratchDir, PathBuf, PathBuf) {
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
structure Subject where\n  artifactHash : String\n  target : String\n  profile : String\n  abi : String\n  artifactRoot : String\n  \
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

const WEAK_FINAL: &str = "import Artifact\nimport AcceptanceSoundness\n\n\
theorem AverCert.Final.cert : AverCert.Schema.Holds manifest := trivial\n\n\
#print axioms AverCert.Final.cert\n";

// Tripwire soundness gates.
//
// These tests share one baseline artifact — the `certprobe2` fixture emitted
// with `--certify` — and differ only in which single tamper they apply before
// demanding a verdict. They used to be one test that ran every check
// sequentially, which made it the critical path of certificate CI.
//
// The cost here is NOT uniform, and the split is arranged around that. A tamper
// the Rust verifier rejects up front (an unsupported schema/format/wall, a
// rebound JSON field, a flipped artifact byte, a candidate outside the charset,
// a cert file the staging gates refuse) costs milliseconds: no Lean process is
// ever started. A tamper of Lean SOURCE inside the cert directory, and every
// gate that must be ACCEPTED, pays a full artifact DATA build plus the checker
// witness — minutes on CI. One test per tamper puts each of those builds on its
// own row so the lanes can run them in parallel, the same way
// `cert_certify_spec.rs` runs its `cert_hostile_model_` family. Prefix in,
// prefix out: the dedicated lanes select `cert_tripwire_` and the `rest` lanes
// exclude exactly it, so a gate added here is run exactly once and needs no
// workflow edit.
//
// The clean certificate is verified end to end ONCE, by
// `cert_tripwire_accepts_clean_certificate_end_to_end`, and not again per gate:
// that keeps the number of full verifications the same as before the split.
// Every gate below pins the REASON it was declined (or, for the two gates that
// must be accepted, the verdict it was granted), so a fixture that stopped
// verifying for an unrelated reason surfaces as a wrong-reason failure rather
// than as a vacuous pass; if the honest certificate itself stops verifying, the
// clean-certificate test is the one that says so.

/// `true` when `lake` is on PATH. Prints the skip note otherwise, exactly as
/// the single tripwire test did before the split.
fn tripwire_lake_available() -> bool {
    if !lean_required::lake_available() {
        eprintln!("skipping cert verify test: `lake` not available");
        return false;
    }
    true
}

/// Emits the shared `certprobe2` tripwire baseline.
///
/// Every gate below runs this itself rather than depending on a baseline built
/// by another test (and therefore another CI lane), so each one fails on its
/// own terms. The compile is a fraction of a second, so duplicating the setup
/// per gate is nearly free — unlike the verification each gate then performs.
///
/// Returns `None` when `lake` is unavailable; the caller then skips, as before.
fn tripwire_baseline(prefix: &str) -> Option<ScratchDir> {
    if !tripwire_lake_available() {
        return None;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir(prefix);

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

    Some(out_dir)
}

/// The freshly emitted certificate verifies end to end through the
/// production clean-cache path: the green report, its export count, and the
/// boundary line it names are all pinned here.
///
/// This is the ONE full clean verification of the family, and the reason no
/// gate below re-verifies a clean certificate: every gate pins the reason it
/// was declined instead, and this test is what fails if the honest artifact
/// stops verifying at all.
#[test]
fn cert_tripwire_accepts_clean_certificate_end_to_end() {
    let Some(out_dir) = tripwire_baseline("certverify-clean") else {
        return;
    };

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
}

/// The real wasip2 producer emits a component-bound package whose embedded core
/// carries exact standard Console, Disk, and Time imports. It closes through
/// the same production verifier as a raw wasm-gc module. No test-side manifest,
/// hash, envelope, capability, or Lean rewriting is allowed on this path.
#[cfg(feature = "wasip2")]
#[test]
fn cert_tripwire_accepts_produced_wasip2_wasi_imports_end_to_end() {
    if !tripwire_lake_available() {
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certverify-wasip2-produced");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tests/fixtures/wasip2_wasi_imports.av")
        .arg("--target")
        .arg("wasip2")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --target wasip2 --certify runs");
    assert!(
        compile.status.success(),
        "wasip2 producer failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let component_path = out_dir.join("wasip2_wasi_imports.component.wasm");
    let cert = out_dir.join("cert");
    let manifest: serde_json::Value = serde_json::from_slice(
        &std::fs::read(cert.join("cert-manifest.json")).expect("certificate manifest"),
    )
    .expect("valid certificate manifest");
    let capabilities = manifest["capabilities"]
        .as_array()
        .expect("capability import list");
    for (module, name) in [
        ("wasi:cli/stdout@0.2.4", "get-stdout"),
        ("wasi:clocks/wall-clock@0.2.4", "now"),
        ("wasi:filesystem/preopens@0.2.4", "get-directories"),
        ("wasi:filesystem/types@0.2.4", "[method]descriptor.stat-at"),
        (
            "wasi:io/streams@0.2.4",
            "[method]output-stream.blocking-write-and-flush",
        ),
    ] {
        assert!(
            capabilities
                .iter()
                .any(|pair| pair["module"] == module && pair["name"] == name),
            "manifest is missing exact WASI import {module}.{name}"
        );
    }

    let (ok, report) = aver_verify_clean_cache(&component_path, &cert);
    assert!(
        ok,
        "producer-emitted wasip2 component certificate should verify:\n{report}"
    );
    assert!(
        report.contains("CERTIFIED") && report.contains("1 certified export"),
        "wasip2 verifier report should name the pure export beside WASI imports:\n{report}"
    );
}

/// Emits the nested-module fixture baseline: a project whose dotted module
/// dependency (`Nested.Deep.Util`) makes the certificate carry a nested model
/// file (`AverModel/Nested/Deep/Util.lean`) that the bridge modules import by
/// its dotted module name. Returns `None` when `lake` is
/// unavailable, mirroring `tripwire_baseline`.
fn nested_module_baseline(prefix: &str) -> Option<ScratchDir> {
    if !tripwire_lake_available() {
        return None;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir(prefix);
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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify nestedmods failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    Some(out_dir)
}

/// A certificate whose model tree carries a nested module file stages, builds,
/// and verifies CERTIFIED end to end. Before nested staging existed the
/// checker silently skipped a nested model file and the build failed on the
/// unresolvable `import AverModel.Nested.Deep.Util`.
#[test]
fn cert_verify_accepts_nested_module_certificate() {
    let Some(out_dir) = nested_module_baseline("certverify-nested-clean") else {
        return;
    };

    let wasm = out_dir.join("app.wasm");
    let cert = out_dir.join("cert");
    assert!(
        cert.join("AverModel")
            .join("Nested")
            .join("Deep")
            .join("Util.lean")
            .is_file(),
        "fixture must emit its dependency model at a nested path"
    );
    let (ok, report) = aver_verify_clean_cache(&wasm, &cert);
    assert!(
        ok,
        "expected nested-module certificate to verify, got:\n{report}"
    );
    assert!(report.contains("CERTIFIED"), "missing CERTIFIED:\n{report}");
    assert!(
        report.contains("6 certified exports"),
        "expected the entry exports and all three nested-module exports:\n{report}"
    );
    assert!(
        report.contains("Nested_Deep_Util_combine")
            && report.contains("Nested_Deep_Util_bump")
            && report.contains("Nested_Deep_Util_tally"),
        "the exports whose models live in the nested module must certify:\n{report}"
    );
}

/// A module carrying records verifies end to end.
///
/// The model has to state each record's default value itself, because the
/// certificate model does not derive `Inhabited`. Stating that value as
/// the record's own default made the instance its own premise, so a model
/// carrying a one-field record never built and its certificate was DECLINED.
/// The fixture pairs a one-field record with a record whose field is that
/// record: the witness has to name fields, AND the inner record's witness has
/// to be stated before the outer one uses it.
#[test]
fn cert_verify_accepts_record_carrying_model() {
    if !tripwire_lake_available() {
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certverify-record-model");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/offsetrec.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify offsetrec failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("offsetrec.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify_clean_cache(&wasm, &cert);
    assert!(
        ok,
        "expected the record-carrying certificate to verify, got:\n{report}"
    );
    assert!(report.contains("CERTIFIED"), "missing CERTIFIED:\n{report}");
}

/// A model carrying a recursive sum type with no nullary constructor builds
/// and its certificate verifies end to end.
///
/// The model states each sum type's `Inhabited` witness itself (the
/// certificate model does not derive it). The witness used to default the FIRST
/// constructor's arguments whenever no nullary constructor existed — for
/// `Chain = More(Chain) | Stop(Int)` that stated `⟨Chain.more default⟩`,
/// whose `default` asks for the very instance being stated, so the model
/// never built and the certificate was DECLINED. The witness now seeds the
/// first constructor in declaration order whose arguments all bottom out:
/// `⟨Chain.stop default⟩`.
#[test]
fn cert_verify_accepts_recursive_sum_carrying_model() {
    if !tripwire_lake_available() {
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certverify-sum-model");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/chainsum.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify chainsum failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("chainsum.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify_clean_cache(&wasm, &cert);
    assert!(
        ok,
        "expected the sum-carrying certificate to verify, got:\n{report}"
    );
    assert!(report.contains("CERTIFIED"), "missing CERTIFIED:\n{report}");
}

/// A model carrying a sum type whose FIRST constructor holds a refined record
/// builds and its certificate verifies end to end.
///
/// The refined record (`Natural` — single `Int` field plus a validating smart
/// constructor) emits as a `Subtype` abbrev with deliberately NO `Inhabited`
/// instance. The sum's witness scan must therefore skip `Payload.raw` — its
/// `default` argument would ask for `Inhabited Natural`, which does not exist —
/// and seed the nullary `Payload.empty` instead, exactly as Lean's own
/// `deriving Inhabited` skips constructors it cannot inhabit. The certified
/// export `bump` never mentions `Payload`; the model still carries the type and
/// its instance, so a failing instance would decline the whole certificate with
/// "failed to synthesize Inhabited Natural".
#[test]
fn cert_verify_accepts_refined_argument_sum_carrying_model() {
    if !tripwire_lake_available() {
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certverify-refined-sum-model");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/refinedsum.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify refinedsum failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("refinedsum.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify_clean_cache(&wasm, &cert);
    assert!(
        ok,
        "expected the refined-argument-sum certificate to verify, got:\n{report}"
    );
    assert!(report.contains("CERTIFIED"), "missing CERTIFIED:\n{report}");
}

/// A nested `.lean` file that no staged `Manifest.lean`/`Certificate.lean`
/// import admits is ignored outright: it never reaches the build tree, so a
/// planted file that would poison the build (a Lean type error and a banned
/// token) leaves the verdict unchanged.
#[test]
fn cert_verify_ignores_unimported_nested_lean_file() {
    let Some(out_dir) = nested_module_baseline("certverify-nested-unimported") else {
        return;
    };

    let wasm = out_dir.join("app.wasm");
    let cert = out_dir.join("cert");
    let decoy_dir = cert.join("Unimported");
    std::fs::create_dir_all(&decoy_dir).unwrap();
    // If this file were staged, the token scan would decline it; if it were
    // staged past a broken scan, the type error would fail the build. A clean
    // verdict therefore proves it never joined the build tree.
    std::fs::write(
        decoy_dir.join("Decoy.lean"),
        "#eval IO.println \"pwned\"\ndef broken : Nat := \"not a nat\"\n",
    )
    .unwrap();

    let (ok, report) = aver_check(&wasm, &cert);
    assert!(
        ok,
        "unimported nested file must be ignored outright:\n{report}"
    );
    assert!(
        report.contains("CHECKED") && !report.contains("DECLINED"),
        "verdict must be unchanged by the unimported decoy:\n{report}"
    );
}

/// A nested path with a bad segment declines with the filename-gate message
/// before any Lean process, exactly like a hostile flat file name.
#[test]
fn cert_verify_declines_bad_nested_path_segment() {
    let Some(out_dir) = nested_module_baseline("certverify-nested-badseg") else {
        return;
    };

    let wasm = out_dir.join("app.wasm");
    let cert = out_dir.join("cert");
    let dir = cert.join("Nested");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("Bad Name.lean"), "-- inert\ndef x : Nat := 0\n").unwrap();

    let (ok, report) = aver_check(&wasm, &cert);
    assert!(!ok, "bad nested path segment must decline:\n{report}");
    assert!(
        report.contains("Nested/Bad Name.lean")
            && report.contains("^[A-Za-z][A-Za-z0-9_]*\\.lean$"),
        "wrong reason for bad nested segment:\n{report}"
    );
}

/// A nested file whose dotted module name has a checker-owned or toolchain
/// root as a prefix is rejected the same way a flat shadow is.
#[test]
fn cert_verify_declines_nested_shadow_of_checker_root() {
    let Some(out_dir) = nested_module_baseline("certverify-nested-shadow") else {
        return;
    };

    let wasm = out_dir.join("app.wasm");
    let cert = out_dir.join("cert");
    let dir = cert.join("Schema");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("Sub.lean"), "-- inert\ndef x : Nat := 0\n").unwrap();

    let (ok, report) = aver_check(&wasm, &cert);
    assert!(
        !ok,
        "nested shadow of a checker root must decline:\n{report}"
    );
    assert!(
        report.contains("shadows a checker/toolchain import"),
        "wrong reason for nested shadow:\n{report}"
    );
}

/// `ArtifactComponentBytes` is checker-owned just like `ArtifactBytes`: a
/// package must not be able to stage nested DATA under that import prefix.
#[test]
fn cert_verify_declines_nested_shadow_of_component_bytes_root() {
    let Some(out_dir) = nested_module_baseline("certverify-nested-component-bytes-shadow") else {
        return;
    };

    let wasm = out_dir.join("app.wasm");
    let cert = out_dir.join("cert");
    let dir = cert.join("ArtifactComponentBytes");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("Decoy.lean"), "-- inert\ndef x : Nat := 0\n").unwrap();

    let (ok, report) = aver_check(&wasm, &cert);
    assert!(
        !ok,
        "nested ArtifactComponentBytes shadow must decline:\n{report}"
    );
    assert!(
        report.contains("shadows a checker/toolchain import"),
        "wrong reason for nested ArtifactComponentBytes shadow:\n{report}"
    );
}

/// Two staged paths that differ only by ASCII case are rejected at staging:
/// on a case-insensitive filesystem they silently merge and the later write
/// clobbers the earlier one nondeterministically. Authoring both casings
/// requires a case-sensitive filesystem, so this skips (with a note) where
/// the two paths cannot coexist; the unit test on the collision gate itself
/// runs everywhere.
#[test]
fn cert_verify_declines_case_colliding_nested_paths() {
    let Some(out_dir) = nested_module_baseline("certverify-nested-case-collision") else {
        return;
    };

    let wasm = out_dir.join("app.wasm");
    let cert = out_dir.join("cert");

    // Case-sensitivity probe: both casings must be creatable side by side.
    let upper = cert.join("Casing");
    let lower = cert.join("casing");
    std::fs::create_dir_all(&upper).unwrap();
    std::fs::write(upper.join("Store.lean"), "def a : Nat := 0\n").unwrap();
    std::fs::create_dir_all(&lower).unwrap();
    std::fs::write(lower.join("Store.lean"), "def b : Nat := 1\n").unwrap();
    let coexist = std::fs::read_to_string(upper.join("Store.lean")).unwrap()
        != std::fs::read_to_string(lower.join("Store.lean")).unwrap();
    if !coexist {
        eprintln!("skipping case-collision test: staging filesystem is case-insensitive");
        return;
    }

    // Admit both casings so both are actually staged — unimported nested
    // files are ignored before the collision gate could bite.
    let manifest = cert.join("Manifest.lean");
    let src = std::fs::read_to_string(&manifest).unwrap();
    std::fs::write(
        &manifest,
        format!("import Casing.Store\nimport casing.Store\n{src}"),
    )
    .unwrap();

    let (ok, report) = aver_check(&wasm, &cert);
    assert!(!ok, "case-colliding staged paths must decline:\n{report}");
    assert!(
        report.contains("collide case-insensitively"),
        "wrong reason for case collision:\n{report}"
    );
}

/// A cert-data shape this checker does not know is rejected, never
/// reinterpreted under the current schema. Rejected before any Lean process.
#[test]
fn cert_tripwire_declines_unsupported_schema_version() {
    let Some(out_dir) = tripwire_baseline("certverify-schema-version") else {
        return;
    };

    // The cert schema version is a breaking cert-data shape. The checker rejects
    // unsupported manifests instead of trying to reinterpret them under the
    // current schema.
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

/// Schema 6 keeps the artifact target as a required transport discriminator.
/// A missing target is rejected before any Lean process.
#[test]
fn cert_tripwire_declines_missing_artifact_target() {
    let Some(out_dir) = tripwire_baseline("certverify-missing-target") else {
        return;
    };

    let dir = temp_dir("neg-missing-target");
    copy_dir(&out_dir, &dir);
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    m.as_object_mut().unwrap().remove("target");
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "missing target must be rejected:\n{out}");
    assert!(
        out.contains("missing string field `target`"),
        "wrong reason for missing target rejection:\n{out}"
    );
}

/// The current schema admits only the wasm-gc and wasip2 envelope targets;
/// unknown targets are not reinterpreted as core wasm.
#[test]
fn cert_tripwire_declines_unsupported_artifact_target() {
    let Some(out_dir) = tripwire_baseline("certverify-target") else {
        return;
    };

    let dir = temp_dir("neg-target");
    copy_dir(&out_dir, &dir);
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    m["target"] = serde_json::json!("native");
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "unsupported target must be rejected:\n{out}");
    assert!(
        out.contains("unsupported certificate target `native`"),
        "wrong reason for unsupported target rejection:\n{out}"
    );
}

/// Schema 6 also fixes the emitted-fragment profile, so a future profile cannot
/// be accepted under the current wall's target statements.
#[test]
fn cert_tripwire_declines_unsupported_artifact_profile() {
    let Some(out_dir) = tripwire_baseline("certverify-profile") else {
        return;
    };

    let dir = temp_dir("neg-profile");
    copy_dir(&out_dir, &dir);
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    m["profile"] = serde_json::json!("AverUserProfile/v2");
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "unsupported profile must be rejected:\n{out}");
    assert!(
        out.contains("unsupported certificate profile `AverUserProfile/v2`"),
        "wrong reason for unsupported profile rejection:\n{out}"
    );
}

/// Schema 6 fixes the runtime ABI alongside the target/profile pair.
#[test]
fn cert_tripwire_declines_unsupported_artifact_abi() {
    let Some(out_dir) = tripwire_baseline("certverify-abi") else {
        return;
    };

    let dir = temp_dir("neg-abi");
    copy_dir(&out_dir, &dir);
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    m["abi"] = serde_json::json!("aver-wasm-gc/1");
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "unsupported ABI must be rejected:\n{out}");
    assert!(
        out.contains("unsupported certificate ABI `aver-wasm-gc/1`"),
        "wrong reason for unsupported ABI rejection:\n{out}"
    );
}

/// (y) Package-format drift: the package format is versioned independently
/// of the Lean statement schema, and an unknown version is refused up front.
#[test]
fn cert_tripwire_declines_unsupported_format_version() {
    let Some(out_dir) = tripwire_baseline("certverify-format-version") else {
        return;
    };

    // The package format is independently versioned from the Lean statement
    // schema. Unknown versions are never reinterpreted as the current shape.
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

/// (z) Wall drift: an unknown aggregate `wall_id` is rejected before any Lean
/// build, with no filesystem, environment, or network fallback.
#[test]
fn cert_tripwire_declines_unknown_wall_id() {
    let Some(out_dir) = tripwire_baseline("certverify-wall-id") else {
        return;
    };

    // One aggregate identifier commits to every checker-owned Lean source and
    // the exact toolchain. Resolution is embedded-only: no path, URL, or
    // ambient installation is consulted for an unknown wall.
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

/// The JSON must pin the artifact-level certificate root the checker expects.
/// A consumer should not have to guess which theorem is the self-check root.
#[test]
fn cert_tripwire_declines_json_artifact_root_drift() {
    let Some(out_dir) = tripwire_baseline("certverify-artifact-root-pin") else {
        return;
    };

    // The artifact-level certificate root is pinned as routing metadata. A
    // consumer should not have to guess which theorem is the self-check root.
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

/// The LEAN manifest must name the artifact-level certificate root too: JSON
/// consistency alone is not enough, so this one reaches the kernel witness.
#[test]
fn cert_tripwire_declines_lean_artifact_root_drift() {
    let Some(out_dir) = tripwire_baseline("certverify-lean-artifact-root-pin") else {
        return;
    };

    // The Lean manifest must also name the artifact-level certificate root.
    // JSON consistency alone is not enough; the checker witness pins the
    // proven manifest literal and the artifact predicate checks the same root.
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
    // The artifact root proves `subjectMatchesArtifactRoot` by `rfl`, so the
    // drifted root fails the package's own acceptance root; the checker
    // witness pins the same field as its backstop.
    assert!(
        (out.contains("did not build") && out.contains("ArtifactCertificate.lean"))
            || out.contains("manifest.subject.artifactRoot"),
        "wrong reason for Lean artifact root drift:\n{out}"
    );
}

/// (ab) Artifact-data decoy: a cert-supplied `Artifact.data` that points at
/// zero module bytes must bind its byte/manifest fields — it cannot.
#[test]
fn cert_tripwire_declines_tampered_artifact_data() {
    let Some(out_dir) = tripwire_baseline("certverify-artifact-data-pin") else {
        return;
    };

    // The artifact-carried data root is useful metadata, not authority. Since
    // the recursion exports carry byte-origin plan claims, an `Artifact.lean`
    // whose `data` points at zero module bytes can no longer even prove its own
    // claims (`exactFuncBindingForExport 0 modLen name code = some …` has no `rfl`), so the tamper
    // dies at the cert's own build — before the checker witness pins
    // `AverCert.Artifact.data` to the actual bytes and manifest.
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

/// (a) One flipped wasm byte is an artifact hash mismatch, caught before any
/// Lean build.
#[test]
fn cert_tripwire_declines_flipped_wasm_byte() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-a") else {
        return;
    };

    // (a) One flipped wasm byte → hash mismatch, before any build.
    //
    // The byte is picked by CONTENT, not by offset: the flip must leave a
    // structurally valid module, or the checker refuses at wasm validation and
    // the hash pin never gets its turn. The first byte of the `sumTo` export
    // name is such a byte — a name is opaque to validation — and it is unique
    // in the module. An offset-derived choice (this test used the file
    // midpoint) silently changes meaning whenever the emitter's section sizes
    // move, which is what happened when the module gained two runtime exports:
    // the midpoint drifted into a function body and the decline became a
    // validation error instead of the hash mismatch this test exists to pin.
    let dir = temp_dir("neg-a");
    copy_dir(&out_dir, &dir);
    let w = dir.join("certprobe2.wasm");
    let mut bytes = std::fs::read(&w).unwrap();
    let name = b"sumTo";
    let at = bytes
        .windows(name.len())
        .position(|win| win == name)
        .expect("the `sumTo` export name should be present in the wasm");
    bytes[at] ^= 0x01;
    std::fs::write(&w, &bytes).unwrap();
    let (ok, out) = aver_check(&w, &dir.join("cert"));
    assert!(!ok, "flipped wasm byte must fail:\n{out}");
    assert!(out.contains("hash mismatch"), "wrong reason (a):\n{out}");
}

/// (a2) A byte flipped inside the certified `countDown` body is the same hard
/// decline, and is deliberately caught by the hash before any Lean build.
#[test]
fn cert_tripwire_declines_flipped_countdown_body_byte() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-a2") else {
        return;
    };

    // (a2) A byte flipped inside the newly certified `countDown` body is still a
    //      hard decline. This is intentionally caught by the artifact hash
    //      before the checker spends time building Lean.
    let dir = temp_dir("neg-a2-countdown-body");
    copy_dir(&out_dir, &dir);
    let w = dir.join("certprobe2.wasm");
    let mut bytes = std::fs::read(&w).unwrap();
    let count_down_prefix = [
        0x20, 0x01, 0x05, 0x20, 0x00, 0x42, 0x01, 0x10, 0x07, 0x10, 0x09, 0x20, 0x01, 0x20, 0x00,
        0x10, 0x08, 0x12, 0x02,
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

/// (b) `Module.lean` (the artifact hash `Schema.Holds` compares against) is
/// checker-owned: the wall imports it, so the verifier renders it from the
/// bytes it read. A package file of that name, even one pinning a wrong hash,
/// is ignored, and the certificate still checks.
#[test]
fn cert_tripwire_ignores_a_package_module_file() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-b") else {
        return;
    };

    let dir = temp_dir("neg-b");
    copy_dir(&out_dir, &dir);
    let m = dir.join("cert").join("Module.lean");
    assert!(!m.exists(), "the producer must not write Module.lean");
    std::fs::write(
        &m,
        "namespace CertModule\ndef wasmSha256 : String := \"0000\"\nend CertModule\n",
    )
    .unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(ok, "a package Module.lean must be ignored:\n{out}");
}

/// (c) A trivialized final theorem — same name, `: True := trivial` — fails
/// the build, because the artifact-carried self-check root imports it.
#[test]
fn cert_tripwire_declines_trivialized_final_theorem() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-c") else {
        return;
    };

    // (c) A trivialized final theorem (same name, `: True := trivial`) → the
    //     artifact-carried self-check root imports `Final.cert`, so the cert
    //     build fails before the checker witness can ascribe `Final.cert` to
    //     `Holds manifest`.
    let dir = temp_dir("neg-c");
    copy_dir(&out_dir, &dir);
    let f = dir.join("cert").join("Final.lean");
    let trivial = "import Artifact\nimport AcceptanceSoundness\n\n\
         theorem AverCert.Final.cert : True := trivial\n\n\
         #print axioms AverCert.Final.cert\n";
    std::fs::write(&f, trivial).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "trivialized final theorem must fail:\n{out}");
    assert!(out.contains("did not build"), "wrong reason (c):\n{out}");
}

/// (d) A cert-supplied `Schema.lean` is IGNORED — the checker builds against
/// its own embedded audited schema — so the genuine certificate still passes.
#[test]
fn cert_tripwire_ignores_cert_supplied_schema() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-d") else {
        return;
    };

    // (d) A swapped Schema.lean is IGNORED: the checker builds against its own
    //     embedded audited schema, never the cert's. Even a weakened schema in
    //     the cert dir has no effect, so the genuine cert still verifies.
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

/// (e) A1 hash rebind: a different but genuine module, with `wasm_sha256`
/// edited to match it, must not buy acceptance.
#[test]
fn cert_tripwire_declines_hash_rebind_to_foreign_module() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-e") else {
        return;
    };
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // (e) A1 hash rebind: replace the artifact with a DIFFERENT but genuine cert
    //     module (certprobe's wasm) and edit ONLY `wasm_sha256` in the JSON to
    //     match it. The fast JSON pre-check passes and the swapped module still
    //     validates as Wasm. The recursion exports carry byte-origin plan claims that
    //     bind the checker-staged `ArtifactBytes` (the actual, swapped bytes), so
    //     the cert's own build now fails before the checker witness even runs —
    //     an even earlier fail-closed decline than the witness hash face.
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

/// (e2) A1 hash rebind against a CLAIM-FREE certificate: no plan claim can
/// catch this swap, only the artifact-hash pin can, so this gate keeps it
/// exercised.
///
/// Compiles its own `certempty` fixture and never touches the shared
/// `certprobe2` baseline, so it takes the lake check alone.
#[test]
fn cert_tripwire_declines_hash_rebind_on_claim_free_cert() {
    if !tripwire_lake_available() {
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // (e2) A1 hash rebind against a CLAIM-FREE cert: `certempty` proves zero
    //      obligations and ships no plan claims, so its Lean data builds green
    //      over any staged bytes. Appending an inert custom section changes the
    //      artifact hash without perturbing any byte-derived fact, and the JSON
    //      pin is rebound to match — so ONLY the artifact-hash pin can catch the
    //      swap: the package's Lean data still carries the ORIGINAL hash, and
    //      the checker computes the new one from the staged bytes. Since the
    //      checker renders `Module.lean` itself, the kernel meets that mismatch
    //      as soon as the acceptance theorem is applied to the package data
    //      (`artifactHash = CertModule.wasmSha256` fails `rfl`); before, it
    //      surfaced in the witness's hash face. Either way it is the hash face.
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
    assert!(!ok, "A1 hash rebind on claim-free cert must fail:\n{out}");
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "wrong reason (e2):\n{out}"
    );
    // The kernel names the exact face it rejected: the artifact hash.
    assert!(
        out.contains("manifest.subject.artifactHash"),
        "hash face not exercised (e2):\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "hash rebind credited (e2):\n{out}"
    );
}

/// (f) A2 comment smuggle: the approved statement present only in a COMMENT,
/// plus a trivial theorem of the same name.
#[test]
fn cert_tripwire_declines_comment_smuggled_final_theorem() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-f") else {
        return;
    };

    // (f) A2 comment smuggle: the approved statement line present only in a
    //     COMMENT, plus a `theorem AverCert.Final.cert : True := trivial`. The
    //     artifact-carried self-check root imports `Final.cert`, so this now
    //     fails at cert build time before the checker witness.
    let dir = temp_dir("neg-f");
    copy_dir(&out_dir, &dir);
    let f = dir.join("cert").join("Final.lean");
    let smuggled = "import Artifact\nimport AcceptanceSoundness\n\n\
         -- theorem AverCert.Final.cert : AverCert.Schema.Holds manifest := by trivial\n\
         theorem AverCert.Final.cert : True := trivial\n\n\
         #print axioms AverCert.Final.cert\n";
    std::fs::write(&f, smuggled).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "A2 comment smuggle must fail:\n{out}");
    assert!(out.contains("did not build"), "wrong reason (f):\n{out}");
}

/// (g) A3 build-tree subversion: a decoy tree behind a redirected `srcDir`
/// plus a weak final proof. The checker ignores the cert's lakefile.
#[test]
fn cert_tripwire_declines_srcdir_build_tree_subversion() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-g") else {
        return;
    };

    // (g) A3 build-tree subversion: point the cert's lakefile `srcDir` at a
    //     hidden decoy tree whose `Holds := True`, and weaken the final proof to
    //     `trivial`. Under the OLD (cert-controlled) build this passed. The
    //     checker now ignores the cert's lakefile/srcDir and builds `Final.lean`
    //     against its OWN embedded schema, so `trivial` fails closed.
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

/// (h) A3 olean cache: a poisoned `.lake` cache shipped in the certificate is
/// never consumed — the checker builds in a fresh dir — so the genuine
/// certificate still passes.
#[test]
fn cert_tripwire_ignores_shipped_olean_cache() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-h") else {
        return;
    };

    // (h) A3 olean cache: ship a poisoned `.lake` cache in the cert. The checker
    //     builds in a fresh dir and never consumes it, so a genuine cert still
    //     verifies (a checker that reused the cache would choke on the garbage).
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

/// (i) A4 report forgery: a fabricated certified export and contract appended
/// to ONLY the JSON. The report candidates are kernel-bound to the proven
/// manifest, so the forged names are never credited.
#[test]
fn cert_tripwire_declines_forged_report_json() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-i") else {
        return;
    };

    // (i) A4 report forgery: append a fabricated certified export + contract to
    //     ONLY the JSON. The report names/count/contracts are now candidates the
    //     kernel witness binds to the proven Lean manifest with `rfl`, so a JSON
    //     that claims an export or contract the manifest does not have makes a
    //     binding fail: the cert is DECLINED and the forged names never appear.
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
            "class": "source-plan-v1",
            "facets": [],
            "policy": "simulatesModel",
            "level": "L1",
            "theorem": "AcceptanceSoundness.fn_claim_discharges"
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

/// Report labels are paired with exports by Lean, not compared as two bags:
/// every export carries the one plan class, so the per-export label is its
/// facet list, and swapping two distinct facet lists while preserving names
/// and order is declined.
#[test]
fn cert_tripwire_declines_swapped_report_class_pairs() {
    if !tripwire_lake_available() {
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certverify-neg-i-pair-swap-base");
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
    assert!(compile.status.success(), "compose fixture compile failed");

    // `double` reports no facet and `quad` reports `calls`: the wall derives
    // both (`ClaimAxes.reportFacets`) and the witness pins each pair.
    let dir = temp_dir("neg-i-report-pair-swap");
    copy_dir(&out_dir, &dir);
    let mf = dir.join("cert/cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    let certified = m["certified"].as_array_mut().unwrap();
    assert!(certified.len() >= 2);
    let first = certified[0]["facets"].clone();
    let second = certified[1]["facets"].clone();
    assert_ne!(
        first, second,
        "fixture needs two distinct report facet lists"
    );
    certified[0]["facets"] = second;
    certified[1]["facets"] = first;
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

    let (ok, out) = aver_check(&dir.join("compose.wasm"), &dir.join("cert"));
    assert!(!ok, "swapped export/facet pairs must be DECLINED:\n{out}");
    assert!(
        out.contains("does not bind"),
        "wrong report-pair decline:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "swapped facets were credited:\n{out}"
    );
}

/// (j) Drift, JSON claims MORE than the manifest: a charset-clean certified
/// entry that no obligation backs.
#[test]
fn cert_tripwire_declines_json_claiming_an_extra_export() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-j") else {
        return;
    };

    // (j) Drift, JSON claims MORE than the manifest: a second certified entry
    //     whose name is charset-clean but absent from the obligations. The
    //     `obligations.length = N` / export-name bindings fail closed.
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
            "class": "source-plan-v1",
            "facets": [],
            "policy": "simulatesModel",
            "level": "L1",
            "theorem": "AcceptanceSoundness.fn_claim_discharges"
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

/// (k) Drift, JSON claims FEWER than the manifest: an empty `certified` while
/// the manifest still proves an obligation.
#[test]
fn cert_tripwire_declines_json_dropping_a_real_export() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-k") else {
        return;
    };

    // (k) Drift, JSON claims FEWER than the manifest: an empty `certified` while
    //     the manifest still proves one obligation. `length = 0 := rfl` fails.
    let dir = temp_dir("neg-k");
    copy_dir(&out_dir, &dir);
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    // The bridge surface names certified exports only, so a JSON that drops
    // them drops their bridges too; the Lean data is untouched.
    m["certified"] = serde_json::Value::Array(vec![]);
    m["sourceBridges"] = serde_json::Value::Array(vec![]);
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "JSON dropping a real export must fail (k):\n{out}");
    assert!(out.contains("does not bind"), "wrong reason (k):\n{out}");
}

/// (l) Charset gate: a certified name carrying a control character is rejected
/// before any splice, so it can never reach the Lean witness.
#[test]
fn cert_tripwire_declines_control_char_in_candidate_name() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-l") else {
        return;
    };

    // (l) Charset gate: a certified name carrying a control character (decoded
    //     from the JSON) is rejected before any splice, so it can never reach
    //     the Lean witness.
    let dir = temp_dir("neg-l");
    copy_dir(&out_dir, &dir);
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    // The bridge surface is optional transport; dropping it keeps the
    // certified name the first candidate the gate reads.
    m["certified"][0]["name"] = serde_json::Value::String("sumTo\nevil := by rfl".into());
    m["sourceBridges"] = serde_json::Value::Array(vec![]);
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "control char in a candidate must fail (l):\n{out}");
    assert!(out.contains("printable ASCII"), "wrong reason (l):\n{out}");
}

/// (m) Evil axiom: `Final.cert` proved from a smuggled `axiom`. The build
/// succeeds — an axiom is valid Lean — and the witness axiom collector throws.
#[test]
fn cert_tripwire_declines_axiom_backed_final_theorem() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-m") else {
        return;
    };

    // (m) Evil axiom: prove `Final.cert` from a smuggled `axiom evil`. The build
    //     succeeds (an axiom is valid Lean), but the witness runs the kernel's
    //     axiom collector over the ascribed constant and throws on `evil`.
    let dir = temp_dir("neg-m");
    copy_dir(&out_dir, &dir);
    let f = dir.join("cert").join("Final.lean");
    let evil = "import Artifact\nimport AcceptanceSoundness\n\n\
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

/// (n) A7 filename gate: a cert data file whose name is not a plain Lean
/// module identifier is rejected before staging, so it cannot inject tokens
/// into the checker-authored lakefile roots.
#[test]
fn cert_tripwire_declines_hostile_cert_file_name() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-n") else {
        return;
    };

    // (n) A7 filename gate: a cert data file whose name is not a plain Lean
    //     module identifier (a space here) is rejected before staging, so it
    //     cannot inject tokens into the checker-authored lakefile roots.
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

/// (o) A8 token scan: a cert data file carrying an elaboration-executes-code
/// token is rejected before it is staged (deliberately brittle wall).
#[test]
fn cert_tripwire_declines_code_executing_token_in_cert_data() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-o") else {
        return;
    };

    // (o) A8 token scan: a cert data file carrying an elaboration-executes-code
    //     token is rejected before it is staged (deliberately brittle wall).
    let dir = temp_dir("neg-o");
    copy_dir(&out_dir, &dir);
    let c = dir.join("cert").join("Manifest.lean");
    let mut src = std::fs::read_to_string(&c).unwrap();
    src.push_str("\n#eval IO.println \"pwned\"\n");
    std::fs::write(&c, src).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(
        !ok,
        "code-executing token in a data file must fail (o):\n{out}"
    );
    assert!(
        out.contains("contains refused construct `#eval`"),
        "wrong reason (o):\n{out}"
    );
}

/// (p) Bytes-vs-data divergence: a `Plans.lean` plan that still elaborates
/// but whose lowering is not the real code entry. The wasm is untouched, so
/// the mismatch is purely in the Lean data.
#[test]
fn cert_tripwire_declines_diverging_module_body() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-p") else {
        return;
    };

    // (p) bytes-vs-data divergence: `sumTo`'s base case returns 1 instead of 0
    //     in Plans.lean. The plan still types, but its lowering boxes a
    //     different constant, so it is not the function's code entry: the
    //     per-plan acceptance fails. The wasm bytes are untouched, so the hash
    //     stays consistent — the mismatch is purely in the attacker-editable
    //     Lean data.
    let dir = temp_dir("neg-p");
    copy_dir(&out_dir, &dir);
    tamper_export_plan(
        &dir.join("cert").join("Plans.lean"),
        "sumTo",
        "(.literal (.int 0))) (.literal (.int 0))",
        "(.literal (.int 0))) (.literal (.int 1))",
    );
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(
        !ok,
        "a plan that does not bind to the artifact bytes must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
        "wrong reason (p):\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "a diverging plan must never be credited (p):\n{out}"
    );
}

/// (q) Shadow decoy — the reproduced bypass: the ACTIVE plan mutated plus a
/// byte-honest copy re-planted in a `namespace Shadow`. The decoy text does
/// not change what `fnPlans` names.
#[test]
fn cert_tripwire_declines_shadow_namespace_decoy() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-q") else {
        return;
    };

    // (q) Shadow decoy: mutate the ACTIVE `sumTo` plan and re-plant its
    //     byte-honest text in a `namespace Shadow`. `fnPlans` names the active,
    //     mutated definition, not the shadow, so the plan check fails: DECLINED.
    let dir = temp_dir("neg-q");
    copy_dir(&out_dir, &dir);
    let plans_path = dir.join("cert").join("Plans.lean");
    let honest_plans = std::fs::read_to_string(&plans_path).unwrap();
    let (_, _, _, def) = plan_entry_fields(&honest_plans, "sumTo");
    let honest_block = plan_def_block(&honest_plans, &def);
    tamper_export_plan(
        &plans_path,
        "sumTo",
        "(.literal (.int 0))) (.literal (.int 0))",
        "(.literal (.int 0))) (.literal (.int 1))",
    );
    let mutated = std::fs::read_to_string(&plans_path).unwrap();
    let shadow = format!("namespace Shadow\n{honest_block}\nend Shadow\n\nend AverCert.Plans");
    let planted = mutated.replacen("end AverCert.Plans", &shadow, 1);
    assert_ne!(mutated, planted, "shadow decoy not planted");
    std::fs::write(&plans_path, planted).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "shadow decoy must be DECLINED:\n{out}");
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
        "wrong reason (q):\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "shadow decoy credited (q):\n{out}"
    );
}

/// (r) Comment decoy: the active plan mutated plus a byte-honest copy inside a
/// block comment. Dead text.
#[test]
fn cert_tripwire_declines_block_comment_decoy() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-r") else {
        return;
    };

    // (r) Comment decoy: mutate the active `sumTo` plan and re-plant its
    //     byte-honest text inside a `/- … -/` block comment. Dead text; the
    //     plan `fnPlans` names is still the mutated one: DECLINED.
    let dir = temp_dir("neg-r");
    copy_dir(&out_dir, &dir);
    let plans_path = dir.join("cert").join("Plans.lean");
    let honest_plans = std::fs::read_to_string(&plans_path).unwrap();
    let (_, _, _, def) = plan_entry_fields(&honest_plans, "sumTo");
    let honest_block = plan_def_block(&honest_plans, &def);
    tamper_export_plan(
        &plans_path,
        "sumTo",
        "(.literal (.int 0))) (.literal (.int 0))",
        "(.literal (.int 0))) (.literal (.int 1))",
    );
    let mutated = std::fs::read_to_string(&plans_path).unwrap();
    let comment = format!("/- honest decoy:\n{honest_block}\n-/\n\nend AverCert.Plans");
    let planted = mutated.replacen("end AverCert.Plans", &comment, 1);
    assert_ne!(mutated, planted, "comment decoy not planted");
    std::fs::write(&plans_path, planted).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "comment decoy must be DECLINED:\n{out}");
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
        "wrong reason (r):\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "comment decoy credited (r):\n{out}"
    );
}

/// (s) Plan decouple: `sumTo`'s `fnPlans` entry points at a decoy plan
/// that still types at the export's signature but is not its code. There is
/// no bespoke simulation proof to swap: the wall lowers whatever plan the
/// entry names and pins that lowering to the export's code entry.
#[test]
fn cert_tripwire_declines_recursion_code_decouple() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-s") else {
        return;
    };

    let dir = temp_dir("certverify-neg-s-dir");
    copy_dir(&out_dir, &dir);
    let cert = dir.join("cert");
    let plans_path = cert.join("Plans.lean");
    let plans = std::fs::read_to_string(&plans_path).unwrap();
    let (_, _, _, def) = plan_entry_fields(&plans, "sumTo");
    let honest_entry = plan_entry(&plans, "sumTo");
    let decoy_entry = honest_entry.replacen(&format!(", {def}⟩"), ", fnDecoy⟩", 1);
    assert_ne!(honest_entry, decoy_entry, "fnPlans entry shape changed");
    let decoy = "/-- decoy: a constant, so an unbound simulation would be vacuous. -/\n\
                 def fnDecoy : FnPlan :=\n  \
                 { sig := ⟨[.int], .int⟩, nslots := 1, locals := [.int],\n    \
                 body := (.literal (.int 0)) }\n\ndef fnPlans";
    let tampered = plans
        .replacen("def fnPlans", decoy, 1)
        .replacen(&honest_entry, &decoy_entry, 1);
    std::fs::write(&plans_path, tampered).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &cert);
    assert!(!ok, "plan decouple must be DECLINED (s):\n{out}");
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "wrong reason (s):\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "plan decouple credited (s):\n{out}"
    );
}

/// (t) Self decouple: `sumTo`'s `fnPlans` entry declares a wrong function
/// index; the wall binds the entry to the export's byte-derived index.
#[test]
fn cert_tripwire_declines_recursion_self_decouple() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-t") else {
        return;
    };

    // (t) Self decouple: set `sumTo`'s declared function index to 999. The
    //     export binding compares the bound function's index with the entry's,
    //     and the plan's own self-call now targets an unplanned index, so this
    //     fails closed without any bespoke simulation-proof replacement.
    let dir = temp_dir("neg-t");
    copy_dir(&out_dir, &dir);
    let cert = dir.join("cert");
    let plans_path = cert.join("Plans.lean");
    let plans = std::fs::read_to_string(&plans_path).unwrap();
    let (_, func_idx, _, _) = plan_entry_fields(&plans, "sumTo");
    let honest_entry = plan_entry(&plans, "sumTo");
    let hostile_entry = honest_entry.replacen(&format!(", {func_idx}, "), ", 999, ", 1);
    assert_ne!(
        honest_entry, hostile_entry,
        "fnPlans entry index shape changed"
    );
    std::fs::write(
        &plans_path,
        plans.replacen(&honest_entry, &hostile_entry, 1),
    )
    .unwrap();
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

/// (u) Export-name relabel: an honest plan relabelled to a duplicate export
/// name in the plans, the Lean subject and the JSON.
#[test]
fn cert_tripwire_declines_export_name_relabel() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-u") else {
        return;
    };

    // (u) Export-name relabel: keep the byte-bound honest plan and index, but
    //     relabel the first entry (and the subject and the JSON) to a
    //     duplicate export name (`countDown`). The entry is bound to its code
    //     through its export NAME, so the relabelled entry binds the other
    //     function and its plan no longer lowers to that code entry; the
    //     whole-module export accounting also refuses the duplicate → DECLINED.
    let dir = temp_dir("neg-u");
    copy_dir(&out_dir, &dir);
    let plans = dir.join("cert").join("Plans.lean");
    replace_once(&plans, "⟨\"sumTo\", ", "⟨\"countDown\", ");
    let man = dir.join("cert").join("Manifest.lean");
    replace_once(
        &man,
        "exports := [\"sumTo\", \"countDown\"]",
        "exports := [\"countDown\", \"countDown\"]",
    );
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    for c in m["certified"].as_array_mut().unwrap() {
        if c["name"] == serde_json::json!("sumTo") {
            c["name"] = serde_json::json!("countDown");
        }
    }
    // The relabelled export's bridge goes with it: the bridge surface names
    // certified exports only.
    m["sourceBridges"]
        .as_array_mut()
        .unwrap()
        .retain(|b| b["export"] != serde_json::json!("sumTo"));
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &dir.join("cert"));
    assert!(!ok, "export-name relabel must be DECLINED (u):\n{out}");
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "wrong reason (u):\n{out}"
    );
    assert!(!out.contains("CERTIFIED"), "relabel credited (u):\n{out}");
}

/// (v) Plan decouple: `countDown`'s `fnPlans` entry points at a decoy plan
/// that still types at the export's signature but is not its code. There is
/// no bespoke simulation proof to swap: the wall lowers whatever plan the
/// entry names and pins that lowering to the export's code entry.
#[test]
fn cert_tripwire_declines_accumulator_code_decouple() {
    let Some(out_dir) = tripwire_baseline("certverify-neg-v") else {
        return;
    };

    let dir = temp_dir("certverify-neg-v-dir");
    copy_dir(&out_dir, &dir);
    let cert = dir.join("cert");
    let plans_path = cert.join("Plans.lean");
    let plans = std::fs::read_to_string(&plans_path).unwrap();
    let (_, _, _, def) = plan_entry_fields(&plans, "countDown");
    let honest_entry = plan_entry(&plans, "countDown");
    let decoy_entry = honest_entry.replacen(&format!(", {def}⟩"), ", fnDecoy⟩", 1);
    assert_ne!(honest_entry, decoy_entry, "fnPlans entry shape changed");
    let decoy = "/-- decoy: a constant, so an unbound simulation would be vacuous. -/\n\
                 def fnDecoy : FnPlan :=\n  \
                 { sig := ⟨[.int, .int], .int⟩, nslots := 2, locals := [.int],\n    \
                 body := (.literal (.int 0)) }\n\ndef fnPlans";
    let tampered = plans
        .replacen("def fnPlans", decoy, 1)
        .replacen(&honest_entry, &decoy_entry, 1);
    std::fs::write(&plans_path, tampered).unwrap();
    let (ok, out) = aver_check(&dir.join("certprobe2.wasm"), &cert);
    assert!(!ok, "plan decouple must be DECLINED (v):\n{out}");
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "wrong reason (v):\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "plan decouple credited (v):\n{out}"
    );
}

/// Apply `mutate` to a fresh copy of the package's artifact, re-bind the
/// package's hash pins to the mutated bytes (the attacker controls the whole
/// package, hash included), and assert the developer preflight declines it
/// inside Lean, never crediting an export.
fn assert_rebound_byte_tamper_declines(
    out_dir: &Path,
    wasm_name: &str,
    label: &str,
    mutate: &dyn Fn(&mut Vec<u8>),
) {
    let dir = temp_dir("cert-rebound-byte-tamper");
    copy_dir(out_dir, &dir);
    let w = dir.join(wasm_name);
    let mut bytes = std::fs::read(&w).unwrap();
    let honest = bytes.clone();
    mutate(&mut bytes);
    assert_ne!(
        bytes, honest,
        "{label}: the mutation must change the artifact"
    );
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .unwrap_or_else(|error| panic!("{label}: the mutant must stay valid wasm: {error}"));
    std::fs::write(&w, &bytes).unwrap();
    rebind_cert_wasm_hash(&dir, &bytes);
    let (ok, out) = aver_check(&w, &dir.join("cert"));
    assert!(
        !ok,
        "{label}: the rebound byte tamper must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "{label}: wrong decline reason:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "{label}: the rebound byte tamper credited an export:\n{out}"
    );
}

/// Offsets of every instruction start equal to `opcode` in the body of the
/// function at absolute index `func_idx`.
fn function_opcode_offsets(bytes: &[u8], func_idx: u32, opcode: u8) -> Vec<usize> {
    let mut imported = 0u32;
    let mut ordinal = 0u32;
    let mut hits = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload.expect("compiler-produced wasm must parse") {
            wasmparser::Payload::ImportSection(reader) => {
                for group in reader {
                    for import in group.expect("import group must parse") {
                        let (_, import) = import.expect("import must parse");
                        if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                            imported += 1;
                        }
                    }
                }
            }
            wasmparser::Payload::CodeSectionEntry(body) => {
                if imported + ordinal == func_idx {
                    let mut operators = body.get_operators_reader().unwrap();
                    while !operators.eof() {
                        let at = operators.original_position();
                        operators.read().expect("operator must parse");
                        if bytes[at] == opcode {
                            hits.push(at);
                        }
                    }
                }
                ordinal += 1;
            }
            _ => {}
        }
    }
    hits
}

/// The Euclidean division helper is pinned by its template: one byte of the
/// `__aint_divmod` body changed (`i64.div_s` -> `i64.div_u`, a valid module
/// with every export, index and type unchanged) declines the package even
/// though no plan of this fixture divides — a declared helper is pinned
/// whether or not a plan calls it.
#[test]
fn cert_tripwire_declines_flipped_divmod_helper_byte() {
    let Some(out_dir) = tripwire_baseline("certverify-divmod-template") else {
        return;
    };
    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(out_dir.join("cert/cert-manifest.json")).unwrap(),
    )
    .unwrap();
    let divmod_idx = manifest["hostRoleTable"]["divmod"]
        .as_u64()
        .expect("certprobe2 declares the divmod helper") as u32;
    let wasm = std::fs::read(out_dir.join("certprobe2.wasm")).unwrap();
    let hits = function_opcode_offsets(&wasm, divmod_idx, 0x7f);
    assert_eq!(hits.len(), 1, "the divmod template carries one i64.div_s");
    assert_rebound_byte_tamper_declines(&out_dir, "certprobe2.wasm", "divmod body", &|bytes| {
        bytes[hits[0]] = 0x80;
    });
}

/// Every declared string literal is pinned to the passive data segment it
/// names: one byte of the `!` segment `shout` appends changed to `?` (a valid
/// module, the plan and its lowering untouched) declines the package.
#[test]
fn cert_tripwire_declines_tampered_data_segment_byte() {
    if !tripwire_lake_available() {
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certverify-data-segment");
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
    assert!(compile.status.success(), "stringconcat compile failed");
    let plans = std::fs::read_to_string(out_dir.join("cert/Plans.lean")).unwrap();
    assert!(
        plans.contains("strSegs := [([33], 0)]"),
        "shout's literal must be declared at data segment 0:\n{plans}"
    );
    let wasm = std::fs::read(out_dir.join("stringconcat.wasm")).unwrap();
    let data_range = wasmparser::Parser::new(0)
        .parse_all(&wasm)
        .find_map(|payload| match payload.expect("stringconcat parses") {
            wasmparser::Payload::DataSection(reader) => Some(reader.range()),
            _ => None,
        })
        .expect("stringconcat has a data section");
    // A passive segment: `01 <len> <bytes>`; segment 0 holds the one byte `!`.
    let at = wasm[data_range.clone()]
        .windows(3)
        .position(|window| window == [0x01, 0x01, b'!'])
        .map(|offset| data_range.start + offset + 2)
        .expect("segment 0 is the passive one-byte `!`");
    assert_rebound_byte_tamper_declines(&out_dir, "stringconcat.wasm", "data segment", &|bytes| {
        bytes[at] = b'?';
    });
}

/// A constructor struct must stay FINAL: `ref.test` on a non-final struct
/// would also accept a subtype the program never built. Declaring `gauge`'s
/// nullary constructor struct `sub` instead of `sub final` (a valid module)
/// declines the package at the type-table pin.
#[test]
fn cert_tripwire_declines_non_final_constructor_struct() {
    if !tripwire_lake_available() {
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("certverify-ctor-finality");
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
    assert!(compile.status.success(), "signalgauge compile failed");
    let wasm = std::fs::read(out_dir.join("signalgauge.wasm")).unwrap();
    let type_range = wasmparser::Parser::new(0)
        .parse_all(&wasm)
        .find_map(|payload| match payload.expect("signalgauge parses") {
            wasmparser::Payload::TypeSection(reader) => Some(reader.range()),
            _ => None,
        })
        .expect("signalgauge has a type section");
    // `sub final (root 0) (struct)`: the nullary constructor.
    let header = [0x4f, 0x01, 0x00, 0x5f, 0x00];
    let hits: Vec<usize> = wasm[type_range.clone()]
        .windows(header.len())
        .enumerate()
        .filter_map(|(offset, window)| (window == header).then_some(type_range.start + offset))
        .collect();
    assert_eq!(
        hits.len(),
        1,
        "exactly one final empty constructor struct expected"
    );
    assert_rebound_byte_tamper_declines(&out_dir, "signalgauge.wasm", "non-final ctor", &|bytes| {
        bytes[hits[0]] = 0x50;
    });
}

/// The declared type table is confirmed against the module's type section:
/// a record's field types, a sum's constructor struct indices, the Int
/// carrier and the String array are all byte-pinned, so rewriting any of them
/// in `Plans.lean` declines the package.
#[test]
fn cert_verify_declines_tampered_type_table() {
    if !lean_required::lake_available() {
        eprintln!("skipping type-table tamper test: `lake` not available");
        return;
    }
    let (out_dir, _wasm, _cert) = compile_cert_goals("cert-type-table-tamper");
    let plans = std::fs::read_to_string(out_dir.join("cert/Plans.lean")).unwrap();
    let carrier_at = plans.find("carrier := some ").unwrap() + "carrier := some ".len();
    let carrier: u32 = plans[carrier_at..]
        .split(',')
        .next()
        .unwrap()
        .parse()
        .unwrap();
    let carrier_from = format!("carrier := some {carrier},");
    let carrier_to = format!("carrier := some {},", carrier + 1);
    assert_package_tampers_decline(
        &out_dir,
        "cert_goals.wasm",
        &[
            (
                "record field types swapped",
                "Plans.lean",
                "records := [⟨1, 20, [.string, .int]⟩]",
                "records := [⟨1, 20, [.int, .string]⟩]",
            ),
            (
                "constructor struct indices swapped",
                "Plans.lean",
                "⟨0, 0, [(1, [.int]), (2, [.int]), (3, [])]⟩",
                "⟨0, 0, [(2, [.int]), (1, [.int]), (3, [])]⟩",
            ),
            (
                "carrier index moved",
                "Plans.lean",
                carrier_from.as_str(),
                carrier_to.as_str(),
            ),
            (
                "string array index moved",
                "Plans.lean",
                "str := some 21,",
                "str := some 20,",
            ),
        ],
    );
}

/// A valid custom section pads a real module to the 130,460-byte scale where
/// the old monolithic `List Nat` pin exhausted default heartbeats. The big-Nat
/// pin must close, while a one-byte-flipped expected entry must fail `rfl`.
#[test]
fn big_nat_code_entry_pin_closes_at_130kb_and_flipped_byte_fails() {
    if !lean_required::lake_available() {
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

    let artifact_defs = aver::codegen::cert::wall::render_artifact_bytes(&padded)
        .replace("AverCert.ArtifactBytes", "LargeBytes");
    let positive = format!(
        "{artifact_defs}\n\
         theorem largePin : (AverCert.WasmSlice.funcBindingForExport LargeBytes.modBytes LargeBytes.modLen [97, 100, 100, 84, 119, 111]).map (·.codeEntry) = some {} := rfl\n\
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
         example : (AverCert.WasmSlice.funcBindingForExport LargeBytes.modBytes LargeBytes.modLen [97, 100, 100, 84, 119, 111]).map (·.codeEntry) = some {} := rfl\n",
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
}

#[test]
fn cert_verify_declines_tampered_array_new_data_operands() {
    if !lean_required::lake_available() {
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
    // The denominator counts every function in json.wasm that carries no
    // claim. It rose from 90 to 133 when the string-index pass started
    // synthesizing index workers for the parser's recursive `String.charAt`
    // walks. The Bytes API expansion then added five ordinary stdlib helpers
    // (`empty`, `len`, `concat`, `take`, `drop`), while lowering the JSON
    // parser's `String.firstCodePoint` calls added four `__code` workers. None
    // of those nine functions carries an artifact claim, so the denominator is
    // now 141: `combineSurrogates` moved from the denominator into the
    // numerator (13) when the record projection-compute face gained scalar
    // parameters and absorbed the retired straight-line integer face. The one
    // plan grammar certifies five more: 18 of 154. Since `--certify` compiles
    // the same module as a plain build, the String cursor, builder and
    // codepoint variants the plain build synthesizes are in it too, eight more
    // functions that carry no claim: 18 of 162.
    assert!(
        compile_report.contains("(18 certified, 144 source-level-only)"),
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
        report.contains("18 checked exports"),
        "json should certify the widened data-segment functions:\n{report}"
    );
    assert!(
        report.contains("law-claims: 11 of 11 credited"),
        "every json law-claim must pass its per-pin axiom audit:\n{report}"
    );

    let dir = temp_dir("cert-json-data-tamper");
    copy_dir(&out_dir, &dir);
    let w = dir.join("json.wasm");
    let mut bytes = std::fs::read(&w).unwrap();
    // Find empty array.new_data literals through the decoded instruction
    // stream, so adding unrelated heap types or data segments cannot make this
    // regression guard depend on their numeric indices. Changing the length
    // operand from 0 to 1 keeps the module parseable but violates the decoder's
    // fail-closed data-segment guard.
    let mut length_immediates = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
        if let wasmparser::Payload::CodeSectionEntry(body) =
            payload.expect("compiler-produced json wasm must parse")
        {
            let mut operators = body.get_operators_reader().unwrap();
            let mut previous_i32_consts = [None, None];
            while !operators.eof() {
                let offset = operators.original_position();
                match operators.read().expect("json operator must parse") {
                    wasmparser::Operator::I32Const { value } => {
                        previous_i32_consts[0] = previous_i32_consts[1];
                        previous_i32_consts[1] = Some((value, offset));
                    }
                    wasmparser::Operator::ArrayNewData { .. } => {
                        if matches!(previous_i32_consts[0], Some((0, _)))
                            && matches!(previous_i32_consts[1], Some((0, _)))
                        {
                            length_immediates.push(previous_i32_consts[1].unwrap().1 + 1);
                        }
                        previous_i32_consts = [None, None];
                    }
                    _ => previous_i32_consts = [None, None],
                }
            }
        }
    }
    assert!(
        !length_immediates.is_empty(),
        "expected empty array.new_data literal in json wasm"
    );
    for offset in length_immediates {
        assert_eq!(bytes[offset], 0, "expected one-byte i32.const 0 immediate");
        bytes[offset] = 1;
    }
    std::fs::write(&w, &bytes).unwrap();

    let old_hash = {
        let mf = dir.join("cert").join("cert-manifest.json");
        let m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["wasm_sha256"].as_str().unwrap().to_string()
    };
    let new_hash = aver::codegen::cert::sha256_hex(&bytes);
    let path = dir.join("cert").join("Manifest.lean");
    let src = std::fs::read_to_string(&path).unwrap();
    assert!(
        src.contains(&old_hash),
        "Manifest.lean should pin the old hash"
    );
    std::fs::write(&path, src.replace(&old_hash, &new_hash)).unwrap();
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    m["wasm_sha256"] = serde_json::Value::String(new_hash);
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

    let (ok, out) = aver_check(&w, &dir.join("cert"));
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
fn plans_authority_baseline(prefix: &str) -> Option<ScratchDir> {
    if !lean_required::lake_available() {
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
/// DATA: no checker-generated artifact-byte modules, no fragment sidecars, and
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
        !cert.join("ArtifactComponentBytes.lean").exists(),
        "ArtifactComponentBytes.lean is checker-generated, not package DATA"
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
}

/// A plan's declared locals are part of its code entry: `addTwo`'s plan
/// declaring no locals is DECLINED even with honest bytes and an honest body,
/// because the lowering's locals vector is no longer the code entry's.
#[test]
fn cert_plans_authority_declines_zero_locals_expr_fragment_code() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-zero-locals") else {
        return;
    };

    let dir = temp_dir("cert-expr-zero-locals");
    copy_dir(&out_dir, &dir);
    tamper_export_plan(
        &dir.join("cert/Plans.lean"),
        "addTwo",
        "locals := [.int]",
        "locals := []",
    );
    let (ok, report) = aver_check(&dir.join("cert_goals.wasm"), &dir.join("cert"));
    assert!(!ok, "zero-locals plan must be DECLINED:\n{report}");
    // Pin WHY it declined: on its own lane a bare `!ok` would also be
    // satisfied by a fixture that stopped building for an unrelated reason.
    assert!(
        report.contains("did not build") && !report.contains("CERTIFIED"),
        "zero-locals tamper must fail the plan acceptance:\n{report}"
    );
}

/// The obligations are derived from the plans, never producer data: emptying
/// the manifest's obligation list and re-proving the weakened `Final.cert`
/// over it must not buy acceptance.
#[test]
fn cert_plans_authority_declines_claim_without_manifest_obligation() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-claim-without-obligation")
    else {
        return;
    };

    let dir = temp_dir("cert-expr-claim-without-obligation");
    copy_dir(&out_dir, &dir);
    let wasm = dir.join("cert_goals.wasm");
    let cert = dir.join("cert");
    replace_once(
        &cert.join("Manifest.lean"),
        "obligations := AverCert.AcceptedArtifact.obligationsOf subject Plans.types Plans.fnPlans",
        "obligations := []",
    );
    std::fs::write(
        cert.join("Final.lean"),
        concat!(
            "import Artifact\n",
            "import AcceptanceSoundness\n\n",
            "open AverCert AverCert.Schema\n\n",
            "theorem AverCert.Final.cert : AverCert.Schema.Holds manifest := by\n",
            "  refine ⟨rfl, rfl, rfl, ?_⟩\n",
            "  intro o ho\n",
            "  simp [manifest] at ho\n",
            "\n",
            "#print axioms AverCert.Final.cert\n",
        ),
    )
    .unwrap();
    let (ok, out) = aver_check(&wasm, &cert);
    assert!(
        !ok,
        "a manifest without the derived obligations must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "wrong reason for missing manifest obligations:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "a manifest without the derived obligations credited:\n{out}"
    );
}

/// An obligation that is no longer structurally the one the wall derives —
/// same export, host table wrapped so it differs — must be DECLINED.
#[test]
fn cert_plans_authority_declines_artifact_claim_obligation_tamper() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-artifact-obligation-tamper")
    else {
        return;
    };

    let dir = temp_dir("cert-expr-artifact-obligation-tamper");
    copy_dir(&out_dir, &dir);
    let wasm = dir.join("cert_goals.wasm");
    let cert = dir.join("cert");
    replace_once(
        &cert.join("Manifest.lean"),
        "obligations := AverCert.AcceptedArtifact.obligationsOf subject Plans.types Plans.fnPlans",
        "obligations := (AverCert.AcceptedArtifact.obligationsOf subject Plans.types Plans.fnPlans).map \
         (fun o => { o with host := fun h f => if f = o.self + 999999 then none else o.host h f })",
    );
    let (ok, out) = aver_check(&wasm, &cert);
    assert!(
        !ok,
        "an obligation not structurally the derived one must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "wrong reason for the obligation tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "the obligation tamper credited:\n{out}"
    );
}

/// A package that proves one of its acceptance facts from a carried `axiom`
/// must be DECLINED by the axiom whitelist, naming the offending axiom.
#[test]
fn cert_plans_authority_declines_artifact_carried_axiom_bridge() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-artifact-axiom-tamper")
    else {
        return;
    };

    let dir = temp_dir("cert-expr-artifact-axiom-tamper");
    copy_dir(&out_dir, &dir);
    let wasm = dir.join("cert_goals.wasm");
    let cert = dir.join("cert");
    replace_once(
        &cert.join("Artifact.lean"),
        "theorem axes_ok : AverCert.ClaimAxes.checked data = true := by decide +kernel",
        "axiom artifactEvil : AverCert.ClaimAxes.checked data = true\n\n\
         theorem axes_ok : AverCert.ClaimAxes.checked data = true := artifactEvil",
    );
    let (ok, out) = aver_check(&wasm, &cert);
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
}

/// `Plans.lean` is the authoritative plan DATA, so swapping a plan's operands
/// there must be DECLINED against the module bytes.
#[test]
fn cert_plans_authority_declines_tampered_lean_raw_plan() {
    let Some(out_dir) = plans_authority_baseline("cert-plans-authority-lean-plan-tamper") else {
        return;
    };

    let dir = temp_dir("cert-expr-lean-plan-tamper");
    copy_dir(&out_dir, &dir);
    tamper_export_plan(
        &dir.join("cert/Plans.lean"),
        "floatLeGoal",
        "(.binOp .lte (.local 0) (.local 1))",
        "(.binOp .lte (.local 1) (.local 0))",
    );
    let (ok, out) = aver_check(&dir.join("cert_goals.wasm"), &dir.join("cert"));
    assert!(!ok, "tampered Lean plan data must be DECLINED:\n{out}");
    assert!(
        out.contains("did not build"),
        "wrong reason for the Lean plan tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered Lean plan data credited:\n{out}"
    );
}

/// The helper indices live in the subject's host-role table, not in the plans:
/// swapping the declared `add` and `sub` indices (in the Lean subject and the
/// JSON alike) must be DECLINED — each role is pinned to its helper template.
#[test]
fn cert_verify_declines_host_role_relabel_in_plans_lean() {
    if !lean_required::lake_available() {
        eprintln!("skipping host-role relabel test: `lake` not available");
        return;
    }

    let (_out_dir, wasm, cert) = compile_cert_goals("cert-expr-host-role-swap");
    let mf = cert.join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    let add_idx = m["hostRoleTable"]["add"].as_u64().expect("goals add role");
    let sub_idx = m["hostRoleTable"]["sub"].as_u64().expect("goals sub role");
    assert_ne!(
        add_idx, sub_idx,
        "goals module should have distinct host roles"
    );
    let man = cert.join("Manifest.lean");
    let honest = format!("add := some {add_idx}, mul := ");
    let text = std::fs::read_to_string(&man).unwrap();
    assert!(
        text.contains(&honest),
        "Manifest.lean role table shape changed"
    );
    let swapped = text
        .replacen(&honest, &format!("add := some {sub_idx}, mul := "), 1)
        .replacen(
            &format!("sub := some {sub_idx},"),
            &format!("sub := some {add_idx},"),
            1,
        );
    std::fs::write(&man, swapped).unwrap();
    m["hostRoleTable"]["add"] = serde_json::json!(sub_idx);
    m["hostRoleTable"]["sub"] = serde_json::json!(add_idx);
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

    let (ok, out) = aver_check(&wasm, &cert);
    assert!(!ok, "a swapped host-role table must be DECLINED:\n{out}");
    assert!(
        out.contains("did not build") || out.contains("does not bind"),
        "wrong reason for the add/sub role swap:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "a swapped host-role table credited:\n{out}"
    );
}

/// An ill-typed plan node is refused by the wall's plan typing: an Int
/// literal where `intLessZero`'s `Bool` result stands must be DECLINED.
#[test]
fn cert_verify_declines_expr_fragment_bad_bool01_raw_plan() {
    if !lean_required::lake_available() {
        eprintln!("skipping ill-typed plan test: `lake` not available");
        return;
    }

    let (_out_dir, wasm, cert) = compile_cert_goals("cert-expr-bad-bool01");
    tamper_export_plan(
        &cert.join("Plans.lean"),
        "intLessZero",
        "(.literal (.bool true))",
        "(.literal (.int 2))",
    );
    let (ok, out) = aver_check(&wasm, &cert);
    assert!(!ok, "an ill-typed plan must be DECLINED:\n{out}");
    assert!(
        out.contains("did not build"),
        "an ill-typed plan should fail the plan-data checks:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "an ill-typed plan credited:\n{out}"
    );
}

#[test]
fn cert_verify_declines_tampered_string_eq_helper_shape() {
    if !lean_required::lake_available() {
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
        report.contains("quoteOrSelf  policy: simulatesModel  class: source-plan-v1 (strings)"),
        "quoteOrSelf should report the one plan class with its strings facet:\n{report}"
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
        quote_class, "source-plan-v1",
        "quoteOrSelf should render the one plan class, got {quote_class}"
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
    let path = dir.join("cert").join("Manifest.lean");
    let src = std::fs::read_to_string(&path).unwrap();
    assert!(
        src.contains(&old_hash),
        "Manifest.lean should pin the old hash"
    );
    std::fs::write(&path, src.replace(&old_hash, &new_hash)).unwrap();
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    m["wasm_sha256"] = serde_json::Value::String(new_hash);
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

    let (ok, out) = aver_check(&w, &dir.join("cert"));
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
    if !lean_required::lake_available() {
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

    // Honest bytes and body, zero locals in the plan only. This fixture
    // touches `Int`, so its module carries the Int carrier struct and the
    // emitted concatenation reserves one carrier scratch local; a plan
    // declaring none lowers to a different locals vector than the code entry.
    {
        let dir = temp_dir("cert-stringconcat-zero-locals");
        copy_dir(&out_dir, &dir);
        tamper_export_plan(
            &dir.join("cert/Plans.lean"),
            "shout",
            "locals := [.int]",
            "locals := []",
        );
        let (ok, report) = aver_check(&dir.join("stringconcat.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "String.concat zero-locals code must be DECLINED:\n{report}"
        );
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
        shout_class, "source-plan-v1",
        "shout should render the one plan class, got {shout_class}"
    );
    assert!(shout_entry.get("fragment").is_none());
    assert!(shout_entry.get("source_fragment").is_none());

    let wasm_bytes = std::fs::read(&wasm).unwrap();
    let type_section_range = wasmparser::Parser::new(0)
        .parse_all(&wasm_bytes)
        .find_map(
            |payload| match payload.expect("stringconcat wasm must parse") {
                wasmparser::Payload::TypeSection(reader) => Some(reader.range()),
                _ => None,
            },
        )
        .expect("stringconcat wasm must carry a type section");
    // The `$string` and `Vector<String>` array types, read from the type
    // table the package declares (and the wall confirms against the bytes).
    let plans_text = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    let declared_index = |field: &str| -> usize {
        let head = format!("{field} := some ");
        let at = plans_text
            .find(&head)
            .unwrap_or_else(|| panic!("Plans.lean declares no `{field}`"))
            + head.len();
        plans_text[at..]
            .split(|c: char| !c.is_ascii_digit())
            .next()
            .unwrap()
            .parse()
            .unwrap()
    };
    let result_ty = declared_index("str");
    let container_ty = declared_index("strVec");
    assert!(
        result_ty < 128 && container_ty < 128,
        "fixture type indices should have one-byte LEB encodings"
    );

    {
        let dir = temp_dir("cert-stringconcat-export-type-tamper");
        copy_dir(&out_dir, &dir);
        let w = dir.join("stringconcat.wasm");
        let mut bytes = wasm_bytes.clone();
        let signature = [
            0x60,
            0x01,
            0x63,
            result_ty as u8,
            0x01,
            0x63,
            result_ty as u8,
        ];
        let matches: Vec<usize> = bytes[type_section_range.clone()]
            .windows(signature.len())
            .enumerate()
            .filter_map(|(offset, win)| {
                (win == signature).then_some(type_section_range.start + offset)
            })
            .collect();
        assert_eq!(
            matches.len(),
            1,
            "String.concat export result-ref signature must be unique"
        );
        bytes[matches[0] + 2] = 0x64;
        std::fs::write(&w, &bytes).unwrap();
        rebind_cert_wasm_hash(&dir, &bytes);

        let (ok, out) = aver_check(&w, &dir.join("cert"));
        assert!(
            !ok,
            "tampered String.concat export declared function type must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("does not bind") || out.contains("did not build"),
            "wrong reason for String.concat export type tamper:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "tampered String.concat export type credited:\n{out}"
        );
    }

    {
        let dir = temp_dir("cert-stringconcat-helper-type-tamper");
        copy_dir(&out_dir, &dir);
        let w = dir.join("stringconcat.wasm");
        let mut bytes = wasm_bytes.clone();
        let signature = [
            0x60,
            0x01,
            0x63,
            container_ty as u8,
            0x01,
            0x63,
            result_ty as u8,
        ];
        let matches: Vec<usize> = bytes[type_section_range.clone()]
            .windows(signature.len())
            .enumerate()
            .filter_map(|(offset, win)| {
                (win == signature).then_some(type_section_range.start + offset)
            })
            .collect();
        assert_eq!(
            matches.len(),
            1,
            "String.concat helper container-ref signature must be unique"
        );
        bytes[matches[0] + 2] = 0x64;
        std::fs::write(&w, &bytes).unwrap();
        rebind_cert_wasm_hash(&dir, &bytes);

        let (ok, out) = aver_check(&w, &dir.join("cert"));
        assert!(
            !ok,
            "tampered String.concat helper declared function type must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("does not bind") || out.contains("did not build"),
            "wrong reason for String.concat helper type tamper:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "tampered String.concat helper type credited:\n{out}"
        );
    }

    {
        let dir = temp_dir("cert-stringconcat-source-plan-tamper");
        copy_dir(&out_dir, &dir);
        let plans = dir.join("cert/Plans.lean");
        let plan_text = std::fs::read_to_string(&plans).unwrap();
        let tampered_plan =
            plan_text.replacen("(.literal (.str [33]))", "(.literal (.str [63]))", 1);
        assert_ne!(
            plan_text, tampered_plan,
            "String.concat plan literal shape changed"
        );
        std::fs::write(&plans, &tampered_plan).unwrap();

        let (ok, out) = aver_check(&dir.join("stringconcat.wasm"), &dir.join("cert"));
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
        // The declared literal-to-segment table: the wall confirms each
        // entry against the passive data segment it names.
        let tampered_plan =
            plan_text.replacen("strSegs := [([33], 0)]", "strSegs := [([63], 0)]", 1);
        assert_ne!(
            plan_text, tampered_plan,
            "String.concat data-segment table shape changed"
        );
        std::fs::write(&plans, &tampered_plan).unwrap();

        let (ok, out) = aver_check(&dir.join("stringconcat.wasm"), &dir.join("cert"));
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
    let path = dir.join("cert").join("Manifest.lean");
    let src = std::fs::read_to_string(&path).unwrap();
    assert!(
        src.contains(&old_hash),
        "Manifest.lean should pin the old hash"
    );
    std::fs::write(&path, src.replace(&old_hash, &new_hash)).unwrap();
    let mf = dir.join("cert").join("cert-manifest.json");
    let mut m: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
    m["wasm_sha256"] = serde_json::Value::String(new_hash);
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();

    let (ok, out) = aver_check(&w, &dir.join("cert"));
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

/// End to end on the flagship first example. `examples/core/hello.av` mentions
/// no `Int` anywhere, so the compiler emits no Int carrier struct and no box
/// helper, and its concatenations lower to a code entry with an EMPTY locals
/// vector. Both exports must certify in that state, and the claim's declared
/// carrier state must be pinned to the module's own type section rather than
/// carried as a free field.
#[test]
fn cert_verify_certifies_string_concat_in_a_carrierless_module() {
    if !lean_required::lake_available() {
        eprintln!("skipping carrierless String.concat test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-hello-carrierless");

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
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "hello compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("hello.wasm");
    let cert = out_dir.join("cert");

    // The module really is in the carrierless state: no Int carrier helper is
    // exported, so no claim in it may cite a box/add/mul/sub role either.
    let bytes = std::fs::read(&wasm).unwrap();
    assert!(
        !bytes
            .windows(b"__rt_aint_from_i64".len())
            .any(|window| window == b"__rt_aint_from_i64"),
        "hello.av must stay carrierless for this test to mean anything"
    );

    // The type table declares that state as `none`.
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    assert!(
        plans.contains("carrier := none, mag := none"),
        "a carrierless type table should declare `carrier := none`:\n{plans}"
    );

    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(
        ok,
        "the carrierless hello certificate must verify:\n{report}"
    );
    assert!(
        report.contains("CERTIFIED"),
        "carrierless hello should be CERTIFIED:\n{report}"
    );
    for export in ["greet", "shout"] {
        assert!(
            report.contains(export),
            "carrierless hello should certify `{export}`:\n{report}"
        );
    }

    let manifest: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(cert.join("cert-manifest.json"))
            .expect("cert-manifest.json exists"),
    )
    .expect("manifest is valid JSON");
    assert!(
        manifest.get("hostRoleTable").is_some() && manifest["hostRoleTable"].is_null(),
        "a carrierless module must declare the null host-role table, got {:?}",
        manifest.get("hostRoleTable")
    );
    assert!(
        manifest.get("carrier_type_index").is_some() && manifest["carrier_type_index"].is_null(),
        "a carrierless module must report no carrier type index, got {:?}",
        manifest.get("carrier_type_index")
    );
    for export in ["greet", "shout"] {
        let entry = manifest["certified"]
            .as_array()
            .unwrap()
            .iter()
            .find(|c| c["name"].as_str() == Some(export))
            .unwrap_or_else(|| panic!("{export} manifest entry"));
        assert_eq!(
            entry["class"].as_str(),
            Some("source-plan-v1"),
            "{export} should render the one plan class"
        );
    }

    // The type table cannot buy itself a carrier: declaring a carrier index in
    // a module whose type section holds no carrier struct contradicts
    // `CertDecode.carrierState`, which `TypeTable.carrierConfirmed` pins.
    {
        let dir = temp_dir("cert-hello-carrier-claim");
        copy_dir(&out_dir, &dir);
        let path = dir.join("cert/Plans.lean");
        let src = std::fs::read_to_string(&path).unwrap();
        let tampered = src.replacen(
            "carrier := none, mag := none",
            "carrier := some 2, mag := none",
            1,
        );
        assert_ne!(src, tampered, "carrierless claim shape changed");
        std::fs::write(&path, &tampered).unwrap();
        let (ok, out) = aver_check(&dir.join("hello.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "a carrierless module claiming a carrier index must be DECLINED:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "carrier-index claim credited in a carrierless module:\n{out}"
        );
    }

    // Nor can the subject declare a helper-role table: a carrierless module
    // (no `__rt_aint_from_i64` export) admits only the absent table, which
    // `arithTableCheck` proves from the export section.
    {
        let dir = temp_dir("cert-hello-role-table-claim");
        copy_dir(&out_dir, &dir);
        replace_once(
            &dir.join("cert/Manifest.lean"),
            "hostRoleTable := (none : Option CertDecode.AddSub.Roles)",
            "hostRoleTable := some ({ box := none, add := none, mul := none, sub := none, \
             toIndex := none, cmp := none, eq := none } : CertDecode.AddSub.Roles)",
        );
        let mf = dir.join("cert/cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["hostRoleTable"] = serde_json::json!({
            "box": null, "add": null, "mul": null, "sub": null,
            "toIndex": null, "cmp": null, "eq": null, "divmod": null
        });
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_check(&dir.join("hello.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "a carrierless module declaring a role table must be DECLINED:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "a carrierless role-table declaration credited:\n{out}"
        );
    }
}

/// A cert with zero certified exports is an admission, not a certification: it
/// must NOT print the green CERTIFIED path and must exit nonzero (fail-closed).
#[test]
fn empty_cert_is_admission_only_and_exits_nonzero() {
    if !lean_required::lake_available() {
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
            "class": "source-plan-v1",
            "facets": [],
            "policy": "simulatesModel",
            "level": "L1",
            "theorem": "AcceptanceSoundness.fn_claim_discharges"
        }));
    std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
    let (ok, out) = aver_check(&out_dir.join("certempty.wasm"), &out_dir.join("cert"));
    assert!(!ok, "padded empty cert must still exit nonzero:\n{out}");
    assert!(
        out.contains("does not bind") && !out.contains("CERTIFIED"),
        "padded JSON must be DECLINED, not credited:\n{out}"
    );
}

/// The manifest `dom`/`cod` strings are declared display metadata: no
/// checker-witness line pins them, so editing them cannot fail verification.
/// Precisely because they are unpinned, the trusted CHECKED/CERTIFIED report
/// must never echo them. Sentinels planted in `cert-manifest.json` must leave
/// the trusted check green and stay out of the complete trusted report output
/// and out of `explain`.
#[test]
fn unpinned_manifest_dom_cod_never_reach_the_trusted_report() {
    if !lean_required::lake_available() {
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

    // Schema 9 reports no declared manifest face at all (the certified entry
    // has no `dom`/`cod`), so `explain` must not echo the sentinels either.
    let (ok, explain) = aver_cert(&["explain"], &wasm, &cert);
    assert!(
        ok,
        "explain must accept the certificate with extra dom/cod strings:\n{explain}"
    );
    assert!(
        !explain.contains(DOM_SENTINEL) && !explain.contains(COD_SENTINEL),
        "explain echoed an unpinned manifest dom/cod string:\n{explain}"
    );
}

/// A record projection's plan is bound to the bytes like every other plan:
/// projecting the OTHER field of `User` in `greet`'s plan still types (both
/// fields are read somewhere), but it is not the emitted code → DECLINED.
#[test]
fn adt_witness_body_mutation_is_declined() {
    if !lean_required::lake_available() {
        eprintln!("skipping ADT plan-mutation test: `lake` not available");
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

    // `greet` projects field 0 (the name); field 1 is the Int age, so the
    // mutated plan no longer even has the export's String result type.
    // Mutating the LOCALS instead keeps the typing and changes only the code
    // entry's locals vector; both must be declined, so run both.
    for (label, from, to) in [
        (
            "other field",
            "(.project 0 0 (.local 0))",
            "(.project 0 1 (.local 0))",
        ),
        ("extra local", "locals := [.int]", "locals := [.int, .int]"),
    ] {
        let dir = temp_dir("cert-adt-mut-case");
        copy_dir(&out_dir, &dir);
        tamper_export_plan(&dir.join("cert").join("Plans.lean"), "greet", from, to);
        let (ok, out) = aver_check(&dir.join("user_record.wasm"), &dir.join("cert"));
        assert!(!ok, "{label}: mutated record plan must be DECLINED:\n{out}");
        assert!(
            out.contains("does not bind") || out.contains("did not build"),
            "{label}: wrong reason:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "{label}: mutated record plan credited:\n{out}"
        );
    }
}

/// A `match` over a sum is bound arm by arm: swapping two constructor arms'
/// bodies in `gauge`'s plan still types, but its lowering tests the tags in a
/// different order than the emitted code → DECLINED.
#[test]
fn variant_dispatch_body_mutation_is_declined() {
    if !lean_required::lake_available() {
        eprintln!("skipping variant-dispatch plan-mutation test: `lake` not available");
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

    // (a) the constant of the nullary arm (7 -> 8); (b) the first two
    // constructor patterns' tags exchanged (`user 0 0` <-> `user 0 1`).
    for (label, from, to) in [
        (
            "nullary arm constant",
            "(.literal (.int 7))",
            "(.literal (.int 8))",
        ),
        (
            "constructor tags exchanged",
            "(.cons (.ctor (.user 0 0) [1]) (.binOp .sub (.literal (.int 0)) (.local 1)) (.cons (.ctor (.user 0 1) [2])",
            "(.cons (.ctor (.user 0 1) [1]) (.binOp .sub (.literal (.int 0)) (.local 1)) (.cons (.ctor (.user 0 0) [2])",
        ),
    ] {
        let dir = temp_dir("cert-vd-mut-case");
        copy_dir(&out_dir, &dir);
        tamper_export_plan(&dir.join("cert").join("Plans.lean"), "gauge", from, to);
        let (ok, out) = aver_check(&dir.join("signalgauge.wasm"), &dir.join("cert"));
        assert!(
            !ok,
            "{label}: mutated dispatch plan must be DECLINED:\n{out}"
        );
        assert!(
            out.contains("does not bind") || out.contains("did not build"),
            "{label}: wrong reason:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "{label}: mutated dispatch plan credited:\n{out}"
        );
    }
}

/// Cross-function composition: the CALLEE `double`'s plan is bound to its own
/// code entry, so mutating it declines the package even though the callers'
/// plans are untouched. This is the load-bearing tripwire for composition:
/// a caller's model runs the callee's plan, so an unbound callee would make
/// every caller's claim unfounded.
#[test]
fn composition_callee_mutation_is_declined() {
    if !lean_required::lake_available() {
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

    tamper_export_plan(
        &out_dir.join("cert").join("Plans.lean"),
        "double",
        "(.binOp .add (.local 0) (.local 0))",
        "(.binOp .mul (.local 0) (.literal (.int 2)))",
    );

    let (ok, out) = aver_check(&out_dir.join("compose.wasm"), &out_dir.join("cert"));
    assert!(
        !ok,
        "mutated composition callee plan must be DECLINED:\n{out}"
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

/// Every `fnPlans` entry is byte-checked, including an unexported one no
/// certified export reaches: an extra internal entry whose plan is not its
/// function's code declines the package (there is no "unchecked member" of a
/// composition any more), and so does a caller retargeted at it, since a call
/// must reach a planned function of the same or an earlier group.
#[test]
fn composition_orphan_member_is_declined() {
    if !lean_required::lake_available() {
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

    // An internal entry with `double`'s plan at the index of `_start` — a
    // function that is neither planned nor a helper role. The entry is not
    // exported, so it names no obligation, but it is checked all the same:
    // its plan must be the code entry of the function at its index.
    let wasm = std::fs::read(out_dir.join("compose.wasm")).unwrap();
    let start_idx = wasmparser::Parser::new(0)
        .parse_all(&wasm)
        .find_map(|payload| match payload.expect("compose wasm parses") {
            wasmparser::Payload::ExportSection(reader) => reader
                .into_iter()
                .map(|export| export.expect("export parses"))
                .find(|export| export.name == "_start")
                .map(|export| export.index),
            _ => None,
        })
        .expect("compose exports `_start`");
    let plans_path = out_dir.join("cert").join("Plans.lean");
    let plans = std::fs::read_to_string(&plans_path).unwrap();
    let (_, _, hex16_group, _) = plan_entry_fields(&plans, "hex16");
    let (_, _, _, double_def) = plan_entry_fields(&plans, "double");
    let last_entry = plan_entry(&plans, "hex16");
    let orphan = format!(
        "{last_entry},\n   ⟨\"#{start_idx}\", false, {start_idx}, {}, {double_def}⟩",
        hex16_group + 1
    );
    std::fs::write(&plans_path, plans.replacen(&last_entry, &orphan, 1)).unwrap();

    let (ok, out) = aver_check(&out_dir.join("compose.wasm"), &out_dir.join("cert"));
    assert!(
        !ok,
        "an unchecked extra plan entry must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
        "wrong reason:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "an extra plan entry credited:\n{out}"
    );
}

/// A record projection's field index is plan data bound to the bytes:
/// projecting the other field of `User` in `userName` must be DECLINED.
#[test]
fn cert_verify_declines_flipped_field_projection_plan() {
    if !lean_required::lake_available() {
        eprintln!("skipping field-projection plan tamper test: `lake` not available");
        return;
    }

    let (out_dir, _wasm, _cert) = compile_cert_goals("cert-proj-plan-flip");
    assert_package_tampers_decline(
        &out_dir,
        "cert_goals.wasm",
        &[(
            "projected field",
            "plan:userName",
            "(.project 1 0 (.local 0))",
            "(.project 1 1 (.local 0))",
        )],
    );
}

/// A plan's source types decide its representation: relabelling
/// `userName`'s record parameter as a sum type in its signature must not be
/// able to explain the same bytes, so it is DECLINED.
#[test]
fn cert_verify_declines_relabeled_projection_source_types() {
    if !lean_required::lake_available() {
        eprintln!("skipping projection relabel tamper test: `lake` not available");
        return;
    }

    let (out_dir, _wasm, _cert) = compile_cert_goals("cert-proj-relabel");
    assert_package_tampers_decline(
        &out_dir,
        "cert_goals.wasm",
        &[(
            "parameter type relabel",
            "plan:userName",
            "sig := ⟨[(.record 1)], .string⟩",
            "sig := ⟨[(.sum 0)], .string⟩",
        )],
    );
}

/// A tampered self-recursive plan is declined. The vectors exercise additive,
/// multiplicative and accumulator plans in the shipped `Plans.lean` while
/// leaving the wasm untouched: each one changes the lowering, so the plan is
/// no longer its function's code entry.
#[test]
fn cert_verify_declines_tampered_recursion_plan() {
    if !lean_required::lake_available() {
        eprintln!("skipping recursion-plan tamper test: `lake` not available");
        return;
    }

    let out_dir =
        compile_checked_fixture("tools/certkit/fixtures/recgen.av", "cert-recursion-plan");
    assert_package_tampers_decline(
        &out_dir,
        "recgen.wasm",
        &[
            // The declared locals are part of the code entry.
            (
                "zero locals",
                "plan:sumFrom",
                "locals := [.int]",
                "locals := []",
            ),
            // `sumFrom`'s descent `n - 1` becomes `n + 1`.
            (
                "descent operator",
                "plan:sumFrom",
                "(.call (.fn 1) [(.binOp .sub (.local 0)",
                "(.call (.fn 1) [(.binOp .add (.local 0)",
            ),
            // The self-call retargeted at another user function (`backward`).
            (
                "self-call retarget",
                "plan:sumFrom",
                "(.call (.fn 1)",
                "(.call (.fn 3)",
            ),
            // The base literal `7` becomes `5`.
            (
                "base literal",
                "plan:sumFrom",
                "(.literal (.int 7))",
                "(.literal (.int 5))",
            ),
            // `factorial`'s multiplication relabelled as an addition.
            (
                "multiply relabel",
                "plan:factorial",
                "(.binOp .mul (.local 0)",
                "(.binOp .add (.local 0)",
            ),
            // `countDown`'s accumulator threading swapped.
            (
                "accumulator threading",
                "plan:countDown",
                "(.binOp .add (.local 1) (.local 0))",
                "(.local 1)",
            ),
        ],
    );
}

/// The JSON policy/witness is transport data. Unsupported shapes fail strict
/// decoding; supported-but-wrong values fail the checker witness, which pins
/// every report entry to the policy axes the wall derives from the plans
/// (`AcceptedArtifact.axesOf`) — so no manifest edit can move a policy.
#[test]
fn cert_verify_declines_tampered_termination_manifest() {
    if !lean_required::lake_available() {
        eprintln!("skipping termination manifest round-trip test: `lake` not available");
        return;
    }
    let out_dir = compile_checked_fixture(
        "tools/certkit/fixtures/recgen.av",
        "cert-termination-manifest-roundtrip",
    );

    for (label, mutate, expected) in [
        ("wrong descent", 0_u8, "does not bind"),
        ("unknown measure", 1_u8, "unsupported termination measure"),
        ("missing witness", 2_u8, "is missing `termination_witness`"),
        ("partial policy", 3_u8, "does not bind"),
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
            3 => {
                // A coordinated downgrade to the partial policy, with the
                // witness dropped the way a partial entry carries none.
                entry["policy"] = serde_json::json!("simulatesModel");
                entry["level"] = serde_json::json!("L1");
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
        if expected == "does not bind" {
            assert!(
                report.contains("checker-owned Lean witness"),
                "witness decline must identify the Lean binding:\n{report}"
            );
        }
    }
}

/// A tampered mutually recursive plan is declined. Each vector mutates the
/// plan of `isEven` (whose step tail-calls its SIBLING `isOdd`) in the shipped
/// `Plans.lean` while leaving the wasm untouched, so the plan no longer lowers
/// to `isEven`'s real code entry, or calls outside the planned functions.
#[test]
fn cert_verify_declines_tampered_mutual_plan() {
    if !lean_required::lake_available() {
        eprintln!("skipping mutual-plan tamper test: `lake` not available");
        return;
    }

    let out_dir = compile_checked_fixture("tools/certkit/fixtures/mutual.av", "cert-mutual-plan");
    assert_package_tampers_decline(
        &out_dir,
        "mutual.wasm",
        &[
            (
                "zero locals",
                "plan:isEven",
                "locals := [.int]",
                "locals := []",
            ),
            // (a) the sibling call retargeted OUTSIDE the planned functions.
            (
                "call outside the plans",
                "plan:isEven",
                "(.tailCall 2 [",
                "(.tailCall 5 [",
            ),
            // (b) the tail call made a plain call.
            (
                "tail flag flip",
                "plan:isEven",
                "(.tailCall 2 [(.binOp .sub (.local 0) (.literal (.int 1)))])",
                "(.call (.fn 2) [(.binOp .sub (.local 0) (.literal (.int 1)))])",
            ),
            // (c) the base literal `1` becomes `5`.
            (
                "base literal",
                "plan:isEven",
                "(.literal (.int 1))",
                "(.literal (.int 5))",
            ),
            // (d) the sibling call mislabelled as a self-call: index 1 IS
            //     planned and in the same group, so only the byte binding of
            //     `isEven`'s real code entry (which tail-calls index 2) sees it.
            (
                "sibling as self-call",
                "plan:isEven",
                "(.tailCall 2 [",
                "(.tailCall 1 [",
            ),
        ],
    );
}

/// Call groups are declared per `fnPlans` entry, and a call must reach a
/// planned function of the SAME or an EARLIER group (`callsOrdered`), so two
/// groups can never vouch for each other. Splitting a mutual SCC across two
/// groups — in either order — leaves one member calling a later group, which
/// the per-plan acceptance declines even though every plan and byte is honest.
#[test]
fn cert_verify_declines_broken_mutual_scc_membership() {
    if !lean_required::lake_available() {
        eprintln!("skipping mutual-SCC group test: `lake` not available");
        return;
    }

    for (fixture, wasm, tampers) in [
        (
            "tools/certkit/fixtures/mutual.av",
            "mutual.wasm",
            vec![
                (
                    "sibling in a later group",
                    "Plans.lean",
                    "⟨\"isOdd\", true, 2, 0,",
                    "⟨\"isOdd\", true, 2, 1,",
                ),
                (
                    "self in a later group",
                    "Plans.lean",
                    "⟨\"isEven\", true, 1, 0,",
                    "⟨\"isEven\", true, 1, 1,",
                ),
            ],
        ),
        (
            "tools/certkit/fixtures/mutual3.av",
            "mutual3.wasm",
            vec![
                (
                    "one member split off",
                    "Plans.lean",
                    "⟨\"rotC\", true, 3, 0,",
                    "⟨\"rotC\", true, 3, 1,",
                ),
                (
                    "first member split off",
                    "Plans.lean",
                    "⟨\"rotA\", true, 1, 0,",
                    "⟨\"rotA\", true, 1, 1,",
                ),
            ],
        ),
    ] {
        let out_dir = compile_checked_fixture(fixture, "cert-mutual-scc");
        assert_package_tampers_decline(&out_dir, wasm, &tampers);
    }
}

/// A tampered sum/list/string plan is declined. `wrapItems` matches a sum
/// and returns a list; `tagName` returns one of three string literals.
/// Constructor tags, arm order, the declared type table and the
/// literal-to-segment table are all bound to the bytes.
#[test]
fn cert_verify_declines_tampered_verbatim_plan() {
    if !lean_required::lake_available() {
        eprintln!("skipping verbatim-plan tamper test: `lake` not available");
        return;
    }

    let out_dir = compile_checked_fixture(
        "tools/certkit/fixtures/verbatimgen.av",
        "cert-verbatim-plan",
    );
    assert_package_tampers_decline(
        &out_dir,
        "verbatimgen.wasm",
        &[
            (
                "zero locals",
                "plan:wrapItems",
                "locals := [(.list .int), .eqref, .int]",
                "locals := []",
            ),
            // (a) the tested constructor: `Items` (user 0 0) -> `Empty` (user 0 1).
            (
                "ref.test constructor",
                "plan:wrapItems",
                "(.ctor (.user 0 0) [1])",
                "(.ctor (.user 0 1) [1])",
            ),
            // (b) the dispatch cascade: the first two arms exchanged.
            (
                "swapped dispatch cascade",
                "plan:tagName",
                "(.cons (.ctor (.user 1 0) []) (.literal (.str [97, 108, 112, 104, 97])) (.cons (.ctor (.user 1 1) []) (.literal (.str [98, 101, 116, 97]))",
                "(.cons (.ctor (.user 1 1) []) (.literal (.str [97, 108, 112, 104, 97])) (.cons (.ctor (.user 1 0) []) (.literal (.str [98, 101, 116, 97]))",
            ),
            // (c) the data-segment index a literal is read from.
            (
                "data-segment index",
                "Plans.lean",
                "([97, 108, 112, 104, 97], 0)",
                "([97, 108, 112, 104, 97], 9)",
            ),
            // (d) the declared List<Int> struct index of the default `[]`.
            (
                "list struct index",
                "Plans.lean",
                "lists := [(.int, 10)]",
                "lists := [(.int, 18)]",
            ),
            // (e) an equal-length payload substitution in the PLAN only: the
            //     lowering reads the same segment index and length, so only the
            //     literal-to-segment pin (`DataPin`) sees the changed bytes.
            (
                "equal-length payload collision (plan)",
                "plan:tagName",
                "(.literal (.str [97, 108, 112, 104, 97]))",
                "(.literal (.str [97, 108, 112, 104, 98]))",
            ),
            // (f) the same substitution in the declared segment table only:
            //     the table no longer holds the segment's bytes
            //     (`dataConfirmed`).
            (
                "equal-length payload collision (segment table)",
                "Plans.lean",
                "([97, 108, 112, 104, 97], 0)",
                "([97, 108, 112, 104, 98], 0)",
            ),
            // (g) the element type of the default list literal.
            (
                "list element type",
                "plan:wrapItems",
                "(.list .int [])",
                "(.list .bool [])",
            ),
        ],
    );
}

/// A compiler-produced, wasmparser-valid scalar-f64 widened match must travel
/// through the plan-backed verbatim bridge end to end. Both the f64 immediate
/// and the declared result kind remain bound to the emitted artifact bytes.
#[test]
fn cert_verify_scalar_f64_verbatim_fixture_and_tampers() {
    if !lean_required::lake_available() {
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
    let (_, _, _, def) = plan_entry_fields(&plans, "floatOrZero");
    let block = plan_def_block(&plans, &def);
    assert!(
        block.contains("⟨[(.sum 0)], .float⟩")
            && block.contains("locals := [.float, .eqref, .int]")
            && block.contains("(.cons .wild (.literal (.float 0)) .nil)"),
        "floatOrZero plan must pin the scalar-f64 result, its locals and the zero default:\n{block}"
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
    }
}

/// A tampered Int-producing dispatch plan is declined: constructor tags, arm
/// order, arithmetic roles, arm constants, operand order, the default and the
/// declared locals all reach the lowered bytes. The helper-role indices are
/// no longer plan data (they live in the template-pinned subject table,
/// covered by `cert_verify_declines_host_role_relabel_in_plans_lean`), and
/// the obligation's host wiring is derived by the wall, so neither a role
/// permutation nor a hostile host builder has a surface to tamper.
#[test]
fn cert_verify_declines_tampered_int_dispatch_plan() {
    if !lean_required::lake_available() {
        eprintln!("skipping int-dispatch-plan tamper test: `lake` not available");
        return;
    }

    let out_dir = compile_checked_fixture(
        "tools/certkit/fixtures/intdispatchgen.av",
        "cert-int-dispatch-plan",
    );
    assert_package_tampers_decline(
        &out_dir,
        "intdispatchgen.wasm",
        &[
            (
                "zero locals",
                "plan:boxInt",
                "locals := [.int, .eqref, .int]",
                "locals := []",
            ),
            (
                "ref.test tag",
                "plan:boxInt",
                "(.ctor (.user 0 0) [1])",
                "(.ctor (.user 0 1) [1])",
            ),
            (
                "swapped dispatch cascade",
                "plan:gauge",
                "(.cons (.ctor (.user 1 0) [1]) (.binOp .sub (.literal (.int 0)) (.local 1)) (.cons (.ctor (.user 1 1) [2])",
                "(.cons (.ctor (.user 1 1) [1]) (.binOp .sub (.literal (.int 0)) (.local 1)) (.cons (.ctor (.user 1 0) [2])",
            ),
            (
                "arithmetic role swap",
                "plan:gauge",
                "(.binOp .sub (.literal (.int 0)) (.local 1))",
                "(.binOp .add (.literal (.int 0)) (.local 1))",
            ),
            (
                "arm constant",
                "plan:gauge",
                "(.binOp .add (.local 2) (.literal (.int 9)))",
                "(.binOp .add (.local 2) (.literal (.int 8)))",
            ),
            (
                "operand order flip",
                "plan:gauge",
                "(.binOp .add (.local 2) (.literal (.int 9)))",
                "(.binOp .add (.literal (.int 9)) (.local 2))",
            ),
            (
                "default constant",
                "plan:gauge",
                "(.literal (.int 7))",
                "(.literal (.int 8))",
            ),
        ],
    );
}

/// End-to-end acceptance and fail-closed tamper coverage for the fused
/// `Option.withDefault(Vector.get(vec, idx), d)` read: a `cellAt`-shaped export
/// reaches CERTIFIED, and each of the three holes an attacker could try to
/// move — the literal default, the declared vector array type, and the
/// to-index/box helper wiring — is pinned, so a consistent rewrite of the
/// attacker-editable package data is DECLINED, never re-credited.
#[test]
fn cert_verify_accepts_fused_vector_read_and_declines_three_tampers() {
    if !lean_required::lake_available() {
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

    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(cert.join("cert-manifest.json")).unwrap())
            .unwrap();
    let to_index_idx = manifest["hostRoleTable"]["toIndex"]
        .as_u64()
        .expect("cell_at declares the index helper");
    let box_idx = manifest["hostRoleTable"]["box"]
        .as_u64()
        .expect("cell_at declares the box helper");
    assert_ne!(to_index_idx, box_idx, "helper roles must be distinct");
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    let vecs_at = plans
        .find("vecs := [(.int, ")
        .expect("cell_at declares Vector<Int>")
        + "vecs := [(.int, ".len();
    let arr_ty: u32 = plans[vecs_at..]
        .split(')')
        .next()
        .unwrap()
        .parse()
        .expect("the Vector<Int> array index");

    // (1) the literal default `0` becomes `1`; (2) the declared Vector<Int>
    //     array type index moves; (3) the index and box helpers swap in the
    //     subject's role table.
    let helper_swap_from = format!("box := some {box_idx}, add := ");
    let helper_swap_to = format!("box := some {to_index_idx}, add := ");
    let vec_from = format!("vecs := [(.int, {arr_ty})]");
    let vec_to = format!("vecs := [(.int, {})]", arr_ty + 1);
    let to_index_from = format!("toIndex := some {to_index_idx}");
    let to_index_to = format!("toIndex := some {box_idx}");
    for (label, file, from, to) in [
        (
            "default literal",
            "plan:cellAt",
            "(.literal (.int 0))",
            "(.literal (.int 1))",
        ),
        (
            "array type",
            "Plans.lean",
            vec_from.as_str(),
            vec_to.as_str(),
        ),
        (
            "helper swap",
            "Manifest.lean",
            helper_swap_from.as_str(),
            helper_swap_to.as_str(),
        ),
    ] {
        let dir = temp_dir(&format!(
            "cert-fused-vector-read-{}",
            label.replace(' ', "-")
        ));
        copy_dir(&out_dir, &dir);
        let tampered = dir.join("cert");
        if let Some(export) = file.strip_prefix("plan:") {
            tamper_export_plan(&tampered.join("Plans.lean"), export, from, to);
        } else {
            replace_once(&tampered.join(file), from, to);
        }
        if label == "helper swap" {
            replace_once(
                &tampered.join("Manifest.lean"),
                &to_index_from,
                &to_index_to,
            );
            let mf = tampered.join("cert-manifest.json");
            let mut m: serde_json::Value =
                serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
            m["hostRoleTable"]["box"] = serde_json::json!(to_index_idx);
            m["hostRoleTable"]["toIndex"] = serde_json::json!(box_idx);
            std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        }
        let (ok, out) = aver_verify(&dir.join("cell_at.wasm"), &tampered);
        assert!(!ok, "tamper `{label}` must be DECLINED:\n{out}");
        assert!(
            out.contains("DECLINED"),
            "tamper `{label}` must report a decline verdict, not an error:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "tamper `{label}` must never re-credit the export:\n{out}"
        );
    }
}

/// The five real source functions the Int value-comparison faces were built
/// for, each certified out of the example module it actually lives in and then
/// VERIFIED — the full kernel replay of the emitted certificate, not the
/// producer's own say-so. Between them they cover all three shapes: the
/// comparison helper plus a signed relational tail (`isExpired`, `atLeast`),
/// the equality helper alone (`sameKey`), and the selection whose result is a
/// passthrough of an input local (`minInt`, `bigger`).
#[test]
fn cert_verify_certifies_the_five_int_comparison_witnesses() {
    if !lean_required::lake_available() {
        eprintln!("skipping Int-comparison witness verify test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    for (source, artifact, witnesses) in [
        (
            "examples/formal/int_comparison_laws.av",
            "int_comparison_laws.wasm",
            &["sameKey", "atLeast"][..],
        ),
        (
            "examples/formal/clock_as_data.av",
            "clock_as_data.wasm",
            &["isExpired"][..],
        ),
        (
            "examples/apps/status_board.av",
            "status_board.wasm",
            &["minInt"][..],
        ),
        (
            "examples/data/fibonacci.av",
            "fibonacci.wasm",
            &["bigger"][..],
        ),
    ] {
        let out_dir = temp_dir("cert-intcmp-witness");
        let compile = aver_command()
            .current_dir(&repo_root)
            .arg("compile")
            .arg(source)
            .arg("--target")
            .arg("wasm-gc")
            .arg("--certify")
            .arg("-o")
            .arg(&out_dir)
            .output()
            .expect("aver compile --certify runs");
        assert!(
            compile.status.success(),
            "compile --certify {source} failed:\n{}{}",
            String::from_utf8_lossy(&compile.stdout),
            String::from_utf8_lossy(&compile.stderr)
        );
        let (ok, report) = aver_verify(&out_dir.join(artifact), &out_dir.join("cert"));
        assert!(ok, "{source} must verify CERTIFIED:\n{report}");
        assert!(
            report.contains("CERTIFIED"),
            "{source} must reach a CERTIFIED verdict:\n{report}"
        );
        for witness in witnesses {
            assert!(
                report.contains(witness),
                "the verdict for {source} must credit `{witness}`:\n{report}"
            );
        }
    }
}

/// Fail-closed tamper coverage for the two comparison host roles, on the
/// module that carries one comparison of each kind. Both edits are CONSISTENT
/// rewrites of the subject's role table (the Lean subject and the JSON agree),
/// the kind a checker that merely re-read the certificate's own claims would
/// re-credit. Each is DECLINED instead: `cmp` is bound to its export name and
/// `eq` to its template, and the plans lower to calls at the declared indices.
#[test]
fn cert_verify_declines_int_comparison_role_tampers() {
    if !lean_required::lake_available() {
        eprintln!("skipping Int-comparison tamper test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-intcmp-tamper");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/formal/int_comparison_laws.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify int_comparison_laws failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let wasm = out_dir.join("int_comparison_laws.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_verify(&wasm, &cert);
    assert!(ok, "the honest control must verify CERTIFIED:\n{report}");

    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(cert.join("cert-manifest.json")).unwrap())
            .unwrap();
    let cmp_idx = manifest["hostRoleTable"]["cmp"]
        .as_u64()
        .expect("the module declares the comparison helper");
    let eq_idx = manifest["hostRoleTable"]["eq"]
        .as_u64()
        .expect("the module declares the equality helper");
    assert_ne!(cmp_idx, eq_idx, "the two helper roles must be distinct");

    let tamper = |name: &str, cmp: u64, eq: u64| {
        let dir = temp_dir(&format!("cert-intcmp-tamper-{name}"));
        copy_dir(&out_dir, &dir);
        let man = dir.join("cert").join("Manifest.lean");
        replace_once(
            &man,
            &format!("cmp := some {cmp_idx}, eq := some {eq_idx}"),
            &format!("cmp := some {cmp}, eq := some {eq}"),
        );
        let mf = dir.join("cert").join("cert-manifest.json");
        let mut m: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mf).unwrap()).unwrap();
        m["hostRoleTable"]["cmp"] = serde_json::json!(cmp);
        m["hostRoleTable"]["eq"] = serde_json::json!(eq);
        std::fs::write(&mf, serde_json::to_string_pretty(&m).unwrap()).unwrap();
        let (ok, out) = aver_verify(&dir.join("int_comparison_laws.wasm"), &dir.join("cert"));
        assert!(!ok, "tamper `{name}` must be DECLINED:\n{out}");
        assert!(
            out.contains("DECLINED"),
            "tamper `{name}` must report a decline verdict, not an error:\n{out}"
        );
        assert!(
            !out.contains("CERTIFIED"),
            "tamper `{name}` must never re-credit an export:\n{out}"
        );
    };

    // (1) The comparison helper declared where the equality helper lives.
    tamper("cmp-index-move", eq_idx, eq_idx);
    // (2) The two roles swapped. Both helpers declare the SAME function type,
    //     so nothing in the type section objects; the export-name binding of
    //     `cmp` and the template of `eq` do.
    tamper("cmp-eq-role-swap", eq_idx, cmp_idx);
}

/// Tamper gate for the record projection-compute face's exact-signature pin:
/// flipping one byte of the certified export's declared parameter reference
/// (nullable `0x63` -> non-nullable `0x64`) with the wasm hash re-bound must
/// DECLINE — the #1209 tamper class, now caught by `funcTypeMatchesExact`.
#[test]
fn cert_tripwire_declines_tampered_record_compute_signature() {
    if !lean_required::lake_available() {
        eprintln!("skipping record-compute tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-recordcompute");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("tools/certkit/fixtures/recordcompute.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "recordcompute compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("recordcompute.wasm");
    let cert = out_dir.join("cert");
    let (ok, report) = aver_check(&wasm, &cert);
    assert!(ok, "clean recordcompute certificate must verify:\n{report}");
    assert!(
        report.contains("2 checked exports"),
        "recordcompute should certify combine and flip:\n{report}"
    );

    let wasm_bytes = std::fs::read(&wasm).unwrap();
    let type_section_range = wasmparser::Parser::new(0)
        .parse_all(&wasm_bytes)
        .find_map(
            |payload| match payload.expect("recordcompute wasm must parse") {
                wasmparser::Payload::TypeSection(reader) => Some(reader.range()),
                _ => None,
            },
        )
        .expect("recordcompute wasm must carry a type section");

    // combine: (Pair, Pair) -> Pair over struct index 0:
    // 0x60, 2 params [ref null 0, ref null 0], 1 result [ref null 0].
    let signature = [0x60, 0x02, 0x63, 0x00, 0x63, 0x00, 0x01, 0x63, 0x00];
    let matches: Vec<usize> = wasm_bytes[type_section_range.clone()]
        .windows(signature.len())
        .enumerate()
        .filter_map(|(offset, win)| (win == signature).then_some(type_section_range.start + offset))
        .collect();
    assert_eq!(
        matches.len(),
        1,
        "combine's two-record signature must be unique in the type section"
    );

    let dir = temp_dir("cert-recordcompute-signature-tamper");
    copy_dir(&out_dir, &dir);
    let w = dir.join("recordcompute.wasm");
    let mut bytes = wasm_bytes.clone();
    bytes[matches[0] + 2] = 0x64;
    std::fs::write(&w, &bytes).unwrap();
    rebind_cert_wasm_hash(&dir, &bytes);

    let (ok, out) = aver_check(&w, &dir.join("cert"));
    assert!(
        !ok,
        "tampered record-compute export signature must be DECLINED:\n{out}"
    );
    assert!(
        out.contains("does not bind") || out.contains("did not build"),
        "wrong reason for record-compute signature tamper:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered record-compute signature credited:\n{out}"
    );
}

/// Tamper suite for the Int comparisons against a literal and between two
/// products. The positive half asserts the clean fixture reaches every
/// comparison shape (a sign test against a literal, `<` and `==` between two
/// computed values, and a comparison over a bare field read), and every
/// tamper — the operator, the literal, the parameter slot, the comparison
/// kind — changes the lowering the wall pins to the code entry.
#[test]
fn cert_tripwire_declines_tampered_int_sign_cmp_plan() {
    if !lean_required::lake_available() {
        eprintln!("skipping sign-template tamper test: `lake` not available");
        return;
    }

    let out_dir =
        compile_checked_fixture("tools/certkit/fixtures/intcompare.av", "cert-intcompare");
    let cert = out_dir.join("cert");
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    for (export, shape) in [
        ("isNonNeg", "(.binOp .gte (.binOp .mul"),
        ("isNonPos", "(.binOp .lte (.binOp .mul"),
        ("below", "(.binOp .lt (.binOp .mul"),
        ("sameValue", "(.binOp .eq (.binOp .mul"),
        (
            "isNonNegField",
            "(.binOp .gte (.project 0 0 (.local 0)) (.literal (.int 0)))",
        ),
    ] {
        let (_, _, _, def) = plan_entry_fields(&plans, export);
        assert!(
            plan_def_block(&plans, &def).contains(shape),
            "{export} must keep its comparison shape `{shape}`:\n{plans}"
        );
    }
    let (ok, report) = aver_check(&out_dir.join("intcompare.wasm"), &cert);
    assert!(ok, "clean intcompare certificate must check:\n{report}");
    assert!(
        report.contains("5 checked exports"),
        "intcompare should certify all five comparisons:\n{report}"
    );

    assert_package_tampers_decline(
        &out_dir,
        "intcompare.wasm",
        &[
            (
                "operator",
                "plan:isNonNeg",
                "(.binOp .gte (.binOp .mul",
                "(.binOp .gt (.binOp .mul",
            ),
            (
                "constant",
                "plan:isNonNegField",
                "(.literal (.int 0)))",
                "(.literal (.int 1)))",
            ),
            (
                "parameter slot",
                "plan:below",
                "(.project 0 0 (.local 0)) (.project 0 1 (.local 1))",
                "(.project 0 0 (.local 1)) (.project 0 1 (.local 1))",
            ),
            (
                "comparison kind",
                "plan:sameValue",
                "(.binOp .eq (.binOp .mul",
                "(.binOp .lt (.binOp .mul",
            ),
        ],
    );
}

/// Every certified export's model is its plan, so `explain` states on the
/// export's own line what that model is: `plan` alone, or `plan ≡ <fn>` once a
/// credited plan-equals-source bridge identifies the plan with the source
/// function. The credit is about the STATEMENT printed under SOURCE-BRIDGES,
/// which `explain` prints from the text the checker rendered and pinned, never
/// from the manifest (the manifest has no statement to print).
///
/// The input domain is disclosed too. The retired record projection-compute
/// face printed a per-export "domain: Int leaves assumed in the runtime's
/// normal form" line; now every export's Int inputs are canonical carriers
/// (`Grammar.SRepr` reads an Int through `CanonRepr`), so `explain` states
/// that assumption once, for all exports, under "Certified domain".
#[test]
fn explain_states_the_record_compute_faces_certified_domain() {
    if !lean_required::lake_available() {
        eprintln!("skipping explain model-line test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let k5_dir = temp_dir("cert-k5-explain-domain");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("projects/k5_fdiv/main.av")
        .arg("--module-root")
        .arg("projects/k5_fdiv")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&k5_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "k5_fdiv compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );
    let (ok, explain) = aver_cert(
        &["explain"],
        &k5_dir.join("main.wasm"),
        &k5_dir.join("cert"),
    );
    assert!(ok, "k5 explain must accept the certificate:\n{explain}");
    assert!(
        explain.contains("Certified domain")
            && explain.contains(
                "domain: every Int input (an argument, or a field, element or payload \
                 inside one) is assumed to be a canonical carrier word, the runtime's \
                 normal form; a non-canonical word is outside the certified domain."
            ),
        "explain must disclose the canonical-carrier assumption on Int inputs:\n{explain}"
    );

    let plus_block: Vec<&str> = explain
        .split("  Domain_Rational_plus\n")
        .nth(1)
        .expect("explain names the ring's addition export")
        .lines()
        .take_while(|line| line.starts_with("    "))
        .collect();
    assert!(
        plus_block
            .iter()
            .any(|line| line.trim() == "class: source-plan-v1 (records)"),
        "the export's line block names its plan class and facets:\n{explain}"
    );
    assert!(
        plus_block.iter().any(|line| {
            line.trim()
                == "model: plan ≡ Domain.Rational.plus (credited source-bridge; see SOURCE-BRIDGES)"
        }),
        "a credited bridge must name the source function on the export's line:\n{explain}"
    );
    assert!(
        explain.contains("SOURCE-BRIDGES")
            && explain
                .contains("Domain_Rational_plus  ≡ Domain.Rational.plus  (exact)  [credited]")
            && explain.contains(
                "_root_.AverCert.GrammarBridge.Exact _root_.AverCert.manifest \
                 \"Domain_Rational_plus\" (fun (x : "
            ),
        "the rendered statement and its credit must be printed under SOURCE-BRIDGES:\n{explain}"
    );
}

/// Law-claims pin (schema 9): a clean k5 package carries eleven kernel-checked
/// law corollaries, all credited, each also bridged to the bytes; the checker-owned witness re-elaborates each
/// corollary at exactly the manifest-declared statement and audits its axioms.
///
/// The two failure modes are deliberately different verdicts. A pin that does
/// not ELABORATE — a statement edited in `Laws.lean` (A) or in the manifest
/// (B) — means the declared claim is not what the package proves, and declines
/// the package. A pin that elaborates but fails its AXIOM AUDIT — a corollary
/// proof degraded to `sorry` (C) — costs that law its credit and nothing else:
/// the export verdict and exit code stand.
#[test]
fn cert_tripwire_declines_tampered_law_claims() {
    if !lean_required::lake_available() {
        eprintln!("skipping law-claims tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-k5-laws");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("projects/k5_fdiv/main.av")
        .arg("--module-root")
        .arg("projects/k5_fdiv")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "k5_fdiv compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let wasm = out_dir.join("main.wasm");
    let cert = out_dir.join("cert");
    let laws_lean = std::fs::read_to_string(cert.join("Laws.lean")).unwrap();
    let manifest = std::fs::read_to_string(cert.join("cert-manifest.json")).unwrap();
    // Every k5 law is fully bridged, so each carries TWO corollaries: the law
    // conjoined with `Holds`, and the `_bridged` one that also conjoins the
    // plan-equals-source identities. They are kept apart so a bridge that
    // cannot be closed costs the second and never the first.
    assert_eq!(
        laws_lean.matches("/-- law-claim `").count(),
        22,
        "k5 package must carry eleven law corollaries and eleven bridged ones"
    );
    assert_eq!(
        laws_lean.matches("theorem _root_.AverCert.Laws.").count()
            - laws_lean.matches("_bridged :\n").count(),
        11,
        "eleven of those corollaries are the plain law-claims"
    );
    assert!(
        manifest.contains("\"label\": \"Domain.Rational.isNonNeg.nonNegOfPositive\""),
        "the when-law must be part of the claimed surface"
    );

    let (ok, report) = aver_check(&wasm, &cert);
    assert!(ok, "clean k5 law-claims certificate must check:\n{report}");
    assert!(
        report.contains("12 checked exports"),
        "k5 should keep its twelve certified exports:\n{report}"
    );
    assert!(
        report.contains("law-claims: 11 of 11 credited"),
        "every k5 law must be credited on a clean package:\n{report}"
    );
    assert!(
        !report.contains("law-claim not credited"),
        "a clean package names no uncredited law:\n{report}"
    );
    assert!(
        report.contains("bridged-laws: 11 of 11 credited"),
        "every k5 law must be bridged to the bytes on a clean package:\n{report}"
    );
    assert!(
        report.contains("source-bridges: 12 of 12 credited"),
        "every k5 export must carry a credited plan-equals-source bridge:\n{report}"
    );

    // Tamper A: edit one law statement inside the package's `Laws.lean`. The
    // corollary no longer has the declared type, so the package build (or the
    // witness) must fail — never a silent re-interpretation.
    let needle = "(Domain.Rational.plus a b) (Domain.Rational.plus b a)";
    assert!(laws_lean.contains(needle), "expected commutative statement");
    let dir = temp_dir("cert-k5-laws-file-tamper");
    copy_dir(&out_dir, &dir);
    std::fs::write(
        dir.join("cert").join("Laws.lean"),
        laws_lean.replacen(
            needle,
            "(Domain.Rational.plus a b) (Domain.Rational.plus a a)",
            1,
        ),
    )
    .unwrap();
    let (ok, out) = aver_check(&dir.join("main.wasm"), &dir.join("cert"));
    assert!(!ok, "tampered Laws.lean statement must be DECLINED:\n{out}");
    assert!(
        !out.contains("CERTIFIED"),
        "tampered Laws.lean credited:\n{out}"
    );

    // Tamper B: edit the same statement in `cert-manifest.json` only. The
    // witness re-elaborates the corollary at the manifest-declared statement,
    // so the declared surface and the package theorem no longer agree.
    let json_needle = "(Domain.Rational.plus a b) (Domain.Rational.plus b a)";
    assert!(
        manifest.contains(json_needle),
        "expected statement in manifest"
    );
    let dir = temp_dir("cert-k5-laws-manifest-tamper");
    copy_dir(&out_dir, &dir);
    std::fs::write(
        dir.join("cert").join("cert-manifest.json"),
        manifest.replacen(
            json_needle,
            "(Domain.Rational.plus a b) (Domain.Rational.plus a a)",
            1,
        ),
    )
    .unwrap();
    let (ok, out) = aver_check(&dir.join("main.wasm"), &dir.join("cert"));
    assert!(
        !ok,
        "tampered manifest law statement must be DECLINED:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "tampered manifest law credited:\n{out}"
    );

    // Tamper C: degrade one corollary proof to `sorry`. The package still
    // builds and the pin still elaborates at the declared statement, so the
    // integrity half of the claim holds and only its axiom audit fails. That
    // costs the LAW its credit and nothing else: the exports the same package
    // certified are untouched, exactly as `declaredUncertified` leaves the
    // rest of a package standing. A pin that does not elaborate is tampers A
    // and B above, and still declines the whole package.
    // The proof term is an anonymous constructor whose arity follows the
    // claim's `bridges` list, so the tamper cuts from the law theorem's name to
    // the closing bracket rather than matching a fixed string.
    let proof_start = laws_lean
        .find("⟨_root_.Domain.Rational.plus_law_commutative,")
        .expect("expected corollary proof term");
    let proof_end = laws_lean[proof_start..]
        .find('⟩')
        .expect("the corollary proof term closes")
        + proof_start
        + '⟩'.len_utf8();
    let dir = temp_dir("cert-k5-laws-sorry-tamper");
    copy_dir(&out_dir, &dir);
    std::fs::write(
        dir.join("cert").join("Laws.lean"),
        format!(
            "{}sorry{}",
            &laws_lean[..proof_start],
            &laws_lean[proof_end..]
        ),
    )
    .unwrap();
    let (ok, out) = aver_check(&dir.join("main.wasm"), &dir.join("cert"));
    assert!(
        ok,
        "a law failing only its axiom audit must not sink the exports:\n{out}"
    );
    assert!(
        out.contains("12 checked exports"),
        "the export verdict must stand unchanged beside an uncredited law:\n{out}"
    );
    assert!(
        out.contains("law-claims: 10 of 11 credited"),
        "the sorry'd law must lose exactly its own credit:\n{out}"
    );
    assert!(
        out.contains(
            "law-claim not credited: Domain.Rational.plus.commutative \
             (proof depends on sorryAx)"
        ),
        "the uncredited law must be named together with the axiom that sank it:\n{out}"
    );
    assert!(
        out.contains("source-bridges: 12 of 12 credited"),
        "a law losing its credit must not take the bridges it cited down:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "`check` never says CERTIFIED:\n{out}"
    );

    // Tamper D — the audit line itself. The witness is checker-authored inside
    // a temporary build directory the test cannot reach, so the fourth case is
    // covered where the line is read: `aver-cert`'s parser unit tests
    // (`law_audit_without_a_line_declines_instead_of_crediting` and
    // `law_audit_rejects_malformed_and_repeated_lines`) hold that a pin whose
    // audit line is missing, renamed, repeated, or malformed DECLINES the
    // package rather than passing unaudited. Credit is only ever granted by a
    // well-formed `ok` line.
}

/// The same tripwire for the plan-equals-source bridge surface (schema 9), on
/// the k5 package whose twelve exports are all bridged (`exact` kind: no k5
/// call closure recurses).
///
/// (A) and (B) are the ways a manifest can try to state something other than
/// what the package proves, now that the entry carries STRUCTURE and the
/// checker renders the statement from it: a permuted record accessor list, or
/// a `model` naming a different source function, renders a different claim,
/// so the pin no longer has the package corollary's type; a smuggled
/// `statement` key, or an encoder kind outside the closed set, is refused at
/// the manifest gate before Lean runs at all. A tautology is not expressible,
/// because the left-hand side of the rendered statement is always the named
/// export's own obligation model.
///
/// (C) A bridge proof degraded to `sorry` still elaborates at the rendered
/// statement, so only its axiom audit fails. It costs the bridge and the
/// BRIDGED corollary of every law that mentions its function — and nothing
/// else: the exports keep their verdict and the plain law-claims keep their
/// credit, which is what keeping `Laws.<c>` and `Laws.<c>_bridged` apart is
/// for. `isNonNeg` is mentioned by exactly one k5 law, so the two law counters
/// separate visibly.
///
/// (D) A law-claim that declares MORE bridges than its bridged corollary
/// proves loses the pinned type and declines. Declaring fewer is the opposite
/// case and deliberately not a decline: the claim is then simply not part of
/// the bridged surface, and nothing is credited that was not proven.
///
/// (E) A per-function STEP lemma that does not close falls to `sorry` instead
/// of failing the build, and it costs exactly the bridges whose call closure
/// contains that function. `isNonPos` calls nothing and no other export calls
/// it, and no k5 law mentions it, so (E) isolates one bridge's credit.
///
/// The audit LINES themselves are covered where they are read, by the parser
/// unit tests in `aver-cert`.
#[test]
fn cert_tripwire_declines_tampered_source_bridges() {
    if !lean_required::lake_available() {
        eprintln!("skipping source-bridge tamper test: `lake` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = temp_dir("cert-k5-bridges");
    let compile = aver_command()
        .current_dir(&repo_root)
        .arg("compile")
        .arg("projects/k5_fdiv/main.av")
        .arg("--module-root")
        .arg("projects/k5_fdiv")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "k5_fdiv compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let cert = out_dir.join("cert");
    let bridge_lean = std::fs::read_to_string(cert.join("BridgeProof.lean")).unwrap();
    let manifest = std::fs::read_to_string(cert.join("cert-manifest.json")).unwrap();
    assert_eq!(
        bridge_lean
            .matches("/-- plan-equals-source bridge for `")
            .count(),
        12,
        "k5 package must carry twelve bridge theorems"
    );
    assert!(
        manifest.contains("\"export\": \"Domain_Rational_isNonPos\"")
            && manifest.contains("\"kind\": \"exact\""),
        "the bridge surface must name every certified export, in the exact kind"
    );
    assert!(
        !manifest.contains("\"statement\": \"∃ o"),
        "a bridge entry must transport structure, never statement text:\n{manifest}"
    );

    // Tamper A: permute one record encoder's accessors. The checker renders
    // `SVal.record 0 [bottom x, top x]` where the package proved
    // `[top x, bottom x]`, so the pin no longer has the corollary's type.
    let top =
        "{\"accessor\": \"_root_.Domain.Rational.Fraction.top\", \"encoder\": {\"kind\": \"int\"}}";
    let bottom = "{\"accessor\": \"_root_.Domain.Rational.Fraction.bottom\", \"encoder\": {\"kind\": \"int\"}}";
    let honest_fields = format!("\"fields\": [{top}, {bottom}]");
    assert!(
        manifest.contains(&honest_fields),
        "expected the Fraction encoder to list its two Int fields in order"
    );
    let permuted_fields = format!("\"fields\": [{bottom}, {top}]");
    let dir = temp_dir("cert-k5-bridge-permuted-fields");
    copy_dir(&out_dir, &dir);
    std::fs::write(
        dir.join("cert").join("cert-manifest.json"),
        manifest.replacen(&honest_fields, &permuted_fields, 1),
    )
    .unwrap();
    let (ok, out) = aver_check(&dir.join("main.wasm"), &dir.join("cert"));
    assert!(!ok, "a permuted encoder must be DECLINED:\n{out}");
    assert!(
        !out.contains("CERTIFIED"),
        "a permuted encoder credited:\n{out}"
    );

    // Tamper A': point one bridge at a different source function. The checker
    // renders the claim about `isNonPos` where the package proved one about
    // `isNonNeg`, so the pin fails to elaborate.
    let honest_model = "\"model\": \"Domain.Rational.isNonNeg\"";
    assert!(
        manifest.contains(honest_model),
        "expected the isNonNeg bridge"
    );
    let dir = temp_dir("cert-k5-bridge-other-model");
    copy_dir(&out_dir, &dir);
    std::fs::write(
        dir.join("cert").join("cert-manifest.json"),
        manifest.replacen(honest_model, "\"model\": \"Domain.Rational.isNonPos\"", 1),
    )
    .unwrap();
    let (ok, out) = aver_check(&dir.join("main.wasm"), &dir.join("cert"));
    assert!(
        !ok,
        "a bridge naming another source function must be DECLINED:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "a bridge naming another source function credited:\n{out}"
    );

    // Tamper B: the shapes the manifest gate refuses outright, before any
    // Lean step — a statement smuggled back in beside the structure, an
    // encoder kind outside the closed set, and a statement kind outside the
    // two the checker renders.
    for (label, tampered) in [
        (
            "a declared statement",
            manifest.replacen(
                "{\"export\": \"Domain_Rational_isNonPos\"",
                "{\"statement\": \"_root_.Domain.Rational.isNonPos = _root_.Domain.Rational.isNonPos\", \
                 \"export\": \"Domain_Rational_isNonPos\"",
                1,
            ),
        ),
        (
            "an unknown encoder kind",
            manifest.replacen("\"kind\": \"record\"", "\"kind\": \"matrix\"", 1),
        ),
        (
            "an unknown statement kind",
            manifest.replacen("\"kind\": \"exact\"", "\"kind\": \"total\"", 1),
        ),
    ] {
        let dir = temp_dir("cert-k5-bridge-manifest-gate");
        copy_dir(&out_dir, &dir);
        std::fs::write(dir.join("cert").join("cert-manifest.json"), tampered).unwrap();
        let (ok, out) = aver_check(&dir.join("main.wasm"), &dir.join("cert"));
        assert!(!ok, "{label} must be DECLINED:\n{out}");
        assert!(!out.contains("CERTIFIED"), "{label} credited:\n{out}");
    }

    // Tamper C: replace one bridge's proof with `sorry`. The theorem still has
    // the rendered statement, so the pin elaborates and only the axiom audit
    // fails — and it costs the bridge and the bridged law-claim that mentions
    // it, never the law-claim itself.
    let sorried = sorry_out_theorem(
        &bridge_lean,
        "_root_.AverCert.Bridge.Domain_Rational_isNonNeg",
    );
    let dir = temp_dir("cert-k5-bridge-sorry-tamper");
    copy_dir(&out_dir, &dir);
    std::fs::write(dir.join("cert").join("BridgeProof.lean"), sorried).unwrap();
    let (ok, out) = aver_check(&dir.join("main.wasm"), &dir.join("cert"));
    assert!(
        ok,
        "a bridge failing only its axiom audit must not sink the exports:\n{out}"
    );
    assert!(
        out.contains("12 checked exports"),
        "the export verdict must stand beside an uncredited bridge:\n{out}"
    );
    assert!(
        out.contains("source-bridges: 11 of 12 credited"),
        "the sorry'd bridge must lose exactly its own credit:\n{out}"
    );
    assert!(
        out.contains(
            "source-bridge not credited: Domain_Rational_isNonNeg \
             (proof depends on sorryAx)"
        ),
        "the uncredited bridge must be named with the axiom that sank it:\n{out}"
    );
    // The decoupling this surface exists for: the law about `isNonNeg` is a
    // claim about the SOURCE model, proved without any bridge, so it keeps its
    // credit while the bridged corollary that cites the broken bridge loses
    // exactly one.
    assert!(
        out.contains("law-claims: 11 of 11 credited"),
        "a broken bridge must not cost a law its credit:\n{out}"
    );
    assert!(
        out.contains("bridged-laws: 10 of 11 credited"),
        "the bridged corollary of the law that mentions it loses credit:\n{out}"
    );
    assert!(
        out.contains(
            "bridged law-claim not credited: Domain.Rational.isNonNeg.nonNegOfPositive \
             (proof depends on sorryAx)"
        ),
        "the uncredited bridged claim must be named:\n{out}"
    );

    // Tamper D: widen a law-claim's declared `bridges` list. The checker builds
    // that law's bridged pin at exactly the declared conjunction, so the
    // package's `_bridged` corollary — which proves one conjunct FEWER — no
    // longer has the pinned type. A claim cannot declare more than it proves.
    let at = manifest
        .find("\"bridges\": [\"")
        .expect("the first k5 law-claim declares the bridges it conjoins");
    let end = manifest[at..].find(']').expect("the bridges array closes") + at;
    let dir = temp_dir("cert-k5-law-bridges-tamper");
    copy_dir(&out_dir, &dir);
    std::fs::write(
        dir.join("cert").join("cert-manifest.json"),
        format!(
            "{}{}, \"Domain_Rational_isNonPos\"{}",
            &manifest[..at],
            &manifest[at..end],
            &manifest[end..]
        ),
    )
    .unwrap();
    let (ok, out) = aver_check(&dir.join("main.wasm"), &dir.join("cert"));
    assert!(
        !ok,
        "a law-claim declaring more bridges than it proves must be DECLINED:\n{out}"
    );
    assert!(
        !out.contains("CERTIFIED"),
        "an over-declared law-claim surface credited:\n{out}"
    );

    // Tamper E: the per-function step lemma of `isNonPos`, degraded to `sorry`.
    // The `first | … | sorry` in the emitted proof is what makes a step that
    // cannot close a not-credited bridge instead of a failed build, and the
    // step is cited only by the bridges whose call closure reaches it. The
    // step lemmas are emitted in slices (`BridgeSteps<k>.lean`), so the
    // tamper finds the slice that carries this one.
    let step_doc = "/-- One step of `Domain.Rational.isNonPos`";
    let (steps_file, steps_lean) = std::fs::read_dir(&cert)
        .unwrap()
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().to_string_lossy().to_string();
            (name.starts_with("BridgeSteps") && name.ends_with(".lean")).then_some(name)
        })
        .map(|name| {
            let text = std::fs::read_to_string(cert.join(&name)).unwrap();
            (name, text)
        })
        .find(|(_, text)| text.contains(step_doc))
        .expect("expected the isNonPos step lemma in a BridgeSteps slice");
    let at = steps_lean.find(step_doc).unwrap();
    let name_at = steps_lean[at..].find("theorem ").expect("the step lemma") + at + 8;
    let name_end = steps_lean[name_at..].find(' ').expect("its name ends") + name_at;
    let step_name = &steps_lean[name_at..name_end];
    let sorried = sorry_out_theorem(&steps_lean, step_name);
    let dir = temp_dir("cert-k5-bridge-step-tamper");
    copy_dir(&out_dir, &dir);
    std::fs::write(dir.join("cert").join(&steps_file), sorried).unwrap();
    let (ok, out) = aver_check(&dir.join("main.wasm"), &dir.join("cert"));
    assert!(
        ok,
        "a step lemma that cannot close must not decline the package:\n{out}"
    );
    assert!(
        out.contains("12 checked exports")
            && out.contains("source-bridges: 11 of 12 credited")
            && out.contains(
                "source-bridge not credited: Domain_Rational_isNonPos \
                 (proof depends on sorryAx)"
            ),
        "the unclosed step costs exactly its own bridge:\n{out}"
    );
    assert!(
        out.contains("law-claims: 11 of 11 credited")
            && out.contains("bridged-laws: 11 of 11 credited"),
        "no k5 law mentions isNonPos, so every law keeps both credits:\n{out}"
    );
}

/// Replace one emitted theorem's tactic proof with `sorry`, leaving its
/// statement — and every other declaration in the file — exactly as emitted.
/// The theorem is found by its `theorem <name> :` header and ends at the blank
/// line before the next doc comment.
fn sorry_out_theorem(lean: &str, name: &str) -> String {
    let header = format!("theorem {name} :");
    let at = lean
        .find(&header)
        .unwrap_or_else(|| panic!("expected the theorem {name}"));
    let rest = &lean[at..];
    let assign = rest.find(" := by\n").expect("expected a tactic proof");
    // The next declaration opens with its `#guard_msgs` isolation line, its
    // doc comment, or the namespace's `end`.
    let end = ["\n\n#guard_msgs", "\n\n/--", "\nend AverCert"]
        .iter()
        .filter_map(|next| rest[assign..].find(next))
        .min()
        .expect("expected the theorem to end before the next declaration")
        + assign;
    format!(
        "{}{} := by\n  sorry{}",
        &lean[..at],
        &rest[..assign],
        &rest[end..]
    )
}
