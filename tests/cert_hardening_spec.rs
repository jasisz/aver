//! Tamper tests for the checker's trust boundary around package Lean text.
//!
//! Each test emits one small certificate (two exports, two law-claims, two
//! source-bridges), applies ONE hostile edit to the package, and demands a
//! decline with the reason pinned. The edits are the attacks the checker must
//! refuse:
//!
//! * a shadow of the accepted predicate and root under the witness's own
//!   namespace (`AverCertChecker.AverCert.…`), so an unqualified witness name
//!   would reach the shadow;
//! * `set_option debug.skipKernelTC true`, which lets `decide +kernel` add a
//!   false lemma the kernel never checks;
//! * a package instance of a core class at a core type (`LE Nat` meaning
//!   `False`, which made the exact bridge kind's fuel bound vacuous), the same
//!   instance behind a name alias, and a `sorry`-backed decision procedure a
//!   report pin's `decide +kernel` picks up;
//! * a law-claim listing a bridge of a function its statement never names;
//! * a bridge over a record whose encoder lists only some of its fields (the
//!   omitted one a proof of `False`, which makes every quantifier over the
//!   record vacuous).
//!
//! Gated behind `wasm` and skipped when `lake` is unavailable, like the other
//! certificate suites.
#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/scratch_dir.rs"]
mod scratch_dir;

use aver_cert::bridge_statement::{BridgeKind, SourceEncoder, render_bridge_statement};
use aver_cmd::aver_command;
use scratch_dir::{ScratchDir, temp_dir};
use std::path::{Path, PathBuf};
use std::process::Command;

const TINY: &str = "module Tiny
    intent = \"Two bridged exports with one law each.\"
    exposes [addTwo, double]

fn addTwo(x: Int) -> Int
    ? \"Adds two.\"
    x + 2

verify addTwo
    addTwo(1) => 3

verify addTwo law isPlusTwo
    given a: Int = [0, 1, 2]
    addTwo(a) => a + 2

fn double(x: Int) -> Int
    ? \"Doubles.\"
    x + x

verify double law isAddTwice
    given a: Int = [0, 1, 2]
    double(a) => a + a
";

fn lake_available() -> bool {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping cert hardening test: `lake` not available");
        return false;
    }
    true
}

/// Emit the baseline certificate into a fresh scratch directory.
fn baseline(prefix: &str) -> Option<(ScratchDir, PathBuf, PathBuf)> {
    if !lake_available() {
        return None;
    }
    let dir = temp_dir(prefix);
    std::fs::write(dir.join("tiny.av"), TINY).unwrap();
    let out = dir.join("out");
    let compile = aver_command()
        .current_dir(&*dir)
        .arg("compile")
        .arg("tiny.av")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let wasm = out.join("tiny.wasm");
    let cert = out.join("cert");
    Some((dir, wasm, cert))
}

fn aver_cert(sub: &str, artifact: &Path, cert_dir: &Path) -> (bool, String) {
    let out = aver_command()
        .arg("cert")
        .arg(sub)
        .arg(artifact)
        .arg(cert_dir)
        .output()
        .expect("aver cert runs");
    (
        out.status.success(),
        format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        ),
    )
}

fn replace_once(path: &Path, needle: &str, replacement: &str) {
    let text = std::fs::read_to_string(path).unwrap();
    assert!(
        text.contains(needle),
        "{} should contain `{needle}`",
        path.display()
    );
    std::fs::write(path, text.replacen(needle, replacement, 1)).unwrap();
}

fn append(path: &Path, text: &str) {
    let mut contents = std::fs::read_to_string(path).unwrap();
    contents.push_str(text);
    std::fs::write(path, contents).unwrap();
}

fn assert_declined(ok: bool, report: &str, reason: &str) {
    assert!(!ok, "the tampered certificate must be declined:\n{report}");
    assert!(
        report.contains(reason),
        "declined for the wrong reason (expected `{reason}`):\n{report}"
    );
}

/// The honest certificate verifies end to end with every claim credited, so
/// the declines below are about their tamper and nothing else.
#[test]
fn cert_hardening_accepts_the_clean_certificate() {
    let Some((_dir, wasm, cert)) = baseline("certharden-clean") else {
        return;
    };
    let (ok, report) = aver_cert("verify", &wasm, &cert);
    assert!(ok, "clean certificate must verify:\n{report}");
    assert!(
        report.contains("CERTIFIED")
            && report.contains("law-claims: 2 of 2 credited")
            && report.contains("source-bridges: 2 of 2 credited"),
        "{report}"
    );
}

/// B1: the acceptance root replaced by a shadow under the witness namespace.
/// Every witness name is `_root_`-qualified, and the prefix is reserved.
#[test]
fn cert_hardening_declines_a_shadow_under_the_witness_namespace() {
    let Some((_dir, wasm, cert)) = baseline("certharden-shadow") else {
        return;
    };
    std::fs::write(
        cert.join("ArtifactCertificate.lean"),
        "import Artifact\nimport Final\n\n\
         namespace AverCertChecker.AverCert.AcceptedArtifact\n\
         def accepted (_ : _root_.AverCert.AcceptedArtifact.ArtifactData) : Prop := True\n\
         end AverCertChecker.AverCert.AcceptedArtifact\n\n\
         namespace AverCertChecker.AverCert.Artifact\n\
         theorem certificate :\n    \
         AverCertChecker.AverCert.AcceptedArtifact.accepted _root_.AverCert.Artifact.data := \
         trivial\n\
         end AverCertChecker.AverCert.Artifact\n",
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "`AverCertChecker`");
}

/// B2: a false law proved through a lemma the kernel never checked.
#[test]
fn cert_hardening_declines_a_skipped_kernel_check() {
    let Some((_dir, wasm, cert)) = baseline("certharden-skiptc") else {
        return;
    };
    let laws = cert.join("Laws.lean");
    replace_once(
        &laws,
        "set_option autoImplicit false\n",
        "set_option autoImplicit false\n\n\
         set_option\n  debug.skipKernelTC true in\n\
         theorem bogus : (1 : Nat) = 2 := by decide +kernel\n",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "`set_option`");
}

/// B3: `≤` on `Nat` redefined as `False`. The bridge statements no longer
/// spell `≤` at all (they go through the wall's `Exact`), and the audit
/// refuses a package instance of a core class at a core type.
#[test]
fn cert_hardening_declines_a_core_class_instance() {
    let Some((_dir, wasm, cert)) = baseline("certharden-le") else {
        return;
    };
    append(
        &cert.join("Bridge.lean"),
        "\ninstance (priority := high) evilLE : LE Nat := ⟨fun _ _ => False⟩\n",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "of class LE is not admitted");
}

/// B3, disguised: the same instance declared through an alias of the class.
/// The audit reads the class off the elaborated declaration.
#[test]
fn cert_hardening_declines_an_aliased_core_class_instance() {
    let Some((_dir, wasm, cert)) = baseline("certharden-alias") else {
        return;
    };
    append(
        &cert.join("Bridge.lean"),
        "\nabbrev Order (α : Type) := LE α\n\
         instance (priority := high) evilOrder : Order Nat := ⟨fun _ _ => False⟩\n",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "is not admitted in a certificate");
}

/// B3: a report forged through a `sorry`-backed decision procedure. The
/// report pins are theorems now, audited with the accepted root.
#[test]
fn cert_hardening_declines_a_sorry_backed_report_pin() {
    let Some((_dir, wasm, cert)) = baseline("certharden-dec") else {
        return;
    };
    replace_once(
        &cert.join("Laws.lean"),
        "set_option autoImplicit false\n",
        "set_option autoImplicit false\n\n\
         instance (priority := high) forgedDec : DecidableEq (List (String × List String)) :=\n  \
         fun _ _ => isTrue sorry\n",
    );
    let manifest = cert.join("cert-manifest.json");
    replace_once(
        &manifest,
        "{\"name\": \"addTwo\", \"class\": \"source-plan-v1\", \"facets\": []",
        "{\"name\": \"addTwo\", \"class\": \"source-plan-v1\", \"facets\": [\"forged\"]",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "non-whitelisted axiom: sorryAx");
}

/// A law-claim that conjoins the bridge of a function its statement never
/// names is refused before Lean runs.
#[test]
fn cert_hardening_declines_a_law_citing_an_unrelated_bridge() {
    let Some((_dir, wasm, cert)) = baseline("certharden-lawtie") else {
        return;
    };
    replace_once(
        &cert.join("cert-manifest.json"),
        "\"corollary\": \"addTwo_isPlusTwo\", \"bridges\": []",
        "\"corollary\": \"addTwo_isPlusTwo\", \"bridges\": [\"double\"]",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "not exactly those of the functions");
}

/// A bridge over a record whose encoder omits a field — here a proof of
/// `False`, so the statement quantifies over no value at all and holds
/// vacuously. The corollary genuinely proves the pinned statement; the
/// audit refuses the encoder.
#[test]
fn cert_hardening_declines_a_record_encoder_missing_a_field() {
    let Some((_dir, wasm, cert)) = baseline("certharden-record") else {
        return;
    };
    let record = SourceEncoder::Record {
        tid: 0,
        lean_type: "_root_.Evil".to_string(),
        fields: vec![("_root_.Evil.a".to_string(), SourceEncoder::Int)],
    };
    let statement = render_bridge_statement(
        "addTwo",
        "Evil.fake",
        BridgeKind::Exact,
        &[record],
        &SourceEncoder::Int,
    );
    let bridge = cert.join("Bridge.lean");
    let text = std::fs::read_to_string(&bridge).unwrap();
    let start = text
        .find("#guard_msgs (drop error) in\n/-- The claim the manifest names")
        .expect("the addTwo corollary");
    let end = text[start + 1..]
        .find("#guard_msgs (drop error) in")
        .map(|at| at + start + 1)
        .expect("the double corollary follows");
    let replacement = format!(
        "structure Evil where\n  a : Int\n  h : False\n\n\
         def Evil.fake (e : Evil) : Int := e.a\n\n\
         theorem _root_.AverCert.Bridge.addTwo_certified :\n    ({statement}) ∧ \
         (_root_.AverCert.Schema.Holds _root_.AverCert.manifest) :=\n  \
         ⟨by\n    unfold _root_.AverCert.GrammarBridge.Exact\n    \
         exact match _root_.AverCert.Bridge.addTwo with\n      \
         | ⟨o, ho, _, _⟩ => ⟨o, ho, fun x => x.h.elim, 0, fun _ _ x => x.h.elim⟩,\n   \
         _root_.AverCert.Final.cert⟩\n\n"
    );
    std::fs::write(
        &bridge,
        format!("{}{replacement}{}", &text[..start], &text[end..]),
    )
    .unwrap();
    let manifest = cert.join("cert-manifest.json");
    replace_once(
        &manifest,
        "\"model\": \"Tiny.addTwo\", \"kind\": \"exact\", \"params\": [{\"kind\": \"int\"}]",
        "\"model\": \"Evil.fake\", \"kind\": \"exact\", \"params\": [{\"kind\": \"record\", \
         \"tid\": 0, \"type\": \"_root_.Evil\", \"fields\": [{\"accessor\": \"_root_.Evil.a\", \
         \"encoder\": {\"kind\": \"int\"}}]}]",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(
        ok,
        &report,
        "the bridge encoder of Evil does not list exactly its fields in order",
    );
}
