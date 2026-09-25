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
//!   record vacuous);
//! * a package `Module.lean` declaring a name the wall's `accepted` would
//!   resolve to (the wall imports `Module`, so it is checker-rendered);
//! * a package constant nested under a wall namespace;
//! * a law statement whose literals hide a parenthesis that re-associates the
//!   witness's conjunction;
//! * a section cut (the producer's declared entry lengths) that moves one
//!   byte between two exports, or between two code entries, or one of the
//!   type section;
//! * a package constant a report pin used to read as a dotted path
//!   (`AverCert.manifest.obligations`, `AverCert.manifest.subject.contracts`,
//!   `AverCert.Artifact.data.manifest`, and `.modBytes` on wasip2), with and
//!   without the JSON forged to match it;
//! * a declared layout that lies about a code entry's offset or length, a
//!   function's type index or type, or an export's position;
//! * a closure claim hiding a helper, `__aint_divmod` at a supertype
//!   signature, a renamed `aver:work` import, and a certified closure that
//!   reaches a work import.
//!
//! Gated behind `wasm` and skipped when `lake` is unavailable, like the other
//! certificate suites.
#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;
#[path = "support/scratch_dir.rs"]
mod scratch_dir;

use aver_cert::bridge_statement::{BridgeKind, SourceEncoder, render_bridge_statement};
use aver_cmd::aver_command;
use scratch_dir::{ScratchDir, temp_dir};
use std::path::{Path, PathBuf};

/// `size` reads a `Map`, so the shipped model carries the map prelude and its
/// `AverKeyOrder` class with instances at `Int`, `String` and `Bool`: the clean
/// certificate then shows the audit admits instances of a class the package
/// declares. `size` itself is outside the plan grammar and stays uncertified.
const TINY: &str = "module Tiny
    intent = \"Two bridged exports with one law each.\"
    exposes [addTwo, double, size]

fn size(m: Map<Int, Int>) -> Int
    ? \"Entries.\"
    Map.len(m)

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
    if !lean_required::lake_available() {
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
            && report.contains("bridged-laws: 2 of 2 credited")
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
        "\"corollary\": \"addTwo_isPlusTwo\", \"bridges\": [\"addTwo\"]",
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
    // The `addTwo` law names `Tiny.addTwo`, whose bridge this one replaces,
    // so it no longer lists a bridge.
    edit_json(&cert, |json| {
        for law in json["laws"].as_array_mut().unwrap() {
            if law["label"] == "addTwo.isPlusTwo" {
                law["bridges"] = serde_json::json!([]);
            }
        }
    });
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(
        ok,
        &report,
        "the bridge encoder of Evil does not list exactly its fields in order",
    );
}

/// The wall's `Schema` imports `Module`, and Lean resolves a dotted name in
/// the innermost namespace first. A package `Module.lean` declaring
/// `AverCert.AcceptedArtifact.AverCert.ClaimAxes.checked := true` therefore
/// replaced the runtime-contract conjunct inside the wall's own `accepted`,
/// and a certificate with every runtime contract dropped verified. The
/// checker now renders `Module.lean` from the bytes it read and ignores the
/// package's, so the forged conjunct is the wall's again and the package no
/// longer builds.
#[test]
fn cert_hardening_declines_a_package_module_hijacking_the_wall() {
    let Some((_dir, wasm, cert)) = baseline("certharden-module") else {
        return;
    };
    std::fs::write(
        cert.join("Module.lean"),
        "namespace CertModule\n\
         def wasmSha256 : String := \"0000\"\n\
         end CertModule\n\n\
         def AverCert.AcceptedArtifact.AverCert.ClaimAxes.checked {α : Type} (_ : α) : Bool := \
         true\n",
    )
    .unwrap();
    let contracts_lean = std::fs::read_to_string(cert.join("Manifest.lean")).unwrap();
    let start = contracts_lean
        .find("contracts := [")
        .expect("the manifest lists its contracts");
    let end = start + contracts_lean[start..].find("] }").unwrap() + 1;
    std::fs::write(
        cert.join("Manifest.lean"),
        format!(
            "{}contracts := []{}",
            &contracts_lean[..start],
            &contracts_lean[end..]
        ),
    )
    .unwrap();
    let json_path = cert.join("cert-manifest.json");
    let mut json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&json_path).unwrap()).unwrap();
    json["runtime_contracts"] = serde_json::json!([]);
    std::fs::write(&json_path, serde_json::to_string_pretty(&json).unwrap()).unwrap();
    replace_once(
        &cert.join("Artifact.lean"),
        "theorem axes_ok : AverCert.ClaimAxes.checked data = true := by decide +kernel",
        "",
    );
    replace_once(
        &cert.join("ArtifactCertificate.lean"),
        "strings_ok, axes_ok,",
        "strings_ok, rfl,",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// A package constant under a wall namespace, nested or not, is where a
/// dotted reference in the wall resolves first. The audit declines it even
/// when no wall module imports the package.
#[test]
fn cert_hardening_declines_a_constant_under_a_wall_namespace() {
    let Some((_dir, wasm, cert)) = baseline("certharden-nested") else {
        return;
    };
    append(
        &cert.join("Manifest.lean"),
        "\ndef AverCert.AcceptedArtifact.AverCert.ClaimAxes.checked {α : Type} (_ : α) : Bool := \
         true\n",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "which nests a checker namespace");
}

/// A law statement whose string literals hide parentheses: counted naively
/// it balances, but Lean closes the witness's wrapping parenthesis after the
/// first literal, so `(S) ∧ (Holds) ∧ (bridge)` would parse as
/// `A ∨ (B ∧ Holds ∧ bridge)`, provable by `Or.inl rfl` with no bridge at all.
/// The statement gate lexes literals and refuses it before Lean runs.
#[test]
fn cert_hardening_declines_a_law_statement_that_reassociates_its_pin() {
    let Some((_dir, wasm, cert)) = baseline("certharden-assoc") else {
        return;
    };
    replace_once(
        &cert.join("cert-manifest.json"),
        "\"statement\": \"∀ (a : Int), _root_.Tiny.addTwo a = (a + 2)\"",
        "\"statement\": \"\\\"(\\\" = \\\"(\\\" ) ∨ ( _root_.Tiny.addTwo 0 = _root_.Tiny.addTwo 0 ∧ \
         \\\")\\\" = \\\")\\\"\"",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "is not a single plain term-position line");
}

/// Move one byte from the first entry of the section cut `name` to the second:
/// the count and the total length stay right, and every window after the
/// first two is unchanged.
fn shift_first_cut(layout: &Path, name: &str) {
    let text = std::fs::read_to_string(layout).unwrap();
    let head = format!("def {name} : List Nat :=\n  [");
    let start = text.find(&head).expect("the layout declares the cut") + head.len();
    let end = start + text[start..].find(']').unwrap();
    let mut cuts: Vec<u64> = text[start..end]
        .split(',')
        .map(|x| x.trim().parse().unwrap())
        .collect();
    assert!(cuts.len() >= 2 && cuts[1] > 1, "{name}: {cuts:?}");
    cuts[0] += 1;
    cuts[1] -= 1;
    let body = cuts
        .iter()
        .map(u64::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    std::fs::write(layout, format!("{}{body}{}", &text[..start], &text[end..])).unwrap();
}

/// The section cuts are producer hints: the wall decodes each declared entry
/// on its own window and requires it to fill the window exactly. A cut that
/// moves one byte between two exports decodes neither, and the package
/// declines.
#[test]
fn cert_hardening_declines_a_lying_export_cut() {
    let Some((_dir, wasm, cert)) = baseline("certharden-exportcut") else {
        return;
    };
    shift_first_cut(&cert.join("ArtifactLayout.lean"), "exportCuts");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The same lie about the code section's entries.
#[test]
fn cert_hardening_declines_a_lying_code_cut() {
    let Some((_dir, wasm, cert)) = baseline("certharden-codecut") else {
        return;
    };
    shift_first_cut(&cert.join("ArtifactLayout.lean"), "codeCuts");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

// ---- package constants that extend a name the witness reads ----------------
//
// Lean resolves a dotted identifier to the longest prefix that is a declared
// constant and reads the rest as fields. The report pins used to read dotted
// paths such as `_root_.AverCert.manifest.subject.contracts`, so a package
// constant `AverCert.manifest.subject.contracts` was what that pin meant. The
// pins now read every field through a wall projection function, and the audit
// refuses a package constant under `AverCert` outside the exact producer shapes
// or extending another declared constant. The first two tests below forge the
// JSON the way the attack did and are refused by the pins; the rest keep the
// JSON honest and are refused by the audit.

fn edit_json(cert: &Path, edit: impl FnOnce(&mut serde_json::Value)) {
    let path = cert.join("cert-manifest.json");
    let mut json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
    edit(&mut json);
    std::fs::write(&path, serde_json::to_string_pretty(&json).unwrap()).unwrap();
}

/// (a) A false L3: a package constant `AverCert.manifest.obligations` whose
/// policies are all `simulatesModelTotally`, and a JSON that claims them. The
/// policy pin reads the real obligations, so the forged report does not bind.
#[test]
fn cert_hardening_declines_forged_policies_behind_a_shadowed_obligations() {
    let Some((_dir, wasm, cert)) = baseline("certharden-l3") else {
        return;
    };
    // Appended to `Laws.lean`, which the witness imports after the byte
    // facts, so the package's own proofs keep reading the real manifest.
    append(
        &cert.join("Laws.lean"),
        "\ndef AverCert.manifest.obligations : List AverCert.Schema.Obligation :=\n  \
         (_root_.AverCert.Schema.Manifest.obligations _root_.AverCert.manifest).map fun o =>\n    \
         { o with policy := AverCert.Schema.Policy.simulatesModelTotally, termination? := \
         some (AverCert.Schema.TerminationWitness.mk (.intNatAbs 0) (-1)) }\n",
    );
    edit_json(&cert, |json| {
        json["level"] = serde_json::json!("L3");
        for entry in json["certified"].as_array_mut().unwrap() {
            entry["policy"] = serde_json::json!("simulatesModelTotally");
            entry["level"] = serde_json::json!("L3");
            entry["termination_witness"] = serde_json::json!({
                "measure": {"kind": "intNatAbs", "param_index": 0},
                "descent": -1
            });
        }
    });
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "does not bind to this artifact");
    assert!(report.contains("Obligation.policy"), "{report}");
}

/// (b) Hidden runtime contracts: a package constant
/// `AverCert.manifest.subject.contracts := []` and a JSON with no contracts.
/// The contracts pin reads the real subject's, so the report does not bind.
#[test]
fn cert_hardening_declines_hidden_contracts_behind_a_shadowed_subject_field() {
    let Some((_dir, wasm, cert)) = baseline("certharden-contracts") else {
        return;
    };
    append(
        &cert.join("Manifest.lean"),
        "\ndef AverCert.manifest.subject.contracts : List String := []\n",
    );
    edit_json(&cert, |json| {
        json["runtime_contracts"] = serde_json::json!([]);
    });
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "does not bind to this artifact");
    assert!(report.contains("contracts"), "{report}");
}

/// The same kind of constant with the JSON left honest: the pins elaborate
/// against the real subject, and the audit refuses a package constant under
/// `AverCert` outside the producer's exact shapes.
#[test]
fn cert_hardening_declines_a_constant_under_the_manifest() {
    let Some((_dir, wasm, cert)) = baseline("certharden-subject") else {
        return;
    };
    // In `Manifest.lean` the constant would already redirect the package's
    // own `AverCert.manifest.subject.hostRoleTable` rewrite in `Artifact.lean`
    // and break the build; `Laws.lean` is imported after the byte facts.
    append(
        &cert.join("Laws.lean"),
        "\ndef AverCert.manifest.subject : AverCert.Schema.Subject := AverCert.subject\n",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(
        ok,
        &report,
        "declares AverCert.manifest.subject inside the checker's AverCert namespace",
    );
}

/// (c) A package constant `AverCert.Artifact.data.manifest` is what the
/// dotted pin `data.manifest = manifest` used to read: pointed at a manifest
/// of the package's choosing, laws and bridges were credited against it
/// while the accepted `data` carried the honest one. `AverCert.Artifact.*` is
/// a producer namespace, so the audit refuses it as an extension of the
/// declared constant `AverCert.Artifact.data`.
#[test]
fn cert_hardening_declines_a_constant_extending_the_artifact_data() {
    let Some((_dir, wasm, cert)) = baseline("certharden-datamanifest") else {
        return;
    };
    append(
        &cert.join("Artifact.lean"),
        "\nnoncomputable def AverCert.Artifact.data.manifest : AverCert.Schema.Manifest := \
         AverCert.manifest\n",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(
        ok,
        &report,
        "declares AverCert.Artifact.data.manifest, which extends the declared constant \
         AverCert.Artifact.data",
    );
}

/// The wasip2 form: `report_pin_0` ties the checker's `ArtifactBytes` to
/// `data.modBytes`, and a package constant `AverCert.Artifact.data.modBytes`
/// was what it read.
#[cfg(feature = "wasip2")]
#[test]
fn cert_hardening_declines_a_wasip2_constant_extending_the_artifact_data() {
    if !lake_available() {
        return;
    }
    let dir = temp_dir("certharden-wasip2");
    let compile = aver_command()
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .arg("compile")
        .arg("tests/fixtures/wasip2_carrierless.av")
        .arg("--target")
        .arg("wasip2")
        .arg("--certify")
        .arg("-o")
        .arg(&*dir)
        .output()
        .expect("aver compile --target wasip2 --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let component = dir.join("wasip2_carrierless.component.wasm");
    let cert = dir.join("cert");
    append(
        &cert.join("Artifact.lean"),
        "\nnoncomputable def AverCert.Artifact.data.modBytes : Nat := \
         AverCert.ArtifactBytes.modBytes\n",
    );
    let (ok, report) = aver_cert("check", &component, &cert);
    assert_declined(
        ok,
        &report,
        "declares AverCert.Artifact.data.modBytes, which extends the declared constant \
         AverCert.Artifact.data",
    );
}

// ---- late trust points: the declared layout, the closure, the helpers ------

/// The type section's cut, like the export and code cuts.
#[test]
fn cert_hardening_declines_a_lying_type_cut() {
    let Some((_dir, wasm, cert)) = baseline("certharden-typecut") else {
        return;
    };
    shift_first_cut(&cert.join("ArtifactLayout.lean"), "typeCuts");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("decodeTypesCut"), "{report}");
}

/// Add one to the entry of function 0 in the packed layout table `field`
/// (the lowest 32 bits of its hex numeral).
fn bump_first_layout_entry(layout: &Path, field: &str) {
    let text = std::fs::read_to_string(layout).unwrap();
    let head = format!("{field} := 0x");
    let start = text.find(&head).expect("the layout declares the table") + head.len();
    let end = start
        + text[start..]
            .find(|c: char| !c.is_ascii_hexdigit())
            .unwrap();
    let digits = &text[start..end];
    assert!(digits.len() > 8, "{field}: {digits}");
    let (high, low) = digits.split_at(digits.len() - 8);
    let low = u32::from_str_radix(low, 16).unwrap() + 1;
    std::fs::write(
        layout,
        format!("{}{high}{low:08x}{}", &text[..start], &text[end..]),
    )
    .unwrap();
}

/// A declared code-entry offset one byte off: `layoutConfirmed` reads every
/// declared slice and requires it to be the entry the decoder reads.
#[test]
fn cert_hardening_declines_a_lying_code_offset() {
    let Some((_dir, wasm, cert)) = baseline("certharden-offset") else {
        return;
    };
    bump_first_layout_entry(&cert.join("ArtifactLayout.lean"), "offsets");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("ArtifactLayout"), "{report}");
}

/// A declared code-entry length one byte long.
#[test]
fn cert_hardening_declines_a_lying_code_length() {
    let Some((_dir, wasm, cert)) = baseline("certharden-length") else {
        return;
    };
    bump_first_layout_entry(&cert.join("ArtifactLayout.lean"), "lengths");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("ArtifactLayout"), "{report}");
}

/// A declared function type index naming the next type.
#[test]
fn cert_hardening_declines_a_lying_function_type_index() {
    let Some((_dir, wasm, cert)) = baseline("certharden-typeidx") else {
        return;
    };
    bump_first_layout_entry(&cert.join("ArtifactLayout.lean"), "types");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("ArtifactLayout"), "{report}");
}

/// A declared function type whose result is not the type section's:
/// `fnTypesConfirmed` compares each declaration with the entry at its index.
#[test]
fn cert_hardening_declines_a_lying_function_type() {
    let Some((_dir, wasm, cert)) = baseline("certharden-fntype") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout).unwrap();
    let start = text.find("def fnTypes : List FnType :=\n  [(").unwrap();
    // `(index, [params], [results])`: the first `])` closes the first
    // entry's results; they become `[i64]`.
    let close = start + text[start..].find("])").unwrap() + 1;
    let results = start + text[start..close].rfind(", [").unwrap();
    std::fs::write(
        &layout,
        format!("{}, [.numeric 126]{}", &text[..results], &text[close..]),
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(
        report.contains("typesMatch 0 info.entries fnTypes"),
        "{report}"
    );
}

/// Two planned exports whose declared export positions are swapped: each
/// declaration's position must hold the export of its own name.
#[test]
fn cert_hardening_declines_a_lying_export_position() {
    let Some((_dir, wasm, cert)) = baseline("certharden-exportpos") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout).unwrap();
    let start = text.find("def fnDecls : List FnDecl :=").unwrap();
    let end = start + text[start..].find("\n\n").unwrap();
    // `⟨name, exportPos, sigPos⟩`, one per planned export.
    let decls = &text[start..end];
    let positions: Vec<&str> = decls
        .split('⟩')
        .filter_map(|decl| decl.rsplit_once("], ").map(|(_, rest)| rest))
        .collect();
    assert!(positions.len() >= 2, "{decls}");
    let first = positions[0].split(", ").next().unwrap();
    let second = positions[1].split(", ").next().unwrap();
    assert_ne!(first, second);
    let swapped = decls
        .replacen(&format!("], {first}, "), "], @SWAP@, ", 1)
        .replacen(&format!("], {second}, "), &format!("], {first}, "), 1)
        .replacen("@SWAP@", second, 1);
    std::fs::write(
        &layout,
        format!("{}{swapped}{}", &text[..start], &text[end..]),
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("exportsLazy"), "{report}");
}

/// The closure claim with one reachable helper left out. The claim is checked
/// on the `SortedKeys` path (`closureIsolationL_of_S`) against the closure it
/// recomputes from the code section.
#[test]
fn cert_hardening_declines_a_closure_claim_hiding_a_helper() {
    let Some((_dir, wasm, cert)) = baseline("certharden-closure") else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let text = std::fs::read_to_string(&artifact).unwrap();
    assert!(text.contains("closureIsolationL_of_S"), "{text}");
    let head = "closureClaim := ⟨";
    let start = text.find(head).unwrap() + head.len();
    let end = start + text[start..].find('⟩').unwrap();
    let lists: Vec<Vec<u32>> = text[start..end]
        .split("], ")
        .map(|list| {
            list.trim_matches(|c| c == '[' || c == ']')
                .split(", ")
                .map(|x| x.parse().unwrap())
                .collect()
        })
        .collect();
    let (roots, mut helpers, admitted) = (lists[0].clone(), lists[1].clone(), lists[2].clone());
    let hidden = helpers.pop().expect("the closure has a helper");
    let admitted: Vec<u32> = admitted.into_iter().filter(|x| *x != hidden).collect();
    let show = |xs: &[u32]| {
        format!(
            "[{}]",
            xs.iter().map(u32::to_string).collect::<Vec<_>>().join(", ")
        )
    };
    std::fs::write(
        &artifact,
        format!(
            "{}{}, {}, {}{}",
            &text[..start],
            show(&roots),
            show(&helpers),
            show(&admitted),
            &text[end..]
        ),
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("closureIsolationS"), "{report}");
}

/// Replace the delivered artifact with `bytes` and restamp the package with
/// their hash, so the only lie left is the one in the bytes.
fn restamp(wasm: &Path, cert: &Path, bytes: &[u8]) {
    use sha2::{Digest, Sha256};
    let hex = |digest: &[u8]| {
        digest
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect::<String>()
    };
    let old = hex(&Sha256::digest(std::fs::read(wasm).unwrap()));
    let new = hex(&Sha256::digest(bytes));
    std::fs::write(wasm, bytes).unwrap();
    for file in ["cert-manifest.json", "Manifest.lean"] {
        let path = cert.join(file);
        let text = std::fs::read_to_string(&path).unwrap();
        assert!(text.contains(&old), "{file} names the artifact hash");
        std::fs::write(&path, text.replace(&old, &new)).unwrap();
    }
}

/// The body range of the defined function at absolute index `func` (empty
/// when it is not a defined function) and the byte range of the import
/// section.
fn code_body_and_imports(
    bytes: &[u8],
    func: u32,
) -> (std::ops::Range<usize>, std::ops::Range<usize>) {
    let mut imports = 0u32;
    let mut import_range = 0..0;
    let mut defined = 0u32;
    let mut body = None;
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload.expect("parses") {
            wasmparser::Payload::ImportSection(reader) => {
                import_range = reader.range();
                for group in reader {
                    for import in group.expect("import group") {
                        if let wasmparser::TypeRef::Func(_) = import.expect("import").1.ty {
                            imports += 1;
                        }
                    }
                }
            }
            wasmparser::Payload::CodeSectionEntry(entry) => {
                if imports + defined == func {
                    body = Some(entry.range());
                }
                defined += 1;
            }
            _ => {}
        }
    }
    (body.unwrap_or(0..0), import_range)
}

fn validate(bytes: &[u8]) {
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
        .validate_all(bytes)
        .expect("the tampered module stays valid WebAssembly");
}

/// The module's type of `__aint_divmod` widened to a supertype result
/// (`anyref` for the carrier). The body is unchanged, so its template still
/// matches and the module stays valid (nothing in it calls the helper);
/// `roleTypesPinned` fixes the exact type `carrier carrier i32 -> carrier`.
#[test]
fn cert_hardening_declines_divmod_at_a_supertype_signature() {
    let Some((_dir, wasm, cert)) = baseline("certharden-divmod") else {
        return;
    };
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(cert.join("cert-manifest.json")).unwrap())
            .unwrap();
    let carrier = manifest["carrier_type_index"].as_u64().unwrap() as u8;
    assert!(
        manifest["hostRoleTable"]["divmod"].is_u64(),
        "the tiny module has a divmod helper: {manifest:#}"
    );
    // `func (param carrier carrier i32) (result carrier)`, with the
    // nullable reference `0x63 carrier`; the result becomes `0x63 0x6e`,
    // `(ref null any)`, of the same length.
    let exact = [
        0x60, 3, 0x63, carrier, 0x63, carrier, 0x7f, 1, 0x63, carrier,
    ];
    let mut bytes = std::fs::read(&wasm).unwrap();
    let at: Vec<usize> = bytes
        .windows(exact.len())
        .enumerate()
        .filter(|(_, w)| *w == exact)
        .map(|(i, _)| i)
        .collect();
    assert_eq!(at.len(), 1, "one divmod function type");
    bytes[at[0] + exact.len() - 1] = 0x6e;
    validate(&bytes);
    restamp(&wasm, &cert, &bytes);
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("indicesDistinct"), "{report}");
}

/// Compile the work-job fixture, whose module imports `aver:work/v1`.
fn work_baseline(prefix: &str) -> Option<(ScratchDir, PathBuf, PathBuf)> {
    if !lake_available() {
        return None;
    }
    let dir = temp_dir(prefix);
    let compile = aver_command()
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .arg("compile")
        .arg("tests/fixtures/cert_work_job/main.av")
        .arg("--module-root")
        .arg("tests/fixtures/cert_work_job")
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&*dir)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let wasm = dir.join("main.wasm");
    let cert = dir.join("cert");
    Some((dir, wasm, cert))
}

/// The job imports renamed to `aver:work/v2` in the module and everywhere
/// the package declares them: only the capability registry, which admits
/// exactly `aver:work/v1`, stands between them and acceptance.
#[test]
fn cert_hardening_declines_a_renamed_work_import() {
    let Some((_dir, wasm, cert)) = work_baseline("certharden-workv2") else {
        return;
    };
    let mut bytes = std::fs::read(&wasm).unwrap();
    let (_, imports) = code_body_and_imports(&bytes, 0);
    let (from, to) = (b"aver:work/v1", b"aver:work/v2");
    let mut renamed = 0;
    let mut at = imports.start;
    while at + from.len() <= imports.end {
        if &bytes[at..at + from.len()] == from {
            bytes[at..at + from.len()].copy_from_slice(to);
            renamed += 1;
        }
        at += 1;
    }
    assert_eq!(renamed, 4, "submit, take, task and complete");
    validate(&bytes);
    restamp(&wasm, &cert, &bytes);
    for file in ["cert-manifest.json", "Manifest.lean"] {
        let path = cert.join(file);
        let text = std::fs::read_to_string(&path).unwrap();
        assert_eq!(text.matches("aver:work/v1").count(), 4, "{file}");
        std::fs::write(&path, text.replace("aver:work/v1", "aver:work/v2")).unwrap();
    }
    let chars = "['a', 'v', 'e', 'r', ':', 'w', 'o', 'r', 'k', '/', 'v', '1']";
    let artifact = cert.join("Artifact.lean");
    let text = std::fs::read_to_string(&artifact).unwrap();
    assert_eq!(text.matches(chars).count(), 4, "Artifact.lean");
    std::fs::write(&artifact, text.replace(chars, &chars.replace("'1'", "'2'"))).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(
        report.contains("importsWithinCapabilitiesChars"),
        "{report}"
    );
}

/// A certified closure that reaches a work import. The bignum `strip`
/// sub-routine is in the closure of the certified exports and its body is not
/// pinned by a template, so five of its bytes (`local.get 1; i32.const 1;
/// i32.sub`) become `i32.const 0; call task; ref.is_null`, of the same length
/// and stack effect. The closure scan finds the reachable import.
#[test]
fn cert_hardening_declines_a_certified_closure_reaching_a_work_import() {
    let Some((_dir, wasm, cert)) = work_baseline("certharden-workreach") else {
        return;
    };
    let manifest_lean = std::fs::read_to_string(cert.join("Manifest.lean")).unwrap();
    let strip: u32 = manifest_lean
        .split("strip := ")
        .nth(1)
        .and_then(|rest| rest.split(|c: char| !c.is_ascii_digit()).next())
        .and_then(|n| n.parse().ok())
        .expect("the manifest declares the strip sub-routine");
    let artifact = std::fs::read_to_string(cert.join("Artifact.lean")).unwrap();
    assert!(
        artifact.contains(&format!(", {strip},")),
        "strip is in the certified closure"
    );
    let mut bytes = std::fs::read(&wasm).unwrap();
    // The absolute index of the `task` import.
    let mut task = None;
    let mut index = 0u32;
    for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
        if let wasmparser::Payload::ImportSection(reader) = payload.expect("parses") {
            for group in reader {
                for import in group.expect("import group") {
                    let (_, import) = import.expect("import");
                    if let wasmparser::TypeRef::Func(_) = import.ty {
                        if import.module == "aver:work/v1" && import.name == "task" {
                            task = Some(index);
                        }
                        index += 1;
                    }
                }
            }
        }
    }
    let task = u8::try_from(task.expect("the module imports task")).unwrap();
    assert!(task < 0x80, "a one-byte LEB index");
    let (body, _) = code_body_and_imports(&bytes, strip);
    assert!(!body.is_empty(), "strip is a defined function");
    let pattern = [0x20, 0x01, 0x41, 0x01, 0x6b];
    let at = body.start
        + bytes[body.clone()]
            .windows(pattern.len())
            .position(|w| w == pattern)
            .expect("strip decrements its index");
    bytes[at..at + pattern.len()].copy_from_slice(&[0x41, 0x00, 0x10, task, 0xd1]);
    validate(&bytes);
    restamp(&wasm, &cert, &bytes);
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("closureIsolationS"), "{report}");
}

// ---- law statements read inside a namespace the package chose -----------------
//
// The witness used to elaborate a law statement inside `namespace <prefix>`,
// the manifest's `theorem` minus its last segment. Lean resolves a name in
// the innermost enclosing namespace first, so a package constant
// `<prefix>.Tiny.addTwo` made the statement text `Tiny.addTwo` mean the
// slipped-in function: the law was credited, and bridged through the real
// `Tiny.addTwo`'s bridge, for a property the real function does not have.
// The statements are read at the root now and a bridged model must be spelled
// `_root_.<model>`, so a constant where the law's namespace would resolve a
// mentioned model is harmless and needs no rule of its own.

/// The honest statement of the `addTwo` law, as the manifest and `Laws.lean`
/// spell it.
const ADD_TWO_LAW: &str = "∀ (a : Int), _root_.Tiny.addTwo a = (a + 2)";

/// Point the `addTwo` law at `theorem` (so at its namespace), with
/// `statement` and `bridges` in the manifest.
fn retarget_add_two_law(cert: &Path, theorem: &str, statement: &str, bridges: &[&str]) {
    edit_json(cert, |json| {
        let law = json["laws"]
            .as_array_mut()
            .unwrap()
            .iter_mut()
            .find(|law| law["label"] == "addTwo.isPlusTwo")
            .expect("the addTwo law is claimed");
        law["theorem"] = serde_json::json!(theorem);
        law["statement"] = serde_json::json!(statement);
        law["bridges"] = serde_json::json!(bridges);
    });
}

/// Rewrite `Laws.lean` so both `addTwo` corollaries state `statement` (read
/// inside `namespace`, where `slipped_in` is declared) and prove it by
/// `proof`. Every other corollary stays as produced.
fn slip_into_laws(cert: &Path, namespace: &str, slipped_in: &str, statement: &str, proof: &str) {
    let laws = cert.join("Laws.lean");
    let text = std::fs::read_to_string(&laws).unwrap();
    let honest = format!("({ADD_TWO_LAW})");
    assert_eq!(text.matches(&honest).count(), 2, "{text}");
    let text = text
        .replace(&honest, &format!("({statement})"))
        .replace("⟨_root_.Tiny.addTwo_law_isPlusTwo,", &format!("⟨{proof},"))
        .replacen(
            "set_option autoImplicit false\n",
            &format!("set_option autoImplicit false\n\nnamespace {namespace}\n\n{slipped_in}\n"),
            1,
        );
    std::fs::write(&laws, format!("{text}\nend {namespace}\n")).unwrap();
}

/// Bridged, in the theorem's own namespace: `Tiny.Tiny.addTwo := a + 3`
/// makes `Tiny.addTwo a = a + 3` true inside `namespace Tiny`, and the token
/// matches the bridged model. A bridged model spelled without `_root_.` is
/// refused before Lean runs.
#[test]
fn cert_hardening_declines_a_bridged_law_naming_its_model_unqualified() {
    let Some((_dir, wasm, cert)) = baseline("certharden-lawns-bare") else {
        return;
    };
    let evil = "∀ (a : Int), Tiny.addTwo a = (a + 3)";
    retarget_add_two_law(&cert, "Tiny.addTwo_law_isPlusTwo", evil, &["addTwo"]);
    slip_into_laws(
        &cert,
        "Tiny",
        "def Tiny.addTwo (a : Int) : Int := a + 3",
        evil,
        "fun _ => rfl",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(
        ok,
        &report,
        "names the bridged model `Tiny.addTwo` without `_root_.`",
    );
}

/// Bridged, `_root_`-spelled, with a slipped-in `<ns>.Tiny.addTwo := a + 3`
/// where `<ns>` is the law's namespace (`Evil`, from the theorem `Evil.law`)
/// or the model's own (`Tiny`). The witness reads the statement at the root,
/// where `_root_.Tiny.addTwo` is the real function, so the shadow is
/// harmless: beside the honest statement it changes nothing and the law is
/// credited about the real function; and a statement only the shadow makes
/// true, proved against the shadow inside that namespace, does not bind.
#[test]
fn cert_hardening_a_shadow_of_a_bridged_model_in_the_law_namespace_is_harmless() {
    for (theorem, namespace) in [("Evil.law", "Evil"), ("Tiny.addTwo_law_isPlusTwo", "Tiny")] {
        let shadow = format!("{namespace}.Tiny.addTwo");
        let Some((_dir, wasm, cert)) = baseline("certharden-lawns-shadow") else {
            return;
        };
        retarget_add_two_law(&cert, theorem, ADD_TWO_LAW, &["addTwo"]);
        append(
            &cert.join("Laws.lean"),
            &format!("\ndef {shadow} (a : Int) : Int := a + 3\n"),
        );
        let (ok, report) = aver_cert("verify", &wasm, &cert);
        assert!(
            ok,
            "a harmless shadow `{shadow}` must not decline:\n{report}"
        );
        assert!(
            report.contains("CERTIFIED")
                && report.contains("law-claims: 2 of 2 credited")
                && report.contains("bridged-laws: 2 of 2 credited"),
            "{report}"
        );

        let Some((_dir, wasm, cert)) = baseline("certharden-lawns-shadow-false") else {
            return;
        };
        let pinned = "∀ (a : Int), _root_.Tiny.addTwo a = (a + 3)";
        retarget_add_two_law(&cert, theorem, pinned, &["addTwo"]);
        slip_into_laws(
            &cert,
            namespace,
            "def Tiny.addTwo (a : Int) : Int := a + 3",
            "∀ (a : Int), Tiny.addTwo a = (a + 3)",
            "fun _ => rfl",
        );
        let (ok, report) = aver_cert("check", &wasm, &cert);
        assert_declined(ok, &report, "does not bind to this artifact");
        assert!(
            report.contains(&format!("{shadow} a = a + 3"))
                && report.contains("AverCertChecker.law_statement_0"),
            "{report}"
        );
    }
}

/// An entry module `Tiny` beside a dependency `Foo.Tiny`, each with an
/// `addTwo` and a law about it.
const TWO_TINY_ENTRY: &str = "module Tiny
    intent = \"An entry module whose name is also the last segment of a dependency.\"
    exposes [addTwo, viaFoo]
    depends [Foo.Tiny]

fn addTwo(x: Int) -> Int
    ? \"Adds two.\"
    x + 2

verify addTwo law isPlusTwo
    given a: Int = [0, 1, 2]
    addTwo(a) => a + 2

fn viaFoo(x: Int) -> Int
    ? \"Adds three through the dependency.\"
    Foo.Tiny.addTwo(x)

verify viaFoo
    viaFoo(1) => 4
";

const TWO_TINY_DEP: &str = "module Tiny
    intent = \"A dependency whose function shares the entry function's name.\"
    exposes [addTwo]

fn addTwo(x: Int) -> Int
    ? \"Adds three, despite the name.\"
    x + 3

verify addTwo law isPlusThree
    given a: Int = [0, 1, 2]
    addTwo(a) => a + 3
";

/// An honest program whose model declares `Foo.Tiny.addTwo` beside
/// `Tiny.addTwo`, with a law in `Foo.Tiny` and a law naming
/// `_root_.Tiny.addTwo`. Read at the root, every statement means the function
/// it spells, so nothing here is a shadow and the package is accepted with
/// every claim credited.
#[test]
fn cert_hardening_accepts_a_model_named_inside_another_laws_namespace() {
    if !lake_available() {
        return;
    }
    let dir = temp_dir("certharden-two-tiny");
    std::fs::create_dir_all(dir.join("foo")).unwrap();
    std::fs::write(dir.join("tiny.av"), TWO_TINY_ENTRY).unwrap();
    std::fs::write(dir.join("foo").join("tiny.av"), TWO_TINY_DEP).unwrap();
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
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(cert.join("cert-manifest.json")).unwrap())
            .unwrap();
    let laws = manifest["laws"].as_array().unwrap();
    // The shape a namespace-prefix shadow rule would refuse: a law in
    // `Foo.Tiny`, a law naming `_root_.Tiny.addTwo`, and a model constant
    // `Foo.Tiny.addTwo`.
    assert!(
        laws.iter()
            .any(|law| law["theorem"] == "Foo.Tiny.addTwo_law_isPlusThree"),
        "{manifest}"
    );
    assert!(
        laws.iter().any(|law| law["statement"]
            .as_str()
            .unwrap()
            .contains("_root_.Tiny.addTwo a")),
        "{manifest}"
    );
    assert!(
        manifest["sourceBridges"]
            .as_array()
            .unwrap()
            .iter()
            .any(|bridge| bridge["model"] == "Foo.Tiny.addTwo"),
        "{manifest}"
    );
    let (ok, report) = aver_cert("verify", &wasm, &cert);
    assert!(ok, "the honest two-Tiny certificate must verify:\n{report}");
    assert!(
        report.contains("CERTIFIED")
            && report.contains("law-claims: 2 of 2 credited")
            && report.contains("bridged-laws: 2 of 2 credited")
            && report.contains("source-bridges: 3 of 3 credited"),
        "{report}"
    );
}

/// A term-level `set_option … in` or `open … in` inside a law statement is
/// refused by the statement gate before Lean runs: the first would bypass the
/// package gate's option whitelist, the second change what the statement's
/// names mean. The producer writes neither.
#[test]
fn cert_hardening_declines_set_option_and_open_in_a_law_statement() {
    for (prefix, statement) in [
        (
            "certharden-law-set-option",
            "set_option maxRecDepth 100 in ∀ (a : Int), _root_.Tiny.addTwo a = (a + 2)",
        ),
        (
            "certharden-law-open",
            "open _root_.Tiny in ∀ (a : Int), _root_.Tiny.addTwo a = (a + 2)",
        ),
    ] {
        let Some((_dir, wasm, cert)) = baseline(prefix) else {
            return;
        };
        retarget_add_two_law(&cert, "Tiny.addTwo_law_isPlusTwo", statement, &["addTwo"]);
        let (ok, report) = aver_cert("check", &wasm, &cert);
        assert_declined(
            ok,
            &report,
            "statement is not a single plain term-position line",
        );
    }
}

/// Not bridged: the same redirect used to move a plain law-claim's credit.
/// `Tiny.addTwo a = a + 3` is false of the real `Tiny.addTwo`, and true of a
/// slipped-in `Evil.Tiny.addTwo` (theorem `Evil.law`) or `Tiny.Tiny.addTwo`
/// inside the namespace the package's corollary is written in. Read at the
/// root, the pinned statement is about the real function, so the package's
/// corollary no longer proves it and the certificate does not bind.
#[test]
fn cert_hardening_declines_a_plain_law_about_a_slipped_in_function() {
    // Declared inside `namespace <ns>`, so the constant is `<ns>.Tiny.addTwo`.
    let slipped_in = "def Tiny.addTwo (a : Int) : Int := a + 3";
    for (theorem, namespace) in [("Evil.law", "Evil"), ("Tiny.addTwo_law_isPlusTwo", "Tiny")] {
        let Some((_dir, wasm, cert)) = baseline("certharden-lawns-plain") else {
            return;
        };
        let evil = "∀ (a : Int), Tiny.addTwo a = (a + 3)";
        retarget_add_two_law(&cert, theorem, evil, &[]);
        slip_into_laws(&cert, namespace, slipped_in, evil, "fun _ => rfl");
        let (ok, report) = aver_cert("check", &wasm, &cert);
        assert_declined(ok, &report, "does not bind to this artifact");
        assert!(
            report.contains(&format!("{namespace}.Tiny.addTwo a = a + 3"))
                && report.contains("AverCertChecker.law_statement_0"),
            "{report}"
        );
    }
}

/// Bridged, `_root_`-spelled, but in a position elaboration drops (a type
/// ascription's type): the statement is `True` and never uses the bridged
/// function. The audit reads the elaborated statement and refuses it.
#[test]
fn cert_hardening_declines_a_bridged_law_that_does_not_use_its_model() {
    let Some((_dir, wasm, cert)) = baseline("certharden-lawns-unused") else {
        return;
    };
    let hollow = "∀ (a : Int), (True : (fun (_ : Int → Int) => Prop) _root_.Tiny.addTwo)";
    retarget_add_two_law(&cert, "Tiny.addTwo_law_isPlusTwo", hollow, &["addTwo"]);
    let laws = cert.join("Laws.lean");
    let text = std::fs::read_to_string(&laws)
        .unwrap()
        .replace(&format!("({ADD_TWO_LAW})"), &format!("({hollow})"))
        .replace("⟨_root_.Tiny.addTwo_law_isPlusTwo,", "⟨fun _ => trivial,");
    std::fs::write(&laws, text).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(
        ok,
        &report,
        "the law statement AverCertChecker.law_statement_0 does not use the bridged model \
         Tiny.addTwo",
    );
}
