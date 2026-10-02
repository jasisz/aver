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
//!   byte between two exports, or one of the type section;
//! * on the chunked byte path: a code slice moved by one byte that still
//!   tiles, overlapping code entries, a gap between code entries, a lying
//!   code count, a false section header, a header inside a payload, a header
//!   past the end of the module, a plan whose packed lowering is not its code
//!   entry, swapped export sites, role bits hiding a helper call, and a package chunk list with a
//!   wrong chunk boundary;
//! * a package constant a report pin used to read as a dotted path
//!   (`AverCert.manifest.obligations`, `AverCert.manifest.subject.contracts`,
//!   `AverCert.Artifact.data.manifest`, and `.modBytes` on wasip2), with and
//!   without the JSON forged to match it;
//! * a declared layout that lies about a code entry's offset or length, a
//!   function's type index or type, or an export's site;
//! * a closure claim hiding a helper, a declared callee list missing a call,
//!   naming a callee outside the closure, declared twice, or out of the
//!   fold's order, `__aint_divmod` at a supertype
//!   signature, a renamed `aver:work` import, and a certified closure that
//!   reaches a work import;
//! * a List match whose arm results are exchanged, List cons structs declared
//!   for each other's instantiation, and a cons pattern whose head and tail
//!   slots are exchanged;
//! * a List literal's cons helper declared as a user function of the same
//!   signature, a cons helper whose plan is not the cons plan, and the cons
//!   helpers of two instantiations exchanged;
//! * a bridge whose List argument encoder names another element type, a
//!   List-of-records bridge whose element encoder names another type, a
//!   List-helper bridge naming a source function that does not call the
//!   plan's helper, and a source model whose helper is not the plan's;
//! * a duplicate planned function index that passes every per-plan check,
//!   call groups numbered against the plans' order (which must check
//!   unchanged), a conjunct of the plans' acceptance proved by `sorry`,
//!   declared-uncertified names out of key order, listed twice, left out or
//!   spelled unlike their export, and export names for the bridges'
//!   distinctness that are not the obligations' names;
//! * on the export walk: an export section out of key order (with every
//!   declared offset kept true), a block boundary declared inside an entry,
//!   and site bits that hand a declared-uncertified export to the plans;
//! * on the type section by index: a top-level entry declared at a wrong
//!   offset, a byte moved between two rec subtypes, and the rec group
//!   declared one subtype short; on the data section: a byte moved between
//!   two segments and a block lying about its first segment; an Int helper's
//!   export site moved, and an obligation's policy answer flipped;
//! * on the type walk: a shape bit on a type without a helper's shape, a
//!   String helper's type with its shape bit cleared, a helper-shaped type
//!   declared with another signature, and a block of functions that drops
//!   its String helper.
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
    baseline_source(prefix, TINY)
}

fn baseline_source(prefix: &str, source: &str) -> Option<(ScratchDir, PathBuf, PathBuf)> {
    if !lake_available() {
        return None;
    }
    let dir = temp_dir(prefix);
    std::fs::write(dir.join("tiny.av"), source).unwrap();
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

/// Export accounting can fail in the wall's walk or in its proved-equivalent
/// character helper. The diagnostic must still identify that check; rejection
/// itself and the build-failure reason are asserted separately by each test.
fn assert_export_walk_error(report: &str) {
    assert!(
        report.contains("walkExports") || report.contains("ExportChars.walk"),
        "{report}"
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

/// Report blocks supply proofs only. Omitting their import leaves the same
/// report checked by the witness's direct-computation fallback.
#[test]
fn cert_hardening_checks_without_optional_report_proofs() {
    let Some((_dir, wasm, cert)) = baseline("certharden-report-fallback") else {
        return;
    };
    let (ok, with_blocks) = aver_cert("check", &wasm, &cert);
    assert!(ok, "{with_blocks}");
    replace_once(
        &cert.join("ArtifactCertificate.lean"),
        "import ArtifactReports\n",
        "",
    );
    let (ok, without_blocks) = aver_cert("check", &wasm, &cert);
    assert!(ok, "{without_blocks}");
    assert_eq!(with_blocks, without_blocks);
}

/// A block lemma is reached by the report pin's axiom audit, even though it
/// is not a dependency of the accepted-artifact theorem itself.
#[test]
fn cert_hardening_declines_a_sorry_backed_report_block() {
    let Some((_dir, wasm, cert)) = baseline("certharden-report-block-axiom") else {
        return;
    };
    replace_once(
        &cert.join("ArtifactReportBlocks0.lean"),
        "= report_facets_0 := by\n  decide +kernel",
        "= report_facets_0 := by\n  sorry",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "non-whitelisted axiom: sorryAx");
}

/// Exercise the join at 64 entries and its non-empty final tail. A missing
/// entry at that boundary must fail the block's own kernel equality.
#[test]
fn cert_hardening_report_blocks_check_the_last_export() {
    let names = (0..65).map(|i| format!("f{i}")).collect::<Vec<_>>();
    let mut source = format!(
        "module ReportBlocks\n    intent = \"A report across a block boundary.\"\n    exposes [{}]\n\n",
        names.join(", ")
    );
    for (i, name) in names.iter().enumerate() {
        source.push_str(&format!("fn {name}(x: Int) -> Int\n    x + {i}\n\n"));
    }
    let Some((_dir, wasm, cert)) = baseline_source("certharden-report-tail", &source) else {
        return;
    };
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(ok && report.contains("65 checked exports"), "{report}");
    let block = cert.join("ArtifactReportBlocks0.lean");
    let text = std::fs::read_to_string(&block).unwrap();
    let start = text.find("def report_entries_1 :").expect("second block");
    let value = start + text[start..].find("[(").expect("report entry pair");
    let end = value + text[value..].find(']').unwrap() + 1;
    std::fs::write(&block, format!("{}[]{}", &text[..value], &text[end..])).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "= report_entries_1");
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
        "theorem axes_ok : AverCert.ClaimAxes.checked data = true :=\n  \
         AverCert.ScaleLayout.checked_of_bits plans_roles\n    \
         (AverCert.ScaleTables.checkedBits_of_pols axes_pols (by decide +kernel))",
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
    shift_first_cut(&cert.join("ArtifactLayout.lean"), "exportCuts_0");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

// ---- the byte path: chunks, framing, code tiling, packed plans --------------
//
// The module is read through checker-rendered 1 KiB chunks. The producer
// declares every section header's offset, every code entry's offset and
// length, and each plan's role bits; the wall reads each on its own chunk
// window and requires the declarations to chain from the magic to the end of
// the module (`ScaleLayout.framingOk`) and to tile the code section
// (`ScaleLayout.codeTiled`). Each test below tells one lie that keeps the
// rest of the package intact.

/// The packed layout table `field` as its entries, entry 0 first, and a
/// writer for it (`packed_hex` in `layout.rs`: `0x0` then 8 hex digits per
/// entry, the last entry first).
fn layout_table(layout: &Path, field: &str) -> (Vec<i64>, impl Fn(&[i64])) {
    let text = std::fs::read_to_string(layout).unwrap();
    let head = format!("{field} := 0x0");
    let start = text.find(&head).expect("the layout declares the table") + head.len();
    let end = start
        + text[start..]
            .find(|c: char| !c.is_ascii_hexdigit())
            .unwrap();
    let digits = &text[start..end];
    assert_eq!(digits.len() % 8, 0, "{field}: {digits}");
    let mut entries: Vec<i64> = digits
        .as_bytes()
        .chunks(8)
        .map(|d| i64::from_str_radix(std::str::from_utf8(d).unwrap(), 16).unwrap())
        .collect();
    entries.reverse();
    let (before, after) = (text[..start].to_string(), text[end..].to_string());
    let path = layout.to_path_buf();
    let write = move |entries: &[i64]| {
        let body: String = entries.iter().rev().map(|e| format!("{e:08x}")).collect();
        std::fs::write(&path, format!("{before}{body}{after}")).unwrap();
    };
    (entries, write)
}

/// Replace the list literal that follows `def {name} : List Nat :=`.
fn edit_nat_list(path: &Path, name: &str, edit: impl FnOnce(&mut Vec<u64>)) {
    let text = std::fs::read_to_string(path).unwrap();
    let head = format!("def {name} : List Nat :=\n  [");
    let start = text.find(&head).expect("the list is declared") + head.len();
    let end = start + text[start..].find(']').unwrap();
    let mut items: Vec<u64> = text[start..end]
        .split(',')
        .map(|x| x.trim().parse().unwrap())
        .collect();
    edit(&mut items);
    let body = items
        .iter()
        .map(u64::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    std::fs::write(path, format!("{}{body}{}", &text[..start], &text[end..])).unwrap();
}

fn declines_in_layout(cert: &Path, wasm: &Path, needle: &str) {
    let (ok, report) = aver_cert("check", wasm, cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("ArtifactLayout"), "{report}");
    assert!(report.contains(needle), "expected `{needle}`:\n{report}");
}

/// A lying slice that still tiles: one byte moved from the second code entry
/// to the first (its length one longer, the next entry one byte later and one
/// shorter). Count, offsets and lengths chain, so only the entries' own size
/// LEBs catch it: neither decodes on its declared window.
#[test]
fn cert_hardening_declines_a_lying_code_slice() {
    let Some((_dir, wasm, cert)) = baseline("certharden-codeslice") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let (mut lengths, write_lengths) = layout_table(&layout, "lengths");
    lengths[0] += 1;
    lengths[1] -= 1;
    write_lengths(&lengths);
    let (mut offsets, write_offsets) = layout_table(&layout, "offsets");
    offsets[1] += 1;
    write_offsets(&offsets);
    declines_in_layout(&cert, &wasm, "codeEntryOk");
}

/// Two code entries that overlap: a middle entry declared one byte longer,
/// reaching into the next one, which still starts where it does.
#[test]
fn cert_hardening_declines_overlapping_code_entries() {
    let Some((_dir, wasm, cert)) = baseline("certharden-codeoverlap") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let (mut lengths, write) = layout_table(&layout, "lengths");
    let mid = lengths.len() / 2;
    lengths[mid] += 1;
    write(&lengths);
    declines_in_layout(&cert, &wasm, "codeTiled");
}

/// A gap between two code entries: a middle entry declared one byte later
/// and one byte shorter, so it still ends where it did and one byte of the
/// section belongs to no entry.
#[test]
fn cert_hardening_declines_a_gap_between_code_entries() {
    let Some((_dir, wasm, cert)) = baseline("certharden-codegap") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let (mut offsets, write_offsets) = layout_table(&layout, "offsets");
    let mid = offsets.len() / 2;
    offsets[mid] += 1;
    write_offsets(&offsets);
    let (mut lengths, write_lengths) = layout_table(&layout, "lengths");
    lengths[mid] -= 1;
    write_lengths(&lengths);
    declines_in_layout(&cert, &wasm, "codeTiled");
}

/// A code section declared one function short: the section's own count
/// must equal the declared count.
#[test]
fn cert_hardening_declines_a_lying_code_count() {
    let Some((_dir, wasm, cert)) = baseline("certharden-codecount") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout).unwrap();
    let at = text.find("count := ").unwrap() + "count := ".len();
    let end = at + text[at..].find(',').unwrap();
    let count: u64 = text[at..end].parse().unwrap();
    std::fs::write(
        &layout,
        format!("{}{}{}", &text[..at], count - 1, &text[end..]),
    )
    .unwrap();
    declines_in_layout(&cert, &wasm, "codeTiled");
}

/// A false section header: one header declared one byte into its section,
/// so its id and size are read from the wrong bytes and the chain of
/// payloads breaks.
#[test]
fn cert_hardening_declines_a_false_section_header() {
    let Some((_dir, wasm, cert)) = baseline("certharden-header") else {
        return;
    };
    edit_nat_list(&cert.join("ArtifactLayout.lean"), "headers", |hs| {
        assert!(hs.len() > 2, "{hs:?}");
        hs[1] += 1;
    });
    declines_in_layout(&cert, &wasm, "framingOk");
}

/// A header declared inside a payload: an extra section between two real
/// ones, where the type section's payload is.
#[test]
fn cert_hardening_declines_a_header_inside_a_payload() {
    let Some((_dir, wasm, cert)) = baseline("certharden-innerheader") else {
        return;
    };
    edit_nat_list(&cert.join("ArtifactLayout.lean"), "headers", |hs| {
        let inner = (hs[0] + hs[1]) / 2;
        hs.insert(1, inner);
    });
    declines_in_layout(&cert, &wasm, "framingOk");
}

/// A read past the end: one more header declared at the module's length,
/// where there is no byte left to read.
#[test]
fn cert_hardening_declines_a_header_past_the_end() {
    let Some((_dir, wasm, cert)) = baseline("certharden-pastend") else {
        return;
    };
    let len = std::fs::metadata(&wasm).unwrap().len();
    edit_nat_list(&cert.join("ArtifactLayout.lean"), "headers", |hs| {
        hs.push(len)
    });
    declines_in_layout(&cert, &wasm, "framingOk");
}

/// A lying packed code entry: the first plan's lowering changed (its integer
/// literal) while its declared code entry stays. The plan still types, the
/// layout still tiles, and only the packed comparison of the lowering with
/// the entry's chunk window refuses it.
#[test]
fn cert_hardening_declines_a_lying_packed_code_entry() {
    let Some((_dir, wasm, cert)) = baseline("certharden-packed") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let at = text.find("def fn").expect("a plan");
    let body_end = at + text[at..].find("\n\n").unwrap();
    let body = &text[at..body_end];
    let lit = body.find(".int 2").expect("addTwo adds the literal 2");
    let edited = format!("{}.int 3{}", &body[..lit], &body[lit + ".int 2".len()..]);
    std::fs::write(
        &plans,
        format!("{}{edited}{}", &text[..at], &text[body_end..]),
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("planCheck"), "{report}");
}

/// Role bits that hide a helper call: the first plan calls the carrier
/// helpers, and its declared bits say it calls none. Its own declaration
/// compares the bits with its lowering.
#[test]
fn cert_hardening_declines_lying_role_bits() {
    let Some((_dir, wasm, cert)) = baseline("certharden-rolebits") else {
        return;
    };
    let mut first = 0;
    edit_nat_list(&cert.join("ArtifactLayout.lean"), "callBits", |bits| {
        assert_ne!(bits[0], 0, "{bits:?}");
        first = bits[0];
        bits[0] = 0;
    });
    // The plan's own declaration names its bits literally, before its export
    // site.
    replace_once(
        &cert.join("ArtifactPlans.lean"),
        &format!("\n    {first} ("),
        "\n    0 (",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("planCheck"), "{report}");
}

/// A wrong chunk boundary: the package reads its own chunk list with the
/// second chunk moved into the first. The list joins to the same module
/// numeral, so `join = modBytes` holds, but the first chunk no longer fits
/// 1 KiB, and every window past it would read the wrong bytes.
#[test]
fn cert_hardening_declines_a_wrong_chunk_boundary() {
    let Some((_dir, wasm, cert)) = baseline("certharden-chunks") else {
        return;
    };
    let bytes = std::fs::read(&wasm).unwrap();
    assert!(bytes.len() > 2048, "the module spans more than two chunks");
    let hex = |chunk: &[u8]| {
        let digits: String = chunk.iter().rev().map(|b| format!("{b:02x}")).collect();
        format!("0x{digits}")
    };
    let mut chunks = vec![hex(&bytes[..2048]), "0".to_string()];
    chunks.extend(bytes[2048..].chunks(1024).map(hex));
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout)
        .unwrap()
        .replace("AverCert.ArtifactBytes.chunks", "liedChunks")
        .replace(
            "(AverCert.ScaleBytes.joinTree_eq _ _ _).symm",
            "by decide +kernel",
        );
    let at = text.find("def layout : Layout").unwrap();
    let text = format!(
        "{}noncomputable def liedChunks : List Nat :=\n  [{}]\n\n{}",
        &text[..at],
        chunks.join(",\n   "),
        &text[at..]
    );
    std::fs::write(&layout, text).unwrap();
    for name in [
        "ArtifactPlans.lean",
        "ArtifactStrings.lean",
        "Artifact.lean",
    ] {
        let path = cert.join(name);
        if path.exists() {
            let text = std::fs::read_to_string(&path)
                .unwrap()
                .replace("AverCert.ArtifactBytes.chunks", "liedChunks");
            std::fs::write(&path, text).unwrap();
        }
    }
    declines_in_layout(&cert, &wasm, "chunksFit");
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
    assert!(report.contains("ClaimAxes.policiesFast"), "{report}");
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
    shift_first_cut(&cert.join("ArtifactLayout.lean"), "typeCuts_0");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("walkTypes"), "{report}");
}

// ---- the type walk and the String helper roles --------------------------------
//
// The type section is read in blocks, every type checked against three
// declarations: the String byte-array types, the shape bits (a type whose
// signature has an eq or concat helper's shape) and those types' signatures.
// The functions are classified in blocks through the shape bits. Each test
// tells one lie about them.

/// Compile a repository fixture with `--certify` into a fresh scratch
/// directory.
fn fixture_baseline(prefix: &str, fixture: &str) -> Option<(ScratchDir, PathBuf, PathBuf)> {
    if !lake_available() {
        return None;
    }
    let dir = temp_dir(prefix);
    let out = dir.join("out");
    let source = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(fixture);
    let compile = aver_command()
        .current_dir(&*dir)
        .arg("compile")
        .arg(&source)
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
    let stem = std::path::Path::new(fixture)
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap();
    let wasm = out.join(format!("{stem}.wasm"));
    let cert = out.join("cert");
    Some((dir, wasm, cert))
}

/// The hex digits of the numeral declared as `def {name} : Nat :=`, and a
/// writer for it.
fn layout_numeral(layout: &Path, name: &str) -> (String, impl Fn(&str)) {
    let text = std::fs::read_to_string(layout).unwrap();
    let head = format!("def {name} : Nat :=\n  0x");
    let start = text.find(&head).expect("the layout declares the numeral") + head.len();
    let end = start + text[start..].find('\n').unwrap();
    let digits = text[start..end].to_string();
    let (before, after) = (text[..start].to_string(), text[end..].to_string());
    let path = layout.to_path_buf();
    (digits, move |digits: &str| {
        std::fs::write(&path, format!("{before}{digits}{after}")).unwrap()
    })
}

/// A shape bit set on a type that is no function type (type 0, the tiny
/// module's first type): the type walk reads every type's shape itself.
#[test]
fn cert_hardening_declines_a_lying_shape_bit() {
    let Some((_dir, wasm, cert)) = baseline("certharden-shapebit") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let (digits, write) = layout_numeral(&layout, "shapeBits");
    let last = u8::from_str_radix(&digits[digits.len() - 1..], 16).unwrap();
    assert_eq!(last & 1, 0, "type 0 has no helper shape");
    write(&format!("{}{:x}", &digits[..digits.len() - 1], last | 1));
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("walkTypes"), "{report}");
}

/// The String eq helper's type with its shape bit cleared and its signature
/// dropped: its functions would then be classified without reading their
/// code, and the helper hidden. The type walk reads the type's shape.
#[test]
fn cert_hardening_declines_a_shape_bit_hiding_a_string_helper() {
    let Some((_dir, wasm, cert)) = fixture_baseline(
        "certharden-hiddenhelper",
        "tools/certkit/fixtures/stringeq.av",
    ) else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout).unwrap();
    let head = "def shapeSigs : List (Nat × CertDecode.StringHost.Sig) :=\n  [";
    let start = text.find(head).unwrap() + head.len();
    let end = start + text[start..].find("\n\n").unwrap();
    let sigs = text[start..end].trim_end_matches(']').to_string();
    let first: usize = sigs[1..sigs.find(',').unwrap()].parse().unwrap();
    std::fs::write(&layout, format!("{}{}", &text[..start], &text[end - 1..])).unwrap();
    let (digits, write) = layout_numeral(&layout, "shapeBits");
    let mut value: Vec<u8> = digits
        .chars()
        .rev()
        .map(|c| c.to_digit(16).unwrap() as u8)
        .collect();
    assert_ne!(
        value[first / 4] & (1 << (first % 4)),
        0,
        "type {first} has its bit"
    );
    value[first / 4] &= !(1 << (first % 4));
    let lied: String = value
        .iter()
        .rev()
        .map(|d| std::char::from_digit(u32::from(*d), 16).unwrap())
        .collect();
    write(&lied);
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("walkTypes"), "{report}");
}

/// A helper-shaped type declared with another signature (its result an
/// `i32` said to be another scalar): the walk compares every declared
/// signature with the type's own.
#[test]
fn cert_hardening_declines_a_lying_helper_signature() {
    let Some((_dir, wasm, cert)) =
        fixture_baseline("certharden-helpersig", "tools/certkit/fixtures/stringeq.av")
    else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout).unwrap();
    let head = "def shapeSigs : List (Nat × CertDecode.StringHost.Sig) :=\n  [";
    let start = text.find(head).unwrap() + head.len();
    let at = start + text[start..].find("[.i32").expect("an eq-shaped signature");
    std::fs::write(
        &layout,
        format!("{}[.scalar{}", &text[..at], &text[at + "[.i32".len()..]),
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("walkTypes"), "{report}");
}

/// A block of functions that claims no String helper where the module has
/// one: each block classifies its functions itself.
#[test]
fn cert_hardening_declines_a_string_block_dropping_its_helper() {
    let Some((_dir, wasm, cert)) = fixture_baseline(
        "certharden-stringblock",
        "tools/certkit/fixtures/stringeq.av",
    ) else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let text = std::fs::read_to_string(&artifact).unwrap();
    let at = text
        .find(", .eq)] := by")
        .expect("a block names the eq helper");
    let open = text[..at].rfind("=\n    [").unwrap() + "=\n    [".len();
    std::fs::write(
        &artifact,
        format!("{}{}", &text[..open], &text[at + ", .eq)".len()..]),
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("classifyBlock"), "{report}");
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

/// Two planned exports whose declared export entries (module offset and
/// length) are swapped: each plan reads the entry at its declared site on its
/// own window and requires it to be the export of its own name.
#[test]
fn cert_hardening_declines_a_lying_export_site() {
    let Some((_dir, wasm, cert)) = baseline("certharden-exportsite") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout).unwrap();
    let head = "def exportSites : List (Nat × Nat) :=\n  [";
    let start = text.find(head).unwrap() + head.len();
    let end = start + text[start..].find(']').unwrap();
    let sites: Vec<String> = text[start..end]
        .split("),")
        .map(|site| format!("{})", site.trim().trim_end_matches(')')))
        .collect();
    assert!(sites.len() >= 2 && sites[0] != sites[1], "{sites:?}");
    let swap = |text: &str, pre: &str, post: &str| {
        text.replacen(&format!("{pre}{}{post}", sites[0]), "@SWAP@", 1)
            .replacen(
                &format!("{pre}{}{post}", sites[1]),
                &format!("{pre}{}{post}", sites[0]),
                1,
            )
            .replacen("@SWAP@", &format!("{pre}{}{post}", sites[1]), 1)
    };
    std::fs::write(
        &layout,
        format!(
            "{}{}{}",
            &text[..start],
            swap(&text[start..end], "", ""),
            &text[end..]
        ),
    )
    .unwrap();
    // Each plan's declaration names its site literally: the lie is told there
    // too.
    let plans = cert.join("ArtifactPlans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    std::fs::write(&plans, swap(&text, " ", " = true")).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("planCheck"), "{report}");
}

/// Every export entry of a module as its name and its byte range.
fn export_entries(bytes: &[u8]) -> Vec<(String, std::ops::Range<usize>)> {
    fn uleb(bytes: &[u8], at: &mut usize) -> usize {
        let (mut value, mut shift) = (0usize, 0u32);
        loop {
            let byte = bytes[*at];
            *at += 1;
            value |= usize::from(byte & 0x7f) << shift;
            shift += 7;
            if byte < 0x80 {
                return value;
            }
        }
    }
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        if let wasmparser::Payload::ExportSection(reader) = payload.expect("parses") {
            let mut at = reader.range().start;
            let count = uleb(bytes, &mut at);
            let mut entries = Vec::with_capacity(count);
            for _ in 0..count {
                let start = at;
                let len = uleb(bytes, &mut at);
                let name = String::from_utf8(bytes[at..at + len].to_vec()).unwrap();
                at += len + 1;
                uleb(bytes, &mut at);
                entries.push((name, start..at));
            }
            return entries;
        }
    }
    panic!("the module has an export section")
}

/// The emitter sorts the export section by the wall's name key, and the walk
/// requires every name's key above the one before. Two exports of one length
/// exchanged in the section, with the plans' declared sites exchanged too:
/// every declared offset and length is then true of the tampered module, and
/// only the order is wrong.
#[test]
fn cert_hardening_declines_unsorted_exports() {
    let Some((_dir, wasm, cert)) = baseline("certharden-unsorted") else {
        return;
    };
    let mut bytes = std::fs::read(&wasm).unwrap();
    let entries = export_entries(&bytes);
    let at = |name: &str| {
        entries
            .iter()
            .position(|(n, _)| n == name)
            .unwrap_or_else(|| panic!("an export named {name}: {entries:?}"))
    };
    let (d, a) = (at("double"), at("addTwo"));
    assert_eq!(
        a,
        d + 1,
        "sorted by key, `double` comes just before `addTwo`"
    );
    let (rd, ra) = (entries[d].1.clone(), entries[a].1.clone());
    assert_eq!(rd.len(), ra.len());
    let (ed, ea) = (bytes[rd.clone()].to_vec(), bytes[ra.clone()].to_vec());
    bytes[rd.start..rd.start + ea.len()].copy_from_slice(&ea);
    bytes[ra.start..ra.start + ed.len()].copy_from_slice(&ed);
    validate(&bytes);
    restamp(&wasm, &cert, &bytes);
    let (sd, sa) = (
        format!("({}, {})", rd.start, rd.len()),
        format!("({}, {})", ra.start, ra.len()),
    );
    for file in ["ArtifactLayout.lean", "ArtifactPlans.lean"] {
        let path = cert.join(file);
        let text = std::fs::read_to_string(&path).unwrap();
        assert!(
            text.contains(&sd) && text.contains(&sa),
            "{file} names both sites"
        );
        std::fs::write(
            &path,
            text.replace(&sd, "@SWAP@")
                .replace(&sa, &sd)
                .replace("@SWAP@", &sa),
        )
        .unwrap();
    }
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert_export_walk_error(&report);
}

/// The export walk is written in blocks, each a declaration that states where
/// its entries start and where the next block starts. The one block of the
/// tiny module split in two, the boundary declared one byte into the second
/// entry: the first block's entries end one byte earlier than it states.
#[test]
fn cert_hardening_declines_an_export_block_boundary_inside_an_entry() {
    let Some((_dir, wasm, cert)) = baseline("certharden-exportblock") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout).unwrap();
    let head = "def exportCuts_0 : List Nat :=\n  [";
    let start = text.find(head).unwrap() + head.len();
    let end = start + text[start..].find(']').unwrap();
    let cuts: Vec<usize> = text[start..end]
        .split(',')
        .map(|x| x.trim().parse().unwrap())
        .collect();
    let rest = cuts[1..]
        .iter()
        .map(usize::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    let text = format!(
        "{}{}]\n\ndef exportCuts_1 : List Nat :=\n  [{rest}{}",
        &text[..start],
        cuts[0],
        &text[end..]
    );
    let text = text.replacen(
        "def exportCuts : List Nat :=\n  exportCuts_0\n",
        "def exportCuts : List Nat :=\n  exportCuts_0 ++ (exportCuts_1)\n",
        1,
    );
    let e0: usize = {
        let head = "def exportStart : Nat := ";
        let at = text.find(head).unwrap() + head.len();
        text[at..at + text[at..].find('\n').unwrap()]
            .parse()
            .unwrap()
    };
    std::fs::write(&layout, text).unwrap();
    // The first entry is the declared `size`: it is the first block's piece.
    let manifest = cert.join("Manifest.lean");
    let text = std::fs::read_to_string(&manifest).unwrap();
    let head = "def Plans.subject_declaredUncertified_0 : List (String × String) :=\n  [";
    let first = "(\"size\", \"Call Builtin(Map.len)\"),\n   ";
    assert!(text.contains(&format!("{head}{first}")), "{text}");
    let text = text
        .replacen(
            &format!("{head}{first}"),
            &format!(
                "{head}{}]\n\ndef Plans.subject_declaredUncertified_1 : List (String × String) :=\n  [",
                first.trim_end_matches(",\n   ")
            ),
            1,
        )
        .replacen(
            "declaredUncertified := Plans.subject_declaredUncertified_0\n",
            "declaredUncertified := Plans.subject_declaredUncertified_0 ++ \
             (Plans.subject_declaredUncertified_1)\n",
            1,
        );
    std::fs::write(&manifest, text).unwrap();
    let exports = cert.join("ArtifactExports.lean");
    let text = std::fs::read_to_string(&exports).unwrap();
    let head = "theorem exports_block_0 : ";
    let start = text.find(head).unwrap();
    let end =
        start + text[start..].find("decide +kernel\n\n").unwrap() + "decide +kernel\n\n".len();
    let block = &text[start..end];
    let tail = &block[block.find("=\n    some (").unwrap()..];
    let size: Vec<String> = b"size".iter().map(u8::to_string).collect();
    let prev = format!("(some [{}])", size.join(", "));
    let boundary = e0 + cuts[0] + 1;
    let blocks = format!(
        "theorem exports_block_0 : AverCert.ScaleExports.walkExports AverCert.ArtifactBytes.chunks \
         exportStart\n    exportCertified none exportStart exportCuts_0\n    \
         AverCert.Plans.subject_declaredUncertified_0 =\n    some ({prev}, {boundary}) := by\n  \
         decide +kernel\n\n\
         theorem exports_block_1 : AverCert.ScaleExports.walkExports AverCert.ArtifactBytes.chunks \
         exportStart\n    exportCertified {prev} {boundary} exportCuts_1\n    \
         AverCert.Plans.subject_declaredUncertified_1 {tail}"
    );
    let text = format!("{}{blocks}{}", &text[..start], &text[end..]);
    let text = text.replacen(
        ") :=\n  exports_block_0\n",
        ") :=\n  AverCert.ScaleExports.walk_cons exports_block_0\n    (exports_block_1)\n",
        1,
    );
    std::fs::write(&exports, text).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("subject_declaredUncertified_0"), "{report}");
}

/// The site bits (`exportCertified`) hand an entry to the plan checks, which
/// read only the planned exports' sites. A bit added for the declared `size`
/// entry would let the walk skip it: the bits are checked against the plans'
/// sites.
#[test]
fn cert_hardening_declines_site_bits_hiding_a_declared_export() {
    let Some((_dir, wasm, cert)) = baseline("certharden-sitebits") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout).unwrap();
    let head = "def exportCertified : Nat :=\n  0x";
    let start = text.find(head).unwrap() + head.len();
    let end = start + text[start..].find('\n').unwrap();
    let digits = &text[start..end];
    let last = u8::from_str_radix(&digits[digits.len() - 1..], 16).unwrap();
    assert_eq!(last & 1, 0, "the first entry, `size`, is no site");
    let lied = format!("{}{:x}", &digits[..digits.len() - 1], last | 1);
    std::fs::write(&layout, format!("{}{lied}{}", &text[..start], &text[end..])).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("certBitsOf"), "{report}");
}

/// The closure claim with one reachable helper left out. The claim is checked
/// against the fold over the declared callee lists (`closureIsolationD`),
/// each list checked against its function's code.
#[test]
fn cert_hardening_declines_a_closure_claim_hiding_a_helper() {
    let Some((_dir, wasm, cert)) = baseline("certharden-closure") else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let text = std::fs::read_to_string(&artifact).unwrap();
    assert!(text.contains("closureIsolation_of_callees"), "{text}");
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
    assert!(report.contains("closureIsolationD"), "{report}");
}

// ---- the closure's declared callee lists ---------------------------------------
//
// The package declares every function the closure fold scans, in the order it
// meets them, with its direct callees (`closureCallees`). Each list is checked
// against its function's code on its chunk window (`calleesOk`), and the fold
// runs over the lists (`closureIsolationD`), which requires every newly met
// function to be the next one declared and every declaration to be used.

/// The declared callee lists of `Artifact.lean`: the text before and after
/// the list, and its entries.
fn closure_callees(artifact: &Path) -> (String, Vec<(u32, Vec<u32>)>, String) {
    let text = std::fs::read_to_string(artifact).unwrap();
    let head = "def closureCallees_0 : List (Nat × List Nat) :=\n  [";
    let start = text.find(head).expect("the package declares callee lists") + head.len();
    let end = start + text[start..].find("]\n\n").unwrap();
    let body = &text[start..end];
    let entries = if body.is_empty() {
        Vec::new()
    } else {
        body.split(",\n   ")
            .map(|entry| {
                let entry = entry.trim_start_matches('(').trim_end_matches(')');
                let (func, callees) = entry.split_once(", [").unwrap();
                let callees = callees.trim_end_matches(']');
                (
                    func.parse().unwrap(),
                    if callees.is_empty() {
                        Vec::new()
                    } else {
                        callees.split(", ").map(|x| x.parse().unwrap()).collect()
                    },
                )
            })
            .collect()
    };
    (text[..start].to_string(), entries, text[end..].to_string())
}

fn write_closure_callees(
    artifact: &Path,
    (before, entries, after): (String, Vec<(u32, Vec<u32>)>, String),
) {
    let body = entries
        .iter()
        .map(|(func, callees)| {
            format!(
                "({func}, [{}])",
                callees
                    .iter()
                    .map(u32::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        })
        .collect::<Vec<_>>()
        .join(",\n   ");
    std::fs::write(artifact, format!("{before}{body}{after}")).unwrap();
}

/// Every defined function's index with its direct callees in code order.
fn direct_calls(bytes: &[u8]) -> Vec<(u32, Vec<u32>)> {
    let mut imports = 0u32;
    let mut out = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload.expect("parses") {
            wasmparser::Payload::ImportSection(reader) => {
                for group in reader {
                    for import in group.expect("import group") {
                        if let wasmparser::TypeRef::Func(_) = import.expect("import").1.ty {
                            imports += 1;
                        }
                    }
                }
            }
            wasmparser::Payload::CodeSectionEntry(body) => {
                let mut calls = Vec::new();
                let mut ops = body.get_operators_reader().expect("operators");
                while !ops.eof() {
                    match ops.read().expect("operator") {
                        wasmparser::Operator::Call { function_index }
                        | wasmparser::Operator::ReturnCall { function_index } => {
                            calls.push(function_index)
                        }
                        _ => {}
                    }
                }
                out.push((imports + out.len() as u32, calls));
            }
            _ => {}
        }
    }
    out
}

/// A declared callee list with one call left out. The fold over the lists
/// would miss the callee; the list's own check reads the function's code.
#[test]
fn cert_hardening_declines_a_callee_list_missing_a_call() {
    let Some((_dir, wasm, cert)) = baseline("certharden-calleemiss") else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let (before, mut entries, after) = closure_callees(&artifact);
    let entry = entries
        .iter_mut()
        .find(|(_, callees)| !callees.is_empty())
        .expect("a scanned function makes a call");
    entry.1.pop();
    write_closure_callees(&artifact, (before, entries, after));
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("calleesOk"), "{report}");
}

/// A declared callee list naming a function outside the closure as a callee:
/// the closure would grow by a function the code never calls.
#[test]
fn cert_hardening_declines_a_callee_outside_the_closure() {
    let Some((_dir, wasm, cert)) = baseline("certharden-calleeout") else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let (before, mut entries, after) = closure_callees(&artifact);
    let bytes = std::fs::read(&wasm).unwrap();
    let outside = direct_calls(&bytes)
        .into_iter()
        .map(|(func, _)| func)
        .find(|func| entries.iter().all(|(f, _)| f != func))
        .expect("a defined function outside the closure");
    entries[0].1.push(outside);
    write_closure_callees(&artifact, (before, entries, after));
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("calleesOk"), "{report}");
}

/// A true callee list declared twice, the copy after the others. Every list
/// passes its own check; the fold meets the function once and leaves the copy
/// unused.
#[test]
fn cert_hardening_declines_an_unused_callee_list() {
    let Some((_dir, wasm, cert)) = baseline("certharden-calleeextra") else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let (before, mut entries, after) = closure_callees(&artifact);
    entries.push(entries[0].clone());
    write_closure_callees(&artifact, (before, entries, after));
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("closureIsolationD"), "{report}");
    assert!(!report.contains("calleesOk"), "{report}");
}

/// The first two callee lists exchanged. Each is still its function's scan,
/// but the fold meets the functions in the other order.
#[test]
fn cert_hardening_declines_callee_lists_out_of_fold_order() {
    let Some((_dir, wasm, cert)) = baseline("certharden-calleeorder") else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let (before, mut entries, after) = closure_callees(&artifact);
    assert!(entries.len() >= 2, "the closure scans two functions");
    entries.swap(0, 1);
    write_closure_callees(&artifact, (before, entries, after));
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("closureIsolationD"), "{report}");
    assert!(!report.contains("calleesOk"), "{report}");
}

// ---- the type section by index, the data section, the helpers, the axes -------
//
// The package declares every top-level type entry and every subtype of the
// opening rec group by offset and length (`typeLayout`, `recLayout`), every
// data segment's length (`dataCuts`), the Int helpers' export sites and every
// obligation's policy answers (`axesPols`). Each test tells one lie.

/// The packed table `field` of the layout literal `def {name} : Layout`, and
/// a writer for it. Entry 0 is the lowest 32 bits.
fn packed_table(path: &Path, name: &str, field: &str) -> (Vec<u64>, impl Fn(&[u64])) {
    let text = std::fs::read_to_string(path).unwrap();
    let def = format!("def {name} : Layout :=");
    let at = text.find(&def).expect("the layout declares the table");
    let head = format!("{field} := 0x");
    let start = at + text[at..].find(&head).unwrap() + head.len();
    let end = start
        + text[start..]
            .find(|c: char| !c.is_ascii_hexdigit())
            .unwrap();
    let digits = &text[start..end];
    let body = &digits[digits.len() % 8..];
    let entries: Vec<u64> = body
        .as_bytes()
        .chunks(8)
        .rev()
        .map(|c| u64::from_str_radix(std::str::from_utf8(c).unwrap(), 16).unwrap())
        .collect();
    let (before, after) = (text[..start].to_string(), text[end..].to_string());
    let path = path.to_path_buf();
    (entries, move |entries: &[u64]| {
        let hex: String = entries.iter().rev().map(|e| format!("{e:08x}")).collect();
        std::fs::write(&path, format!("{before}0{hex}{after}")).unwrap()
    })
}

/// A top-level type entry declared one byte later than it starts. The
/// declared entries must tile the type cut.
#[test]
fn cert_hardening_declines_a_type_entry_declared_at_a_wrong_offset() {
    let Some((_dir, wasm, cert)) = baseline("certharden-typeoff") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let (mut offs, write) = packed_table(&layout, "typeLayout", "offsets");
    offs[1] += 1;
    write(&offs);
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("typesTiled"), "{report}");
}

/// A byte moved between the rec group's first two subtypes, the offsets kept
/// consistent: the subtypes still tile the group, but the first no longer
/// decodes exactly on its window.
#[test]
fn cert_hardening_declines_a_rec_subtype_boundary_moved() {
    let Some((_dir, wasm, cert)) = baseline("certharden-subtype") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let (mut lens, write_lens) = packed_table(&layout, "recLayout", "lengths");
    assert!(
        lens.len() >= 2 && lens[1] > 1,
        "the rec group has two subtypes"
    );
    lens[0] += 1;
    lens[1] -= 1;
    write_lens(&lens);
    let (mut offs, write_offs) = packed_table(&layout, "recLayout", "offsets");
    offs[1] += 1;
    write_offs(&offs);
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("subOk"), "{report}");
}

/// The rec group declared one subtype short: its last subtype would be read
/// as the section's next entry. The group's count is read from its head.
#[test]
fn cert_hardening_declines_a_rec_group_declared_short() {
    let Some((_dir, wasm, cert)) = baseline("certharden-recshort") else {
        return;
    };
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout).unwrap();
    let def = "def recLayout : Layout :=\n  { imports := 0, count := ";
    let at = text.find(def).expect("the layout declares the rec group") + def.len();
    let count: u64 = text[at..].split(',').next().unwrap().parse().unwrap();
    std::fs::write(
        &layout,
        format!(
            "{}{}{}",
            &text[..at],
            count - 1,
            &text[at + count.to_string().len()..]
        ),
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("recTiled"), "{report}");
}

/// A byte moved between the first two data segments' declared lengths:
/// neither window decodes as one segment.
#[test]
fn cert_hardening_declines_a_data_cut_moving_a_byte() {
    let Some((_dir, wasm, cert)) =
        fixture_baseline("certharden-datacut", "tools/certkit/fixtures/stringeq.av")
    else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let text = std::fs::read_to_string(&artifact).unwrap();
    let head = "def dataCuts_0 : List Nat :=\n  [";
    let start = text.find(head).expect("the package declares data cuts") + head.len();
    let end = start + text[start..].find(']').unwrap();
    let mut cuts: Vec<u64> = text[start..end]
        .split(", ")
        .map(|x| x.parse().unwrap())
        .collect();
    assert!(cuts.len() >= 2, "the module has two data segments");
    cuts[0] += 1;
    cuts[1] -= 1;
    let body = cuts
        .iter()
        .map(u64::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    std::fs::write(
        &artifact,
        format!("{}{body}{}", &text[..start], &text[end..]),
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("dataBlock"), "{report}");
}

/// A data block declared to start at segment 1 instead of 0: every declared
/// String segment would be confirmed against the segment before its own.
#[test]
fn cert_hardening_declines_a_data_block_lying_about_its_first_segment() {
    let Some((_dir, wasm, cert)) =
        fixture_baseline("certharden-datablock", "tools/certkit/fixtures/stringeq.av")
    else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let text = std::fs::read_to_string(&artifact).unwrap();
    let head = "AverCert.manifest.types.strSegs 0 dataStart dataCuts_0 = some (";
    let at = text.find(head).expect("the package reads data block 0") + head.len();
    let next: u64 = text[at..].split(',').next().unwrap().parse().unwrap();
    let lie = text.replacen(
        &format!("{head}{next},"),
        &format!(
            "AverCert.manifest.types.strSegs 1 dataStart dataCuts_0 = some ({},",
            next + 1
        ),
        1,
    );
    std::fs::write(&artifact, lie).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("dataBlock"), "{report}");
}

/// The Int box helper's export site moved one byte: the entry read there is
/// not the helper's export.
#[test]
fn cert_hardening_declines_a_helper_export_site_moved() {
    let Some((_dir, wasm, cert)) = baseline("certharden-helpersite") else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let text = std::fs::read_to_string(&artifact).unwrap();
    let head = "theorem helper_box :";
    let at = text.find(head).expect("the box helper is read at its site");
    let off_head = "(off := ";
    let start = at + text[at..].find(off_head).unwrap() + off_head.len();
    let end = start + text[start..].find(')').unwrap();
    let off: u64 = text[start..end].parse().unwrap();
    std::fs::write(
        &artifact,
        format!("{}{}{}", &text[..start], off + 1, &text[end..]),
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("exportSite"), "{report}");
}

/// An obligation declared total: the policy answers are each obligation's
/// own group check.
#[test]
fn cert_hardening_declines_a_lying_policy_answer() {
    let Some((_dir, wasm, cert)) = baseline("certharden-pols") else {
        return;
    };
    let artifact = cert.join("Artifact.lean");
    let text = std::fs::read_to_string(&artifact).unwrap();
    let head = "def axesPols_0 : List (Bool × Bool) :=\n  [(false, false)";
    assert!(text.contains(head), "the first obligation is not total");
    std::fs::write(
        &artifact,
        text.replacen(
            head,
            "def axesPols_0 : List (Bool × Bool) :=\n  [(true, false)",
            1,
        ),
    )
    .unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("polOf"), "{report}");
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
    // The role types are their own declaration (`rest_roles`), read through
    // the type section by index.
    assert!(report.contains("typeAt"), "{report}");
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
    // The package declares the callee lists of the untampered code, so
    // `strip`'s list no longer matches its scan.
    assert!(report.contains("calleesOk"), "{report}");
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

/// A program whose three exports are List matches: `[]` then a cons arm with
/// the head ignored, a cons arm first with the tail ignored, and `[]` then
/// `_`. Each shape lowers to one `ref.is_null` on the stashed subject, the
/// `[]` arm in `then`, and the head and tail binders read from the cons
/// struct in `else`. Every export takes a List, so each has a source bridge
/// only through the List decoder, and the law over `count` is on bytes only
/// through `count`'s bridge.
const LISTY: &str = "module Listy
    intent = \"List match shapes.\"
    exposes [count, firstOr, isEmpty]

fn count(xs: List<Int>) -> Int
    ? \"Number of elements.\"
    match xs
        [] -> 0
        [_, ..rest] -> 1 + count(rest)

fn firstOr(xs: List<Int>, d: Int) -> Int
    ? \"The head, or the default.\"
    match xs
        [h, .._] -> h
        [] -> d

fn isEmpty(xs: List<String>) -> Bool
    ? \"No elements.\"
    match xs
        [] -> true
        _ -> false

verify count
    count([]) => 0

verify count law neverNegative
    given xs: List<Int> = [[], [1], [1, 2]]
    count(xs) >= 0 => true
";

/// Emit the List certificate into a fresh scratch directory.
fn list_baseline(prefix: &str) -> Option<(ScratchDir, PathBuf, PathBuf)> {
    if !lake_available() {
        return None;
    }
    let dir = temp_dir(prefix);
    std::fs::write(dir.join("listy.av"), LISTY).unwrap();
    let out = dir.join("out");
    let compile = aver_command()
        .current_dir(&*dir)
        .args([
            "compile",
            "listy.av",
            "--target",
            "wasm-gc",
            "--certify",
            "-o",
        ])
        .arg(&out)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    Some((dir, out.join("listy.wasm"), out.join("cert")))
}

/// The honest List certificate checks, with all three exports certified.
#[test]
fn cert_hardening_accepts_list_matches() {
    let Some((_dir, wasm, cert)) = list_baseline("certharden-list-clean") else {
        return;
    };
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(ok, "the List certificate must check:\n{report}");
    assert!(report.contains("3 checked exports"), "{report}");
    assert!(
        report.contains("source-bridges: 3 of 3 credited"),
        "every List argument decodes:\n{report}"
    );
    assert!(
        report.contains("bridged-laws: 1 of 1 credited"),
        "the law over a List function is on bytes:\n{report}"
    );
}

/// The encoders of a bridge are the statement: the checker renders it from
/// the manifest. Declaring `count`'s argument a List of Strings states a
/// bridge about a model function that takes a List of Ints: the statement no
/// longer elaborates, and the package is refused.
#[test]
fn cert_hardening_declines_a_list_bridge_with_another_element_encoder() {
    let Some((_dir, wasm, cert)) = list_baseline("certharden-list-bridge-elem") else {
        return;
    };
    replace_once(
        &cert.join("cert-manifest.json"),
        "\"model\": \"Listy.count\", \"kind\": \"adequate\", \"params\": [{\"kind\": \"list\", \
         \"elem\": {\"kind\": \"int\"}}]",
        "\"model\": \"Listy.count\", \"kind\": \"adequate\", \"params\": [{\"kind\": \"list\", \
         \"elem\": {\"kind\": \"string\"}}]",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "the checker-owned Lean witness failed");
}

/// The arm pick is the wall's, not the plan's: a plan that gives the `[]`
/// arm the cons arm's result (`isEmpty` answering `false` for `[]`) lowers
/// to other bytes than the emitted `then` branch, so the plan is refused.
#[test]
fn cert_hardening_declines_a_list_match_with_its_arms_exchanged() {
    let Some((_dir, wasm, cert)) = list_baseline("certharden-list-arms") else {
        return;
    };
    replace_once(
        &cert.join("Plans.lean"),
        "(.cons .emptyList (.literal (.bool true)) (.cons .wild (.literal (.bool false)) .nil))",
        "(.cons .emptyList (.literal (.bool false)) (.cons .wild (.literal (.bool true)) .nil))",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The cons struct a List match casts to and reads the head and tail from
/// is the type table's, confirmed against the type section: declaring the
/// `List<String>` struct for `List<Int>` (and back) is refused.
#[test]
fn cert_hardening_declines_exchanged_list_cons_structs() {
    let Some((_dir, wasm, cert)) = list_baseline("certharden-list-structs") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let index_after = |needle: &str| -> String {
        let at = text
            .find(needle)
            .expect("the List instantiation is declared")
            + needle.len();
        text[at..]
            .chars()
            .take_while(char::is_ascii_digit)
            .collect()
    };
    let (int_idx, str_idx) = (index_after("lists := [(.int, "), index_after("(.string, "));
    replace_once(
        &plans,
        &format!("lists := [(.int, {int_idx}), (.string, {str_idx})]"),
        &format!("lists := [(.int, {str_idx}), (.string, {int_idx})]"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// A cons pattern whose head and tail slots are exchanged: the head slot
/// would hold the tail, which the typing refuses before any byte is read.
#[test]
fn cert_hardening_declines_a_cons_pattern_with_head_and_tail_exchanged() {
    let Some((_dir, wasm, cert)) = list_baseline("certharden-list-binders") else {
        return;
    };
    replace_once(
        &cert.join("Plans.lean"),
        "(.cons 65535 1)",
        "(.cons 1 65535)",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// A program building non-empty List literals: Int literals, String
/// parameters, and a user function `keep` with the cons helper's signature
/// whose plan is not the cons plan (it returns the tail).
const LITS: &str = "module Lits
    intent = \"List literals.\"
    exposes [three, pair, keep]

fn three() -> List<Int>
    ? \"Three Ints.\"
    [1, 2, 3]

fn pair(a: String, b: String) -> List<String>
    ? \"Two Strings.\"
    [a, b]

fn keep(h: Int, t: List<Int>) -> List<Int>
    ? \"The tail, unchanged.\"
    t
";

/// Emit the List-literal certificate into a fresh scratch directory.
fn literal_baseline(prefix: &str) -> Option<(ScratchDir, PathBuf, PathBuf)> {
    if !lake_available() {
        return None;
    }
    let dir = temp_dir(prefix);
    std::fs::write(dir.join("lits.av"), LITS).unwrap();
    let out = dir.join("out");
    let compile = aver_command()
        .current_dir(&*dir)
        .args([
            "compile",
            "lits.av",
            "--target",
            "wasm-gc",
            "--certify",
            "-o",
        ])
        .arg(&out)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    Some((dir, out.join("lits.wasm"), out.join("cert")))
}

/// The digits right after the first occurrence of `needle` in `text`.
fn number_after(text: &str, needle: &str) -> String {
    let at = text
        .find(needle)
        .unwrap_or_else(|| panic!("`{needle}` is in Plans.lean"))
        + needle.len();
    text[at..]
        .chars()
        .take_while(char::is_ascii_digit)
        .collect()
}

/// The honest certificate checks, with every export certified; the cons
/// helpers ride along as planned internal functions.
#[test]
fn cert_hardening_accepts_list_literals() {
    let Some((_dir, wasm, cert)) = literal_baseline("certharden-lits-clean") else {
        return;
    };
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    assert!(plans.contains("listCons := [(.int, "), "{plans}");
    assert!(plans.contains("(.string, "), "{plans}");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(ok, "the List-literal certificate must check:\n{report}");
    assert!(report.contains("3 checked exports"), "{report}");
}

/// The cons helper a literal calls is the one the type table declares, and
/// the wall requires its plan to be exactly the cons plan: pointing
/// `List<Int>`'s entry at `keep`, a user function of the same signature
/// whose plan returns the tail, is refused.
#[test]
fn cert_hardening_declines_a_cons_helper_that_is_a_user_function() {
    let Some((_dir, wasm, cert)) = literal_baseline("certharden-lits-user") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let cons = number_after(&text, "listCons := [(.int, ");
    let keep = number_after(&text, "⟨\"keep\", true, ");
    replace_once(
        &plans,
        &format!("listCons := [(.int, {cons})"),
        &format!("listCons := [(.int, {keep})"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The cons helper's own plan is pinned to the cons plan: a plan that
/// returns the tail instead of consing lowers to other bytes and is not the
/// wall's cons plan, so the package is refused.
#[test]
fn cert_hardening_declines_a_cons_helper_whose_plan_is_not_the_cons_plan() {
    let Some((_dir, wasm, cert)) = literal_baseline("certharden-lits-plan") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let cons = number_after(&text, "listCons := [(.int, ");
    let def = format!("def fn{cons} : FnPlan :=");
    let at = text.find(&def).expect("the cons helper is planned");
    let body = "body := (.call (.builtin .listPrepend) [(.local 0), (.local 1)])";
    let rel = text[at..]
        .find(body)
        .expect("the cons helper's body is the cons plan");
    let mut tampered = text.clone();
    tampered.replace_range(at + rel..at + rel + body.len(), "body := (.local 1)");
    std::fs::write(&plans, tampered).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The cons helpers of two instantiations exchanged: each literal would call
/// the other type's helper, whose planned signature is not its cons
/// signature, so the typing refuses the literals.
#[test]
fn cert_hardening_declines_exchanged_cons_helpers() {
    let Some((_dir, wasm, cert)) = literal_baseline("certharden-lits-swap") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let int_f = number_after(&text, "listCons := [(.int, ");
    let str_f = number_after(&text, &format!("listCons := [(.int, {int_f}), (.string, "));
    replace_once(
        &plans,
        &format!("listCons := [(.int, {int_f}), (.string, {str_f})]"),
        &format!("listCons := [(.int, {str_f}), (.string, {int_f})]"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The first planned entry of `Plans.lean` (`⟨"name", exported, funcIdx,
/// group, fnN⟩`) and its declaration in `ArtifactLayout.lean`
/// (`⟨[chars], exportPos, sigPos⟩`), as text.
fn first_plan_entry(cert: &Path) -> (String, String) {
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    let at = plans.find("def fnPlans : List FnEntry :=\n  [").unwrap()
        + "def fnPlans : List FnEntry :=\n  [".len();
    let entry = plans[at..at + plans[at..].find('⟩').unwrap() + '⟩'.len_utf8()].to_string();
    let layout = std::fs::read_to_string(cert.join("ArtifactLayout.lean")).unwrap();
    let at = layout.find("def fnDecls : List FnDecl :=\n  [").unwrap()
        + "def fnDecls : List FnDecl :=\n  [".len();
    let decl = layout[at..at + layout[at..].find('⟩').unwrap() + '⟩'.len_utf8()].to_string();
    (entry, decl)
}

/// Insert `item` as the last element of the list literal that follows
/// `header` in `path`.
fn append_to_list(path: &Path, header: &str, item: &str) {
    let text = std::fs::read_to_string(path).unwrap();
    let at = text.find(header).unwrap();
    let close = at + text[at..].find("⟩]").unwrap() + '⟩'.len_utf8();
    std::fs::write(
        path,
        format!("{},\n   {item}{}", &text[..close], &text[close..]),
    )
    .unwrap();
}

/// A second, internal entry for the first plan's function index, with its
/// declaration: every per-plan check passes for it (it is the same plan at
/// the same code entry), so only the distinctness of the planned indices
/// (`rest_indices`, decided on a bitmap) refuses it.
#[test]
fn cert_hardening_declines_a_duplicate_planned_index() {
    let Some((_dir, wasm, cert)) = baseline("certharden-dupidx") else {
        return;
    };
    let (entry, decl) = first_plan_entry(&cert);
    // `⟨"addTwo", true, 2, 0, fn2⟩` becomes `⟨"#2", false, 2, 0, fn2⟩`.
    let fields: Vec<&str> = entry
        .trim_start_matches('⟨')
        .trim_end_matches('⟩')
        .split(", ")
        .collect();
    assert_eq!(fields.len(), 5, "{entry}");
    let (idx, group, plan) = (fields[2], fields[3], fields[4]);
    let sig_pos = decl.trim_end_matches('⟩').rsplit(", ").next().unwrap();
    let chars = format!("#{idx}")
        .chars()
        .map(|c| format!("'{c}'"))
        .collect::<Vec<_>>()
        .join(", ");
    append_to_list(
        &cert.join("Plans.lean"),
        "def fnPlans : List FnEntry :=",
        &format!("⟨\"#{idx}\", false, {idx}, {group}, {plan}⟩"),
    );
    append_to_list(
        &cert.join("ArtifactLayout.lean"),
        "def fnDecls : List FnDecl :=",
        &format!("⟨[{chars}], 0, {sig_pos}⟩"),
    );
    // Its role bits, and its own plan declaration joined to the others.
    let mut bits = 0;
    edit_nat_list(&cert.join("ArtifactLayout.lean"), "callBits", |all| {
        bits = all[0];
        all.push(bits);
    });
    let layout = cert.join("ArtifactLayout.lean");
    let text = std::fs::read_to_string(&layout).unwrap();
    let head = "def exportSites : List (Nat × Nat) :=\n  [";
    let close = text.find(head).unwrap() + head.len();
    let close = close + text[close..].find(']').unwrap();
    std::fs::write(
        &layout,
        format!("{}, (0, 0){}", &text[..close], &text[close..]),
    )
    .unwrap();
    let plans = cert.join("ArtifactPlans.lean");
    replace_once(
        &plans,
        "theorem plans_block_0 :",
        &format!(
            "theorem plan_extra : planCheck\n    ⟨\"#{idx}\", false, {idx}, {group}, AverCert.Plans.{plan}⟩\n    \
             ⟨[{chars}], 0, {sig_pos}⟩\n    {bits} (0, 0) = true := by\n  decide +kernel\n\n\
             theorem plans_block_0 :"
        ),
    );
    replace_once(
        &plans,
        "((AverCert.ScaleLayout.plansFrom_nil _ _ _ _ _ _ _ :",
        "((AverCert.ScaleLayout.plansFrom_cons plan_extra\n      \
         (AverCert.ScaleLayout.plansFrom_nil _ _ _ _ _ _ _) :",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(report.contains("indicesDistinctBits"), "{report}");
}

/// Call groups numbered against the plans' order: the report pins compute
/// the facets, policies and termination witnesses once per run of one group
/// only when the runs' groups strictly increase, and otherwise through
/// `groupMembers`. A package that lies about the order still checks its
/// exports, with the same report. (Its bridge proofs, which the producer
/// wrote for the groups it declared, lose their credit.)
#[test]
fn cert_hardening_checks_call_groups_out_of_order_unchanged() {
    let Some((_dir, wasm, cert)) = baseline("certharden-grouporder") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let at = text.find("def fnPlans : List FnEntry :=").unwrap();
    let end = at + text[at..].find("⟩]").unwrap();
    // Two singleton groups, `0` and `1`, numbered the other way round.
    let entries = &text[at..end];
    assert!(
        entries.contains(", 0, fn") && entries.contains(", 1, fn"),
        "{entries}"
    );
    let swapped = entries
        .replacen(", 0, fn", ", @G@, fn", 1)
        .replacen(", 1, fn", ", 0, fn", 1)
        .replacen(", @G@, fn", ", 1, fn", 1);
    std::fs::write(&plans, format!("{}{swapped}{}", &text[..at], &text[end..])).unwrap();
    // Each plan's declaration names its entry literally, with its group.
    let checks = cert.join("ArtifactPlans.lean");
    let text = std::fs::read_to_string(&checks).unwrap();
    let swapped = text
        .replacen(", 0, AverCert.Plans.fn", ", @G@, AverCert.Plans.fn", 1)
        .replacen(", 1, AverCert.Plans.fn", ", 0, AverCert.Plans.fn", 1)
        .replacen(", @G@, AverCert.Plans.fn", ", 1, AverCert.Plans.fn", 1);
    std::fs::write(&checks, swapped).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(ok, "groups out of order must still check:\n{report}");
    assert!(
        report.contains("2 checked exports, level L1")
            && report.contains("law-claims: 2 of 2 credited")
            && report.contains("addTwo  policy: simulatesModel")
            && report.contains("double  policy: simulatesModel"),
        "{report}"
    );
}

/// One conjunct of the plans' acceptance, now a declaration of its own,
/// proved by `sorry`: the axiom audit of the accepted root still reaches it
/// through `plansAcceptedRestL_of_parts`.
#[test]
fn cert_hardening_declines_a_sorry_backed_plans_part() {
    let Some((_dir, wasm, cert)) = baseline("certharden-restpart") else {
        return;
    };
    replace_once(
        &cert.join("Artifact.lean"),
        "theorem rest_eqref : AverCert.TypeTable.eqrefConfined AverCert.manifest.types \
         AverCert.manifest.fnPlans = true := by\n  decide +kernel",
        "theorem rest_eqref : AverCert.TypeTable.eqrefConfined AverCert.manifest.types \
         AverCert.manifest.fnPlans = true := by\n  sorry",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "non-whitelisted axiom: sorryAx");
}

/// Every bridge proof cites `export_names_nodup`, which `BridgeNames.lean`
/// reads from the byte facts' export accounting. Proved by `sorry` instead,
/// the axiom audit reaches it through every bridge: each loses its credit,
/// and the exports stand.
#[test]
fn cert_hardening_uncredits_bridges_over_a_sorry_backed_names_fact() {
    let Some((_dir, wasm, cert)) = baseline("certharden-names-sorry") else {
        return;
    };
    replace_once(
        &cert.join("BridgeNames.lean"),
        "(AverCert.manifest.obligations.map (·.export_)).Nodup :=\n  \
         AverCert.SortedKeys.obligationNamesNodup_of_accounted AverCert.Artifact.exports_ok",
        "(AverCert.manifest.obligations.map (·.export_)).Nodup := by\n  sorry",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(
        ok,
        "a bridge proof failing only its axiom audit keeps the exports:\n{report}"
    );
    assert!(
        report.contains("2 checked exports")
            && report.contains("source-bridges: 0 of 2 credited")
            && report.contains("(proof depends on sorryAx)"),
        "{report}"
    );
}

/// The bridge theorems' modules are producer data: the checker pins each
/// theorem by name and statement, wherever it is declared. A bridge moved
/// into a slice of its own checks exactly as before.
#[test]
fn cert_hardening_checks_a_bridge_moved_to_another_slice_unchanged() {
    let Some((_dir, wasm, cert)) = baseline("certharden-bridge-moved") else {
        return;
    };
    let slice = cert.join("BridgeProof0.lean");
    let text = std::fs::read_to_string(&slice).unwrap();
    let start = text
        .find("#guard_msgs (drop error) in\n/-- plan-equals-source bridge for `addTwo`")
        .expect("the addTwo bridge is in the first slice");
    let end = start
        + text[start..]
            .find("\n  | sorry\n\n")
            .expect("the addTwo bridge ends in a sorry rung")
        + "\n  | sorry\n\n".len();
    let header = &text[..text.find("namespace AverCert.Bridge\n\n").unwrap()];
    std::fs::write(
        cert.join("BridgeProof1.lean"),
        format!(
            "{header}namespace AverCert.Bridge\n\n{}end AverCert.Bridge\n",
            &text[start..end]
        ),
    )
    .unwrap();
    std::fs::write(&slice, format!("{}{}", &text[..start], &text[end..])).unwrap();
    replace_once(
        &cert.join("Bridge.lean"),
        "import BridgeProof0\n",
        "import BridgeProof0\nimport BridgeProof1\n",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(ok, "a moved bridge must still check:\n{report}");
    assert!(
        report.contains("source-bridges: 2 of 2 credited")
            && report.contains("bridged-laws: 2 of 2 credited"),
        "{report}"
    );
}

/// Lean permits different modules to declare the same theorem with identical
/// type and universe parameters (Lean's import-time `subsumesInfo`). Bridge
/// assembly slices now contain only such theorems, with definitions factored
/// into their dependencies. Re-importing an identical slice must not mint
/// additional bridge or law credits; the manifest's pinned claims decide them.
#[test]
fn cert_hardening_credits_identical_bridge_slices_once() {
    let Some((_dir, wasm, cert)) = baseline("certharden-bridge-dup") else {
        return;
    };
    std::fs::copy(
        cert.join("BridgeProof0.lean"),
        cert.join("BridgeProof1.lean"),
    )
    .unwrap();
    replace_once(
        &cert.join("Bridge.lean"),
        "import BridgeProof0\n",
        "import BridgeProof0\nimport BridgeProof1\n",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(ok, "identical bridge theorems remain valid:\n{report}");
    assert!(
        report.contains("source-bridges: 2 of 2 credited")
            && report.contains("bridged-laws: 2 of 2 credited"),
        "identical slices must not add claims:\n{report}"
    );
}

/// A bridge slice `Bridge.lean` imports but the package does not ship: the
/// package does not build, and nothing is credited.
#[test]
fn cert_hardening_declines_a_missing_bridge_slice() {
    let Some((_dir, wasm, cert)) = baseline("certharden-bridge-missing") else {
        return;
    };
    std::fs::remove_file(cert.join("BridgeProof0.lean")).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert!(!report.contains("CERTIFIED"), "{report}");
}

/// `Bridge.lean` without the import of the slice that proves the bridges:
/// the checker's witness cannot state the bridges it pins, and the package
/// is declined.
#[test]
fn cert_hardening_declines_bridges_whose_slice_is_not_imported() {
    let Some((_dir, wasm, cert)) = baseline("certharden-bridge-unimported") else {
        return;
    };
    replace_once(&cert.join("Bridge.lean"), "import BridgeProof0\n", "");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "does not bind to this artifact");
    assert!(!report.contains("CERTIFIED"), "{report}");
}

/// The declared-uncertified names as the two places of a package state
/// them: the JSON and the manifest's `(name, reason)` list, written as the
/// one piece of the export walk's one block. `edit` rearranges the entries
/// (as indices into the original list) and both places are rewritten alike.
fn rearrange_declared(cert: &Path, edit: impl Fn(Vec<usize>) -> Vec<usize>) {
    let json_path = cert.join("cert-manifest.json");
    let json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&json_path).unwrap()).unwrap();
    let entries = json["declaredUncertified"].as_array().unwrap().clone();
    let order = edit((0..entries.len()).collect());
    let pairs: Vec<(String, String)> = entries
        .iter()
        .map(|e| {
            (
                e["name"].as_str().unwrap().to_string(),
                e["reason"].as_str().unwrap().to_string(),
            )
        })
        .collect();
    edit_json(cert, |json| {
        json["declaredUncertified"] =
            serde_json::Value::Array(order.iter().map(|&i| entries[i].clone()).collect());
    });
    let tuple = |(name, reason): &(String, String)| format!("(\"{name}\", \"{reason}\")");
    let old = pairs.iter().map(tuple).collect::<Vec<_>>().join(",\n   ");
    let new = order
        .iter()
        .map(|&i| tuple(&pairs[i]))
        .collect::<Vec<_>>()
        .join(",\n   ");
    let head = "def Plans.subject_declaredUncertified_0 : List (String × String) :=\n  ";
    replace_once(
        &cert.join("Manifest.lean"),
        &format!("{head}[{old}]"),
        &format!("{head}[{new}]"),
    );
}

/// The producer lists `declaredUncertified` in the wall's key order, the
/// order of the sorted export section, and the export walk matches every
/// entry outside the planned exports with the next declared name. The order
/// is checked: a package that lists the names in another order declines.
#[test]
fn cert_hardening_declines_declared_names_out_of_order() {
    let Some((_dir, wasm, cert)) = baseline("certharden-declared-order") else {
        return;
    };
    rearrange_declared(&cert, |mut order| {
        order.reverse();
        order
    });
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert_export_walk_error(&report);
}

/// A declared-uncertified name left out of the declared list, in both
/// places: its export is then matched with no declared name.
#[test]
fn cert_hardening_declines_a_missing_declared_name() {
    let Some((_dir, wasm, cert)) = baseline("certharden-declared-missing") else {
        return;
    };
    rearrange_declared(&cert, |mut order| {
        order.remove(1);
        order
    });
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert_export_walk_error(&report);
}

/// A declared-uncertified name whose manifest spelling is not its export's
/// bytes, in both places, one letter changed and the key order kept: the
/// walk reads the manifest name's own bytes and compares them with the
/// entry's.
#[test]
fn cert_hardening_declines_a_declared_name_that_is_not_its_export() {
    let Some((_dir, wasm, cert)) = baseline("certharden-declared-spelling") else {
        return;
    };
    let json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(cert.join("cert-manifest.json")).unwrap())
            .unwrap();
    let names: Vec<String> = json["declaredUncertified"]
        .as_array()
        .unwrap()
        .iter()
        .map(|e| e["name"].as_str().unwrap().to_string())
        .collect();
    assert_eq!(names[0], "size", "{names:?}");
    edit_json(&cert, |json| {
        json["declaredUncertified"][0]["name"] = serde_json::Value::from("sizf");
    });
    replace_once(&cert.join("Manifest.lean"), "(\"size\", ", "(\"sizf\", ");
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert_export_walk_error(&report);
}

/// A declared-uncertified name listed twice, the same in all three places:
/// the declared list, sorted or not, must be strictly increasing.
#[test]
fn cert_hardening_declines_a_duplicate_declared_name() {
    let Some((_dir, wasm, cert)) = baseline("certharden-declared-dup") else {
        return;
    };
    rearrange_declared(&cert, |mut order| {
        order.insert(1, order[0]);
        order
    });
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
    assert_export_walk_error(&report);
}

/// A program calling every List helper the plan grammar admits, over Lists
/// of Ints, Strings and Bools, plus a user function `twice` with the reverse
/// helper's signature whose body is not a reverse.
const HELPERS: &str = "module Helpers
    intent = \"List helpers.\"
    exposes [lenI, revI, catI, takeI, dropI, hasI, lenS, revS, hasS, hasB, twice]

fn lenI(xs: List<Int>) -> Int
    ? \"Length.\"
    List.len(xs)

fn revI(xs: List<Int>) -> List<Int>
    ? \"Reverse.\"
    List.reverse(xs)

fn catI(xs: List<Int>, ys: List<Int>) -> List<Int>
    ? \"Concatenation.\"
    List.concat(xs, ys)

fn takeI(xs: List<Int>, n: Int) -> List<Int>
    ? \"A prefix.\"
    List.take(xs, n)

fn dropI(xs: List<Int>, n: Int) -> List<Int>
    ? \"A suffix.\"
    List.drop(xs, n)

fn hasI(xs: List<Int>, x: Int) -> Bool
    ? \"Membership.\"
    List.contains(xs, x)

fn lenS(xs: List<String>) -> Int
    ? \"Length.\"
    List.len(xs)

fn revS(xs: List<String>) -> List<String>
    ? \"Reverse.\"
    List.reverse(xs)

fn hasS(xs: List<String>, x: String) -> Bool
    ? \"Membership.\"
    List.contains(xs, x)

fn hasB(xs: List<Bool>, x: Bool) -> Bool
    ? \"Membership.\"
    List.contains(xs, x)

fn twice(xs: List<Int>) -> List<Int>
    ? \"The list, unchanged.\"
    xs
";

/// Emit the List-helper certificate into a fresh scratch directory.
fn helpers_baseline(prefix: &str) -> Option<(ScratchDir, PathBuf, PathBuf)> {
    if !lake_available() {
        return None;
    }
    let dir = temp_dir(prefix);
    std::fs::write(dir.join("helpers.av"), HELPERS).unwrap();
    let out = dir.join("out");
    let compile = aver_command()
        .current_dir(&*dir)
        .args([
            "compile",
            "helpers.av",
            "--target",
            "wasm-gc",
            "--certify",
            "-o",
        ])
        .arg(&out)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    Some((dir, out.join("helpers.wasm"), out.join("cert")))
}

/// The honest certificate checks, with every export certified: each helper
/// is declared, pinned to its template and run by the wall.
#[test]
fn cert_hardening_accepts_list_helpers() {
    let Some((_dir, wasm, cert)) = helpers_baseline("certharden-helpers-clean") else {
        return;
    };
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    for row in [
        "(.int, .len, ",
        "(.int, .reverse, ",
        "(.int, .concat, ",
        "(.int, .take, ",
        "(.int, .drop, ",
        "(.int, .contains, ",
        "(.string, .len, ",
        "(.string, .reverse, ",
        "(.string, .contains, ",
        "(.bool, .contains, ",
        "intSat := some ",
    ] {
        assert!(plans.contains(row), "`{row}` is declared:\n{plans}");
    }
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(ok, "the List-helper certificate must check:\n{report}");
    assert!(report.contains("11 checked exports"), "{report}");
    assert!(
        report.contains("source-bridges: 11 of 11 credited"),
        "every helper call meets its source List function:\n{report}"
    );
}

/// A bridge names its source function: `revI`'s bridge claiming the plan
/// that reverses computes `twice`, a source function of the same signature
/// that never reverses. The checker renders and pins that statement, the
/// package's theorem is about `revI`, and the package is refused.
#[test]
fn cert_hardening_declines_a_helper_bridge_naming_a_source_without_the_helper() {
    let Some((_dir, wasm, cert)) = helpers_baseline("certharden-helpers-bridge-model") else {
        return;
    };
    replace_once(
        &cert.join("cert-manifest.json"),
        "\"model\": \"Helpers.revI\"",
        "\"model\": \"Helpers.twice\"",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "the checker-owned Lean witness failed");
}

/// The source model is package data: a model whose `revI` takes a prefix
/// instead of reversing leaves the plan, the bytes and every other bridge
/// alone, and costs exactly `revI`'s bridge its credit — the step proof meets
/// a source that is not the plan's helper and falls to `sorry`.
#[test]
fn cert_hardening_uncredits_a_helper_bridge_whose_source_calls_another_helper() {
    let Some((_dir, wasm, cert)) = helpers_baseline("certharden-helpers-model-helper") else {
        return;
    };
    replace_once(
        &cert.join("AverModel/Helpers.lean"),
        "def revI (xs : List Int) : List Int :=\n  xs.reverse",
        "def revI (xs : List Int) : List Int :=\n  xs.take 1",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(
        ok,
        "a wrong source model touches no export verdict:\n{report}"
    );
    assert!(
        report.contains("source-bridges: 10 of 11 credited")
            && report.contains("source-bridge not credited: revI (proof depends on sorryAx)"),
        "exactly the bridge through the wrong definition loses its credit:\n{report}"
    );
}

/// Lists of records, of records with a List field, and of Lists: each List
/// argument decodes through the generic `decList` over its element's own
/// decoder, so every export has a source bridge, and the law over a List of
/// Lists is on bytes through `sizes`' bridge.
const RECS: &str = "module Recs
    intent = \"Lists of records and Lists of Lists.\"
    exposes [total, firstName, withEntry, anyTagged, sizes]

record Entry
    name: String
    amount: Int
    tags: List<String>

fn total(es: List<Entry>) -> Int
    ? \"Sum of the amounts.\"
    match es
        [] -> 0
        [e, ..rest] -> e.amount + total(rest)

fn firstName(es: List<Entry>) -> String
    ? \"The first name, or nothing.\"
    match es
        [] -> \"\"
        [e, .._] -> e.name

fn withEntry(es: List<Entry>, n: String) -> List<Entry>
    ? \"One more entry in front.\"
    List.prepend(Entry(name = n, amount = 1, tags = []), es)

fn anyTagged(es: List<Entry>) -> Bool
    ? \"Some entry carries a tag.\"
    match es
        [] -> false
        [e, ..rest] -> match e.tags
            [] -> anyTagged(rest)
            [_, .._] -> true

fn sizes(xss: List<List<Int>>) -> Int
    ? \"The number of Ints in all the Lists.\"
    match xss
        [] -> 0
        [xs, ..rest] -> List.len(xs) + sizes(rest)

verify sizes law neverNegative
    given xss: List<List<Int>> = [[], [[1]], [[1, 2], []]]
    sizes(xss) >= 0 => true
";

/// Emit the List-of-records certificate into a fresh scratch directory.
fn recs_baseline(prefix: &str) -> Option<(ScratchDir, PathBuf, PathBuf)> {
    if !lake_available() {
        return None;
    }
    let dir = temp_dir(prefix);
    std::fs::write(dir.join("recs.av"), RECS).unwrap();
    let out = dir.join("out");
    let compile = aver_command()
        .current_dir(&*dir)
        .args([
            "compile",
            "recs.av",
            "--target",
            "wasm-gc",
            "--certify",
            "-o",
        ])
        .arg(&out)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    Some((dir, out.join("recs.wasm"), out.join("cert")))
}

#[test]
fn cert_hardening_accepts_list_of_records_bridges() {
    let Some((_dir, wasm, cert)) = recs_baseline("certharden-recs-clean") else {
        return;
    };
    let elems = std::fs::read_to_string(cert.join("BridgeElems.lean")).unwrap();
    assert!(
        elems.contains("noncomputable def decElem_0 ")
            && elems.contains("noncomputable def decElem_1 "),
        "one element decoder per element encoder:\n{elems}"
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(ok, "the List-of-records certificate must check:\n{report}");
    assert!(report.contains("5 checked exports"), "{report}");
    assert!(
        report.contains("source-bridges: 5 of 5 credited"),
        "every List of records or Lists decodes:\n{report}"
    );
    assert!(
        report.contains("bridged-laws: 1 of 1 credited"),
        "the law over a List of Lists is on bytes:\n{report}"
    );
}

/// The element decoder follows the element TYPE of the plan, through the
/// encoder the manifest transports: declaring `total`'s List of `Entry` a
/// List of Ints states a bridge about a model function that takes a List of
/// records. The checker's statement no longer elaborates, and the package is
/// refused.
#[test]
fn cert_hardening_declines_a_record_list_bridge_with_another_element_encoder() {
    let Some((_dir, wasm, cert)) = recs_baseline("certharden-recs-bridge-elem") else {
        return;
    };
    let path = cert.join("cert-manifest.json");
    let mut json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
    let bridge = json["sourceBridges"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|b| b["export"] == "total")
        .expect("`total` has a bridge");
    assert_eq!(bridge["params"][0]["elem"]["kind"], "record");
    bridge["params"][0]["elem"] = serde_json::json!({"kind": "int"});
    std::fs::write(&path, serde_json::to_string_pretty(&json).unwrap()).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "the checker-owned Lean witness failed");
}

/// A role row naming another instantiation's helper: `List<Int>`'s reverse
/// declared at `List<String>`'s. The wall synthesizes the `List<Int>`
/// template (its cons struct, its element local) and the bytes at that index
/// are not it, so the package is refused.
#[test]
fn cert_hardening_declines_a_helper_of_another_instantiation() {
    let Some((_dir, wasm, cert)) = helpers_baseline("certharden-helpers-inst") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let int_rev = number_after(&text, "(.int, .reverse, ");
    let str_rev = number_after(&text, "(.string, .reverse, ");
    replace_once(
        &plans,
        &format!("(.int, .reverse, {int_rev})"),
        &format!("(.int, .reverse, {str_rev})"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// A `len` role on the `reverse` body: the declaration says the function at
/// that index counts, the bytes there reverse. Template equality refuses it.
#[test]
fn cert_hardening_declines_a_len_role_on_the_reverse_body() {
    let Some((_dir, wasm, cert)) = helpers_baseline("certharden-helpers-role") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let len = number_after(&text, "(.int, .len, ");
    let rev = number_after(&text, "(.int, .reverse, ");
    replace_once(
        &plans,
        &format!("(.int, .len, {len})"),
        &format!("(.int, .len, {rev})"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// A helper declared at a user function: `twice` has the reverse helper's
/// signature, but it is a planned function, so it is not a helper (the
/// indices must be distinct) and its bytes are not the template.
#[test]
fn cert_hardening_declines_a_helper_that_is_a_user_function() {
    let Some((_dir, wasm, cert)) = helpers_baseline("certharden-helpers-user") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let rev = number_after(&text, "(.int, .reverse, ");
    let twice = number_after(&text, "⟨\"twice\", true, ");
    replace_once(
        &plans,
        &format!("(.int, .reverse, {rev})"),
        &format!("(.int, .reverse, {twice})"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The saturating count conversion is pinned too: pointing it at the
/// `List<Int>` length helper is refused.
#[test]
fn cert_hardening_declines_a_saturation_helper_at_another_function() {
    let Some((_dir, wasm, cert)) = helpers_baseline("certharden-helpers-sat") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let sat = number_after(&text, "intSat := some ");
    let other = number_after(&text, "(.int, .len, ");
    replace_once(
        &plans,
        &format!("intSat := some {sat}"),
        &format!("intSat := some {other}"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// `contains` over Ints calls `__aint_eq` and over Strings `String.eq`, so
/// the certificate is conditional on both contracts and the contract list
/// must say so. No plan of this module calls either helper itself; only the
/// `contains` helpers do. Both contracts are dropped from both manifests, so
/// the JSON pin agrees with the Lean one and the refusal can only come from
/// the wall's own contract derivation: the `contains` helpers' inner calls
/// (`ListHelpers.innerCalls`) put both helpers among the used calls, and the
/// package's `axes_ok`, read from the plans' role bits and the helpers' own
/// calls (`ScaleLayout.checkedBits`, which implies `ClaimAxes.checked`), no
/// longer holds.
#[test]
fn cert_hardening_declines_a_contains_without_its_equality_contract() {
    let Some((_dir, wasm, cert)) = helpers_baseline("certharden-helpers-contract") else {
        return;
    };
    let json_path = cert.join("cert-manifest.json");
    let mut json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&json_path).unwrap()).unwrap();
    let manifest = cert.join("Manifest.lean");
    for name in [
        "__aint_eq (canonical carrier pair -> i32 boolean; 1 when equal, else 0)",
        "String.eq (WVal byte-array equality; non-arrays compare false)",
    ] {
        let quoted = format!("\"{name}\"");
        let text = std::fs::read_to_string(&manifest).unwrap();
        assert!(text.contains(&quoted), "`{name}` is disclosed:\n{text}");
        let dropped = text
            .replace(&format!(", {quoted}"), "")
            .replace(&format!("{quoted}, "), "");
        assert_ne!(dropped, text);
        std::fs::write(&manifest, dropped).unwrap();
        let contracts = json["runtime_contracts"]
            .as_array_mut()
            .expect("the JSON manifest lists its contracts");
        let before = contracts.len();
        contracts.retain(|c| c.as_str() != Some(name));
        assert_eq!(contracts.len() + 1, before, "the JSON lists `{name}` once");
    }
    std::fs::write(&json_path, serde_json::to_string_pretty(&json).unwrap()).unwrap();
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(
        ok,
        &report,
        "checkedPols data callBits axesPols = true\nis false",
    );
}

/// A program calling every packed `Bytes` operation the plan grammar admits,
/// through the standard library's `Bytes` module: its functions pack
/// (`empty`), unpack (`octets`), read the length inline (`len`), and keep
/// `concat` / `take` / `drop` packed.
const BYTES: &str = "module BytesProbe
    intent = \"Bytes helpers.\"
    depends [Bytes]
    exposes [none, size, values, both, front, rest]

fn none() -> Bytes
    ? \"No bytes.\"
    Bytes.empty()

fn size(b: Bytes) -> Int
    ? \"The byte count.\"
    Bytes.len(b)

fn values(b: Bytes) -> List<Int>
    ? \"The octets.\"
    Bytes.octets(b)

fn both(a: Bytes, b: Bytes) -> Bytes
    ? \"Concatenation.\"
    Bytes.concat(a, b)

fn front(b: Bytes, n: Int) -> Bytes
    ? \"A prefix.\"
    Bytes.take(b, n)

fn rest(b: Bytes, n: Int) -> Bytes
    ? \"A suffix.\"
    Bytes.drop(b, n)
";

/// Emit the `Bytes` certificate into a fresh scratch directory.
fn bytes_baseline(prefix: &str) -> Option<(ScratchDir, PathBuf, PathBuf)> {
    if !lake_available() {
        return None;
    }
    let dir = temp_dir(prefix);
    std::fs::write(dir.join("bytesprobe.av"), BYTES).unwrap();
    let out = dir.join("out");
    let compile = aver_command()
        .current_dir(&*dir)
        .args([
            "compile",
            "bytesprobe.av",
            "--target",
            "wasm-gc",
            "--certify",
            "-o",
        ])
        .arg(&out)
        .output()
        .expect("aver compile --certify runs");
    assert!(
        compile.status.success(),
        "compile --certify failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    Some((dir, out.join("bytesprobe.wasm"), out.join("cert")))
}

/// The honest certificate checks: every `Bytes` helper is declared, pinned
/// to its template and run by the wall, and the probe's six functions, the
/// six `Bytes` functions they call and the `Bytes` module's other certified
/// functions (20 in all) check.
#[test]
fn cert_hardening_accepts_bytes_helpers() {
    let Some((_dir, wasm, cert)) = bytes_baseline("certharden-bytes-clean") else {
        return;
    };
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    for row in [
        "bytesArr := some ",
        "(.pack, ",
        "(.unpack, ",
        "(.concat, ",
        "(.take, ",
        "(.drop, ",
        "intChk := some ",
        "intSat := some ",
        "(.builtin .bytesLen)",
    ] {
        assert!(plans.contains(row), "`{row}` is declared:\n{plans}");
    }
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(ok, "the Bytes-helper certificate must check:\n{report}");
    assert!(report.contains("20 checked exports"), "{report}");
}

/// A role row naming another helper of the same type: `take` declared at the
/// `drop` helper's index. Both are `(Bytes, i64) -> Bytes`, so only the
/// template pins the role; the bytes at that index are `drop`'s.
#[test]
fn cert_hardening_declines_a_bytes_take_role_on_the_drop_body() {
    let Some((_dir, wasm, cert)) = bytes_baseline("certharden-bytes-role") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let take = number_after(&text, "(.take, ");
    let drop = number_after(&text, "(.drop, ");
    replace_once(
        &plans,
        &format!("(.take, {take})"),
        &format!("(.take, {drop})"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// A helper of another type: `unpack` (`Bytes -> List<Int>`) declared at the
/// `pack` helper (`List<Int> -> Bytes`). A plan reading `bytes.values` then
/// calls a function of the wrong type; the type pin and the template refuse
/// it.
#[test]
fn cert_hardening_declines_a_bytes_helper_of_another_type() {
    let Some((_dir, wasm, cert)) = bytes_baseline("certharden-bytes-type") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let unpack = number_after(&text, "(.unpack, ");
    let pack = number_after(&text, "(.pack, ");
    replace_once(
        &plans,
        &format!("(.unpack, {unpack})"),
        &format!("(.unpack, {pack})"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The checked conversion `pack` calls is pinned too: pointing it at the
/// `unpack` helper is refused.
#[test]
fn cert_hardening_declines_a_checked_conversion_at_another_function() {
    let Some((_dir, wasm, cert)) = bytes_baseline("certharden-bytes-chk") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let chk = number_after(&text, "intChk := some ");
    let other = number_after(&text, "(.unpack, ");
    replace_once(
        &plans,
        &format!("intChk := some {chk}"),
        &format!("intChk := some {other}"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The packed array declared at the `$string` array: both are
/// `(array (mut i8))`, but one index cannot serve two declarations.
#[test]
fn cert_hardening_declines_bytes_declared_at_the_string_array() {
    let Some((_dir, wasm, cert)) = bytes_baseline("certharden-bytes-str") else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let arr = number_after(&text, "bytesArr := some ");
    let Some(at) = text.find("str := some ") else {
        // A module without strings has no `$string` array to collide with.
        return;
    };
    let str_ = number_after(&text[at..], "str := some ");
    replace_once(
        &plans,
        &format!("bytesArr := some {arr}"),
        &format!("bytesArr := some {str_}"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// A tampered helper body: the `take` helper's count test `i64.const 0`
/// becomes `i64.const 1`, so a count of one would take nothing. The module
/// stays valid and is restamped; only the template pin of the declared
/// `take` helper sees the change.
#[test]
fn cert_hardening_declines_a_tampered_bytes_helper_body() {
    let Some((_dir, wasm, cert)) = bytes_baseline("certharden-bytes-body") else {
        return;
    };
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    let take: u32 = number_after(&plans, "(.take, ").parse().unwrap();
    let mut bytes = std::fs::read(&wasm).unwrap();
    let (body, _) = code_body_and_imports(&bytes, take);
    assert!(!body.is_empty(), "the take helper is a defined function");
    // `local.get 1; i64.const 0; i64.gt_s`: the clamp's positive-count test.
    let test = [0x20, 0x01, 0x42, 0x00, 0x55];
    let at = bytes[body.clone()]
        .windows(test.len())
        .position(|w| w == test)
        .expect("the take helper tests its count")
        + body.start;
    bytes[at + 3] = 0x01;
    validate(&bytes);
    restamp(&wasm, &cert, &bytes);
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// A program whose functions return early through `?`: in an argument
/// (`bumped`) and in a binding and an argument (`twice`).
const TRY: &str = "module TryProbe
    intent = \"Early returns.\"
    exposes [half, bumped, twice]

fn half(n: Int) -> Result<Int, String>
    ? \"The number itself, if it is not negative.\"
    match n >= 0
        true -> Result.Ok(n)
        false -> Result.Err(\"negative\")

fn bumped(n: Int) -> Result<Int, String>
    ? \"One more than half, through an early return in an argument.\"
    Result.Ok(half(n)? + 1)

fn twice(n: Int) -> Result<Int, String>
    ? \"Two early returns: a binding and an argument.\"
    a = half(n)?
    Result.Ok(a + half(a - 1)?)
";

/// The honest certificate checks: both early-returning bodies are planned
/// under a `scope`, and the three functions check.
#[test]
fn cert_hardening_accepts_early_returns() {
    let Some((_dir, wasm, cert)) = baseline_source("certharden-try-clean", TRY) else {
        return;
    };
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    for row in ["(.scope ", "(.try_ (.call (.fn "] {
        assert!(plans.contains(row), "`{row}` is planned:\n{plans}");
    }
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(ok, "the early-return certificate must check:\n{report}");
    assert!(report.contains("3 checked exports"), "{report}");
}

/// The bytes of `bumped`'s `?` and where its `Err` branch starts: the first
/// `else; i32.const 0` of its body, the tag of the `Err` it returns.
fn try_err_branch(bytes: &[u8], cert: &Path) -> (std::ops::Range<usize>, usize) {
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    let bumped: u32 = number_after(&plans, "⟨\"bumped\", true, ").parse().unwrap();
    let (body, _) = code_body_and_imports(bytes, bumped);
    assert!(!body.is_empty(), "bumped is a defined function");
    let at = bytes[body.clone()]
        .windows(3)
        .position(|w| w == [0x05, 0x41, 0x00])
        .expect("the `?` rebuilds an `Err` in its `else`")
        + body.start;
    (body, at)
}

/// The `?` returns an `Ok` instead: the tag of the value it returns early
/// becomes `1`. The module stays valid and is restamped; the plan's lowering
/// no longer matches the code entry.
#[test]
fn cert_hardening_declines_an_early_return_of_the_other_variant() {
    let Some((_dir, wasm, cert)) = baseline_source("certharden-try-variant", TRY) else {
        return;
    };
    let mut bytes = std::fs::read(&wasm).unwrap();
    let (_, at) = try_err_branch(&bytes, &cert);
    bytes[at + 2] = 0x01;
    validate(&bytes);
    restamp(&wasm, &cert, &bytes);
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The `?` does not return: its `return` becomes `unreachable`, so an `Err`
/// traps instead of leaving the function. The plan still says `try_`.
#[test]
fn cert_hardening_declines_an_early_return_that_does_not_return() {
    let Some((_dir, wasm, cert)) = baseline_source("certharden-try-noreturn", TRY) else {
        return;
    };
    let mut bytes = std::fs::read(&wasm).unwrap();
    let (body, at) = try_err_branch(&bytes, &cert);
    let ret = bytes[at..body.end]
        .windows(2)
        .position(|w| w == [0x0f, 0x0b])
        .expect("the `Err` branch ends with `return`")
        + at;
    bytes[ret] = 0x00;
    validate(&bytes);
    restamp(&wasm, &cert, &bytes);
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// A plan whose `?` names another return type: the `Err` it says it returns
/// is a `Result<Bool, String>`, which no code entry builds.
#[test]
fn cert_hardening_declines_an_early_return_at_another_type() {
    let Some((_dir, wasm, cert)) = baseline_source("certharden-try-type", TRY) else {
        return;
    };
    replace_once(
        &cert.join("Plans.lean"),
        "[(.local 0)]) (.result .int .string))",
        "[(.local 0)]) (.result .bool .string))",
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// A program interpolating Ints: alone, and among String parts.
const INTERP: &str = "module InterpProbe
    intent = \"Int interpolation.\"
    exposes [show, label]

fn show(n: Int) -> String
    ? \"The number in decimal.\"
    \"n = {n}\"

fn label(n: Int, s: String) -> String
    ? \"Mixed parts.\"
    \"{s}: {n + 1}!\"
";

/// The honest certificate checks: each Int part calls the declared
/// `String.fromInt`, pinned to its template.
#[test]
fn cert_hardening_accepts_int_interpolation() {
    let Some((_dir, wasm, cert)) = baseline_source("certharden-interp-clean", INTERP) else {
        return;
    };
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    for row in ["strFromInt := some ", "(.call (.builtin .strFromInt) "] {
        assert!(plans.contains(row), "`{row}` is planned:\n{plans}");
    }
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert!(
        ok,
        "the Int-interpolation certificate must check:\n{report}"
    );
    assert!(report.contains("2 checked exports"), "{report}");
}

/// `String.fromInt` declared at a planned function's index: one index
/// cannot be both the pinned helper and a plan.
#[test]
fn cert_hardening_declines_the_int_formatter_at_a_planned_function() {
    let Some((_dir, wasm, cert)) = baseline_source("certharden-interp-index", INTERP) else {
        return;
    };
    let plans = cert.join("Plans.lean");
    let text = std::fs::read_to_string(&plans).unwrap();
    let from_int = number_after(&text, "strFromInt := some ");
    let show = number_after(&text, "⟨\"show\", true, ");
    replace_once(
        &plans,
        &format!("strFromInt := some {from_int}"),
        &format!("strFromInt := some {show}"),
    );
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The bytes of the declared `String.fromInt` and the position of `needle`
/// in them.
fn from_int_site(bytes: &[u8], cert: &Path, needle: &[u8]) -> usize {
    let plans = std::fs::read_to_string(cert.join("Plans.lean")).unwrap();
    let from_int: u32 = number_after(&plans, "strFromInt := some ").parse().unwrap();
    let (body, _) = code_body_and_imports(bytes, from_int);
    assert!(!body.is_empty(), "String.fromInt is a defined function");
    bytes[body.clone()]
        .windows(needle.len())
        .position(|w| w == needle)
        .expect("the helper has the instruction")
        + body.start
}

/// A tampered Small branch: the fill loop writes each digit from `1`
/// instead of `0`. The module stays valid and is restamped; the template pin
/// sees the change.
#[test]
fn cert_hardening_declines_a_tampered_int_formatter() {
    let Some((_dir, wasm, cert)) = baseline_source("certharden-interp-body", INTERP) else {
        return;
    };
    let mut bytes = std::fs::read(&wasm).unwrap();
    // `local.get 6; i32.const 48; local.get 3; i64.const 10; i64.rem_u`.
    let at = from_int_site(
        &bytes,
        &cert,
        &[0x20, 0x06, 0x41, 0x30, 0x20, 0x03, 0x42, 0x0a, 0x82],
    );
    bytes[at + 3] = 0x31;
    validate(&bytes);
    restamp(&wasm, &cert, &bytes);
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}

/// The Big branch is pinned though the wall does not run it: its digit
/// buffer's size `len * 10` becomes `len + 10`.
#[test]
fn cert_hardening_declines_a_tampered_big_int_branch() {
    let Some((_dir, wasm, cert)) = baseline_source("certharden-interp-big", INTERP) else {
        return;
    };
    let mut bytes = std::fs::read(&wasm).unwrap();
    // `local.get 10; i32.const 10; i32.mul`.
    let at = from_int_site(&bytes, &cert, &[0x20, 0x0a, 0x41, 0x0a, 0x6c]);
    bytes[at + 4] = 0x6a;
    validate(&bytes);
    restamp(&wasm, &cert, &bytes);
    let (ok, report) = aver_cert("check", &wasm, &cert);
    assert_declined(ok, &report, "did not build");
}
