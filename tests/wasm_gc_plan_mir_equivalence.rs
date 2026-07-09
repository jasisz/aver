//! Plan-emitter vs MIR-emitter equivalence gate for the
//! `PlanEmittedCanonicalCodegen` decision (unconditional canonical plan
//! emission).
//!
//! A plan-shaped pure float/bool function lowers through the canonical
//! certification plan lowerer (`lower_expr_fragment_plan_function`) in any
//! module that declares the Int carrier type (`registry.aint_struct_idx` is
//! `Some`), and through the ordinary MIR body emitter in a module with no Int
//! at all. Canonical plan emission is UNCONDITIONAL — it does not wait for
//! `--certify` — because the certificate must describe the exact bytes users
//! ship. The two emitters MUST therefore be observationally equal.
//!
//! This test runs the SAME `floatLeGoal` / `boolAndGoal` source
//!   (A) in a module WITH unrelated Int usage -> carrier declared -> plan-emitted body, and
//!   (B) in a module WITHOUT any Int          -> no carrier       -> MIR-emitted body,
//! on the wasm-gc backend and asserts identical results.
//!
//! `floatLeGoal` (a `<=` comparison) and `boolAndGoal` (a pure `match`) are the
//! plan-shaped functions under comparison precisely because neither contains an
//! `+`/`-`/`*`/`/` operator or an Int literal, so neither one FORCES the carrier
//! on by itself (unlike e.g. `a + b`). Variant (A) turns the carrier on via a
//! separate, unrelated `unrelatedInt(x: Int)` helper; variant (B) has no Int
//! anywhere, so the very same functions go down the MIR path.
//!
//! It also asserts, via a `--certify` manifest, that variant (A)'s functions
//! actually took the plan path (class `expr-fragment-v1`). If canonical plan
//! emission silently disengaged and (A) quietly fell back to the MIR body, the
//! classifier would stop reporting `expr-fragment-v1` and this test would fail
//! loudly rather than letting the A==B check pass vacuously.

#![cfg(feature = "wasm")]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

/// The float/bool functions under test plus the reporting harness. Byte-for-byte
/// identical across both variants — only the surrounding module (Int present vs
/// absent) differs, which is the single variable this test isolates.
const SHARED_BODY: &str = r#"
fn floatLeGoal(a: Float, b: Float) -> Bool
    match a <= b
        true -> true
        false -> false

fn boolAndGoal(a: Bool, b: Bool) -> Bool
    match a
        true -> b
        false -> false

fn report() -> String
    "le={floatLeGoal(1.0, 2.0)}/{floatLeGoal(3.0, 2.0)} and={boolAndGoal(true, true)}/{boolAndGoal(true, false)}"
"#;

/// The report line both variants print. Bool-valued throughout, so the text is
/// format-stable (no float-rendering ambiguity to reason about).
const EXPECTED_REPORT: &str = "le=true/false and=true/false";

/// Variant (A): the shared float/bool functions in a module that ALSO declares
/// Int (via `unrelatedInt`), which flips the `$AverInt` carrier on and routes
/// `floatLeGoal`/`boolAndGoal` through the canonical plan lowerer. `main` calls
/// `unrelatedInt` so the Int usage is reachable, and prints it on a second line
/// (ignored by the run comparison, which only reads line 1).
fn variant_a_source() -> String {
    format!(
        r#"module PlanCarrierOn
    intent = "plan-emitted float/bool functions in a module that also declares Int"
    exposes [floatLeGoal, boolAndGoal]
    effects [Console]
{SHARED_BODY}
fn unrelatedInt(x: Int) -> Int
    x + 1

fn main() -> Unit
    ! [Console.print]
    Console.print(report())
    Console.print("int={{unrelatedInt(41)}}")
"#
    )
}

/// Variant (B): the same float/bool functions in a module with NO Int anywhere,
/// so the carrier stays off and the functions go down the MIR body emitter.
fn variant_b_source() -> String {
    format!(
        r#"module PlanCarrierOff
    intent = "MIR-emitted float/bool functions in a module with no Int at all"
    effects [Console]
{SHARED_BODY}
fn main() -> Unit
    ! [Console.print]
    Console.print(report())
"#
    )
}

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver-{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    std::fs::write(&path, source).expect("write temp module source");
    path
}

fn cleanup(path: &Path) {
    let _ = std::fs::remove_dir_all(path.parent().expect("temp module has parent"));
}

/// Run `source` on the wasm-gc backend (`aver run --wasm-gc`) and return
/// `(success, trimmed stdout)`.
fn run_wasm_gc(prefix: &str, source: &str) -> (bool, String) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&path)
        .arg("--wasm-gc")
        .output()
        .expect("aver run --wasm-gc executes");
    cleanup(&path);
    (
        out.status.success(),
        String::from_utf8_lossy(&out.stdout).trim().to_string(),
    )
}

/// `aver compile <source> --target wasm-gc --certify -o <out>` and return the
/// parsed `cert/cert-manifest.json`.
fn certify_manifest(prefix: &str, source: &str) -> serde_json::Value {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let out_dir = path.parent().expect("temp module has parent").join("out");
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--certify")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile --certify executes");
    assert!(
        out.status.success(),
        "{prefix}: compile --certify failed:\n{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
    let manifest_text = std::fs::read_to_string(out_dir.join("cert").join("cert-manifest.json"))
        .expect("cert-manifest.json exists");
    let manifest: serde_json::Value =
        serde_json::from_str(&manifest_text).expect("manifest is valid JSON");
    cleanup(&path);
    manifest
}

/// The certified class the manifest records for `name`, if any.
fn certified_class<'a>(manifest: &'a serde_json::Value, name: &str) -> Option<&'a str> {
    manifest["certified"]
        .as_array()
        .expect("manifest has a certified array")
        .iter()
        .find(|c| c["name"].as_str() == Some(name))
        .and_then(|c| c["class"].as_str())
}

/// The core equivalence: the SAME plan-shaped float/bool functions produce the
/// SAME observable result whether the module declares Int (plan-emitted body)
/// or not (MIR-emitted body).
#[test]
fn plan_and_mir_emitters_agree_on_float_bool_functions() {
    let (a_ok, a_out) = run_wasm_gc("plan-carrier-on", &variant_a_source());
    let (b_ok, b_out) = run_wasm_gc("plan-carrier-off", &variant_b_source());
    assert!(a_ok, "variant A (carrier on) run failed:\n{a_out}");
    assert!(b_ok, "variant B (carrier off) run failed:\n{b_out}");

    // Variant A prints the shared report on line 1 and an unrelated Int result
    // on line 2; variant B prints only the report. Compare the report lines.
    let a_report = a_out.lines().next().unwrap_or_default();
    let b_report = b_out.trim();

    assert_eq!(
        a_report, b_report,
        "plan-emitted (A) and MIR-emitted (B) bodies diverged for the same \
         float/bool functions:\n  A(plan) = {a_report:?}\n  B(mir)  = {b_report:?}"
    );
    assert_eq!(
        b_report, EXPECTED_REPORT,
        "MIR-emitted report changed unexpectedly: {b_report:?}"
    );
    // Sanity-check that variant A really did the reachable Int work (the thing
    // that turns the carrier on), so the A==B agreement is between a genuine
    // plan-emitted body and a genuine MIR-emitted body.
    assert_eq!(
        a_out.lines().nth(1),
        Some("int=42"),
        "variant A should also print its unrelated Int result:\n{a_out}"
    );
}

/// Guard against silent disengagement: variant (A)'s functions must actually
/// take the canonical plan path, which the `--certify` manifest records as
/// class `expr-fragment-v1`. If plan emission regressed to a MIR fallback for a
/// carrier-declaring module, the classifier would stop reporting this class and
/// the assertion below would fail — so the equivalence test above cannot pass
/// vacuously with both variants secretly on the MIR path.
#[test]
fn variant_a_functions_take_the_plan_path() {
    let manifest = certify_manifest("plan-carrier-certify", &variant_a_source());
    for name in ["floatLeGoal", "boolAndGoal"] {
        assert_eq!(
            certified_class(&manifest, name),
            Some("expr-fragment-v1"),
            "{name} should be certified via the canonical plan path (expr-fragment-v1); \
             a different or missing class means plan emission silently disengaged for a \
             carrier-declaring module. manifest:\n{manifest:#}"
        );
    }
}
