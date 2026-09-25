//! Carrier on/off agreement on the wasm-gc backend.
//!
//! Every user function is emitted by the MIR body emitter
//! (`MirEmitterIsTheOnlyEmitter` in `decisions/architecture.av`). Until
//! 2026-09-22, plan-shaped functions in a module that declares the Int carrier
//! type were instead lowered through the certification plan lowerer; this file
//! was the gate that the two emitters agreed. It now pins that the same
//! float/bool functions give the same results whether or not the surrounding
//! module turns the Int carrier on.
//!
//! This test runs the SAME `floatLeGoal` / `boolAndGoal` source
//!   (A) in a module WITH unrelated Int usage -> carrier declared, and
//!   (B) in a module WITHOUT any Int          -> no carrier,
//! on the wasm-gc backend and asserts identical results.
//!
//! `floatLeGoal` (a `<=` comparison) and `boolAndGoal` (a pure `match`) are
//! used precisely because neither contains an `+`/`-`/`*`/`/` operator or an
//! Int literal, so neither one FORCES the carrier on by itself (unlike e.g.
//! `a + b`). Variant (A) turns the carrier on via a separate, unrelated
//! `unrelatedInt(x: Int)` helper; variant (B) has no Int anywhere.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::cleanup;

use std::path::PathBuf;
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

fn letBoolGoal(a: Bool, b: Bool) -> Bool
    c = a
    match c
        true -> b
        false -> false

fn floatPickGoal(a: Float, b: Float, c: Float) -> Bool
    match a <= b
        true -> b <= c
        false -> c <= a

fn boolAndPrimGoal(a: Bool, b: Bool) -> Bool
    Bool.and(a, b)

fn report() -> String
    "le={floatLeGoal(1.0, 2.0)}/{floatLeGoal(3.0, 2.0)} and={boolAndGoal(true, true)}/{boolAndGoal(true, false)} let={letBoolGoal(true, true)}/{letBoolGoal(false, true)} pick={floatPickGoal(1.0, 2.0, 3.0)}/{floatPickGoal(3.0, 2.0, 4.0)} prim={boolAndPrimGoal(true, true)}/{boolAndPrimGoal(true, false)}"
"#;

/// The report line both variants print. Bool-valued throughout, so the text is
/// format-stable (no float-rendering ambiguity to reason about).
const EXPECTED_REPORT: &str =
    "le=true/false and=true/false let=true/false pick=true/false prim=true/false";

/// Variant (A): the shared float/bool functions in a module that ALSO declares
/// Int (via `unrelatedInt`), which flips the `$AverInt` carrier on. `main` calls
/// `unrelatedInt` so the Int usage is reachable, and prints it on a second line
/// (ignored by the run comparison, which only reads line 1).
fn variant_a_source() -> String {
    format!(
        r#"module PlanCarrierOn
    intent = "float/bool functions in a module that also declares Int"
    exposes [floatLeGoal, boolAndGoal, letBoolGoal, floatPickGoal, boolAndPrimGoal]
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
/// so the carrier stays off.
fn variant_b_source() -> String {
    format!(
        r#"module PlanCarrierOff
    intent = "float/bool functions in a module with no Int at all"
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

/// The core equivalence: the SAME float/bool functions produce the SAME
/// observable result whether the module declares Int or not.
#[test]
fn carrier_on_and_off_agree_on_float_bool_functions() {
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
        "carrier-on (A) and carrier-off (B) modules diverged for the same \
         float/bool functions:\n  A = {a_report:?}\n  B = {b_report:?}"
    );
    assert_eq!(
        b_report, EXPECTED_REPORT,
        "report changed unexpectedly: {b_report:?}"
    );
    // Sanity-check that variant A really did the reachable Int work (the thing
    // that turns the carrier on), so the A==B agreement really compares a
    // carrier-on module with a carrier-off one.
    assert_eq!(
        a_out.lines().nth(1),
        Some("int=42"),
        "variant A should also print its unrelated Int result:\n{a_out}"
    );
}
