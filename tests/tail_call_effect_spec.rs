//! A call in tail position carries the same effect obligation as any other call.
//!
//! Before the type checker runs, a call to a peer of the caller's recursion group is
//! rewritten into `Expr::TailCall`. The effect walk used to descend into that node's
//! arguments and stop there, so the target's declared effects were never charged to the
//! caller. Loops in Aver are tail calls, so that left a large share of every program's
//! call edges unchecked while the same call one line earlier was rejected.
//!
//! Every recursion group shares one effect list. A tail call runs in both directions
//! around the cycle, so each member of the group must declare what the group as a whole
//! performs. That is the rule ordinary calls have always followed.

use aver::diagnostics::{AnalyzeOptions, analyze_source};
use std::time::{SystemTime, UNIX_EPOCH};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn diagnostics(label: &str, source: &str, base_dir: Option<&str>) -> Vec<(bool, String, String)> {
    let mut options = AnalyzeOptions::new(label.to_string());
    if let Some(dir) = base_dir {
        options = options.with_module_base_dir(dir.to_string());
    }
    analyze_source(source, &options)
        .diagnostics
        .into_iter()
        .map(|diagnostic| {
            (
                diagnostic.is_error(),
                diagnostic.slug.to_string(),
                diagnostic.summary.clone(),
            )
        })
        .collect()
}

fn errors_for(label: &str, source: &str) -> Vec<String> {
    diagnostics(label, source, None)
        .into_iter()
        .filter(|(is_error, _, _)| *is_error)
        .map(|(_, _, summary)| summary)
        .collect()
}

fn errors_for_in(label: &str, source: &str, base_dir: &str) -> Vec<String> {
    diagnostics(label, source, Some(base_dir))
        .into_iter()
        .filter(|(is_error, _, _)| *is_error)
        .map(|(_, _, summary)| summary)
        .collect()
}

fn warnings_for(label: &str, source: &str) -> Vec<(String, String)> {
    diagnostics(label, source, None)
        .into_iter()
        .filter(|(is_error, _, _)| !*is_error)
        .map(|(_, slug, summary)| (slug, summary))
        .collect()
}

/// The missing-effect errors only, so an unrelated diagnostic cannot make a test pass.
fn missing_effect_errors(label: &str, source: &str) -> Vec<String> {
    errors_for(label, source)
        .into_iter()
        .filter(|message| message.contains("does not declare it"))
        .collect()
}

fn assert_reports(actual: &[String], expected: &[&str], label: &str) {
    for wanted in expected {
        assert!(
            actual.iter().any(|message| message == wanted),
            "{label}: expected the report\n  {wanted}\ngot:\n  {}",
            if actual.is_empty() {
                "<nothing>".to_string()
            } else {
                actual.join("\n  ")
            }
        );
    }
    assert_eq!(
        actual.len(),
        expected.len(),
        "{label}: expected exactly {} report(s), got:\n  {}",
        expected.len(),
        if actual.is_empty() {
            "<nothing>".to_string()
        } else {
            actual.join("\n  ")
        }
    );
}

fn temp_module_root(tag: &str) -> std::path::PathBuf {
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock went backwards")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver_tail_call_effect_{}_{}", tag, stamp));
    std::fs::create_dir_all(&dir).expect("create temp module dir failed");
    dir
}

// ---------------------------------------------------------------------------
// The defect
// ---------------------------------------------------------------------------

const MUTUAL_TAIL: &str = r#"module TailHole
    intent =
        "A tail call into a function with more effects than the caller declares."
    exposes [loopA, loopB]
    effects [Console.print, Time.unixMs]

fn loopA(n: Int) -> Result<Unit, String>
    ? "Reads the clock, then hands over to loopB."
    ! [Console.print, Time.unixMs]
    _now = Time.unixMs()
    match n < 1
        true -> Result.Ok(Unit)
        false -> loopB(n - 1)

fn loopB(n: Int) -> Result<Unit, String>
    ? "Declares only Console.print, yet tail-calls loopA, which reads the clock."
    ! [Console.print]
    Console.print("step")
    loopA(n)
"#;

const NON_TAIL: &str = r#"module NonTail
    intent =
        "The same call, not in tail position."
    exposes [loopA, loopB]
    effects [Console.print, Time.unixMs]

fn loopA(n: Int) -> Result<Unit, String>
    ? "Reads the clock."
    ! [Console.print, Time.unixMs]
    _now = Time.unixMs()
    Result.Ok(Unit)

fn loopB(n: Int) -> Result<Unit, String>
    ? "Declares only Console.print, calls loopA in the middle."
    ! [Console.print]
    _done = loopA(n)?
    Console.print("step")
    Result.Ok(Unit)
"#;

#[test]
fn a_tail_call_charges_the_targets_effects() {
    assert_reports(
        &missing_effect_errors("tailhole.av", MUTUAL_TAIL),
        &[
            "Function 'loopB' calls 'loopA' which has effect 'Time.unixMs', but 'loopB' does not declare it",
        ],
        "tail call into a wider effect list",
    );
}

#[test]
fn the_same_call_outside_tail_position_charges_the_same_effect() {
    assert_reports(
        &missing_effect_errors("nontail.av", NON_TAIL),
        &[
            "Function 'loopB' calls 'loopA' which has effect 'Time.unixMs', but 'loopB' does not declare it",
        ],
        "ordinary call into a wider effect list",
    );
}

#[test]
fn declaring_the_targets_effect_settles_the_tail_call() {
    let source = MUTUAL_TAIL.replace(
        "    ! [Console.print]\n    Console.print(\"step\")",
        "    ! [Console.print, Time.unixMs]\n    Console.print(\"step\")",
    );
    assert_reports(
        &errors_for("tailhole_fixed.av", &source),
        &[],
        "both members of the group declare the group's effects",
    );
}

// ---------------------------------------------------------------------------
// Shapes the fix must not disturb
// ---------------------------------------------------------------------------

#[test]
fn a_self_tail_call_charges_nothing_new() {
    // A function's own effects always satisfy a call to itself. Charging a self tail call
    // must not make every loop in the language report against its own declaration.
    let source = r#"module SelfLoop
    intent =
        "A self tail call must not invent an effect obligation."
    exposes [countdown]
    effects [Console.print]

fn countdown(n: Int) -> Unit
    ? "Prints down to zero."
    ! [Console.print]
    Console.print("step")
    match n < 1
        true -> Unit
        false -> countdown(n - 1)
"#;
    assert_reports(&errors_for("selfloop.av", source), &[], "self tail call");
}

#[test]
fn an_effect_reached_only_through_a_tail_call_is_not_reported_unused() {
    // The unused-effect lint already counts a tail call target's effects. The checker now
    // agrees with it, so a declaration the checker requires must not be called dead.
    let source = MUTUAL_TAIL.replace(
        "    ! [Console.print]\n    Console.print(\"step\")",
        "    ! [Console.print, Time.unixMs]\n    Console.print(\"step\")",
    );
    let unused: Vec<String> = warnings_for("tailhole_fixed.av", &source)
        .into_iter()
        .filter(|(slug, _)| slug == "unused-effect")
        .map(|(_, summary)| summary)
        .collect();
    assert!(
        unused.is_empty(),
        "expected no unused-effect warning, got:\n  {}",
        unused.join("\n  ")
    );
}

// ---------------------------------------------------------------------------
// Where a tail call can sit
// ---------------------------------------------------------------------------

#[test]
fn a_tail_call_inside_a_match_arm_charges_the_targets_effects() {
    let source = r#"module ArmHole
    intent =
        "The tail call that needs the effect sits in a match arm."
    exposes [pump, tick]
    effects [Console.print, Time.unixMs]

fn tick(n: Int) -> Int
    ? "Reads the clock, then hands over to pump."
    ! [Time.unixMs]
    _now = Time.unixMs()
    match n < 1
        true -> 0
        false -> pump(n - 1)

fn pump(n: Int) -> Int
    ? "Only prints, yet one arm hands over to tick."
    ! [Console.print]
    Console.print("pump")
    match n < 1
        true -> 0
        false -> tick(n - 1)
"#;
    assert_reports(
        &missing_effect_errors("armhole.av", source),
        &[
            "Function 'tick' calls 'pump' which has effect 'Console.print', but 'tick' does not declare it",
            "Function 'pump' calls 'tick' which has effect 'Time.unixMs', but 'pump' does not declare it",
        ],
        "tail calls in match arms, in both directions",
    );
}

#[test]
fn a_tail_call_to_a_capability_operation_wrapper_charges_the_operation() {
    let source = r#"module CapHole
    intent =
        "The tail call target performs a capability operation directly."
    exposes [readAll, step]
    effects [Console.print, Disk.readText]

fn readAll(path: String, n: Int) -> Result<String, String>
    ? "Reads the file, or hands back to step."
    ! [Disk.readText]
    match n < 1
        true -> Disk.readText(path)
        false -> step(path, n - 1)

fn step(path: String, n: Int) -> Result<String, String>
    ? "Prints, then hands over to the reader."
    ! [Console.print]
    Console.print("step")
    readAll(path, n - 1)
"#;
    assert_reports(
        &missing_effect_errors("caphole.av", source),
        &[
            "Function 'readAll' calls 'step' which has effect 'Console.print', but 'readAll' does not declare it",
            "Function 'step' calls 'readAll' which has effect 'Disk.readText', but 'step' does not declare it",
        ],
        "tail call to a function that performs a capability operation",
    );
}

#[test]
fn a_namespace_declaration_satisfies_a_tail_calls_operation_effect() {
    // `! [Disk]` covers `Disk.readText`, so the tail call from `wide` is settled. The
    // reverse edge is not: `Disk.readText` does not cover the whole `Disk` namespace, and
    // that is the only report this program earns.
    let source = r#"module CapNamespace
    intent =
        "A namespace-wide declaration covers the tail call target's operation."
    exposes [wide, narrow]
    effects [Console.print, Disk]

fn wide(path: String, n: Int) -> Result<String, String>
    ? "Declares the whole Disk namespace and hands over to the reader."
    ! [Disk, Console.print]
    Console.print("wide")
    narrow(path, n - 1)

fn narrow(path: String, n: Int) -> Result<String, String>
    ? "Reads the file, or hands back."
    ! [Disk.readText, Console.print]
    match n < 1
        true -> Disk.readText(path)
        false -> wide(path, n - 1)
"#;
    assert_reports(
        &missing_effect_errors("capnamespace.av", source),
        &[
            "Function 'narrow' calls 'wide' which has effect 'Disk', but 'narrow' does not declare it",
        ],
        "namespace shorthand across a tail call",
    );
}

#[test]
fn a_call_to_an_imported_module_in_tail_position_stays_charged() {
    // A call across a module boundary is written with a dotted path and is never rewritten
    // into a tail call, so the ordinary call path carries it. This pins that, because the
    // rewrite's reach is the reason the hole was as wide as it was.
    let root = temp_module_root("cross_module");
    let domain = root.join("Domain");
    std::fs::create_dir_all(&domain).expect("create Domain dir failed");
    std::fs::write(
        domain.join("Clock.av"),
        r#"module Clock
    exposes [read]
    intent =
        "Reads the clock."
    effects [Time.unixMs]

fn read() -> Int
    ? "Current epoch milliseconds."
    ! [Time.unixMs]
    Time.unixMs()
"#,
    )
    .expect("write Clock.av failed");

    let source = r#"module App
    depends [Domain.Clock]
    intent =
        "Calls the imported reader from a tail-position match arm."
    effects [Console.print]

fn pump(n: Int) -> Int
    ? "Prints, then hands over to the imported reader."
    ! [Console.print]
    Console.print("step")
    match n < 1
        true -> Domain.Clock.read()
        false -> pump(n - 1)
"#;
    let errors = errors_for_in(
        "app.av",
        source,
        root.to_str().expect("utf-8 temp module root"),
    );
    let missing: Vec<String> = errors
        .into_iter()
        .filter(|message| message.contains("does not declare it"))
        .collect();
    assert_reports(
        &missing,
        &[
            "Function 'pump' calls 'Domain.Clock.read' which has effect 'Time.unixMs', but 'pump' does not declare it",
        ],
        "imported call in tail position",
    );

    let _ = std::fs::remove_dir_all(&root);
}
