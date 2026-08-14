//! Schema-stability tests for `aver compile --explain-passes --json`.
//!
//! These tests pin the typed JSON shape that CI scripts query via `jq`.
//! Adding fields is fine; renaming or removing requires bumping
//! `schema_version`. Each pass variant has its own structural assertion
//! so the failure message points at the broken contract directly.

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

// Parallel test fns share this binary's process, so a process-wide atomic
// counter (plus pid) makes every `tempfile` path unique. nanos alone raced:
// same-nanosecond callers got the same path and clobbered / removed each
// other's files mid-compile, so a varying subset of tests failed per run.
static TEMP_SEQ: AtomicU64 = AtomicU64::new(0);

fn tempfile(prefix: &str, suffix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let seq = TEMP_SEQ.fetch_add(1, Ordering::Relaxed);
    let pid = std::process::id();
    std::env::temp_dir().join(format!("{prefix}-{pid}-{nanos}-{seq}{suffix}"))
}

fn run_explain_passes(source: &str) -> serde_json::Value {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = tempfile("explain-passes", ".av");
    fs::write(&path, source).expect("write tempfile");
    let output = Command::new(aver_bin)
        .arg("compile")
        .arg(&path)
        .arg("--explain-passes")
        .arg("--json")
        .output()
        .expect("invoke aver");
    fs::remove_file(&path).ok();
    assert!(
        output.status.success(),
        "aver compile failed: stdout={} stderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).expect("parse JSON output")
}

#[test]
fn schema_version_is_pinned_to_1() {
    let json = run_explain_passes(
        r#"
module Demo
    intent = "smoke"
    depends []

fn main() -> Int
    1
"#,
    );
    assert_eq!(json["schema_version"], 1);
    assert!(json["passes"].is_array());
}

#[test]
fn every_canonical_stage_appears_in_order() {
    let json = run_explain_passes(
        r#"
module Demo
    intent = "smoke"
    depends []

fn main() -> Int
    1
"#,
    );
    let stages: Vec<&str> = json["passes"]
        .as_array()
        .unwrap()
        .iter()
        .map(|p| p["stage"].as_str().unwrap())
        .collect();
    assert_eq!(
        stages,
        vec![
            "tco",
            "typecheck",
            "interp_lower",
            "buffer_build",
            "resolve",
            "analyze",
            "escape",
            "last_use",
            "build_symbols",
            // Phase E (#147): NameResolve runs unconditionally after
            // BuildSymbols, mirroring how the symbol table itself is
            // always built. Backends consume `resolved_items` in lieu
            // of re-resolving `Expr` themselves.
            "name_resolve",
            "refinement_lower",
            // Per-module interval analysis (read-only diagnostic) runs
            // right after refinement_lower, consuming its refined types.
            "interval_analyze",
            "contract_lower",
            "law_lower",
        ]
    );
}

#[test]
fn tco_pass_exposes_typed_fields() {
    let json = run_explain_passes(
        r#"
module Demo
    intent = "tco fires"
    depends []

fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)

fn main() -> Int
    factorial(5, 1)
"#,
    );
    let tco = json["passes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["stage"] == "tco")
        .expect("tco pass present");
    let data = &tco["data"];
    assert!(data["tail_calls_added"].is_u64());
    assert!(data["fns_changed"].is_array());
    assert!(data["non_tail_recursive"].is_array());

    let factorial_change = data["fns_changed"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"] == "factorial")
        .expect("factorial in fns_changed");
    assert_eq!(factorial_change["before"], 0);
    assert!(factorial_change["after"].as_u64().unwrap() >= 1);
}

#[test]
fn buffer_build_pass_exposes_sink_data_when_fusion_fires() {
    let json = run_explain_passes(
        r#"
module Demo
    intent = "fusion site"
    depends []

fn build(xs: List<Int>, acc: List<String>) -> List<String>
    match xs
        [] -> acc
        [h, ..t] -> build(t, List.prepend(String.fromInt(h), acc))

fn main() -> String
    String.join(List.reverse(build([1, 2, 3], [])), ",")
"#,
    );
    let bb = json["passes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["stage"] == "buffer_build")
        .expect("buffer_build pass present");
    let data = &bb["data"];
    assert_eq!(data["rewrites"], 1);
    let synthesized = data["synthesized"].as_array().unwrap();
    assert!(
        synthesized.iter().any(|s| s == "build__buffered"),
        "expected build__buffered in synthesized: {synthesized:?}"
    );
    let sinks = data["sinks"].as_array().unwrap();
    assert!(sinks.iter().any(|s| s == "build"));
    assert_eq!(data["rewrites_by_sink"]["build"], 1);
}

/// The pipeline `--explain-passes` runs sees the entry file only, so a
/// program whose DEPENDENCY carries the fusable shape was reported as
/// having no fusion sites at all — while the compile path was quietly
/// deforesting that dependency. The report has to cover every module
/// the artifact is built from, with dep-side names module-qualified.
#[test]
fn buffer_build_pass_reports_a_fusion_site_living_in_a_dependency() {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(
        dir.path().join("rows.av"),
        r#"module Rows
    intent = "a dependency that owns the builder"
    exposes [render]
    effects []

fn collect(xs: List<Int>, acc: List<String>) -> List<String>
    ? "Render each value into the accumulator."
    match xs
        [] -> acc
        [h, ..t] -> collect(t, List.prepend(String.fromInt(h), acc))

fn render(xs: List<Int>) -> String
    ? "Join the rendered values with commas."
    String.join(List.reverse(collect(xs, [])), ",")
"#,
    )
    .expect("write rows.av");
    let entry = dir.path().join("main.av");
    std::fs::write(
        &entry,
        r#"module Main
    intent = "entry with no fusable shape of its own"
    depends [Rows]
    effects []

fn main() -> String
    Rows.render([1, 2, 3])
"#,
    )
    .expect("write main.av");

    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("compile")
        .arg(&entry)
        .arg("--explain-passes")
        .arg("--json")
        .arg("--module-root")
        .arg(dir.path())
        .output()
        .expect("invoke aver");
    assert!(
        output.status.success(),
        "aver compile failed: stdout={} stderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let json: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("parse JSON output");

    let bb = json["passes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["stage"] == "buffer_build")
        .expect("buffer_build pass present");
    let data = &bb["data"];
    assert_eq!(
        data["rewrites"], 1,
        "the dependency's fusion site must be counted: {data}"
    );
    let synthesized = data["synthesized"].as_array().unwrap();
    assert!(
        synthesized.iter().any(|s| s == "Rows.collect__buffered"),
        "expected the module-qualified synthesized name: {synthesized:?}"
    );
    assert_eq!(data["rewrites_by_sink"]["Rows.collect"], 1);
}

#[test]
fn analyze_pass_exposes_alloc_recursion_summary() {
    let json = run_explain_passes(
        r#"
module Demo
    intent = "analyze"
    depends []

fn main() -> Int
    1
"#,
    );
    let analyze = json["passes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["stage"] == "analyze")
        .expect("analyze pass present");
    let data = &analyze["data"];
    for field in [
        "total_fns",
        "no_alloc_fns",
        "recursive_fns",
        "mutual_tco_members",
        "unknown_alloc",
    ] {
        assert!(
            data[field].is_u64(),
            "analyze.data.{field} missing or wrong type: {data:?}"
        );
    }
    assert!(data["total_fns"].as_u64().unwrap() >= 1);
}

#[test]
fn interval_analyze_pass_exposes_count_fields() {
    // Drive a real two-sided refinement (`IntRange`, [0,100]) through
    // the diagnostic and pin the interval_analyze report's COUNT
    // fields (not free text). `IntRange.add` is the keystone
    // overflow-free op.
    let json = run_explain_passes(
        r#"
module IntRange
    exposes [fromInt, toInt, add]
    exposes opaque [IntRange]
    intent = "Range-bounded refinement [0,100]."
    effects []

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    ? "Smart constructor — admits 0..=100."
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("IntRange must be in 0..=100")

fn toInt(n: IntRange) -> Int
    ? "Unwrap."
    n.value

fn add(a: IntRange, b: IntRange) -> Result<IntRange, String>
    ? "Sum, re-validated."
    fromInt(a.value + b.value)
"#,
    );
    let pass = json["passes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["stage"] == "interval_analyze")
        .expect("interval_analyze pass present");
    let data = &pass["data"];
    for field in [
        "types_analyzed",
        "two_sided_bounded",
        "ops_overflow_free",
        "ops_needs_wider",
        "ops_unbounded",
        "raw_i64_eligible",
    ] {
        assert!(
            data[field].is_u64(),
            "interval_analyze.data.{field} missing or wrong type: {data:?}"
        );
    }
    // IntRange: 1 type, two-sided, `add` overflow-free, nothing else —
    // and the recognizer certifies it raw-i64-eligible.
    assert_eq!(data["types_analyzed"], 1);
    assert_eq!(data["two_sided_bounded"], 1);
    assert_eq!(data["ops_overflow_free"], 1);
    assert_eq!(data["ops_needs_wider"], 0);
    assert_eq!(data["ops_unbounded"], 0);
    assert_eq!(
        data["raw_i64_eligible"], 1,
        "IntRange [0,100] with an overflow-free `add` is raw-i64-eligible"
    );
}

#[test]
fn interval_analyze_pass_reports_natural_not_eligible() {
    // A one-sided refinement (`Natural`, n >= 0 → [0, +inf]) is NOT
    // raw-i64-eligible: the open upper bound never fits a machine word.
    // The recognizer must report 0 even though the type IS analyzed.
    let json = run_explain_passes(
        r#"
module Natural
    exposes [fromInt, toInt, add]
    exposes opaque [Natural]
    intent = "Non-negative refinement (one-sided)."
    effects []

record Natural
    value: Int

fn fromInt(n: Int) -> Result<Natural, String>
    ? "Smart constructor — admits non-negative ints."
    match n >= 0
        true  -> Result.Ok(Natural(value = n))
        false -> Result.Err("Nat must be non-negative")

fn toInt(n: Natural) -> Int
    ? "Unwrap."
    n.value

fn add(a: Natural, b: Natural) -> Result<Natural, String>
    ? "Sum, re-validated."
    fromInt(a.value + b.value)
"#,
    );
    let pass = json["passes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["stage"] == "interval_analyze")
        .expect("interval_analyze pass present");
    let data = &pass["data"];
    // The type is seen (one-sided interval recognized), its `add` op is
    // Unbounded ([0,+inf] + [0,+inf]), so it is NOT eligible.
    assert_eq!(data["types_analyzed"], 1);
    assert_eq!(data["two_sided_bounded"], 0);
    assert_eq!(data["ops_unbounded"], 1);
    assert_eq!(
        data["raw_i64_eligible"], 0,
        "Natural's open upper bound makes it NOT raw-i64-eligible"
    );
}
