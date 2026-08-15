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

/// `aver compile FILE --emit-ir-after=STAGE`, returning the dump.
fn run_emit_ir_after(source: &str, stage: &str) -> String {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = tempfile("emit-ir-after", ".av");
    fs::write(&path, source).expect("write tempfile");
    let output = Command::new(aver_bin)
        .arg("compile")
        .arg(&path)
        .arg(format!("--emit-ir-after={stage}"))
        .output()
        .expect("invoke aver");
    fs::remove_file(&path).ok();
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(
        !stderr.contains("unknown --emit-ir-after stage"),
        "{stage} is not an --emit-ir-after stage: {stderr}"
    );
    assert!(
        output.status.success(),
        "aver compile failed: stdout={stdout} stderr={stderr}"
    );
    stdout
}

/// Every AST stage that rewrites the program is dumpable, so a fusion can
/// be read rather than inferred from a count. `chars_fusion` runs AFTER
/// `buffer_build`, so without it the last dumpable AST stage is no longer
/// the program the runtime backends compile.
#[test]
fn the_chars_fusion_stage_can_be_dumped_like_its_sibling() {
    let source = r#"
module Dumpable
    intent = "a character loop and the match it calls"
    effects []

fn value(character: String) -> Int
    match character
        "0" -> 0
        "1" -> 1
        _ -> -1

fn total(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> total(tail, acc + value(head))

fn main() -> Int
    total(String.chars("101"), 0)
"#;

    let before = run_emit_ir_after(source, "buffer_build");
    assert!(
        !before.contains("total__cursor") && !before.contains("__str_cursor"),
        "the stage before chars_fusion must not carry its output:\n{before}"
    );

    let after = run_emit_ir_after(source, "chars_fusion");
    assert!(
        after.contains("total__cursor")
            && after.contains("__str_cursor_end")
            && after.contains("__str_code1"),
        "the dump must show the cursor variant and the codepoint match:\n{after}"
    );
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
            "chars_fusion",
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
    // The pass runs for the rust and VM pipelines only; `--explain-passes`
    // runs one pipeline whatever `--target` says, so the report has to
    // name the targets its count is about.
    let targets: Vec<&str> = data["targets"]
        .as_array()
        .expect("targets field present")
        .iter()
        .map(|t| t.as_str().unwrap())
        .collect();
    assert_eq!(targets, vec!["rust", "vm"]);
}

/// The pass is off for `--target wasm-gc` / `--target wasip2`, so a
/// report of rewritten sites would be describing an artifact the reader
/// did not ask to build. The human report says which pipelines the
/// count belongs to; the reader should not have to know the toggle.
#[test]
fn buffer_build_report_names_the_targets_its_count_belongs_to() {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = tempfile("explain-passes-targets", ".av");
    fs::write(
        &path,
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
    )
    .expect("write tempfile");
    let output = Command::new(aver_bin)
        .arg("compile")
        .arg(&path)
        .arg("--explain-passes")
        .output()
        .expect("invoke aver");
    fs::remove_file(&path).ok();
    assert!(output.status.success());
    let report = String::from_utf8_lossy(&output.stdout);
    assert!(
        report.contains("--target wasm-gc and --target wasip2 build without this pass"),
        "the buffer_build section must scope its count to the deforesting targets:\n{report}"
    );
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

/// Chars fusion has to make the same two statements the buffer-build
/// report makes: what fired, and where the artifact that carries it is.
/// A loop it DECLINED is reported too — a fusion that silently stops
/// firing is the regression this diagnostic exists to catch, and there
/// is no other way to see it from outside.
#[test]
fn chars_fusion_pass_reports_what_fired_what_declined_and_for_which_targets() {
    let json = run_explain_passes(
        r#"
module Chars
    intent = "one loop that fuses, one that cannot, and a character match"
    effects []

fn value(character: String) -> Int
    ? "Decode one decimal digit."
    match String.toLower(character)
        "0" -> 0
        "1" -> 1
        _ -> -1

fn total(chars: List<String>, acc: Int) -> Int
    ? "Add up the decodable digits."
    match chars
        [] -> acc
        [head, ..tail] -> total(tail, acc + value(head))

fn sized(chars: List<String>, acc: Int) -> Int
    ? "Stops by measuring the list, so no cursor can stand in for it."
    match chars
        [] -> acc
        [head, ..tail] -> match List.len(chars) > 2
            true -> sized(tail, acc + 1)
            false -> acc

fn main() -> Int
    total(String.chars("101"), 0) + sized(String.chars("101"), 0)
"#,
    );
    let pass = json["passes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["stage"] == "chars_fusion")
        .expect("chars_fusion pass present");
    let data = &pass["data"];
    assert_eq!(data["cursor_rewrites"], 1, "one traversal fuses: {data}");
    assert_eq!(
        data["synthesized"].as_array().unwrap(),
        &vec![serde_json::json!("total__cursor")],
        "{data}"
    );
    assert_eq!(
        data["codepoint_matches"], 1,
        "the character match fuses too: {data}"
    );
    assert_eq!(data["codepoint_matches_by_fn"]["value"], 1);
    assert!(
        data["declined"]["sized"]
            .as_str()
            .expect("the measured loop is reported as declined")
            .contains("cursor cannot stand in for"),
        "{data}"
    );
    assert_eq!(
        data["targets"].as_array().unwrap(),
        &vec![serde_json::json!("rust"), serde_json::json!("vm")],
        "the count belongs to the rust and VM artifacts and to no other: {data}"
    );
}

/// The same honesty the buffer-build report owes about dependencies:
/// `aver run` and the Rust compile path fuse every dependency too, so a
/// report that only looked at the entry file would say a program with a
/// fused dependency fused nothing.
#[test]
fn chars_fusion_pass_reports_a_loop_living_in_a_dependency() {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(
        dir.path().join("scan.av"),
        r#"module Scan
    intent = "a dependency that owns the character loop"
    exposes [digits]
    effects []

fn value(character: String) -> Int
    ? "Decode one decimal digit."
    match character
        "0" -> 0
        "1" -> 1
        _ -> -1

fn walk(chars: List<String>, acc: Int) -> Int
    ? "Add up the decodable digits."
    match chars
        [] -> acc
        [head, ..tail] -> walk(tail, acc + value(head))

fn digits(text: String) -> Int
    ? "Sum the decimal digits of a string."
    walk(String.chars(text), 0)
"#,
    )
    .expect("write scan.av");
    let entry = dir.path().join("main.av");
    std::fs::write(
        &entry,
        r#"module Main
    intent = "entry with no character loop of its own"
    depends [Scan]
    effects []

fn main() -> Int
    Scan.digits("101")
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
    let data = &json["passes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["stage"] == "chars_fusion")
        .expect("chars_fusion pass present")["data"];
    assert_eq!(
        data["cursor_rewrites"], 1,
        "the dependency's traversal must be counted: {data}"
    );
    assert!(
        data["synthesized"]
            .as_array()
            .unwrap()
            .iter()
            .any(|s| s == "Scan.walk__cursor"),
        "expected the module-qualified synthesized name: {data}"
    );
    assert_eq!(data["codepoint_matches_by_fn"]["Scan.value"], 1, "{data}");
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
