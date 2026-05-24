//! Schema-stability tests for `aver compile --explain-passes --json`.
//!
//! These tests pin the typed JSON shape that CI scripts query via `jq`.
//! Adding fields is fine; renaming or removing requires bumping
//! `schema_version`. Each pass variant has its own structural assertion
//! so the failure message points at the broken contract directly.

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn tempfile(prefix: &str, suffix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    std::env::temp_dir().join(format!("{prefix}-{nanos}{suffix}"))
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
            "refinement_lower",
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
