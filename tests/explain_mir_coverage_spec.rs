//! Schema-stability tests for `aver compile --explain-mir-coverage --json`.
//!
//! MIR is the default VM compile path; this diagnostic reports how many
//! fns the MIR lowering accepts vs. drops to the HIR fallback. The JSON
//! shape is queried by tooling, so adding fields is fine but renaming or
//! removing one requires bumping `schema_version`.

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

fn run_coverage(source: &str) -> serde_json::Value {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = tempfile("explain-mir-coverage", ".av");
    fs::write(&path, source).expect("write tempfile");
    let output = Command::new(aver_bin)
        .arg("compile")
        .arg(&path)
        .arg("--explain-mir-coverage")
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

const FULLY_LOWERED: &str = r#"
module Demo
    intent = "smoke"
    depends []

fn double(x: Int) -> Int
    x + x

fn main() -> Int
    double(21)
"#;

#[test]
fn schema_shape_is_pinned() {
    let json = run_coverage(FULLY_LOWERED);
    assert_eq!(json["schema_version"], 1);
    assert!(json["total"].is_number());
    assert!(json["mir_lowered"].is_number());
    assert!(json["hir_fallback"].is_number());
    assert!(json["coverage_ratio"].is_number());
    assert!(
        json["blocked_by_shape"].is_array(),
        "blocked_by_shape must be an array"
    );
}

#[test]
fn arithmetic_program_is_fully_covered() {
    let json = run_coverage(FULLY_LOWERED);
    // Both `double` and `main` sit in the MIR subset, so nothing rides
    // the HIR fallback.
    assert_eq!(json["total"], 2);
    assert_eq!(json["mir_lowered"], 2);
    assert_eq!(json["hir_fallback"], 0);
    assert_eq!(json["coverage_ratio"], 1.0);
    assert_eq!(
        json["blocked_by_shape"].as_array().map(|a| a.len()),
        Some(0),
        "fully-lowered program lists no blockers"
    );
}

#[test]
fn totals_are_internally_consistent() {
    // `mir_lowered + hir_fallback == total`, and each blocker entry has a
    // string shape + positive count — the invariant tooling relies on.
    let json = run_coverage(FULLY_LOWERED);
    let total = json["total"].as_u64().unwrap();
    let lowered = json["mir_lowered"].as_u64().unwrap();
    let fallback = json["hir_fallback"].as_u64().unwrap();
    assert_eq!(lowered + fallback, total);
    for entry in json["blocked_by_shape"].as_array().unwrap() {
        assert!(entry["shape"].is_string());
        assert!(entry["count"].as_u64().unwrap() >= 1);
    }
}
