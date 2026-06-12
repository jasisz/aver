//! Regression — `aver audit` must count `error[verify-rhs]` (and every
//! other static check error) in the check-error total and the exit code.
//!
//! The audit's per-file aggregation excluded every diagnostic whose slug
//! starts with `verify-` from the check-error count. The intent was to
//! avoid double-counting verify EXECUTION failures (slugs
//! `verify-mismatch` / `verify-runtime-error` / …, already counted via
//! the verify scorecard) — but `verify-rhs` is a STATIC check error (a
//! verify case calling the function under test on the right side of
//! `=>`) that happens to share the slug prefix. It was printed as
//! `error[verify-rhs]` yet contributed to neither total, so a file whose
//! only problems were verify-rhs errors audited as "0 check errors" with
//! exit 0. The fix keys the exclusion on severity (`Fail` = verify
//! execution, counted elsewhere; `Error` = static check error, counted
//! here) instead of the slug prefix.

use std::fs;
use std::process::Command;

/// Run `aver audit --json` on `source` written to a temp file; return
/// (exit_code, stdout).
fn run_audit_json(name: &str, source: &str) -> (Option<i32>, String) {
    let dir = std::env::temp_dir().join(format!("aver_audit_count_{name}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("create temp dir");
    let file = dir.join(format!("{name}.av"));
    fs::write(&file, source).expect("write source");

    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("audit")
        .arg(&file)
        .arg("--json")
        .output()
        .expect("spawn aver audit");
    let _ = fs::remove_dir_all(&dir);
    (
        out.status.code(),
        String::from_utf8_lossy(&out.stdout).to_string(),
    )
}

/// Extract the `"check_errors":N` count from the audit's trailing
/// summary JSON line.
fn summary_check_errors(stdout: &str) -> usize {
    let summary = stdout
        .lines()
        .find(|l| l.contains("\"kind\":\"summary\""))
        .unwrap_or_else(|| panic!("no summary line in audit output:\n{stdout}"));
    let tail = summary
        .split("\"check_errors\":")
        .nth(1)
        .unwrap_or_else(|| panic!("no check_errors field in summary: {summary}"));
    tail.chars()
        .take_while(|c| c.is_ascii_digit())
        .collect::<String>()
        .parse()
        .unwrap_or_else(|_| panic!("unparseable check_errors in summary: {summary}"))
}

/// A verify case calling the target on the right side of `=>` is a
/// static check error (`error[verify-rhs]`). Audit must report a
/// nonzero check-error count and exit 1 — before the fix it printed
/// the error but reported "0 check errors" and exited 0.
#[test]
fn verify_rhs_error_counts_and_fails_audit() {
    let (code, stdout) = run_audit_json(
        "verify_rhs",
        r#"module VerifyRhsAudit
    intent = "Fixture: verify case calls the target on the right side"
    exposes [double]
    effects []

fn double(x: Int) -> Int
    ? "Doubles the input"
    x * 2

verify double
    double(2) => double(2)
    double(3) => 6
"#,
    );
    let check_errors = summary_check_errors(&stdout);
    assert!(
        check_errors >= 1,
        "verify-rhs error must count as a check error, got {check_errors}:\n{stdout}"
    );
    assert_eq!(
        code,
        Some(1),
        "audit with a verify-rhs error must exit 1:\n{stdout}"
    );
}

/// Control — a clean file still audits green (0 check errors, exit 0),
/// so the severity-keyed counting did not start counting warnings or
/// passing verify blocks.
#[test]
fn clean_file_still_audits_green() {
    let (code, stdout) = run_audit_json(
        "clean",
        r#"module CleanAudit
    intent = "Fixture: clean module for the audit control case"
    exposes [double]
    effects []

fn double(x: Int) -> Int
    ? "Doubles the input"
    x * 2

verify double
    double(2) => 4
    double(3) => 6
"#,
    );
    let check_errors = summary_check_errors(&stdout);
    assert_eq!(check_errors, 0, "clean file: {stdout}");
    assert_eq!(code, Some(0), "clean file must exit 0:\n{stdout}");
}

/// Control — a failing verify CASE is a verify failure, not a check
/// error: it must not be double-counted in both totals, and the exit
/// code stays 1 through the verify-failures axis.
#[test]
fn failing_verify_case_counts_as_verify_failure_not_check_error() {
    let (code, stdout) = run_audit_json(
        "verify_fail",
        r#"module VerifyFailAudit
    intent = "Fixture: one failing verify case, no static errors"
    exposes [inc]
    effects []

fn inc(x: Int) -> Int
    ? "Adds one"
    x + 1

verify inc
    inc(1) => 3
"#,
    );
    let check_errors = summary_check_errors(&stdout);
    assert_eq!(
        check_errors, 0,
        "verify execution failure must not inflate check errors:\n{stdout}"
    );
    let summary = stdout
        .lines()
        .find(|l| l.contains("\"kind\":\"summary\""))
        .unwrap();
    assert!(
        summary.contains("\"verify_failures\":1"),
        "expected 1 verify failure in summary: {summary}"
    );
    assert_eq!(code, Some(1), "failing verify case must exit 1:\n{stdout}");
}
