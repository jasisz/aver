//! Snapshot tests locking in CLI diagnostic output after the
//! canonical-diagnostics migration.
//!
//! Tty rendering is pinned via [`aver::tty_render::render_tty`]; JSON
//! output is the canonical bundle produced by
//! [`aver::diagnostics::AnalysisReport::to_json`] and per-record serde
//! serialization of [`aver::diagnostics::Diagnostic`].
//!
//! Colors are disabled so snapshots stay readable and diffable.

use aver::diagnostics::{
    AnalyzeOptions, analyze_source, verify_mismatch_diagnostic, verify_runtime_error_diagnostic,
};
use aver::tty_render::render_tty;

fn strip_colors() {
    colored::control::set_override(false);
}

#[test]
fn analyze_reports_type_error() {
    strip_colors();
    let source = r#"module Test
fn pick() -> Int
    ? "returns an Int"
    "hello"
"#;
    let opts = AnalyzeOptions::new("test.av");
    let report = analyze_source(source, &opts);
    let diag = report
        .diagnostics
        .iter()
        .find(|d| d.is_error() && d.slug.starts_with("type-"))
        .unwrap_or_else(|| {
            panic!(
                "expected a type error; got slugs: {:?}",
                report
                    .diagnostics
                    .iter()
                    .map(|d| d.slug)
                    .collect::<Vec<_>>()
            )
        });
    insta::assert_snapshot!("type_error_tty", render_tty(diag, false));
    insta::assert_snapshot!(
        "type_error_json",
        serde_json::to_string(diag).expect("serde serialization must not fail")
    );
}

#[test]
fn analyze_reports_missing_verify() {
    strip_colors();
    let source = r#"module Test
fn add(x: Int, y: Int) -> Int
    ? "adds two integers"
    x + y
"#;
    let opts = AnalyzeOptions::new("test.av");
    let report = analyze_source(source, &opts);
    let diag = report
        .diagnostics
        .iter()
        .find(|d| d.slug == "missing-verify")
        .unwrap_or_else(|| {
            panic!(
                "expected missing-verify diagnostic; got slugs: {:?}",
                report
                    .diagnostics
                    .iter()
                    .map(|d| d.slug)
                    .collect::<Vec<_>>()
            )
        });
    insta::assert_snapshot!("missing_verify_tty", render_tty(diag, false));
    insta::assert_snapshot!(
        "missing_verify_json",
        serde_json::to_string(diag).expect("serde serialization must not fail")
    );
}

#[test]
fn verify_mismatch_diagnostic_has_expected_fields() {
    strip_colors();
    let diag = verify_mismatch_diagnostic(
        "test.av",
        "verify add:\n    add(1, 2) == 3\n",
        "add",
        "add(1, 2) == 3",
        "3",
        "4",
        2,
        5,
        false,
        None,
    );
    insta::assert_snapshot!("verify_mismatch_tty", render_tty(&diag, false));
    insta::assert_snapshot!(
        "verify_mismatch_json",
        serde_json::to_string(&diag).expect("serde serialization must not fail")
    );
}

#[test]
fn verify_runtime_error_diagnostic_renders() {
    strip_colors();
    let diag = verify_runtime_error_diagnostic(
        "test.av",
        "verify div:\n    div(1, 0) == 0\n",
        "div",
        "div(1, 0) == 0",
        "divide by zero",
        2,
        5,
    );
    insta::assert_snapshot!("verify_runtime_error_tty", render_tty(&diag, false));
    insta::assert_snapshot!(
        "verify_runtime_error_json",
        serde_json::to_string(&diag).expect("serde serialization must not fail")
    );
}

#[test]
fn analysis_report_serializes_schema_version() {
    let report = analyze_source(
        r#"module Test
fn ok() -> Int
    ? "always zero"
    0

verify ok
    ok() => 0
"#,
        &AnalyzeOptions::new("test.av"),
    );
    let json = report.to_json();
    assert!(
        json.contains("\"schema_version\":1"),
        "AnalysisReport JSON must carry schema_version:1 — got: {}",
        json
    );
    assert!(
        json.contains("\"kind\":\"analysis\""),
        "AnalysisReport must declare kind — got: {}",
        json
    );
    assert!(
        json.contains("\"diagnostics\""),
        "AnalysisReport must contain diagnostics array — got: {}",
        json
    );
}
