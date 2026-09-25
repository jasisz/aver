//! Nested literal / constructor patterns and general list patterns end to
//! end: the front door compiles them into flat matches, so every backend
//! runs the same program. One fixture, one expected stdout, each backend
//! compared against it; plus the checker's diagnostics as the CLI prints
//! them.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, cleanup, format_output, repo_root, temp_module};
use std::process::Command;

const FIXTURE: &str = "tests/fixtures/nested_patterns.av";
const EXPECTED: &str = "tests/fixtures/nested_patterns.expected";

fn expected() -> String {
    std::fs::read_to_string(repo_root().join(EXPECTED)).expect("read the expected output")
}

fn run(extra: &[&str]) -> String {
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(FIXTURE)
        .args(extra)
        .output()
        .expect("run aver");
    assert!(out.status.success(), "{}", format_output(&out));
    String::from_utf8_lossy(&out.stdout).into_owned()
}

#[test]
fn vm_runs_nested_patterns() {
    assert_eq!(run(&[]), expected());
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_runs_nested_patterns_like_the_vm() {
    assert_eq!(run(&["--wasm-gc"]), expected());
}

#[cfg(feature = "wasip2")]
#[test]
fn wasip2_runs_nested_patterns_like_the_vm() {
    assert_eq!(run(&["--wasip2"]), expected());
}

#[test]
fn verify_runs_cases_over_nested_patterns() {
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("verify")
        .arg(FIXTURE)
        .output()
        .expect("run aver verify");
    assert!(out.status.success(), "{}", format_output(&out));
}

fn check(prefix: &str, source: &str) -> String {
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("check")
        .arg(&path)
        .output()
        .expect("run aver check");
    cleanup(&path);
    format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

#[test]
fn check_names_the_missing_nested_case() {
    let report = check(
        "nested-missing",
        "module M\n    intent = \"t\"\n\nfn f(o: Option<Int>) -> Int\n    ? \"t\"\n    match o\n        Option.Some(0) -> 1\n        Option.None -> 0\n",
    );
    assert!(
        report.contains("Non-exhaustive match: missing pattern Option.Some(_)"),
        "{report}"
    );
}

#[test]
fn check_reports_an_arm_only_the_compiled_match_can_see_is_dead() {
    // No single earlier arm covers `Option.Some(_)`; `true` and `false`
    // together do, which only the compiled decision tree sees.
    let report = check(
        "nested-dead",
        "module M\n    intent = \"t\"\n\nfn f(o: Option<Bool>) -> Int\n    ? \"t\"\n    match o\n        Option.Some(true) -> 1\n        Option.Some(false) -> 2\n        Option.Some(_) -> 3\n        Option.None -> 0\n",
    );
    assert!(
        report.contains("Unreachable match arm: no value reaches pattern Option.Some(_)"),
        "{report}"
    );
}

#[test]
fn nested_patterns_inside_a_yield_function_are_refused() {
    let report = check(
        "nested-yield",
        "module M\n    intent = \"t\"\n\nfn f(o: Option<Int>) -> Int\n    ? \"t\"\n    ! [yield]\n    match o\n        Option.Some(0) -> 1\n        _ -> 0\n",
    );
    assert!(
        report.contains("not supported inside the `yield` function 'f'"),
        "{report}"
    );
}
