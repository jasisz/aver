//! Missing custom providers fail only concrete verify cases that dispatch them.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

const HASH160_SOURCE: &str = "module Hash160
    kind = capability
    semantics = pure
    exposes [digest]

operation digest(input: List<Int>) -> List<Int>
    ? \"Hash bytes through a custom provider.\"
";

const MAIN_SOURCE: &str = "module Main
    depends [Hash160]
    exposes [hashed, unrelated]

fn hashed(input: List<Int>) -> List<Int>
    ? \"Reach the unbound capability.\"
    Hash160.digest(input)

verify hashed
    hashed([0]) => [1]

fn unrelated(n: Int) -> Int
    ? \"Touch no capability.\"
    n + 1

verify unrelated
    unrelated(1) => 2
    unrelated(0) => 1

fn main() -> Int
    unrelated(1)
";

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn command_report(output: &Output) -> String {
    format!(
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn write_fixture(root: &Path) {
    fs::write(root.join("Hash160.av"), HASH160_SOURCE).expect("write capability module");
    fs::write(root.join("main.av"), MAIN_SOURCE).expect("write entry module");
}

#[test]
fn unrelated_cases_survive_an_unbound_capability_in_the_same_program() {
    let temp = tempfile::tempdir().expect("temporary module root");
    write_fixture(temp.path());
    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        !output.status.success(),
        "the reached capability case must still fail:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("hashed") && report.contains("capability provider missing"),
        "the reached case must fail at provider dispatch:\n{report}"
    );
    assert!(
        report.contains("unrelated") && report.contains("2/2"),
        "unrelated cases must keep running:\n{report}"
    );
    assert!(
        !report.contains("capability contract missing at runtime"),
        "verify must install the checked contract registry:\n{report}"
    );
}

#[test]
fn normal_run_keeps_strict_whole_program_provider_preflight() {
    let temp = tempfile::tempdir().expect("temporary module root");
    write_fixture(temp.path());
    let output = Command::new(aver_bin())
        .arg("run")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver program");
    assert!(
        !output.status.success(),
        "normal execution must keep strict provider preflight:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("capability provider missing for 'Hash160.digest'"),
        "normal execution must reject the unbound program before main runs:\n{report}"
    );
}

#[test]
fn loaded_verify_path_has_the_same_case_granularity() {
    let items = aver::source::parse_source(MAIN_SOURCE).expect("parse entry module");
    let loaded = vec![aver::source::LoadedModule {
        dep_name: "Hash160".to_string(),
        items: aver::source::parse_source(HASH160_SOURCE).expect("parse capability module"),
        path: "Hash160.av".into(),
    }];
    let results = aver::diagnostics::vm_verify::run_verify_for_items_vm_with_loaded(
        items, loaded, None, "main.av",
    )
    .expect("loaded verify path must compile the checked capability contract");

    let hashed = results
        .iter()
        .find(|result| result.fn_name == "hashed")
        .expect("hashed verify result");
    assert_eq!((hashed.passed, hashed.failed), (0, 1));
    assert!(matches!(
        &hashed.case_results[0].outcome,
        aver::checker::VerifyCaseOutcome::RuntimeError { error }
            if error.contains("capability provider missing for 'Hash160.digest'")
    ));

    let unrelated = results
        .iter()
        .find(|result| result.fn_name == "unrelated")
        .expect("unrelated verify result");
    assert_eq!((unrelated.passed, unrelated.failed), (2, 0));
}
