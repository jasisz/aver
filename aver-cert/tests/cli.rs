#![cfg(feature = "verify")]

use std::path::Path;
use std::process::Command;

#[test]
fn library_verify_api_fails_closed_on_missing_inputs() {
    let error = aver_cert::verify(
        Path::new("definitely-missing-artifact.wasm"),
        Path::new("definitely-missing-cert"),
    )
    .expect_err("missing artifact must be declined");

    assert!(error.contains("cannot read artifact"), "{error}");
}

#[test]
fn standalone_cli_exposes_the_verifier_commands() {
    let output = Command::new(env!("CARGO_BIN_EXE_aver-cert"))
        .arg("--help")
        .output()
        .expect("standalone verifier starts");
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success(), "{stdout}");
    assert!(stdout.contains("verify"), "{stdout}");
    assert!(stdout.contains("explain"), "{stdout}");
    assert!(stdout.contains("inspect"), "{stdout}");
}

#[test]
fn standalone_cli_reports_declined_and_exits_nonzero() {
    let output = Command::new(env!("CARGO_BIN_EXE_aver-cert"))
        .args([
            "verify",
            "definitely-missing-artifact.wasm",
            "definitely-missing-cert",
        ])
        .output()
        .expect("standalone verifier starts");
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success(), "missing inputs must fail closed");
    assert!(stderr.contains("DECLINED"), "{stderr}");
    assert!(stderr.contains("cannot read artifact"), "{stderr}");
}
