//! Contract-v1 must reject imported named boundary types before publishing a
//! descriptor or attempting to bind a provider.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

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
    fs::write(
        root.join("Bytes.av"),
        "module Bytes\n    exposes [Bytes]\n\nrecord Bytes\n    values: List<Int>\n",
    )
    .expect("write Bytes module");
    fs::write(
        root.join("Ripemd.av"),
        "module Ripemd\n    kind = capability\n    semantics = pure\n    exposes [hash160]\n    depends [Bytes]\n\noperation hash160(input: Bytes) -> Bytes\n    ? \"Hash bytes.\"\n",
    )
    .expect("write capability module");
    fs::write(
        root.join("main.av"),
        "module Main\n    depends [Bytes, Ripemd]\n    exposes [main]\n\nfn main() -> Int\n    0\n",
    )
    .expect("write entry module");
}

fn run(root: &Path, command: &str) -> Output {
    Command::new(aver_bin())
        .arg(command)
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(root)
        .output()
        .expect("run aver command")
}

#[test]
fn imported_bare_boundary_type_is_rejected_before_contract_hash_publication() {
    let temp = tempfile::tempdir().expect("temporary module root");
    write_fixture(temp.path());

    for command in ["check", "capabilities"] {
        let output = run(temp.path(), command);
        assert!(
            !output.status.success(),
            "{command} unexpectedly passed:\n{}",
            command_report(&output)
        );
        let report = command_report(&output);
        assert!(
            report.contains("cross-module boundary type 'Bytes'")
                && report.contains("contract_hash"),
            "{command} missed the static contract diagnostic:\n{report}"
        );
        assert_eq!(
            report.matches("cross-module boundary type 'Bytes'").count(),
            2,
            "{command} should report the parameter and result once each:\n{report}"
        );
        assert!(
            report.contains("operation 'Ripemd.hash160' parameter 0 uses")
                && report.contains("operation 'Ripemd.hash160' result uses"),
            "{command} did not identify the two boundary positions:\n{report}"
        );
        if command == "check" {
            let lower = report.to_lowercase();
            assert!(
                lower.contains("at: ripemd.av:7:1")
                    && report.contains("operation hash160(input: Bytes) -> Bytes")
                    && !lower.contains("at: main.av:7:1"),
                "check did not render the capability module's source location:\n{report}"
            );
        }
        assert!(
            !output
                .stdout
                .windows("contract_hash: sha256:".len())
                .any(|window| window == b"contract_hash: sha256:"),
            "{command} published a hash for an invalid contract:\n{report}"
        );
        assert!(
            !report.contains("Ripemd.Bytes"),
            "{command} invented a capability-owned type name:\n{report}"
        );
    }
}
