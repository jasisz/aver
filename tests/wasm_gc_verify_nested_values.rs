//! `aver verify --wasm-gc` on compound values (jasisz/aver#1348): nested
//! sums — including fields whose declared type is newtype-erased to a
//! primitive — compare by value inside wasm, and a failing case prints
//! the actual runtime value, rendered by synthesized `__verify_repr_*`
//! helpers, exactly as the VM lane prints it.
#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::path::PathBuf;
use std::process::{Command, Output};

fn fixture(name: &str) -> PathBuf {
    repo_root().join("tests/fixtures").join(name)
}

fn verify(fixture_name: &str, args: &[&str]) -> Output {
    let dir = fixture(fixture_name);
    let mut cmd = Command::new(aver_bin());
    cmd.current_dir(repo_root());
    cmd.arg("verify").arg(dir.join("main.av"));
    cmd.arg("--module-root").arg(&dir);
    cmd.args(args);
    cmd.output().expect("aver runs")
}

/// Every `expected:` / `actual:` line in a verify report, in order —
/// the two lanes must agree on the whole set.
fn expected_actual_lines(out: &Output) -> Vec<String> {
    let stdout = String::from_utf8_lossy(&out.stdout);
    stdout
        .lines()
        .filter(|line| {
            let trimmed = line.trim_start();
            trimmed.starts_with("expected:") || trimmed.starts_with("actual:")
        })
        .map(|line| line.trim().to_string())
        .collect()
}

/// The pinned fixture exercises every compound shape the synthesized
/// renderer supports — three-deep sums, a newtype-erased field, Option,
/// Result, a record, a list, a tuple and a nullary variant — and both
/// lanes must pass it with the same tally.
#[test]
fn nested_sums_verify_the_same_on_both_lanes() {
    for args in [&[][..], &["--wasm-gc"][..]] {
        let out = verify("wasm_verify_nested_sums", args);
        assert!(
            out.status.success(),
            "verify {:?} failed:\n{}",
            args,
            format_output(&out)
        );
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert!(
            stdout.contains("9/9 cases passed | 0 failed"),
            "expected 9/9 passing on {:?}:\n{}",
            args,
            format_output(&out)
        );
    }
}

/// A wrong expectation must name the actual runtime value on wasm-gc,
/// rendered identically to the VM lane — not a placeholder.
#[test]
fn a_wrong_expectation_shows_the_actual_value_on_wasm_gc() {
    let vm = verify("wasm_verify_nested_sums_wrong", &[]);
    let wasm = verify("wasm_verify_nested_sums_wrong", &["--wasm-gc"]);
    assert!(
        !vm.status.success(),
        "VM lane should fail:\n{}",
        format_output(&vm)
    );
    assert!(
        !wasm.status.success(),
        "wasm-gc lane should fail:\n{}",
        format_output(&wasm)
    );
    let vm_lines = expected_actual_lines(&vm);
    let wasm_lines = expected_actual_lines(&wasm);
    assert_eq!(vm_lines.len(), 6, "three cases, two lines each:\n{vm:?}");
    assert_eq!(
        vm_lines, wasm_lines,
        "both lanes must print the same expected/actual lines"
    );
    for line in &wasm_lines {
        assert!(
            !line.contains("wasm-gc compound-value repr") && !line.contains("no wasm-gc rendering"),
            "placeholder leaked into wasm-gc output: {line}"
        );
    }
}
