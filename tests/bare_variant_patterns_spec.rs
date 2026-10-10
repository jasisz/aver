//! A constructor pattern without parentheses (`Shape.Rect ->`,
//! `Result.Ok ->`) matches its variant whatever the fields hold. The
//! front door spells it with one `_` per field, so every backend and the
//! Lean export read the ordinary form. One fixture, one expected stdout,
//! each backend compared against it. The Rust backend builds the same
//! fixture in `rust_codegen_regression.rs`.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};
use std::process::Command;

const FIXTURE: &str = "tests/fixtures/bare_variant_patterns.av";
const EXPECTED: &str = "tests/fixtures/bare_variant_patterns.expected";

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
fn vm_runs_bare_variant_patterns() {
    assert_eq!(run(&[]), expected());
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_runs_bare_variant_patterns_like_the_vm() {
    assert_eq!(run(&["--wasm-gc"]), expected());
}

#[cfg(feature = "wasip2")]
#[test]
fn wasip2_runs_bare_variant_patterns_like_the_vm() {
    assert_eq!(run(&["--wasip2"]), expected());
}

#[test]
fn verify_runs_cases_over_bare_variant_patterns() {
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("verify")
        .arg(FIXTURE)
        .output()
        .expect("run aver verify");
    assert!(out.status.success(), "{}", format_output(&out));
}

#[test]
fn lean_export_spells_every_field_of_a_bare_variant_pattern() {
    let dir = std::env::temp_dir().join(format!(
        "aver-bare-variant-lean-{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0)
    ));
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("proof")
        .arg(FIXTURE)
        .arg("-o")
        .arg(&dir)
        .output()
        .expect("run aver proof");
    assert!(out.status.success(), "{}", format_output(&out));
    let lean = std::fs::read_to_string(dir.join("BareVariants.lean")).expect("read the export");
    let _ = std::fs::remove_dir_all(&dir);
    for arm in [
        "| .circle _ => \"circle\"",
        "| .rect _ _ => \"rect\"",
        "| .dot => \"dot\"",
        "| .ok _ => true",
        "| .error _ => false",
    ] {
        assert!(lean.contains(arm), "missing `{arm}` in:\n{lean}");
    }
}
