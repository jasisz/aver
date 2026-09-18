//! Imported protocol segments consume a typed tape, never live host effects.
#[path = "support/aver_cmd.rs"]
mod aver_cmd;
use aver_cmd::{aver_bin, format_output, repo_root};
use std::process::Command;

fn verify(args: &[&str]) {
    let root = repo_root().join("tests/fixtures/yield_imported_effects");
    let out = Command::new(aver_bin())
        .arg("verify")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(root)
        .args(args)
        .output()
        .unwrap();
    assert!(out.status.success(), "{}", format_output(&out));
}

#[test]
fn imported_effects_preserve_prefixes_private_recursion_and_early_errors() {
    verify(&[]);
}

#[test]
#[cfg(feature = "wasm")]
fn imported_effect_observers_execute_on_wasm_gc() {
    verify(&["--wasm-gc"]);
}
