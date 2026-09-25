//! `missing-verify` and capability resources.
//!
//! A verify case cannot write a `Tcp.Connection` or a `Work.Job`: a
//! provider mints them and no source expression denotes one. A function
//! that needs such a value as an argument, directly or inside a tuple,
//! record or sum that always carries one, cannot be called from a verify
//! block, so `check` does not ask it for one. A parameter that has an empty
//! value (`List`, `Option`, a sum with a resource-free variant) can still be
//! written, and its function still needs its verify block.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
use aver_cmd::{aver_bin, format_output, repo_root};
use std::process::Command;

#[test]
fn check_asks_no_verify_block_of_a_function_no_case_could_call() {
    let root = repo_root().join("tests/fixtures/verify_handle_params");
    let output = Command::new(aver_bin())
        .arg("check")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(&root)
        .output()
        .expect("run aver check");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let mut missing: Vec<&str> = text
        .lines()
        .filter_map(|line| {
            let (_, rest) = line.split_once("Function '")?;
            let (name, tail) = rest.split_once('\'')?;
            tail.starts_with(" has no verify block").then_some(name)
        })
        .collect();
    missing.sort_unstable();
    missing.dedup();
    assert_eq!(
        missing,
        ["maybeKind", "optional", "owedCount"],
        "{}",
        format_output(&output)
    );
    assert!(
        !text.contains("error[type"),
        "the fixture must type-check:\n{}",
        format_output(&output)
    );
}
