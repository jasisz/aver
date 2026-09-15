//! The Knowledge laws are exercised by an actual answer module and Work seam.
#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};
use std::process::{Command, Output};

fn aver(args: &[&str]) -> Output {
    Command::new(aver_bin())
        .current_dir(repo_root().join("examples/knowledge"))
        .args(args)
        .output()
        .expect("aver runs")
}

fn assert_run(out: &Output) {
    assert!(out.status.success(), "{}", format_output(out));
    let text = String::from_utf8_lossy(&out.stdout);
    let mut lines: Vec<_> = text.lines().collect();
    lines.sort();
    assert_eq!(
        lines,
        [
            "local verdict: invalid",
            "peer: forged=false, admitted=true",
            "reader: stable=true"
        ],
        "{}",
        format_output(out)
    );
}

#[test]
fn admission_and_local_work_feed_a_stable_reader() {
    assert_run(&aver(&["run", "main.av", "--module-root", "."]));
}

#[test]
fn provider_cases_and_generated_laws_verify() {
    for args in [
        vec!["check", "main.av", "--module-root", "."],
        vec!["verify", "main.av", "--module-root", "."],
        vec!["format", ".", "--check"],
    ] {
        let out = aver(&args);
        assert!(out.status.success(), "{}", format_output(&out));
    }
}

#[test]
fn local_work_observations_replay() {
    let recording = tempfile::tempdir().unwrap();
    let out = aver(&[
        "run",
        "main.av",
        "--module-root",
        ".",
        "--record",
        recording.path().to_str().unwrap(),
    ]);
    assert!(out.status.success(), "{}", format_output(&out));
    let out = aver(&[
        "replay",
        recording.path().to_str().unwrap(),
        "--test",
        "--diff",
    ]);
    assert!(out.status.success(), "{}", format_output(&out));
}

#[cfg(feature = "wasm")]
#[test]
fn knowledge_provider_runs_with_wasm_gc_workers() {
    assert_run(&aver(&[
        "run",
        "main.av",
        "--module-root",
        ".",
        "--wasm-gc",
    ]));
}

#[cfg(feature = "wasip2")]
#[test]
fn knowledge_provider_runs_on_wasip2() {
    assert_run(&aver(&["run", "main.av", "--module-root", ".", "--wasip2"]));
}

#[cfg(feature = "runtime")]
#[test]
fn knowledge_provider_runs_on_generated_rust() {
    let project = tempfile::tempdir().unwrap();
    let out = aver(&[
        "compile",
        "main.av",
        "--module-root",
        ".",
        "--target",
        "rust",
        "--name",
        "knowledge_provider",
        "-o",
        project.path().to_str().unwrap(),
    ]);
    assert!(out.status.success(), "{}", format_output(&out));
    let target = repo_root().join("target/knowledge-provider-rust");
    let out = Command::new("cargo")
        .args(["build", "--offline", "--quiet", "--manifest-path"])
        .arg(project.path().join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", &target)
        .output()
        .unwrap();
    assert!(out.status.success(), "{}", format_output(&out));
    let out = Command::new(target.join("debug").join(format!(
        "knowledge_provider{}",
        std::env::consts::EXE_SUFFIX
    )))
    .output()
    .unwrap();
    assert_run(&out);
}
