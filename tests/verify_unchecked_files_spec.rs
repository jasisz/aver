//! `aver verify` must never report a green summary while a whole file went
//! unchecked. A file whose program does not type-check, or whose verify run
//! the engine refused, is counted, named and listed — and the file that
//! carries the fault is the one named, not the entry that depends on it.
//!
//! The negative control at the bottom is load-bearing: a clean run must print
//! exactly what it printed before, byte for byte.

use std::fs;
use std::process::Command;

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "aver-verify-unchecked-{tag}-{}",
        std::process::id()
    ));
    if dir.exists() {
        fs::remove_dir_all(&dir).ok();
    }
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn write(dir: &std::path::Path, name: &str, content: &str) {
    let path = dir.join(name);
    fs::write(&path, content).unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
}

struct Run {
    stdout: String,
    stderr: String,
    code: Option<i32>,
}

fn run_verify(dir: &std::path::Path, args: &[&str]) -> Run {
    let out = Command::new(aver_bin())
        .current_dir(dir)
        .arg("verify")
        .args(args)
        .output()
        .expect("run aver verify");
    Run {
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
        code: out.status.code(),
    }
}

/// A dependency with a type error, and the entry that carries the verify
/// block. Nothing in the entry is wrong.
fn write_broken_dependency_program(dir: &std::path::Path) {
    write(
        dir,
        "main.av",
        "module Main\n    depends [Dep]\n\nfn double(n: Int) -> Int\n    Dep.twice(n)\n\nverify double\n    double(2) => 4\n    double(0) => 0\n    double(-3) => -6\n",
    );
    write(
        dir,
        "dep.av",
        "module Dep\n\nfn twice(n: Int) -> Int\n    n + \"oops\"\n",
    );
}

#[test]
fn a_fault_in_a_dependency_names_the_dependency_not_the_entry() {
    let dir = temp_dir("names-dep");
    write_broken_dependency_program(&dir);

    let run = run_verify(&dir, &["main.av"]);

    assert!(
        run.stderr.starts_with("dep.av: error["),
        "the file that carries the fault must be the one named, got stderr:\n{}",
        run.stderr
    );
    assert!(
        !run.stdout.contains("No verify blocks found"),
        "main.av declares a verify block — claiming otherwise is a lie, got stdout:\n{}",
        run.stdout
    );
    assert!(
        run.stdout
            .contains("2 file(s) not checked — type errors (run aver check for details):"),
        "both modules went unchecked and the summary must say so, got stdout:\n{}",
        run.stdout
    );
    assert!(
        run.stdout.contains("main.av (1 verify block unchecked)"),
        "the block that never ran must be counted against its file, got stdout:\n{}",
        run.stdout
    );
    assert!(
        run.stdout.contains("| 0 failed | 2 files not checked"),
        "the summary line must carry the unchecked-file count, got stdout:\n{}",
        run.stdout
    );
    assert_eq!(run.code, Some(1), "stdout:\n{}", run.stdout);

    fs::remove_dir_all(&dir).ok();
}

#[test]
fn the_json_summary_separates_an_unchecked_file_from_a_file_with_no_blocks() {
    let dir = temp_dir("json");
    write_broken_dependency_program(&dir);

    let broken = run_verify(&dir, &["main.av", "--json"]);
    let summary = broken
        .stdout
        .lines()
        .last()
        .expect("a summary record")
        .to_string();
    assert!(
        summary.contains("\"files_skipped\":2,\"blocks_unchecked\":1"),
        "the JSON summary must count what went unchecked, got:\n{summary}"
    );
    assert_eq!(broken.code, Some(1));

    // The control the old JSON was byte-identical to: a file that simply has
    // no verify blocks. The two must not read the same any more.
    let quiet = temp_dir("json-quiet");
    write(
        &quiet,
        "quiet.av",
        "module Quiet\n\nfn twice(n: Int) -> Int\n    n * 2\n",
    );
    let clean = run_verify(&quiet, &["quiet.av", "--json"]);
    let clean_summary = clean.stdout.lines().last().expect("a summary record");
    assert!(
        clean_summary.contains("\"files_skipped\":0,\"blocks_unchecked\":0"),
        "a file with no blocks skipped nothing, got:\n{clean_summary}"
    );
    assert_ne!(
        summary, clean_summary,
        "an unchecked file must not produce the same summary as a file with no blocks"
    );
    assert_eq!(clean.code, Some(0));

    fs::remove_dir_all(&dir).ok();
    fs::remove_dir_all(&quiet).ok();
}

#[test]
fn a_sibling_module_still_reports_and_the_summary_is_not_green() {
    let dir = temp_dir("siblings");
    write_broken_dependency_program(&dir);
    write(
        &dir,
        "ok.av",
        "module Ok\n\nfn inc(n: Int) -> Int\n    n + 1\n\nverify inc\n    inc(1) => 2\n",
    );

    let out = Command::new(aver_bin())
        .current_dir(&dir)
        // `colored` honours CLICOLOR_FORCE, so the summary's colour is
        // observable off a pipe. Green here would be the whole bug.
        .env("CLICOLOR_FORCE", "1")
        .args(["verify", "."])
        .output()
        .expect("run aver verify .");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();

    assert!(
        stdout.contains("Verify: ./ok.av") && stdout.contains("1/1"),
        "a module that verified must still report, got stdout:\n{stdout}"
    );
    let summary = stdout
        .lines()
        .find(|line| line.contains("Summary:"))
        .unwrap_or_else(|| panic!("a summary line, got stdout:\n{stdout}"));
    assert!(
        summary.contains("1/1 cases passed | 0 failed | 2 files not checked"),
        "the summary must carry both what ran and what did not, got:\n{summary}"
    );
    assert!(
        summary.starts_with("\u{1b}[33m"),
        "a run with unchecked files must not be green, got:\n{summary:?}"
    );
    assert_eq!(out.status.code(), Some(1));

    fs::remove_dir_all(&dir).ok();
}

#[test]
fn a_refusal_by_verify_itself_does_not_send_the_user_to_aver_check() {
    let dir = temp_dir("engine");
    // The `--hostile` expansion cap: the source type-checks, so `aver check`
    // passes and pointing the user there is a blind alley.
    write(
        &dir,
        "wide.av",
        "module Wide\n    effects []\n\nfn f(x: Int) -> Int\n    x\n\nverify f law big\n    given x: Int = 1..10000\n    f(x) => x\n",
    );

    let run = run_verify(&dir, &["wide.av", "--hostile"]);

    assert!(
        run.stdout
            .contains("1 file(s) not checked — verify could not run (not a source error"),
        "an engine refusal needs its own bucket, got stdout:\n{}",
        run.stdout
    );
    assert!(
        !run.stdout.contains("run aver check for details"),
        "`aver check` passes on this file — sending the user there is a dead end, got stdout:\n{}",
        run.stdout
    );
    assert!(
        !run.stdout.contains("No verify blocks found"),
        "wide.av declares a verify block, got stdout:\n{}",
        run.stdout
    );
    assert!(
        run.stdout.contains("| 1 file not checked"),
        "the summary line must carry the count, got stdout:\n{}",
        run.stdout
    );
    assert_eq!(run.code, Some(1));

    fs::remove_dir_all(&dir).ok();
}

#[test]
fn a_clean_run_prints_exactly_what_it_printed_before() {
    let dir = temp_dir("clean");
    write(
        dir.as_path(),
        "main.av",
        "module Main\n    depends [Dep]\n\nfn double(n: Int) -> Int\n    Dep.twice(n)\n\nverify double\n    double(2) => 4\n",
    );
    write(
        dir.as_path(),
        "dep.av",
        "module Dep\n\nfn twice(n: Int) -> Int\n    n * 2\n\nverify twice\n    twice(3) => 6\n",
    );

    let run = run_verify(&dir, &["main.av"]);

    assert_eq!(
        run.stdout,
        "Verify: dep.av\n  \u{2713} twice      1/1\n\nVerify: main.av\n  \u{2713} double      1/1\n\nSummary: 2 modules | 2 blocks | 2/2 cases passed | 0 failed\n",
        "a clean run must be byte-identical to what it printed before",
    );
    assert_eq!(run.stderr, "");
    assert_eq!(run.code, Some(0));

    fs::remove_dir_all(&dir).ok();
}
