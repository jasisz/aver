//! WASI coordinators stop through their policy or exhaustion, without a
//! generated signal read. Explicit unsupported effects still fail the door.

#![cfg(feature = "wasip2")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn fixture(name: &str) -> PathBuf {
    repo_root().join("tests/fixtures").join(name)
}

fn run(dir: &Path, target: Option<&str>) -> Output {
    let mut command = Command::new(aver_bin());
    command
        .current_dir(dir)
        .args(["run", "main.av", "--module-root", "."]);
    if let Some(target) = target {
        command.arg(target);
    }
    command.output().expect("run Aver")
}

fn stdout(output: &Output) -> String {
    assert!(output.status.success(), "{}", format_output(output));
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn compile(dir: &Path, output: &Path) -> Output {
    Command::new(aver_bin())
        .current_dir(dir)
        .args([
            "compile",
            "main.av",
            "--module-root",
            ".",
            "--target",
            "wasip2",
            "-o",
        ])
        .arg(output)
        .env("AVER_YIELD_DUMP", "1")
        .output()
        .expect("compile Aver component")
}

#[test]
fn default_guide_runs_and_compiles_without_a_signal_import() {
    let dir = fixture("run_guide_example");
    assert_eq!(stdout(&run(&dir, Some("--wasip2"))), "scored 60");
    let out_dir = tempfile::tempdir().unwrap();
    let output = compile(&dir, out_dir.path());
    assert!(output.status.success(), "{}", format_output(&output));
    let lowered = String::from_utf8_lossy(&output.stderr);
    // Dependency preparation may also dump its target-neutral entry view;
    // the last turn is the one the component pipeline actually emits.
    let turn = lowered.rsplit("\nfn __turn(").next().unwrap();
    assert!(turn.contains("stopping = false"), "{turn}");
    assert!(!turn.contains("Process.stopRequested"), "{turn}");
    let bytes = fs::read(out_dir.path().join("main.component.wasm")).expect("component artifact");
    assert_eq!(&bytes[..8], b"\0asm\x0d\0\x01\0");
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .expect("valid component");
}

#[test]
fn a_keyed_family_is_seated_and_dropped_on_wasip2() {
    let dir = fixture("run_families");
    assert_eq!(
        stdout(&run(&dir, Some("--wasip2"))),
        stdout(&run(&dir, None))
    );
}

#[test]
fn a_cancelled_job_is_answered_the_same_on_wasip2() {
    let dir = fixture("run_failed_job");
    assert_eq!(
        stdout(&run(&dir, Some("--wasip2"))),
        stdout(&run(&dir, None))
    );
}

#[test]
fn several_job_kinds_finish_with_default_policies() {
    let dir = fixture("run_two_job_kinds");
    let sorted = |text: String| {
        let mut lines: Vec<_> = text.lines().map(str::to_string).collect();
        lines.sort();
        lines
    };
    assert_eq!(
        sorted(stdout(&run(&dir, Some("--wasip2")))),
        sorted(stdout(&run(&dir, None)))
    );
}

#[test]
fn a_custom_policy_can_stop_with_a_process_still_seated() {
    let dir = fixture("run_policy_stop");
    assert_eq!(stdout(&run(&dir, Some("--wasip2"))), "seated");
    assert_eq!(stdout(&run(&dir, None)), "seated");
}

#[test]
fn an_explicit_signal_read_in_a_process_is_still_rejected() {
    let dir = tempfile::tempdir().unwrap();
    for name in ["main.av", "clock.av", "ticks.av"] {
        fs::copy(
            fixture("run_all_from_main").join(name),
            dir.path().join(name),
        )
        .unwrap();
    }
    let main = dir.path().join("main.av");
    let source = fs::read_to_string(&main)
        .unwrap()
        .replace(
            "effects [Args.get, Clock.tick, Console.print, yield]",
            "effects [Args.get, Clock.tick, Console.print, Process.stopRequested, yield]",
        )
        .replace(
            "    ! [Clock.tick, Console.print, yield]\n    ticking(0)",
            "    ! [Clock.tick, Console.print, Process.stopRequested, yield]\n    Console.print(\"{Process.stopRequested()}\")\n    ticking(0)",
        );
    fs::write(main, source).unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    for output in [
        run(dir.path(), Some("--wasip2")),
        compile(dir.path(), out_dir.path()),
    ] {
        assert!(
            !output.status.success(),
            "explicit signal read was accepted"
        );
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(
            error.contains("error[capability-target-unsupported]"),
            "{error}"
        );
        assert!(
            error.contains("required operations: Process.stopRequested"),
            "{error}"
        );
    }
}

#[test]
fn native_lowering_keeps_the_host_signal_observation() {
    let output = Command::new(aver_bin())
        .current_dir(fixture("run_guide_example"))
        .args(["check", "main.av", "--module-root", "."])
        .env("AVER_YIELD_DUMP", "1")
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", format_output(&output));
    let lowered = String::from_utf8_lossy(&output.stderr);
    assert!(
        lowered.contains("stopping = Process.stopRequested()"),
        "{lowered}"
    );
}
