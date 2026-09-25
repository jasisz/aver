//! Real parallel progress, cancellation, and the public JavaScript host ABI.
#![cfg(feature = "wasm")]
#[path = "support/aver_cmd.rs"]
mod aver_cmd;
use aver_cmd::{aver_bin, format_output, repo_root};
use std::process::{Command, Output, Stdio};
use std::time::{Duration, Instant};

fn bounded(command: &mut Command) -> Output {
    let mut child = command
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn host");
    let deadline = Instant::now() + Duration::from_secs(60);
    loop {
        if child.try_wait().expect("poll host").is_some() {
            return child.wait_with_output().expect("collect host");
        }
        if Instant::now() >= deadline {
            let _ = child.kill();
            let output = child.wait_with_output().expect("collect timed-out host");
            panic!(
                "host did not make independent progress or stop its cancelled worker:\n{}",
                format_output(&output)
            );
        }
        std::thread::sleep(Duration::from_millis(20));
    }
}

#[test]
fn finite_wasm_job_finishes_beside_an_infinite_job_and_cancel_stops_it() {
    let root = repo_root().join("tests/fixtures/work_jobs_parallel");
    let output = bounded(
        Command::new(aver_bin())
            .args(["run"])
            .arg(root.join("main.av"))
            .arg("--module-root")
            .arg(root)
            .arg("--wasm-gc"),
    );
    assert!(output.status.success(), "{}", format_output(&output));
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "parallel 340282366920938463463374607431768211457"
    );
}

#[test]
fn wasm_workers_transport_unit_tasks_and_unit_results() {
    let root = repo_root().join("tests/fixtures/work_jobs_unit_result");
    let output = bounded(
        Command::new(aver_bin())
            .arg("run")
            .arg(root.join("main.av"))
            .arg("--module-root")
            .arg(root)
            .arg("--wasm-gc"),
    );
    assert!(output.status.success(), "{}", format_output(&output));
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "unit-work-ok"
    );
}

#[test]
fn javascript_workers_run_the_coordinator_and_progress_independently() {
    let node = std::env::var_os("AVER_NODE").unwrap_or_else(|| "node".into());
    let version = Command::new(&node)
        .arg("--version")
        .output()
        .expect("Node is required for the public JS Work host test (set AVER_NODE)");
    let major: u32 = String::from_utf8_lossy(&version.stdout)
        .trim()
        .trim_start_matches('v')
        .split('.')
        .next()
        .unwrap_or("")
        .parse()
        .unwrap_or(0);
    assert!(
        major >= 22,
        "the JS host test needs Node 22+ with WasmGC and tail calls; set AVER_NODE"
    );
    let out = tempfile::tempdir().expect("wasm artifacts");
    for name in [
        "work_jobs_parallel",
        "run_guide_example",
        "work_jobs_record",
        "work_jobs_unit_task",
        "work_jobs_unit_result",
        // A program that waits and binds no job kind: the adapter has to
        // accept a module that declares none and still decode its wait set.
        "wait_socket_only",
        // A turn that calls Run.fail: the coordinator answers its reason.
        "run_fail",
    ] {
        let fixture = repo_root().join("tests/fixtures").join(name);
        let output = bounded(
            Command::new(aver_bin())
                .arg("compile")
                .arg(fixture.join("main.av"))
                .arg("--module-root")
                .arg(fixture)
                .args(["--target", "wasm-gc", "-o"])
                .arg(out.path().join(name)),
        );
        assert!(output.status.success(), "{}", format_output(&output));
    }
    let mut command = Command::new(node);
    if (24..26).contains(&major) {
        command.arg("--experimental-wasm-jspi");
    }
    let output = bounded(
        command
            .arg(repo_root().join("tools/wasm-work/spec.mjs"))
            .arg(out.path().join("work_jobs_parallel/main.wasm"))
            .arg(out.path().join("run_guide_example/main.wasm"))
            .arg(out.path().join("work_jobs_record/main.wasm"))
            .arg(out.path().join("work_jobs_unit_task/main.wasm"))
            .arg(out.path().join("work_jobs_unit_result/main.wasm"))
            .arg(out.path().join("wait_socket_only/main.wasm"))
            .arg(out.path().join("run_fail/main.wasm")),
    );
    assert!(output.status.success(), "{}", format_output(&output));
    assert!(String::from_utf8_lossy(&output.stdout).contains("worker ABI passed"));
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("socket-only wait ABI passed"),
        "{}",
        format_output(&output)
    );
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("Run.fail coordinator passed"),
        "{}",
        format_output(&output)
    );
    if major >= 24 {
        assert!(String::from_utf8_lossy(&output.stdout).contains("JSPI Work main passed"));
    }
}
