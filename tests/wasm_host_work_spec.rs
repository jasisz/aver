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

/// Oldest Node whose V8 runs the emitted module: WasmGC and tail calls are
/// on by default from Node 22.
const MIN_NODE_MAJOR: u32 = 22;

fn node_major(node: &std::ffi::OsStr) -> Option<u32> {
    let output = Command::new(node).arg("--version").output().ok()?;
    if !output.status.success() {
        return None;
    }
    String::from_utf8_lossy(&output.stdout)
        .trim()
        .trim_start_matches('v')
        .split('.')
        .next()?
        .parse()
        .ok()
}

/// The Node that runs the JS host test and its major version.
///
/// `AVER_NODE` wins when set, and a too-old value is reported rather than
/// silently replaced by another install. Otherwise `node` on `PATH` is used when it is new enough,
/// then the newest suitable install under `~/.nvm/versions/node`. The
/// error names every candidate so a skip or failure says what was tried.
fn suitable_node() -> Result<(std::ffi::OsString, u32), String> {
    if let Some(node) = std::env::var_os("AVER_NODE") {
        return match node_major(&node) {
            Some(major) if major >= MIN_NODE_MAJOR => Ok((node, major)),
            Some(major) => Err(format!(
                "AVER_NODE={} is Node {major}; Node {MIN_NODE_MAJOR}+ is required",
                node.to_string_lossy()
            )),
            None => Err(format!(
                "AVER_NODE={} did not run `--version`",
                node.to_string_lossy()
            )),
        };
    }
    let mut tried = Vec::new();
    let path_node = std::ffi::OsString::from("node");
    match node_major(&path_node) {
        Some(major) if major >= MIN_NODE_MAJOR => return Ok((path_node, major)),
        Some(major) => tried.push(format!("`node` on PATH is Node {major}")),
        None => tried.push("no `node` on PATH".to_string()),
    }
    if let Some(home) = std::env::var_os("HOME") {
        let nvm = std::path::PathBuf::from(home).join(".nvm/versions/node");
        let mut installs: Vec<(u32, std::path::PathBuf)> = std::fs::read_dir(&nvm)
            .into_iter()
            .flatten()
            .filter_map(|entry| entry.ok())
            .filter_map(|entry| {
                let name = entry.file_name();
                let major = name
                    .to_str()?
                    .trim_start_matches('v')
                    .split('.')
                    .next()?
                    .parse()
                    .ok()?;
                Some((major, entry.path().join("bin/node")))
            })
            .filter(|(major, bin)| *major >= MIN_NODE_MAJOR && bin.is_file())
            .collect();
        installs.sort();
        if let Some((_, bin)) = installs.pop() {
            let node = bin.into_os_string();
            if let Some(major) = node_major(&node) {
                return Ok((node, major));
            }
        }
        tried.push(format!("no Node {MIN_NODE_MAJOR}+ under {}", nvm.display()));
    }
    Err(tried.join("; "))
}

/// A suitable Node, or `None` after saying why the named test skips.
fn node_or_skip(test: &str) -> Option<(std::ffi::OsString, u32)> {
    match suitable_node() {
        Ok(found) => Some(found),
        // CI installs a suitable Node for this lane, so a missing one there
        // is a broken lane, not a reason to skip.
        Err(why) if std::env::var_os("CI").is_some() => {
            panic!("{test} needs Node {MIN_NODE_MAJOR}+ with WasmGC and tail calls: {why}")
        }
        Err(why) => {
            eprintln!(
                "SKIP {test}: needs Node {MIN_NODE_MAJOR}+ with WasmGC and tail calls ({why}); \
                 set AVER_NODE to a suitable node"
            );
            None
        }
    }
}

#[test]
fn javascript_workers_run_the_coordinator_and_progress_independently() {
    let Some((node, major)) =
        node_or_skip("javascript_workers_run_the_coordinator_and_progress_independently")
    else {
        return;
    };
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
        // A program that reads Run.lastTurn: the host's clock marks the
        // waits it awaits.
        "run_last_turn",
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
            .arg(out.path().join("run_fail/main.wasm"))
            .arg(out.path().join("run_last_turn/main.wasm")),
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
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("Run.lastTurn coordinator passed"),
        "{}",
        format_output(&output)
    );
    if major >= 24 {
        assert!(String::from_utf8_lossy(&output.stdout).contains("JSPI Work main passed"));
    }
}

/// `tools/wasm-work/run.mjs` runs a plain program's `main` under Node with the
/// output the VM gives: its arguments, Console, positional and whole-file Disk
/// reads, and the exit status `main -> Int` names.
#[test]
fn node_runner_runs_a_plain_main_like_the_vm() {
    let Some((node, _)) = node_or_skip("node_runner_runs_a_plain_main_like_the_vm") else {
        return;
    };
    let fixture = repo_root().join("tests/fixtures/node_runner");
    let out = tempfile::tempdir().expect("wasm artifacts");
    let data = out.path().join("data.txt");
    std::fs::write(&data, "read in chunks of five, then whole\n").expect("data file");
    let missing = out.path().join("missing.txt");
    let compiled = bounded(
        Command::new(aver_bin())
            .arg("compile")
            .arg(fixture.join("main.av"))
            .arg("--module-root")
            .arg(&fixture)
            .args(["--target", "wasm-gc", "-o"])
            .arg(out.path()),
    );
    assert!(compiled.status.success(), "{}", format_output(&compiled));
    let vm = bounded(
        Command::new(aver_bin())
            .arg("run")
            .arg(fixture.join("main.av"))
            .arg("--module-root")
            .arg(&fixture)
            .arg("--")
            .arg(&data)
            .arg(&missing),
    );
    let v8 = bounded(
        Command::new(node)
            .arg(repo_root().join("tools/wasm-work/run.mjs"))
            .arg(out.path().join("main.wasm"))
            .arg(&data)
            .arg(&missing),
    );
    assert_eq!(vm.status.code(), Some(3), "{}", format_output(&vm));
    assert_eq!(v8.status.code(), Some(3), "{}", format_output(&v8));
    assert_eq!(
        String::from_utf8_lossy(&v8.stdout),
        String::from_utf8_lossy(&vm.stdout)
    );
    assert!(String::from_utf8_lossy(&vm.stdout).contains("end at 35"));
    assert_eq!(String::from_utf8_lossy(&v8.stderr), "missing file fails\n");
}
